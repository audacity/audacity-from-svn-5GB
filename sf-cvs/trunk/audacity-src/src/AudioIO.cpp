/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.cpp

  Dominic Mazzoni

  Use the PortAudio library to play and record sound

**********************************************************************/

#include <math.h>

#include <wx/log.h>
#include <wx/textctrl.h>
#include <wx/msgdlg.h>
#include <wx/timer.h>
#include <wx/intl.h>

#include "AudioIO.h"
#include "Project.h"
#include "Track.h"
#include "WaveTrack.h"  
#include "Mix.h"
#include "ControlToolBar.h"
#include "Prefs.h"

AudioIO *gAudioIO;

void InitAudioIO()
{
   gAudioIO = new AudioIO();
}

AudioIO::AudioIO()
{
   mProject = NULL;
   mTracks = NULL;
   mNumOutTracks = 0;
   mInBufferSize = 44100 * 5;
   mInBuffers = NULL;
   mInTracks = NULL;
   mOutBufferSize = 44100 * 5;
   mOutBuffers = NULL;
   mOutTracks = NULL;
   mRate = 44100.0;
   mT = 0.0;
   mRecT = 0.0;
   mT0 = 0.0;
   mT1 = 0.0;
   mHardStop = false;
   mPaused = false;
   mAlwaysEnablePause = false;
   mStarted = false;
   mReachedEnd = false;
   mPausePosition = 0.0;
   mPortStream = NULL;
   mNumInChannels = 0;
   mNumOutChannels = 0;
   mFormat = floatSample;
   mTempFloats = new float[65536]; // TODO: out channels * PortAudio buffer size
   mLostSamples = 0;
   mDroppedSamples = 0;

   PaError err = Pa_Initialize();

   if (err != paNoError) {
      wxString errStr = _("Audacity was unable to find any audio devices.");
      wxString paErrStr = Pa_GetErrorText(err);
      if (paErrStr)
         errStr += "\nError: "+paErrStr;
      wxMessageBox(errStr);
      return;
   }

   // Start thread
   mThread = new AudioThread();
   mThread->Create();
   mThread->Run();
}

AudioIO::~AudioIO()
{
   Pa_Terminate();

   mThread->Wait();
}

int audacityAudioCallback(
   void *inputBuffer, void *outputBuffer,
   unsigned long framesPerBuffer,
   PaTimestamp outTime, void *userData )
{
   int numOutChannels = gAudioIO->mNumOutChannels;
   int numOutTracks = gAudioIO->mNumOutTracks;
   int numInChannels = gAudioIO->mNumInChannels;
   float *tempFloats = gAudioIO->mTempFloats;
   unsigned int i;
   int t;

   if(gAudioIO->GetPaused()) {
      if (outputBuffer && numOutChannels > 0) {
         ClearSamples((samplePtr)outputBuffer, gAudioIO->GetFormat(),
                      0, framesPerBuffer * numOutChannels);
      }
      
      gAudioIO->AddDroppedSamples(framesPerBuffer);
      return 0;
   }

   //
   // Mix and copy to PortAudio's output buffer
   //

   if (outputBuffer && numOutChannels > 0) {

      float *outputFloats = (float *)outputBuffer;
      for(i=0; i<framesPerBuffer*numOutChannels; i++)
         outputFloats[i] = 0.0;
      
      int numSolo = 0;
      for(t=0; t<numOutTracks; t++)
         if (gAudioIO->mOutTracks[t]->GetSolo())
            numSolo++;
      
      for(t=0; t<numOutTracks; t++) {   
         WaveTrack *vt = (WaveTrack *)gAudioIO->mOutTracks[t];
         WaveTrack *mt = vt;
         
         // We want to extract mute and solo information from
         // the top of the two tracks if they're linked
         // (i.e. a stereo pair only has one set of mute/solo buttons)
         Track *partner = gAudioIO->mTracks->GetLink(vt);
         if (partner && !vt->GetLinked())
            mt = (WaveTrack *)partner;
         else
            mt = vt;
         
         // Cut if somebody else is soloing
         if (numSolo>0 && !mt->GetSolo())
            continue;
         
         // Cut if we're muted (unless we're soloing)
         if (mt->GetMute() && !mt->GetSolo())
            continue;
         
         unsigned int len = (unsigned int)
            gAudioIO->mOutBuffers[t]->Get((samplePtr)tempFloats, floatSample,
                                          (int)framesPerBuffer);

         // If our buffer is empty and the time indicator is past
         // the end, then we've actually finished playing the entire
         // selection.
         if (len == 0 && gAudioIO->mT >= gAudioIO->mT1)
            gAudioIO->mReachedEnd = true;

         if (vt->GetChannel() == Track::LeftChannel ||
             vt->GetChannel() == Track::MonoChannel)
            for(i=0; i<len; i++)
               outputFloats[numOutChannels*i] += tempFloats[i];
         
         if (vt->GetChannel() == Track::RightChannel ||
             vt->GetChannel() == Track::MonoChannel)
            for(i=0; i<len; i++)
               outputFloats[numOutChannels*i+1] += tempFloats[i];
      }
   }
   
   //
   // Copy from PortAudio to our input buffers.
   //
   
   if (inputBuffer && numInChannels > 0) {
      float *inputFloats = (float *)inputBuffer;
      unsigned int len = framesPerBuffer;
      for(t=0; t<numInChannels; t++) {
         unsigned int avail =
            (unsigned int)gAudioIO->mInBuffers[t]->AvailForPut();
         if (avail < len)
            len = avail;
      }

      if (len < framesPerBuffer)
         gAudioIO->mLostSamples += (framesPerBuffer - len);

      if (len > 0) {
         for(t=0; t<numInChannels; t++) {
            for(i=0; i<len; i++)
               tempFloats[i] = inputFloats[numInChannels*i+t];
            
            gAudioIO->mInBuffers[t]->Put((samplePtr)tempFloats,
                                         floatSample, len);
         }
      }
   }
   
   return 0;
}

bool AudioIO::OpenDevice()
{
   PaError         error;
   int             numPortAudioBuffers;
   int             recDeviceNum;
   int             playDeviceNum;
   PaSampleFormat  paFormat;
   wxString        recDevice;
   wxString        playDevice;

   numPortAudioBuffers = Pa_GetMinNumBuffers(mPortAudioBufferSize, mRate);

   if (mNumInChannels>0)
      numPortAudioBuffers *= 2;

   recDeviceNum = Pa_GetDefaultInputDeviceID();
   playDeviceNum = Pa_GetDefaultOutputDeviceID();

   recDevice = gPrefs->Read("/AudioIO/RecordingDevice", "");
   playDevice = gPrefs->Read("/AudioIO/PlaybackDevice", "");

   mFormat = (sampleFormat)gPrefs->Read("/AudioIO/SampleFormat", floatSample);
   paFormat = paFloat32;

   for(int j=0; j<Pa_CountDevices(); j++) {
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      if (info->name == playDevice && info->maxOutputChannels > 0)
         playDeviceNum = j;
      if (info->name == recDevice && info->maxInputChannels > 0)
         recDeviceNum = j;
   }

   if (mNumOutChannels<=0)
      playDeviceNum = paNoDevice;
   if (mNumInChannels<=0)
      recDeviceNum = paNoDevice;

   error = Pa_OpenStream(&mPortStream,
                         recDeviceNum,
                         mNumInChannels,
                         paFormat,
                         NULL, /* inputDriverInfo */
                         playDeviceNum,
                         mNumOutChannels,
                         paFormat,
                         NULL,
                         mRate,
                         (unsigned long)mPortAudioBufferSize,
                         (unsigned long)numPortAudioBuffers,
                         paClipOff | paDitherOff,
                         audacityAudioCallback,
                         NULL);

   return (mPortStream != NULL && error == paNoError);
}

bool AudioIO::Start()
{
   mT = mT0;
   mPortAudioBufferSize = 512;
   mDroppedSamples = 0;
   mLostSamples = 0;
   mLastIndicator = mT0;
   mLastStableIndicator = mT0;
   if(!mAlwaysEnablePause)
      mPaused = false;
   mPausePosition = 0.0;

   unsigned int i;

   if (mNumInChannels > 0) {
      // 5 seconds of buffering per channel
      mInBufferSize = (sampleCount)(mRate * 5);
      mInBuffers = new RingBuffer*[mNumInChannels];
      for(i=0; i<mNumInChannels; i++)
         mInBuffers[i] = new RingBuffer(floatSample, mInBufferSize);
   }

   if (mNumOutChannels > 0 && mNumOutTracks > 0) {
      // 5 seconds of buffering per channel
      mOutBufferSize = (sampleCount)(mRate * 5);
      mOutBuffers = new RingBuffer*[mNumOutTracks];
      for(i=0; i<mNumOutChannels; i++)
         mOutBuffers[i] = new RingBuffer(floatSample, mOutBufferSize);
   }
   
   FillBuffers();

   if (!OpenDevice()) {
      wxMessageBox(_("Error opening audio device.\n"
                     "(Change the device in the Preferences dialog.)"));

      return false;
   }

   PaError error = Pa_StartStream(mPortStream);

   if (error == paNoError)
      mStarted = true;

   mProject->SetStop(false);
   mProject->HandleResize();

   return (error == paNoError);
}

void AudioIO::PrepareOutTracks(TrackList * tracks)
{
   mNumOutTracks = 0;
   TrackListIterator iter(mTracks);
   Track *vt = iter.First();
   while (vt) {
      if (vt->GetKind() == Track::Wave)
         mNumOutTracks++;
      vt = iter.Next();
   }

   mOutTracks = new WaveTrack*[mNumOutTracks];

   int j = 0;
   TrackListIterator iter2(mTracks);
   vt = iter2.First();
   while (vt) {
      if (vt->GetKind() == Track::Wave)
         mOutTracks[j++] = (WaveTrack *)vt;
      vt = iter2.Next();
   }
}

bool AudioIO::StartPlay(AudacityProject * project, TrackList * tracks,
                        double t0, double t1)
{
   if (mProject)
      return false;

   mProject = project;
   mTracks = tracks;
   mRate = project->GetRate();
   mT0 = t0;
   mT1 = t1;

   mNumInChannels = 0;
   mNumOutChannels = 2;

   PrepareOutTracks(tracks);

   return Start();
}

bool AudioIO::StartRecord(AudacityProject * project, TrackList * tracks, 
                         double t0, double t1)
{
   if (mProject)
      return false;
      
   mProject = project;
   mTracks = tracks;
   mRate = project->GetRate();
   mT0 = t0;
   mT1 = t1;

   bool stereo, duplex;
   gPrefs->Read("/AudioIO/RecordStereo", &stereo, false);
   gPrefs->Read("/AudioIO/Duplex", &duplex, false);
   mFormat = (sampleFormat)gPrefs->Read("/AudioIO/SampleFormat", floatSample);

   mNumInChannels = stereo? 2: 1;

#ifdef __MACOSX__
   mNumInChannels = 2; // DM: OS X is buggy, only does stereo
#endif

   mNumOutChannels = duplex? 2: 0;
   
   if (mNumOutChannels > 0)
      PrepareOutTracks(tracks);

   mProject->SelectNone();

   mInTracks = new WaveTrack*[mNumInChannels];
   for(unsigned int i=0; i<mNumInChannels; i++) {
      mInTracks[i] = new WaveTrack(project->GetDirManager(), mFormat);
      mInTracks[i]->SetSelected(true);
      mInTracks[i]->SetOffset(mT0);
      if (stereo)
         mInTracks[i]->SetChannel(i==0? Track::LeftChannel : Track::RightChannel);
      else
         mInTracks[i]->SetChannel(Track::MonoChannel);
      if (stereo && i==0)
         mInTracks[i]->SetLinked(true);
      mInTracks[i]->SetRate(mRate);
      
      mTracks->Add(mInTracks[i]);
   }

   return Start();
}

void AudioIO::AddDroppedSamples(sampleCount nSamples)
{
   mDroppedSamples += nSamples;
}

double AudioIO::GetPauseIndicator()
{
   return mPausePosition;
}

void AudioIO::SetPaused(bool state)
{
   if(state)
      mPausePosition = GetIndicator();

   mPaused = state;
}

bool AudioIO::GetPaused(){
   return mPaused;
}

void AudioIO::FillBuffers()
{
   int i;

   // Playback buffers

   if (mNumOutChannels > 0) {

      unsigned int block = mOutBufferSize;
      
      for(i=0; i<mNumOutTracks; i++) {
         unsigned int avail = mOutBuffers[i]->AvailForPut();
         if (avail < block)
            block = avail;
      }
      
      // Don't fill playback buffers unless we have at least 1 second free
      if (block >= mRate || (mStopping && !mHardStop)) {
         double deltat = block / mRate;

         if (mT + deltat > mT1) {
            deltat = mT1 - mT;
            if(deltat < 0.0)
               deltat = 0.0;
            block = (unsigned int)(deltat * mRate + 0.5);
         }
         
         if (block > 0) {
            for(i=0; i<mNumOutTracks; i++) {
               WaveTrack *t = mOutTracks[i];
               
               // Use the mixer only as a format and rate converter for now   
               Mixer *mixer = new Mixer(1, block, true, mRate, floatSample);
               mixer->Clear();
               mixer->MixMono(t, mT, mT + deltat);
               samplePtr outbytes = mixer->GetBuffer();
               mOutBuffers[i]->Put(outbytes, floatSample, block);
               delete mixer;
            }
         }
         
         mT += deltat;
      }
   }
   
   // Recording buffers

   if (mNumInChannels > 0) {

      unsigned int len = mInBufferSize;
      for(i=0; i<(int)mNumInChannels; i++) {
         unsigned int avail = (unsigned int)mInBuffers[i]->AvailForGet();
         if (avail < len)
            len = avail;
      }
      
      // Don't empty recording buffers unless we have at least
      // 1 second recorded (or the user has pressed 'stop')
      if (len >= mRate || (mStopping && !mHardStop && len>0)) {
         samplePtr temp = NewSamples(len, floatSample);
         for(i=0; i<(int)mNumInChannels; i++) {
            mInBuffers[i]->Get(temp, floatSample, len);
            mInTracks[i]->Append(temp, floatSample, len);
         }
         DeleteSamples(temp);
         
         // We can't redraw from this thread, so post a message
         // telling the project to redraw itself next time through
         // its main event loop.
         mProject->PostRedrawMessage();
      }
   }
}

void AudioIO::Stop()
{
   if (!mProject)
      return;

   if (!mHardStop) {
      mProject->GetControlToolBar()->SetPlay(false);
      mProject->GetControlToolBar()->SetStop(false);
      mProject->GetControlToolBar()->SetRecord(false);
   }
   
   // If we reached the end of the selection, we call Pa_StopStream,
   // which waits until all buffers are finished.  If the user requested
   // the stop, we call Pa_AbortStream, which stops immediately.
   if (mReachedEnd && !mHardStop)
      Pa_StopStream(mPortStream);
   else
      Pa_AbortStream(mPortStream);

   Pa_CloseStream(mPortStream);

   mPortStream = NULL;

   mStopping = true;
}

void AudioIO::HardStop()
{
   mHardStop = true;
   Stop();
}

sampleFormat AudioIO::GetFormat()
{
   return mFormat;
}

bool AudioIO::IsBusy()
{
   return (mProject != NULL);
}

bool AudioIO::IsPlaying()
{
   return (mProject && mNumOutChannels > 0);
}

bool AudioIO::IsRecording(Track *t)
{
   if (!mProject || !mNumInChannels)
      return false;

   if (t) {
      for (unsigned int i = 0; i < mNumInChannels; i++)
         if (mInTracks[i] == t)
            return true;
      return false;
   }
   return true;
}

AudacityProject *AudioIO::GetProject()
{
   return mProject;
}

double AudioIO::GetIndicator()
{
   if (mProject && mPortStream)
      if(GetPaused())
         return GetPauseIndicator();
      else {
         double streamTime = Pa_StreamTime(mPortStream);
         double indicator = mT0 + ((streamTime-mDroppedSamples)/ mRate);

         // Pa_StreamTime can sometimes return wacky results, so we
         // try to filter those out...
         if (fabs(indicator - mLastIndicator) > 0.1) {
            mLastIndicator = indicator;
            return mLastStableIndicator;
         }
         mLastIndicator = indicator;
         mLastStableIndicator = indicator;
         return indicator;
      }
   else
      return -1000000000.0;
}

void AudioIO::SetAlwaysEnablePause(bool bEnable)
{
   mAlwaysEnablePause = bEnable;
}

void AudioIO::Finish()
{
   // Note that this should only be called from the AudioThread,
   // after it has received the Stop message

   int i;
   if (gAudioIO->mNumOutChannels > 0) {
      for(i=0; i<mNumOutTracks; i++)
         delete mOutBuffers[i];
      delete[] mOutBuffers;
      delete[] mOutTracks;
   }
   if (mNumInChannels > 0) {
      for(i=0; i<(int)mNumInChannels; i++)
         delete mInBuffers[i];
      delete[] mInBuffers;
      delete[] mInTracks;
   }

   if (!mHardStop) {
      if (mNumInChannels > 0)
         mProject->TP_PushState("Recorded Audio");
      
      if (mLostSamples) {
         wxString str;
         str.Printf(_("Sorry, %d samples were lost during recording."),
                    mLostSamples);
         wxMessageBox(str);
      }
   }
   
   mNumOutChannels = 0;
   mOutBuffers = NULL;
   mOutTracks = NULL;
   mNumInChannels = 0;
   mInBuffers = NULL;
   mInTracks = NULL;
   
   mProject->SetStop(true);
   mProject->HandleResize();
   mStarted = false;
   mProject = NULL;
   mStopping = false;
   mPaused = false;
   mHardStop = false;   
   mReachedEnd = false;
}

AudioThread::AudioThread():
   wxThread(wxTHREAD_JOINABLE)
{
}

wxThread::ExitCode AudioThread::Entry()
{
   while(!TestDestroy()) {
      if (gAudioIO->mProject && gAudioIO->mStarted)   
         gAudioIO->FillBuffers();

      if (gAudioIO->mReachedEnd)
         gAudioIO->Stop();

      if (gAudioIO->mStopping) {
         gAudioIO->FillBuffers();
         gAudioIO->Finish();
      }

      Yield();
   }

   return 0;
}

