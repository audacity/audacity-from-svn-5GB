/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.cpp

  Dominic Mazzoni

  Use the PortAudio library to play and record sound

**********************************************************************/

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
   mHardStop = false;
   mInTracks = NULL;
   mPortStream = NULL;
   mMaxBuffers = 48;
   mInitialNumOutBuffers = 8;
   mFormat = floatSample;
   mPaused = false;

   PaError err = Pa_Initialize();

   if (err != paNoError) {
      wxMessageBox(Pa_GetErrorText(err));
      return;
   }
   
   // Run our timer function once every 200 ms, i.e. 5 times/sec
   mTimer.Start(200, FALSE);
}

AudioIO::~AudioIO()
{
   Pa_Terminate();
}

int audacityAudioCallback(
		void *inputBuffer, void *outputBuffer,
		unsigned long framesPerBuffer,
		PaTimestamp outTime, void *userData )
{
 
   int numOutChannels = gAudioIO->mNumOutChannels;
   int numInChannels = gAudioIO->mNumInChannels;
   int minIndex = 0, minID = 0;
   unsigned int i;



   

   //
   // Copy from our pool of output buffers to PortAudio's output buffer
   //
   
   if (outputBuffer && numOutChannels > 0) {
   
      for(i=0; i<gAudioIO->mNumOutBuffers; i++) {
         if (gAudioIO->mOutBuffer[i].ID>0 &&
             (minID==0 || gAudioIO->mOutBuffer[i].ID < minID)) {
            minID = gAudioIO->mOutBuffer[i].ID;
            minIndex = i;
         }
      }
      if (minID > 0) {
         AudioIOBuffer *b = &gAudioIO->mOutBuffer[minIndex];
         sampleCount len  = b->len;
      
         memcpy(outputBuffer, b->data,
                len * numOutChannels * SAMPLE_SIZE(gAudioIO->GetFormat()));

         // Fill rest of buffer with silence
         if (len < (sampleCount) framesPerBuffer)
            ClearSamples((samplePtr)outputBuffer, gAudioIO->GetFormat(),
                         len*numOutChannels,
                         (framesPerBuffer-len)*numOutChannels);
         
         b->ID = 0;
      }
      else {
         // we had a buffer underrun!
         
         // play silence
         ClearSamples((samplePtr)outputBuffer, gAudioIO->GetFormat(),
                      0, framesPerBuffer * numOutChannels);
         
         // increase number of output buffers to prevent another underrun!
         if (gAudioIO->mNumOutBuffers < gAudioIO->mMaxBuffers)
            gAudioIO->mNumOutBuffers++;
      }
   }
   
   //
   // Copy from PortAudio's input buffer to one of our input buffers.
   //
   
   if (inputBuffer && numInChannels > 0) {
      bool found = false;
      for(i=0; i<gAudioIO->mNumInBuffers; i++) {
         if (gAudioIO->mInBuffer[i].ID == 0) {
            int len = framesPerBuffer;
            int sampleSize = SAMPLE_SIZE(gAudioIO->GetFormat());
            
            AudioIOBuffer *b = &gAudioIO->mInBuffer[i];
            memcpy(b->data, inputBuffer,
                   len * numInChannels * sampleSize);
            b->len = len;
            b->ID = gAudioIO->mInID;
            gAudioIO->mInID++;
            found = true;
            
            int checksum = 0;
            for(int k=0; k<len*numInChannels*sampleSize; k++)
               checksum += ((char *)inputBuffer)[k];
            if (checksum == gAudioIO->mLastChecksum) {
               gAudioIO->mRepeats++;
               gAudioIO->mRepeatPoint = gAudioIO->mT;
            }
            gAudioIO->mLastChecksum = checksum;
            
            break;
         }
      }
      
      if (!found) {
         // we had a buffer underrun!
         
         gAudioIO->mInUnderruns++;
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

   numPortAudioBuffers = Pa_GetMinNumBuffers(mBufferSize, mRate);

   if (mNumInChannels>0)
      numPortAudioBuffers *= 2;

   recDeviceNum = Pa_GetDefaultInputDeviceID();
   playDeviceNum = Pa_GetDefaultOutputDeviceID();

   recDevice = gPrefs->Read("/AudioIO/RecordingDevice", "");
   playDevice = gPrefs->Read("/AudioIO/PlaybackDevice", "");

   mFormat = (sampleFormat)gPrefs->Read("/AudioIO/SampleFormat", floatSample);

   switch(mFormat) {
   case floatSample:
      paFormat = paFloat32;
      break;
   case int16Sample:
      paFormat = paInt16;
      break;
   default:
      // Debug message only
      printf("Cannot output this sample format using PortAudio\n");
      return false;
   }

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
                         (unsigned long)mBufferSize,
                         (unsigned long)numPortAudioBuffers,
                         paClipOff | paDitherOff,
                         audacityAudioCallback,
                         NULL);

   return (mPortStream != NULL && error == paNoError);
}

bool AudioIO::Start()
{
   mT = mT0;
   mOutID = 1;
   mInID = 1;
   mBufferSize = 4096;
   mInUnderruns = 0;
   mRepeats = 0;

   unsigned int i;

   if (mNumInChannels > 0) {
      mInBuffer = new AudioIOBuffer[mMaxBuffers];
      for(i=0; i<mMaxBuffers; i++) {
         mInBuffer[i].ID = 0;   // means it's empty
         mInBuffer[i].len = 0;
         mInBuffer[i].data = NewSamples(mBufferSize * mNumInChannels,
                                        mFormat);
      }
   }
   else
      mInBuffer = NULL;

   if (mNumOutChannels > 0) {
      mOutBuffer = new AudioIOBuffer[mMaxBuffers];
      for(i=0; i<mMaxBuffers; i++) {
         mOutBuffer[i].ID = 0;   // means it's empty
         mOutBuffer[i].len = 0;
         mOutBuffer[i].data = NewSamples(mBufferSize * mNumOutChannels,
                                         mFormat);
      }
   }
   else
      mOutBuffer = NULL;
   
   FillBuffers();

   if (!OpenDevice()) {
      wxMessageBox(_("Error opening audio device.\n"
                     "(Change the device in the Preferences dialog.)"));

      return false;
   }

   PaError error = Pa_StartStream(mPortStream);

   mProject->SetStop(false);
   mProject->ReReadSettings();
   mProject->HandleResize();

   return (error == paNoError);
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
   mNumInBuffers = 0;

   mNumOutChannels = 2;
   mNumOutBuffers = mInitialNumOutBuffers;

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
   mNumInBuffers = mMaxBuffers;

   mNumOutChannels = duplex? 2: 0;
   mNumOutBuffers = duplex? mInitialNumOutBuffers: 0;
   
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

void AudioIO::SetPaused(bool state)
{
   mPaused = state;
}

bool AudioIO::GetPaused(){
   return mPaused;
}

void AudioIO::FillBuffers()
{

   unsigned int numEmpty = 0;
   unsigned int i;
   
   // Playback buffers

   for(i=0; i<mNumOutBuffers; i++) {
      if (mOutBuffer[i].ID == 0)
         numEmpty++;
   }
   
   if (numEmpty > (mNumOutBuffers/2)) {
      sampleCount block = numEmpty * mBufferSize;   
      double deltat = block / mRate;
      if (mT + deltat > mT1) {
         deltat = mT1 - mT;
         if(deltat < 0.0) return;
         block = (sampleCount)(deltat * mRate + 0.5);
      }
      
      Mixer *mixer = new Mixer(mNumOutChannels, block, true,
                               mRate, mFormat);
      mixer->UseVolumeSlider(mProject->GetControlToolBar());

      mixer->Clear();

      TrackListIterator iter2(mTracks);
      int numSolo = 0;
      Track *vt = iter2.First();
      while (vt) {
         if (vt->GetKind() == Track::Wave && vt->GetSolo())
            numSolo++;
         vt = iter2.Next();
      }

      TrackListIterator iter(mTracks);
      vt = iter.First();
      while (vt) {
         if (vt->GetKind() == Track::Wave) {      

            Track *mt = vt;
         
            // We want to extract mute and solo information from
            // the top of the two tracks if they're linked
            // (i.e. a stereo pair only has one set of mute/solo buttons)
            Track *partner = mTracks->GetLink(vt);
            if (partner && !vt->GetLinked())
               mt = partner;
            else
               mt = vt;

            // Cut if somebody else is soloing
            if (numSolo>0 && !mt->GetSolo()) {
               vt = iter.Next();
               continue;
            }
            
            // Cut if we're muted (unless we're soloing)
            if (mt->GetMute() && !mt->GetSolo()) {
               vt = iter.Next();
               continue;
            }

            WaveTrack *t = (WaveTrack *) vt;
            
            switch (t->GetChannel()) {
            case Track::LeftChannel:
               mixer->MixLeft(t, mT, mT + deltat);
               break;
            case Track::RightChannel:
               mixer->MixRight(t, mT, mT + deltat);
               break;
            case Track::MonoChannel:
               mixer->MixMono(t, mT, mT + deltat);
               break;
            }
         }

         vt = iter.Next();
      }     
   
      // Copy the mixed samples into the buffers

      samplePtr outbytes = mixer->GetBuffer();   

      for(i=0; i<mNumOutBuffers && block>0; i++)
         if (mOutBuffer[i].ID == 0) {
            sampleCount count;
            if (block > mBufferSize)
               count = mBufferSize;
            else
               count = block;
            
            memcpy(mOutBuffer[i].data, outbytes,
                   count*mNumOutChannels*SAMPLE_SIZE(mFormat));
            block -= count;
            outbytes += (count*mNumOutChannels*SAMPLE_SIZE(mFormat));
            mOutBuffer[i].len = count;
            mOutBuffer[i].ID = mOutID;
            mOutID++;
         }

      delete mixer;

      mT += deltat;
   }
   
   // Recording buffers
   
   unsigned int numFull = 0;
   unsigned int f, c; // loop counters
   sampleCount flatLen;
      
   for(i=0; i<mNumInBuffers; i++) {
      if (mInBuffer[i].ID != 0)
         numFull++;
   }
   
   if (numFull > 8) {
   
      samplePtr *flat = new samplePtr[mNumInChannels];
      for(i=0; i<mNumInChannels; i++)
         flat[i] = NewSamples(numFull * mBufferSize, mFormat);
      
      flatLen = 0;
      for(f=0; f<numFull; f++) {
         int minID = mInID+1;
         int minIndex = 0;
         for(i=0; i<mNumInBuffers; i++)
            if (mInBuffer[i].ID > 0 &&
                mInBuffer[i].ID < minID) {
               minIndex = i;
               minID = mInBuffer[i].ID;
            }

         switch(mFormat) {
         case floatSample:
            int j;
            for(j=0; j<mInBuffer[minIndex].len; j++)
               for(c=0; c<mNumInChannels; c++) {
                  ((float *)flat[c])[flatLen+j] =
                     ((float *)mInBuffer[minIndex].data)[j*mNumInChannels + c];
               }
            break;
         default:
            wxASSERT(0);
         }

         flatLen += mInBuffer[minIndex].len;
         mInBuffer[minIndex].ID = 0;
      }
      
      for(i=0; i<mNumInChannels; i++)
         mInTracks[i]->Append(flat[i], mFormat, flatLen);

      for(i=0; i<mNumInChannels; i++)
         DeleteSamples(flat[i]);
      delete[] flat;

      mProject->RedrawProject();
   }
}

void AudioIO::OnTimer()
{
   if (!mProject)
      return;
   
   FillBuffers();
   
   if (mT1!=mT0 && mT >= mT1 && GetIndicator() >= mT1)
      Stop();
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

   Pa_AbortStream(mPortStream);
   Pa_CloseStream(mPortStream);

   mPortStream = NULL;

   if (mNumOutChannels > 0) {
      for(unsigned int i=0; i<mMaxBuffers; i++) {
         mOutBuffer[i].ID = 0;
         DeleteSamples(mOutBuffer[i].data);
         mOutBuffer[i].data = NULL;
      }
      mInitialNumOutBuffers = mNumOutBuffers;
   }

   if (mNumInChannels > 0) {
      for(unsigned int i=0; i<mMaxBuffers; i++) {
         mInBuffer[i].ID = 0;
         DeleteSamples(mInBuffer[i].data);
         mInBuffer[i].data = NULL;
      }
      delete[] mInTracks;
      mInTracks = NULL;
      
      if (!mHardStop)
         mProject->TP_PushState("Recorded Audio");
   }

   mProject->SetStop(true);
   mProject->ReReadSettings();
   mProject->HandleResize();

   mProject = NULL;
   mHardStop = false;
   
   if (mInUnderruns) {
      wxString str;
      str.Printf(_("There were %d buffer underruns, data was lost."),
                mInUnderruns);
      wxMessageBox(str);
   }
   if (mRepeats) {
      wxString str;
      str.Printf(_("There were %d possibly repeated frames, "
                   "last near %lf seconds."),
                 mRepeats, mRepeatPoint);
      wxMessageBox(str);
   }
}

void AudioIO::HardStop()
{
   mProject = NULL;
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

void AudioIOTimer::Notify()
{
   gAudioIO->OnTimer();
}

AudacityProject *AudioIO::GetProject()
{
   return mProject;
}

double AudioIO::GetIndicator()
{
   if (mProject && mPortStream)
      return mT0 + (Pa_StreamTime(mPortStream)/ mRate);
   else
      return -1000000000.0;
}
