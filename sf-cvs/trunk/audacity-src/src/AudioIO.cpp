/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.cpp

  Dominic Mazzoni

  Use the PortAudio library to play and record sound

**********************************************************************/

#include <wx/textctrl.h>
#include <wx/msgdlg.h>
#include <wx/timer.h>
#include <wx/intl.h>

#include "AudioIO.h"
#include "Project.h"
#include "Track.h"
#include "WaveTrack.h"
#include "Mix.h"
#include "APalette.h"
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
   mMaxBuffers = 24;
   mInitialNumOutBuffers = 4;

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
   int minIndex, minID = 0;
   int i;
   
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
      
         memcpy(outputBuffer, b->data, len * numOutChannels * sizeof(sampleType));

         // Fill rest of buffer with silence
         if (len < framesPerBuffer)
            for(i=len*numOutChannels; i<framesPerBuffer*numOutChannels; i++)
               ((sampleType *)outputBuffer)[i] = 0;
         
         b->ID = 0;
      }
      else {
         // we had a buffer underrun!
         
         // play silence
         for(i=0; i<framesPerBuffer * numOutChannels; i++)
            ((sampleType *)outputBuffer)[i] = 0;
         
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
            
            AudioIOBuffer *b = &gAudioIO->mInBuffer[i];
            memcpy(b->data, inputBuffer, len * numInChannels * sizeof(sampleType));
            b->len = len;
            b->ID = gAudioIO->mInID;
            gAudioIO->mInID++;
            found = true;
            
            int checksum = 0;
            for(int k=0; k<len*numInChannels; k++)
               checksum += ((sampleType *)inputBuffer)[k];
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
   wxString        recDevice;
   wxString        playDevice;

   numPortAudioBuffers = Pa_GetMinNumBuffers(mBufferSize, mRate);

   recDeviceNum = Pa_GetDefaultInputDeviceID();
   playDeviceNum = Pa_GetDefaultOutputDeviceID();

   recDevice = gPrefs->Read("/AudioIO/RecordingDevice", "");
   playDevice = gPrefs->Read("/AudioIO/PlaybackDevice", "");

   for(int j=0; j<Pa_CountDevices(); j++) {
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      if (info->name == playDevice)
         playDeviceNum = j;
      if (info->name == recDevice)
         recDeviceNum = j;
   }

   if (mNumOutChannels<=0)
      playDeviceNum = paNoDevice;
   if (mNumInChannels<=0)
      recDeviceNum = paNoDevice;

   error = Pa_OpenStream(&mPortStream,
                         recDeviceNum,
                         mNumInChannels,
                         paInt16,
                         NULL, /* inputDriverInfo */
                         playDeviceNum,
                         mNumOutChannels,
                         paInt16,
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

   int i;

   if (mNumInChannels > 0) {
      mInBuffer = new AudioIOBuffer[mMaxBuffers];
      for(i=0; i<mMaxBuffers; i++) {
         mInBuffer[i].ID = 0;   // means it's empty
         mInBuffer[i].len = 0;
         mInBuffer[i].data = new sampleType[mBufferSize*mNumInChannels];
      }
   }
   else
      mInBuffer = NULL;

   if (mNumOutChannels > 0) {
      mOutBuffer = new AudioIOBuffer[mMaxBuffers];
      for(i=0; i<mMaxBuffers; i++) {
         mOutBuffer[i].ID = 0;   // means it's empty
         mOutBuffer[i].len = 0;
         mOutBuffer[i].data = new sampleType[mBufferSize*mNumOutChannels];
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
   
   project->ReReadSettings();

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

   mNumInChannels = stereo? 2: 1;
   mNumInBuffers = mMaxBuffers;

   mNumOutChannels = duplex? 2: 0;
   mNumOutBuffers = duplex? mInitialNumOutBuffers: 0;
   
   mProject->SelectNone();

   mInTracks = new WaveTrack*[mNumInChannels];
   for(int i=0; i<mNumInChannels; i++) {
      mInTracks[i] = new WaveTrack(project->GetDirManager());
      mInTracks[i]->SetSelected(true);
      mInTracks[i]->SetOffset(mT0);
      if (stereo)
         mInTracks[i]->SetChannel(i==0? VTrack::LeftChannel : VTrack::RightChannel);
      else
         mInTracks[i]->SetChannel(VTrack::MonoChannel);
      if (stereo && i==0)
         mInTracks[i]->SetLinked(true);
      mInTracks[i]->SetRate(mRate);
      
      mTracks->Add(mInTracks[i]);
   }

   project->ReReadSettings();
   mProject->HandleResize();

   return Start();
}

void AudioIO::FillBuffers()
{
   unsigned int numEmpty = 0;
   int i;
   
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
      
      Mixer *mixer = new Mixer(mNumOutChannels, block, true, mRate);
      mixer->UseVolumeSlider(mProject->GetAPalette());
      mixer->Clear();

      TrackListIterator iter2(mTracks);
      int numSolo = 0;
      VTrack *vt = iter2.First();
      while (vt) {
         if (vt->GetKind() == VTrack::Wave && vt->GetSolo())
            numSolo++;
         vt = iter2.Next();
      }

      TrackListIterator iter(mTracks);
      vt = iter.First();
      while (vt) {
         if (vt->GetKind() == VTrack::Wave) {      

            VTrack *mt = vt;
         
            // We want to extract mute and solo information from
            // the top of the two tracks if they're linked
            // (i.e. a stereo pair only has one set of mute/solo buttons)
            VTrack *partner = mTracks->GetLink(vt);
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
            case VTrack::LeftChannel:
               mixer->MixLeft(t, mT, mT + deltat);
               break;
            case VTrack::RightChannel:
               mixer->MixRight(t, mT, mT + deltat);
               break;
            case VTrack::MonoChannel:
               mixer->MixMono(t, mT, mT + deltat);
               break;
            }
         }

         vt = iter.Next();
      }     
   
      // Copy the mixed samples into the buffers

      sampleType *outbytes = mixer->GetBuffer();   
      for(i=0; i<mNumOutBuffers && block>0; i++)
         if (mOutBuffer[i].ID == 0) {
            sampleCount count;
            if (block > mBufferSize)
               count = mBufferSize;
            else
               count = block;
            
            memcpy(mOutBuffer[i].data, outbytes, count*mNumOutChannels*sizeof(sampleType));
            block -= count;
            outbytes += (count*mNumOutChannels);
            mOutBuffer[i].len = count;
            mOutBuffer[i].ID = mOutID;
            mOutID++;
         }

      delete mixer;

      mT += deltat;
   }
   
   // Recording buffers
   
   int numFull = 0;
   int j, f, c;
   sampleCount flatLen;
      
   for(i=0; i<mNumInBuffers; i++) {
      if (mInBuffer[i].ID != 0)
         numFull++;
   }
   
   if (numFull > 8) {
   
      sampleType **flat = new sampleType*[mNumInChannels];
      for(i=0; i<mNumInChannels; i++)
         flat[i] = new sampleType[numFull * mBufferSize];
      
      flatLen = 0;
      for(f=0; f<numFull; f++) {
         int minID = mInID+1;
         int minIndex;
         for(i=0; i<mNumInBuffers; i++)
            if (mInBuffer[i].ID > 0 &&
                mInBuffer[i].ID < minID) {
               minIndex = i;
               minID = mInBuffer[i].ID;
            }

         if (minID == mInID+1) {    ///////
            minID = mInID+1;       /////////
         }                         /////////
         
         for(j=0; j<mInBuffer[minIndex].len; j++)
            for(c=0; c<mNumInChannels; c++) {
               flat[c][flatLen+j] = mInBuffer[minIndex].data[j*mNumInChannels + c];
            }
         flatLen += mInBuffer[minIndex].len;
         mInBuffer[minIndex].ID = 0;
      }
      
      for(i=0; i<mNumInChannels; i++)
         mInTracks[i]->Append(flat[i], flatLen);

      for(i=0; i<mNumInChannels; i++)
         delete[] flat[i];
      delete[] flat;

      mProject->RedrawProject();
   }
}

void AudioIO::OnTimer()
{
   if (!mProject)
      return;
   
   FillBuffers();
   
   if (mT1!=mT0 && mT >= mT1 && GetIndicator() >= mT1) {
      Stop();
      return;
   }
}

void AudioIO::Stop()
{
   if (!mProject)
      return;

   if (!mHardStop) {
      mProject->GetAPalette()->SetPlay(false);
      mProject->GetAPalette()->SetStop(false);
      mProject->GetAPalette()->SetRecord(false);
   }

   Pa_AbortStream(mPortStream);
   Pa_CloseStream(mPortStream);

   mPortStream = NULL;

   if (mNumOutChannels > 0) {
      for(int i=0; i<mMaxBuffers; i++) {
         mOutBuffer[i].ID = 0;
         delete [] mOutBuffer[i].data;
         mOutBuffer[i].data = NULL;
      }
      mInitialNumOutBuffers = mNumOutBuffers;
   }

   if (mNumInChannels > 0) {
      for(int i=0; i<mMaxBuffers; i++) {
         mInBuffer[i].ID = 0;
         delete [] mInBuffer[i].data;
         mInBuffer[i].data = NULL;
      }
      delete[] mInTracks;
      mInTracks = NULL;
      
      if (!mHardStop)
         mProject->TP_PushState("Recorded Audio");
   }

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

bool AudioIO::IsBusy()
{
   return (mProject != NULL);
}

bool AudioIO::IsPlaying()
{
   return (mProject != NULL && mNumOutChannels > 0);
}

bool AudioIO::IsRecording(VTrack *t)
{
   bool recording = (mProject != NULL && mNumInChannels > 0);

   if (t && recording)
      for (int i = 0; i < mNumInChannels; i++)
         if (mInTracks[i] == t)
            return true;

   return recording;
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
