/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.cpp

  Dominic Mazzoni

  Use the PortAudio library to play and record sound

**********************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#include <wx/textdlg.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/msgdlg.h>
#endif

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
   
   // Run our timer function once every 100 ms, i.e. 10 times/sec
   mTimer.Start(100, FALSE);
}

AudioIO::~AudioIO()
{
}

int audacityAudioCallback(
		void *inputBuffer, void *outputBuffer,
		unsigned long framesPerBuffer,
		PaTimestamp outTime, void *userData )
{
   int numOutChannels = gAudioIO->mNumOutChannels;
   int minIndex, minID = 0;
   int i;
   for(i=0; i<gAudioIO->mNumBuffers; i++) {
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
      for(i=0; i<framesPerBuffer * numOutChannels; i++)
         ((sampleType *)outputBuffer)[i] = 0;
   }
   
   return 0;
}

bool AudioIO::OpenDevice()
{
   PaError         error;
 
   error = Pa_OpenDefaultStream(&mPortStream,
                                0,   // input channels
                                2,   // output channels
                                paInt16,
                                mRate,
                                (unsigned long)mBufferSize,
                                (unsigned long)2,    // number of buffers
                                audacityAudioCallback,
                                NULL);

   return (error == paNoError);
}

bool AudioIO::Start()
{
   mT = mT0;
   mID = 1;
   mNumBuffers = 2;
   mBufferSize = 4096;
   mInBuffer = NULL;
   mOutBuffer = new AudioIOBuffer[mNumBuffers];
   int i;
   for(i=0; i<mNumBuffers; i++) {
      mOutBuffer[i].ID = 0;   // means it's empty
      mOutBuffer[i].len = 0;
      mOutBuffer[i].data = new sampleType[mBufferSize*mNumOutChannels];
   }
   
   FillBuffers();

   if (!OpenDevice()) {
      wxMessageBox("Error opening audio device.\n"
                   "(Change the device in the Preferences dialog.)");

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

   mRecording = false;
   mProject = project;
   mTracks = tracks;
   mRate = project->GetRate();
   mT0 = t0;
   mT1 = t1;
   mNumInChannels = 0;
   mNumOutChannels = 2;
   
   return Start();
}

bool AudioIO::StartRecord(AudacityProject * project, TrackList * tracks, 
                         double t0, double t1)
{
   if (mProject)
      return false;

   bool recordStereo;
   gPrefs->Read("/AudioIO/RecordStereo", &recordStereo, false);
   gPrefs->Read("/AudioIO/Duplex", &mDuplex, false);

   mRecording = true;
   mProject = project;
   mTracks = tracks;
   mRate = project->GetRate();
   mT0 = t0;
   mT1 = t1;
   mNumInChannels = 2;
   if (mDuplex)
      mNumOutChannels = recordStereo? 2: 1;
   else
      mNumOutChannels = 0;

   //mInTracks =  
   
   return Start();

/*
   mRecordLeft = new WaveTrack(project->GetDirManager());
   mRecordLeft->selected = true;
   mRecordLeft->channel = (mRecordStereo ?
                           VTrack::LeftChannel : VTrack::MonoChannel);

   if (mRecordStereo) {
      mRecordLeft->linked = true;
      mRecordRight = new WaveTrack(project->GetDirManager());
      mRecordRight->selected = true;
      mRecordRight->channel = VTrack::RightChannel;
   }

   project->SelectNone();

   tracks->Add(mRecordLeft);
   if (mRecordStereo)
      tracks->Add(mRecordRight);

   mRecording = true;
   mTracks = tracks;
   mT0 = 0.0;
   mT1 = 0.0;
   mT = 0.0;
   mRecT = 0.0;

   mRecordNode.device = SND_DEVICE_AUDIO;
   mRecordNode.write_flag = SND_READ;
   mRecordNode.format.channels = mRecordStereo ? 2 : 1;
   mRecordNode.format.mode = SND_MODE_PCM;
   mRecordNode.format.bits = 16;
   mRecordNode.format.srate = project->GetRate();
   wxString deviceStr = gPrefs->Read("/AudioIO/RecordingDevice", "");
#ifdef __WXGTK__
   if (deviceStr == "")
      deviceStr = "/dev/dsp";
#endif
   strcpy(mRecordNode.u.audio.devicename, deviceStr.c_str());
   strcpy(mRecordNode.u.audio.interfacename, "");
   mRecordNode.u.audio.descriptor = 0;
   mRecordNode.u.audio.protocol = SND_COMPUTEAHEAD;
   if (mDuplex)
      mRecordNode.u.audio.latency = 6.0;
   else {
#ifdef __WXMAC__
      mRecordNode.u.audio.latency = 1.0;
#else
      mRecordNode.u.audio.latency = 0.25;
#endif
   }
   mRecordNode.u.audio.granularity = mRecordNode.u.audio.latency;

   long flags = 0;
   int err = snd_open(&mRecordNode, &flags);

   if (err) {
      wxMessageBox("Error opening audio device.\n"
                   "(Change the device in the Preferences dialog.)");

      return false;
   }

   if (mDuplex) {
      if (!OpenPlaybackDevice(project)) {
         wxMessageBox("Error opening audio device.\n"
                      "Perhaps your sound card does not support\n"
                      "simultaneous playing and recording.\n"
                      "(Change the settings in the Preferences dialog.)");

         snd_close(&mRecordNode);

         return false;
      }
   }

   mStopWatch.Start(0);

#ifdef __WXMAC__
   mStartTicks = TickCount();
#endif

   mTicks = 0;

   // Do this last because this is what signals the timer to go
   mProject = project;

   mProject->RedrawProject();
   
   return true;

*/
}


void AudioIO::FillBuffers()
{
   // Playback buffers

   unsigned int numEmpty = 0;
   int i;
   for(i=0; i<mNumBuffers; i++) {
      if (mOutBuffer[i].ID == 0)
         numEmpty++;
   }
   
   if (numEmpty == 0)
      return;

   sampleCount block = numEmpty * mBufferSize;   
   double deltat = block / mRate;
   if (mT + deltat > mT1) {
      deltat = mT1 - mT;
      block = (sampleCount)(deltat * mRate + 0.5);
   }
   
   Mixer *mixer = new Mixer(mNumOutChannels, block, true);
   mixer->UseVolumeSlider(mProject->GetAPalette());
   mixer->Clear();

   TrackListIterator iter2(mTracks);
   int numSolo = 0;
   VTrack *vt = iter2.First();
   while (vt) {
      if (vt->GetKind() == VTrack::Wave && vt->solo)
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
         if (partner && !vt->linked)
            mt = partner;
         else
            mt = vt;

         // Cut if somebody else is soloing
         if (numSolo>0 && !mt->solo) {
            vt = iter.Next();
            continue;
         }
         
         // Cut if we're muted (unless we're soloing)
         if (mt->mute && !mt->solo) {
            vt = iter.Next();
            continue;
         }

         WaveTrack *t = (WaveTrack *) vt;
         
         switch (t->channel) {
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
   for(i=0; i<mNumBuffers && block>0; i++)
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
         mOutBuffer[i].ID = mID;
         mID++;
      }

   delete mixer;

   mT += deltat;
   
   // Recording buffers
   
   // TODO
}

void AudioIO::OnTimer()
{
   if (!mProject)
      return;
   
   FillBuffers();
   
   if (!mRecording && mT >= mT1 && GetIndicator() >= mT1) {
      Stop();
      return;
   }

   /*
   if (mRecording) {
      int block = snd_poll(&mRecordNode);

      #ifdef __WXMAC__
      if (block > 22050) {
      #else
      if (block > 0) {
      #endif

         sampleType *in = new sampleType[block * 2];

         snd_read(&mRecordNode, in, block);

         if (mRecordStereo) {
            sampleType *left = new sampleType[block];
            sampleType *right = new sampleType[block];
            for (int i = 0; i < block; i++) {
               left[i] = in[2 * i];
               right[i] = in[2 * i + 1];
            }

            mRecordLeft->Append(left, block);
            mRecordRight->Append(right, block);
            delete[]left;
            delete[]right;
         } else {
            mRecordLeft->Append(in, block);
         }

         mProject->RedrawProject();

         delete[]in;
      }

      if (!mDuplex)
         return;
   }*/

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

   mHardStop = false;

   Pa_AbortStream(mPortStream);
   Pa_CloseStream(mPortStream);
   
/*
   if (mRecording) {
      if (!mHardStop)
         mProject->TP_PushState("Recorded Audio");
   }
*/

   mProject = NULL;
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
   if (mProject != NULL) {
      if (!mRecording || (mRecording && mDuplex))
         return true;
   }

   return false;
}

bool AudioIO::IsRecording()
{
   return (mProject != NULL && mRecording);
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
   return mT0 + (Pa_StreamTime(mPortStream) / mRate);
}


