/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.cpp

  Dominic Mazzoni
  Joshua Haberman

  Use the PortAudio library to play and record sound

  Great care and attention to detail are necessary for understanding and
  modifying this system.  The code in this file is run from three
  different thread contexts: the UI thread, the disk thread (which
  this file creates and maintains) and the PortAudio callback thread.
  To highlight this deliniation, the file is divided into three parts
  based on what thread context each function is intended to run in.

  TODO: run through all functions called from audio and portaudio threads
  to verify they are thread-safe.

**********************************************************************/

#include <iostream>
#include <math.h>
#include <stdlib.h>

#include <wx/log.h>
#include <wx/textctrl.h>
#include <wx/msgdlg.h>
#include <wx/timer.h>
#include <wx/intl.h>

#include "AudioIO.h"
#include "WaveTrack.h"
#include "Mix.h"
#include "RingBuffer.h"
#include "Prefs.h"
#include "TimeTrack.h"

#if USE_PORTMIXER
#include "MixerToolBar.h"
#endif

AudioIO *gAudioIO;

#if USE_PORTAUDIO_V19
int audacityAudioCallback(void *inputBuffer, void *outputBuffer,
                          unsigned long framesPerBuffer,
                          const PaStreamCallbackTimeInfo *timeInfo,
                          const PaStreamCallbackFlags statusFlags, void *userData );
#else
int audacityAudioCallback(void *inputBuffer, void *outputBuffer,
                          unsigned long framesPerBuffer,
                          PaTimestamp outTime, void *userData );
#endif

//////////////////////////////////////////////////////////////////////
//
//     UI Thread Context
//
//////////////////////////////////////////////////////////////////////

void InitAudioIO()
{
   gAudioIO = new AudioIO();
   gAudioIO->mThread->Run();
}

void DeinitAudioIO()
{
   delete gAudioIO;
}

AudioIO::AudioIO()
{
   mAudioThreadShouldCallFillBuffersOnce = false;
   mAudioThreadFillBuffersLoopRunning = false;
#if USE_PORTAUDIO_V19
   mPortStreamV19 = NULL;
#else
   mPortStreamV18 = NULL;
   mInCallbackFinishedState = false;
#endif
   mStreamToken = 0;
   mTempFloats = new float[65536]; // TODO: out channels * PortAudio buffer size

   mNumCaptureChannels = 0;
   mPaused = false;

   PaError err = Pa_Initialize();

   if (err != paNoError) {
      wxString errStr = _("There was an error initializing the audio i/o layer.\n");
      errStr += _("You will not be able to play or record audio.\n\n");
      wxString paErrStr = Pa_GetErrorText(err);
      if (paErrStr)
         errStr += "Error: "+paErrStr;
      // XXX: we are in libaudacity, popping up dialogs not allowed!  A
      // long-term solution will probably involve exceptions
      wxMessageBox(errStr, "Error Initializing Audio", wxICON_ERROR|wxOK);

      // Since PortAudio is not initialized, all calls to PortAudio
      // functions will fail.  This will give reasonable behavior, since
      // the user will be able to do things not relating to audio i/o,
      // but any attempt to play or record will simply fail.
   }

   // Start thread
   mThread = new AudioThread();
   mThread->Create();
}

AudioIO::~AudioIO()
{
   Pa_Terminate();

   /* Delete is a "graceful" way to stop the thread.
      (Kill is the not-graceful way.) */
   mThread->Delete();

   delete [] mTempFloats;
   delete mThread;
}

void AudioIO::AdjustMixer()
{
#if USE_PORTMIXER
#ifndef USE_PORTAUDIO_V19 // PortMixer doesn't support v19 yet...

   MixerToolBar *mixerToolbar = GetCurrentMixerToolBar();
   PxMixer *mixer = mPortMixer;

   if (mixerToolbar && mixer) {
      if (mNumPlaybackChannels > 0) {
         Px_SetPCMOutputVolume(mixer, mixerToolbar->GetOutputVol());
      }
      if (mNumCaptureChannels > 0) {
         Px_SetCurrentInputSource(mixer, mixerToolbar->GetInputSource());
         Px_SetInputVolume(mixer, mixerToolbar->GetInputVol());
      }
   }

#endif
#endif
}

int AudioIO::StartStream(WaveTrackArray playbackTracks,
                         WaveTrackArray captureTracks,
                         TimeTrack *timeTrack, double sampleRate,
                         double t0, double t1)
{
   if( IsStreamActive() )
      return false;

   mRate    = sampleRate;
   mT0      = t0;
   mT       = t0;
   mT1      = t1;
   mPaused  = false;
   mPlaybackTracks = playbackTracks;
   mCaptureTracks  = captureTracks;
   mTotalSamplesPlayed = 0;
   mPausedSeconds = 0;

   //
   // Attempt to open the device using the given parameters.  If we can't, it's
   // pointless to proceed any further
   //

#if USE_PORTAUDIO_V19
   PaStreamParameters *playbackParameters;
   PaStreamParameters *captureParameters;

   if( playbackTracks.GetCount() > 0 )
   {
      // For playback, everything gets mixed down to stereo
      mNumPlaybackChannels = 2;
      playbackParameters = new PaStreamParameters;
      const PaDeviceInfo *playbackDeviceInfo;

      playbackParameters->device = gPrefs->Read("/AudioIO/PlaybackDevice",
                                                Pa_GetDefaultOutputDevice() );

      playbackDeviceInfo = Pa_GetDeviceInfo( playbackParameters->device );

      if( playbackDeviceInfo == NULL )
          return false;

      // regardless of source formats, we always mix to float
      playbackParameters->sampleFormat = paFloat32;
      playbackParameters->hostApiSpecificStreamInfo = NULL;
      playbackParameters->channelCount = mNumPlaybackChannels;
      playbackParameters->suggestedLatency =
         playbackDeviceInfo->defaultLowOutputLatency;
   }
   else
   {
      playbackParameters = NULL;
   }

   if( captureTracks.GetCount() > 0 )
   {
      // For capture, every input channel gets its own track
      mNumCaptureChannels = mCaptureTracks.GetCount();

      // I don't deal with the possibility of the capture tracks
      // having different sample formats, since it will never happen
      // with the current code.  This code wouldn't *break* if this
      // assumption was false, but it would be sub-optimal.  For example,
      // if the first track was 16-bit and the second track was 24-bit,
      // we would set the sound card to capture in 16 bits and the second
      // track wouldn't get the benefit of all 24 bits the card is capable
      // of.
      mCaptureFormat = mCaptureTracks[0]->GetSampleFormat();
      captureParameters = new PaStreamParameters;
      const PaDeviceInfo *captureDeviceInfo;

      captureParameters->device = gPrefs->Read("/AudioIO/RecordingDevice",
                                               Pa_GetDefaultInputDevice() );

      captureDeviceInfo = Pa_GetDeviceInfo( captureParameters->device );

      if( captureDeviceInfo == NULL )
          return false;

      // capture in the requested format
      switch( mCaptureFormat )
      {
         case int16Sample:
            captureParameters->sampleFormat = paInt16;
            break;

         case int24Sample:
            captureParameters->sampleFormat = paInt24;
            break;

         case floatSample:
            captureParameters->sampleFormat = paFloat32;
            break;
      }

      captureParameters->hostApiSpecificStreamInfo = NULL;
      captureParameters->channelCount = mNumCaptureChannels;
      captureParameters->suggestedLatency =
         captureDeviceInfo->defaultHighInputLatency;
   }
   else
   {
      captureParameters = NULL;
      mCaptureFormat = 0;
   }

   PaError err = Pa_OpenStream( &mPortStreamV19,
                                captureParameters, playbackParameters,
                                mRate, paFramesPerBufferUnspecified,
                                paNoFlag,
                                audacityAudioCallback, NULL );

#if 0 //USE_PORTMIXER  TODO: support PortMixer with v19
   mPortMixer = NULL;
   if (mPortStream != NULL && error == paNoError) {
      mPortMixer = Px_OpenMixer(mPortStream, 0);
      if (mPortMixer)
         AdjustMixer();
   }
#endif

   // these may be null, but deleting a null pointer should never crash.
   delete captureParameters;
   delete playbackParameters;

#else

   PaDeviceID captureDevice, playbackDevice;
   PaSampleFormat paCaptureFormat = floatSample;

   if( playbackTracks.GetCount() > 0 )
   {
      // For playback, everything gets mixed down to stereo
      mNumPlaybackChannels = 2;

      playbackDevice =  Pa_GetDefaultOutputDeviceID();
      wxString playbackDeviceName = gPrefs->Read("/AudioIO/PlaybackDevice", "");

      if( playbackDeviceName != "" )
      {
         for( int i = 0; i < Pa_CountDevices(); i++)
         {
            const PaDeviceInfo* info = Pa_GetDeviceInfo(i);
            if (info->name == playbackDeviceName && info->maxOutputChannels > 0)
               playbackDevice = i;
         }
      }
   }
   else
   {
      playbackDevice = paNoDevice;
      mNumPlaybackChannels = 0;
   }


   if( captureTracks.GetCount() > 0 )
   {
      // For capture, every input channel gets its own track
      mNumCaptureChannels = mCaptureTracks.GetCount();

      // I don't deal with the possibility of the capture tracks
      // having different sample formats, since it will never happen
      // with the current code.  This code wouldn't *break* if this
      // assumption was false, but it would be sub-optimal.  For example,
      // if the first track was 16-bit and the second track was 24-bit,
      // we would set the sound card to capture in 16 bits and the second
      // track wouldn't get the benefit of all 24 bits the card is capable
      // of.
      mCaptureFormat = mCaptureTracks[0]->GetSampleFormat();
      captureDevice =  Pa_GetDefaultInputDeviceID();
      wxString captureDeviceName = gPrefs->Read("/AudioIO/RecordingDevice", "");

      if( captureDeviceName != "" )
      {
         for( int i = 0; i < Pa_CountDevices(); i++)
         {
            const PaDeviceInfo* info = Pa_GetDeviceInfo(i);
            if (info->name == captureDeviceName && info->maxInputChannels > 0)
               captureDevice = i;
         }
      }

      // capture in the requested format
      switch( mCaptureFormat )
      {
         case int16Sample:
            paCaptureFormat = paInt16;
            break;

         case int24Sample:
            paCaptureFormat = paInt24;
            break;

         case floatSample:
            paCaptureFormat = paFloat32;
            break;
      }
   }
   else
   {
      captureDevice = paNoDevice;
      mNumCaptureChannels = 0;
      mCaptureFormat = (sampleFormat)0;
   }

   PaError err = Pa_OpenStream( &mPortStreamV18,
                                /* capture parameters */
                                captureDevice,
                                mNumCaptureChannels,
                                paCaptureFormat,
                                NULL,
                                /* playback parameters */
                                playbackDevice,
                                mNumPlaybackChannels,
                                paFloat32,
                                NULL,
                                /* general parameters */
                                mRate, 256, 0,
                                paClipOff | paDitherOff,
                                audacityAudioCallback, NULL );

#if USE_PORTMIXER
   mPortMixer = NULL;
   if (mPortStreamV18 != NULL && err == paNoError) {
      mPortMixer = Px_OpenMixer(mPortStreamV18, 0);
      if (mPortMixer)
         AdjustMixer();
   }
#endif

   mInCallbackFinishedState = false;

#endif

   if( err != paNoError )
   {
      // we'll need a more complete way to indicate error
      printf("%s\n", Pa_GetErrorText(err));
      return 0;
   }

   //
   // The stream has successfully been opened.  We now proceed in allocating
   // the memory structures the stream will need.
   //

   if( mNumPlaybackChannels > 0 )
   {
      // Allocate output buffers.  For every output track we allocate
      // a ring buffer of five seconds
      sampleCount playbackBufferSize = (sampleCount)(mRate * 5);
      mPlaybackBuffers = new RingBuffer* [mPlaybackTracks.GetCount()];
      mPlaybackMixers  = new Mixer*      [mPlaybackTracks.GetCount()];

      for( unsigned int i = 0; i < mPlaybackTracks.GetCount(); i++ )
      {
         mPlaybackBuffers[i] = new RingBuffer(floatSample, playbackBufferSize);

         mPlaybackMixers[i]  = new Mixer(1, &mPlaybackTracks[i],
                                         timeTrack, mT0, mT1, 1,
                                         playbackBufferSize, false,
                                         mRate, floatSample, false);
         mPlaybackMixers[i]->ApplyTrackGains(false);
      }
   }

   if( mNumCaptureChannels > 0 )
   {
      // Allocate input buffers.  For every input track we allocate
      // a ring buffer of five seconds
      sampleCount captureBufferSize = (sampleCount)(mRate * 5);
      mCaptureBuffers = new RingBuffer* [mCaptureTracks.GetCount()];

      for( unsigned int i = 0; i < mCaptureTracks.GetCount(); i++ )
         mCaptureBuffers[i] = new RingBuffer( mCaptureTracks[i]->GetSampleFormat(),
                                              captureBufferSize );
   }

   // We signal the audio thread to call FillBuffers, to prime the RingBuffers
   // so that they will have data in them when the stream starts.  Having the
   // audio thread call FillBuffers here makes the code more predictable, since
   // FillBuffers will ALWAYS get called from the Audio thread.
   mAudioThreadShouldCallFillBuffersOnce = true;

   while( mAudioThreadShouldCallFillBuffersOnce == true )
      wxUsleep( 50 );

   // Now start the PortAudio stream!
#if USE_PORTAUDIO_V19
   err = Pa_StartStream( mPortStreamV19 );
#else
   err = Pa_StartStream( mPortStreamV18 );
#endif

   if( err != paNoError )
   {
      // TODO
      // we'll need a more complete way to indicate error.
      // AND we need to delete the ring buffers and mixers, etc.
      printf("%s\n", Pa_GetErrorText(err));
      return 0;
   }

   mAudioThreadFillBuffersLoopRunning = true;
   mStreamToken = rand();

#if USE_PORTAUDIO_V19
   // To make GetStreamTime behave correctly before the callback sets
   // mLastBufferAudibleTime the first time, we make a rough guess
   // TODO: guess better
   mLastBufferAudibleTime = Pa_GetStreamTime( mPortStreamV19 ) + (2048 / mRate);
#endif

   return mStreamToken;
}

void AudioIO::StopStream()
{
#if USE_PORTAUDIO_V19
   if( mPortStreamV19 == NULL || mStreamToken == 0 )
      return;

   if( Pa_IsStreamStopped( mPortStreamV19 ) )
      return;
#else
   if( mPortStreamV18 == NULL || mStreamToken == 0 )
      return;

   if( IsStreamActive() == false && mInCallbackFinishedState == false )
      return;
#endif

   //
   // We got here in one of two ways:
   //
   // 1. The user clicked the stop button and we therefore want to stop
   //    as quickly as possible.  So we use AbortStream().  If this is
   //    the case the portaudio stream is still in the Running state
   //    (see PortAudio state machine docs).
   //
   // 2. The callback told PortAudio to stop the stream since it had
   //    reached the end of the selection.  The UI thread discovered
   //    this by noticing that AudioIO::IsActive() returned false.
   //    IsActive() (which calls Pa_GetStreamActive()) will not return
   //    false until all buffers have finished playing, so we can call
   //    AbortStream without losing any samples.  If this is the case
   //    we are in the "callback finished state" (see PortAudio state
   //    machine docs).
   //
   // The moral of the story: We can call AbortStream safely, without
   // losing samples.
   //

   mAudioThreadFillBuffersLoopRunning = false;

#if USE_PORTAUDIO_V19
   Pa_AbortStream( mPortStreamV19 );
   Pa_CloseStream( mPortStreamV19 );
   mPortStreamV19 = NULL;
#else
   Pa_AbortStream( mPortStreamV18 );
   Pa_CloseStream( mPortStreamV18 );
   mPortStreamV18 = NULL;
   mInCallbackFinishedState = false;
#endif

   mStreamToken = 0;

   // In either of the above cases, we want to make sure that any
   // capture data that made it into the PortAudio callback makes it
   // to the target WaveTrack.  To do this, we ask the audio thread to
   // call FillBuffers one last time (it normally would not do so since
   // Pa_GetStreamActive() would now return false
   mAudioThreadShouldCallFillBuffersOnce = true;

   while( mAudioThreadShouldCallFillBuffersOnce == true )
   {
      wxYield();
      wxUsleep( 50 );
   }

   //
   // Everything is taken care of.  Now, just free all the resources
   // we allocated in StartStream()
   //

   if( mPlaybackTracks.GetCount() > 0 )
   {
      for( unsigned int i = 0; i < mPlaybackTracks.GetCount(); i++ )
      {
         delete mPlaybackBuffers[i];
         delete mPlaybackMixers[i];
      }

      delete[] mPlaybackBuffers;
      delete[] mPlaybackMixers;
   }

   if( mCaptureTracks.GetCount() > 0 )
   {
      for( unsigned int i = 0; i < mCaptureTracks.GetCount(); i++ )
      {
         delete mCaptureBuffers[i];
         mCaptureTracks[i]->Flush();
      }

      delete[] mCaptureBuffers;
   }
}

void AudioIO::SetPaused(bool state)
{
   if(state)
   {
      // When we are beginning a pause, we note the time so that GetStreamTime
      // can always return this position while we're paused
      mPausePosition = GetStreamTime();
   }
   else
   {
#if USE_PORTAUDIO_V19
      // When we're coming out of a pause, we guess mLastBufferAudibleTime so
      // that the indicator won't be erratic between now and when the callback
      // sets mLastBufferAudibleTime again.  This is similar to what happens
      // in StartStream
      mLastBufferAudibleTime = Pa_GetStreamTime( mPortStreamV19 ) + (2048 / mRate);
#endif
   }

   mPaused = state;
}

bool AudioIO::IsPaused()
{
   return mPaused;
}

bool AudioIO::IsStreamActive()
{
#if USE_PORTAUDIO_V19
   if( mPortStreamV19 )
      return Pa_IsStreamActive( mPortStreamV19 );
   else
      return false;
#else
   if( mPortStreamV18 &&
       Pa_StreamActive( mPortStreamV18 ) &&
       mInCallbackFinishedState == false )
      return true;
   else
      return false;
#endif
}

bool AudioIO::IsStreamActive(int token)
{
   if( IsStreamActive() && token == mStreamToken )
      return true;
   else
      return false;
}

double AudioIO::GetStreamTime()
{
   if( !IsStreamActive() )
      return -1000000000;

   if( mPaused )
      return mPausePosition;

#if USE_PORTAUDIO_V19

   PaStream *stream = mPortStreamV19;

   // Based on the number of samples we have written, this is the value
   // in time that represents how far we are through the source data
   double lastBufferTime =  mT0 + (mTotalSamplesPlayed / mRate);

   // This is the number of seconds ago that the last buffer started
   // being heard.  It will be negative if the last buffer is not
   // yet audible.
   double deltat = Pa_GetStreamTime(stream) - mLastBufferAudibleTime;

   double time = lastBufferTime + deltat;
   return time;
#else
   PaStream *stream = mPortStreamV18;

   double streamTime = Pa_StreamTime(stream);
   double indicator = mT0 + (streamTime / mRate) - mPausedSeconds;

   // Pa_StreamTime can sometimes return wacky results, so we
   // try to filter those out...
   if (fabs(indicator - mLastIndicator) > 0.1) {
      mLastIndicator = indicator;
      return mLastStableIndicator;
   }
   mLastIndicator = indicator;
   mLastStableIndicator = indicator;
   return indicator;
#endif
}


//////////////////////////////////////////////////////////////////////
//
//     Audio Thread Context
//
//////////////////////////////////////////////////////////////////////

AudioThread::AudioThread():
   wxThread(wxTHREAD_JOINABLE)
{
}

wxThread::ExitCode AudioThread::Entry()
{
   while( !TestDestroy() )
   {
      if( gAudioIO->mAudioThreadShouldCallFillBuffersOnce )
      {
         gAudioIO->FillBuffers();
         gAudioIO->mAudioThreadShouldCallFillBuffersOnce = false;
      }
      else if( gAudioIO->mAudioThreadFillBuffersLoopRunning )
      {
         gAudioIO->FillBuffers();
      }

      Sleep(10);
   }

   return 0;
}

// This method is the data gateway between the audio thread (which
// communicates with the disk) and the PortAudio callback thread
// (which communicates with the audio device.
void AudioIO::FillBuffers()
{
   unsigned int i;

   if( mPlaybackTracks.GetCount() > 0 )
   {
      // Though extremely unlikely, it is possible that some buffers
      // will have more samples available than others.  This could happen
      // if we hit this code during the PortAudio callback.  To keep
      // things simple, we only write as much data as is vacant in
      // ALL buffers, and advance the global time by that much.
      int commonlyAvail = mPlaybackBuffers[0]->AvailForPut();

      for( i = 1; i < mPlaybackTracks.GetCount(); i++ )
      {
         int thisBlockAvail = mPlaybackBuffers[i]->AvailForPut();

         if( thisBlockAvail < commonlyAvail )
            commonlyAvail = thisBlockAvail;
      }

      //
      // Now determine how much this will globally advance playback time
      //
      double deltat = commonlyAvail / mRate;

      if( mT + deltat > mT1 )
      {
         deltat = mT1 - mT;
         if( deltat < 0.0 )
            deltat = 0.0;
         commonlyAvail = (int)(deltat * mRate + 0.5);
      }
      mT += deltat;

      if( commonlyAvail > 0 )
      {
         for( i = 0; i < mPlaybackTracks.GetCount(); i++ )
         {
            // The mixer here isn't actually mixing: it's just doing whatever
            // warping is needed for the time track
            int processed = mPlaybackMixers[i]->Process(commonlyAvail);
            samplePtr warpedSamples = mPlaybackMixers[i]->GetBuffer();
            mPlaybackBuffers[i]->Put(warpedSamples, floatSample, processed);
         }
      }
   }

   if( mCaptureTracks.GetCount() > 0 )
   {
      int commonlyAvail = mCaptureBuffers[0]->AvailForGet();
      for( i = 1; i < mCaptureTracks.GetCount(); i++ )
      {
         int avail = mCaptureBuffers[i]->AvailForGet();
         if( avail < commonlyAvail )
            commonlyAvail = avail;
      }

      // For capture buffers, save everything available to disk
      for( i = 0; i < mCaptureTracks.GetCount(); i++ )
      {
         //int avail = mCaptureBuffers[i]->AvailForGet();
         int avail = commonlyAvail;
         sampleFormat trackFormat = mCaptureTracks[i]->GetSampleFormat();
         samplePtr temp = NewSamples(avail, trackFormat);

         mCaptureBuffers[i]->Get   (temp, trackFormat, avail);
         mCaptureTracks[i]-> Append(temp, trackFormat, avail);

         DeleteSamples(temp);
      }
   }
}

//////////////////////////////////////////////////////////////////////
//
//    PortAudio callback thread context
//
//////////////////////////////////////////////////////////////////////

#if USE_PORTAUDIO_V19
int audacityAudioCallback(void *inputBuffer, void *outputBuffer,
                          unsigned long framesPerBuffer,
                          const PaStreamCallbackTimeInfo *timeInfo,
                          const PaStreamCallbackFlags statusFlags, void *userData )
#else

#define paContinue 0

int audacityAudioCallback(void *inputBuffer, void *outputBuffer,
                          unsigned long framesPerBuffer,
                          PaTimestamp outTime, void *userData )
#endif
{
   int numPlaybackChannels = gAudioIO->mNumPlaybackChannels;
   int numPlaybackTracks = gAudioIO->mPlaybackTracks.GetCount();
   int numCaptureChannels = gAudioIO->mNumCaptureChannels;
   int callbackReturn = paContinue;
   float *tempFloats = gAudioIO->mTempFloats;
   unsigned int i;
   int t;

   if( gAudioIO->mPaused )
   {
      if (outputBuffer && numPlaybackChannels > 0)
      {
         ClearSamples((samplePtr)outputBuffer, floatSample,
                      0, framesPerBuffer * numPlaybackChannels);
      }

      gAudioIO->mPausedSeconds += (float)framesPerBuffer / gAudioIO->mRate;
      return paContinue;
   }

   gAudioIO->AdjustMixer();
   //printf("Callback called.\n");

   //
   // Mix and copy to PortAudio's output buffer
   //

   if( outputBuffer && (numPlaybackChannels > 0) )
   {
      bool cut = false;
      bool linkFlag = false;

      float *outputFloats = (float *)outputBuffer;
      for( i = 0; i < framesPerBuffer*numPlaybackChannels; i++)
         outputFloats[i] = 0.0;

      int numSolo = 0;
      for( t = 0; t < numPlaybackTracks; t++ )
         if( gAudioIO->mPlaybackTracks[t]->GetSolo() )
            numSolo++;

      for( t = 0; t < numPlaybackTracks; t++)
      {
         WaveTrack *vt = gAudioIO->mPlaybackTracks[t];

         if (linkFlag)
            linkFlag = false;
         else {
            cut = false;

            // Cut if somebody else is soloing
            if (numSolo>0 && !vt->GetSolo())
               cut = true;
            
            // Cut if we're muted (unless we're soloing)
            if (vt->GetMute() && !vt->GetSolo())
               cut = true;

            linkFlag = vt->GetLinked();
         }

         if (cut)
         {
            gAudioIO->mPlaybackBuffers[t]->Discard(framesPerBuffer);
            continue;
         }

         unsigned int len = (unsigned int)
            gAudioIO->mPlaybackBuffers[t]->Get((samplePtr)tempFloats, floatSample,
                                               (int)framesPerBuffer);

         // If our buffer is empty and the time indicator is past
         // the end, then we've actually finished playing the entire
         // selection.
         if (len == 0 && gAudioIO->mT >= gAudioIO->mT1 )
         {
#if USE_PORTAUDIO_V19
            callbackReturn = paComplete;
#else
            callbackReturn = 1;
            gAudioIO->mInCallbackFinishedState = true;
#endif
         }

         if (vt->GetChannel() == Track::LeftChannel ||
             vt->GetChannel() == Track::MonoChannel)
         {
            float gain = vt->GetChannelGain(0);
            for(i=0; i<len; i++)
               outputFloats[numPlaybackChannels*i] += gain*tempFloats[i];
         }

         if (vt->GetChannel() == Track::RightChannel ||
             vt->GetChannel() == Track::MonoChannel)
         {
            float gain = vt->GetChannelGain(1);
            for(i=0; i<len; i++)
               outputFloats[numPlaybackChannels*i+1] += gain*tempFloats[i];
         }
      }
   }

   //
   // Copy from PortAudio to our input buffers.
   //

   if( inputBuffer && (numCaptureChannels > 0) )
   {
      float *inputFloats = (float *)inputBuffer;
      unsigned int len = framesPerBuffer;
      for( t = 0; t < numCaptureChannels; t++) {
         unsigned int avail =
            (unsigned int)gAudioIO->mCaptureBuffers[t]->AvailForPut();
         if (avail < len)
            len = avail;
      }

      if (len < framesPerBuffer)
      {
         gAudioIO->mLostSamples += (framesPerBuffer - len);
         printf("lost %d samples\n", (int)(framesPerBuffer - len));
      }

      if (len > 0) {
         for( t = 0; t < numCaptureChannels; t++) {
            for( i = 0; i < len; i++)
               tempFloats[i] = inputFloats[numCaptureChannels*i+t];

            gAudioIO->mCaptureBuffers[t]->Put((samplePtr)tempFloats,
                                              gAudioIO->mCaptureFormat, len);
         }
      }
   }

#if USE_PORTAUDIO_V19
   gAudioIO->mTotalSamplesPlayed += framesPerBuffer;
   gAudioIO->mLastBufferAudibleTime = timeInfo->outputBufferDacTime;
#endif

   return callbackReturn;
}

