/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.cpp

  Dominic Mazzoni
  Joshua Haberman
  Markus Meyer

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

#ifdef __WXMSW__
#include <malloc.h>
#endif

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

#if 0 // VU METER
#include "widgets/Meter.h"
#endif

#if USE_PORTMIXER
#include "MixerToolBar.h"
#endif

AudioIO *gAudioIO;

// static
int AudioIO::mNextStreamToken = 0;

#if USE_PORTAUDIO_V19
int audacityAudioCallback(const void *inputBuffer, void *outputBuffer,
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
//     class AudioThread - declaration and glue code
//
//////////////////////////////////////////////////////////////////////

#ifdef __WXMAC__

// On Mac OS X, it's better not to use the wxThread class.
// We use our own implementation based on pthreads instead.

#include <pthread.h>
#include <time.h>

class AudioThread {
 public:
   typedef int ExitCode;
   AudioThread() { mDestroy = false; mThread = NULL; }
   ExitCode Entry();
   void Create() {}
   void Delete() {
      mDestroy = true;
      pthread_join(mThread, NULL);
   }
   bool TestDestroy() { return mDestroy; }
   void Sleep(int ms) {
      struct timespec spec;
      spec.tv_sec = 0;
      spec.tv_nsec = ms * 1000 * 1000;
      nanosleep(&spec, NULL);
   }
   static void *callback(void *p) {
      AudioThread *th = (AudioThread *)p;
      return (void *)th->Entry();
   }
   void Run() {
      pthread_create(&mThread, NULL, callback, this);
   }
 private:
   bool mDestroy;
   pthread_t mThread;

};

#else

// The normal wxThread-derived AudioThread class for all other
// platforms:
class AudioThread : public wxThread {
 public:
   AudioThread():wxThread(wxTHREAD_JOINABLE) {}
   virtual ExitCode Entry();
};

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
   mPlayLooped = false;

   PaError err = Pa_Initialize();

   if (err != paNoError) {
      wxString errStr = _("There was an error initializing the audio i/o layer.\n");
      errStr += _("You will not be able to play or record audio.\n\n");
      wxString paErrStr = Pa_GetErrorText(err);
      if (paErrStr)
         errStr += _("Error: ")+paErrStr;
      // XXX: we are in libaudacity, popping up dialogs not allowed!  A
      // long-term solution will probably involve exceptions
      wxMessageBox(errStr, _("Error Initializing Audio"), wxICON_ERROR|wxOK);

      // Since PortAudio is not initialized, all calls to PortAudio
      // functions will fail.  This will give reasonable behavior, since
      // the user will be able to do things not relating to audio i/o,
      // but any attempt to play or record will simply fail.
   }

   // Start thread
   mThread = new AudioThread();
   mThread->Create();

   mPortMixer = NULL;

#if defined(USE_PORTMIXER) && !defined(USE_PORTAUDIO_V19)
   HandleDeviceChange();
#else
   mEmulateMixerOutputVol = true;
   mMixerOutputVol = 1.0;
   mEmulateMixerInputVol = true;
   mMixerInputVol = 1.0;
#endif
}

AudioIO::~AudioIO()
{
#if defined(USE_PORTMIXER) && !defined(USE_PORTAUDIO_V19)
   if( mPortMixer )
      Px_CloseMixer(mPortMixer);
   mPortMixer = NULL;
#endif
   Pa_Terminate();

   /* Delete is a "graceful" way to stop the thread.
      (Kill is the not-graceful way.) */
   wxYield();
   mThread->Delete();

   delete [] mTempFloats;
   delete mThread;
}

void AudioIO::SetMixer(int recordDevice, float recordVolume,
                       float playbackVolume)
{
   mMixerOutputVol = playbackVolume;
   mMixerInputVol = recordVolume;

#if defined(USE_PORTMIXER) && !defined(USE_PORTAUDIO_V19)

   PxMixer *mixer = mPortMixer;

   if( mixer )
   {
      int oldRecordDevice = Px_GetCurrentInputSource(mixer);
      float oldRecordVolume = Px_GetInputVolume(mixer);
      float oldPlaybackVolume = Px_GetPCMOutputVolume(mixer);

      if( recordDevice != oldRecordDevice )
         Px_SetCurrentInputSource(mixer, recordDevice);
      if( fabs(oldRecordVolume-recordVolume) > 0.05 )
         Px_SetInputVolume(mixer, recordVolume);
      if( fabs(oldPlaybackVolume-playbackVolume) > 0.05 )
         Px_SetPCMOutputVolume(mixer, playbackVolume);

      return;
   }
#endif
}

void AudioIO::GetMixer(int *recordDevice, float *recordVolume,
                       float *playbackVolume)
{
#if defined(USE_PORTMIXER) && !defined(USE_PORTAUDIO_V19)

   PxMixer *mixer = mPortMixer;

   if( mixer )
   {
      *recordDevice = Px_GetCurrentInputSource(mixer);

      if (mEmulateMixerInputVol)
         *recordVolume = mMixerInputVol;
      else
         *recordVolume = Px_GetInputVolume(mixer);

      if (mEmulateMixerOutputVol)
         *playbackVolume = mMixerOutputVol;
      else
         *playbackVolume = Px_GetPCMOutputVolume(mixer);

      return;
   }

#endif

   *recordDevice = 0;
   *recordVolume = mMixerInputVol;
   *playbackVolume = mMixerOutputVol;
}

wxArrayString AudioIO::GetInputSourceNames()
{
#if defined(USE_PORTMIXER) && !defined(USE_PORTAUDIO_V19)

   wxArrayString deviceNames;

   if( mPortMixer )
   {
      int numSources = Px_GetNumInputSources(mPortMixer);
      for( int source = 0; source < numSources; source++ )
         deviceNames.Add(Px_GetInputSourceName(mPortMixer, source));
   }

   return deviceNames;

#else

   wxArrayString blank;

   return blank;

#endif
}

void AudioIO::HandleDeviceChange()
{
   // This should not happen, but it would screw things up if it did.
   if( IsBusy() )
      return;

#if defined(USE_PORTMIXER) && !defined(USE_PORTAUDIO_V19)

   if( mPortMixer )
      Px_CloseMixer(mPortMixer);
   mPortMixer = NULL;

   int recDeviceNum = Pa_GetDefaultInputDeviceID();
   int playDeviceNum = Pa_GetDefaultOutputDeviceID();
   wxString recDevice = gPrefs->Read("/AudioIO/RecordingDevice", "");
   wxString playDevice = gPrefs->Read("/AudioIO/PlaybackDevice", "");
   int j;
   
   // msmeyer: This tries to open the device with the highest samplerate
   // available on this device, using 44.1kHz as the default, if the info
   // cannot be fetched.

   for(j=0; j<Pa_CountDevices(); j++) {
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);

      if (info->name == playDevice && info->maxOutputChannels > 0)
         playDeviceNum = j;

      if (info->name == recDevice && info->maxInputChannels > 0)
         recDeviceNum = j;
   }
   
   wxArrayLong supportedSampleRates = GetSupportedSampleRates(playDevice, recDevice);
   int highestSampleRate = supportedSampleRates[supportedSampleRates.GetCount() - 1];

   PortAudioStream *stream;
   int error;
   error = Pa_OpenStream(&stream, recDeviceNum, 2, paFloat32, NULL,
                         playDeviceNum, 2, paFloat32, NULL,
                         highestSampleRate, 512, 1, paClipOff | paDitherOff,
                         audacityAudioCallback, NULL);

   if( error ) {
      error = Pa_OpenStream(&stream, recDeviceNum, 2, paFloat32, NULL,
                            paNoDevice, 0, paFloat32, NULL,
                            highestSampleRate, 512, 1, paClipOff | paDitherOff,
                            audacityAudioCallback, NULL);
   }

   if( error ) {
      mEmulateMixerInputVol = true;
      mEmulateMixerOutputVol = true;
      mMixerInputVol = 1.0;
      mMixerOutputVol = 1.0;

      return;
   }

   mPortMixer = Px_OpenMixer(stream, 0);
   Pa_CloseStream(stream);

   // Determine mixer capabilities - it it doesn't support either
   // input or output, we emulate them (by multiplying this value
   // by all incoming/outgoing samples)

   mMixerOutputVol = Px_GetPCMOutputVolume(mPortMixer);
   mEmulateMixerOutputVol = false;
   Px_SetPCMOutputVolume(mPortMixer, 0.0);
   if (Px_GetPCMOutputVolume(mPortMixer) > 0.05)
      mEmulateMixerOutputVol = true;
   Px_SetPCMOutputVolume(mPortMixer, 1.0);
   if (Px_GetPCMOutputVolume(mPortMixer) < 0.95)
      mEmulateMixerOutputVol = true;
   Px_SetPCMOutputVolume(mPortMixer, mMixerOutputVol);

   mMixerInputVol = Px_GetInputVolume(mPortMixer);
   mEmulateMixerInputVol = false;
   Px_SetInputVolume(mPortMixer, 0.0);
   if (Px_GetInputVolume(mPortMixer) > 0.05)
      mEmulateMixerInputVol = true;
   Px_SetInputVolume(mPortMixer, 1.0);
   if (Px_GetInputVolume(mPortMixer) < 0.95)
      mEmulateMixerInputVol = true;
   Px_SetInputVolume(mPortMixer, mMixerInputVol);

   #if 0
   printf("PortMixer: Output: %s Input: %s\n",
          mEmulateMixerOutputVol? "emulated": "native",
          mEmulateMixerInputVol? "emulated": "native");
   #endif

   mMixerInputVol = 1.0;
   mMixerOutputVol = 1.0;

#endif
}

int AudioIO::StartStream(WaveTrackArray playbackTracks,
                         WaveTrackArray captureTracks,
                         TimeTrack *timeTrack, double sampleRate,
                         double t0, double t1, bool playLooped /* = false */)
{
   if( IsBusy() )
      return 0;

   // We just want to set mStreamToken to -1 - this way avoids
   // an extremely rare but possible race condition, if two functions
   // somehow called StartStream at the same time...
   mStreamToken--;
   if (mStreamToken != -1)
      return 0;

   mRate    = sampleRate;
   mT0      = t0;
   mT       = t0;
   mT1      = t1;
   mPlaybackTracks = playbackTracks;
   mCaptureTracks  = captureTracks;
   mTotalSamplesPlayed = 0;
   mPlayLooped = playLooped;

#ifndef USE_PORTAUDIO_V19
   mPausedSeconds = 0;
#endif

   //
   // The RingBuffer sizes, and the max amount of the buffer to
   // fill at a time, both grow linearly with the number of
   // tracks.  This allows us to scale up to many tracks without
   // killing performance.
   //

   mPlaybackRingBufferSecs = 4.5 + (0.5 * mPlaybackTracks.GetCount());
   mMaxPlaybackSecsToCopy = 0.75 + (0.25 * mPlaybackTracks.GetCount());

   mCaptureRingBufferSecs = 4.5 + 0.5 * mCaptureTracks.GetCount();   
   mMinCaptureSecsToCopy = 0.2 + (0.2 * mCaptureTracks.GetCount());

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
      wxString playbackDeviceName = gPrefs->Read("/AudioIO/PlaybackDevice", "");
      const PaDeviceInfo *playbackDeviceInfo;

      playbackParameters->device = Pa_GetDefaultOutputDevice();

      if( playbackDeviceName != "" )
      {
         for( int i = 0; i < Pa_GetDeviceCount(); i++)
         {
            const PaDeviceInfo* info = Pa_GetDeviceInfo(i);
            if (info->name == playbackDeviceName && info->maxOutputChannels > 0)
               playbackParameters->device = i;
         }
      }

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
      wxString captureDeviceName = gPrefs->Read("/AudioIO/RecordingDevice", "");

      captureParameters->device = Pa_GetDefaultInputDevice();

      if( captureDeviceName != "" )
      {
         for( int i = 0; i < Pa_GetDeviceCount(); i++)
         {
            const PaDeviceInfo* info = Pa_GetDeviceInfo(i);
            if (info->name == captureDeviceName && info->maxInputChannels > 0)
               captureParameters->device = i;
         }
      }

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
   if( mPortMixer )
      Px_CloseMixer(mPortMixer);
   mPortMixer = Px_OpenMixer(mPortStreamV18, 0);
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
      sampleCount playbackBufferSize =
         (sampleCount)(mRate * mPlaybackRingBufferSecs + 0.5);
      sampleCount playbackMixBufferSize = 
         (sampleCount)(mRate * mMaxPlaybackSecsToCopy + 0.5);
      mPlaybackBuffers = new RingBuffer* [mPlaybackTracks.GetCount()];
      mPlaybackMixers  = new Mixer*      [mPlaybackTracks.GetCount()];

      for( unsigned int i = 0; i < mPlaybackTracks.GetCount(); i++ )
      {
         mPlaybackBuffers[i] = new RingBuffer(floatSample, playbackBufferSize);

         mPlaybackMixers[i]  = new Mixer(1, &mPlaybackTracks[i],
                                         timeTrack, mT0, mT1, 1,
                                         playbackMixBufferSize, false,
                                         mRate, floatSample, false);
         mPlaybackMixers[i]->ApplyTrackGains(false);
      }
   }

   if( mNumCaptureChannels > 0 )
   {
      // Allocate input buffers.  For every input track we allocate
      // a ring buffer of five seconds
      sampleCount captureBufferSize =
         (sampleCount)(mRate * mCaptureRingBufferSecs + 0.5);
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

   //
   // Generate an unique value each time, to be returned to
   // clients accessing the AudioIO API, so they can query if
   // are the ones who have reserved AudioIO or not.
   //
   mStreamToken = (++mNextStreamToken);

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

   //
   // Only set token to 0 after we're totally finished with everything
   //
   mStreamToken = 0;
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

bool AudioIO::IsBusy()
{
   if (IsStreamActive())
      return true;

#if USE_PORTAUDIO_V19
   if (mPortStreamV19)
      return true;
#else
   if (mPortStreamV18)
      return true;
#endif

   if (mStreamToken != 0)
      return true;

   return false;
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
   if( IsStreamActive() && token > 0 && token == mStreamToken )
      return true;
   else
      return false;
}

bool AudioIO::IsAudioTokenActive(int token)
{
   return ( token > 0 && token == mStreamToken );
}

double AudioIO::NormalizeStreamTime(double absoluteTime) const
{
   // msmeyer: Just to be sure, the returned stream time should
   //          never be smaller than the actual start time.
   if (absoluteTime < mT0)
      absoluteTime = mT0;
   
   // msmeyer: This can happen if we are playing in looped mode.
   while (absoluteTime > mT1)
      absoluteTime -= mT1 - mT0;
      
   return absoluteTime;
}

double AudioIO::GetStreamTime()
{
   if( !IsBusy() )
      return -1000000000;

   if( mPaused )
      return NormalizeStreamTime(mPausePosition);

#if USE_PORTAUDIO_V19

   PaStream *stream = mPortStreamV19;

   // Based on the number of samples we have written, this is the value
   // in time that represents how far we are through the source data
   double lastBufferTime =  mT0 + (mTotalSamplesPlayed / mRate);

   // This is the number of seconds ago that the last buffer started
   // being heard.  It will be negative if the last buffer is not
   // yet audible.
   double deltat = Pa_GetStreamTime(stream) - mLastBufferAudibleTime;

   // [JH]: I need to diagram this, but I'm pretty sure this calculation
   // is off by one buffer size
   double time = lastBufferTime + deltat;
   return NormalizeStreamTime(time);
#else
   PaStream *stream = mPortStreamV18;

   double streamTime = Pa_StreamTime(stream);
   double indicator = mT0 + (streamTime / mRate) - mPausedSeconds;

   // Pa_StreamTime can sometimes return wacky results, so we
   // try to filter those out...
   if (fabs(indicator - mLastIndicator) > 0.1) {
      mLastIndicator = indicator;
      return NormalizeStreamTime(mLastStableIndicator);
   }
   mLastIndicator = indicator;
   mLastStableIndicator = indicator;
   return NormalizeStreamTime(indicator);
#endif
}


wxArrayLong AudioIO::GetSupportedSampleRates(wxString playDevice, wxString recDevice)
{
   int numDefaultRates = 7;
   int defaultRates[] = {
      8000,
      11025,
      16000,
      22050,
      44100,
      48000,
      96000
   };

   const PaDeviceInfo* playInfo = NULL;
   const PaDeviceInfo* recInfo = NULL;

   if (playDevice.IsEmpty())
      playDevice = gPrefs->Read("/AudioIO/PlaybackDevice", "");
   if (recDevice.IsEmpty())
      recDevice = gPrefs->Read("/AudioIO/RecordingDevice", "");

   int i;

   // msmeyer: Find info structs for playing/recording devices
#if USE_PORTAUDIO_V19
   for (i = 0; i < Pa_GetDeviceCount(); i++) {
#else
   for (i = 0; i < Pa_CountDevices(); i++) {
#endif
      const PaDeviceInfo* info = Pa_GetDeviceInfo(i);

      if (info->name == playDevice && info->maxOutputChannels > 0)
         playInfo = info;
      if (info->name == recDevice && info->maxInputChannels > 0)
         recInfo = info;
   }

   // msmeyer: Check which sample rates the play device supports
   wxArrayLong playSampleRates;

   if (playInfo)
   {
#if USE_PORTAUDIO_V19
      // TODO: implement using IsFormatSupported()
      for (i = 0; i < numDefaultRates; i++)
         playSampleRates.Add(defaultRates[i]);
#else
      if (playInfo->numSampleRates == -1)
      {
         for (i = 0; i < numDefaultRates; i++)
            if (defaultRates[i] >= playInfo->sampleRates[0] &&
                defaultRates[i] <= playInfo->sampleRates[1])
               playSampleRates.Add(defaultRates[i]);
      } else
      {
         for (i = 0; i < playInfo->numSampleRates; i++)
            playSampleRates.Add((int)playInfo->sampleRates[i]);
      }
#endif
   }

   if (playSampleRates.IsEmpty())
   {
      for (i = 0; i < numDefaultRates; i++)
         playSampleRates.Add(defaultRates[i]);
   }

   // msmeyer: Check which sample rates the record device supports
   wxArrayLong recSampleRates;

   if (recInfo)
   {
#if USE_PORTAUDIO_V19
      // TODO: implement using IsFormatSupported()
      for (i = 0; i < numDefaultRates; i++)
         recSampleRates.Add(defaultRates[i]);
#else
      if (recInfo->numSampleRates == -1)
      {
         for (i = 0; i < numDefaultRates; i++)
            if (defaultRates[i] >= recInfo->sampleRates[0] &&
                defaultRates[i] <= recInfo->sampleRates[1])
               recSampleRates.Add(defaultRates[i]);
      } else
      {
         for (i = 0; i < recInfo->numSampleRates; i++)
            recSampleRates.Add((int)recInfo->sampleRates[i]);
      }
#endif
   }

   if (recSampleRates.IsEmpty())
   {
      for (i = 0; i < numDefaultRates; i++)
         recSampleRates.Add(defaultRates[i]);
   }

   // Return only sample rates which are in both arrays
   wxArrayLong result;

   for (i = 0; i < (int)playSampleRates.GetCount(); i++)
      if (recSampleRates.Index(playSampleRates[i]) != wxNOT_FOUND)
         result.Add(playSampleRates[i]);

   // If this yields no results, use the default sample rates nevertheless
   if (result.IsEmpty())
   {
      for (i = 0; i < numDefaultRates; i++)
         result.Add(defaultRates[i]);
   }

   return result;
}


int AudioIO::GetOptimalSupportedSampleRate()
{
   wxArrayLong rates = GetSupportedSampleRates();

   if (rates.Index(44100) != wxNOT_FOUND)
      return 44100;

   if (rates.Index(48000) != wxNOT_FOUND)
      return 48000;

   return rates[rates.GetCount() - 1];
}



//////////////////////////////////////////////////////////////////////
//
//     Audio Thread Context
//
//////////////////////////////////////////////////////////////////////

AudioThread::ExitCode AudioThread::Entry()
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

int AudioIO::GetCommonlyAvailPlayback()
{
   int commonlyAvail = mPlaybackBuffers[0]->AvailForPut();
   unsigned int i;

   for( i = 1; i < mPlaybackTracks.GetCount(); i++ )
   {
      int thisBlockAvail = mPlaybackBuffers[i]->AvailForPut();

      if( thisBlockAvail < commonlyAvail )
         commonlyAvail = thisBlockAvail;
   }

   return commonlyAvail;
}

int AudioIO::GetCommonlyAvailCapture()
{
   int commonlyAvail = mCaptureBuffers[0]->AvailForGet();
   unsigned int i;

   for( i = 1; i < mCaptureTracks.GetCount(); i++ )
   {
      int avail = mCaptureBuffers[i]->AvailForGet();
      if( avail < commonlyAvail )
         commonlyAvail = avail;
   }

   return commonlyAvail;
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
      int commonlyAvail = GetCommonlyAvailPlayback();

      //
      // Determine how much this will globally advance playback time
      //
      double secsAvail = commonlyAvail / mRate;

      //
      // Don't fill the buffers at all unless we can do the
      // full mMaxPlaybackSecsToCopy.  This improves performance
      // by not always trying to process tiny chunks, eating the
      // CPU unnecessarily.
      //
      // The exception is if we're at the end of the selected
      // region - then we should just fill the buffer.
      //
      if (secsAvail >= mMaxPlaybackSecsToCopy ||
          (!mPlayLooped && (secsAvail > 0 && mT+secsAvail >= mT1)))
      {
         // Limit maximum buffer size (increases performance)
         if (secsAvail > mMaxPlaybackSecsToCopy)
            secsAvail = mMaxPlaybackSecsToCopy;

         double deltat;

         // msmeyer: When playing a very short selection in looped
         // mode, the selection must be copied to the buffer multiple
         // times, to ensure, that the buffer has a reasonable size
         // This is the purpose of this loop.
         do {
            deltat = secsAvail;

            if( mT + deltat > mT1 )
            {
               deltat = mT1 - mT;
               if( deltat < 0.0 )
                  deltat = 0.0;
            }
            mT += deltat;
            secsAvail -= deltat;

            for( i = 0; i < mPlaybackTracks.GetCount(); i++ )
            {
               // The mixer here isn't actually mixing: it's just doing
               // resampling, format conversion, and possibly time track
               // warping
               int processed =
                  mPlaybackMixers[i]->Process((int)(deltat * mRate + 0.5));
               samplePtr warpedSamples = mPlaybackMixers[i]->GetBuffer();
               mPlaybackBuffers[i]->Put(warpedSamples, floatSample, processed);
            }

            // msmeyer: If playing looped, check if we are at the end of the buffer
            // and if yes, restart from the beginning.
            if (mPlayLooped && mT >= mT1)
            {
               for (i = 0; i < mPlaybackTracks.GetCount(); i++)
                  mPlaybackMixers[i]->Restart();
               mT = mT0;
            }
         } while (mPlayLooped && secsAvail > 0 && deltat > 0);
      }
   }

   if( mCaptureTracks.GetCount() > 0 )
   {
      int commonlyAvail = GetCommonlyAvailCapture();

      //
      // Determine how much this will add to captured tracks
      //
      double deltat = commonlyAvail / mRate;

      if (mAudioThreadShouldCallFillBuffersOnce ||
          deltat >= mMinCaptureSecsToCopy)
      {
         // Append captured samples to the end of the WaveTracks.
         // The WaveTracks have their own buffering for efficiency.
         for( i = 0; i < mCaptureTracks.GetCount(); i++ )
         {
            int avail = commonlyAvail;
            sampleFormat trackFormat = mCaptureTracks[i]->GetSampleFormat();
            samplePtr temp = NewSamples(avail, trackFormat);

            mCaptureBuffers[i]->Get   (temp, trackFormat, avail);
            mCaptureTracks[i]-> Append(temp, trackFormat, avail);

            DeleteSamples(temp);
         }
      }
   }
}

//////////////////////////////////////////////////////////////////////
//
//    PortAudio callback thread context
//
//////////////////////////////////////////////////////////////////////

#define MAX(a,b) ((a) > (b) ? (a) : (b))

#if USE_PORTAUDIO_V19
int audacityAudioCallback(const void *inputBuffer, void *outputBuffer,
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
   float *tempFloats = (float*)alloca(framesPerBuffer*sizeof(float)*
                                      MAX(numCaptureChannels,numPlaybackChannels));
   unsigned int i;
   int t;

   if( gAudioIO->mPaused )
   {
      if (outputBuffer && numPlaybackChannels > 0)
      {
         ClearSamples((samplePtr)outputBuffer, floatSample,
                      0, framesPerBuffer * numPlaybackChannels);
      }

#ifndef USE_PORTAUDIO_V19
      gAudioIO->mPausedSeconds += (float)framesPerBuffer / gAudioIO->mRate;
#endif
      return paContinue;
   }

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
         // msmeyer: We never finish if we are playing looped
         if (len == 0 && gAudioIO->mT >= gAudioIO->mT1 && !gAudioIO->mPlayLooped)
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

            if (gAudioIO->mEmulateMixerOutputVol)
               gain *= gAudioIO->mMixerOutputVol;

            for(i=0; i<len; i++)
               outputFloats[numPlaybackChannels*i] += gain*tempFloats[i];
         }

         if (vt->GetChannel() == Track::RightChannel ||
             vt->GetChannel() == Track::MonoChannel)
         {
            float gain = vt->GetChannelGain(1);

            if (gAudioIO->mEmulateMixerOutputVol)
               gain *= gAudioIO->mMixerOutputVol;

            for(i=0; i<len; i++)
               outputFloats[numPlaybackChannels*i+1] += gain*tempFloats[i];
         }
      }

      //
      // Clip output to [-1.0,+1.0] range (msmeyer)
      //
      for( i = 0; i < framesPerBuffer*numPlaybackChannels; i++)
      {
          float f = outputFloats[i];
          if (f > 1.0)
              outputFloats[i] = 1.0;
          else if (f < -1.0)
              outputFloats[i] = -1.0;
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

      float gain = 1.0;

      if (gAudioIO->mEmulateMixerInputVol)
         gain = gAudioIO->mMixerInputVol;

      if (len > 0) {
         for( t = 0; t < numCaptureChannels; t++) {
            for( i = 0; i < len; i++)
               tempFloats[i] = inputFloats[numCaptureChannels*i+t] * gain;

            gAudioIO->mCaptureBuffers[t]->Put((samplePtr)tempFloats,
                                              gAudioIO->mCaptureFormat, len);
         }
      }
   }

#if USE_PORTAUDIO_V19
   gAudioIO->mTotalSamplesPlayed += framesPerBuffer;
   gAudioIO->mLastBufferAudibleTime = timeInfo->outputBufferDacTime;
#endif

   #if 0 // VU METER

   // Example code showing how to use the Meter class

   if( outputBuffer && numPlaybackChannels == 2 )
   {
      float left = 0, right = 0;
      float *outputFloats = (float *)outputBuffer;

      for( i = 0; i < framesPerBuffer; i++) {
         if (outputFloats[2*i] > left)
            left = outputFloats[2*i];
         if (outputFloats[2*i+1] > right)
            right = outputFloats[2*i+1];
      }
      gMeter->PostUpdate(left, right, (double)outTime);
   }

   #endif

   return callbackReturn;
}

