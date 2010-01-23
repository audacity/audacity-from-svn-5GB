/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.cpp

  Copyright 2000-2004:
  Dominic Mazzoni
  Joshua Haberman
  Markus Meyer
  Matt Brubeck

  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

********************************************************************//**

\class AudioIO
\brief AudioIO uses the PortAudio library to play and record sound.

  Great care and attention to detail are necessary for understanding and
  modifying this system.  The code in this file is run from three
  different thread contexts: the UI thread, the disk thread (which
  this file creates and maintains) and the PortAudio callback thread.
  To highlight this deliniation, the file is divided into three parts
  based on what thread context each function is intended to run in.

  \todo run through all functions called from audio and portaudio threads
  to verify they are thread-safe.

*//****************************************************************//**

\class AudioThread
\brief Defined different on Mac and other platforms (on Mac it does not
use wxWidgets wxThread), this class sits in a thread loop reading and 
writing audio.

*//*******************************************************************/

#include "Audacity.h"
#include "float_cast.h"

#include <math.h>
#include <stdlib.h>

#ifdef __WXMSW__
#include <malloc.h>
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include <wx/log.h>
#include <wx/textctrl.h>
#include <wx/msgdlg.h>
#include <wx/timer.h>
#include <wx/intl.h>
#include <wx/debug.h>
#include <wx/sstream.h>
#include <wx/txtstrm.h>

#include "AudacityApp.h"
#include "AudioIO.h"
#include "WaveTrack.h"
#include "Mix.h"
#include "Resample.h"
#include "RingBuffer.h"
#include "Prefs.h"
#include "Project.h"
#include "toolbars/ControlToolBar.h"

#include "widgets/Meter.h"
#include "../Experimental.h"

#define NO_STABLE_INDICATOR -1000000000

AudioIO *gAudioIO;

// static
int AudioIO::mNextStreamToken = 0;

const int AudioIO::StandardRates[] = {
   8000,
   16000,
   22050,
   44100,
   48000,
   96000
};
const int AudioIO::NumStandardRates = sizeof(AudioIO::StandardRates) /
                                      sizeof(AudioIO::StandardRates[0]);
const int AudioIO::RatesToTry[] = {
   8000,
   9600,
   11025,
   12000,
   15000,
   16000,
   22050,
   24000,
   32000,
   44100,
   48000,
   88200,
   96000,
   192000
};
const int AudioIO::NumRatesToTry = sizeof(AudioIO::RatesToTry) /
                                      sizeof(AudioIO::RatesToTry[0]);

#if USE_PORTAUDIO_V19
int audacityAudioCallback(const void *inputBuffer, void *outputBuffer,
                          unsigned long framesPerBuffer,
                          const PaStreamCallbackTimeInfo *timeInfo,
                          PaStreamCallbackFlags statusFlags, void *userData );
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

wxString DeviceName(const PaDeviceInfo* info)
{
#if USE_PORTAUDIO_V19
   wxString hostapiName(Pa_GetHostApiInfo(info->hostApi)->name, wxConvLocal);
   wxString infoName(info->name, wxConvLocal);

   return wxString::Format(wxT("%s: %s"),
                           hostapiName.c_str(),
                           infoName.c_str());
#else  
   return wxString(info->name, wxConvLocal);
#endif
}

AudioIO::AudioIO()
{
   mAudioThreadShouldCallFillBuffersOnce = false;
   mAudioThreadFillBuffersLoopRunning = false;
   mAudioThreadFillBuffersLoopActive = false;
#if USE_PORTAUDIO_V19
   mPortStreamV19 = NULL;
#else
   mPortStreamV18 = NULL;
   mInCallbackFinishedState = false;
#endif
   mStreamToken = 0;
   mStopStreamCount = 0;
   mTempFloats = new float[65536]; // TODO: out channels * PortAudio buffer size

   mLastPaError = paNoError;

   mLastRecordingOffset = 0.0;
   mNumCaptureChannels = 0;
   mPaused = false;
   mPlayLooped = false;

   mListener = NULL;
   mUpdateMeters = false;
   mUpdatingMeters = false;

   PaError err = Pa_Initialize();

   if (err != paNoError) {
      wxString errStr = _("Could not find any audio devices.\n");
      errStr += _("You will not be able to play or record audio.\n\n");
      wxString paErrStr = LAT1CTOWX(Pa_GetErrorText(err));
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

#if defined(USE_PORTMIXER)
   mPortMixer = NULL;
   mPreviousHWPlaythrough = -1.0;
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
#if defined(USE_PORTMIXER)
   if (mPortMixer) {
      #if __WXMAC__
      if (Px_SupportsPlaythrough(mPortMixer) && mPreviousHWPlaythrough >= 0.0)
         Px_SetPlaythrough(mPortMixer, mPreviousHWPlaythrough);
         mPreviousHWPlaythrough = -1.0;
      #endif
      Px_CloseMixer(mPortMixer);
      mPortMixer = NULL;
   }
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

#if defined(USE_PORTMIXER)
   PxMixer *mixer = mPortMixer;

   if( mixer )
   {
      int oldRecordDevice = Px_GetCurrentInputSource(mixer);
      float oldRecordVolume = Px_GetInputVolume(mixer);
      float oldPlaybackVolume = Px_GetPCMOutputVolume(mixer);

      if( recordDevice != oldRecordDevice )
         Px_SetCurrentInputSource(mixer, recordDevice);
      if( oldRecordVolume != recordVolume )
         Px_SetInputVolume(mixer, recordVolume);
      if( oldPlaybackVolume != playbackVolume )
         Px_SetPCMOutputVolume(mixer, playbackVolume);

      return;
   }
#endif
}

void AudioIO::GetMixer(int *recordDevice, float *recordVolume,
                       float *playbackVolume)
{
#if defined(USE_PORTMIXER)

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
#if defined(USE_PORTMIXER)

   wxArrayString deviceNames;

   if( mPortMixer )
   {
      int numSources = Px_GetNumInputSources(mPortMixer);
      for( int source = 0; source < numSources; source++ )
         deviceNames.Add(LAT1CTOWX(Px_GetInputSourceName(mPortMixer, source)));
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
   if (IsStreamActive())
      return;
   // this function only does something (at the moment) for portmixer.
#if defined(USE_PORTMIXER)

   // if we have a PortMixer object, close it down
   if (mPortMixer) {
      #if __WXMAC__
      // on the Mac we must make sure that we restore the hardware playthrough
      // state of the sound device to what it was before, because there isn't
      // a UI for this (!)
      if (Px_SupportsPlaythrough(mPortMixer) && mPreviousHWPlaythrough >= 0.0)
         Px_SetPlaythrough(mPortMixer, mPreviousHWPlaythrough);
         mPreviousHWPlaythrough = -1.0;
      #endif
      Px_CloseMixer(mPortMixer);
      mPortMixer = NULL;
   }

   // get the selected record and playback devices
   int playDeviceNum = getPlayDevIndex();
   int recDeviceNum = getRecordDevIndex();

   wxArrayLong supportedSampleRates = GetSupportedSampleRates(playDeviceNum, recDeviceNum);
   // that might have given us no rates whatsoever, so we have to guess an
   // answer to do the next bit
   int numrates = supportedSampleRates.GetCount();
   int highestSampleRate;
   if (numrates > 0)
   {
      highestSampleRate = supportedSampleRates[numrates - 1];
   }
   else
   {  // we don't actually have any rates that work for Rec and Play. Guess one
      // to use for messing with the mixer, which doesn't actually do either
      highestSampleRate = 44100;
      // supportedSampleRates is still empty, but it's not used again, so
      // can ignore
   }
   mEmulateMixerInputVol = true;
   mEmulateMixerOutputVol = true;
   mMixerInputVol = 1.0;
   mMixerOutputVol = 1.0;

   int error;
   // This tries to open the device with the samplerate worked out above, which
   // will be the highest available for play and record on the device, or
   // 44.1kHz if the info cannot be fetched.

#if USE_PORTAUDIO_V19

   PaStream *stream;

   PaStreamParameters playbackParameters;

   playbackParameters.device = playDeviceNum;
   playbackParameters.sampleFormat = paFloat32;
   playbackParameters.hostApiSpecificStreamInfo = NULL;
   playbackParameters.channelCount = 2;
   if (Pa_GetDeviceInfo(playDeviceNum))
      playbackParameters.suggestedLatency =
         Pa_GetDeviceInfo(playDeviceNum)->defaultLowOutputLatency;
   else
      playbackParameters.suggestedLatency = DEFAULT_LATENCY_CORRECTION/1000.0; 

   PaStreamParameters captureParameters;
 
   captureParameters.device = recDeviceNum;
   captureParameters.sampleFormat = paFloat32;;
   captureParameters.hostApiSpecificStreamInfo = NULL;
   captureParameters.channelCount = 2;
   if (Pa_GetDeviceInfo(recDeviceNum))
      captureParameters.suggestedLatency =
         Pa_GetDeviceInfo(recDeviceNum)->defaultLowInputLatency;
   else
      captureParameters.suggestedLatency = DEFAULT_LATENCY_CORRECTION/1000.0; 

   // try opening for record and playback
   error = Pa_OpenStream(&stream,
                         &captureParameters, &playbackParameters,
                         highestSampleRate, paFramesPerBufferUnspecified,
                         paClipOff | paDitherOff,
                         audacityAudioCallback, NULL);
   // if that failed, try just for record
   if( error ) {
      error = Pa_OpenStream(&stream,
                            &captureParameters, NULL,
                            highestSampleRate, paFramesPerBufferUnspecified,
                            paClipOff | paDitherOff,
                            audacityAudioCallback, NULL);
   }
  
#else

   PortAudioStream *stream;

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

#endif
   // if it's still not working, give up
   if( error )
      return;
   // set up portmixer on the open portaudio stream
   mPortMixer = Px_OpenMixer(stream, 0);

   if (!mPortMixer) {
      Pa_CloseStream(stream);
      return;
   }

   // Determine mixer capabilities - if it doesn't support either
   // input or output, we emulate them (by multiplying this value
   // by all incoming/outgoing samples)

   mMixerOutputVol = Px_GetPCMOutputVolume(mPortMixer);
   mEmulateMixerOutputVol = false;
   Px_SetPCMOutputVolume(mPortMixer, 0.0);
   if (Px_GetPCMOutputVolume(mPortMixer) > 0.1)
      mEmulateMixerOutputVol = true;
   Px_SetPCMOutputVolume(mPortMixer, 0.2f);
   if (Px_GetPCMOutputVolume(mPortMixer) < 0.1 ||
       Px_GetPCMOutputVolume(mPortMixer) > 0.3)
      mEmulateMixerOutputVol = true;
   Px_SetPCMOutputVolume(mPortMixer, mMixerOutputVol);

   mMixerInputVol = Px_GetInputVolume(mPortMixer);
   mEmulateMixerInputVol = false;
   Px_SetInputVolume(mPortMixer, 0.0);
   if (Px_GetInputVolume(mPortMixer) > 0.1)
      mEmulateMixerInputVol = true;
   Px_SetInputVolume(mPortMixer, 0.2f);
   if (Px_GetInputVolume(mPortMixer) < 0.1 ||
       Px_GetInputVolume(mPortMixer) > 0.3)
      mEmulateMixerInputVol = true;
   Px_SetInputVolume(mPortMixer, mMixerInputVol);

   Pa_CloseStream(stream);

   #if 0
   printf("PortMixer: Output: %s Input: %s\n",
          mEmulateMixerOutputVol? "emulated": "native",
          mEmulateMixerInputVol? "emulated": "native");
   #endif

   mMixerInputVol = 1.0;
   mMixerOutputVol = 1.0;

#endif   // USE_PORTMIXER
}

PaSampleFormat AudacityToPortAudioSampleFormat(sampleFormat format)
{
   switch(format) {
   case int16Sample:
      return paInt16;
   case int24Sample:
      return paInt24;
   case floatSample:
   default:
      return paFloat32;
   }
}

bool AudioIO::StartPortAudioStream(double sampleRate,
                                   unsigned int numPlaybackChannels,
                                   unsigned int numCaptureChannels,
                                   sampleFormat captureFormat)
{
   mLastPaError = paNoError;
   // pick a rate to do the audio I/O at, from those available. The project
   // rate is suggested, but we may get something else if it isn't supported
   mRate = GetBestRate(numCaptureChannels > 0, numPlaybackChannels > 0, sampleRate);
   if (mListener) {
      // advertise the chosen I/O sample rate to the UI
      mListener->OnAudioIORate((int)mRate);
   }
   
   // Special case: Our 24-bit sample format is different from PortAudio's
   // 3-byte packed format. So just make PortAudio return float samples,
   // since we need float values anyway to apply the gain.
   if (captureFormat == int24Sample)
      captureFormat = floatSample;

   mNumPlaybackChannels = numPlaybackChannels;
   mNumCaptureChannels = numCaptureChannels;

#if USE_PORTAUDIO_V19
   PaStreamParameters *playbackParameters = NULL;
   PaStreamParameters *captureParameters = NULL;
   
   double latencyDuration = DEFAULT_LATENCY_DURATION;
   gPrefs->Read(wxT("/AudioIO/LatencyDuration"), &latencyDuration);

   if( numPlaybackChannels > 0)
   {
      playbackParameters = new PaStreamParameters;
      // this sets the device index to whatever is "right" based on preferences,
      // then defaults
      playbackParameters->device = getPlayDevIndex();
     
      const PaDeviceInfo *playbackDeviceInfo;
      playbackDeviceInfo = Pa_GetDeviceInfo( playbackParameters->device );
      
      if( playbackDeviceInfo == NULL )
         return false;
      
      // regardless of source formats, we always mix to float
      playbackParameters->sampleFormat = paFloat32;
      playbackParameters->hostApiSpecificStreamInfo = NULL;
      playbackParameters->channelCount = mNumPlaybackChannels;

      if (mSoftwarePlaythrough)
         playbackParameters->suggestedLatency =
            playbackDeviceInfo->defaultLowOutputLatency;
      else
         playbackParameters->suggestedLatency = latencyDuration/1000.0;
   }

   if( numCaptureChannels > 0)
   {
      mCaptureFormat = captureFormat;
      
      captureParameters = new PaStreamParameters;
      const PaDeviceInfo *captureDeviceInfo;
      // retrieve the index of the device set in the prefs, or a sensible
      // default if it isn't set/valid
      captureParameters->device = getRecordDevIndex();

      captureDeviceInfo = Pa_GetDeviceInfo( captureParameters->device );

      if( captureDeviceInfo == NULL )
         return false;

      captureParameters->sampleFormat =
         AudacityToPortAudioSampleFormat(mCaptureFormat);

      captureParameters->hostApiSpecificStreamInfo = NULL;
      captureParameters->channelCount = mNumCaptureChannels;

      if (mSoftwarePlaythrough)
         captureParameters->suggestedLatency =
            captureDeviceInfo->defaultHighInputLatency;
      else
         captureParameters->suggestedLatency = latencyDuration/1000.0;
   }

   mLastPaError = Pa_OpenStream( &mPortStreamV19,
                                 captureParameters, playbackParameters,
                                 mRate, paFramesPerBufferUnspecified,
                                 paNoFlag,
                                 audacityAudioCallback, NULL );

#if USE_PORTMIXER
   if (mPortStreamV19 != NULL && mLastPaError == paNoError) {
      #ifdef __MACOSX__
      if (mPortMixer) {
         if (Px_SupportsPlaythrough(mPortMixer)) {
            bool playthrough;

            mPreviousHWPlaythrough = Px_GetPlaythrough(mPortMixer);

            gPrefs->Read(wxT("/AudioIO/Playthrough"), &playthrough, false);
            if (playthrough)
               Px_SetPlaythrough(mPortMixer, 1.0);
            else
               Px_SetPlaythrough(mPortMixer, 0.0);
         }
      }
      #endif
   }
#endif

   // these may be null, but deleting a null pointer should never crash.
   delete captureParameters;
   delete playbackParameters;

#else

   PaDeviceID captureDevice = paNoDevice,
              playbackDevice = paNoDevice;
   PaSampleFormat paCaptureFormat = paFloat32;

   if( numPlaybackChannels > 0 )
   {
      playbackDevice =  Pa_GetDefaultOutputDeviceID();
      wxString playbackDeviceName = gPrefs->Read(wxT("/AudioIO/PlaybackDevice"), wxT(""));

      if( playbackDeviceName != wxT("") )
      {
         for( int i = 0; i < Pa_CountDevices(); i++)
         {
            const PaDeviceInfo* info = Pa_GetDeviceInfo(i);

            if(!info)
               continue;

            if (DeviceName(info) == playbackDeviceName && info->maxOutputChannels > 0)
               playbackDevice = i;
         }
      }
   }

   if( numCaptureChannels > 0 )
   {
      // For capture, every input channel gets its own track
      mCaptureFormat = captureFormat;
      captureDevice =  Pa_GetDefaultInputDeviceID();
      wxString captureDeviceName = gPrefs->Read(wxT("/AudioIO/RecordingDevice"), wxT(""));

      if( captureDeviceName != wxT("") )
      {
         for( int i = 0; i < Pa_CountDevices(); i++)
         {
            const PaDeviceInfo* info = Pa_GetDeviceInfo(i);

            if(!info)
               continue;

            if (DeviceName(info) == captureDeviceName && info->maxInputChannels > 0)
               captureDevice = i;
         }
      }

      paCaptureFormat =
         AudacityToPortAudioSampleFormat(mCaptureFormat);
   }

   mPortStreamV18 = NULL;

   mLastPaError = Pa_OpenStream( &mPortStreamV18,
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
   if (mPortStreamV18 != NULL && mLastPaError == paNoError) {
      #ifdef __MACOSX__
      if (mPortMixer) {
         if (Px_SupportsPlaythrough(mPortMixer)) {
            bool playthrough;

            mPreviousHWPlaythrough = Px_GetPlaythrough(mPortMixer);

            gPrefs->Read(wxT("/AudioIO/Playthrough"), &playthrough, false);
            if (playthrough)
               Px_SetPlaythrough(mPortMixer, 1.0);
            else
               Px_SetPlaythrough(mPortMixer, 0.0);
         }
      }
      #endif
   }

#endif

   mInCallbackFinishedState = false; // v18 only

#endif   

   return (mLastPaError == paNoError);
}

void AudioIO::StartMonitoring(double sampleRate)
{
#if USE_PORTAUDIO_V19
   if ( mPortStreamV19 || mStreamToken )
      return;
#else
   if ( mPortStreamV18 || mStreamToken )
      return;
#endif

   bool success;
   long captureChannels;
   sampleFormat captureFormat = (sampleFormat)
      gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample);
   gPrefs->Read(wxT("/AudioIO/RecordChannels"), &captureChannels, 1L);
   gPrefs->Read(wxT("/AudioIO/SWPlaythrough"), &mSoftwarePlaythrough, false);
   int playbackChannels = 0;

   if (mSoftwarePlaythrough)
      playbackChannels = 2;

   success = StartPortAudioStream(sampleRate, (unsigned int)playbackChannels,
                                  (unsigned int)captureChannels,
                                  captureFormat);

   // Now start the PortAudio stream!
#if USE_PORTAUDIO_V19
   mLastPaError = Pa_StartStream( mPortStreamV19 );
#else
   mLastPaError = Pa_StartStream( mPortStreamV18 );
#endif
}

int AudioIO::StartStream(WaveTrackArray playbackTracks,
                         WaveTrackArray captureTracks,
                         TimeTrack *timeTrack, double sampleRate,
                         double t0, double t1,
                         AudioIOListener* listener,
                         bool playLooped /* = false */,
                         double cutPreviewGapStart /* = 0.0 */,
                         double cutPreviewGapLen /* = 0.0 */)
{
   if( IsBusy() )
      return 0;

   // We just want to set mStreamToken to -1 - this way avoids
   // an extremely rare but possible race condition, if two functions
   // somehow called StartStream at the same time...
   mStreamToken--;
   if (mStreamToken != -1)
      return 0;

   // TODO: we don't really need to close and reopen stream if the
   // format matches; however it's kind of tricky to keep it open...
   //
   //   if (sampleRate == mRate &&
   //       playbackChannels == mNumPlaybackChannels &&
   //       captureChannels == mNumCaptureChannels &&
   //       captureFormat == mCaptureFormat) {

   #if USE_PORTAUDIO_V19
   if (mPortStreamV19) {
      StopStream();
      while(mPortStreamV19)
         wxMilliSleep( 50 );            
   }
   #else
   if (mPortStreamV18) {
      StopStream();
      while(mPortStreamV18)
         wxMilliSleep( 50 );
   }
   #endif

   gPrefs->Read(wxT("/AudioIO/SWPlaythrough"), &mSoftwarePlaythrough, false);

#ifdef EXPERIMENTAL_SMART_RECORD
   gPrefs->Read(wxT("/AudioIO/PauseRecOnSilence"), &mPauseRec, false);
   int silenceLevelDB;
   gPrefs->Read(wxT("/AudioIO/SilenceLevel"), &silenceLevelDB, -50);
   mSilenceLevel = (silenceLevelDB + 60.)/60.;  // meter goes -60dB -> 0dB
#endif

   mTimeTrack = timeTrack;
   mListener = listener;
   mInputMeter = NULL;
   mOutputMeter = NULL;
   mRate    = sampleRate;
   mT0      = t0;
   mT       = t0;
   mT1      = t1;
   mTime    = t0;
   mSeek    = 0;
   mLastRecordingOffset = 0;
   mPlaybackTracks = playbackTracks;
   mCaptureTracks  = captureTracks;
   mPlayLooped = playLooped;
   mCutPreviewGapStart = cutPreviewGapStart;
   mCutPreviewGapLen = cutPreviewGapLen;

   double factor = 1.0;
   if (mTimeTrack) {
      factor = mTimeTrack->GetEnvelope()->Average(mT0, mT1);
      factor = (mTimeTrack->GetRangeLower() *
               (1 - factor) +
               factor *
               mTimeTrack->GetRangeUpper()) / 
               100.0;
   }
   mWarpedT1 = factor >= 1 ? mT1 : mT0 + ((mT1 - mT0) / factor);

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

   unsigned int playbackChannels = 0;
   unsigned int captureChannels = 0;
   sampleFormat captureFormat = floatSample;

   if( playbackTracks.GetCount() > 0 )
      playbackChannels = 2;

   if (mSoftwarePlaythrough)
      playbackChannels = 2;

   if( captureTracks.GetCount() > 0 )
   {
      // For capture, every input channel gets its own track
      captureChannels = mCaptureTracks.GetCount();
      // I don't deal with the possibility of the capture tracks
      // having different sample formats, since it will never happen
      // with the current code.  This code wouldn't *break* if this
      // assumption was false, but it would be sub-optimal.  For example,
      // if the first track was 16-bit and the second track was 24-bit,
      // we would set the sound card to capture in 16 bits and the second
      // track wouldn't get the benefit of all 24 bits the card is capable
      // of.
      captureFormat = mCaptureTracks[0]->GetSampleFormat();
      
      // Tell project that we are about to start recording
      if (mListener)
         mListener->OnAudioIOStartRecording();
   }
   
   bool success = StartPortAudioStream(sampleRate, playbackChannels,
                                       captureChannels, captureFormat);
   
   if (!success) {
      if (mListener && captureChannels > 0)
         mListener->OnAudioIOStopRecording();
      mStreamToken = 0;
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
                                         mTimeTrack, mT0, mWarpedT1, 1,
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
      mResample = new Resample* [mCaptureTracks.GetCount()];
      mFactor = sampleRate / mRate;

      for( unsigned int i = 0; i < mCaptureTracks.GetCount(); i++ )
      {
         mCaptureBuffers[i] = new RingBuffer( mCaptureTracks[i]->GetSampleFormat(),
                                              captureBufferSize );
         mResample[i] = new Resample( true, mFactor, mFactor );
      }
   }

   // We signal the audio thread to call FillBuffers, to prime the RingBuffers
   // so that they will have data in them when the stream starts.  Having the
   // audio thread call FillBuffers here makes the code more predictable, since
   // FillBuffers will ALWAYS get called from the Audio thread.
   mAudioThreadShouldCallFillBuffersOnce = true;

   while( mAudioThreadShouldCallFillBuffersOnce == true )
      wxMilliSleep( 50 );

   // Now start the PortAudio stream!
   PaError err;
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
      if (mListener && mNumCaptureChannels > 0)
         mListener->OnAudioIOStopRecording();
      wxPrintf(wxT("%hs\n"), Pa_GetErrorText(err));
      mStreamToken = 0;
      return 0;
   }

   mAudioThreadFillBuffersLoopRunning = true;

   //
   // Generate an unique value each time, to be returned to
   // clients accessing the AudioIO API, so they can query if
   // are the ones who have reserved AudioIO or not.
   //
   mStreamToken = (++mNextStreamToken);

   return mStreamToken;
}

void AudioIO::SetMeters(Meter *inputMeter, Meter *outputMeter)
{
   mInputMeter = inputMeter;
   mOutputMeter = outputMeter;

   if (mInputMeter)
      mInputMeter->Reset(mRate, true);
   if (mOutputMeter)
      mOutputMeter->Reset(mRate, true);

   mUpdateMeters = true;
}

void AudioIO::StopStream()
{
 #if USE_PORTAUDIO_V19
   if( mPortStreamV19 == NULL )
      return;

   if( Pa_IsStreamStopped( mPortStreamV19 ) )
      return;
 #else
   if( mPortStreamV18 == NULL )
      return;

   if( IsStreamActive() == false && mInCallbackFinishedState == false )
      return;
 #endif

   // Avoid race condition by making sure this function only
   // gets called once at a time
   mStopStreamCount++;
   if (mStopStreamCount != 1)
      return;

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
   // DMM: This doesn't seem to be true; it seems to be necessary to
   // call StopStream if the callback brought us here, and AbortStream
   // if the user brought us here.
   //

   mAudioThreadFillBuffersLoopRunning = false;

   // Audacity can deadlock if it tries to update meters while
   // we're stopping PortAudio (because the meter updating code
   // tries to grab a UI mutex while PortAudio tries to join a
   // pthread).  So we tell the callback to stop updating meters,
   // and wait until the callback has left this part of the code
   // if it was already there.
   mUpdateMeters = false;
   while(mUpdatingMeters) {
      wxYield();
      wxMilliSleep( 50 );
   }

   // Turn off HW playthrough if PortMixer is being used

  #if defined(USE_PORTMIXER)
   if( mPortMixer ) {
      #if __WXMAC__
      if (Px_SupportsPlaythrough(mPortMixer) && mPreviousHWPlaythrough >= 0.0)
         Px_SetPlaythrough(mPortMixer, mPreviousHWPlaythrough);
         mPreviousHWPlaythrough = -1.0;
      #endif
   }
  #endif

#if USE_PORTAUDIO_V19
   if (mPortStreamV19) {
      Pa_AbortStream( mPortStreamV19 );
      Pa_CloseStream( mPortStreamV19 );
      mPortStreamV19 = NULL;
   }
#else
   if (mPortStreamV18) {
      if (mInCallbackFinishedState)
         Pa_StopStream( mPortStreamV18 );
      else
         Pa_AbortStream( mPortStreamV18 );
      Pa_CloseStream( mPortStreamV18 );
      mPortStreamV18 = NULL;
      mInCallbackFinishedState = false;
   }
#endif

   // If there's no token, we were just monitoring, so we can
   // skip this next part...
   if (mStreamToken > 0) {
      // In either of the above cases, we want to make sure that any
      // capture data that made it into the PortAudio callback makes it
      // to the target WaveTrack.  To do this, we ask the audio thread to
      // call FillBuffers one last time (it normally would not do so since
      // Pa_GetStreamActive() would now return false
      mAudioThreadShouldCallFillBuffersOnce = true;

      while( mAudioThreadShouldCallFillBuffersOnce == true )
      {
         // LLL:  Experienced recursive yield here...once.
         wxGetApp().Yield( true );
         wxMilliSleep( 50 );
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

      //
      // Offset all recorded tracks to account for latency
      //
      if( mCaptureTracks.GetCount() > 0 )
      {
         //
         // We only apply latency correction when we actually played back
         // tracks during the recording. If we did not play back tracks,
         // there's nothing we could be out of sync with. This also covers the
         // case that we do not apply latency correction when recording the
         // first track in a project.
         //
         double latencyCorrection = DEFAULT_LATENCY_CORRECTION;
         gPrefs->Read(wxT("/AudioIO/LatencyCorrection"), &latencyCorrection);
         
         double recordingOffset =
            mLastRecordingOffset + latencyCorrection / 1000.0;

         for( unsigned int i = 0; i < mCaptureTracks.GetCount(); i++ )
            {
               delete mCaptureBuffers[i];
               
               WaveTrack* track = mCaptureTracks[i];
               track->Flush();

               if (mPlaybackTracks.GetCount() > 0)
               {  // only do latency correction if some tracks are being played back
                  WaveTrackArray playbackTracks;
                  AudacityProject *p = GetActiveProject();
                  // we need to get this as mPlaybackTracks does not contain tracks being recorded into
                  playbackTracks = p->GetTracks()->GetWaveTrackArray(false);
                  bool appendRecord = false;
                  for( unsigned int j = 0; j < playbackTracks.GetCount(); j++)
                  {  // find if we are recording into an existing track (append-record)
                     WaveTrack* trackP = playbackTracks[j];
                     if( track == trackP )
                     {
                        if( track->GetStartTime() != mT0 )  // in a new track if these are equal
                        {
                           appendRecord = true;
                           break;
                        }
                     }
                  }
                  if( appendRecord )
                  {  // append-recording
                     if (recordingOffset < 0)
                        track->Clear(mT0, mT0 - recordingOffset); // cut the latency out
                     else
                        track->InsertSilence(mT0, recordingOffset); // put silence in
                  }
                  else
                  {  // recording into a new track
                     track->SetOffset(track->GetStartTime() + recordingOffset);
                     if(track->GetEndTime() < 0.)
                     {
                        wxMessageDialog m(NULL, _("Latency Correction setting has caused the recorded audio to be hidden before zero.\nAudacity has brought it back to start at zero.\nYou may have to use the Time Shift Tool (<---> or F5) to drag the track to the right place."), _("Latency problem"), wxOK);
                        m.ShowModal();
                        track->SetOffset(0.);
                     }
                  }
               }
            }
         
         delete[] mCaptureBuffers;
      }
   }

   if (mInputMeter)
      mInputMeter->Reset(mRate, false);
   if (mOutputMeter)
      mOutputMeter->Reset(mRate, false);

   if (mListener && mNumCaptureChannels > 0)
      mListener->OnAudioIOStopRecording();
      
   //
   // Only set token to 0 after we're totally finished with everything
   //
   mStreamToken = 0;
   mStopStreamCount = 0;
}

void AudioIO::SetPaused(bool state)
{
   mPaused = state;
}

bool AudioIO::IsPaused()
{
   return mPaused;
}

bool AudioIO::IsBusy()
{
   // dmazzoni: Old code...now it's safe to call StartStream if
   // PortAudio is open, just not if there's a token active
 #if 0
   if (IsStreamActive())
      return true;
  #if USE_PORTAUDIO_V19
   if (mPortStreamV19)
      return true;
  #else
   if (mPortStreamV18)
      return true;
  #endif
#endif

   if (mStreamToken != 0)
      return true;

   return false;
}

bool AudioIO::IsStreamActive()
{
#if USE_PORTAUDIO_V19
   if( mPortStreamV19 )
      return Pa_IsStreamActive( mPortStreamV19 ) != 0;
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

bool AudioIO::IsMonitoring()
{
#if USE_PORTAUDIO_V19
   return ( mPortStreamV19 && mStreamToken==0 );
#else
   return ( mPortStreamV18 && mStreamToken==0 );
#endif
}

double AudioIO::NormalizeStreamTime(double absoluteTime) const
{
   // dmazzoni: This function is needed for two reasons:
   // One is for looped-play mode - this function makes sure that the
   // position indicator keeps wrapping around.  The other reason is
   // more subtle - it's because PortAudio can query the hardware for
   // the current stream time, and this query is not always accurate.
   // Sometimes it's a little behind or ahead, and so this function
   // makes sure that at least we clip it to the selection.
   //
   // msmeyer: There is also the possibility that we are using "cut preview"
   //          mode. In this case, we should jump over a defined "gap" in the
   //          audio.

   // msmeyer: Just to be sure, the returned stream time should
   //          never be smaller than the actual start time.
   if (absoluteTime < mT0)
      absoluteTime = mT0;

   if (absoluteTime > mT1)
      absoluteTime = mT1;
   
   if (mCutPreviewGapLen > 0)
   {
      // msmeyer: We're in cut preview mode, so if we are on the right
      // side of the gap, we jump over it.
      if (absoluteTime > mCutPreviewGapStart)
         absoluteTime += mCutPreviewGapLen;
   }

   return absoluteTime;
}

double AudioIO::GetStreamTime()
{
   if( !IsStreamActive() )
      return -1000000000;

   return NormalizeStreamTime(mTime);
}


wxArrayLong AudioIO::GetSupportedPlaybackRates(int devIndex, double rate)
{
   wxArrayLong supported;
   int irate = (int)rate;
   const PaDeviceInfo* devInfo = NULL;
   int i;

   if (devIndex == -1)
   {  // weren't given a device index, get the prefs / default one
      devIndex = getPlayDevIndex();
   }

   devInfo = Pa_GetDeviceInfo(devIndex);
   
   if (!devInfo)
   {
      return supported;
   }

#if USE_PORTAUDIO_V19
   PaStreamParameters pars;

   pars.device = devIndex;
   pars.channelCount = 2;
   pars.sampleFormat = paFloat32;
   pars.suggestedLatency = devInfo->defaultHighOutputLatency;
   pars.hostApiSpecificStreamInfo = NULL;
   
   for (i = 0; i < NumRatesToTry; i++)
   {
      if (Pa_IsFormatSupported(NULL, &pars, RatesToTry[i]) == 0)
      {
         supported.Add(RatesToTry[i]);
      }
   }

   if (irate != 0 && supported.Index(irate) == wxNOT_FOUND)
   {
      if (Pa_IsFormatSupported(NULL, &pars, irate) == 0)
      {
         supported.Add(irate);
      }
   }
#else
   if (devInfo->numSampleRates == -1)
   {
      for (i = 0; i < NumRatesToTry; i++)
      {
         if (RatesToTry[i] >= devInfo->sampleRates[0] &&
             RatesToTry[i] <= devInfo->sampleRates[1])
         {
            supported.Add(RatesToTry[i]);
         }
      }

      if (irate != 0 && supported.Index(irate) == wxNOT_FOUND)
      {
         if (irate >= devInfo->sampleRates[0] &&
             irate <= devInfo->sampleRates[1])
         {
            supported.Add(irate);
         }
      }
   }
   else
   {
      for (i = 0; i < devInfo->numSampleRates; i++)
      {
         supported.Add((int)devInfo->sampleRates[i]);
      }
   }
#endif

   return supported;
}

wxArrayLong AudioIO::GetSupportedCaptureRates(int devIndex, double rate)
{
   wxArrayLong supported;
   int irate = (int)rate;
   const PaDeviceInfo* devInfo = NULL;
   int i;

   if (devIndex == -1)
   {  // not given a device, look up in prefs / default
      devIndex = getRecordDevIndex();
   }
   devInfo = Pa_GetDeviceInfo(devIndex);

   if (!devInfo)
   {
      return supported;
   }

#if USE_PORTAUDIO_V19
   double latencyDuration = DEFAULT_LATENCY_DURATION;
   long recordChannels = 1;
   gPrefs->Read(wxT("/AudioIO/LatencyDuration"), &latencyDuration);
   gPrefs->Read(wxT("/AudioIO/RecordChannels"), &recordChannels);

   PaStreamParameters pars;

   pars.device = devIndex;
   pars.channelCount = recordChannels;
   pars.sampleFormat = paFloat32;
   pars.suggestedLatency = latencyDuration / 1000.0;
   pars.hostApiSpecificStreamInfo = NULL;
   
   for (i = 0; i < NumRatesToTry; i++)
   {
      if (Pa_IsFormatSupported(&pars, NULL, RatesToTry[i]) == 0)
      {
         supported.Add(RatesToTry[i]);
      }
   }

   if (irate != 0 && supported.Index(irate) == wxNOT_FOUND)
   {
      if (Pa_IsFormatSupported(&pars, NULL, irate) == 0)
      {
         supported.Add(irate);
      }
   }
#else
   if (devInfo->numSampleRates == -1)
   {
      for (i = 0; i < NumRatesToTry; i++)
      {
         if (RatesToTry[i] >= devInfo->sampleRates[0] &&
             RatesToTry[i] <= devInfo->sampleRates[1])
         {
            supported.Add(RatesToTry[i]);
         }
      }

      if (irate != 0 && supported.Index(irate) == wxNOT_FOUND)
      {
         if (irate >= devInfo->sampleRates[0] &&
             irate <= devInfo->sampleRates[1])
         {
            supported.Add(irate);
         }
      }
   }
   else
   {
      for (i = 0; i < devInfo->numSampleRates; i++)
      {
         supported.Add((int)devInfo->sampleRates[i]);
      }
   }
#endif

   return supported;
}

wxArrayLong AudioIO::GetSupportedSampleRates(int playDevice, int recDevice, double rate)
{
   wxArrayLong playback = GetSupportedPlaybackRates(playDevice, rate);
   wxArrayLong capture = GetSupportedCaptureRates(recDevice, rate);
   int i;

   // Return only sample rates which are in both arrays
   wxArrayLong result;

   for (i = 0; i < (int)playback.GetCount(); i++)
      if (capture.Index(playback[i]) != wxNOT_FOUND)
         result.Add(playback[i]);

   // If this yields no results, use the default sample rates nevertheless
/*   if (result.IsEmpty())
   {
      for (i = 0; i < NumStandardRates; i++)
         result.Add(StandardRates[i]);
   }*/

   return result;
}

/** \todo: should this take into account PortAudio's value for 
 * PaDeviceInfo::defaultSampleRate? In principal this should let us work out
 * which rates are "real" and which resampled in the drivers, and so prefer
 * the real rates. */
int AudioIO::GetOptimalSupportedSampleRate()
{
   wxArrayLong rates = GetSupportedSampleRates();

   if (rates.Index(44100) != wxNOT_FOUND)
      return 44100;

   if (rates.Index(48000) != wxNOT_FOUND)
      return 48000;

   // if there are no supported rates, the next bit crashes. So check first,
   // and give them a "sensible" value if there are no valid values. They
   // will still get an error later, but with any luck may have changed
   // something by then. It's no worse than having an invalid default rate
   // stored in the preferences, which we don't check for
   if (rates.IsEmpty()) return 44100;

   return rates[rates.GetCount() - 1];
}

double AudioIO::GetBestRate(bool capturing, bool playing, double sampleRate)
{
   wxArrayLong rates;

   if (capturing && !playing) {
      rates = GetSupportedCaptureRates(-1, sampleRate);
   }
   if (playing && !capturing) {
      rates = GetSupportedPlaybackRates(-1, sampleRate);
   }
   else {   // we assume capturing and playing - the alternative would be a 
            // bit odd
      wxArrayLong playrates = GetSupportedPlaybackRates(-1, sampleRate);
      wxArrayLong caprates = GetSupportedCaptureRates(-1, sampleRate);
      int i;
      for (i = 0; i < (int)caprates.GetCount(); i++)  // for each capture rate
         {
         if (playrates.Index(caprates[i]) != wxNOT_FOUND)
            rates.Add(caprates[i]);
         // if the capture rate is also a playback rate, then add to
         // list of rates available
         }
   }
   /* rem rates is the array of hardware-supported sample rates (in the current
    * configuration), sampleRate is the Project Rate (desired sample rate) */
   long rate = (long)sampleRate;
   
   if (rates.Index(rate) != wxNOT_FOUND) {
      return rate;
      /* the easy case - the suggested rate (project rate) is in the list, and
       * we can just accept that and send back to the caller. This should be
       * the case for most users most of the time (all of the time on
       * Win MME as the OS does resampling) */
   }

   /* if we get here, there is a problem - the project rate isn't supported
    * on our hardware, so we can't us it. Need to come up with an alternative
    * rate to use. The process goes like this:
    * * If there are no rates to pick from, we're stuck and return 0 (error)
    * * If there are some rates, we pick the next one higher than the requested
    *   rate to use.
    * * If there aren't any higher, we use the highest available rate */

   if (rates.IsEmpty()) {
      /* we're stuck - there are no supported rates with this hardware. Error */
      return 0;
   }
   int i;
   for (i = 0; i < (int)rates.GetCount(); i++)  // for each supported rate
         {
         if (rates[i] > rate) {
            // supported rate is greater than requested rate
            return rates[i];
         }
         }

   return rates[rates.GetCount() - 1]; // the highest available rate
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

int AudioIO::getRecordDevIndex(wxString devName)
{
   // if we don't get given a device, look up the preferences
   if (devName.IsEmpty())
   {
      devName = gPrefs->Read(wxT("/AudioIO/RecordingDevice"), wxT(""));
   }

   int i;
#if USE_PORTAUDIO_V19
   for (i = 0; i < Pa_GetDeviceCount(); i++)
#else
   for (i = 0; i < Pa_CountDevices(); i++)
#endif
   {
      const PaDeviceInfo* info = Pa_GetDeviceInfo(i);

      if (info && (DeviceName(info) == devName) && (info->maxInputChannels > 0))
      {
         // this device name matches the stored one, and works.
         // So we say this is the answer and return it
         return i;
      }
   }
   // landing here, we either don't have a value in the preferences, or 
   // the stored / supplied value doesn't exist on the system. So we need to
   // use a default value
#if USE_PORTAUDIO_V19
   int recDeviceNum = Pa_GetDefaultInputDevice();
#else
   int recDeviceNum = Pa_GetDefaultInputDeviceID();
#endif
   // Sometimes PortAudio returns -1 if it cannot find a suitable default
   // device, so we just use the first one available
   if (recDeviceNum < 0)
      recDeviceNum = 0;
   return recDeviceNum;
}

int AudioIO::getPlayDevIndex(wxString devName )
{
   // if we don't get given a device, look up the preferences
   if (devName.IsEmpty())
   {
      devName = gPrefs->Read(wxT("/AudioIO/PlaybackDevice"), wxT(""));
   }

   int i;
#if USE_PORTAUDIO_V19
   for (i = 0; i < Pa_GetDeviceCount(); i++)
#else
   for (i = 0; i < Pa_CountDevices(); i++)
#endif
   {
      const PaDeviceInfo* info = Pa_GetDeviceInfo(i);

      if (info && (DeviceName(info) == devName) && (info->maxOutputChannels > 0))
      {
         // this device name matches the stored one, and works.
         // So we say this is the answer and return it
         return i;
      }
   }
   // landing here, we either don't have a value in the preferences, or 
   // the stored / supplied value doesn't exist on the system. So we need to
   // use a default value
#if USE_PORTAUDIO_V19
   int DeviceNum = Pa_GetDefaultOutputDevice();
#else
   int DeviceNum = Pa_GetDefaultOutputDeviceID();
#endif
   // Sometimes PortAudio returns -1 if it cannot find a suitable default
   // device, so we just use the first one available
   if (DeviceNum < 0)
      DeviceNum = 0;
   return DeviceNum;
}

wxString AudioIO::GetDeviceInfo()
{
   wxStringOutputStream o;
   wxTextOutputStream s(o, wxEOL_UNIX);
   wxString e(wxT("\n"));

   if (IsStreamActive()) {
      return wxT("Stream is active ... unable to gather information.");
   }


#if USE_PORTAUDIO_V19
   int recDeviceNum = Pa_GetDefaultInputDevice();
   int playDeviceNum = Pa_GetDefaultOutputDevice();
#else
   int recDeviceNum = Pa_GetDefaultInputDeviceID();
   int playDeviceNum = Pa_GetDefaultOutputDeviceID();
#endif

   s << wxT("==============================") << e;
   s << wxT("Default capture device number: ") << recDeviceNum << e;
   s << wxT("Default playback device number: ") << playDeviceNum << e;

   // Sometimes PortAudio returns -1 if it cannot find a suitable default
   // device, so we just use the first one available
   if (recDeviceNum < 0)
      recDeviceNum = 0;
   if (playDeviceNum < 0)
      playDeviceNum = 0;
      
   wxString recDevice = gPrefs->Read(wxT("/AudioIO/RecordingDevice"), wxT(""));
   wxString playDevice = gPrefs->Read(wxT("/AudioIO/PlaybackDevice"), wxT(""));
   int j;

   // This gets info on all available audio devices (input and output)

#if USE_PORTAUDIO_V19
   int cnt = Pa_GetDeviceCount();
#else
   int cnt = Pa_CountDevices();
#endif

   if (cnt <= 0) {
      s << wxT("No devices found\n");
      return o.GetString();
   }

   const PaDeviceInfo* info;
     
   for (j = 0; j < cnt; j++) {
      s << wxT("==============================") << e;

      info = Pa_GetDeviceInfo(j);
      if (!info) {
         s << wxT("Device info unavailable for: ") << j << wxT("\n");
         continue;
      }

      wxString name = DeviceName(info);

      s << wxT("Device ID: ") << j << e;
      s << wxT("Device name: ") << name << e;
      s << wxT("Input channels: ") << info->maxInputChannels << e;
      s << wxT("Output channels: ") << info->maxOutputChannels << e;
#if USE_PORTAUDIO_V19
      s << wxT("Low Input Latency: ") << info->defaultLowInputLatency << e;
      s << wxT("Low Output Latency: ") << info->defaultLowOutputLatency << e;
      s << wxT("High Input Latency: ") << info->defaultHighInputLatency << e;
      s << wxT("High Output Latency: ") << info->defaultHighOutputLatency << e;
#endif
      wxArrayLong rates = GetSupportedPlaybackRates(j, 0.0);

      s << wxT("Supported Rates:") << e;
      for (int k = 0; k < (int) rates.GetCount(); k++) {
         s << wxT("    ") << (int)rates[k] << e;
      }

      if (name == playDevice && info->maxOutputChannels > 0)
         playDeviceNum = j;

      if (name == recDevice && info->maxInputChannels > 0)
         recDeviceNum = j;
   }

   s << wxT("==============================") << e;
   s << wxT("Selected capture device: ") << recDeviceNum << wxT(" - ") << recDevice << e;
   s << wxT("Selected playback device: ") << playDeviceNum << wxT(" - ") << playDevice << e;

   wxArrayLong supportedSampleRates = GetSupportedSampleRates(playDeviceNum, recDeviceNum);

   s << wxT("Supported Rates:");
   for (int k = 0; k < (int) supportedSampleRates.GetCount(); k++) {
      s << wxT("    ") << (int)supportedSampleRates[k] << e;
   }

#if defined(USE_PORTMIXER)
   int highestSampleRate = supportedSampleRates[supportedSampleRates.GetCount() - 1];
   bool EmulateMixerInputVol = true;
   bool EmulateMixerOutputVol = true;
   float MixerInputVol = 1.0;
   float MixerOutputVol = 1.0;

   int error;

#if USE_PORTAUDIO_V19

   PaStream *stream;

   PaStreamParameters playbackParameters;

   playbackParameters.device = playDeviceNum;
   playbackParameters.sampleFormat = paFloat32;
   playbackParameters.hostApiSpecificStreamInfo = NULL;
   playbackParameters.channelCount = 2;
   if (Pa_GetDeviceInfo(playDeviceNum))
      playbackParameters.suggestedLatency =
         Pa_GetDeviceInfo(playDeviceNum)->defaultLowOutputLatency;
   else
      playbackParameters.suggestedLatency = DEFAULT_LATENCY_CORRECTION/1000.0;

   PaStreamParameters captureParameters;
 
   captureParameters.device = recDeviceNum;
   captureParameters.sampleFormat = paFloat32;;
   captureParameters.hostApiSpecificStreamInfo = NULL;
   captureParameters.channelCount = 2;
   if (Pa_GetDeviceInfo(recDeviceNum))
      captureParameters.suggestedLatency =
         Pa_GetDeviceInfo(recDeviceNum)->defaultLowInputLatency;
   else
      captureParameters.suggestedLatency = DEFAULT_LATENCY_CORRECTION/1000.0; 
 
   error = Pa_OpenStream(&stream,
                         &captureParameters, &playbackParameters,
                         highestSampleRate, paFramesPerBufferUnspecified,
                         paClipOff | paDitherOff,
                         audacityAudioCallback, NULL);

   if (error) {
      error = Pa_OpenStream(&stream,
                            &captureParameters, NULL,
                            highestSampleRate, paFramesPerBufferUnspecified,
                            paClipOff | paDitherOff,
                            audacityAudioCallback, NULL);
   }
  
#else

   PortAudioStream *stream;

   error = Pa_OpenStream(&stream, recDeviceNum, 2, paFloat32, NULL,
                         playDeviceNum, 2, paFloat32, NULL,
                         highestSampleRate, 512, 1, paClipOff | paDitherOff,
                         audacityAudioCallback, NULL);

   if (error) {
      error = Pa_OpenStream(&stream, recDeviceNum, 2, paFloat32, NULL,
                            paNoDevice, 0, paFloat32, NULL,
                            highestSampleRate, 512, 1, paClipOff | paDitherOff,
                            audacityAudioCallback, NULL);
   }

#endif

   if (error) {
      s << wxT("Recieved ") << error << wxT(" while opening devices") << e;
      return o.GetString();
   }

   PxMixer *PortMixer = Px_OpenMixer(stream, 0);

   if (!PortMixer) {
      s << wxT("Unable to open Portmixer") << e;
      Pa_CloseStream(stream);
      return o.GetString();
   }

   s << wxT("==============================") << e;
   s << wxT("Available mixers:") << e;

   cnt = Px_GetNumMixers(stream);
   for (int i = 0; i < cnt; i++) {
      wxString name(Px_GetMixerName(stream, i), wxConvLocal);
      s << i << wxT(" - ") << name << e;
   }

   s << wxT("==============================") << e;
   s << wxT("Available capture sources:") << e;
   cnt = Px_GetNumInputSources(PortMixer);
   for (int i = 0; i < cnt; i++) {
      wxString name(Px_GetInputSourceName(PortMixer, i), wxConvLocal);
      s << i << wxT(" - ") << name << e;
   }

   s << wxT("==============================") << e;
   s << wxT("Available playback volumes:") << e;
   cnt = Px_GetNumOutputVolumes(PortMixer);
   for (int i = 0; i < cnt; i++) {
      wxString name(Px_GetOutputVolumeName(PortMixer, i), wxConvLocal);
      s << i << wxT(" - ") << name << e;
   }

   // Determine mixer capabilities - it it doesn't support either
   // input or output, we emulate them (by multiplying this value
   // by all incoming/outgoing samples)

   MixerOutputVol = Px_GetPCMOutputVolume(PortMixer);
   EmulateMixerOutputVol = false;
   Px_SetPCMOutputVolume(PortMixer, 0.0);
   if (Px_GetPCMOutputVolume(PortMixer) > 0.1)
      EmulateMixerOutputVol = true;
   Px_SetPCMOutputVolume(PortMixer, 0.2f);
   if (Px_GetPCMOutputVolume(PortMixer) < 0.1 ||
       Px_GetPCMOutputVolume(PortMixer) > 0.3)
      EmulateMixerOutputVol = true;
   Px_SetPCMOutputVolume(PortMixer, MixerOutputVol);

   MixerInputVol = Px_GetInputVolume(PortMixer);
   EmulateMixerInputVol = false;
   Px_SetInputVolume(PortMixer, 0.0);
   if (Px_GetInputVolume(PortMixer) > 0.1)
      EmulateMixerInputVol = true;
   Px_SetInputVolume(PortMixer, 0.2f);
   if (Px_GetInputVolume(PortMixer) < 0.1 ||
       Px_GetInputVolume(PortMixer) > 0.3)
      EmulateMixerInputVol = true;
   Px_SetInputVolume(PortMixer, MixerInputVol);

   Pa_CloseStream(stream);

   s << wxT("==============================") << e;
   s << wxT("Capture volume is ") << (EmulateMixerInputVol? wxT("emulated"): wxT("native")) << e;
   s << wxT("Capture volume is ") << (EmulateMixerOutputVol? wxT("emulated"): wxT("native")) << e;

   Px_CloseMixer(PortMixer);

#endif

   return o.GetString();
}

// This method is the data gateway between the audio thread (which
// communicates with the disk) and the PortAudio callback thread
// (which communicates with the audio device.
void AudioIO::FillBuffers()
{
   unsigned int i;

   gAudioIO->mAudioThreadFillBuffersLoopActive = true;

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
          (!mPlayLooped && (secsAvail > 0 && mT+secsAvail >= mWarpedT1)))
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
            if( mT + deltat > mWarpedT1 )
            {
               deltat = mWarpedT1 - mT;
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
                  mPlaybackMixers[i]->Process(lrint(deltat * mRate));
               samplePtr warpedSamples = mPlaybackMixers[i]->GetBuffer();
               mPlaybackBuffers[i]->Put(warpedSamples, floatSample, processed);
            }

            // msmeyer: If playing looped, check if we are at the end of the buffer
            // and if yes, restart from the beginning.
            if (mPlayLooped && mT >= mWarpedT1)
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
         XMLStringWriter blockFileLog;
         int numChannels = mCaptureTracks.GetCount();
         
         for( i = 0; (int)i < numChannels; i++ )
         {
            int avail = commonlyAvail;
            sampleFormat trackFormat = mCaptureTracks[i]->GetSampleFormat();

            XMLStringWriter appendLog;

            if( mFactor == 1.0 )
            {
               samplePtr temp = NewSamples(avail, trackFormat);
               mCaptureBuffers[i]->Get   (temp, trackFormat, avail);
               mCaptureTracks[i]-> Append(temp, trackFormat, avail, 1, 
                                          &appendLog);
               DeleteSamples(temp);
            }
            else
            {
               int size = lrint(avail * mFactor);
               samplePtr temp1 = NewSamples(avail, floatSample);
               samplePtr temp2 = NewSamples(size, floatSample);
               mCaptureBuffers[i]->Get(temp1, floatSample, avail);

               size = mResample[i]->Process(mFactor, (float *)temp1, avail, true,
                                            &size, (float *)temp2, size);
               mCaptureTracks[i]-> Append(temp2, floatSample, size, 1, 
                                          &appendLog);
               DeleteSamples(temp1);
               DeleteSamples(temp2);
            }

            if (!appendLog.IsEmpty())
            {
               blockFileLog.StartTag(wxT("recordingrecovery"));
               blockFileLog.WriteAttr(wxT("channel"), (int)i);
               blockFileLog.WriteAttr(wxT("numchannels"), numChannels);
               blockFileLog.WriteSubTree(appendLog);
               blockFileLog.EndTag(wxT("recordingrecovery"));
            }
         }
         
         if (mListener && !blockFileLog.IsEmpty())
            mListener->OnAudioIONewBlockFiles(blockFileLog);
      }
   }

   gAudioIO->mAudioThreadFillBuffersLoopActive = false;
}

//////////////////////////////////////////////////////////////////////
//
//    PortAudio callback thread context
//
//////////////////////////////////////////////////////////////////////

#define MAX(a,b) ((a) > (b) ? (a) : (b))

void DoSoftwarePlaythrough(const void *inputBuffer,
                           sampleFormat inputFormat,
                           int inputChannels,
                           float *outputBuffer,
                           int len,
                           float gain)
{
   float *tempBuffer = (float *)alloca(len * sizeof(float));
   int i, j;

   for(j=0; j<inputChannels; j++) {
      samplePtr inputPtr = ((samplePtr)inputBuffer) + (j * SAMPLE_SIZE(inputFormat));
      
      CopySamples(inputPtr, inputFormat,
                  (samplePtr)tempBuffer, floatSample,
                  len, true, inputChannels);

      for(i=0; i<len; i++)
         outputBuffer[2*i + (j%2)] = tempBuffer[i];

      // One mono input channel goes to both output channels...
      if (inputChannels == 1)
         for(i=0; i<len; i++)
            outputBuffer[2*i + 1] = tempBuffer[i];
   }
   
}

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
   void *tempBuffer = alloca(framesPerBuffer*sizeof(float)*
                             MAX(numCaptureChannels,numPlaybackChannels));
   float *tempFloats = (float*)tempBuffer;
   unsigned int i;
   int t;

   // Send data to VU meters

   // It's critical that we don't update the meters while
   // StopStream is trying to stop PortAudio, otherwise it can
   // lead to a freeze.  We use two variables to synchronize:
   // mUpdatingMeters tells StopStream when the callback is about
   // to enter the code where it might update the meters, and
   // mUpdateMeters is how the rest of the code tells the callback
   // when it is allowed to actually do the updating.  Note that
   // mUpdatingMeters must be set first to avoid a race condition.
   gAudioIO->mUpdatingMeters = true;
   if (gAudioIO->mUpdateMeters) {

      if (gAudioIO->mOutputMeter && 
          !gAudioIO->mOutputMeter->IsMeterDisabled() &&
          outputBuffer) {
         gAudioIO->mOutputMeter->UpdateDisplay(numPlaybackChannels,
                                               framesPerBuffer,
                                               (float *)outputBuffer);
      }

      if (gAudioIO->mInputMeter &&
          !gAudioIO->mInputMeter->IsMeterDisabled() &&
          inputBuffer) {
         if (gAudioIO->mCaptureFormat == floatSample)
            gAudioIO->mInputMeter->UpdateDisplay(numCaptureChannels,
                                                 framesPerBuffer,
                                                 (float *)inputBuffer);
         else {
            CopySamples((samplePtr)inputBuffer, gAudioIO->mCaptureFormat,
                        (samplePtr)tempFloats, floatSample,
                        framesPerBuffer * numCaptureChannels);
            gAudioIO->mInputMeter->UpdateDisplay(numCaptureChannels,
                                                 framesPerBuffer,
                                                 tempFloats);
         }
      }
   }
   gAudioIO->mUpdatingMeters = false;

#ifdef EXPERIMENTAL_SMART_RECORD
   // Stop recording if 'silence' is detected
   if(gAudioIO->mPauseRec && inputBuffer) {
      if(gAudioIO->mInputMeter->GetMaxPeak() < gAudioIO->mSilenceLevel ) {
         if(!gAudioIO->IsPaused()) {
            AudacityProject *p = GetActiveProject();
            wxCommandEvent dummyEvt;
            p->GetControlToolBar()->OnPause(dummyEvt);
         }
      }
      else {
         if(gAudioIO->IsPaused()) {
            AudacityProject *p = GetActiveProject();
            wxCommandEvent dummyEvt;
            p->GetControlToolBar()->OnPause(dummyEvt);
         }
      }
   }
#endif
   if( gAudioIO->mPaused )
   {
      if (outputBuffer && numPlaybackChannels > 0)
      {
         ClearSamples((samplePtr)outputBuffer, floatSample,
                      0, framesPerBuffer * numPlaybackChannels);

         if (inputBuffer && gAudioIO->mSoftwarePlaythrough) {
            float gain = 1.0;
            if (gAudioIO->mEmulateMixerInputVol)
               gain = gAudioIO->mMixerInputVol;

            DoSoftwarePlaythrough(inputBuffer, gAudioIO->mCaptureFormat,
                                  numCaptureChannels,
                                  (float *)outputBuffer, (int)framesPerBuffer, gain);
         }
      }

      return paContinue;
   }

   if (gAudioIO->mStreamToken > 0)
   {
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

         if (inputBuffer && gAudioIO->mSoftwarePlaythrough) {
            float gain = 1.0;
            if (gAudioIO->mEmulateMixerInputVol)
               gain = gAudioIO->mMixerInputVol;

            DoSoftwarePlaythrough(inputBuffer, gAudioIO->mCaptureFormat,
                                  numCaptureChannels,
                                  (float *)outputBuffer, (int)framesPerBuffer, gain);
         }

         if (gAudioIO->mSeek)
         {
            // Pause audio thread and wait for it to finish
            gAudioIO->mAudioThreadFillBuffersLoopRunning = false;
            while( gAudioIO->mAudioThreadFillBuffersLoopActive == true )
            {
               wxMilliSleep( 50 );
            }

            // Calculate the new time position
            gAudioIO->mTime += gAudioIO->mSeek;
            if (gAudioIO->mTime < gAudioIO->mT0)
                gAudioIO->mTime = gAudioIO->mT0;
            else if (gAudioIO->mTime > gAudioIO->mT1)
                gAudioIO->mTime = gAudioIO->mT1;
            gAudioIO->mSeek = 0.0;
            
            // Reset mixer positions and flush buffers for all tracks
            gAudioIO->mT = gAudioIO->mT0 + ((gAudioIO->mTime - gAudioIO->mT0));
            for (i = 0; i < (unsigned int)numPlaybackTracks; i++)
            {
               gAudioIO->mPlaybackMixers[i]->Reposition(gAudioIO->mT);
               gAudioIO->mPlaybackBuffers[i]->Discard(gAudioIO->mPlaybackBuffers[i]->AvailForGet());
            }

            // Reload the ring buffers
            gAudioIO->mAudioThreadShouldCallFillBuffersOnce = true;
            while( gAudioIO->mAudioThreadShouldCallFillBuffersOnce == true )
            {
               wxMilliSleep( 50 );
            }

            // Reenable the audio thread
            gAudioIO->mAudioThreadFillBuffersLoopRunning = true;
               
            return paContinue;
         }

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
               gAudioIO->mPlaybackBuffers[t]->Get((samplePtr)tempFloats,
                                                  floatSample,
                                                  (int)framesPerBuffer);

            // If our buffer is empty and the time indicator is past
            // the end, then we've actually finished playing the entire
            // selection.
            // msmeyer: We never finish if we are playing looped
            if (len == 0 && gAudioIO->mTime >= gAudioIO->mT1 &&
                !gAudioIO->mPlayLooped)
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
            wxPrintf(wxT("lost %d samples\n"), (int)(framesPerBuffer - len));
         }

         float gain = 1.0;
         
         if (gAudioIO->mEmulateMixerInputVol)
            gain = gAudioIO->mMixerInputVol;

         if (len > 0) {
            for( t = 0; t < numCaptureChannels; t++) {
               
               // dmazzoni:
               // Un-interleave.  Ugly special-case code required because the
               // capture channels could be in three different sample formats;
               // it'd be nice to be able to call CopySamples, but it can't
               // handle multiplying by the gain and then clipping.  Bummer.

               switch(gAudioIO->mCaptureFormat) {
               case floatSample: {
                  float *inputFloats = (float *)inputBuffer;
                  for( i = 0; i < len; i++)
                     tempFloats[i] =
                        inputFloats[numCaptureChannels*i+t] * gain;
               } break;
               case int24Sample:
                  // We should never get here. Audacity's int24Sample format
                  // is different from PortAudio's sample format and so we
                  // make PortAudio return float samples when recording in
                  // 24-bit samples.
                  wxASSERT(false);
                  break;
               case int16Sample: {
                  short *inputShorts = (short *)inputBuffer;
                  short *tempShorts = (short *)tempBuffer;
                  for( i = 0; i < len; i++) {
                     float tmp = inputShorts[numCaptureChannels*i+t] * gain;
                     if (tmp > 32767)
                        tmp = 32767;
                     if (tmp < -32768)
                        tmp = -32768;
                     tempShorts[i] = (short)(tmp);
                  }
               } break;
               } // switch
               
               gAudioIO->mCaptureBuffers[t]->Put((samplePtr)tempBuffer,
                                                 gAudioIO->mCaptureFormat,
                                                 len);
            }
         }
      }

      // Calcuate the warp factor for this time position
      double factor = 1.0;
      if (gAudioIO->mTimeTrack) {
         factor = gAudioIO->mTimeTrack->GetEnvelope()->GetValue(gAudioIO->mTime);
         factor = (gAudioIO->mTimeTrack->GetRangeLower() *
                  (1 - factor) +
                  factor *
                  gAudioIO->mTimeTrack->GetRangeUpper()) / 
                  100.0;
      }

      // Wrap to start if looping
      if (gAudioIO->mPlayLooped && gAudioIO->mTime >= gAudioIO->mT1)
      {
         // LL:  This is not exactly right, but I'm at my wits end trying to
         //      figure it out.  Feel free to fix it.  :-)
         gAudioIO->mTime = gAudioIO->mT0 - ((gAudioIO->mTime - gAudioIO->mT1) * factor);
      }

      // Update the current time position
      gAudioIO->mTime += ((framesPerBuffer / gAudioIO->mRate) * factor);

      // Record the reported latency from PortAudio.
      // TODO: Don't recalculate this with every callback?
     #if USE_PORTAUDIO_V19
      // As of 06/17/2006, portaudio v19 returns inputBufferAdcTime set to
      // zero.  It is being worked on, but for now we just can't do much
      // but follow the leader.
      //
      // 08/27/2006: too inconsistent for now...just leave it a zero.
      //
      // 04/16/2008: Looks like si->inputLatency comes back with something useful though.
      // This rearranged logic uses si->inputLatency, but if PortAudio fixes inputBufferAdcTime, 
      // this code won't have to be modified to use it. 
      // Also avoids setting mLastRecordingOffset except when simultaneously playing and recording.
      //
      if (numCaptureChannels > 0 && numPlaybackChannels > 0) // simultaneously playing and recording
      {
         if (timeInfo->inputBufferAdcTime > 0)
            gAudioIO->mLastRecordingOffset = timeInfo->inputBufferAdcTime - timeInfo->outputBufferDacTime;
         else if (gAudioIO->mLastRecordingOffset == 0.0) 
         {
            const PaStreamInfo* si = Pa_GetStreamInfo( gAudioIO->mPortStreamV19 );
            gAudioIO->mLastRecordingOffset = -si->inputLatency;
         }
      }
     #else
      if (numCaptureChannels > 0 && numPlaybackChannels > 0)
         gAudioIO->mLastRecordingOffset = (Pa_StreamTime(gAudioIO->mPortStreamV18) - outTime) / gAudioIO->mRate;
      else
         gAudioIO->mLastRecordingOffset = 0;
     #endif

   } // if mStreamToken > 0
   else {
      // No tracks to play, but we should clear the output, and
      // possibly do software playthrough...
      
      if( outputBuffer && (numPlaybackChannels > 0) ) {
         float *outputFloats = (float *)outputBuffer;
         for( i = 0; i < framesPerBuffer*numPlaybackChannels; i++)
            outputFloats[i] = 0.0;
         
         if (inputBuffer && gAudioIO->mSoftwarePlaythrough) {
            float gain = 1.0;
            if (gAudioIO->mEmulateMixerInputVol)
               gain = gAudioIO->mMixerInputVol;
            
            DoSoftwarePlaythrough(inputBuffer, gAudioIO->mCaptureFormat,
                                  numCaptureChannels,
                                  (float *)outputBuffer, (int)framesPerBuffer, gain);
         }
      }
   }

   return callbackReturn;
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 7ee3c9aa-b58b-4069-8a07-8866f2303963

