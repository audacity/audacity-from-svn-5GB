/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.h

  Dominic Mazzoni

  Use the PortAudio library to play and record sound

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO__
#define __AUDACITY_AUDIO_IO__

#include "portaudio.h"

#include "Audacity.h"

#if USE_PORTMIXER
#include "portmixer.h"
#endif

#include <wx/string.h>
#include <wx/thread.h>

#include "WaveTrack.h"
#include "SampleFormat.h"

#ifdef paUseHostApiSpecificDeviceSpecification
#define USE_PORTAUDIO_V19 1
#endif

class AudioIO;
class RingBuffer;
class Mixer;
class TimeTrack;

extern AudioIO *gAudioIO;

void InitAudioIO();
void DeinitAudioIO();

class AudioThread : public wxThread {
 public:
   AudioThread();
   virtual ExitCode Entry();
};

class AudioIO {

 public:
   AudioIO();
   ~AudioIO();

   /* If successful, returns a token identifying this particular stream
    * instance.  For use with IsStreamActive() below */
   int StartStream(WaveTrackArray playbackTracks, WaveTrackArray captureTracks,
                   TimeTrack *timeTrack, double sampleRate,
                   double t0, double t1);

   void StopStream();
   /* Returns true if the audio i/o is running at all */
   bool IsStreamActive();
   /* Returns true if the audio i/o is still running the stream instance
    * identified by this token */
   bool IsStreamActive(int token);

   void SetPaused(bool state);
   bool IsPaused();

   // This is given in seconds based on starting at t0
   double GetStreamTime();
   sampleFormat GetCaptureFormat() { return mCaptureFormat; }
   int GetNumCaptureChannels() { return mNumCaptureChannels; }

 private:

   void AdjustMixer();
   void FillBuffers();

   AudioThread        *mThread;
   RingBuffer        **mCaptureBuffers;
   WaveTrackArray      mCaptureTracks;
   RingBuffer        **mPlaybackBuffers;
   WaveTrackArray      mPlaybackTracks;
   Mixer             **mPlaybackMixers;
   int                 mStreamToken;
   double              mRate;
   double              mT;
   double              mT0;
   double              mT1;
   bool                mPaused;
   double              mPausePosition;
#if USE_PORTAUDIO_V19
   PaStream           *mPortStreamV19;
#else
   PortAudioStream    *mPortStreamV18;
   double              mLastIndicator;
   double              mLastStableIndicator;
   volatile double     mPausedSeconds;
   volatile bool       mInCallbackFinishedState;
#endif
   unsigned int        mNumCaptureChannels;
   unsigned int        mNumPlaybackChannels;
   sampleFormat        mCaptureFormat;
   float              *mTempFloats;
   int                 mLostSamples;
   volatile bool       mAudioThreadShouldCallFillBuffersOnce;
   volatile bool       mAudioThreadFillBuffersLoopRunning;
   volatile double     mLastBufferAudibleTime;
   volatile double     mTotalSamplesPlayed;

   #if USE_PORTMIXER
   PxMixer            *mPortMixer;
   #endif

   friend class AudioThread;

   friend void InitAudioIO();
   friend void DeinitAudioIO();

#if USE_PORTAUDIO_V19
   friend int audacityAudioCallback(
                void *inputBuffer, void *outputBuffer,
                unsigned long framesPerBuffer,
                const PaStreamCallbackTimeInfo *timeInfo,
                PaStreamCallbackFlags statusFlags, void *userData );
#else
   friend int audacityAudioCallback(
                void *inputBuffer, void *outputBuffer,
                unsigned long framesPerBuffer,
                PaTimestamp outTime, void *userData );
#endif

};

#endif
