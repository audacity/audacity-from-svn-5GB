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
    * identified by this token, but doesn't return true if the device
    * has been closed but some disk i/o or cleanup is still going on
    */
   bool IsStreamActive(int token);
   /* Returns true if the stream is active, or even if audio I/O is
    * busy cleaning up its data or writing to disk.  This is used
    * by TrackPanel to determine when a track has been completely
    * recorded, and it's safe to flush to disk.
   */
   bool IsAudioTokenActive(int token);

   void SetPaused(bool state);
   bool IsPaused();

   /* Mixer services are always available.  If no stream is running, these
    * methods use whatever device is specified by the preferences.  If a
    * stream *is* running, naturally they manipulate the mixer associated
    * with that stream.  If no mixer is available for whatever reason, the
    * set method does nothing and the get method returns zeros (which is
    * about the sanest thing they can do from a GUI perspective. */
   void SetMixer(int inputSource, float inputVolume,
                 float playbackVolume);
   void GetMixer(int *inputSource, float *inputVolume,
                 float *playbackVolume);
   wxArrayString GetInputSourceNames();
   void HandleDeviceChange();

   // This is given in seconds based on starting at t0
   double GetStreamTime();
   sampleFormat GetCaptureFormat() { return mCaptureFormat; }
   int GetNumCaptureChannels() { return mNumCaptureChannels; }

 private:

   void FillBuffers();

   int GetCommonlyAvailPlayback();
   int GetCommonlyAvailCapture();

   AudioThread        *mThread;
   RingBuffer        **mCaptureBuffers;
   WaveTrackArray      mCaptureTracks;
   RingBuffer        **mPlaybackBuffers;
   WaveTrackArray      mPlaybackTracks;
   Mixer             **mPlaybackMixers;
   int                 mStreamToken;
   static int          mNextStreamToken;
   double              mRate;
   double              mT;
   double              mT0;
   double              mT1;
   double              mPlaybackRingBufferSecs;
   double              mCaptureRingBufferSecs;
   double              mMaxPlaybackSecsToCopy;
   double              mMinCaptureSecsToCopy;
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
