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

#include "RingBuffer.h"
#include "SampleFormat.h"
#include "WaveTrack.h"

class AudioIO;
class AudacityProject;

extern AudioIO *gAudioIO;

void InitAudioIO();

class AudioThread : public wxThread {
 public:
   AudioThread();
   virtual ExitCode Entry();
};

class AudioIO {

 public:
   AudioIO();
   ~AudioIO();

   bool StartPlay(AudacityProject * project,
                  TrackList * tracks, double t0, double t1);

   bool StartRecord(AudacityProject * project,
                    TrackList * tracks, double t0, double t1);

   void Stop();
   void HardStop();
   void SetPaused(bool state);
   bool GetPaused(bool bIgnoreFirstPause = false);
   void SetAlwaysEnablePause(bool bEnable);

   bool IsBusy();
   bool IsPlaying();
   bool IsRecording(Track *t = NULL);

   // AS: This would be more properly named GetCurrentTime or
   //  something like that.  The indicator is the little graphic
   //  displayed in TrackPanel showing the current position while
   //  you're playing or recording.
   double GetIndicator();

   AudacityProject *GetProject();
   sampleFormat GetFormat();

 private:

   bool Start();
   bool OpenDevice();
   void AdjustMixer();
   void PrepareOutTracks(TrackList * tracks);
   void FillBuffers();
   void AddDroppedSamples(sampleCount nSamples);
   double GetPauseIndicator();
   void Finish();

   wxCriticalSection   mFinishSection;
   wxCriticalSection   mStopSection;
   AudioThread        *mThread;
   AudacityProject    *mProject;
   TrackList          *mTracks;
   sampleCount         mInBufferSize;
   RingBuffer        **mInBuffers;
   WaveTrack         **mInTracks;
   int                 mNumOutTracks;
   sampleCount         mOutBufferSize;
   RingBuffer        **mOutBuffers;
   WaveTrack         **mOutTracks;
   double              mRate;
   double              mT;
   double              mRecT;
   double              mT0;
   double              mT1;
   bool                mStopping;
   bool                mHardStop;
   bool                mPaused;
   bool                mAlwaysEnablePause;
   bool                mStarted;
   bool                mReachedEnd;
   double              mPausePosition;
   PortAudioStream    *mPortStream;
   double              mLastIndicator;
   double              mLastStableIndicator;
   int                 mPortAudioBufferSize;
   unsigned int        mNumInChannels;
   unsigned int        mNumOutChannels;
   sampleFormat        mFormat;
   float              *mTempFloats;
   int                 mDroppedSamples;
   int                 mLostSamples;
   bool                mFirstPause;

   #if USE_PORTMIXER
   PxMixer            *mMixer;
   #endif

   friend class AudioThread;

   friend void InitAudioIO();
   
   friend int audacityAudioCallback(
		void *inputBuffer, void *outputBuffer,
		unsigned long framesPerBuffer,
		PaTimestamp outTime, void *userData );
};

#endif
