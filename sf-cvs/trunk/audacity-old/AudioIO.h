/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.h

  Dominic Mazzoni

  Use the SND library to play and record sound

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO__
#define __AUDACITY_AUDIO_IO__

#include "snd/snd.h"

#include <wx/string.h>
#include <wx/timer.h>

#include "WaveTrack.h"

class AudioIO;
class AudacityProject;

extern AudioIO *gAudioIO;

void InitAudioIO();

class AudioIOTimer:public wxTimer {
 public:
   virtual void Notify();
};

class AudioIO {

 public:
   AudioIO();
   ~AudioIO();

   bool StartPlay(AudacityProject * project,
                  TrackList * tracks, double t0, double t1);

   bool StartRecord(AudacityProject * project, TrackList * tracks);

   void OnTimer();

   void Stop();
   bool IsBusy();
   bool IsPlaying();
   bool IsRecording();

   AudacityProject *GetProject();
   double GetIndicator();

 private:

   void Finish();
   bool OpenPlaybackDevice(AudacityProject * project);

   AudacityProject *mProject;
   TrackList *mTracks;
   double mT;
   double mRecT;
   double mT0;
   double mT1;
   int mTicks;
   bool mStop;
   snd_node mPlayNode;
   snd_node mRecordNode;

   bool mRecordStereo;
   bool mDuplex;                // play and record at same time

   bool mRecording;
   WaveTrack *mRecordLeft;
   WaveTrack *mRecordRight;

   AudioIOTimer mTimer;
   wxStopWatch mStopWatch;

#ifdef __WXMAC__
   int mStartTicks;
#endif
};

#endif
