/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.cpp

  Dominic Mazzoni

**********************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

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

AudioIO *gAudioIO;

void InitAudioIO()
{
  gAudioIO = new AudioIO();
  
}

AudioIO::AudioIO()
{
  mProject = NULL;
  mTracks = NULL;
  mStop = false;
  mRecordLeft = NULL;
  mRecordRight = NULL;
  // Run our timer function once every 100 ms, i.e. 10 times/sec
  mTimer.Start(100, FALSE);
}

AudioIO::~AudioIO()
{
}

bool AudioIO::StartPlay(AudacityProject *project,
                        TrackList *tracks,
                        double t0, double t1)
{
  if (mProject)
	return false;

  mRecording = false;
  mTracks = tracks;
  mT0 = t0;
  mT1 = t1;
  mT = mT0;

  mSndNode.device = SND_DEVICE_AUDIO;
  mSndNode.write_flag = SND_WRITE;
  mSndNode.format.channels = 2;
  mSndNode.format.mode = SND_MODE_PCM;
  mSndNode.format.bits = 16;
  mSndNode.format.srate = project->GetRate();
  strcpy(mSndNode.u.audio.devicename,"");
  strcpy(mSndNode.u.audio.interfacename,"");
  mSndNode.u.audio.descriptor = 0;
  mSndNode.u.audio.protocol = SND_COMPUTEAHEAD;
  mSndNode.u.audio.latency = 1.0;
  mSndNode.u.audio.granularity = 0.0;

  long flags = 0;
  int err = snd_open(&mSndNode, &flags);

  if (err) {
	wxMessageBox(wxString::Format("Error opening audio device: %d",err));
	return false;
  }
  
  mStopWatch.Start(0);

  #ifdef __WXMAC__
  mStartTicks = TickCount();
  #endif

  mTicks = 0;

  // Do this last because this is what signals the timer to go
  mProject = project;

  return true;
}

void AudioIO::Finish()
{
  snd_close(&mSndNode);

  mProject->GetAPalette()->SetPlay(false);
  mProject->GetAPalette()->SetStop(false);
  mProject->GetAPalette()->SetRecord(false);
  mStop = false;

  if (mRecording) {
    mProject->TP_PushState();
  }

  // TODO mProject->SoundDone();
  mProject = NULL;
  mRecordLeft = NULL;
  mRecordRight = NULL;
}

bool AudioIO::StartRecord(AudacityProject *project,
                          TrackList *tracks)
{
  if (mProject)
	return false;

  mRecordLeft = new WaveTrack(project->GetDirManager());
  mRecordLeft->selected = true;
  mRecordLeft->channel = VTrack::LeftChannel;

  mRecordRight = new WaveTrack(project->GetDirManager());
  mRecordRight->selected = true;
  mRecordRight->channel = VTrack::RightChannel;

  project->SelectNone();

  tracks->Add(mRecordLeft);
  tracks->Add(mRecordRight);

  mRecording = true;
  mTracks = tracks;
  mT0 = 0.0;
  mT1 = 0.0;
  mT = 0.0;

  mSndNode.device = SND_DEVICE_AUDIO;
  mSndNode.write_flag = SND_READ;
  mSndNode.format.channels = 2;
  mSndNode.format.mode = SND_MODE_PCM;
  mSndNode.format.bits = 16;
  mSndNode.format.srate = project->GetRate();
  strcpy(mSndNode.u.audio.devicename,"");
  strcpy(mSndNode.u.audio.interfacename,"");
  mSndNode.u.audio.descriptor = 0;
  mSndNode.u.audio.protocol = SND_COMPUTEAHEAD;
  mSndNode.u.audio.latency = 0.25;
  mSndNode.u.audio.granularity = 0.0;

  long flags = 0;
  int err = snd_open(&mSndNode, &flags);

  if (err) {
	wxMessageBox(wxString::Format("Error opening audio device: %d",err));
	return false;
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
}

void AudioIO::OnTimer()
{
  if (!mProject) return;

  if (mRecording) {
	int block = snd_poll(&mSndNode);

	if (block <= 0)
	  return;

	sampleType *in = new sampleType[block*2];
	sampleType *left = new sampleType[block];
	sampleType *right = new sampleType[block];

	snd_read(&mSndNode, in, block);

	for(int i=0; i<block; i++) {
	  left[i] = in[2*i];
	  right[i] = in[2*i+1];
	}
	
	mRecordLeft->Append(left, block);
	mRecordRight->Append(right, block);

        mProject->RedrawProject();

	delete[] in;
	delete[] left;
	delete[] right;

        if (mStop)
          Finish();

	return;
  }

  if (mStop) {
    Finish();
    return;
  }

  if (mT>=mT1) {
    if (GetIndicator() >= mT1) {
      if (snd_flush(&mSndNode) == SND_SUCCESS) {
        Finish();
        return;
      }
    }
  }

  // TODO: Don't fill the buffer with more data every time
  // timer is called

  double deltat = mSndNode.u.audio.latency;
  if (mT + deltat > mT1)
	deltat = mT1 - mT;
  
  int maxFrames = int(mProject->GetRate() * deltat);
  int block = snd_poll(&mSndNode);
  
  if (block == 0)
	return;
  
  if (block > maxFrames)
	block = maxFrames;
  
  if (block < maxFrames) {
	deltat = block / mProject->GetRate();
  }
  
  Mixer *mixer = new Mixer(2, block, true);
  mixer->UseVolumeSlider(mProject->GetAPalette());
  mixer->Clear();
  
  TrackListIterator iter(mTracks);
  
  VTrack *vt = iter.First();
  while(vt) {
	if (vt->GetKind() == VTrack::Wave) {
	  WaveTrack *t = (WaveTrack *)vt;
	  
	  switch(t->channel) {
	  case VTrack::LeftChannel:
		mixer->MixLeft(t, mT, mT+deltat);
		break;
	  case VTrack::RightChannel:
		mixer->MixRight(t, mT, mT+deltat);
		break;
	  case VTrack::MonoChannel:
		mixer->MixMono(t, mT, mT+deltat);
		break;
	  }
	}
	
	vt = iter.Next();
  }
  
  sampleType *outbytes = mixer->GetBuffer();
  snd_write(&mSndNode, outbytes, block);
  if (mT + deltat >= mT1)
    snd_flush(&mSndNode);
  
  delete mixer;
  
  mT += deltat;
}

void AudioIO::Stop()
{
  mStop = true;
}

bool AudioIO::IsBusy()
{
  return (mProject != NULL);
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
  double i;

#ifdef __WXMAC__
  i = mT0 + ((TickCount() - mStartTicks) / 60.0);
#else
  i = mT0 + (mStopWatch.Time() / 1000.0);
#endif

  if (!mRecording && i > mT1)
    i = mT1;
    
  return i;
}
