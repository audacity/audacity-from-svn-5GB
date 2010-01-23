/**********************************************************************

  Audacity: A Digital Audio Editor

  Import.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/file.h>
#include <wx/string.h>
#include <wx/thread.h>
#include <wx/timer.h>
#include <wx/msgdlg.h>
#include <wx/generic/progdlgg.h>

#include "WaveTrack.h"
#include "DirManager.h"

#include "snd/snd.h"

bool ImportWAV(wxString fName, WaveTrack **dest1, WaveTrack **dest2, DirManager *dirManager)
{
  *dest1 = 0;
  *dest2 = 0;

  snd_node sndfile;
  snd_node sndbuffer;
  
  sndfile.device = SND_DEVICE_FILE;
  sndfile.write_flag = SND_READ;
  strcpy(sndfile.u.file.filename, (const char *)fName);
  sndfile.u.file.file = 0;
  
  int err;
  long flags=0;
  
  err = snd_open(&sndfile, &flags);
  if (err) return false;
  
  if (sndfile.format.channels < 1 || sndfile.format.channels > 2)
  {
    wxMessageBox("Unknown audio format.");
	  return false;
  }

  *dest1 = new WaveTrack(dirManager);
  wxASSERT(*dest1);
  (*dest1)->rate = sndfile.format.srate;
  if (sndfile.format.channels == 2) {
    *dest2 = new WaveTrack(dirManager);
    wxASSERT(*dest1);
    (*dest2)->rate = sndfile.format.srate;
  }

  sndbuffer.device = SND_DEVICE_MEM;
  sndbuffer.write_flag = SND_WRITE;
  sndbuffer.u.mem.buffer_max = 0;
  sndbuffer.u.mem.buffer = 0;
  sndbuffer.u.mem.buffer_len = 0;
  sndbuffer.u.mem.buffer_pos = 0;
  sndbuffer.format.channels = sndfile.format.channels;
  sndbuffer.format.mode = SND_MODE_PCM; // SND_MODE_FLOAT
  sndbuffer.format.bits = 16;
  sndbuffer.format.srate = sndfile.format.srate;

  int maxblocksize = WaveTrack::GetIdealBlockSize();

  char *srcbuffer = new char[maxblocksize*2];
  char *dstbuffer = new char[maxblocksize*2];
  char *leftbuffer = new char[maxblocksize*2];
  char *rightbuffer = new char[maxblocksize*2];

  long filelen = sndfile.u.file.end_offset - sndfile.u.file.byte_offset;
  long bytescompleted = 0;

  wxProgressDialog *progress = NULL;  
  wxYield();
  wxStartTimer();
  wxBusyCursor busy;

  long block;
  do {
    block = maxblocksize;
    block = snd_read(&sndfile, srcbuffer, block);
    if (block > 0) {
      long b2 = snd_convert(&sndbuffer, dstbuffer,  // to
			    &sndfile, srcbuffer, block);      // from
      if (sndfile.format.channels == 1)
	(*dest1)->Append((sampleType *)dstbuffer, b2);
      else {
	for(int i=0; i<b2; i++) {
	  ((sampleType *)leftbuffer)[i] = ((sampleType *)dstbuffer)[2*i];
	  ((sampleType *)rightbuffer)[i] = ((sampleType *)dstbuffer)[2*i+1];
	}
	(*dest1)->Append((sampleType *)leftbuffer, (sampleCount)b2);
	(*dest2)->Append((sampleType *)rightbuffer, (sampleCount)b2);
      }
      
      bytescompleted += block;
      
    }
    
	if (!progress && wxGetElapsedTime(false) > 500) {
	  progress =
		new wxProgressDialog("Import","Importing audio file",
							 filelen);
	}	
	if (progress) {
	  int progressvalue = (bytescompleted > filelen)? filelen : bytescompleted;
	  progress->Update(progressvalue);
	}
  } while (block > 0);

  snd_close(&sndfile);

  #ifndef __WXMAC__
  if (bytescompleted != filelen) {
	printf("Bytes completed: %d   Expected file len: %d\n",
		   bytescompleted, filelen);
  }
  #endif

  if (progress)
	delete progress;
  
  delete[] srcbuffer;
  delete[] dstbuffer;
  delete[] leftbuffer;
  delete[] rightbuffer;
  
  return true;
}

/*
bool ImportWAV(wxString fName, WaveTrack **dest1, WaveTrack **dest2, DirManager *dirManager)
{
  *dest1 = 0;
  *dest2 = 0;

  wxFile inf;

  inf.Open(fName, wxFile::read);

  if (!inf.IsOpened()) {
	wxMessageBox("Could not open "+fName);
	return false;
  }

  char tag[5];
  int intRate=0;
  short channels=0;
  short bytesPerSample=0;
  
  inf.Read((void *)tag, 4);
  tag[4] = 0;
  if (strcmp(tag, "RIFF"))
  {
	wxMessageBox("Missing RIFF: Not a WAV file.");
	return false;
  }	

  inf.Seek(4, wxFromCurrent);

  inf.Read((void *)tag, 4);
  tag[4] = 0;
  if (strcmp(tag, "WAVE"))
  {
	wxMessageBox("Missing WAVE: Not a WAV file.");
	return false;
  }	

  inf.Read((void *)tag, 4);
  tag[4] = 0;
  if (strcmp(tag, "fmt "))
  {
	wxMessageBox("Missing fmt : Not a WAV file.");
	return false;
  }	

  inf.Seek(6, wxFromCurrent);

  inf.Read((short *)&channels, 2);
  channels = wxUINT16_SWAP_ON_BE(channels);

#ifdef VERBOSE
  printf("channels: %d\n",(int)channels);
#endif

  inf.Read((int *)&intRate, 4);
  intRate = wxUINT32_SWAP_ON_BE(intRate);

#ifdef VERBOSE
  printf("rate: %d\n", intRate);
#endif

  *dest1 = new WaveTrack(dirManager);
  wxASSERT(*dest1);
  (*dest1)->rate = (double)intRate;
  if (channels == 2) {
    *dest2 = new WaveTrack(dirManager);
    wxASSERT(*dest1);
    (*dest2)->rate = (double)intRate;
  }

  inf.Seek(4, wxFromCurrent);

  inf.Read((short *)&bytesPerSample, 2);
  bytesPerSample = wxUINT16_SWAP_ON_BE(bytesPerSample);
  bytesPerSample /= channels;

#ifdef VERBOSE
  printf("bps: %d\n", (int)bytesPerSample);
#endif

  inf.Seek(2, wxFromCurrent);
	
  inf.Read((void *)tag, 4);
  tag[4] = 0;
  if (strcmp(tag, "data"))
  {
	wxMessageBox("Missing data: Not a WAV file.");
	return false;
  }

  int len=0;

  inf.Read((int *)&len, 4);
  len = wxUINT32_SWAP_ON_BE(len);

#ifdef VERBOSE
  printf("Len: %d\n",len);
#endif

  int blockSize = WaveTrack::GetIdealBlockSize();

  wxProgressDialog *progress = NULL;
  
  wxYield();

  wxStartTimer();

  switch(bytesPerSample) {
  case 2: {
	sampleType *buffer = new sampleType[blockSize];
	sampleType *buffer2 = new sampleType[blockSize/2];
	wxASSERT(buffer);
	wxASSERT(buffer2);
	int numSamples = len / 2;
	int block;
	while(numSamples) {
	  int block = (numSamples < blockSize? numSamples : blockSize);
	  int actual = inf.Read((void *)buffer, sizeof(sampleType) * block);
	  int i;
	  for(i=0; i<actual/2; i++) {
		  buffer[i] = wxUINT16_SWAP_ON_BE(buffer[i]);
		}
      if (channels==1)
	    (*dest1)->Append(buffer, actual/2);
	  else {
	    for(i=0; i<actual/4; i++)
	      buffer2[i] = buffer[i*2];
	    (*dest1)->Append(buffer2, actual/4);
	    for(i=0; i<actual/4; i++)
	      buffer2[i] = buffer[i*2+1];
	    (*dest2)->Append(buffer2, actual/4);	    
	  }
	  numSamples -= (actual/2);
	  if (!progress && wxGetElapsedTime(false) > 500) {
		progress =
		  new wxProgressDialog("Import","Importing WAV file",
							   len/2);
	  }

	  if (progress)
		progress->Update(len/2 - numSamples);
	}
	delete[] buffer;
	delete[] buffer2;
  }
  break;
  default:
	wxMessageBox("Sorry, WAV file not 16-bit");
	return false;
  }

  if (progress)
	delete progress;

  inf.Close();

  return true;
}

*/

