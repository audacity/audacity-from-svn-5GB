/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportPCM.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/file.h>
#include <wx/string.h>
#include <wx/thread.h>
#include <wx/timer.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>

#include "Import.h"
#include "ImportPCM.h"
#include "WaveTrack.h"
#include "DirManager.h"
#include "Prefs.h"

#include "snd/snd.h"

bool IsPCM(wxString fName)
{
  wxFile testFile;
  testFile.Open(fName);
  if (!testFile.IsOpened())
    return false;

  snd_node sndfile;
  
  sndfile.device = SND_DEVICE_FILE;
  sndfile.write_flag = SND_READ;
  strcpy(sndfile.u.file.filename, (const char *)fName);
  sndfile.u.file.file = 0;
  
  int err;
  long flags=0;
  
  err = snd_open(&sndfile, &flags);
  if (err)
	return false;

  int channels = sndfile.format.channels;
  if (channels < 1 || channels > 128) {
	snd_close(&sndfile);
	return false;
  }

  if (sndfile.u.file.header == SND_HEAD_WAVE) {
	snd_close(&sndfile);
	return true;
  }

  int len = sndfile.u.file.end_offset - sndfile.u.file.byte_offset;
  if (len <= 0) {
	snd_close(&sndfile);
	return false;
  }

  snd_close(&sndfile);
  return true;
}

bool ImportPCM(wxWindow *parent,
               wxString fName, WaveTrack **dest1, WaveTrack **dest2,
               DirManager *dirManager)
{
  *dest1 = 0;
  *dest2 = 0;

  // Get actual file len

  int actualFileLen;

  wxFile testFile;
  testFile.Open(fName);
  if (!testFile.IsOpened()) {
    wxMessageBox("Could not open file.");
    return false;
  }
  actualFileLen = testFile.Length();
  testFile.Close();

  // Initialize snd to actually process it

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

  int channels = sndfile.format.channels;

  if (channels > 2 && channels<=128) {
	wxString s;
	s.Printf("Sorry, Audacity does not support importing %d"
			 "-channel files (yet).", channels);
	wxMessageBox(s);
	return false;
  }

  if (channels < 1)
  {
    wxMessageBox("Unknown audio format.");
    return false;
  }

  *dest1 = new WaveTrack(dirManager);
  wxASSERT(*dest1);
  (*dest1)->rate = sndfile.format.srate;
  (*dest1)->name = TrackNameFromFileName(fName);
  (*dest1)->channel = VTrack::MonoChannel;
  if (channels == 2) {
    *dest2 = new WaveTrack(dirManager);
    wxASSERT(*dest1);
    (*dest2)->rate = sndfile.format.srate;
    (*dest2)->name = TrackNameFromFileName(fName);
	(*dest1)->channel = VTrack::LeftChannel;
	(*dest2)->channel = VTrack::RightChannel;
    (*dest1)->linked = true;
  }

  long fileTotalFrames;

  if (sndfile.u.file.header == SND_HEAD_WAVE) {
    // Wave files are notorious for having bad headers, so we
    // use the actual file length to calculate the length of
    // the song, not the length stored in the header
    fileTotalFrames = (actualFileLen - sndfile.u.file.byte_offset)/
      snd_bytes_per_frame(&sndfile);
  }
  else {
    // For any other format (i.e. AIFF) we trust the value in
    // the header as long as it's plausible
    if ((sndfile.u.file.end_offset - sndfile.u.file.byte_offset) <
        actualFileLen)
      fileTotalFrames = (sndfile.u.file.end_offset -
                         sndfile.u.file.byte_offset)/
        snd_bytes_per_frame(&sndfile);
    else
      fileTotalFrames = (actualFileLen - sndfile.u.file.byte_offset)/
        snd_bytes_per_frame(&sndfile);
  }

  int maxblocksize = WaveTrack::GetIdealBlockSize();

  wxString copyEdit = 
    gPrefs->Read("/FileFormats/CopyOrEditUncompressedData", "edit");
  
  bool doEdit = true; // Fall back to "edit" if it doesn't match anything else
  if(copyEdit.IsSameAs("copy", false))
    doEdit = false;

  if (doEdit) {

    // If this mode has been selected, we form the tracks as
    // aliases to the files we're editing, i.e. ("foo.wav", 12000-18000)
    // instead of actually making fresh copies of the samples.

    wxProgressDialog *progress = NULL;  
    wxYield();
    wxStartTimer();
    wxBusyCursor busy;
    
    bool cancelling = false;

    for(sampleCount i=0; i<fileTotalFrames; i+=maxblocksize) {
      sampleCount blockLen = maxblocksize;
      if (i + blockLen > fileTotalFrames)
        blockLen = fileTotalFrames - i;
      
      (*dest1)->AppendAlias(fName, i, blockLen, 0);
      if (channels==2) {
        (*dest2)->AppendAlias(fName, i, blockLen, 1);
      }

	  if (!progress && wxGetElapsedTime(false) > 500) {
		progress =
		  new wxProgressDialog("Import","Importing audio file...",
							   1000,
							   parent,
							   wxPD_CAN_ABORT |
							   wxPD_REMAINING_TIME |
							   wxPD_AUTO_HIDE);
	  }
	  if (progress) {
		cancelling =
		  !progress->Update((i*1000.0)/fileTotalFrames);
		
		if (cancelling)
		  i = fileTotalFrames;
	  }
    }

	//printf("Time elapsed: %d\n", wxGetElapsedTime());

	if (progress)
	  delete progress;

	if (cancelling) {
	  if (*dest1) {
		delete *dest1;
		*dest1 = NULL;
	  }
	  if (*dest2) {
		delete *dest2;
		*dest2 = NULL;
	  }

	  return false;
	}

    return true;
  }

  // Otherwise, we're in the "copy" mode, where we read in the actual
  // samples from the file and store our own local copy of the
  // samples in the tracks.

  sndbuffer.device = SND_DEVICE_MEM;
  sndbuffer.write_flag = SND_WRITE;
  sndbuffer.u.mem.buffer_max = 0;
  sndbuffer.u.mem.buffer = 0;
  sndbuffer.u.mem.buffer_len = 0;
  sndbuffer.u.mem.buffer_pos = 0;
  sndbuffer.format.channels = channels;
  sndbuffer.format.mode = SND_MODE_PCM; // SND_MODE_FLOAT
  sndbuffer.format.bits = 16;
  sndbuffer.format.srate = sndfile.format.srate;

  char *srcbuffer = new char[maxblocksize*2*channels];
  char *dstbuffer = new char[maxblocksize*2*channels];
  char *leftbuffer = new char[maxblocksize*2];
  char *rightbuffer = new char[maxblocksize*2];

  long framescompleted = 0;

  wxProgressDialog *progress = NULL;  
  wxYield();
  wxStartTimer();
  wxBusyCursor busy;

  bool cancelling = false;

  long block;
  do {
    block = maxblocksize;
    block = snd_read(&sndfile, srcbuffer, block);
    if (block > 0) {
      long b2 = snd_convert(&sndbuffer, dstbuffer,  // to
                            &sndfile, srcbuffer, block);      // from
      if (channels == 1)
        (*dest1)->Append((sampleType *)dstbuffer, b2);
      else {
        for(int i=0; i<b2; i++) {
          ((sampleType *)leftbuffer)[i] = ((sampleType *)dstbuffer)[2*i];
          ((sampleType *)rightbuffer)[i] = ((sampleType *)dstbuffer)[2*i+1];
        }
        (*dest1)->Append((sampleType *)leftbuffer, (sampleCount)b2);
        (*dest2)->Append((sampleType *)rightbuffer, (sampleCount)b2);
      }
      
      framescompleted += block;
      
    }

    if (!progress && wxGetElapsedTime(false) > 500) {
      progress =
        new wxProgressDialog("Import","Importing audio file...",
                             1000,
                             parent,
                             wxPD_CAN_ABORT |
                             wxPD_REMAINING_TIME |
                             wxPD_AUTO_HIDE);
    }
    if (progress) {
      int progressvalue = (framescompleted > fileTotalFrames)?
                           fileTotalFrames:
                           framescompleted;

      cancelling =
        !progress->Update((progressvalue*1000.0)/fileTotalFrames);

      if (cancelling)
        block = 0;
    }
  } while (block > 0);

  snd_close(&sndfile);

  //printf("Time elapsed: %d\n", wxGetElapsedTime());

  if (progress)
    delete progress;
  
  delete[] srcbuffer;
  delete[] dstbuffer;
  delete[] leftbuffer;
  delete[] rightbuffer;

  if (cancelling) {
    if (*dest1) {
      delete *dest1;
      *dest1 = NULL;
    }
    if (*dest2) {
      delete *dest2;
      *dest2 = NULL;
    }

    return false;
  }
  
  return true;
}

/*

 *
 * This old code isn't used anymore, but is a handy reference.
 * It parses a standard WAV file header and imports it into
 * the project.  Not needed because the snd library handles
 * WAV and many other formats.
 *


bool ImportWAV(wxString fName, WaveTrack **dest1, WaveTrack **dest2,
               DirManager *dirManager)
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

