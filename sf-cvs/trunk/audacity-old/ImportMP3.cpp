/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMP3.cpp

  Dominic Mazzoni
  Joshua Haberman

  Note: Audacity can be compiled to use one of two mp3 importing
  libraries:

  xaudio: a commercial mp3 decoder that we have obtained a free
  unlimited license to.  The library is available in binary form
  only from www.xaudio.com.  It is very fast, robust, and stable.

  mpg123: this is the free version of a library which has since
  been put under a more restrictive license.  It works, but its
  error checking is not very good, it's not super fast, and
  it sometimes fails on MP3 files with certain ID3 tags.  But...
  it's totally Free.

  Choose which library to compile using the USE_MPG123 macro.

**********************************************************************/

#include <wx/file.h>
#include <wx/thread.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/string.h>
#include <wx/timer.h>

#include "ImportMP3.h"

#include "WaveTrack.h"
#include "DirManager.h"

#ifdef USE_MPG123
  #include "mp3/mpg123.h"
  #include "mp3/mpglib.h"
#else
  #ifdef __WXGTK__
    #include "xaudio/linux/include/decoder.h"
    #include "xaudio/linux/include/mpeg_codec.h"
    #include "xaudio/linux/include/file_input.h"
  #endif
  #ifdef __WXMSW__
    #include "xaudio/win/include/decoder.h"
    #include "xaudio/win/include/file_input.h"
  #endif
  #ifdef __WXMAC__
    #include "xaudio/mac/include/decoder.h"
    #include "xaudio/mac/include/mpeg_codec.h"
    #include "xaudio/mac/include/file_input.h"
  #endif
#endif

bool ImportMP3(wxWindow *parent,
			   wxString fName, WaveTrack **left, WaveTrack **right, 
			   DirManager *dirManager)
{
#ifdef USE_MPG123

  wxBusyCursor wait;

  wxFile inf;
  inf.Open(fName, wxFile::read);
  if (!inf.IsOpened()) {
    wxMessageBox("Could not open "+fName);
    return false;
  }

  int len = inf.Length();

  *left = new WaveTrack(dirManager);
  *right = new WaveTrack(dirManager);

  (*left)->channel = VTrack::LeftChannel;
  (*right)->channel = VTrack::RightChannel;

  wxProgressDialog *progress = NULL;

  wxYield();
  wxStartTimer();

  struct mpstr mpinfo;

  InitMP3(&mpinfo);

  int inBufferSize = 32768;
  char *inBuffer = new char[inBufferSize];
  int outBufferSize = (*left)->GetIdealBlockSize() * sizeof(sampleType);
  char *outBuffer = new char[outBufferSize];
  int leftBufferSize = outBufferSize / 2;
  char *leftBuffer = new char[leftBufferSize];
  int rightBufferSize = outBufferSize / 2;
  char *rightBuffer = new char[rightBufferSize];

  wxASSERT(inBuffer);
  wxASSERT(outBuffer);
  wxASSERT(leftBuffer);
  wxASSERT(rightBuffer);

  int count = 0;

  bool cancelled = false;

  while(count < len && !cancelled) {
    int block = inf.Read((void *)inBuffer, inBufferSize);
    int decodedBytes;
	int pos=0;
    if (block > 0) {
      int rval = decodeMP3(&mpinfo,
			   inBuffer, block,
			   &outBuffer[pos], (outBufferSize-pos),
			   &decodedBytes);

      while(rval == MP3_OK) {
		pos += decodedBytes;
		if (pos > (outBufferSize / 2)) {
		  int frames = pos/4;
		  for(int i=0; i<frames; i++){
			((sampleType *)leftBuffer)[i] =
			  ((sampleType *)outBuffer)[2*i];
			((sampleType *)rightBuffer)[i] =
			  ((sampleType *)outBuffer)[2*i+1];
		  }
		  (*left)->Append((sampleType *)leftBuffer, frames);
		  (*right)->Append((sampleType *)rightBuffer, frames);
		  pos = 0;
		}
		
		rval = decodeMP3(&mpinfo,
						 NULL, 0,
						 &outBuffer[pos], (outBufferSize-pos),
						 &decodedBytes);
      }
    }
	
	
	count += block;
	
    if (block==0)
      break;
	
	if (!progress && wxGetElapsedTime(false) > 500) {
	  progress =
		new wxProgressDialog("Import",
							 "Importing MP3 file...",
							 1000,
							 parent,
							 wxPD_CAN_ABORT |
							 wxPD_REMAINING_TIME |
							 wxPD_AUTO_HIDE);
    
	}

	if (progress) {
	  cancelled = !progress->Update((int)(count*1000.0/len));
	}
  }

  ExitMP3(&mpinfo);

  delete[] inBuffer;
  delete[] outBuffer;
  delete[] leftBuffer;
  delete[] rightBuffer;

  delete progress;

  if (cancelled) {
	if (*left) {
	  delete *left;
	  *left = NULL;
	}
	if (*right) {
	  delete *right;
	  *right = NULL;
	}

	return false;
  }

  return true;

#else

  /* Use XAUDIO */
  wxBusyCursor wait;

  XA_DecoderInfo *decoder;
  int             status; 

  /* create a decoder */
  if (decoder_new(&decoder) != XA_SUCCESS) {
	wxMessageBox("Cannot Initialize MP3 Decoder 1");
	return false;
  }
    
  /* register the file input module */
  {
	XA_InputModule module;

	file_input_module_register(&module);
	decoder_input_module_register(decoder, &module);
  }

  /* register mpeg audio codec */
#ifndef __WXMSW__
  {
	XA_CodecModule module;

	mpeg_codec_module_register(&module);
	decoder_codec_module_register(decoder, &module);
  }
#endif

  *left = new WaveTrack(dirManager);
  *right = new WaveTrack(dirManager);

  (*left)->channel = VTrack::LeftChannel;
  (*right)->channel = VTrack::RightChannel;

  wxProgressDialog *progress = NULL;

  wxYield();
  wxStartTimer();
    
  /* create and open input object */
	
#ifdef __WXMAC__
  const char *cFName = ::wxUnix2MacFilename(fName);
#else
  const char *cFName = (const char *)fName;
#endif

  status = decoder_input_new(decoder, cFName, XA_DECODER_INPUT_AUTOSELECT);
  if (status != XA_SUCCESS) {
	wxMessageBox("Cannot open MP3 file");
	return false;
  }
  if (decoder_input_open(decoder) != XA_SUCCESS) {
	wxMessageBox("Cannot open MP3 file");
	return false;
  }

  /* allocate buffers for the PCM samples */
    
  const int bufferSize = (*left)->GetIdealBlockSize();
  int bufferCount = 0;
  sampleType *left_buffer = new sampleType[bufferSize];
  sampleType *right_buffer = new sampleType[bufferSize];
  bool stereo = true;

  bool cancelled = false;

  do {
	status = decoder_decode(decoder, NULL);

	if (status == XA_SUCCESS) {
            
#ifdef __WXGTK__
	  int channels = decoder->output_buffer->channels;
	  int samples = decoder->output_buffer->size / channels / 2;
#endif

#ifdef __WXMSW__
	  int channels = decoder->output_buffer->stereo + 1;
	  int samples = decoder->output_buffer->size / channels / 2;
#endif

#ifdef __WXMAC__
	  int channels = decoder->output_buffer->channels;
	  int samples = decoder->output_buffer->nb_samples;
#endif

	  if (bufferCount + samples > bufferSize) {
		(*left)->Append(left_buffer, bufferCount);
		  if (stereo)
		  (*right)->Append(right_buffer, bufferCount);
		bufferCount = 0;
	  }
            
	  if (channels == 1) {
		stereo = false;
                
		for(int i=0; i<samples; i++)
		  left_buffer[bufferCount+i] = 
			(((sampleType *)decoder->output_buffer->pcm_samples)[i]);
	  }
	  else {
		stereo = true;
                
		// Un-interleave the samples
		for(int i=0; i<samples; i++) {
		  left_buffer[bufferCount+i] =
			(((sampleType *)(decoder->output_buffer->pcm_samples))[2*i]);
		  right_buffer[bufferCount+i] =
			(((sampleType *)(decoder->output_buffer->pcm_samples))[2*i+1]);
		}
	  }

          bufferCount += samples;                

		  (*left)->rate = double(decoder->output_buffer->sample_rate);
		  (*right)->rate = double(decoder->output_buffer->sample_rate);
	}

	if (!progress && wxGetElapsedTime(false) > 500)
	  progress =
		new wxProgressDialog("Import",
							 "Importing MP3 file...",
							 1000,
							 parent,
							 wxPD_CAN_ABORT |
							 wxPD_REMAINING_TIME |
							 wxPD_AUTO_HIDE);
    
	if (progress) {
	  cancelled = !progress->Update(int(decoder->status->position*1000.0));
	}
        
  } while(cancelled == false &&
		  (status == XA_SUCCESS ||
		   status == XA_ERROR_TIMEOUT ||
		   status == XA_ERROR_INVALID_FRAME));
  
  if (bufferCount) {
	(*left)->Append(left_buffer, bufferCount);
	if (stereo)
	  (*right)->Append(right_buffer, bufferCount);
  }

  delete[] left_buffer;
  delete[] right_buffer;

  if (!stereo) {
	delete *right;
	*right = NULL;
        
	(*left)->channel = VTrack::MonoChannel;
  }
    
  if (progress)
	delete progress;

  if (cancelled) {
	if (*left) {
	  delete *left;
	  *left = NULL;
	}
	if (*right) {
	  delete *right;
	  *right = NULL;
	}

	return false;
  }

  return true;

#endif
}

