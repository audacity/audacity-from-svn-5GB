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

  libmpeg3: a free as in speech and beer mp3 decoding library. I'm not
  sure where it will and won't compile, but it works at least on Linux

  There is a master symbol MP3SUPPORT -- if it is not defined, there
  be no MP3 support at all. If MP3SUPPORT is defined, another symbol
  must be defined as well to indicate which library -- at the moment,
  either USE_XAUDIO or USE_LIBMPEG3
  
**********************************************************************/

#include "Audacity.h"

#ifdef MP3SUPPORT

#include <wx/file.h>
#include <wx/thread.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/string.h>
#include <wx/timer.h>

#include "Import.h"
#include "ImportMP3.h"

#include "WaveTrack.h"
#include "DirManager.h"

#ifdef USE_LIBMPEG3
  #include "mpeg3/libmpeg3.h"
#elif defined(USE_XAUDIO)
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
#else
  #error MP3 support selected but no mp3 decoding library specified
#endif

bool ImportMP3(wxWindow *parent,
			   wxString fName, WaveTrack **left, WaveTrack **right, 
			   DirManager *dirManager)
{
#ifdef USE_LIBMPEG3

  wxBusyCursor wait;

  mpeg3_t *file = mpeg3_open((char *)fName.c_str());

  if (!file) {
    wxMessageBox("Could not open "+fName);
    return false;
  }

  int len = mpeg3_audio_samples(file, 0);
  int stereo = (mpeg3_audio_channels(file, 0) > 1);

  *left = new WaveTrack(dirManager);
  (*left)->channel = VTrack::LeftChannel;
  (*left)->name = TrackNameFromFileName(fName);
  if(stereo) {
  	*right = new WaveTrack(dirManager);
  	(*right)->channel = VTrack::RightChannel;
	(*right)->name = TrackNameFromFileName(fName);
    (*left)->linked = true;
  }

  wxProgressDialog *progress = NULL;

  wxYield();
  wxStartTimer();

#define BUFFER_SIZE 32768
#define CODEC_TRANSFER_SIZE 4096
  sampleType *leftBuffer = new sampleType[BUFFER_SIZE];
  sampleType *rightBuffer = new sampleType[BUFFER_SIZE];

  wxASSERT(leftBuffer);
  wxASSERT(rightBuffer);

  int decodedSamples = 0;
  int pos = 0;

  bool cancelled = false;

  mpeg3_seek_percentage(file, 0);
  while(!cancelled && pos < len) {

	  if(decodedSamples + CODEC_TRANSFER_SIZE > BUFFER_SIZE) {
	      (*left)->Append((sampleType *)leftBuffer, decodedSamples);

		  if(stereo)
			  (*right)->Append((sampleType *)rightBuffer, decodedSamples);

		  decodedSamples = 0;
	   }

	 mpeg3_read_audio(file, 
			  NULL,  // pointer to array of floats (but we're using ints)
			  (leftBuffer + decodedSamples), 
			  0,     // Channel to decode
              CODEC_TRANSFER_SIZE, // Number of _samples_ to decode
			  0); 
	 if(stereo)
		 int leftBytes = mpeg3_reread_audio(file, 
			  NULL, 
			  (rightBuffer + decodedSamples),
			  1,          // Channel to decode
              CODEC_TRANSFER_SIZE,
			  0);

	  pos += CODEC_TRANSFER_SIZE;
	  decodedSamples += CODEC_TRANSFER_SIZE;

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
	  cancelled = !progress->Update((int)(mpeg3_tell_percentage(file) * 1000));
	}
  }

  mpeg3_close(file);

  delete[] leftBuffer;
  if(stereo)
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

#elif defined(USE_XAUDIO)

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

  (*left)->name = TrackNameFromFileName(fName);
  (*right)->name = TrackNameFromFileName(fName);
  
  (*left)->linked = true;

  wxProgressDialog *progress = NULL;

  wxYield();
  wxStartTimer();
    
  /* create and open input object */
	
  const char *cFName = (const char *)fName;

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

#endif /* defined(MP3SUPPORT) */

