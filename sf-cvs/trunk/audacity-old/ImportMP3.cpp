/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMP3.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/file.h>
#include <wx/thread.h>
#include <wx/msgdlg.h>
#include <wx/generic/progdlgg.h>
#include <wx/string.h>
#include <wx/timer.h>

#include "ImportMP3.h"

#include "WaveTrack.h"
#include "DirManager.h"

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

bool ImportMP3(wxString fName, WaveTrack **left, WaveTrack **right, 
			   DirManager *dirManager)
{
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
		new wxProgressDialog("Import", "Importing MPEG audio...", 1000);
      
	if (progress)
	  progress->Update(int(decoder->status->position*1000.0));        
        
  } while(status == XA_SUCCESS ||
		  status == XA_ERROR_TIMEOUT ||
		  status == XA_ERROR_INVALID_FRAME);

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

  return true;
}

