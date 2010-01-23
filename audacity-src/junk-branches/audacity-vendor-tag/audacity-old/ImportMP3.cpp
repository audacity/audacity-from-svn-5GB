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

#include "mp3/mpg123.h"
#include "mp3/mpglib.h"

bool ImportMP3(wxString fName, WaveTrack **left, WaveTrack **right, 
	       DirManager *dirManager)
{
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

  while(count < len) {
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
	  new wxProgressDialog("Import","Importing MPEG audio...",
			       1000);
      }
      
      if (progress) {
		progress->Update((int)(count*1000.0/len));
      }
    
  }

  ExitMP3(&mpinfo);

  delete[] inBuffer;
  delete[] outBuffer;
  delete[] leftBuffer;
  delete[] rightBuffer;

  delete progress;

  return true;
}

