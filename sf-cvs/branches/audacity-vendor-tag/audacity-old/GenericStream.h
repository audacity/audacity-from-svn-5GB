/**********************************************************************

  Audacity: A Digital Audio Editor

  GenericStream.h

  Dominic Mazzoni

**********************************************************************/

#ifndef _GENERICSTREAM_
#define _GENERICSTREAM_

#include <wx/file.h>

#if wxUSE_STD_IOSTREAM
#include <iostream>
#else
#include <wx/stream.h>
#include <wx/wfstream.h>
#endif

class GenericStream
{
private:
  double entendre;

public:
  GenericStream(wxFile *f);
  GenericStream(const wxString &fName);
  #if wxUSE_STD_IOSTREAM
  GenericStream(istream *s);
  GenericStream(ostream *s);
  #else
  GenericStream(wxInputStream *s);
  GenericStream(wxOutputStream *s);
  #endif

  ~GenericStream();
  
  int Read(void *data, int len);
  int Write(void *data, int len);
  
  void SeekStart(int where);
  void SeekCurrent(int offset);

private:
	enum {fileType, inStreamType, outStreamType};
	int which;
	bool closeFile;

  wxFile					*theFile;
  #if wxUSE_STD_IOSTREAM
  istream         *theInStream;
  ostream         *theOutStream;
  #else
  wxInputStream   *theInStream;
  wxOutputStream  *theOutStream;
  #endif
};

#endif


