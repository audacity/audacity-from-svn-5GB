/**********************************************************************

  Audacity: A Digital Audio Editor

  GenericStream.cpp

  Dominic Mazzoni

**********************************************************************/

#include "wx/file.h"
#include "wx/stream.h"

#include "GenericStream.h"

GenericStream::GenericStream(wxFile *f)
{
	which = fileType;
	
	theFile = f;
	
	closeFile = false;
}

GenericStream::GenericStream(const wxString &fName)
{
	which = fileType;

	theFile = new wxFile();
	if (!wxFileExists(fName))
    theFile->Create(fName);
  else
	  theFile->Open(fName, wxFile::read_write);
	
  wxASSERT(theFile->IsOpened());
  
  closeFile = true;
}

#if wxUSE_STD_IOSTREAM
GenericStream::GenericStream(istream *s)
{
	which = inStreamType;
	theInStream = s;
}
GenericStream::GenericStream(ostream *s)
{
	which = outStreamType;
	theOutStream = s;
}
#else
GenericStream::GenericStream(wxInputStream *s)
{
	which = inStreamType;
	theInStream = s;
}
GenericStream::GenericStream(wxOutputStream *s)
{
	which = outStreamType;
	theOutStream = s;
}
#endif

GenericStream::~GenericStream()
{
	if (which == fileType && closeFile) {
		theFile->Close();
		delete theFile;
	}
}

int GenericStream::Read(void *data, int len)
{
	switch(which) {
		case fileType:
			return theFile->Read(data, (size_t)len);

		case inStreamType:
		  #if wxUSE_STD_IOSTREAM
		  theInStream->read((char *)data, (streamsize)len);
		  return (theInStream->gcount());
		  #else
		  theInStream->Read(data, (size_t)len);
		  return (theInStream->LastRead());
		  #endif
		case outStreamType:
		  return 0;
		default:
		  return 0;
	}
}

int GenericStream::Write(void *data, int len)
{
	switch(which) {
		case fileType:
			return theFile->Write(data, (size_t)len);

		case inStreamType:
		  return 0;
		
		case outStreamType:
		  #if wxUSE_STD_IOSTREAM
		  theOutStream->write((char *)data, (streamsize)len);
		  #else
		  theOutStream->Write(data, (size_t)len);
		  #endif
		  return len;
		default:
		  return 0;
	}
}

void GenericStream::SeekStart(int where)
{
	switch(which) {
		case fileType:
			theFile->Seek((off_t)where, wxFromStart);
			break;
		
		#if wxUSE_STD_IOSTREAM
		case inStreamType:
			theInStream->seekg((streamoff)where, ios::beg);
			break;
		case outStreamType:
			theOutStream->seekp((streamoff)where, ios::beg);
			break;
		#else
		case inStreamType:
			theInStream->SeekI((off_t)where, wxFromStart);
			break;
		case outStreamType:
			theOutStream->SeekO((off_t)where, wxFromStart);
			break;
		#endif
	}
}

void GenericStream::SeekCurrent(int offset)
{
	switch(which) {
		case fileType:
			theFile->Seek((off_t)offset, wxFromCurrent);
			break;
		
		#if wxUSE_STD_IOSTREAM
		case inStreamType:
			theInStream->seekg((streamoff)offset, ios::cur);
			break;
		case outStreamType:
			theOutStream->seekp((streamoff)offset, ios::cur);
			break;
		#else
		case inStreamType:
			theInStream->SeekI((off_t)offset, wxFromCurrent);
			break;
		case outStreamType:
			theOutStream->SeekO((off_t)offset, wxFromCurrent);
			break;
		#endif
	}
}

