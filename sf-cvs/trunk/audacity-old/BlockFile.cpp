/**********************************************************************

  Audacity: A Digital Audio Editor

  BlockFile.cpp

  Dominic Mazzoni

**********************************************************************/

#include "wx/file.h"
#include "wx/filefn.h"

#include "BlockFile.h"

BlockFile::BlockFile(wxString name, wxString fullPath)
{
  this->name = name;
  this->fullPath = fullPath;
  this->isLightweightFile = false;

  theFile = 0;

  refCount = 1;
}

BlockFile::BlockFile(wxString fullPath, int start, int len)
{
  this->name = "";
  this->fullPath = fullPath;
  this->isLightweightFile = true;
  this->start = start;
  this->len = len;
  this->pos = 0;

  refCount = 1;
}

BlockFile::~BlockFile()
{
  Close();
}

wxString BlockFile::GetName()
{
  if (isLightweightFile)
	return fullPath;
  else
	return name;
}

void BlockFile::Ref()
{
  refCount++;
}

bool BlockFile::Deref()
{
  refCount--;
  if (refCount <= 0) {
	if (!isLightweightFile) {
	  Close();
	  wxRemoveFile(fullPath);
	}
    delete this;
    return true;
  }
  else
    return false;
}

bool BlockFile::Open(bool create)
{
  if (isLightweightFile) {

  }
  else {
	if (theFile)
	  return true;
	
	theFile = new wxFFile();
	bool rval = theFile->Open((const wxChar *)fullPath,
							  create? "w+b": "r+b");
	
	return rval;
  }
}

void BlockFile::Close()
{
  if (isLightweightFile) {
	if (theFile) {
	  theFile->Close();
	  delete theFile;
	  theFile = 0;
	}
  }
}

int BlockFile::Read(void *data, int len)
{
  if (isLightweightFile) {

  }
  else {
	wxASSERT(theFile);
	
	int rval = (int)theFile->Read(data, (size_t)len);
	
	if (rval != len) {
	  printf("Expected %d bytes, got %d\n", len, rval);
	  wxASSERT(0);
	}

	return rval;
  }
}

int BlockFile::Write(void *data, int len)
{
  if (isLightweightFile) {

  }
  else {
	wxASSERT(theFile);
	
	return (int)theFile->Write((const void *)data, (size_t)len);
  }
}

bool BlockFile::Seek(int where, wxSeekMode mode)
{
  if (isLightweightFile) {
	switch(mode) {
	case wxFromStart:
	default:
	  pos = where;
	  break;
	case wxFromCurrent:
	  pos += where;
	  break;
	case wxFromEnd:
	  pos = len - where;
	  break;
	}
  }
  else {
	wxASSERT(theFile);
	
	return theFile->Seek((long)where, mode);
  }
}

int BlockFile::Tell()
{
  if (isLightweightFile) {
	return pos;
  }
  else {
	wxASSERT(theFile);
	
	return (int)theFile->Tell();
  }
}
