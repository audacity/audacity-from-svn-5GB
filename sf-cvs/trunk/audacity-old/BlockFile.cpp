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

  theFile = 0;

  refCount = 1;
}

BlockFile::~BlockFile()
{
  Close();
}

void BlockFile::Ref()
{
  refCount++;
}

bool BlockFile::Deref()
{
  refCount--;
  if (refCount <= 0) {
    Close();
    wxRemoveFile(fullPath);
    delete this;
    return true;
  }
  else
    return false;
}

bool BlockFile::Open(bool create)
{
  if (theFile)
    return true;

  theFile = new wxFFile();
  bool rval = theFile->Open((const wxChar *)fullPath,
			    create? "w+b": "r+b");

  return rval;
}

void BlockFile::Close()
{
  if (theFile) {
    theFile->Close();
    delete theFile;
    theFile = 0;
  }
}

int BlockFile::Read(void *data, int len)
{
  wxASSERT(theFile);

  int rval = (int)theFile->Read(data, (size_t)len);

  if (rval != len) {
    printf("Expected %d bytes, got %d\n", len, rval);
	wxASSERT(0);
  }

  return rval;
}

int BlockFile::Write(void *data, int len)
{
  wxASSERT(theFile);

  return (int)theFile->Write((const void *)data, (size_t)len);
}

bool BlockFile::Seek(int where, wxSeekMode mode)
{
  wxASSERT(theFile);

  return theFile->Seek((long)where, mode);
}

int BlockFile::Tell()
{
  wxASSERT(theFile);

  return (int)theFile->Tell();
}



