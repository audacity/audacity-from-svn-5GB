/**********************************************************************

  Audacity: A Digital Audio Editor

  BlockFile.cpp

  Dominic Mazzoni

**********************************************************************/

#include "wx/file.h"
#include "wx/filefn.h"

#include "BlockFile.h"
#include "WaveTrack.h"

#include "snd/snd.h"

BlockFile::BlockFile(wxString name, wxString fullPath)
{
  mName = name;
  mFullPath = fullPath;
  mAlias = false;

  mFile = NULL;
  mSndNode = NULL;

  mPos = 0;

  mRefCount = 1;
}

BlockFile::BlockFile(wxString name, wxString fullPath,
					 int localLen,
					 wxString aliasFullPath,
					 sampleCount aliasStart,
					 sampleCount aliasLen,
					 int aliasChannel)
{
  mName = name;
  mFullPath = fullPath;

  mAlias = true;

  mFile = NULL;

  mAliasFullPath = aliasFullPath;
  mLocalLen = localLen;
  mStart = aliasStart;
  mLen = aliasLen;
  mChannel = aliasChannel;
  mPos = 0;

  mSndNode = NULL;

  mRefCount = 1;
}

BlockFile::~BlockFile()
{
  Close();
}

wxString BlockFile::GetName()
{
  return mName;
}

bool BlockFile::IsAlias()
{
  return mAlias;
}

void BlockFile::Ref()
{
  mRefCount++;
}

bool BlockFile::Deref()
{
  mRefCount--;
  if (mRefCount <= 0) {
	Close();
	wxRemoveFile(mFullPath);
    delete this;
    return true;
  }
  else
    return false;
}

bool BlockFile::OpenReadHeader()
{
  mPos = 0;

  mFile = new wxFFile();
  bool success = mFile->Open((const wxChar *)mFullPath, "r+b"); 

  return success;
}

bool BlockFile::OpenReadData()
{
  if (mAlias) {
	mSndNode = (void *)new snd_node();
	((snd_node *)mSndNode)->device = SND_DEVICE_FILE;
	((snd_node *)mSndNode)->write_flag = SND_READ;
	strcpy(((snd_node *)mSndNode)->u.file.filename, (const char *)mAliasFullPath);
	((snd_node *)mSndNode)->u.file.file = 0;
	
	int err;
	long flags=0;

	mPos = mLocalLen;
	
	err = snd_open(((snd_node *)mSndNode), &flags);
	
	if (err == 0) {
	  double secs = mStart / ((snd_node *)mSndNode)->format.srate;
	  snd_seek(((snd_node *)mSndNode), secs);
	}

	return (err == 0);
  }
  else {
	mPos = mLocalLen;

	mFile = new wxFFile();
	bool success = mFile->Open((const wxChar *)mFullPath, "r+b"); 

	return success;
  }
}

bool BlockFile::OpenWriting()
{
  mPos = 0;

  mFile = new wxFFile();
  bool success = mFile->Open((const wxChar *)mFullPath, "w+b");

  return success;
}

void BlockFile::Close()
{
  if (mFile) {
	mFile->Close();
	delete mFile;
	mFile = 0;
  }

  if (mAlias && ((snd_node *)mSndNode)) {
	snd_close(((snd_node *)mSndNode));
	delete ((snd_node *)mSndNode);
	((snd_node *)mSndNode) = NULL;
  }

}

int BlockFile::Read(void *data, int len)
{
  wxASSERT(!(mAlias && mPos < mLocalLen && mPos+len > mLocalLen));

  if (!mAlias || (mAlias && mPos < mLocalLen)) {
	wxASSERT(mFile);
	
	int rval = (int)mFile->Read(data, (size_t)len);
	
	if (rval != len) {
	  printf("Expected %d bytes, got %d\n", len, rval);
	  wxASSERT(0);
	}

	mPos += rval;

	return rval;	
  }
  else {
	snd_node sndbuffer;

	int channels = ((snd_node *)mSndNode)->format.channels;

	sndbuffer.device = SND_DEVICE_MEM;
	sndbuffer.write_flag = SND_WRITE;
	sndbuffer.u.mem.buffer_max = 0;
	sndbuffer.u.mem.buffer = 0;
	sndbuffer.u.mem.buffer_len = 0;
	sndbuffer.u.mem.buffer_pos = 0;
	sndbuffer.format.channels = channels;
	sndbuffer.format.mode = SND_MODE_PCM;
	sndbuffer.format.bits = 16;
	sndbuffer.format.srate = ((snd_node *)mSndNode)->format.srate;

	int frames = len/2;
	int fromBufferSize = frames * snd_bytes_per_frame(((snd_node *)mSndNode));
	char *fromBuffer = new char[fromBufferSize];
	int toBufferSize = frames * channels;
	sampleType *toBuffer = new short[toBufferSize];

	int framesRead = snd_read(((snd_node *)mSndNode), fromBuffer, frames);

	mPos += framesRead;

	snd_convert(&sndbuffer, (void *)toBuffer,              // to
				((snd_node *)mSndNode), (void *)fromBuffer, framesRead); // from

	for(int i=0; i<framesRead; i++)
	  ((sampleType *)data)[i] = toBuffer[(channels*i)+mChannel];

	delete[] fromBuffer;
	delete[] toBuffer;

	return (framesRead*sizeof(sampleType));
  }
}

int BlockFile::Write(void *data, int len)
{
  wxASSERT(mFile);
  wxASSERT(!mAlias || mPos < mLocalLen);
  
  int rval = (int)mFile->Write((const void *)data, (size_t)len);
  mPos += rval;

  return rval;
}

bool BlockFile::SeekTo(int where)
{
  if (mAlias && where >= mLocalLen) {
	int skipSamples = (where - mPos)/2;
	mPos = where;
	double secs = skipSamples / ((snd_node *)mSndNode)->format.srate;

	snd_seek(((snd_node *)mSndNode), secs);
	return true;
  }
  else {
	wxASSERT(mFile);	
	mPos = where;
	return mFile->Seek((long)mPos, wxFromStart);	
  }
}
