/**********************************************************************

  Audacity: A Digital Audio Editor

  BlockFile.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/file.h>
#include <wx/filefn.h>

#include "BlockFile.h"
#include "WaveTrack.h"

#include "sndfile.h"

BlockFile::BlockFile(wxString name, wxString fullPath)
{
   mName = name;
   mFullPath = fullPath;
   mType = BLOCK_TYPE_UNCOMPRESSED;
   mMode = BLOCK_MODE_NOT_OPEN;

   mFile = NULL;
   mSoundFile = NULL;
   mInfo = NULL;

   mLocked = false;

   mPos = 0;

   mRefCount = 1;
}

BlockFile::BlockFile(wxString name, wxString fullPath,
                     int localLen,
                     wxString aliasFullPath,
                     sampleCount aliasStart,
                     sampleCount aliasLen, int aliasChannel)
{
   mName = name;
   mFullPath = fullPath;

   mType = BLOCK_TYPE_ALIAS;
   mMode = BLOCK_MODE_NOT_OPEN;

   mFile = NULL;

   mAliasFullPath = aliasFullPath;
   mLocalLen = localLen;
   mStart = aliasStart;
   mLen = aliasLen;
   mChannel = aliasChannel;
   mPos = 0;

   mLocked = false;

   mSoundFile = NULL;
   mInfo = NULL;

   mRefCount = 1;
}

BlockFile::~BlockFile()
{
   if(mMode != BLOCK_MODE_NOT_OPEN)
      Close();
}

void BlockFile::Lock()
{
   mLocked = true;
}

void BlockFile::Unlock()
{
   mLocked = false;
}

bool BlockFile::IsLocked()
{
   return mLocked;
}

wxString BlockFile::GetName()
{
   return mName;
}

wxString BlockFile::GetAliasedFile()
{
   wxASSERT(mAlias);
   
   return mAliasFullPath;
}

void BlockFile::ChangeAliasedFile(wxString newFile)
{
   // This method is only called with the DirManager is moving
   // a file we depend on out of the way, so that a new file
   // with the same name can be exported.
   
   wxASSERT(mAlias);
   
   mAliasFullPath = newFile;
}

bool BlockFile::IsAlias()
{
   return mType == BLOCK_TYPE_ALIAS;
}

void BlockFile::Ref()
{
   mRefCount++;
}

bool BlockFile::Deref()
{
   mRefCount--;
   if (mRefCount <= 0) {
      if(mMode != BLOCK_MODE_NOT_OPEN)
         Close();
      wxRemoveFile(mFullPath);
      delete this;
      return true;
   } else
      return false;
}

bool BlockFile::OpenReadHeader()
{
   wxASSERT(mMode == BLOCK_MODE_NOT_OPEN);

   mPos = 0;

   mFile = new wxFFile();
   bool success = mFile->Open((const wxChar *) mFullPath, "rb");

   if(success)
      mMode = BLOCK_MODE_READ_HEADER;

   return success;
}

bool BlockFile::OpenReadData()
{
   wxASSERT(mMode == BLOCK_MODE_NOT_OPEN);

   if (mType == BLOCK_TYPE_ALIAS) {
      mInfo = (void *)new SF_INFO;
      mSoundFile = (void *)sf_open_read(mAliasFullPath, (SF_INFO *)mInfo);

      if (mSoundFile != 0) {
         sf_seek((SNDFILE *)mSoundFile, mStart, SEEK_SET);
         mMode = BLOCK_MODE_READ_DATA;
         mPos = WaveTrack::GetHeaderLen();
         return true;
      }
      return false;
   } else {

      mFile = new wxFFile();
      bool success = mFile->Open((const wxChar *) mFullPath, "rb");

      if (success) {
         mMode = BLOCK_MODE_READ_DATA;
         SeekTo(0);     /* seek to the beginning of the data area */
      }

      return success;
   }
}

bool BlockFile::OpenWriteHeader()
{
   wxASSERT(mMode == BLOCK_MODE_NOT_OPEN);

   mPos = 0;
   bool success;

   mFile = new wxFFile();

   if(wxFileExists(mFullPath))
      success = mFile->Open((const wxChar *) mFullPath, "r+b");
   else
      success = mFile->Open((const wxChar *) mFullPath, "w+b");

   if(success) {
      mMode = BLOCK_MODE_WRITE_HEADER;
      mFile->Seek((long)0, wxFromStart);
   }

   return success;
}

bool BlockFile::OpenWriteData()
{
   wxASSERT(mMode == BLOCK_MODE_NOT_OPEN);

   mPos = 0;
   bool success;

   mFile = new wxFFile();
   if(wxFileExists(mFullPath))
      success = mFile->Open((const wxChar *) mFullPath, "r+b");
   else
      success = mFile->Open((const wxChar *) mFullPath, "w+b");

   if(success) {
      mMode = BLOCK_MODE_WRITE_DATA;
      mPos = WaveTrack::GetHeaderLen();
      mFile->Seek((long)mPos, wxFromStart);
   }

   return success;
}

void BlockFile::Close()
{
   wxASSERT(mMode != BLOCK_MODE_NOT_OPEN);

   if (mFile) {
      mFile->Close();
      delete mFile;
      mFile = 0;
   }

   if (mType == BLOCK_TYPE_ALIAS && ((SNDFILE *)mSoundFile)) {
      sf_close((SNDFILE *)mSoundFile);
      mSoundFile = NULL;
      delete (SF_INFO *)mInfo;
   }

   mMode = BLOCK_MODE_NOT_OPEN;
}

int BlockFile::Read(void *data, int len)
{
   wxASSERT(mMode & BLOCK_MODE_READING_MODE);
   
   /* make sure the read doesn't start in the header area and
      end in the data area
   */
   
   wxASSERT(!(mPos < WaveTrack::GetHeaderLen() &&
              mPos + len > WaveTrack::GetHeaderLen()));
   
   /* if you're in data mode, make sure the pointer is in the data area */
   
   wxASSERT(mMode == BLOCK_MODE_READ_HEADER ||
            mPos >= WaveTrack::GetHeaderLen());
   
   /* if you're in header mode, make sure the pointer is in the header area */
   
   wxASSERT(mMode == BLOCK_MODE_READ_DATA   ||
            mPos < WaveTrack::GetHeaderLen());

   if (mType == BLOCK_TYPE_ALIAS && mMode == BLOCK_MODE_READ_DATA) {
      int channels = ((SF_INFO *)mInfo)->channels;     
      int frames = len / sizeof(sampleType);
      int bufferSize = frames * channels;
      sampleType *buffer = new short[bufferSize];

      int framesRead = sf_readf_short((SNDFILE *)mSoundFile, buffer, frames);
      
      for (int i = 0; i < framesRead; i++)
         ((sampleType *) data)[i] = buffer[(channels * i) + mChannel];

      delete[]buffer;

      return (framesRead * sizeof(sampleType));
   } else {
      wxASSERT(mFile);

      int rval = (int) mFile->Read(data, (size_t) len);

      if (rval != len) {
         printf("Expected %d bytes, got %d\n", len, rval);
         wxASSERT(0);
      }

      mPos += rval;

      return rval;
   }
}

int BlockFile::Write(void *data, int len)
{
   wxASSERT(mFile);
   wxASSERT(mMode & BLOCK_MODE_WRITING_MODE);
   wxASSERT(mType != BLOCK_TYPE_ALIAS || mMode == BLOCK_MODE_WRITE_HEADER);

   int rval = (int) mFile->Write((const void *) data, (size_t) len);
   mPos += rval;

   return rval;
}

bool BlockFile::SeekTo(int where)
{
   /* all modes but these two are legit */

   wxASSERT(mMode != BLOCK_MODE_NOT_OPEN && mMode != BLOCK_MODE_WRITE_DATA);
   
   if (mType == BLOCK_TYPE_ALIAS && mMode == BLOCK_MODE_READ_DATA) {

      int channels = ((SF_INFO *)mInfo)->channels;
      sf_seek((SNDFILE *)mSoundFile,
              mStart + (where / sizeof(sampleType)),
              SEEK_SET);
      mPos = where + WaveTrack::GetHeaderLen();

      return true;
   } else {
      
      wxASSERT(mFile);
      if(mMode == BLOCK_MODE_READ_DATA)
         mPos = where + WaveTrack::GetHeaderLen();
      else
         mPos = where;
      
      return mFile->Seek((long) mPos, wxFromStart);
   }
}
