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
   mType = BLOCK_TYPE_UNCOMPRESSED;
   mMode = BLOCK_MODE_NOT_OPEN;

   mFile = NULL;
   mSndNode = NULL;

   mPos = 0;

   mRefCount = 1;
}

BlockFile::BlockFile(wxString name, wxString fullPath,
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
   mStart = aliasStart;
   mLen = aliasLen;
   mChannel = aliasChannel;
   mPos = 0;

   mSndNode = NULL;

   mRefCount = 1;
}

BlockFile::~BlockFile()
{
   if(mMode != BLOCK_MODE_NOT_OPEN)
      Close();
}

wxString BlockFile::GetName()
{
   return mName;
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
      mSndNode = (void *) new snd_node();
      ((snd_node *) mSndNode)->device = SND_DEVICE_FILE;
      ((snd_node *) mSndNode)->write_flag = SND_READ;
      strcpy(((snd_node *) mSndNode)->u.file.filename,
             (const char *) mAliasFullPath);
      ((snd_node *) mSndNode)->u.file.file = 0;

      int err;
      long flags = 0;

      err = snd_open(((snd_node *) mSndNode), &flags);

      if (err == 0) {
         double secs = mStart / ((snd_node *) mSndNode)->format.srate;
         snd_seek(((snd_node *) mSndNode), secs);
         mMode = BLOCK_MODE_READ_DATA;
         mPos = WaveTrack::GetHeaderLen();
      }

      return (err == 0);
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

   if (mType == BLOCK_TYPE_ALIAS && ((snd_node *) mSndNode)) {
      snd_close(((snd_node *) mSndNode));
      delete((snd_node *) mSndNode);
      mSndNode = NULL;
   }

   mMode = BLOCK_MODE_NOT_OPEN;
}

int BlockFile::Read(void *data, int len)
{
   wxASSERT(mMode & BLOCK_MODE_READING_MODE);
   
   /* make sure the read doesn't start in the header area and end in the data area */
   
   wxASSERT(!(mPos < WaveTrack::GetHeaderLen() && mPos + len > WaveTrack::GetHeaderLen()));
   
   /* if you're in data mode, make sure the pointer is in the data area */
   
   wxASSERT(mMode == BLOCK_MODE_READ_HEADER || mPos >= WaveTrack::GetHeaderLen());
   
   /* if you're in header mode, make sure the pointer is in the header area */
   
   wxASSERT(mMode == BLOCK_MODE_READ_DATA   || mPos < WaveTrack::GetHeaderLen());

   if (mType == BLOCK_TYPE_ALIAS && mMode == BLOCK_MODE_READ_DATA) {
      snd_node sndbuffer;

      int channels = ((snd_node *) mSndNode)->format.channels;

      sndbuffer.device = SND_DEVICE_MEM;
      sndbuffer.write_flag = SND_WRITE;
      sndbuffer.u.mem.buffer_max = 0;
      sndbuffer.u.mem.buffer = 0;
      sndbuffer.u.mem.buffer_len = 0;
      sndbuffer.u.mem.buffer_pos = 0;
      sndbuffer.format.channels = channels;
      sndbuffer.format.mode = SND_MODE_PCM;
      sndbuffer.format.bits = 16;
      sndbuffer.format.srate = ((snd_node *) mSndNode)->format.srate;

      int frames = len / 2;
      int fromBufferSize =
          frames * snd_bytes_per_frame(((snd_node *) mSndNode));
      char *fromBuffer = new char[fromBufferSize];
      int toBufferSize = frames * channels;
      sampleType *toBuffer = new short[toBufferSize];

      int framesRead =
          snd_read(((snd_node *) mSndNode), fromBuffer, frames);

      mPos += framesRead;

      snd_convert(&sndbuffer, (void *) toBuffer,        // to
                  ((snd_node *) mSndNode), (void *) fromBuffer, framesRead);    // from

      for (int i = 0; i < framesRead; i++)
         ((sampleType *) data)[i] = toBuffer[(channels * i) + mChannel];

      delete[]fromBuffer;
      delete[]toBuffer;

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
      int skipSamples = (where - (mPos - WaveTrack::GetHeaderLen())) / 2;
      mPos = where + WaveTrack::GetHeaderLen();
      double secs = skipSamples / ((snd_node *) mSndNode)->format.srate;

      snd_seek(((snd_node *) mSndNode), secs);
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
