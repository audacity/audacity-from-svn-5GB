/**********************************************************************

  Audacity: A Digital Audio Editor

  BlockFile.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/file.h>
#include <wx/ffile.h>
#include <wx/filefn.h>
#include <wx/intl.h>

#include "BlockFile.h"
#include "WaveTrack.h"

#include "sndfile.h"

BlockFile::BlockFile(wxString name, wxString fullPath, int summaryLen)
{
   mName = name;
   mFullPath = fullPath;
   mType = BLOCK_TYPE_UNCOMPRESSED;
   mSampleFormat = floatSample;
   mSummaryLen = summaryLen;

   mLocked = false;

   mRefCount = 1;
}

BlockFile::~BlockFile()
{
}

bool BlockFile::WriteSummary(void *data)
{
   bool success;
   wxFFile file;

   if(wxFileExists(mFullPath))
      success = file.Open((const wxChar *) mFullPath, "r+b");
   else
      success = file.Open((const wxChar *) mFullPath, "w+b");

   if (!success)
      return false;

   int len = mSummaryLen;
   int written = (int)file.Write(data, (size_t)len);

   file.Close();

   return (written == len);
}

bool BlockFile::WriteData(void *data, sampleFormat format, sampleCount len)
{
   bool success;
   wxFFile file;
   size_t bytes;

   mType = BLOCK_TYPE_UNCOMPRESSED;
   mSampleFormat = format;

   if(wxFileExists(mFullPath))
      success = file.Open((const wxChar *) mFullPath, "r+b");
   else
      success = file.Open((const wxChar *) mFullPath, "w+b");

   if (!success)
      return false;

   file.Seek(mSummaryLen, wxFromStart);
   bytes = len * SAMPLE_SIZE(format);
   int written = (int)file.Write(data, bytes);
   written /= SAMPLE_SIZE(format);

   file.Close();

   return (written == len);
}

void BlockFile::SetAliasedData(wxString aliasFullPath,
                               sampleCount aliasStart, sampleCount aliasLen,
                               int aliasChannel)
{
   mType = BLOCK_TYPE_ALIAS;

   mAliasFullPath = aliasFullPath;
   mStart = aliasStart;
   mLen = aliasLen;
   mChannel = aliasChannel;
}

bool BlockFile::ReadSummary(void *data)
{
   wxFFile file;

   if (!file.Open((const wxChar *) mFullPath, "rb"))
      return false;

   int len = mSummaryLen;
   int read = (int)file.Read(data, (size_t)len);

   file.Close();

   return (read == len);
}

sampleFormat BlockFile::GetNativeFormat()
{
   return mSampleFormat;
}

int BlockFile::ReadData(void *data, sampleFormat format,
                        sampleCount start, sampleCount len)
{
   int i;

   if (mType == BLOCK_TYPE_ALIAS) {
      SF_INFO info;
      SNDFILE *sf = sf_open(mAliasFullPath, SFM_READ, &info);

      if (!sf)
         return 0;

      sf_seek(sf, mStart + start, SEEK_SET);
      samplePtr buffer = NewSamples(len * info.channels, floatSample);

      int framesRead = 0;
      switch(format) {
      case int16Sample:
         framesRead = sf_readf_short(sf, (short *)buffer, len);
         for (i = 0; i < framesRead; i++)
            ((short *)data)[i] =
               ((short *)buffer)[(info.channels * i) + mChannel];
         break;

      case floatSample:
         framesRead = sf_readf_float(sf, (float *)buffer, len);
         for (i = 0; i < framesRead; i++)
            ((float *)data)[i] =
               ((float *)buffer)[(info.channels * i) + mChannel];
         break;
      
      default:
         framesRead = sf_readf_float(sf, (float *)buffer, len);
         for (i = 0; i < framesRead; i++)
            ((float *)buffer)[i] =
               ((float *)buffer)[(info.channels * i) + mChannel];
         CopySamples((samplePtr)buffer, floatSample,
                     (samplePtr)data, format, framesRead);
      }

      DeleteSamples(buffer);

      sf_close(sf);

      return framesRead;

   }
   else {
      wxFFile file;
      int read;

      if (!file.Open((const wxChar *) mFullPath, "rb"))
         return 0;

      file.Seek(mSummaryLen +
                start * SAMPLE_SIZE(mSampleFormat), wxFromStart);

      if (format == mSampleFormat) {
         int bytes = len * SAMPLE_SIZE(mSampleFormat);
         read = (int)file.Read(data, (size_t)bytes);
         read /= SAMPLE_SIZE(mSampleFormat);
      }
      else {
         samplePtr buffer = NewSamples(len, mSampleFormat);
         int srcBytes = len * SAMPLE_SIZE(mSampleFormat);
         read = (int)file.Read(buffer, (size_t)srcBytes);
         read /= SAMPLE_SIZE(mSampleFormat);
         CopySamples(buffer, mSampleFormat,
                     (samplePtr)data, format, read);
         DeleteSamples(buffer);
      }

      file.Close();
      return read;
   }
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
   wxASSERT(IsAlias());
   
   return mAliasFullPath;
}

int BlockFile::GetSummaryLen()
{
   return mSummaryLen;
}

void BlockFile::ChangeAliasedFile(wxString newFile)
{
   // This method is only called with the DirManager is moving
   // a file we depend on out of the way, so that a new file
   // with the same name can be exported.
   
   wxASSERT(IsAlias());
   
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
      wxRemoveFile(mFullPath);
      delete this;
      return true;
   } else
      return false;
}

