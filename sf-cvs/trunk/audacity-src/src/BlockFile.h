/**********************************************************************

  Audacity: A Digital Audio Editor

  BlockFile

  Dominic Mazzoni

  This class is a wrapper for a file that represents a block of
  information.  It supports reference counting (the constructor
  defaults the reference count to 1) so that a single block can
  be pointed to by multiple tracks or virtual tracks.

  In addition, this class abstracts low-level file reading
  and writing, which allows block files to actually point to
  the middle of another audio file on disk for part of its data.

  Currently only DirManager should be creating and destroying
  BlockFiles, or managing their reference counts.  You should
  not need to call Ref or Deref directly from anywhere else in
  the program other than from DirManager.
  
**********************************************************************/

#ifndef __AUDACITY_BLOCKFILE__
#define __AUDACITY_BLOCKFILE__

#include <wx/string.h>

#include "WaveTrack.h"

class BlockFile {
 public:

   // Constructor / Destructor

   BlockFile(wxString name, wxString fullPath, int summaryLen);

   ~BlockFile();

   // Writing

   bool WriteSummary(void *data);

   bool WriteData(void *data, sampleFormat format, sampleCount len);

   void SetAliasedData(wxString aliasFullPath,
                       sampleCount aliasStart, sampleCount aliasLen,
                       int aliasChannel);

   // Reading

   bool ReadSummary(void *data);

   sampleFormat GetNativeFormat();
   int ReadData(void *data, sampleFormat format,
                sampleCount start, sampleCount len);

   // Other Properties

   // this accessor should be used for debugging only
   wxString GetName();

   wxString GetAliasedFile();
   void ChangeAliasedFile(wxString newFile);
   bool IsAlias();
   int  GetSummaryLen();

   // If a BlockFile is locked, it cannot be moved - just
   // copied.  When performing a Save As, the project
   // locks all of the blocks which belonged to the old
   // project, keeping them from being removed.  It doesn't
   // lock blocks which belong to the new project only; they
   // get moved to the new location.
   void Lock();
   void Unlock();
   bool IsLocked();

 private:

   friend class DirManager;
   friend class TrackList;

   void Ref();
   bool Deref();

   // General variables
   int mRefCount;
   bool mLocked;

   enum {
      BLOCK_TYPE_UNCOMPRESSED,
      BLOCK_TYPE_ALIAS,
      BLOCK_TYPE_FLAC
   } mType;

   // Information about local data
   wxString      mName;
   wxString      mFullPath;
   sampleFormat  mSampleFormat;
   int           mSummaryLen;

   // Information about aliased sound data
   wxString      mAliasFullPath;
   sampleCount   mStart;
   sampleCount   mLen;
   int           mChannel;
};

#endif
