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
#include <wx/ffile.h>

#include "WaveTrack.h"

class BlockFile {
 public:
   // Normal block file
   BlockFile(wxString name, wxString fullPath);

   // Alias block file
   BlockFile(wxString name, wxString fullPath,
             int localLen,
             wxString aliasFullPath,
             sampleCount aliasStart, sampleCount aliasLen,
             int aliasChannel);

   ~BlockFile();

   bool OpenReadHeader();
   bool OpenReadData();
   bool OpenWriting();

   void Close();

   int Read(void *data, int len);
   int Write(void *data, int len);
   bool SeekTo(int where);

   // this accessor should be used for debugging only
   wxString GetName();

   bool IsAlias();
   wxString GetAliasedFile();
   void ChangeAliasedFile(wxString newFile);

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

   void Ref();
   bool Deref();

   // General variables
   int mRefCount;

   bool mAlias;                 // Does this file alias another file

   bool mLocked;

   int mPos;

   // Information about local data
   wxString mName;
   wxString mFullPath;
   wxFFile *mFile;

   // Information about aliased sound data
   wxString mAliasFullPath;
   int mLocalLen;
   sampleCount mStart;
   sampleCount mLen;
   int mChannel;
   void *mSndNode;
};

#endif
