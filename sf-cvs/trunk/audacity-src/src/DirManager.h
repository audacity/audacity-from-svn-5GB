/**********************************************************************

  Audacity: A Digital Audio Editor

  DirManager.h

  Dominic Mazzoni

  This class manages the files that a project uses to store most
  of its data.  It creates new BlockFile objects, which can
  be used to store any type of data.  BlockFiles support all of
  the common file operations, but they also support reference
  counting, so two different parts of a project can point to
  the same block of data.

  For example, a track might contain 10 blocks of data representing
  its audio.  If you copy the last 5 blocks and paste at the
  end of the file, no new blocks need to be created - we just store
  pointers to new ones.  When part of a track is deleted, the
  affected blocks decrement their reference counts, and when they
  reach zero they are deleted.  This same mechanism is also used
  to implement Undo.

  The DirManager, besides mapping filenames to absolute paths,
  also hashes all of the block names used in a project, so that
  when reading a project from disk, multiple copies of the
  same block still get mapped to the same BlockFile object.

**********************************************************************/

#ifndef _DIRMANAGER_
#define _DIRMANAGER_

#include <wx/list.h>
#include <wx/string.h>
#include <wx/filename.h>

#include "WaveTrack.h"

class wxHashTable;
class BlockFile;

class DirManager: public XMLTagHandler {
 public:
   DirManager();
   virtual ~DirManager();

   static bool InitDirManager();

   // Returns true on success.
   // If SetProject is told NOT to create the directory
   // but it doesn't already exist, SetProject fails and returns false.
   bool SetProject(wxString & projPath, wxString & projName, bool create);

   wxString GetProjectName();

   BlockFile *NewSimpleBlockFile(samplePtr sampleData, sampleCount sampleLen,
                                 sampleFormat format);
   BlockFile *NewAliasBlockFile( wxString aliasedFile, sampleCount aliasStart,
                                 sampleCount aliasLen, int aliasChannel);

   // Adds one to the reference count of the block file,
   // UNLESS it is "locked", then it makes a new copy of
   // the BlockFile.
   BlockFile *CopyBlockFile(BlockFile *b);

   BlockFile *LoadBlockFile(const char **attrs, sampleFormat format);
   void SaveBlockFile(BlockFile *f, int depth, FILE *fp);

#if LEGACY_PROJECT_FILE_SUPPORT
   BlockFile *LoadBlockFile(wxTextFile * in, sampleFormat format);
   void SaveBlockFile(BlockFile * f, wxTextFile * out);
#endif

   bool MoveToNewProjectDirectory(BlockFile *f);
   bool CopyToNewProjectDirectory(BlockFile *f);

   bool EnsureSafeFilename(wxFileName fName);

   void Ref(BlockFile * f);
   void Deref(BlockFile * f);

   // For debugging only
   int GetRefCount(BlockFile * f);

   void SetLoadingTarget(BlockFile **target) { mLoadingTarget = target; }
   bool HandleXMLTag(const char *tag, const char **attrs);
   XMLTagHandler *HandleXMLChild(const char *tag) { return NULL; }
   void WriteXML(int depth, FILE *fp) { }

 private:
   void CleanTempDir();

   // Create new unique track name
   wxString NewTrackName();

   wxFileName MakeBlockFileName(wxString inProjDir);

   // Create new unique names
   wxString NewTempBlockName();
   wxString NewBlockName();

   //////////////////////////

   wxHashTable *blockFileHash;

   static unsigned int defaultHashTableSize;
   unsigned int hashTableSize;
   void CheckHashTableSize();

   wxString projName;
   wxString projPath;
   wxString projFull;

   wxString lastProject;

   wxStringList aliasList;

   BlockFile **mLoadingTarget;

   static wxString temp;

   static int numDirManagers;
   static int fileIndex;
   static wxString tempDirName;
};

#endif
