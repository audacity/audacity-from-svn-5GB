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

#include <wx/string.h>
#include <wx/hash.h>

#include "BlockFile.h"
#include "WaveTrack.h"

class wxTextFile;

class DirManager
{
public:
  DirManager();
  ~DirManager();

  // Returns true on success.
  // If SetProject is told NOT to create the directory
  // but it doesn't already exist, SetProject fails and returns false.
  bool SetProject(wxString &projPath, wxString &projName, bool create);

  wxString GetProjectName();

  // Create new unique track name
  wxString NewTrackName();

  BlockFile *NewTempBlockFile();
  BlockFile *NewBlockFile();

  BlockFile *NewTempAliasBlockFile(int localLen,
								   wxString fullPath,
								   sampleCount start,
								   sampleCount len,
								   int channel);
  BlockFile *NewAliasBlockFile(int localLen,
							   wxString fullPath,
							   sampleCount start,
							   sampleCount len,
							   int channel);

  BlockFile *LoadBlockFile(wxTextFile *in);
  void SaveBlockFile(BlockFile *f, wxTextFile *out);

  void MakePartOfProject(BlockFile *f);

  void Ref(BlockFile *f);
  void Deref(BlockFile *f);

  static wxString GetHomeDir() {return home;}
  static wxString GetPathChar() {return pathChar;}
  
private:
  void CleanTempDir();

  // Create new unique names
  wxString NewTempBlockName();
  wxString NewBlockName();

  //////////////////////////

  wxHashTable *blockFileHash;

  static int defaultHashTableSize;
  static bool hashWarning;
  void CheckHashTableSize();

  wxString projName;
  wxString projPath;
  wxString projFull;

  static wxString pathChar;
  static wxString home;
  static wxString temp;	
	
  static bool firstCtor;
  static int numDirManagers;
  static int fileIndex;
  static wxString tempDirName;
};

#endif


