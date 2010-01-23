/**********************************************************************

  Audacity: A Digital Audio Editor

  DirManager.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/filefn.h>
#include <wx/hash.h>
#include <wx/msgdlg.h>

#include "DirManager.h"

// Static class variables

int DirManager::numDirManagers = 0;
int DirManager::fileIndex = 0;
bool DirManager::firstCtor = true;
wxString DirManager::tempDirName = ".audacity_temp";

int DirManager::defaultHashTableSize = 10000;
bool DirManager::hashWarning = false;

// The character which separates directories differs from platform
// to platform.  On Unix, it's a "/", on Windows it's "/" or "\"
// (with "\" preferred), and on the Mac it's a ":".

#ifdef __WXMAC__
wxString DirManager::pathChar = "/";
#else
#ifdef __WXMSW__
wxString DirManager::pathChar = "\\";
#else
wxString DirManager::pathChar = "/";
#endif
#endif

wxString DirManager::home = wxGetCwd();
wxString DirManager::temp = (DirManager::home +
			     DirManager::pathChar +
			     DirManager::tempDirName);

// Methods

DirManager::DirManager()
{
  if (firstCtor) {
    if (!wxPathExists(temp))
      wxMkdir(temp);
    firstCtor = false;
  }
  numDirManagers++;
  if (numDirManagers==1) {
    CleanTempDir();
  }
	
  projPath = "";
  projName = "";

  blockFileHash = new wxHashTable(wxKEY_STRING, defaultHashTableSize);
}

DirManager::~DirManager()
{
  if (blockFileHash)
    delete blockFileHash;

  numDirManagers--;
  if (numDirManagers==0) {
    CleanTempDir();
  }
}

void DirManager::CleanTempDir()
{
  wxString fname;
  wxStringList fnameList;
  int count=0;

  fname = wxFindFirstFile((const char *)(temp + pathChar + "*"));
  while (fname != "") {
    count++;
    fnameList.Add(fname);
    fname = wxFindNextFile();
  }

  wxChar **array = fnameList.ListToArray();

  for(int i=0; i<count; i++) {
    wxString fileName = array[i];
    wxRemoveFile(fileName);
  }
}

bool DirManager::SetProject(wxString &projPath, wxString &projName, bool create)
{
  if (projPath=="")
    projPath = ::wxGetCwd();

  this->projPath = projPath;
  this->projName = projName;
  this->projFull = projPath + pathChar + projName;

  if (create) {
    if (!wxPathExists(projFull))
      return wxMkdir(projFull);
  }
  else {
    if (!wxPathExists(projFull))
      return false;
  }
	
  return true;
}

wxString DirManager::GetProjectName()
{
  return projName;
}

BlockFile *DirManager::NewTempBlockFile()
{
  wxString theFileName;
  wxString thePathName;
  do {
    theFileName.Printf("b%05d.vaf",fileIndex++);
    thePathName = temp + pathChar + theFileName;
  } while (wxFileExists(thePathName));

  BlockFile *newBlockFile = new BlockFile(theFileName, thePathName);

  blockFileHash->Put(theFileName, (wxObject *)newBlockFile);

  CheckHashTableSize();

  return newBlockFile;
}

BlockFile *DirManager::NewBlockFile()
{
  if (projFull == "")
    return NewTempBlockFile();

  wxString theFileName;
  wxString thePathName;
  do {
    theFileName.Printf("b%05d.vaf",fileIndex++);
    thePathName = projFull + pathChar + theFileName;
  } while (wxFileExists(thePathName));

  BlockFile *newBlockFile = new BlockFile(theFileName, thePathName);

  blockFileHash->Put(theFileName, (wxObject *)newBlockFile);

  CheckHashTableSize();

  return newBlockFile;
}

BlockFile *DirManager::GetBlockFile(wxString &blockName)
{
  wxASSERT(projFull != "");

  wxString pathName = projFull + pathChar + blockName;
  BlockFile *retrieved = (BlockFile *)blockFileHash->Get(blockName);
  if (retrieved) {
    retrieved->Ref();
    return retrieved;
  }
  else {
    BlockFile *newBlockFile = new BlockFile(blockName, pathName);

    blockFileHash->Put(blockName, (wxObject *)newBlockFile); 

    CheckHashTableSize();

    if (!wxFileExists(pathName))
      return 0;
    return newBlockFile;
  }
}

void DirManager::MakePartOfProject(BlockFile *f)
{
  wxString newFullPath = projFull + pathChar + f->name;
  if (newFullPath != f->fullPath) {
    bool ok = wxRenameFile(f->fullPath, newFullPath);
    if (ok)
      f->fullPath = newFullPath;
    else {
      ok = wxCopyFile(f->fullPath, newFullPath);
      if (ok) {
	wxRemoveFile(f->fullPath);
	f->fullPath = newFullPath;
      } else {
	wxString msg;
	msg.Printf("Could not rename %s to %s",
		   (const char *)f->fullPath, (const char *)newFullPath);
	wxMessageBox(msg);
      }
    }
  }
}

void DirManager::Ref(BlockFile *f)
{
  f->Ref();
  //  printf("Ref(%d): %s\n",f->refCount, (const char *)(f->fullPath));
}

void DirManager::Deref(BlockFile *f)
{
  wxString theFileName = f->name;

  //  printf("Deref(%d): %s\n",f->refCount-1, (const char *)f->fullPath);

  if (f->Deref()) {
    // If Deref() returned true, the reference count reached zero
    // and this block is no longer needed.  Remove it from the hash
    // table.

    blockFileHash->Delete(theFileName);
  }
}

void DirManager::CheckHashTableSize()
{
  #ifdef __WXDEBUG__
  if (!hashWarning && blockFileHash->GetCount() >= defaultHashTableSize) {
    hashWarning = true;
    wxString msg;
    msg.Printf("Warning: DirManager hash table full (%d entries).",
			   blockFileHash->GetCount());
    wxMessageBox(msg);
  }
  #endif
}

