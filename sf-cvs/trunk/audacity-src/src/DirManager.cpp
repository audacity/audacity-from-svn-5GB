/**********************************************************************

  Audacity: A Digital Audio Editor

  DirManager.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/app.h>
#include <wx/log.h>
#include <wx/filefn.h>
#include <wx/hash.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/timer.h>
#include <wx/textfile.h>
#include <wx/intl.h>

#include "DirManager.h"
#include "DiskFunctions.h"
#include "Prefs.h"

#include "prefs/PrefsDialog.h"

// Static class variables

int DirManager::numDirManagers = 0;
int DirManager::fileIndex = 0;
bool DirManager::firstCtor = true;
wxString DirManager::tempDirName = ".audacity_temp";

unsigned int DirManager::defaultHashTableSize = 10000;

// The character which separates directories differs from platform
// to platform.  On Unix, it's a "/", on Windows it's "/" or "\"
// (with "\" preferred), and on the Mac it's a ":".

wxString DirManager::pathChar = wxFILE_SEP_PATH;

#ifdef __WXGTK__
wxString DirManager::home = wxGetHomeDir();
#else
wxString DirManager::home = wxGetCwd();
#endif
wxString DirManager::temp = (DirManager::home +
                             DirManager::pathChar +
                             DirManager::tempDirName);

// Methods

DirManager::DirManager()
{
   if (firstCtor) {
      // The first time a DirManager is created, we need to find
      // a temp directory location.

      firstCtor = false;

      wxString tempFromPrefs = gPrefs->Read("/Directories/TempDir", "");
      wxString tempDefaultLoc = temp;

      temp = "";

      #ifdef __WXGTK__         
      if (tempFromPrefs.GetChar(0) != '/')
         tempFromPrefs = "";
      #endif

      // Stop wxWindows from printing its own error messages

      wxLogNull logNo;

      // Try temp dir that was stored in prefs first

      if (tempFromPrefs != "") {
         if (wxPathExists(tempFromPrefs))
            temp = tempFromPrefs;
         else if (wxMkdir(tempFromPrefs))
            temp = tempFromPrefs;
      }

      // If that didn't work, try the default location

      if (temp=="" && tempDefaultLoc != "") {
         if (wxPathExists(tempDefaultLoc))
            temp = tempDefaultLoc;
         else if (wxMkdir(tempDefaultLoc))
            temp = tempDefaultLoc;
      }

      if (temp != "") {
         // Success
         gPrefs->Write("/Directories/TempDir", temp);
      }
      else {
         wxMessageBox(_("Audacity could not find a place to store "
                        "temporary files.\n  Please enter an appropriate "
                        "directory in the preferences dialog."));

         PrefsDialog dialog(NULL);
         dialog.ShowTempDirPage();
         dialog.ShowModal();

         wxMessageBox(_("Audacity is now going to exit.  Please launch "
                        "Audacity again to use the new temporary directory."));
         wxExit();
      }

   }
   numDirManagers++;
   if (numDirManagers == 1) {
      CleanTempDir();
   }

   projPath = "";
   projName = "";

   hashTableSize = defaultHashTableSize;
   blockFileHash = new wxHashTable(wxKEY_STRING, hashTableSize);

   // Make sure there is plenty of space for temp files

   wxLongLong freeSpace = GetFreeDiskSpace((char *) (const char *) temp);
   if (freeSpace >= 0) {
      if (freeSpace < 1048576) {
         // TODO: allow user to select different temporary volume.
         wxMessageBox(
              _("Warning: there is very little free disk space left on this "
                "volume. Please select another temporary directory in your "
                "preferences."));
      }
   }
}

DirManager::~DirManager()
{
   if (blockFileHash)
      delete blockFileHash;

   numDirManagers--;
   if (numDirManagers == 0) {
      CleanTempDir();
      //::wxRmdir(temp);
   }
}

void DirManager::CleanTempDir()
{
   wxString fname;
   wxStringList fnameList;
   int count = 0;

   fname = wxFindFirstFile((const char *) (temp + pathChar + "*.auf"));
   while (fname != "") {
      if (fname.Length() >= 5 && fname.Right(3) == "auf") {
         count++;
         fnameList.Add(fname);
      }
      fname = wxFindNextFile();
   }

   wxChar **array = fnameList.ListToArray();

   wxProgressDialog *progress = NULL;

   wxYield();
   wxStartTimer();

   for (int i = 0; i < count; i++) {
      wxString fileName = array[i];
      wxRemoveFile(fileName);

      if (!progress && wxGetElapsedTime(false) > 500)
         progress =
             new wxProgressDialog(_("Progress"),
                                  _("Cleaning up temporary files..."),
                                  1000,
                                  NULL,
                                  wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);

      if (progress)
         progress->Update(int ((i * 1000.0) / count));
   }

   if (progress)
      delete progress;
}

bool DirManager::SetProject(wxString & projPath, wxString & projName,
                            bool create)
{
   wxString oldPath = projPath;
   wxString oldName = projName;
   wxString oldFull = projFull;
   wxString oldLoc = projFull;
   if (oldLoc == "")
      oldLoc = temp;
   lastProject = projPath;
   
   if (projPath == "")
      projPath =::wxGetCwd();

   this->projPath = projPath;
   this->projName = projName;
   this->projFull = projPath + pathChar + projName;

   if (create) {
      if (!wxPathExists(projFull))
         if (!wxMkdir(projFull))
            return false;
   } else {
      #ifndef __WXMAC__
      if (!wxPathExists(projFull))
         return false;
      #endif
   }

   /* Move all files into this new directory.  Files which are
      "locked" get copied instead of moved.  (This happens when
      we perform a Save As - the files which belonged to the last
      saved version of the old project must not be moved,
      otherwise the old project would not be safe. */

   blockFileHash->BeginFind();
   wxNode *n = blockFileHash->Next();
   bool success = true;
   while(n && success) {
      BlockFile *b = (BlockFile *)n->GetData();

      if (b->IsLocked())
         success = CopyToNewProjectDirectory(b);
      else
         success = MoveToNewProjectDirectory(b);

      n = blockFileHash->Next();
   }

   if (!success) {
      // If the move failed, we try to move/copy as many files
      // back as possible so that no damage was done.  (No sense
      // in checking for errors this time around - there's nothing
      // that could be done about it.)
      // Likely causes: directory was not writeable, disk was full

      projFull = oldLoc;

      blockFileHash->BeginFind();
      wxNode *n = blockFileHash->Next();
      while(n) {
         BlockFile *b = (BlockFile *)n->GetData();
         MoveToNewProjectDirectory(b);         
         n = blockFileHash->Next();
      }

      projFull = oldFull;
      projPath = oldPath;
      projName = oldName;

      return false;
   }

   return true;
}

wxString DirManager::GetProjectName()
{
   return projName;
}

void DirManager::MakeBlockFileName(wxString inProjDir,
                                   wxString &outFileName,
                                   wxString &outPathName)
{
   do {
      outFileName.Printf("b%05d.auf", fileIndex++);
      outPathName = inProjDir + pathChar + outFileName;
   } while (wxFileExists(outPathName));
}

BlockFile *DirManager::NewTempBlockFile()
{
   wxString theFileName;
   wxString thePathName;
   MakeBlockFileName(temp, theFileName, thePathName);

   BlockFile *newBlockFile = new BlockFile(theFileName, thePathName);

   blockFileHash->Put(theFileName, (wxObject *) newBlockFile);

   CheckHashTableSize();

   return newBlockFile;
}

BlockFile *DirManager::NewBlockFile()
{
   if (projFull == "")
      return NewTempBlockFile();

   wxString theFileName;
   wxString thePathName;
   MakeBlockFileName(projFull, theFileName, thePathName);

   BlockFile *newBlockFile = new BlockFile(theFileName, thePathName);

   blockFileHash->Put(theFileName, (wxObject *) newBlockFile);

   CheckHashTableSize();

   return newBlockFile;
}

BlockFile *DirManager::NewTempAliasBlockFile(int localLen,
                                             wxString fullPath,
                                             sampleCount start,
                                             sampleCount len, int channel)
{
   wxString theFileName;
   wxString thePathName;
   MakeBlockFileName(temp, theFileName, thePathName);

   BlockFile *newBlockFile = new BlockFile(theFileName, thePathName,
                                           localLen,
                                           fullPath,
                                           start, len, channel);

   blockFileHash->Put(theFileName, (wxObject *) newBlockFile);
   aliasList.Add(fullPath);

   CheckHashTableSize();

   return newBlockFile;
}

BlockFile *DirManager::NewAliasBlockFile(int localLen,
                                         wxString fullPath,
                                         sampleCount start,
                                         sampleCount len, int channel)
{
   if (projFull == "")
      return NewTempAliasBlockFile(localLen,
                                   fullPath, start, len,
                                   channel);

   wxString theFileName;
   wxString thePathName;
   MakeBlockFileName(temp, theFileName, thePathName);

   BlockFile *newBlockFile = new BlockFile(theFileName, thePathName,
                                           localLen,
                                           fullPath,
                                           start, len, channel);

   blockFileHash->Put(theFileName, (wxObject *) newBlockFile);
   aliasList.Add(fullPath);

   CheckHashTableSize();

   return newBlockFile;
}

// Adds one to the reference count of the block file,
// UNLESS it is "locked", then it makes a new copy of
// the BlockFile.
BlockFile *DirManager::CopyBlockFile(BlockFile *b)
{
   if (!b->IsLocked()) {
      b->Ref();
      return b;
   }

   wxString theFileName;
   wxString thePathName;
   wxString dir = (projFull != ""? projFull: temp);
   MakeBlockFileName(dir, theFileName, thePathName);

   bool ok = wxCopyFile(b->mFullPath, thePathName);
   if (!ok)
      return NULL;

   BlockFile *b2;
   if (b->IsAlias()) {
      b2 = new BlockFile(theFileName, thePathName,
                         b->mLocalLen,
                         b->mAliasFullPath,
                         b->mStart, b->mLen,
                         b->mChannel);
   }
   else {
      b2 = new BlockFile(theFileName, thePathName);
   }

   blockFileHash->Put(theFileName, (wxObject *) b2);
   aliasList.Add(thePathName);

   CheckHashTableSize();

   return b2;
}

void DirManager::SaveBlockFile(BlockFile * f, wxTextFile * out)
{
   if (f->IsAlias()) {
      out->AddLine("Alias");
      out->AddLine(f->mAliasFullPath);
      out->AddLine(wxString::Format("%d", f->mStart));
      out->AddLine(wxString::Format("%d", f->mLen));
      out->AddLine(wxString::Format("%d", f->mChannel));
   }
   out->AddLine(f->mName);
}

BlockFile *DirManager::LoadBlockFile(wxTextFile * in)
{
   wxASSERT(projFull != "");

   wxString blockName = in->GetNextLine();

   bool alias = false;
   wxString aliasFullPath;
   long localLen, start, len, channel;

   if (blockName == "Alias") {
      alias = true;
      aliasFullPath = in->GetNextLine();
      if (!(in->GetNextLine().ToLong(&localLen)))
         return NULL;
      if (!(in->GetNextLine().ToLong(&start)))
         return NULL;
      if (!(in->GetNextLine().ToLong(&len)))
         return NULL;
      if (!(in->GetNextLine().ToLong(&channel)))
         return NULL;

      blockName = in->GetNextLine();
   }

   wxString pathName = projFull + pathChar + blockName;
   BlockFile *retrieved = (BlockFile *) blockFileHash->Get(blockName);
   if (retrieved) {
      wxASSERT(retrieved->IsAlias() == alias);
      retrieved->Ref();
      return retrieved;
   } else {
      BlockFile *newBlockFile;

      if (alias) {
         newBlockFile = new BlockFile(blockName, pathName,
                                      localLen,
                                      aliasFullPath, start, len, channel);
         aliasList.Add(aliasFullPath);
      }
      else
         newBlockFile = new BlockFile(blockName, pathName);

      blockFileHash->Put(blockName, (wxObject *) newBlockFile);

      CheckHashTableSize();

      if (!wxFileExists(pathName))
         return 0;
      return newBlockFile;
   }
}

bool DirManager::MoveToNewProjectDirectory(BlockFile *f)
{
   wxString newFullPath = projFull + pathChar + f->mName;
   if (newFullPath != f->mFullPath) {
      bool ok = wxRenameFile(f->mFullPath, newFullPath);
      if (ok)
         f->mFullPath = newFullPath;
      else {
         ok = wxCopyFile(f->mFullPath, newFullPath);
         if (ok) {
            wxRemoveFile(f->mFullPath);
            f->mFullPath = newFullPath;
         }
         else
            return false;
      }
   }

   return true;
}

bool DirManager::CopyToNewProjectDirectory(BlockFile *f)
{
   wxString newFullPath = projFull + pathChar + f->mName;
   if (newFullPath != f->mFullPath) {
      bool ok = wxCopyFile(f->mFullPath, newFullPath);
      if (ok) {
         f->mFullPath = newFullPath;
      }
      else
         return false;
   }

   return true;
}

void DirManager::Ref(BlockFile * f)
{
   f->Ref();
   //  printf("Ref(%d): %s\n",f->refCount, (const char *)(f->fullPath));
}

void DirManager::Deref(BlockFile * f)
{
   wxString theFileName = f->mName;

   //  printf("Deref(%d): %s\n",f->mRefCount-1, (const char *)f->mRullPath);

   if (f->Deref()) {
      // If Deref() returned true, the reference count reached zero
      // and this block is no longer needed.  Remove it from the hash
      // table.

      blockFileHash->Delete(theFileName);
   }
}

void DirManager::CheckHashTableSize()
{
   // This method makes sure that our hash table doesn't fill up.
   // When it's about halfway full (i.e. starting to exceed its
   // capacity), we create a new hash table with double the size,
   // and copy everything over.

   if (blockFileHash->GetCount() >= hashTableSize/2) {
      wxBusyCursor busy;
      hashTableSize *= 2;

      wxHashTable *newHash = new wxHashTable(wxKEY_STRING, hashTableSize);
      blockFileHash->BeginFind();
      wxNode *n = blockFileHash->Next();
      while(n) {
         BlockFile *b = (BlockFile *)n->GetData();
         newHash->Put(b->GetName(), (wxObject *) b);
         n = blockFileHash->Next();
      }

      delete blockFileHash;
      blockFileHash = newHash;
   }
}

bool DirManager::EnsureSafeFilename(wxString fName)
{
   // Quick check: If it's not even in our alias list,
   // then the file name is A-OK.
   if (!aliasList.Member(fName))
      return true;

   // If any of the following commands fail, your guess is as
   // good as mine why.  The following error message is the
   // best we can do - we'll use it if any of the renames,
   // creates, or deletes fail.
   wxString errStr =
      "Error: is directory write-protected or disk full?";

   // Figure out what the new name for the existing file
   // would be.  Try to go from "mysong.wav" to "mysong-old1.wav".
   // Keep trying until we find a filename that doesn't exist.

   wxString pathOnly, nameOnly, extension;
   wxString renamedFile;
   ::wxSplitPath(fName, &pathOnly, &nameOnly, &extension);

   int i = 0;
   do {
      i++;
      if (extension != "")
         renamedFile.Printf("%s%s%s-old%d.%s",
                            (const char *)pathOnly,
                            (const char *)pathChar,
                            (const char *)nameOnly,
                            i,
                            (const char *)extension);
      else
         renamedFile.Printf("%s%s%s-old%d",
                            (const char *)pathOnly,
                            (const char *)pathChar,
                            (const char *)nameOnly,
                            i);

   } while (wxFileExists(renamedFile));

   // Test creating a file by that name to make sure it will
   // be possible to do the rename

   wxFile testFile(renamedFile, wxFile::write);
   if (!testFile.IsOpened()) {
      wxMessageBox(errStr);
      return false;
   }
   if (!wxRemoveFile(renamedFile)) {
      wxMessageBox(errStr);
      return false;
   }

   printf("Renamed file: %s\n", (const char *)renamedFile);

   // Go through our block files and see if any indeed point to
   // the file we're concerned about.  If so, point the block file
   // to the renamed file and when we're done, perform the rename.

   bool needToRename = false;
   wxBusyCursor busy;
   blockFileHash->BeginFind();
   wxNode *n = blockFileHash->Next();
   while(n) {
      BlockFile *b = (BlockFile *)n->GetData();

      if (b->IsAlias() && b->GetAliasedFile() == fName) {
         needToRename = true;
         printf("Changing block %s\n", (const char *)b->GetName());
         b->ChangeAliasedFile(renamedFile);
      }

      n = blockFileHash->Next();
   }

   if (needToRename) {
      if (!wxRenameFile(fName, renamedFile)) {
         // ACK!!! The renaming was unsuccessful!!!
         // (This shouldn't happen, since we tried creating a
         // file of this name and then deleted it just a
         // second earlier.)  But we'll handle this scenario
         // just in case!!!

         // Put things back where they were
         blockFileHash->BeginFind();
         n = blockFileHash->Next();
         while(n) {
            BlockFile *b = (BlockFile *)n->GetData();
            if (b->IsAlias() && b->GetAliasedFile() == renamedFile)
               b->ChangeAliasedFile(fName);            
            n = blockFileHash->Next();
         }

         // Print error message and cancel the export
         wxMessageBox(errStr);
         return false;
      }

      aliasList.Delete(fName);
      aliasList.Add(renamedFile);
   }

   // Success!!!  Either we successfully renamed the file,
   // or we didn't need to!
   return true;
}

