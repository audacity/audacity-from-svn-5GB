/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormatPrefs.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/window.h>
#include <wx/statbox.h>
#include <wx/textdlg.h>
#include <wx/msgdlg.h>
#include <wx/filefn.h>
#include <wx/utils.h>
#include <wx/dirdlg.h>
#include <wx/sizer.h>

#include "../Prefs.h"
#include "../DiskFunctions.h"
#include "DirectoriesPrefs.h"

enum {
   SetID = 1000
};

BEGIN_EVENT_TABLE(DirectoriesPrefs, wxPanel)
    EVT_BUTTON(SetID, DirectoriesPrefs::SetTempDir)
END_EVENT_TABLE()

DirectoriesPrefs::DirectoriesPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   wxString dir = gPrefs->Read("/Directories/TempDir", "");

   topSizer = new wxStaticBoxSizer(
      new wxStaticBox(this, -1, "Directories"), wxVERTICAL );

   {
      wxStaticBoxSizer *tempDirSizer = new wxStaticBoxSizer(
         new wxStaticBox(this, -1, "Temp. Directory"), wxVERTICAL );

      wxFlexGridSizer *tempDirGridSizer = new wxFlexGridSizer( 2, 0, 0, 0 );

      mTempDirLabel = new wxStaticText(
         this, -1, "Location:", wxDefaultPosition,
         wxDefaultSize, wxALIGN_RIGHT );

      mTempDir = new wxStaticText(
         this, -1, dir,
         wxDefaultPosition, wxDefaultSize, 0 );

      mFreeSpaceLabel = new wxStaticText(
         this, -1, "Free Space:",
         wxDefaultPosition, wxDefaultSize, 0 );

      mFreeSpace = new wxStaticText(
         this, -1, FormatSize(GetFreeDiskSpace((char *) (const char *) dir)),
         wxDefaultPosition, wxDefaultSize, 0 );

      mChange = new wxButton(
         this, SetID, "Change",
         wxDefaultPosition, wxDefaultSize, 0 );
        
      tempDirGridSizer->Add( mTempDirLabel, 0, wxALIGN_LEFT|wxALL, 2 );
      tempDirGridSizer->Add( mTempDir, 0, wxGROW|wxALL, 2 );
      tempDirGridSizer->Add( mFreeSpaceLabel, 0, wxALIGN_LEFT|wxALL, 2 );
      tempDirGridSizer->Add( mFreeSpace, 0, wxGROW|wxALL, 2 );
      tempDirSizer->Add( tempDirGridSizer, 0, wxGROW|wxALL, 2 );
      tempDirSizer->Add( mChange, 0, wxALIGN_LEFT|wxALL, 2 );

      topSizer->Add( tempDirSizer, 0, wxGROW|wxALL, 5 );
   }

   SetAutoLayout(true);
   SetSizer(topSizer);

   topSizer->Fit(this);
   topSizer->SetSizeHints(this);

}

void DirectoriesPrefs::SetTempDir(wxCommandEvent & event)
{
   wxString dir;
   wxDirDialog dialog(this,
                      "Select Temporary Directory", mTempDir->GetLabel());

   if (dialog.ShowModal() == wxID_CANCEL)
      return;

   dir = dialog.GetPath();

   /* TODO: make sure directory is writable */

   mTempDir->SetLabel(dir);
   mFreeSpace->
       SetLabel(FormatSize(GetFreeDiskSpace((char *) (const char *) dir)));
}


wxString DirectoriesPrefs::FormatSize(long size)
{
   wxString sizeStr;

   if (size == -1L)
      sizeStr = "Unable to determine";
   else {
      /* make it look nice, by formatting into k, MB, etc */
      if (size < 1024)
         sizeStr.sprintf("%d bytes", size);
      else if (size < 1024 * 1024)
         sizeStr.sprintf("%.2fkB", (float)size / 1024);
      else if (size < 1024 * 1024 * 1024)
         sizeStr.sprintf("%.2fMB", (float)size / (1024 * 1024));
      else
         sizeStr.sprintf("%.2fGB", (float)size / (1024 * 1024 * 1024));
   }

   return sizeStr;
}


bool DirectoriesPrefs::Apply()
{
   wxString oldDir;

   gPrefs->SetPath("/Directories");
   oldDir = gPrefs->Read("TempDir", "none");
   gPrefs->Write("TempDir", mTempDir->GetLabel());

   if (mTempDir->GetLabel() != oldDir)
      wxMessageBox
          ("Changes to temporary directory will not take effect until Audacity is restarted");

   return true;
}

DirectoriesPrefs::~DirectoriesPrefs()
{
   delete mTempDirLabel;
   delete mTempDir;
   delete mChange;
   delete mFreeSpaceLabel;
   delete mFreeSpace;
}
