/**********************************************************************

  Audacity: A Digital Audio Editor

  DirectoriesPrefs.cpp

  Joshua Haberman
  James Crook


*******************************************************************//**

\class DirectoriesPrefs
\brief A PrefsPanel used to selct directories.

*//*******************************************************************/

#include "../Audacity.h"

#include <math.h>

#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/dirdlg.h>
#include <wx/event.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/msgdlg.h>
#include <wx/utils.h>

#include "../Prefs.h"
#include "../AudacityApp.h"
#include "../Internat.h"
#include "../ShuttleGui.h"
#include "DirectoriesPrefs.h"

enum {
   TempDirID = 1000,
   ChooseButtonID
};

BEGIN_EVENT_TABLE(DirectoriesPrefs, wxPanel)
   EVT_TEXT(TempDirID, DirectoriesPrefs::UpdateFreeSpace)
   EVT_BUTTON(ChooseButtonID, DirectoriesPrefs::OnChooseTempDir)
END_EVENT_TABLE()

DirectoriesPrefs::DirectoriesPrefs(wxWindow * parent):
   PrefsPanel(parent),
   mTempDirText( NULL )  // JKC: Must be initialised, ready for UpdateFreeSpace to look at.
{
   SetLabel(_("Directories"));         // Provide visual label
   SetName(_("Directories"));          // Provide audible label
   Populate();
}

/// Creates the dialog and its contents.
void DirectoriesPrefs::Populate( )
{
   // First any pre-processing for constructing the GUI.

   mTempDir = gPrefs->Read(wxT("/Directories/TempDir"), wxT(""));
   mOldTempDir = mTempDir;
   //BG: wxWindows 2.3.2 and higher claim to support this, through a function called wxGetDiskSpace
   wxLongLong freeSpace;
   wxGetDiskSpace(mTempDir, NULL, &freeSpace);
   mStrFreeSpace = Internat::FormatSize(freeSpace);
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

/// Places controls on the panel and also exchanges data with them.
/// As of Jun/2006 the only gPrefs value, mTempDir is
/// actually retrieved from gPrefs earlier, so the exchange is
/// with that variable rather than with gPrefs.
void DirectoriesPrefs::PopulateOrExchange( ShuttleGui & S )
{
   S.SetBorder( 2 );
   S.StartStatic( _("Temporary files directory"),0);
   {
      S.StartMultiColumn(3, wxEXPAND);
      S.SetStretchyCol( 1 );// Column 1 is stretchy...
      mTempDirText = S.Id( TempDirID ).AddTextBox( _("Location:"),  mTempDir, 30);
      S.Id( ChooseButtonID ).AddButton( _("Choose..."));
      S.AddFixedText( _("Free Space:"));
      mFreeSpace = S.AddVariableText( mStrFreeSpace );
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic( _("Auto save"),0);
   {
      S.TieCheckBox( _("Auto save a copy of the project in a separate folder"),
                     wxT("/Directories/AutoSaveEnabled"), true);
      S.StartThreeColumn();
      S.TieTextBox( _("Auto save interval:"), wxT("/Directories/AutoSaveMinutes"), 5.0, 9);
      S.AddUnits(  _("minutes") );
      S.EndThreeColumn();
   }
   S.EndStatic();
   
   S.StartStatic( _("Audio cache"),0);
   {
      S.TieCheckBox( _("Hold recorded data in memory until recording is stopped"),
                     wxT("/Directories/CacheBlockFiles"), false);
   }
   S.EndStatic();
}

void DirectoriesPrefs::OnChooseTempDir(wxCommandEvent &event)
{
   wxDirDialog dlog(this, 
                     _("Choose a location to place the temporary directory"), 
                     gPrefs->Read(wxT("/Directories/TempDir"), 
                     wxGetApp().defaultTempDir));
   dlog.ShowModal();
   if (dlog.GetPath() != wxT("")) {
      wxFileName tmpDirPath;
      tmpDirPath.AssignDir(dlog.GetPath());
#ifdef __WXMSW__
      tmpDirPath.AppendDir(wxT("audacity_temp"));
#else
      tmpDirPath.AppendDir(wxT(".audacity_temp"));
#endif
      mTempDirText->SetValue(tmpDirPath.GetPath(wxPATH_GET_VOLUME|wxPATH_GET_SEPARATOR));
      UpdateFreeSpace(event);
   }
}

void DirectoriesPrefs::UpdateFreeSpace(wxCommandEvent &event)
{
   static wxLongLong space;
   static wxString tempDir;

   if (!mTempDirText)
      return;

   tempDir = mTempDirText->GetValue();

   /* Try to be smart: if the directory doesn't exist, go up the
    * directory path until one is, because that's the volume that
    * the new directory would be created on */
   while(!wxDirExists(tempDir) && tempDir.Find(wxFILE_SEP_PATH) != -1)
      tempDir = tempDir.BeforeLast(wxFILE_SEP_PATH);

   //BG: wxWindows 2.3.2 and higher claim to support this, through a function called wxGetDiskSpace
   wxLogNull nolog;
   wxGetDiskSpace(tempDir, NULL, &space);
   mFreeSpace->SetLabel(Internat::FormatSize(space));
}
   
bool DirectoriesPrefs::Apply()
{
   mTempDir = mTempDirText->GetValue();

   if(!wxDirExists(mTempDir)) {
      int ans = wxMessageBox(
            wxString::Format(_("Directory %s does not exist. Create it?"),
                             mTempDir.c_str()),
            _("New Temporary Directory"),
            wxYES_NO|wxCENTRE|wxICON_EXCLAMATION);

      if(ans == wxYES) {
         if(!wxMkdir(mTempDir, 0755)) {
            /* wxWindows throws up a decent looking dialog */
            return false;
         }
      } 
      else {
         return false;
      }
   }
   else {
      /* If the directory already exists, make sure it is writable */
      wxLogNull logNo;
      wxString tempDir = mTempDir + wxFILE_SEP_PATH + wxT("canicreate");
      if(!wxMkdir(tempDir, 0755)) {
         wxMessageBox(
               wxString::Format(_("Directory %s is not writable"),
                                mTempDir.c_str()),
               _("Error"), wxOK|wxICON_ERROR);
         return false;
      }
      wxRmdir(tempDir);
   }

   gPrefs->Write(wxT("/Directories/TempDir"), mTempDir);

   if (mTempDir != mOldTempDir)
      wxMessageBox(
            _("Changes to temporary directory will not take effect until Audacity is restarted"),
            wxT("Temp Directory Update"), wxOK|wxCENTRE|wxICON_INFORMATION);

   ShuttleGui S( this, eIsSavingToPrefs );
   PopulateOrExchange( S );

   return true;
}

DirectoriesPrefs::~DirectoriesPrefs()
{
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: b152d0c9-973a-44a2-a6ce-b4f6e79be37b

