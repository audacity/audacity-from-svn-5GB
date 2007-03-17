/**********************************************************************

  Audacity: A Digital Audio Editor

  Help.cpp

  Dominic Mazzoni

*******************************************************************//*!

\file Help.cpp
\brief Glue code to connect to html help controller.

  Audacity uses the wxWindows HTML help controller and gets the
  HTML files from a zip archive with a "htb" extension, which
  is supposed to stand for "HTML Book".  It expects the help
  file to be called "audacity-1.2-help.htb".

  If you want to edit the help file, unzip audacity-1.2-help.htb
  (rename it to audacity-1.2-help.zip first if you have to), edit
  the files, and then zip them again.  Audacity asks the user
  for the location of the help file the first time (if necessary)
  and then remembers it from then on.

*//*******************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/filedlg.h>
#include <wx/filename.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/help.h>
#include <wx/html/helpctrl.h>
#include <wx/intl.h>

#include "AudacityApp.h"
#include "Help.h"
#include "Internat.h"
#include "Prefs.h"

wxHelpControllerBase *gHelp = NULL;

void InitHelp(wxWindow * parent)
{
   if (!gHelp) {
      wxArrayString helpFiles;
      wxFileName helpFile = gPrefs->Read(wxT("/Help/helpFilePath1.2"), wxT(""));

      if (helpFile != wxT("")) {
         wxGetApp().AddUniquePathToPathList(helpFile.GetPath(),
                                            wxGetApp().audacityPathList);
      }

#if defined(__WXMSW__)
      wxGetApp().FindFilesInPathList(wxT("audacity-1.2-help.chm"),
                                     wxGetApp().audacityPathList,
                                     wxFILE,
                                     helpFiles);
#endif
      if (helpFiles.GetCount() == 0) {
         wxGetApp().FindFilesInPathList(wxT("audacity-1.2-help.htb"),
                                        wxGetApp().audacityPathList,
                                        wxFILE,
                                        helpFiles);
         if (helpFiles.GetCount() == 0) {
            helpFile = wxFileSelector(_("Where is audacity-1.2-help?"),
                                      NULL,
                                      wxT("audacity-1.2-help"),
                                      wxT(""),
#if defined(__WXMSW__)
                                      _("Help Files (*.chm, *.htb)|*.chm;*.htb"),
#else
                                      _("Help Files (*.htb)|*.htb"),
#endif
                                      wxOPEN,
                                      parent);
         }
         else {
            helpFile = helpFiles[0];
         }
      }
      else {
         helpFile = helpFiles[0];
      }

      if (!::wxFileExists(helpFile.GetFullPath())) {
         return;
      }

#if defined(__WXMSW__)
      if (helpFile.GetExt().Lower() == wxT("chm"))
         gHelp = new wxCHMHelpController();
      else
#endif
         gHelp = new wxHtmlHelpController();

      gHelp->SetParentWindow(parent);
      if (!gHelp->Initialize(helpFile.GetFullPath())) {
         wxMessageBox(_("Couldn't open the Audacity Help file."));
         delete gHelp;
         gHelp = NULL;
      }

      gPrefs->Write(wxT("/Help/helpFilePath1.2"), helpFile.GetPath() + wxFILE_SEP_PATH);
   }
}

void ShowHelp(wxWindow * parent)
{
   InitHelp(parent);

   if (gHelp)
      gHelp->DisplayContents();
}

void ShowHelpIndex(wxWindow * parent)
{
   InitHelp(parent);

// LLL: No DisplayIndex() in CHM help controller.  Not using right
//      now anyway.
//
//   if (gHelp)
//      gHelp->DisplayIndex();
}

void ShowHelp(wxWindow * parent, wxString topic)
{
   InitHelp(parent);

   if (gHelp)
      gHelp->KeywordSearch(topic);
}

void SearchHelp(wxWindow * parent)
{
   InitHelp(parent);

   if (gHelp) {
      wxString key = wxGetTextFromUser(_("Search for?"),
                                       _("Search help for keyword"),
                                       wxT(""),
                                       parent);

      if (!key.IsEmpty())
         gHelp->KeywordSearch(key);
   }
}

void QuitHelp()
{
   if (gHelp) {
      delete gHelp;
      gHelp = NULL;
   }
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
// arch-tag: 6165cbc3-295a-4e52-b76c-825123aa7935

