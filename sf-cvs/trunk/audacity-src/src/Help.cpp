/**********************************************************************

  Audacity: A Digital Audio Editor

  Help.cpp

  Dominic Mazzoni

  Audacity uses the wxWindows HTML help controller and gets the
  HTML files from a zip archive with a "htb" extension, which
  is supposed to stand for "HTML Book".  It expects the help
  file to be called "audacity-help.htb".

  If you want to edit the help file, unzip audacity-help.htb
  (rename it to audacity-help.zip first if you have to), edit
  the files, and then zip them again.  Audacity asks the user
  for the location of the help file the first time and then
  remembers it from then on.

**********************************************************************/

#ifdef __WXMAC__

#include <wx/defs.h>
#include <wx/window.h>

#include "Help.h"

void InitHelp(wxWindow * parent)
{
}

void ShowHelp(wxWindow * parent)
{
}

void ShowHelpIndex(wxWindow * parent)
{
}

void ShowHelp(wxWindow * parent, wxString topic)
{
}

void SearchHelp(wxWindow * parent)
{
}

void QuitHelp()
{
}


#else

#include <wx/defs.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/html/helpctrl.h>
#include <wx/intl.h>

#include "Help.h"
#include "Prefs.h"

wxHtmlHelpController *gHelp = NULL;

void InitHelp(wxWindow * parent)
{
   if (!gHelp) {
      wxString defaultLoc = wxGetCwd() + wxFILE_SEP_PATH + "audacity-help.htb";

      wxString helpFilePath =
          gPrefs->Read("/Help/HelpFilePath", defaultLoc);

      if (!::wxFileExists(helpFilePath)) {
         helpFilePath = wxFileSelector(_("Where is audacity-help.htb?"), NULL,
                                       "audacity-help.htb",    // Name
                                       "",                     // Extension
                                       _("HTML Help Books (*.htb)|*.htb"),
                                       wxOPEN, parent);
         if (helpFilePath == "")
            return;
      }

      gHelp = new wxHtmlHelpController();
      if (!gHelp->AddBook(helpFilePath)) {
         wxMessageBox(_("Couldn't open the Audacity Help file."));
         delete gHelp;
         gHelp = NULL;
      }

      gPrefs->Write("/Help/HelpFilePath", helpFilePath);
   }
}

void ShowHelp(wxWindow * parent)
{
   InitHelp(parent);

   if (gHelp)
      gHelp->Display(_("Introduction"));
}

void ShowHelpIndex(wxWindow * parent)
{
   InitHelp(parent);

   if (gHelp)
      gHelp->DisplayIndex();
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
                                       "",
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

#endif
