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

#ifdef foo___WXMAC__

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

#include "Help.h"
#include "Prefs.h"

wxHtmlHelpController *gHelp = NULL;

void SetHelpFile(wxWindow * parent)
{
   if (gHelp) {
      delete gHelp;
      gHelp = NULL;
   }

   wxString helpFilePath = gPrefs->Read("/Help/HelpFilePath", "");
   wxString searchPath;
   if (::wxFileExists(helpFilePath))
      searchPath = ::wxPathOnly(helpFilePath);

   helpFilePath = wxFileSelector("Where is audacity-help.htb?",
                                 searchPath,
                                 "audacity-help.htb",  // Name
                                 "",                   // Extension
                                 "HTML Help Books (*.htb)|*.htb",
                                 wxOPEN, parent);
   if (helpFilePath == "")
      return;

   gHelp = new wxHtmlHelpController();
   if (!gHelp->AddBook(helpFilePath)) {
      wxMessageBox("Couldn't open the Audacity Help file.");
      delete gHelp;
      gHelp = NULL;
      return;
   }
   
   gPrefs->Write("/Help/HelpFilePath", helpFilePath);   

   wxMessageBox("Help file loaded successfully.");
}

void InitHelp(wxWindow * parent)
{
   if (!gHelp) {
      wxArrayString paths;
      wxString helpFilePath = "";

      wxString storedPath =
         gPrefs->Read("/Help/HelpFilePath", "");
      if (storedPath != "")
         paths.Add(storedPath);

      paths.Add(wxGetCwd() + wxFILE_SEP_PATH + "audacity-help.htb");

      #ifdef __WXGTK__
      paths.Add("/usr/local/share/doc/audacity/audacity-help.htb");
      paths.Add("/usr/share/doc/audacity/audacity-help.htb");
      paths.Add("/usr/share/audacity/audacity-help.htb"); // for debian
      #endif

      #ifdef __WXMSW__
      paths.Add("C:\\Program Files\\Audacity\\audacity-help.htb");
      #endif

      for(int i=0; i<paths.GetCount() && helpFilePath==""; i++) {
         if (::wxFileExists(paths[i]))
            helpFilePath = paths[i];
      }

      if (helpFilePath == "") {
         helpFilePath = wxFileSelector("Where is audacity-help.htb?",
                                       NULL,
                                       "audacity-help.htb",  // Name
                                       "",                   // Extension
                                       "HTML Help Books (*.htb)|*.htb",
                                       wxOPEN, parent);
         if (helpFilePath == "")
            return;
      }

      gHelp = new wxHtmlHelpController();
      if (!gHelp->AddBook(helpFilePath)) {
         wxMessageBox("Couldn't open the Audacity Help file.");
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
      gHelp->Display("Main");
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
      wxString key = wxGetTextFromUser("Search for?",
                                       "Search help for keyword",
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
