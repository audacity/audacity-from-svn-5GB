/**********************************************************************

  Audacity: A Digital Audio Editor

  Help.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/textdlg.h>
#include <wx/html/helpctrl.h>

#include "Help.h"

wxHtmlHelpController     *gHelp = NULL;

void InitHelp()
{
  if (!gHelp) {
	gHelp = new wxHtmlHelpController();
	gHelp->Initialize("help/audacity");
  }
}

void ShowHelp()
{
  InitHelp();

  gHelp->Display("Introduction");
}

void ShowHelpIndex()
{
  InitHelp();

  gHelp->DisplayIndex();
}

void ShowHelp(wxString topic)
{
  gHelp->KeywordSearch(topic);
}

void SearchHelp(wxWindow *parent)
{
  wxString key = wxGetTextFromUser("Search for?",
								   "Search help for keyword",
								   "",
								   parent);

  if(!key.IsEmpty())
	gHelp->KeywordSearch(key);
}

void QuitHelp()
{
  if (gHelp) {
	delete gHelp;
	gHelp = NULL;
  }
}
