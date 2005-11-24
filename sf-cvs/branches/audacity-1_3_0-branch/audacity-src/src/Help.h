/**********************************************************************

  Audacity: A Digital Audio Editor

  Help.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_HELP__
#define __AUDACITY_HELP__

void InitHelp(wxWindow * parent);
void ShowHelp(wxWindow * parent);
void ShowHelp(wxWindow * parent, wxString topic);
void SearchHelp(wxWindow * parent);
void ShowHelpIndex(wxWindow * parent);

void QuitHelp();

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: c35b798b-e876-4527-abdf-e3ed9c37c700

