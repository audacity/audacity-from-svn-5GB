/**********************************************************************

  Audacity: A Digital Audio Editor

  Help.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_HELP__
#define __AUDACITY_HELP__

void InitHelp();
void ShowHelp();
void ShowHelp(wxString topic);
void SearchHelp(wxWindow *parent);
void ShowHelpIndex();

void QuitHelp();

#endif



