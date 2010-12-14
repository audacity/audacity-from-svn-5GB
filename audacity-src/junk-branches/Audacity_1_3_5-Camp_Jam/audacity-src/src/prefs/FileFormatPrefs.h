/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormatPrefs.h

  Joshua Haberman
  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_FILE_FORMAT_PREFS__
#define __AUDACITY_FILE_FORMAT_PREFS__

#include "PrefsPanel.h"

class wxCommandEvent;
class wxStaticText;
class wxWindow;
class ShuttleGui;

class FileFormatPrefs:public PrefsPanel 
{
public:
   FileFormatPrefs(wxWindow * parent);
   ~FileFormatPrefs();
   virtual bool Apply();
   
private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void OnMP3FindButton(wxCommandEvent& evt);
   void OnMP3DownButton(wxCommandEvent& evt);
   void SetMP3VersionText(bool prompt = false);

   wxStaticText *mMP3Version;

public:
   DECLARE_EVENT_TABLE();
};

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
// arch-tag: 300a00cc-0770-45a1-8ab5-88cfb7ae1239

