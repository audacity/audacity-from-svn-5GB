/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyConfigPrefs.h

  Joshua Haberman
  Dominic Mazzoni
  Brian Gunlogson

**********************************************************************/

#ifndef __AUDACITY_KEY_CONFIG_PREFS__
#define __AUDACITY_KEY_CONFIG_PREFS__

#include "PrefsPanel.h"

class wxButton;
class wxChoice;
class wxCommandEvent;
class wxRadioButton;
class wxStaticText;
class wxWindow;

class KeyConfigPrefs:public PrefsPanel {

 public:
   KeyConfigPrefs(wxWindow * parent);
   ~KeyConfigPrefs();
   bool Apply();

   void OnFormatChoice(wxCommandEvent& evt);

 private:
   wxRadioButton *mCopyOrEdit[2];
   wxChoice *mMP3Bitrate;
   wxButton *mMP3FindButton;
   wxStaticText *mMP3Version;

   wxChoice *mSelectedCategory;
   wxButton *mExportOptionsButton;
   wxStaticText *mFormatText;

   int mFormat;
   int mFormatBits;

   void SetMP3VersionText();
   void SetFormatText();

   void Other();

 public:
   DECLARE_EVENT_TABLE();
};

#endif
