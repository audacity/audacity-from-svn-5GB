/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormatPrefs.h

  Joshua Haberman
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_FILE_FORMAT_PREFS__
#define __AUDACITY_FILE_FORMAT_PREFS__

#include "../Project.h"
#include "PrefsPanel.h"

class wxButton;
class wxCheckBox;
class wxChoice;
class wxCommandEvent;
class wxRadioButton;
class wxStaticText;
class wxSlider;
class wxWindow;

class FileFormatPrefs:public PrefsPanel {

 public:
   FileFormatPrefs(wxWindow * parent);
   ~FileFormatPrefs();
   bool Apply();
   
   void OnFormatChoice(wxCommandEvent& evt);
   void OnMP3FindButton(wxCommandEvent& evt);

 private:
   wxRadioButton *mCopyOrEdit[2];
   wxCheckBox *mOGGEnabled;
   wxChoice *mMP3Bitrate;
   wxButton *mMP3FindButton;
   wxStaticText *mMP3Version;
   wxSlider *mOGGQuality;

   wxChoice *mDefaultExportFormat;
   wxButton *mExportOptionsButton;
   wxStaticText *mFormatText;

   AudacityProject *mAudacity;

   int mFormat;
   wxRadioButton *mDownMix[2];

   void SetMP3VersionText();
   void SetFormatText();

   void Other();

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

