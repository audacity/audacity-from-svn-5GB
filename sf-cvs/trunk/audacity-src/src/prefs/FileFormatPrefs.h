/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormatPrefs.h

  Joshua Haberman
  Dominic Mazzoni
  James Crook

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
class ShuttleGui;

class FileFormatPrefs:public PrefsPanel 
{
public:
   FileFormatPrefs(wxWindow * parent);
   ~FileFormatPrefs();
   virtual bool Apply();
   
private:
   void Populate();
   void PopulateOrExchange( ShuttleGui & S );
   void GetNamesAndLabels();
   void OnFormatChoice(wxCommandEvent& evt);
   void OnMP3FindButton(wxCommandEvent& evt);
   void OnMP3VBR(wxCommandEvent& evt);
   void OnMP3CBR(wxCommandEvent& evt);
   void SetMP3VersionText();
   void SetFormatText();
   void OpenOtherFormatDialog();

   int mOggQualityUnscaled;
   int mFormat;

   wxArrayString mMP3RateNames;
   wxArrayInt    mMP3RateLabels;
   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;
   wxArrayString mFormatNames;
   wxString      mFormatFromChoice;

   wxRadioButton *mMP3Stereo;
   wxRadioButton *mMP3Joint;
   wxRadioButton *mMP3VBR;
   wxRadioButton *mMP3CBR;
   wxChoice *mMP3Bitrate;
   wxStaticText *mMP3Version;
   wxStaticText *mFormatText;
   wxChoice *mDefaultExportFormat;

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

