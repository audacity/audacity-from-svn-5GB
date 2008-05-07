/**********************************************************************

  Audacity: A Digital Audio Editor

  QualityPrefs.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_QUALITY_PREFS__
#define __AUDACITY_QUALITY_PREFS__

#include "PrefsPanel.h"

class wxChoice;
class wxTextCtrl;

class ShuttleGui;

class QualityPrefs:public PrefsPanel 
{
public:
   QualityPrefs(wxWindow * parent);
   ~QualityPrefs();

   void OnSampleRateChoice(wxCommandEvent& evt);
   virtual bool Apply();
private:
   void Populate();
   void PopulateOrExchange( ShuttleGui & S );
   void DefineSampleRateControl( ShuttleGui & S );
   void GetNamesAndLabels();

   wxArrayString mmDitherNames;
   wxArrayInt    mmDitherLabels;
   wxArrayString mmSampleRateNames;
   wxArrayInt    mmSampleRateLabels;
   wxArrayString mmSampleFormatNames;
   wxArrayInt    mmSampleFormatLabels;
   wxArrayString mConverterNames;
   wxArrayInt    mConverterLabels;

   wxChoice *mSampleRates;
   wxTextCtrl *mOtherSampleRate;
   int mOtherSampleRateValue;
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
// arch-tag: ccb794d2-45d5-4f7b-ba0c-6a4d2438ac93

