/**********************************************************************

  Audacity: A Digital Audio Editor

  QualityPrefs.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_RATE_PREFS__
#define __AUDACITY_SAMPLE_RATE_PREFS__

#include "PrefsPanel.h"

class wxChoice;
class wxTextCtrl;

class QualityPrefs:public PrefsPanel {

 public:
   QualityPrefs(wxWindow * parent);
   ~QualityPrefs();

   void OnSampleRateChoice(wxCommandEvent& evt);

   bool Apply();

 private:
   wxChoice *mSampleRates;
   wxChoice *mSampleFormats;
   wxTextCtrl *mOtherSampleRate;

   wxChoice *mConverters;
   wxChoice *mHQConverters;

   wxChoice *mDithers;
   wxChoice *mHQDithers;

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

