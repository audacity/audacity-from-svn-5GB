/**********************************************************************

  Audacity: A Digital Audio Editor

  SampleRatePrefs.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_RATE_PREFS__
#define __AUDACITY_SAMPLE_RATE_PREFS__

#include "PrefsPanel.h"

class wxRadioButton;

class SampleRatePrefs:public PrefsPanel {

 public:
   SampleRatePrefs(wxWindow * parent);
   ~SampleRatePrefs();
   bool Apply();

 private:
   wxRadioButton *mSampleRates[6];

};

#endif
