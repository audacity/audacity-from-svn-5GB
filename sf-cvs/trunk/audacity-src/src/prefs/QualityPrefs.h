/**********************************************************************

  Audacity: A Digital Audio Editor

  QualityPrefs.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_RATE_PREFS__
#define __AUDACITY_SAMPLE_RATE_PREFS__

#include "PrefsPanel.h"

class wxRadioButton;

class QualityPrefs:public PrefsPanel {

 public:
   QualityPrefs(wxWindow * parent);
   ~QualityPrefs();
   bool Apply();

 private:
   wxRadioButton *mSampleRates[6];

};

#endif
