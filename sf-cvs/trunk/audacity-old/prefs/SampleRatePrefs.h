/**********************************************************************

  Audacity: A Digital Audio Editor

  SampleRatePrefs.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_RATE_PREFS__
#define __AUDACITY_SAMPLE_RATE_PREFS__

#include <wx/window.h>
#include <wx/statbox.h>
#include <wx/radiobox.h>

#include "PrefsPanel.h"

class SampleRatePrefs:public PrefsPanel {

 public:
   SampleRatePrefs(wxWindow * parent);
   ~SampleRatePrefs();
   bool Apply();

 private:
    wxStaticBox * mEnclosingBox;
   wxRadioBox *mDefaultSamplingRate;

};

#endif
