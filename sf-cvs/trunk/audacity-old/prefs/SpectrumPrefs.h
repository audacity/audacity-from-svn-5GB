/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumPrefs.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SPECTRUM_PREFS__
#define __AUDACITY_SPECTRUM_PREFS__

#include <wx/window.h>
#include <wx/statbox.h>
#include <wx/checkbox.h>
#include <wx/radiobox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "PrefsPanel.h"

class SpectrumPrefs:public PrefsPanel {

 public:
   SpectrumPrefs(wxWindow * parent);
   ~SpectrumPrefs();
   bool Apply();

 private:
    wxStaticBox * mEnclosingBox;
   wxRadioBox *mFFTSize;
   wxCheckBox *mGrayscale;

   wxStaticText *mMaxFreqLabel;
   wxTextCtrl *mMaxFreqCtrl;
};

#endif
