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
#include <wx/radiobut.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "PrefsPanel.h"

class SpectrumPrefs:public PrefsPanel {

 public:
   SpectrumPrefs(wxWindow * parent);
   ~SpectrumPrefs();
   bool Apply();

 private:
   /* an arbitrary limit designed to be past what we'll ever need */
   wxRadioButton *mFFTSize[20];
   wxCheckBox *mGrayscale;

   wxTextCtrl *mMaxFreqCtrl;
};

#endif
