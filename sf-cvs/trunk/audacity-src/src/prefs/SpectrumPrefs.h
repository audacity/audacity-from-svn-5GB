/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumPrefs.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SPECTRUM_PREFS__
#define __AUDACITY_SPECTRUM_PREFS__

#include "PrefsPanel.h"

class wxRadioButton;
class wxCheckBox;
class wxTextCtrl;

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

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: d68b1a74-12e3-49a5-a50b-3ba6fe65b40b

