/**********************************************************************

  Audacity: A Digital Audio Editor

  GUIPrefs.h

  Brian Gunlogson
  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_GUI_PREFS__
#define __AUDACITY_GUI_PREFS__

#include "PrefsPanel.h"

class wxWindow;
class wxCheckBox;

class GUIPrefs : public PrefsPanel {

 public:
   GUIPrefs(wxWindow * parent);
   ~GUIPrefs();
   bool Apply();

 private:
    wxCheckBox *mAutoscroll;
    wxCheckBox *mSpectrogram;
};

#endif
