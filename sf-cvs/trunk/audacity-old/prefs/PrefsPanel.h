/**********************************************************************

  Audacity: A Digital Audio Editor

  PrefsPanel.h

  Joshua Haberman

  The interface works like this: Each panel in the preferences dialog
  must derive from PrefsPanel. You must override Apply() with code
  to validate fields (returning false if any are bad), updating the
  global preferences object gPrefs, and instructing the applicable parts
  of the program to re-read the preference options.

  To actually add a the new panel, edit the PrefsDialog constructor
  to append the panel to its list of panels.

**********************************************************************/

#ifndef __AUDACITY_PREFS_PANEL__
#define __AUDACITY_PREFS_PANEL__

#include <wx/panel.h>
#include <wx/window.h>
#include <wx/gdicmn.h>

/* A few constants for an attempt at semi-uniformity */
#ifdef WIN32
#define PREFS_FONT_SIZE     8
#else
#define PREFS_FONT_SIZE     10
#endif

#define PREFS_SIDE_MARGINS  10
#define PREFS_TOP_MARGIN    17
#define PREFS_BOTTOM_MARGIN 10

class PrefsPanel:public wxPanel {

 public:
   PrefsPanel(wxWindow * parent):wxPanel(parent, -1, wxPoint(1160, 20),
                                         wxSize(320, 350)) {
      SetFont(wxFont(PREFS_FONT_SIZE, wxDEFAULT, wxNORMAL, wxNORMAL));
      Show(false);
      mPrefsHidden = true;
   } virtual ~ PrefsPanel() {
   }
   virtual bool Apply() = 0;

   virtual void HidePrefsPanel() {
      if (!mPrefsHidden) {
         Show(false);
         Move(1160, 20);
         mPrefsHidden = true;
      }
   }

   virtual void ShowPrefsPanel() {
      if (mPrefsHidden) {
         Move(160, 20);
         Show(true);
         mPrefsHidden = false;
      }
   }

 private:
   bool mPrefsHidden;

};

#endif
