/**********************************************************************

  Audacity: A Digital Audio Editor

  MousePrefs.h

**********************************************************************/

#ifndef __AUDACITY_MOUSE_PREFS__
#define __AUDACITY_MOUSE_PREFS__

#include <wx/string.h>

#include "PrefsPanel.h"

class wxWindow;
class wxCheckBox;
class wxChoice;
class wxListCtrl;

class MousePrefs:public PrefsPanel {

 public:
   MousePrefs(wxWindow * parent);
   ~MousePrefs();
   void AddItem( wxString const & MouseButtons, 
      wxString const & Tool, 
      wxString const & Action,
      wxString const & Comment = wxString(""));
   bool Apply();

 private:
    wxListCtrl * mList;

 public:
    DECLARE_EVENT_TABLE()

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
// arch-tag: d7366e17-0464-4863-8b68-de4edea64974

