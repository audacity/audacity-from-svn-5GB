/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyConfigPrefs.h

  Brian Gunlogson

**********************************************************************/

#ifndef __AUDACITY_KEY_CONFIG_PREFS__
#define __AUDACITY_KEY_CONFIG_PREFS__

#include "PrefsPanel.h"
#include "../Project.h"

class wxChoice;
class wxCharEvent;
class wxStaticText;
class wxListCtrl;
class wxListEvent;
class wxWindow;
class SysKeyTextCtrl;

class KeyConfigPrefs:public PrefsPanel {

 public:
   KeyConfigPrefs(wxWindow * parent);
   ~KeyConfigPrefs();
   bool Apply();

 private:
   void OnItemSelected(wxListEvent &event);
   void OnKeyEvent(wxCharEvent & event);

   wxListCtrl *mCommandsList;
   wxListCtrl *mKeysList;
   SysKeyTextCtrl *mCurrentComboText;

   int mCommandSelected;

   AudacityProject *mAudacity;

 public:
   DECLARE_EVENT_TABLE();
};

//BG: A quick and dirty override of wxTextCtrl to capture keys like Ctrl, Alt

class SysKeyTextCtrl:public wxTextCtrl
{
public:
   SysKeyTextCtrl(wxWindow *parent, wxWindowID id,
                  const wxString& value = wxEmptyString,
                  const wxPoint& pos = wxDefaultPosition,
                  const wxSize& size = wxDefaultSize,
                  long style = 0,
                  const wxValidator& validator = wxDefaultValidator,
                  const wxString& name = wxTextCtrlNameStr);
   ~SysKeyTextCtrl();

private:
   void OnChar(wxKeyEvent& event);

protected:
   DECLARE_EVENT_TABLE()
};

#endif
