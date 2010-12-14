/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyConfigPrefs.h

  Brian Gunlogson
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_KEY_CONFIG_PREFS__
#define __AUDACITY_KEY_CONFIG_PREFS__

#include <wx/defs.h>
#include <wx/textctrl.h>
#include <wx/string.h>

#include "PrefsPanel.h"
#include "../Project.h"

class wxChoice;
class wxCharEvent;
class wxStaticText;
class wxListCtrl;
class wxListEvent;
class wxWindow;

class SysKeyTextCtrl;
class CommandManager;
class ShuttleGui;

class KeyConfigPrefs:public PrefsPanel 
{

public:
   KeyConfigPrefs(wxWindow * parent);
   ~KeyConfigPrefs();
   virtual bool Apply();

private:
   void Populate();
   void PopulateOrExchange( ShuttleGui & S );
   void CreateList();

   void OnDefaults(wxCommandEvent& event);
   void OnLoad(wxCommandEvent& event);
   void OnSave(wxCommandEvent& event);
   void OnSet(wxCommandEvent& event);
   void OnClear(wxCommandEvent& event);
   void OnItemSelected(wxListEvent &event);
   void OnKeyDown(wxListEvent &event);
   void RepopulateBindingsList();

   SysKeyTextCtrl *mCurrentComboText;
   wxListCtrl * mList;
   CommandManager *mManager;
   int mCommandSelected;
   wxArrayString mNames;

public:
   DECLARE_EVENT_TABLE();
};

/// \brief BG: A quick and dirty override of wxTextCtrl to capture keys like Ctrl, Alt
///
/// This is only ever used by KeyConfigPrefs.
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
   void OnKey(wxKeyEvent& event);
   void OnChar(wxKeyEvent& event);

protected:
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
// arch-tag: 40d9b726-ab6b-431f-b384-e1a66303dba5

