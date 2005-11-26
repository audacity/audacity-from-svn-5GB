/**********************************************************************

  Audacity: A Digital Audio Editor

  GUIPrefs.h

  Brian Gunlogson
  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_GUI_PREFS__
#define __AUDACITY_GUI_PREFS__

#include <wx/defs.h>
#include <wx/string.h>

#include "PrefsPanel.h"

#ifdef __WXMSW__
    #include  <wx/ownerdrw.h>
#endif

//#include  "wx/log.h"
#include  <wx/sizer.h>
#include  <wx/menuitem.h>
#include  <wx/checklst.h>

#if wxUSE_CHECKLISTBOX
//  JKC: uncomment the #define to convert the lists of checkboxes
//  into scrollable lists.  We're likely to need this when the 
//  list of preferences is longer.
//#define USE_SCROLLING_CHECK_LISTBOX_IN_PREFS 1
#endif

class wxWindow;
class wxCheckBox;
class wxChoice;
class wxTextCtrl;
class wxStaticText;
class wxRadioButton;

//CheckBox containers may be sizers or CCheckListBoxes.
const int MAX_CHECKBOXES=30;
const int MAX_RADIO_BUTTONS=5;
const int NUM_CHECKBOX_CONTAINERS=2;

class GUIPrefs : public PrefsPanel {

public:
   GUIPrefs(wxWindow * parent);
   ~GUIPrefs();

   bool Apply();

private:
   wxCheckBox * CreateCheckBox( const wxString Description, const bool State);
   void CheckBoxAction(const wxString mDescription, const wxString mSettingName,
      bool bDefault, int mWindowID=0); //0 should be NoneID.
   void RadioButtonAction( const wxString Description, int iValue);
   void AllCheckBoxActions();
   void AllRadioButtonActions();

   wxRadioButton *mButtonArray[MAX_RADIO_BUTTONS];

   wxChoice *mLocale;
   wxStaticText *mLocaleLabel;

   wxArrayString mLangCodes;
   wxArrayString mLangNames;

// These private member variables are used in
// creating lists of checkboxes.
   wxCheckBox *mCheckBoxes[MAX_CHECKBOXES]; // CheckBoxes not contained in a ListBox.
#if USE_SCROLLING_CHECK_LISTBOX_IN_PREFS
   wxCheckListBox *mCheckListBoxes[  NUM_CHECKBOX_CONTAINERS]; // (Optional) ListBoxes of checkboxes
#endif
   wxSizer *mCheckListSizers[ NUM_CHECKBOX_CONTAINERS]; // Sizers that contain them.
   int      mCheckBoxCounters[NUM_CHECKBOX_CONTAINERS]; // How many in each list?
   int mCurrentCheckBoxContainer;  // Current container when creating / applying.
   wxSizer * mCurrentSizer;
   int mCurrentCheckBox; // 0..MAX_CHECKBOXES-1
   int mCurrentRadioButton;
   int mSelectedRadioButton;
   wxString mCurrentPrefName;
   int mCurrentPrefValue;
   bool mbCreating; // Are we currently creating the checkboxes?  
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
// arch-tag: 57018e2b-d264-4f93-bfa7-06752ebf631e

