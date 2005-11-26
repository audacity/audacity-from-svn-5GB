/**********************************************************************

  Audacity: A Digital Audio Editor

  GUIPrefs.cpp

  Brian Gunlogson
  Joshua Haberman
  Dominic Mazzoni

**********************************************************************/

#include "GUIPrefs.h"

#include "../Audacity.h"
#include "../Envelope.h"
#include "../Languages.h"
#include "../Prefs.h"
#include "../Project.h"  //lda

#include <wx/defs.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/radiobut.h>


// Titles for the lists of check boxes.
const wxString Titles[NUM_CHECKBOX_CONTAINERS] =
{
   _("Behaviors"),  // e.g. 'Pause Always Allowed'.
   _("Show / Hide") // e.g. show/hide a toolbar or a tabbed window.
};

GUIPrefs::GUIPrefs(wxWindow * parent):
   PrefsPanel(parent)
{
   int i;

   topSizer = new wxBoxSizer( wxVERTICAL );
   // CheckSizer is a sizer that holds the left and right sizers.
   wxBoxSizer * CheckSizer = new wxBoxSizer( wxHORIZONTAL );

   // We have two containers, a left and a right one.
   for(i=0;i<NUM_CHECKBOX_CONTAINERS;i++)
   {
      mCheckListSizers[i] = new wxStaticBoxSizer( 
            new wxStaticBox(this, -1, 
         Titles[i] ),
         wxVERTICAL );

#if USE_SCROLLING_CHECK_LISTBOX_IN_PREFS
      mCheckListBoxes[i] = new wxCheckListBox
           (
            this,                  // parent
            -1,                    // control id
            wxPoint(10, 10),       // listbox poistion
            wxSize(245, 130),      // listbox size
            0,                     // initially no entries.
            NULL,                  // initial empty list of strings.
            0
           );
      if( mCheckListBoxes[i] )
         mCheckListSizers[i]->Add(mCheckListBoxes[i], 1, wxGROW|wxALL, 2);
#endif
      CheckSizer->Add( mCheckListSizers[i], 1, wxGROW|wxALL, 1);
   }

   // And CheckSizer is itself added in to the topSizer.
   topSizer->Add( CheckSizer, 0, wxGROW | wxALL, TOP_LEVEL_BORDER );

   // Create all the checkboxes.
   mbCreating=true;
   AllCheckBoxActions();

   // Locale
   GetLanguages(mLangCodes, mLangNames);
   int numLangs = mLangNames.GetCount();

   wxString currentLang = gPrefs->Read(wxT("/Locale/Language"), wxT("en"));
   wxString *langArray = new wxString[numLangs];

   for(i=0; i<numLangs; i++)
      langArray[i] = mLangNames[i];
   mLocale = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                          numLangs, langArray);
   mLocale->SetSelection(0); // in case nothing else matches
   delete[] langArray;
   for(i=0; i<numLangs; i++)
      if (mLangCodes[i] == currentLang)
         mLocale->SetSelection(i);

   mLocaleLabel = new wxStaticText(this, -1, _("Language:"));

   wxStaticBoxSizer *staticSizer = new wxStaticBoxSizer(new wxStaticBox(this, -1, _("Language")), wxVERTICAL);
   wxBoxSizer *localeSizer = new wxBoxSizer(wxHORIZONTAL);
   localeSizer->Add(mLocaleLabel, 0, wxALIGN_LEFT|wxRIGHT|wxALIGN_CENTER_VERTICAL, 2);
   localeSizer->Add(mLocale, 1, wxGROW|wxLEFT|wxALIGN_CENTER_VERTICAL, 2 );
   staticSizer->Add(localeSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER); 
   topSizer->Add(staticSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);
   
   // dB range display setup
   mCurrentSizer = new wxStaticBoxSizer(
      new wxStaticBox(this, -1,
          _("Minimum of dB mode display range")),
      wxVERTICAL );

   AllRadioButtonActions();
   topSizer->Add( mCurrentSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER );

   // Finish layout
   outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   SetAutoLayout(true);
   SetSizer(outSizer);

   outSizer->Fit(this);
   outSizer->SetSizeHints(this);
}


wxCheckBox * GUIPrefs::CreateCheckBox(const wxString Description, const bool State)
{
   wxSizer * pSizer;
   pSizer = mCheckListSizers[mCurrentCheckBoxContainer];
   wxASSERT( pSizer );
   wxCheckBox * pCheckBox = new wxCheckBox(this, -1, Description);
   pCheckBox->SetValue(State);
   pSizer->Add(pCheckBox, 0, wxGROW|wxALL, 2);
   return pCheckBox;
}

// Function that deals with one checkbox, either creating it or
// using its value to update gPrefs and the GUI.
// The function is made more complex by coping with 
// checkboxes in a sizer or checkboxes in a check list box.
void GUIPrefs::CheckBoxAction(
   const wxString Description,
   const wxString SettingName,
   bool bDefault,
   int mWindowID)
{
   bool bValue=false;
   bool bIsInListBox;

// TODO:  What should we do if on a Mac?
// wxWidgets 2.4.1 documentation suggests that wxCheckListBox isn't 
// supported on Mac.
#if USE_SCROLLING_CHECK_LISTBOX_IN_PREFS
   int Counter = ++(mCheckBoxCounters[mCurrentCheckBoxContainer]);

   wxCheckListBox * pCurrentList = mCheckListBoxes[mCurrentCheckBoxContainer];
   // If there was a non null list box, then it goes in it.
   bIsInListBox = ( pCurrentList != NULL );
#else
   bIsInListBox = false;
#endif

   if( !bIsInListBox )
   {
      // It's on the dialog, better make sure we
      // have a slot for a check box pointer for it.
      mCurrentCheckBox++;
      wxASSERT( mCurrentCheckBox < MAX_CHECKBOXES );
   }

   // IF Creating THEN create the checkbox, reading from gPrefs
   if( mbCreating )
   {
      gPrefs->Read(SettingName, &bValue, bDefault);
      if( !bIsInListBox )
      {
         mCheckBoxes[ mCurrentCheckBox ] = 
            CreateCheckBox( Description, bValue );
      }
      else
      {
#if USE_SCROLLING_CHECK_LISTBOX_IN_PREFS
         wxASSERT( pCurrentList );
         pCurrentList->Append( Description );
         pCurrentList->Check(Counter, bValue);
#endif
      }
   }
   // ELSE we are taking the value and writing it to 
   // gPrefs and then applying the settings
   else
   {
      if( !bIsInListBox )
      {
         wxASSERT( mCheckBoxes[ mCurrentCheckBox ] != NULL );
         bValue = mCheckBoxes[ mCurrentCheckBox ]->GetValue();
      }
      else
      {
#if USE_SCROLLING_CHECK_LISTBOX_IN_PREFS
         wxASSERT( pCurrentList );
         bValue = pCurrentList->IsChecked( Counter );
#endif
      }
      gPrefs->Write(SettingName,bValue);
   }
}


void GUIPrefs::RadioButtonAction(
   const wxString mDescription,
   int iValue)
{
   mCurrentRadioButton++;
   if( mbCreating )
   {
      mButtonArray[mCurrentRadioButton] = new wxRadioButton(
         this, -1, mDescription,
         wxDefaultPosition, 
         wxDefaultSize, 
         (mCurrentRadioButton==0) ? wxRB_GROUP : 0 );

      mCurrentSizer->Add( mButtonArray[mCurrentRadioButton], 0,
         wxGROW|wxLEFT | wxRIGHT, 2 );
      mButtonArray[mCurrentRadioButton]->SetValue(mCurrentPrefValue<=iValue);
      if( (mSelectedRadioButton<0) &&(mCurrentPrefValue<=iValue ))
      {
         mSelectedRadioButton = mCurrentRadioButton;
      }
      else
      {
         mButtonArray[mCurrentRadioButton]->SetValue(false);
      }
   }
   else
   {
      if((mCurrentRadioButton == 0) || mButtonArray[mCurrentRadioButton]->GetValue())
         mCurrentPrefValue = iValue;
   }
}



/// This is a condensed list of all the check boxes.
/// This function is used in two ways :
///   a) when creating the check boxes.
///   b) when applying the settings.
/// Member variables of GUIPrefs are set before calling this function
/// so that it 'knows what to do'.
/// This removes repetitive code and guarantees that the same setting 
/// names are used in saving the parameters as were used in retrieving 
/// them.
void GUIPrefs::AllCheckBoxActions()
{

   // Code duplication warning: this default is repeated in Project.cpp
   // in the destructor.  -DMM
   #ifdef __WXMAC__
      const bool bQuitOnCloseDefault = false;
   #else
      const bool bQuitOnCloseDefault = true;
   #endif
   // End code duplication warning
   mCurrentCheckBox=-1;
   mCheckBoxCounters[0]=-1;
   mCheckBoxCounters[1]=-1;
   mCurrentCheckBoxContainer=0;

   CheckBoxAction( _("Autoscroll while playing"),
      wxT("/GUI/AutoScroll"), true);
   CheckBoxAction( _("Always allow pausing"),
      wxT("/GUI/AlwaysEnablePause"), true);
   CheckBoxAction( _("Update spectrogram while playing"),
      wxT("/GUI/UpdateSpectrogram"), true);
   CheckBoxAction( _("Quit Audacity upon closing last window"),
      wxT("/GUI/QuitOnClose"), bQuitOnCloseDefault );
   CheckBoxAction(
      _("Dragging of left and right selection edges"),
      wxT("/GUI/AdjustSelectionEdges"), true);
   CheckBoxAction(
      _("Ergonomic order of audio I/O buttons"),
      wxT("/GUI/ErgonomicTransportButtons"), true);

//lda
   CheckBoxAction(_("Tracks fit vertically zoomed"), wxT("/GUI/TracksFitVerticallyZoomed"), false );
   mCurrentCheckBoxContainer=1;

	CheckBoxAction(
      _("Enable cut lines"),
      wxT("/GUI/EnableCutLines"), false);
   CheckBoxAction(
		_("Show warnings about temp files"), 
		wxT("/GUI/WarnAboutTempFiles"), true );
   
// Don't yet allow normal users to disable the main mix window.
//   CheckBoxAction(
//      _("Main Mix"),
//      wxT("/GUI/MainMix"), true,-1);
#if 0
   CheckBoxAction(
      _("Main Mix (as a tree)"),
      wxT("/GUI/MainMixTree"), false,-1);
   CheckBoxAction(
      _("Clips"),
      wxT("/GUI/Clips"), false,-1);
   CheckBoxAction(
      _("Effects Graph"),
      wxT("/GUI/EffectsGraph"), false,-1);
   CheckBoxAction(
      _("Nyquist Debugger"),
      wxT("/GUI/NyquistDebugger"), false,-1);
   CheckBoxAction(
      _("Audacity Tester"), 
      wxT("/GUI/AudacityTester"), false,-1);
#endif
};

/// This is a condensed list of all the RadioButtons.
/// This function is used in two ways :
///   a) when creating the buttons.
///   b) when applying the settings.
/// Member variables of GUIPrefs are set before calling this function
/// so that it 'knows what to do'.
void GUIPrefs::AllRadioButtonActions()
{
   mCurrentRadioButton=-1;

   mCurrentPrefName = wxT("/GUI/EnvdBRange");
   mCurrentPrefValue = -1;
   mSelectedRadioButton=-1;

   if( mbCreating) 
      gPrefs->Read(mCurrentPrefName, &mCurrentPrefValue, ENV_DB_RANGE);

   RadioButtonAction( 
      _("-36 dB (shallow range for high-amplitude editing)"),36);
   RadioButtonAction( 
      _("-48 dB (PCM range of 8 bit samples)"),48);
   RadioButtonAction( 
      _("-96 dB (PCM range of 16 bit samples)"),96);
   RadioButtonAction( 
      _("-120 dB (approximate limit of human hearing)"),120);
   RadioButtonAction( 
      _("-145 dB (PCM range of 24 bit samples)"),145);

   if( mbCreating )
      mButtonArray[mSelectedRadioButton]->SetValue(true);
   else
      gPrefs->Write(mCurrentPrefName, mCurrentPrefValue);
}

bool GUIPrefs::Apply()
{
   mbCreating = false;
   AllCheckBoxActions();
   int localeIndex = mLocale->GetSelection();
   if (localeIndex >= 0 && (unsigned) localeIndex < mLangCodes.GetCount())
      gPrefs->Write(wxT("/Locale/Language"), mLangCodes[localeIndex]);

   AllRadioButtonActions();
   unsigned int j;
   for(j = 0; j < gAudacityProjects.GetCount(); j++)
   {
      gAudacityProjects[j]->UpdateGuiPrefs();
   }
   return true;
}

GUIPrefs::~GUIPrefs()
{
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 7e997d04-6b94-4abb-b3d6-748400f86598

