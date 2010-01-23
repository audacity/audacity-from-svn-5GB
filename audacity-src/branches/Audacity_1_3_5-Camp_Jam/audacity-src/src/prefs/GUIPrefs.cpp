/**********************************************************************

  Audacity: A Digital Audio Editor

  GUIPrefs.cpp

  Brian Gunlogson
  Joshua Haberman
  Dominic Mazzoni
  James Crook


*******************************************************************//**

\class GUIPrefs
\brief A PrefsPanel for general GUI prefernces.

*//*******************************************************************/

#include <wx/defs.h>
#include "../Audacity.h"
#include "../AudacityApp.h"
#include "../Envelope.h"
#include "../Languages.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../ShuttleGui.h"

#include "GUIPrefs.h"

GUIPrefs::GUIPrefs(wxWindow * parent):
   PrefsPanel(parent)
{
   SetLabel(_("Interface"));         // Provide visual label
   SetName(_("Interface"));          // Provide audible label

   mHtmlHelpCodes.Add(wxT("Standard"));
   mHtmlHelpCodes.Add(wxT("InBrowser"));
   mHtmlHelpCodes.Add(wxT("FromInternet"));

   mHtmlHelpChoices.Add(_("Standard"));
   mHtmlHelpChoices.Add(_("In Browser"));
   mHtmlHelpChoices.Add(_("From Internet"));

   mSoloCodes.Add( wxT("Standard") );
   mSoloCodes.Add( wxT("Simple") );
   mSoloCodes.Add( wxT("None") );

   mSoloChoices.Add(_("Standard") );
   mSoloChoices.Add(_("Simple") );
   mSoloChoices.Add(_("None") );

   Populate( );
}

void GUIPrefs::Populate( )
{
   // First any pre-processing for constructing the GUI.
   GetLanguages(mLangCodes, mLangNames);
#if 0
   // only for testing...
   mLangCodes.Add("kg");   mLangNames.Add("Klingon");
   mLangCodes.Add("ep");   mLangNames.Add("Esperanto");
#endif
#ifdef EXPERIMENTAL_SAVE_DEFAULT_VIEW
   gPrefs->Read(wxT("/DefaultViewMode"), &mDefaultViewMode, 0L);
#endif //EXPERIMENTAL_SAVE_DEFAULT_VIEW

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

// Code duplication warning: this default is repeated in Project.cpp
// in the destructor.  -DMM
#ifdef __WXMAC__
   const bool bQuitOnCloseDefault = false;
#else
   const bool bQuitOnCloseDefault = true;
#endif
// End code duplication warning

void GUIPrefs::PopulateOrExchange( ShuttleGui & S )
{
   S.SetBorder( 2 );
   S.StartHorizontalLay( wxEXPAND, 1 );
   S.StartVerticalLay();
   S.StartStatic( _("Behaviors"),1 );
   {
      S.TieCheckBox( _("&Update display while playing"),
         wxT("/GUI/AutoScroll"), true);
      S.TieCheckBox( _("Closing last window quits Audacity"),
         wxT("/GUI/QuitOnClose"), bQuitOnCloseDefault );
      S.TieCheckBox( _("Enable &dragging of left and right selection edges"),
         wxT("/GUI/AdjustSelectionEdges"), true);
      S.TieCheckBox( _("&Ergonomic order of audio I/O buttons"),
         wxT("/GUI/ErgonomicTransportButtons"), true);
      S.TieCheckBox( _("Automatically &fit tracks vertically zoomed"), 
         wxT("/GUI/TracksFitVerticallyZoomed"), false );
      S.TieCheckBox( _("\"Move track focus\" &cycles repeatedly through tracks"), 
         wxT("/GUI/CircularTrackNavigation"), false );
      S.TieCheckBox( _("Editing a &clip can move other clips"),
         wxT("/GUI/EditClipCanMove"), true );
      S.TieCheckBox( _("&Select all audio in project, if none selected"),    
         wxT("/GUI/SelectAllOnNone"), true );   
      S.TieCheckBox( _("&Beep on completion of longer activities"),    
         wxT("/GUI/BeepOnCompletion"), false );   
   }
   S.EndStatic();
   S.StartStatic( _("Show / Hide"),0 );
   {
	   S.TieCheckBox( _("Enable cut &lines"),
         wxT("/GUI/EnableCutLines"), false);
      S.TieCheckBox( _("Show warnings about &temp files"), 
         wxT("/GUI/WarnAboutTempFiles"), true );
      S.TieCheckBox( _("&Show prompt to save, even if project is empty"),    
         wxT("/GUI/EmptyCanBeDirty"), true );
      S.TieCheckBox( _("Show Welcome Message at program start up"),    
         wxT("/GUI/ShowSplashScreen"), true );
   }
   S.EndStatic();
   S.StartStatic( _("Modes"),0 );
   {
#ifdef __WXDEBUG__
      S.TieCheckBox( _("&Don't apply effects in batch mode"),  
         wxT("/Batch/Debug"), false);
#endif
      S.TieCheckBox( _("Cl&eanSpeech Mode (Customized GUI)"), 
         wxT("/Batch/CleanSpeechMode"), false);
   }
   S.EndStatic();
   S.EndVerticalLay();
   S.StartVerticalLay();
   S.StartStatic( _("Display range minimum: meters and 'Waveform (dB)'"),0 );
   {
      S.StartRadioButtonGroup( wxT("/GUI/EnvdBRange"), ENV_DB_RANGE );
      S.TieRadioButton( _("-36 dB (shallow range for high-amplitude editing)"),36);
      S.TieRadioButton( _("-48 dB (PCM range of 8 bit samples)"),48);
      S.TieRadioButton( _("-60 dB (PCM range of 10 bit samples)"),60);
      S.TieRadioButton( _("-96 dB (PCM range of 16 bit samples)"),96);
      S.TieRadioButton( _("-120 dB (approximate limit of human hearing)"),120);
      S.TieRadioButton( _("-145 dB (PCM range of 24 bit samples)"),145);
      S.EndRadioButtonGroup();
   }
   S.EndStatic();
#ifdef EXPERIMENTAL_SAVE_DEFAULT_VIEW
   S.StartStatic( _("Default View Mode") );
   {
      S.StartRadioButtonGroup( wxT("/GUI/DefaultViewMode"), mDefaultViewMode );
      S.TieRadioButton( _("WaveformDisplay"), 0);
      S.TieRadioButton( _("WaveformDBDisplay"), 1);
      S.TieRadioButton( _("SpectrumDisplay"), 2);
#ifdef LOGARITHMIC_SPECTRUM
      S.TieRadioButton( _("SpectrumLogDisplay"), 3);
#endif //LOGARITHMIC_SPECTRUM
      S.TieRadioButton( _("PitchDisplay"), 4);
      S.EndRadioButtonGroup();
   }
   S.EndStatic();
#endif //EXPERIMENTAL_SAVE_DEFAULT_VIEW

   S.StartStatic( _("Other interface choices"),1 );
   S.StartTwoColumn();
   S.TieChoice(_("Language:"),   wxT("/Locale/Language"),wxT("en"),mLangNames, mLangCodes );  
   S.TieChoice(_("Help:"),       wxT("/GUI/Help"),wxT("Standard"),mHtmlHelpChoices, mHtmlHelpCodes );  
   S.TieChoice(_("Solo Button:"),wxT("/GUI/Solo"),wxT("Standard"),mSoloChoices, mSoloCodes );  
   S.EndTwoColumn();
   S.EndStatic();

   S.EndVerticalLay();
   S.EndHorizontalLay();
}

bool GUIPrefs::Apply()
{
   ShuttleGui S( this, eIsSavingToPrefs );
   PopulateOrExchange( S );

   // If language has changed, we want to change it now, not on the next reboot.
   wxString lang = gPrefs->Read(wxT("/Locale/Language"), wxT(""));
   wxGetApp().InitLang( lang );

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

