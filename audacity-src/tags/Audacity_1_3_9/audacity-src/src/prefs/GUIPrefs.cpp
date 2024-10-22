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

#include "../Audacity.h"

#include <wx/defs.h>

#include "../AudacityApp.h"
#include "../Languages.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"

#include "GUIPrefs.h"

GUIPrefs::GUIPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Interface"))
{
   Populate();
}

GUIPrefs::~GUIPrefs()
{
}

void GUIPrefs::Populate()
{
   // First any pre-processing for constructing the GUI.
   GetLanguages(mLangCodes, mLangNames);

   mHtmlHelpCodes.Add(wxT("Local"));
   mHtmlHelpCodes.Add(wxT("FromInternet"));

   mHtmlHelpChoices.Add(_("Local"));
   mHtmlHelpChoices.Add(_("From Internet"));

   mRangeCodes.Add(_("36"));
   mRangeCodes.Add(_("48"));
   mRangeCodes.Add(_("60"));
   mRangeCodes.Add(_("96"));
   mRangeCodes.Add(_("120"));
   mRangeCodes.Add(_("145"));

   mRangeChoices.Add(_("-36 dB (shallow range for high-amplitude editing)"));
   mRangeChoices.Add(_("-48 dB (PCM range of 8 bit samples)"));
   mRangeChoices.Add(_("-60 dB (PCM range of 10 bit samples)"));
   mRangeChoices.Add(_("-96 dB (PCM range of 16 bit samples)"));
   mRangeChoices.Add(_("-120 dB (approximate limit of human hearing)"));
   mRangeChoices.Add(_("-145 dB (PCM range of 24 bit samples)"));

#if 0
   // only for testing...
   mLangCodes.Add("kg");   mLangNames.Add("Klingon");
   mLangCodes.Add("ep");   mLangNames.Add("Esperanto");
#endif

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

void GUIPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("Display"));
   {
      S.TieCheckBox(_("Er&gonomic order of audio I/O buttons"),
                    wxT("/GUI/ErgonomicTransportButtons"),
                    true);
      S.TieCheckBox(_("Show '&How to get Help' message at program start up"),
                    wxT("/GUI/ShowSplashScreen"),
                    true);

      S.AddSpace(10);

      S.StartMultiColumn(2);
      {
         S.TieChoice(_("Meter/Waveform dB range:"),
                     wxT("/GUI/EnvdBRange"),
                     wxT("60"),
                     mRangeChoices,
                     mRangeCodes);
         S.SetSizeHints(mRangeChoices);

         S.TieChoice(_("Language:"),
                     wxT("/Locale/Language"),
                     wxT("en"),
                     mLangNames,
                     mLangCodes);
         S.SetSizeHints(mLangNames);

         S.TieChoice(_("Location of Manual:"),
                     wxT("/GUI/Help"),
                     wxT("Local"),
                     mHtmlHelpChoices,
                     mHtmlHelpCodes);
         S.SetSizeHints(mHtmlHelpChoices);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Behaviors"));
   {
      S.TieCheckBox(_("Cl&osing last window quits Audacity"),
                    wxT("/GUI/QuitOnClose"),
                    bQuitOnCloseDefault);
      S.TieCheckBox(_("&Beep on completion of longer activities"),
                    wxT("/GUI/BeepOnCompletion"),
                    false);
   }
   S.EndStatic();

   S.StartStatic(_("Modes"));
   {
      S.TieCheckBox(_("Cl&eanSpeech Mode (Customized GUI)"), 
                    wxT("/Batch/CleanSpeechMode"),
                    false);
#ifdef __WXDEBUG__
      S.TieCheckBox(_("Don't a&pply effects in batch mode"),  
                    wxT("/Batch/Debug"),
                    false);
#endif
   }
   S.EndStatic();
}

bool GUIPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   // If language has changed, we want to change it now, not on the next reboot.
   wxString lang = gPrefs->Read(wxT("/Locale/Language"), wxT(""));
   wxGetApp().InitLang(lang);

   return true;
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
