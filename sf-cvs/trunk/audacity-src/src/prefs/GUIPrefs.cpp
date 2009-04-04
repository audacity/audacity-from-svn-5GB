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

#include "GUIPrefs.h"

////////////////////////////////////////////////////////////////////////////////

ShowPrefs::ShowPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Show"))
{
   Populate();
}

ShowPrefs::~ShowPrefs()
{
}

void ShowPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void ShowPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("Show"));
   {
	   S.TieCheckBox(_("Enable cut &lines"),
                    wxT("/GUI/EnableCutLines"),
                    false);
      S.TieCheckBox(_("Show warnings about &temp files"),
                    wxT("/GUI/WarnAboutTempFiles"),
                    true);
      S.TieCheckBox(_("Show prompt to sa&ve, even if project is empty"),    
                    wxT("/GUI/EmptyCanBeDirty"),
                    true);
      S.TieCheckBox(_("Show &Welcome Message at program start up"),
                    wxT("/GUI/ShowSplashScreen"),
                    true);
   }
   S.EndStatic();
}

bool ShowPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

////////////////////////////////////////////////////////////////////////////////

TracksPrefs::TracksPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Tracks"))
{
   Populate();
}

TracksPrefs::~TracksPrefs()
{
}

void TracksPrefs::Populate()
{
   mSoloCodes.Add(wxT("Standard"));
   mSoloCodes.Add(wxT("Simple"));
   mSoloCodes.Add(wxT("None"));

   mSoloChoices.Add(_("Standard"));
   mSoloChoices.Add(_("Simple"));
   mSoloChoices.Add(_("None"));

   mViewCodes.Add(wxT("Waveform"));
   mViewCodes.Add(wxT("WaveformdB"));
   mViewCodes.Add(wxT("Spectrum"));
   mViewCodes.Add(wxT("SpectrumLogF"));
   mViewCodes.Add(wxT("PitchEAC"));

   mViewChoices.Add(_("Waveform"));
   mViewChoices.Add(_("Waveform (dB)"));
   mViewChoices.Add(_("Spectrum"));
   mViewChoices.Add(_("Spectrum log(f)"));
   mViewChoices.Add(_("Pitch (EAC)"));

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void TracksPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("Display"));
   {
      S.TieCheckBox(_("&Update display while playing"),
                    wxT("/GUI/AutoScroll"),
                    true);
      S.TieCheckBox(_("Automatically &fit tracks vertically zoomed"), 
                    wxT("/GUI/TracksFitVerticallyZoomed"),
                    false);

      S.AddSpace(10);

      S.StartTwoColumn();
      {
         S.TieChoice(_("Default View Mode:"),
                     wxT("/GUI/DefaultViewMode"),
                     wxT("Waveform"),
                     mViewChoices,
                     mViewCodes);
      }
      S.EndTwoColumn();

   }
   S.EndStatic();

   S.StartStatic(_("Behaviors"));
   {
      S.TieCheckBox(_("Enable &dragging of left and right selection edges"),
                    wxT("/GUI/AdjustSelectionEdges"),
                    true);
      S.TieCheckBox(_("\"Move track focus\" c&ycles repeatedly through tracks"), 
                    wxT("/GUI/CircularTrackNavigation"),
                    false);
      S.TieCheckBox(_("Editing a &clip can move other clips"),
                    wxT("/GUI/EditClipCanMove"),
                    true);
      S.TieCheckBox(_("&Select all audio in project, if none selected"),    
                    wxT("/GUI/SelectAllOnNone"),
                    true);

      S.AddSpace(10);

      S.StartTwoColumn();
      {
         S.TieChoice(_("Solo Button:"),
                     wxT("/GUI/Solo"),
                     wxT("Standard"),
                     mSoloChoices,
                     mSoloCodes);
      }
      S.EndTwoColumn();
   }
   S.EndStatic();
}

bool TracksPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

////////////////////////////////////////////////////////////////////////////////

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

   mHtmlHelpCodes.Add(wxT("Standard"));
   mHtmlHelpCodes.Add(wxT("InBrowser"));
   mHtmlHelpCodes.Add(wxT("FromInternet"));

   mHtmlHelpChoices.Add(_("Standard"));
   mHtmlHelpChoices.Add(_("In Browser"));
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

      S.AddSpace(10);

      S.StartTwoColumn();
      {
         S.TieChoice(_("Display range:"),
                     wxT("/GUI/EnvdBRange"),
                     wxT("60"),
                     mRangeChoices,
                     mRangeCodes);
         S.TieChoice(_("Language:"),
                     wxT("/Locale/Language"),
                     wxT("en"),
                     mLangNames,
                     mLangCodes);
      }
      S.EndTwoColumn();
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

      S.AddSpace(10);

      S.StartTwoColumn();
      {
         S.TieChoice(_("Help:"),
                     wxT("/GUI/Help"),
                     wxT("Standard"),
                     mHtmlHelpChoices,
                     mHtmlHelpCodes);
      }
      S.EndTwoColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Modes"));
   {
#ifdef __WXDEBUG__
      S.TieCheckBox(_("Don't a&pply effects in batch mode"),  
                    wxT("/Batch/Debug"),
                    false);
#endif
      S.TieCheckBox(_("Cl&eanSpeech Mode (Customized GUI)"), 
                    wxT("/Batch/CleanSpeechMode"),
                    false);
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

