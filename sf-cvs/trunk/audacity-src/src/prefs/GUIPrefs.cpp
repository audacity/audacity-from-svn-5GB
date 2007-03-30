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
   S.StartHorizontalLay( wxEXPAND, 0 );
   S.StartStatic( _("Behaviors"),1 );
   {
      S.TieCheckBox( _("&Autoscroll while playing"),
         wxT("/GUI/AutoScroll"), true);
      S.TieCheckBox( _("Al&ways allow pausing"),
         wxT("/GUI/AlwaysEnablePause"), false);
      S.TieCheckBox( _("&Update spectrogram while playing"),
         wxT("/GUI/UpdateSpectrogram"), true);
      S.TieCheckBox( _("&Quit Audacity upon closing last window"),
         wxT("/GUI/QuitOnClose"), bQuitOnCloseDefault );
      S.TieCheckBox( _("&Dragging of left and right selection edges"),
         wxT("/GUI/AdjustSelectionEdges"), true);
      S.TieCheckBox( _("&Ergonomic order of audio I/O buttons"),
         wxT("/GUI/ErgonomicTransportButtons"), true);
      S.TieCheckBox( _("Tracks &fit vertically zoomed"), 
         wxT("/GUI/TracksFitVerticallyZoomed"), false );
      S.TieCheckBox( _("&Circular track keyboard navigation"), 
         wxT("/GUI/CircularTrackNavigation"), false );
      S.TieCheckBox( _("&Present track number to accessibility aid"), 
         wxT("/GUI/PresentTrackNumber"), true );
      S.TieCheckBox( _("Editing a &clip can move other clips"),
         wxT("/GUI/EditClipCanMove"), true );
   }
   S.EndStatic();
   S.StartStatic( _("Show / Hide"),1 );
   {
	   S.TieCheckBox( _("Enable cut &lines"),
         wxT("/GUI/EnableCutLines"), false);
      S.TieCheckBox( _("Show warnings about &temp files"), 
         wxT("/GUI/WarnAboutTempFiles"), true );
   }
   S.EndStatic();
   S.EndHorizontalLay();
   S.StartStatic( _("Language") );
   S.StartTwoColumn();
   S.TieChoice(_("Language:"),wxT("/Locale/Language"),wxT("en"),mLangNames, mLangCodes );  
   S.EndTwoColumn();
   S.EndStatic();
   S.StartStatic( _("Minimum of dB mode display range") );
   {
      S.StartRadioButtonGroup( wxT("/GUI/EnvdBRange"), ENV_DB_RANGE );
      S.TieRadioButton( _("-36 dB (shallow range for high-amplitude editing)"),36);
      S.TieRadioButton( _("-48 dB (PCM range of 8 bit samples)"),48);
      S.TieRadioButton( _("-96 dB (PCM range of 16 bit samples)"),96);
      S.TieRadioButton( _("-120 dB (approximate limit of human hearing)"),120);
      S.TieRadioButton( _("-145 dB (PCM range of 24 bit samples)"),145);
      S.EndRadioButtonGroup();
   }
   S.EndStatic();
}

bool GUIPrefs::Apply()
{
   ShuttleGui S( this, eIsSavingToPrefs );
   PopulateOrExchange( S );

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

