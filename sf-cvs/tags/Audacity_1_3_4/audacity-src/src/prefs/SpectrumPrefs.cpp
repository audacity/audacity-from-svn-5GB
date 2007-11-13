/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumPrefs.cpp

  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class SpectrumPrefs
\brief A PrefsPanel for spectrum settings.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>

#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "SpectrumPrefs.h"
#include "../FFT.h"

enum {
   ID_MINFREQUENCY = 8000,
   ID_MAXFREQUENCY
};

SpectrumPrefs::SpectrumPrefs(wxWindow * parent):
   PrefsPanel(parent)
{
   SetLabel(_("Spectrograms"));         // Provide visual label
   SetName(_("Spectrograms"));          // Provide audible label
   Populate( );
}

void SpectrumPrefs::Populate( )
{
   int minFreq;
   int maxFreq;

   // First any pre-processing for constructing the GUI.
   // Unusual handling of maxFreqStr because it is a validated input.
   gPrefs->Read(wxT("/Spectrum/MaxFreq"), &maxFreq, 8000L);
   gPrefs->Read(wxT("/Spectrum/MinFreq"), &minFreq, 0L);
   gPrefs->Read(wxT("/Spectrum/WindowType"), &windowType, 3L);

   minFreqStr.Printf(wxT("%d"), minFreq);
   maxFreqStr.Printf(wxT("%d"), maxFreq);
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void SpectrumPrefs::PopulateOrExchange( ShuttleGui & S )
{
   wxArrayString windowTypeList;

   for(int i=0; i<NumWindowFuncs(); i++)
      windowTypeList.Add(WindowFuncName(i));

   S.SetBorder( 2 );
   S.StartHorizontalLay(wxEXPAND, 0 );
   S.StartStatic( _("FFT Size"), 0 );
   {
      S.StartRadioButtonGroup( wxT("/Spectrum/FFTSize"), 256 );
      S.TieRadioButton( _("8 - most wideband"),     8);
      S.TieRadioButton( wxT("16"),                  16);
      S.TieRadioButton( wxT("32"),                  32);
      S.TieRadioButton( wxT("64"),                  64);
      S.TieRadioButton( wxT("128"),                 128);
      S.TieRadioButton( _("256 - default"),         256);
      S.TieRadioButton( wxT("512"),                 512);
      S.TieRadioButton( wxT("1024"),                1024);
      S.TieRadioButton( wxT("2048"),                2048);
      S.TieRadioButton( _("4096 - most narrowband"),4096);
      S.EndRadioButtonGroup();

      // add choice for windowtype
      S.StartMultiColumn(2, wxCENTER);
      {
         S.TieChoice( _("Window type:"), windowType,  &windowTypeList);
         S.SetSizeHints(-1,-1);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
   S.StartStatic( _("Display"),1 );
   {
      // JC: For layout of mixtures of controls I prefer checkboxes on the right,
      // with everything in two columns over what we have here.
      S.TieCheckBox( _("&Grayscale"), wxT("/Spectrum/Grayscale"), false);
      S.StartTwoColumn(); // 2 cols because we have a control with a separate label.
      S.Id(ID_MINFREQUENCY).TieTextBox(
         _("Minimum Frequency (Hz):"), // prompt
         minFreqStr, // String to exchange with
         12 // max number of characters (used to size the control).
         );
      S.Id(ID_MAXFREQUENCY).TieTextBox(
         _("Maximum Frequency (Hz):"), // prompt
         maxFreqStr, // String to exchange with
         12 // max number of characters (used to size the control).
         );
      S.EndTwoColumn();
   }
   S.EndStatic();
   S.EndHorizontalLay();
}


bool SpectrumPrefs::Apply()
{
   ShuttleGui S( this, eIsSavingToPrefs );
   PopulateOrExchange( S );

   // max/minFreqStr are input fields that have validation.
   // We've handled them slightly differently to all the
   // other fields, which just go straight through to gPrefs.
   // Instead ShuttleGui has been told to only do a one step
   // exchange with them, not including gPrefs..  so we now
   // need to validate them and possibly write them back to gPrefs.

   //---- Validation of maxFreqStr
   long maxFreq;
   if (!maxFreqStr.ToLong(&maxFreq)) {
      wxMessageBox(_("The maximum frequency must be an integer"));
      return false;
   }
   if (maxFreq < 100 || maxFreq > 100000) {
      wxMessageBox(_("Maximum frequency must be in the range 100 Hz - 100,000 Hz"));
      return false;
   }
   //---- End of validation of maxFreqStr.

   //---- Validation of minFreqStr
   long minFreq;
   if (!minFreqStr.ToLong(&minFreq)) {
      wxMessageBox(_("The minimum frequency must be an integer"));
      return false;
   }
   if (minFreq < 0) {
      wxMessageBox(_("Minimum frequency must be at least 0 Hz"));
      return false;
   }
   //---- End of validation of maxFreqStr.

   gPrefs->Write(wxT("/Spectrum/MinFreq"), minFreq);
   gPrefs->Write(wxT("/Spectrum/MaxFreq"), maxFreq);
   gPrefs->Write(wxT("/Spectrum/WindowType"), windowType);

   // TODO: Force all projects to repaint themselves
   return true;
}

SpectrumPrefs::~SpectrumPrefs()
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
// arch-tag: 54d8e954-f415-40e9-afa4-9d53ab37770d

