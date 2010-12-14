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
   ID_MAXFREQUENCY,
#ifdef EXPERIMENTAL_FIND_NOTES
   ID_FIND_NOTES_MIN_A,
   ID_FIND_NOTES_N,
   ID_FIND_NOTES_QUANTIZE,
#endif //EXPERIMENTAL_FIND_NOTES
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
#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
   gPrefs->Read(wxT("/Spectrum/FFTSkipPoints"), &fftSkipPoints, 0L);
#endif //EXPERIMENTAL_FFT_SKIP_POINTS
#ifdef EXPERIMENTAL_FFT_Y_GRID
   gPrefs->Read(wxT("/Spectrum/FFTYGrid"), &fftYGrid, false);
#endif //EXPERIMENTAL_FFT_Y_GRID
#ifdef EXPERIMENTAL_FIND_NOTES
   gPrefs->Read(wxT("/Spectrum/FFTFindNotes"), &fftFindNotes, false);
   gPrefs->Read(wxT("/Spectrum/FindNotesMinA"), &findNotesMinA, -30L);
   findNotesMinAStr.Printf(wxT("%d"), findNotesMinA);
   gPrefs->Read(wxT("/Spectrum/FindNotesN"), &findNotesN, 5L);
   findNotesNStr.Printf(wxT("%d"), findNotesN);
   gPrefs->Read(wxT("/Spectrum/FindNotes"), &findNotesQuantize, false);
#endif //EXPERIMENTAL_FIND_NOTES

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
#ifdef EXPERIMENTAL_FIND_NOTES
      S.TieRadioButton( wxT("4096"),                4096);
      S.TieRadioButton( wxT("8192"),                8192);
      S.TieRadioButton( wxT("16384"),               16384);
      S.TieRadioButton( _("32768 - most narrowband"),32768);
#else
      S.TieRadioButton( _("4096 - most narrowband"),4096);
#endif //LOGARITHMIC_SPECTRUM
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

#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
   S.StartHorizontalLay(wxEXPAND, 0 );
   S.StartStatic( _("FFT Skip Points"), 0 );
   {
      S.StartRadioButtonGroup(wxT("/Spectrum/FFTSkipPoints"), 0);
      S.TieRadioButton(wxT("0"), 0);
      S.TieRadioButton(wxT("1"), 1);
      S.TieRadioButton(wxT("3"), 3);
      S.TieRadioButton(wxT("7"), 7);
      S.TieRadioButton(wxT("15"), 15);
      S.TieRadioButton(wxT("31"), 31);
      S.TieRadioButton(wxT("63"), 63);
      S.EndRadioButtonGroup();
   }
   S.EndStatic();
#endif //EXPERIMENTAL_FFT_SKIP_POINTS

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
#ifdef EXPERIMENTAL_FFT_Y_GRID
      S.TieCheckBox( _("&Y-Grid"), wxT("/Spectrum/FFTYGrid"), false);
#endif //EXPERIMENTAL_FFT_Y_GRID
#ifdef EXPERIMENTAL_FIND_NOTES
      S.TieCheckBox( _("&Find Notes"), wxT("/Spectrum/FFTFindNotes"), false);
      S.TieCheckBox( _("&Quantize Notes"), wxT("/Spectrum/FindNotesQuantize"), false);
      S.StartTwoColumn(); // 2 cols because we have a control with a separate label.
      S.Id(ID_FIND_NOTES_MIN_A).TieTextBox(
         _("Minimum Amplitude (dB):"), // prompt
         findNotesMinAStr, // String to exchange with
         8 // max number of characters (used to size the control).
         );
      S.Id(ID_FIND_NOTES_N).TieTextBox(
         _("Max. Number of Notes (1..128):"), // prompt
         findNotesNStr, // String to exchange with
         8 // max number of characters (used to size the control).
         );
      S.EndTwoColumn();
#endif //EXPERIMENTAL_FIND_NOTES
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
#ifdef EXPERIMENTAL_FIND_NOTES
   long findNotesMinA;
   if (!findNotesMinAStr.ToLong(&findNotesMinA)) {
      wxMessageBox(_("The minimum amplitude (dB) must be an integer"));
      return false;
   }
   gPrefs->Write(wxT("/Spectrum/FindNotesMinA"), findNotesMinA);
   long findNotesN;
   if (!findNotesNStr.ToLong(&findNotesN)) {
      wxMessageBox(_("The Maximum Number of Notes must be an integer in the range 1..128"));
      return false;
   }
   if (findNotesN < 1 || findNotesN > 128) {
      wxMessageBox(_("The Maximum Number of Notes must be an integer in the range 1..128"));
      return false;
   }
   gPrefs->Write(wxT("/Spectrum/FindNotesN"), findNotesN);
#endif //EXPERIMENTAL_FIND_NOTES

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

