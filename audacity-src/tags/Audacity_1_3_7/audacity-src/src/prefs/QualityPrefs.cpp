/**********************************************************************

  Audacity: A Digital Audio Editor

  QualityPrefs.cpp

  Joshua Haberman
  James Crook


*******************************************************************//**

\class QualityPrefs
\brief A PrefsPanel used for setting audio quality.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/choice.h>
#include <wx/textctrl.h>
#include <wx/intl.h>

#include "../AudioIO.h"
#include "../Dither.h"
#include "../Prefs.h"
#include "../Resample.h"
#include "../SampleFormat.h"
#include "../ShuttleGui.h"
#include "QualityPrefs.h"

#define ID_SAMPLE_RATE_CHOICE           7001

BEGIN_EVENT_TABLE(QualityPrefs, wxPanel)
   EVT_CHOICE(ID_SAMPLE_RATE_CHOICE,   QualityPrefs::OnSampleRateChoice)
END_EVENT_TABLE()

QualityPrefs::QualityPrefs(wxWindow * parent):
   PrefsPanel(parent)
{
   SetLabel(_("Quality"));         // Provide visual label
   SetName(_("Quality"));          // Provide audible label
   mSampleRates = NULL;            // Pointers to controls.
   mOtherSampleRate = NULL;
   Populate( );
}

void QualityPrefs::Populate( )
{
   // First any pre-processing for constructing the GUI.
   GetNamesAndLabels();
   gPrefs->Read( wxT("/SamplingRate/DefaultProjectSampleRate"), &mOtherSampleRateValue, 44100 );
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
   wxCommandEvent e;
   OnSampleRateChoice(e); // Enable/disable the control.
}

/// Gets the lists of names and lists of labels which are
/// used in the choice controls.
/// The names are what the user sees in the wxChoice.
/// The corresponding labels are what gets stored.
void QualityPrefs::GetNamesAndLabels()
{
   //------------ Dither Names 
   mmDitherNames.Add( _("None") );        mmDitherLabels.Add( Dither::none );
   mmDitherNames.Add( _("Rectangle") );   mmDitherLabels.Add( Dither::rectangle );
   mmDitherNames.Add( _("Triangle") );    mmDitherLabels.Add( Dither::triangle );
   mmDitherNames.Add( _("Shaped") );      mmDitherLabels.Add( Dither::shaped );

   //------------ Sample Rate Names
   // JKC: I don't understand the following comment.
   //      Can someone please explain or correct it?
   // XXX: This should use a previously changed, but not yet saved
   //      sound card setting from the "I/O" preferences tab.
   // LLL: It means that until the user clicks "Ok" in preferences, the
   //      GetSupportedSampleRates() call should use the devices they
   //      may have changed on the Audio I/O page.  As coded, the sample
   //      rates it will return could be completely invalid as they will
   //      be what's supported by the devices that were selected BEFORE
   //      coming into preferences.
   //
   //      GetSupportedSampleRates() allows passing in device names, but
   //      how do you get at them as they are on the Audio I/O page????
   for (int i=0; i<AudioIO::NumStandardRates; i++)
   {
      int iRate = (int)AudioIO::StandardRates[i];
      mmSampleRateLabels.Add( iRate );
      mmSampleRateNames.Add( wxString::Format(wxT("%i Hz"), iRate ));
   }
   mmSampleRateNames.Add( _("Other..."));
   // The label for the 'Other...' case can be any value at all.
   mmSampleRateLabels.Add( 44100 ); // If chosen, this value will be overwritten

   //------------- Sample Format Names
   mmSampleFormatNames.Add( wxT("16-bit")       );mmSampleFormatLabels.Add(int16Sample);
   mmSampleFormatNames.Add( wxT("24-bit")       );mmSampleFormatLabels.Add(int24Sample);
   mmSampleFormatNames.Add( wxT("32-bit float") );mmSampleFormatLabels.Add(floatSample);

   //------------- Converter Names
   // We used to set and get best/fast method via Resample.cpp.
   // Need to ensure that preferences strings in Resample.cpp match.
   // int converterHQ = Resample::GetBestMethod();
   // int converter = Resample::GetFastMethod();
   int numConverters = Resample::GetNumMethods();
   for(int i=0; i<numConverters; i++)
   {
      mConverterNames.Add(  Resample::GetMethodName(i) );
      mConverterLabels.Add( i );
   }
}

void QualityPrefs::PopulateOrExchange( ShuttleGui & S )
{
   S.SetBorder( 2 );
   S.StartStatic(_("Sampling"),0 );
   {
      S.StartTwoColumn();
      S.AddPrompt( _("Default Sample Rate:") );
      DefineSampleRateControl( S ); // A tricky control.
      S.TieChoice(_("Default Sample Format:"),
         wxT("/SamplingRate/DefaultProjectSampleFormat"),
         floatSample, mmSampleFormatNames, mmSampleFormatLabels );

// JKC For the old style of layout, uncomment the next line.
// #define OLD_STYLE_LAYOUT
// Once we've chosen which layout to use we'll remove the 
// other one.  (June/2006).
#ifdef OLD_STYLE_LAYOUT
      S.TieChoice(_("Real-time sample rate converter:"),
         wxT("/Quality/LibresampleSampleRateConverter"),
         (int)0, mConverterNames, mConverterLabels),
      S.TieChoice(_("High-quality sample rate converter:"),
         wxT("/Quality/LibresampleHQSampleRateConverter"),
         (int)1, mConverterNames, mConverterLabels),
      S.TieChoice(_("Real-time dither:"),
         wxT("/Quality/DitherAlgorithm"),
         Dither::none, mmDitherNames, mmDitherLabels );  
      S.TieChoice(_("High-quality dither:"),
         wxT("/Quality/HQDitherAlgorithm"),
         Dither::shaped, mmDitherNames, mmDitherLabels );  
#endif
      S.EndTwoColumn();
   }
   S.EndStatic();

// The new style of layout has 
//   - columns for converter and dither.
//   - rows for Real-time and High-quality.
#ifndef OLD_STYLE_LAYOUT
   S.StartStatic( _("Conversion") );
   {
      // We use blank labels here and there to end up with
      // a three column layout.
      S.StartMultiColumn(3);
      S.AddFixedText(wxT(""));
      S.AddTitle(_("Sample Rate Converter" ));
      S.AddTitle(_("Dither"));
      S.TieChoice(_("Real-time:"),
         wxT("/Quality/LibresampleSampleRateConverter"),
         (int)0, mConverterNames, mConverterLabels),
      S.TieChoice(wxT(""),
         wxT("/Quality/DitherAlgorithm"),
         Dither::none, mmDitherNames, mmDitherLabels );  
      S.TieChoice(_("High-quality:"),
         wxT("/Quality/LibresampleHQSampleRateConverter"),
         (int)1, mConverterNames, mConverterLabels),
      S.TieChoice(wxT(""),
         wxT("/Quality/HQDitherAlgorithm"),
         Dither::shaped, mmDitherNames, mmDitherLabels );  
      S.EndMultiColumn();
   }
   S.EndStatic();
#endif
}

/// Add a compound control made up from a choice and an edit 
/// box.  
void QualityPrefs::DefineSampleRateControl( ShuttleGui & S )
{
   // We use a sizer within a sizer to get the effect we want.
   // We also use the SetIfCreated idiom to get pointers to
   // the controls, so that we can drive them from
   // our own code.

   S.SetBorder(2);
   S.StartHorizontalLay(wxALIGN_LEFT );
   // If the value in Prefs isn't in the list, then we want
   // the last item, 'Other...' to be shown.
   S.SetNoMatchSelector( mmSampleRateNames.GetCount()-1 );
   // First the choice...
   // We make sure it uses the ID we want, so that we get changes
   S.Id( ID_SAMPLE_RATE_CHOICE );
   // We make sure we have a pointer to it, so that we can drive it.
   mSampleRates = S.TieChoice( wxT(""),
      wxT("/SamplingRate/DefaultProjectSampleRate"),
      AudioIO::GetOptimalSupportedSampleRate(),
      mmSampleRateNames, mmSampleRateLabels );
   // Now do the edit box...
   mOtherSampleRate = S.TieTextBox(wxT(""),
      mOtherSampleRateValue, 9);
   S.EndHorizontalLay();
}

bool QualityPrefs::Apply()
{
   ShuttleGui S( this, eIsSavingToPrefs );
   PopulateOrExchange( S );

   // The complex compound control may have value 'other' in which case the
   // value in prefs comes from the second field.
   if( mOtherSampleRate->IsEnabled() )
   {
      gPrefs->Write( wxT("/SamplingRate/DefaultProjectSampleRate"), mOtherSampleRateValue );
   }
   // Tell CopySamples() to use these ditherers now
   InitDitherers();
   return true;
}

/// Enables or disables the Edit box depending on
/// whether we selected 'Other...' or not.
void QualityPrefs::OnSampleRateChoice(wxCommandEvent& evt)
{
   int sel = mSampleRates->GetSelection();
   mOtherSampleRate->Enable(sel == (int)mSampleRates->GetCount() - 1);
}


QualityPrefs::~QualityPrefs()
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
// arch-tag: 135e3a62-5d8a-472d-ab66-462a5157e6b8

