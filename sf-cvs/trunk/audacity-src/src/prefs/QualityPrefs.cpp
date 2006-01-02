/**********************************************************************

  Audacity: A Digital Audio Editor

  QualityPrefs.cpp

  Joshua Haberman

**********************************************************************/

#include "../Audacity.h"

#include "QualityPrefs.h"

#include "../AudioIO.h"
#include "../Dither.h"
#include "../Prefs.h"
#include "../Resample.h"
#include "../SampleFormat.h"

#include <wx/defs.h>
#include <wx/choice.h>
#include <wx/stattext.h>
#include <wx/sizer.h>
#include <wx/intl.h>
#include <wx/textctrl.h>
#include <wx/statbox.h>

int formats[] = {
   int16Sample,
   int24Sample,
   floatSample
};

wxString stringFormats[] = {
   wxT("16-bit"),
   wxT("24-bit"),
   wxT("32-bit float")
};

#define NUM_FORMATS 3

#define ID_SAMPLE_RATE_CHOICE           7001

BEGIN_EVENT_TABLE(QualityPrefs, wxPanel)
   EVT_CHOICE(ID_SAMPLE_RATE_CHOICE,   QualityPrefs::OnSampleRateChoice)
END_EVENT_TABLE()


QualityPrefs::QualityPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   int i;

   SetLabel(_("Quality"));         // Provide visual label
   SetName(_("Quality"));          // Provide audible label

   // XXX: This should use a previously changed, but not yet saved
   //      sound card setting from the "I/O" preferences tab.
   wxArrayLong sampleRates = AudioIO::GetSupportedSampleRates();
   
   int rate =
       gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleRate"),
                    AudioIO::GetOptimalSupportedSampleRate());

   int format =
      gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample);

   int pos = sampleRates.GetCount(); // Fall back to "Other..."
   for (i = 0; i < (int)sampleRates.GetCount(); i++)
      if (rate == sampleRates[i]) {
         pos = i;
         break;
      }

   int fmtpos = NUM_FORMATS-1;
   for (i = 0; i < NUM_FORMATS; i++)
      if (format == formats[i]) {
         fmtpos = i;
         break;
      }

    topSizer = new wxBoxSizer( wxHORIZONTAL );
    // horizontal box sizer for two controls in the first row
    wxBoxSizer *firstRowSizer = new wxBoxSizer(wxHORIZONTAL);
    // flexgrid sizer and make second column stretchable only
    wxFlexGridSizer *gridSizer = new wxFlexGridSizer(6, 2, 5, 5);
    gridSizer->AddGrowableCol(1);
    // static box sizer
    wxStaticBoxSizer *staticSizer = new wxStaticBoxSizer( new wxStaticBox(this, -1, _("Sampling") ), wxHORIZONTAL );
    // adding
    staticSizer->Add(gridSizer, 1, wxEXPAND | wxALL, 1);
    topSizer->Add( staticSizer, 1, wxEXPAND | wxALL, 1 );
    
   {
      gridSizer->Add(
         new wxStaticText(this, -1, _("Default Sample Rate:"), wxPoint(-1,-1), wxDefaultSize, wxALIGN_RIGHT), 0, 
         wxALIGN_RIGHT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);
         
      wxString *stringRates = new wxString[sampleRates.GetCount() + 1];
      
      for (i = 0; i < (int)sampleRates.GetCount(); i++)
      {
         int sampleRate = (int)sampleRates[i];
         stringRates[i] = wxString::Format(wxT("%i Hz"), sampleRate);
      }
      
      stringRates[sampleRates.GetCount()] = _("Other...");

      mSampleRates = new wxChoice(this, ID_SAMPLE_RATE_CHOICE, wxDefaultPosition, wxDefaultSize,
                                 (int)sampleRates.GetCount() + 1, stringRates);
      mSampleRates->SetSelection(pos);

      firstRowSizer->Add( mSampleRates, 0, wxRIGHT|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER);

      mOtherSampleRate = NULL;
      mOtherSampleRate = new wxTextCtrl(
         this, -1, wxString::Format(wxT("%i"), rate),
         wxDefaultPosition, wxSize(50, -1), 0 );

      mOtherSampleRate->Enable(pos == (int)sampleRates.GetCount() + 1);

      firstRowSizer->Add( mOtherSampleRate, 0, wxLEFT|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER );
      gridSizer->Add(firstRowSizer, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER );
      delete[] stringRates;
   }

   {
      gridSizer->Add(
         new wxStaticText(this, -1, _("Default Sample Format:"), wxPoint(-1,-1), wxDefaultSize, wxALIGN_RIGHT), 0, 
         wxALIGN_RIGHT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

      mSampleFormats = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                                 NUM_FORMATS, stringFormats);
      mSampleFormats->SetSelection(fmtpos);

      gridSizer->Add( mSampleFormats, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER );
   }

   int converterHQ = Resample::GetBestMethod();
   int converter = Resample::GetFastMethod();
   int numConverters = Resample::GetNumMethods();

   wxString *converterStrings;
   converterStrings = new wxString[numConverters];
   for(i=0; i<numConverters; i++)
      converterStrings[i] = Resample::GetMethodName(i);

   gridSizer->Add(
         new wxStaticText(this, -1, _("Real-time sample rate converter:"), wxPoint(-1,-1), wxDefaultSize, wxALIGN_RIGHT), 0, 
         wxALIGN_RIGHT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

   mConverters = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                              numConverters, converterStrings);
   mConverters->SetSelection(converter);
   gridSizer->Add(mConverters, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER );

   gridSizer->Add(
         new wxStaticText(this, -1, _("High-quality sample rate converter:"), wxPoint(-1,-1), wxDefaultSize, wxALIGN_RIGHT), 0, 
         wxALIGN_RIGHT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

   mHQConverters = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                                numConverters, converterStrings);
   mHQConverters->SetSelection(converterHQ);
   gridSizer->Add(mHQConverters, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER );
   
   delete[] converterStrings;

   // These ditherers are currently defined
   int numDithers = 4;
   wxString ditherStrings[4];
   ditherStrings[Dither::none] = _("None");
   ditherStrings[Dither::rectangle] = _("Rectangle");
   ditherStrings[Dither::triangle] = _("Triangle");
   ditherStrings[Dither::shaped] = _("Shaped");
   
   // Low-quality dithering option
   int dither = gPrefs->Read(wxT("/Quality/DitherAlgorithm"), (long)Dither::none);
   
   gridSizer->Add(
         new wxStaticText(this, -1, _("Real-time dither:"), wxPoint(-1,-1), wxDefaultSize, wxALIGN_RIGHT), 0,
         wxALIGN_RIGHT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);
   
   mDithers = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                           numDithers, ditherStrings);
   mDithers->SetSelection(dither);
   
   gridSizer->Add(mDithers, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER);
   
   // High quality dithering option
   int ditherHQ = gPrefs->Read(wxT("/Quality/HQDitherAlgorithm"), (long)Dither::triangle);;
   
   gridSizer->Add(
         new wxStaticText(this, -1, _("High-quality dither:"), wxPoint(-1,-1), wxDefaultSize, wxALIGN_RIGHT), 0,
         wxALIGN_RIGHT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);
   
   mHQDithers = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                             numDithers, ditherStrings);
   mHQDithers->SetSelection(ditherHQ);
   
   gridSizer->Add(mHQDithers, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER);
   
   outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);
   
   SetAutoLayout(true);
   outSizer->Fit(this);
   outSizer->SetSizeHints(this);
   SetSizer(outSizer);

}

bool QualityPrefs::Apply()
{
   long rate = 44100;
   long format = floatSample;
   int sel = mSampleRates->GetSelection();
   int fmtsel = mSampleFormats->GetSelection();
   
   if (sel < mSampleRates->GetCount()-1)
      (mSampleRates->GetString(sel)).ToLong(&rate);
   else
      (mOtherSampleRate->GetValue()).ToLong(&rate);

   gPrefs->Write(wxT("/SamplingRate/DefaultProjectSampleRate"), rate);

   format = formats[fmtsel];

   gPrefs->Write(wxT("/SamplingRate/DefaultProjectSampleFormat"), format);

   /* Audacity will automatically re-read this value whenever a new project
    * is created, so don't bother making it do so now... */

   int converter = mConverters->GetSelection();
   int converterHQ = mHQConverters->GetSelection();
   Resample::SetFastMethod(converter);
   Resample::SetBestMethod(converterHQ);

   // Save dither options
   int dither = mDithers->GetSelection();
   int ditherHQ = mHQDithers->GetSelection();
   gPrefs->Write(wxT("/Quality/HQDitherAlgorithm"), (long)ditherHQ);
   gPrefs->Write(wxT("/Quality/DitherAlgorithm"), (long)dither);
   
   // Tell CopySamples() to use these ditherers now
   InitDitherers();

   return true;
}

void QualityPrefs::OnSampleRateChoice(wxCommandEvent& evt)
{
   int sel = mSampleRates->GetSelection();

   mOtherSampleRate->Enable(sel == mSampleRates->GetCount() - 1);
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

