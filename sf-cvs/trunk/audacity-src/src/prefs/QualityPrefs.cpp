/**********************************************************************

  Audacity: A Digital Audio Editor

  QualityPrefs.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/defs.h>
#include <wx/choice.h>
#include <wx/stattext.h>
#include <wx/sizer.h>
#include <wx/intl.h>
#include <wx/textctrl.h>

#include "../Audacity.h"
#include "../Prefs.h"
#include "../SampleFormat.h"
#include "QualityPrefs.h"

#if USE_LIBSAMPLERATE
#include <samplerate.h>
#endif

int rates[] = { 8000,
   11025,
   16000,
   22050,
   44100,
   48000,
   44100
};

wxString stringRates[] = { "8000 Hz",
   "11025 Hz",
   "16000 Hz",
   "22050 Hz",
   "44100 Hz",
   "48000 Hz",
   "Other"
};

int formats[] = {
   int16Sample,
   int24Sample,
   floatSample
};

wxString stringFormats[] = {
   "16-bit",
   "24-bit",
   "32-bit float"
};

#define NUM_RATES 6
#define NUM_FORMATS 3

#define ID_SAMPLE_RATE_CHOICE           7001

BEGIN_EVENT_TABLE(QualityPrefs, wxPanel)
   EVT_CHOICE(ID_SAMPLE_RATE_CHOICE,   QualityPrefs::OnSampleRateChoice)
END_EVENT_TABLE()

QualityPrefs::QualityPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   int i;
   int rate =
       gPrefs->Read("/SamplingRate/DefaultProjectSampleRate", 44100);
   int format =
      gPrefs->Read("/SamplingRate/DefaultProjectSampleFormat", floatSample);

   int pos = NUM_RATES;     // Fall back to other
   for (i = 0; i < NUM_RATES; i++)
      if (rate == rates[i]) {
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

   {
      topSizer->Add(
         new wxStaticText(this, -1, _("Default Sample Rate:")), 0, 
         wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

      mSampleRates = new wxChoice(this, ID_SAMPLE_RATE_CHOICE, wxDefaultPosition, wxDefaultSize,
                                 NUM_RATES+1, stringRates);
      mSampleRates->SetSelection(pos);

      topSizer->Add( mSampleRates, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER );

      mOtherSampleRate = NULL;
      mOtherSampleRate = new wxTextCtrl(
         this, -1, wxString::Format("%i", rate),
         wxDefaultPosition, wxSize(50, -1), 0 );

      mOtherSampleRate->Enable(pos == NUM_RATES);

      topSizer->Add( mOtherSampleRate, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER );
   }

    wxBoxSizer *top2Sizer = new wxBoxSizer( wxHORIZONTAL );

   {
      top2Sizer->Add(
         new wxStaticText(this, -1, _("Default Sample Format:")), 0, 
         wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

      mSampleFormats = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                                 NUM_FORMATS, stringFormats);
      mSampleFormats->SetSelection(fmtpos);

      top2Sizer->Add( mSampleFormats, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER );
   }

   #if USE_LIBSAMPLERATE

   int converterHQ = 
      gPrefs->Read("/Quality/HQSampleRateConverter", (long)SRC_SINC_FASTEST);
   int converter = 
      gPrefs->Read("/Quality/SampleRateConverter", (long)SRC_LINEAR);

   wxString *converterStrings;
   int numConverters = 0;
   while(src_get_name(numConverters))
      numConverters++;
   converterStrings = new wxString[numConverters];
   for(i=0; i<numConverters; i++)
      converterStrings[i] = src_get_name(i);

   wxBoxSizer *top3Sizer = new wxBoxSizer( wxHORIZONTAL );

   top3Sizer->Add(
         new wxStaticText(this, -1, _("Real-time sample rate converter:")), 0, 
         wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

   mConverters = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                              numConverters, converterStrings);
   mConverters->SetSelection(converter);
   top3Sizer->Add(mConverters, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER );

   wxBoxSizer *top4Sizer = new wxBoxSizer( wxHORIZONTAL );

   top4Sizer->Add(
         new wxStaticText(this, -1, _("High-quality sample rate converter:")), 0, 
         wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

   mHQConverters = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                                numConverters, converterStrings);
   mHQConverters->SetSelection(converterHQ);
   top4Sizer->Add(mHQConverters, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER );
   
   delete[] converterStrings;
   #endif

   outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);
   outSizer->Add(top2Sizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   #if USE_LIBSAMPLERATE
   outSizer->Add(top3Sizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);
   outSizer->Add(top4Sizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);
   #endif

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
   
   if(sel < NUM_RATES) rate = rates[sel];
   else (mOtherSampleRate->GetValue()).ToLong(&rate);

   gPrefs->Write("/SamplingRate/DefaultProjectSampleRate", rate);

   format = formats[fmtsel];

   gPrefs->Write("/SamplingRate/DefaultProjectSampleFormat", format);

   /* Audacity will automatically re-read this value whenever a new project
    * is created, so don't bother making it do so now... */

   #if USE_LIBSAMPLERATE
   int converter = mConverters->GetSelection();
   int converterHQ = mHQConverters->GetSelection();
   gPrefs->Write("/Quality/HQSampleRateConverter", (long)converterHQ);
   gPrefs->Write("/Quality/SampleRateConverter", (long)converter);
   #endif

   return true;

}

void QualityPrefs::OnSampleRateChoice(wxCommandEvent& evt)
{
   int sel = mSampleRates->GetSelection();

   mOtherSampleRate->Enable(sel == NUM_RATES);
}


QualityPrefs::~QualityPrefs()
{
}


