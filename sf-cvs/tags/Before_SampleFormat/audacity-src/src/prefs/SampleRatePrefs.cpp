/**********************************************************************

  Audacity: A Digital Audio Editor

  SampleRatePrefs.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/radiobut.h>
#include <wx/statbox.h>
#include <wx/sizer.h>
#include <wx/intl.h>

#include "../Prefs.h"
#include "SampleRatePrefs.h"

int rates[] = { 8000,
   11025,
   16000,
   22050,
   44100,
   48000
};

wxString stringRates[] = { "8000",
   "11025",
   "16000",
   "22050",
   "44100",
   "48000"
};

// Don't forget to change the size of the mSampleRates array in
// SampleRatePrefs.h when you change this
#define NUM_RATES 6

SampleRatePrefs::SampleRatePrefs(wxWindow * parent):
PrefsPanel(parent)
{
   int rate =
       gPrefs->Read("/SamplingRate/DefaultProjectSampleRate", 44100);

   int pos = 4;     // Fall back to 44100 if it doesn't match anything else
   for (int i = 0; i < NUM_RATES; i++)
      if (rate == rates[i]) {
         pos = i;
         break;
      }

    topSizer = new wxStaticBoxSizer(
      new wxStaticBox(this, -1, _("Sample Rate Preferences")), 
      wxVERTICAL );

   {
      wxStaticBoxSizer *defProjSizer = new wxStaticBoxSizer(
         new wxStaticBox(this, -1, _("Default Project Sample Rate")),
         wxVERTICAL );

      mSampleRates[0] = new wxRadioButton(this, -1, stringRates[0],
                                          wxDefaultPosition, wxDefaultSize,
                                          wxRB_GROUP);
      mSampleRates[0]->SetValue(false);

      defProjSizer->Add(
         mSampleRates[0], 0, wxGROW | wxLEFT | wxRIGHT, RADIO_BUTTON_BORDER);

      for(int j = 1; j < NUM_RATES; j++) {
         mSampleRates[j] = new wxRadioButton(this, -1, stringRates[j]);
         mSampleRates[j]->SetValue(false);
         defProjSizer->Add(
            mSampleRates[j], 0, wxGROW| wxLEFT | wxRIGHT, RADIO_BUTTON_BORDER);
      }

      mSampleRates[pos]->SetValue(true);

      topSizer->Add( defProjSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER );
   }

   SetAutoLayout(true);
   topSizer->Fit(this);
   topSizer->SetSizeHints(this);
   SetSizer(topSizer);
}

bool SampleRatePrefs::Apply()
{
   long rate = 44100;

   for(int i = 0; i < NUM_RATES; i++)
      if(mSampleRates[i]->GetValue()) {
         rate = rates[i];
         break;
      }

   gPrefs->Write("/SamplingRate/DefaultProjectSampleRate", rate);

   /* Audacity will automatically re-read this value whenever a new project
    * is created, so don't bother making it do so now... */

   return true;

}


SampleRatePrefs::~SampleRatePrefs()
{
}


