/**********************************************************************

  Audacity: A Digital Audio Editor

  SampleRatePrefs.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/window.h>
#include <wx/statbox.h>

#include "../Prefs.h"
#include "SampleRatePrefs.h"

int rates[]            = { 8000, 
						   11025, 
						   22050, 
						   44100, 
						   48000 };

wxString stringRates[] = { "8000", 
						   "11025",
						   "22050", 
						   "44100",
						   "48000" };

SampleRatePrefs::SampleRatePrefs(wxWindow *parent):
	PrefsPanel(parent)
{
	int rate = gPrefs->Read("/SamplingRate/DefaultProjectSampleRate", 44100);
	
	int pos = 3;   // Fall back to 44100 if it doesn't match anything else
	for(int i = 0; i < 5; i++)
		if(rate == rates[i]) {
			pos = i;
			break;
		}

	mEnclosingBox        = new wxStaticBox(this, -1, 
										  "Sample Rate Settings",
										  wxPoint(0, 0),
										  GetSize());
	
	mDefaultSamplingRate = new wxRadioBox(this, -1,
										 "Default Project Sample Rate",
										  wxPoint(PREFS_SIDE_MARGINS,
										 		 PREFS_TOP_MARGIN),
										  wxSize(GetSize().GetWidth() -
								   		   PREFS_SIDE_MARGINS * 2,
												 GetSize().GetHeight() -
												 (2 * PREFS_TOP_MARGIN)),
										  5, // number of items
										  stringRates,
										  1);
	
	mDefaultSamplingRate->SetSelection(pos);
}

bool SampleRatePrefs::Apply()
{
	long rate = rates[mDefaultSamplingRate->GetSelection()];

	gPrefs->Write("/SamplingRate/DefaultProjectSampleRate", rate);

	/* Audacity will automatically re-read this value whenever a new project
	 * is created, so don't bother making it do so now... */

	 return true;
	
}


SampleRatePrefs::~SampleRatePrefs()
{
	delete mEnclosingBox;
	delete mDefaultSamplingRate;
}
