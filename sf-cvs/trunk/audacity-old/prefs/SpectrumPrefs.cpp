/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumPrefs.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/window.h>
#include <wx/statbox.h>
#include <wx/colordlg.h>

#include "../Prefs.h"
#include "SpectrumPrefs.h"

int numFFTSizes = 7;

int FFTSizes[]            = { 64, 
							  128,
							  256,
							  512,
							  1024,
							  2048,
							  4096};

wxString stringFFTSizes[] = { "64 - most narrowband",
							  "128",
							  "256 - default",
							  "512",
							  "1024",
							  "2048",
							  "4096 - most wideband"};

SpectrumPrefs::SpectrumPrefs(wxWindow *parent):
	PrefsPanel(parent)
{
	int fftSize = gPrefs->Read("/Spectrum/FFTSize", 256);
	bool isGrayscale = false;
  gPrefs->Read("/Spectrum/Grayscale", &isGrayscale, false);
	
	int pos = 3;   // Fall back to 256 if it doesn't match anything else
	for(int i = 0; i < numFFTSizes; i++)
		if(fftSize == FFTSizes[i]) {
			pos = i;
			break;
		}

	mEnclosingBox        = new wxStaticBox(this, -1, 
										  "Spectrogram Options",
										  wxPoint(0, 0),
										  GetSize());
	
	mFFTSize = new wxRadioBox(this, -1,
							  "FFT Size",
							  wxPoint(PREFS_SIDE_MARGINS,
									  PREFS_TOP_MARGIN),
							  wxSize(GetSize().GetWidth() -
									 PREFS_SIDE_MARGINS * 2,
									 180),
							  numFFTSizes, // number of items
							  stringFFTSizes,
							  1);
	
	mFFTSize->SetSelection(pos);

	mGrayscale = new wxCheckBox(this, -1,
								"Grayscale",
								wxPoint(PREFS_SIDE_MARGINS,
										PREFS_TOP_MARGIN + 190),
								wxSize(GetSize().GetWidth() -
									   PREFS_SIDE_MARGINS * 2,
									   15));
	if (isGrayscale)
	  mGrayscale->SetValue(true);
}

bool SpectrumPrefs::Apply()
{
  /*
  wxColourDialog dlog(this);
  dlog.ShowModal();
  */

	long fftSize = FFTSizes[mFFTSize->GetSelection()];
	gPrefs->Write("/Spectrum/FFTSize", fftSize);

	bool isGrayscale = mGrayscale->GetValue();
	gPrefs->Write("/Spectrum/Grayscale", isGrayscale);

	// TODO: Force all projects to repaint themselves

	return true;
	
}


SpectrumPrefs::~SpectrumPrefs()
{
	delete mEnclosingBox;
	delete mFFTSize;
	delete mGrayscale;
}
