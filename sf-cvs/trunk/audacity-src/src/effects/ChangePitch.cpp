/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangePitch.cpp

  Vaughan Johnson, Dominic Mazzoni
  
  Change Pitch effect provides raising or lowering 
  the pitch without changing the tempo.

**********************************************************************/

#include "../Audacity.h"

#if USE_SOUNDTOUCH

#include <math.h>

#include <wx/intl.h>
#include <wx/valtext.h>

#include <SoundTouch.h>

#include "ChangePitch.h"

#include "../PitchName.h"
#include "../Spectrum.h"
#include "../WaveTrack.h"

//
// EffectChangePitch
//

EffectChangePitch::EffectChangePitch()
{
	m_FromPitchIndex = -1;		// -1 => uninitialized
	m_bWantPitchDown = false;
	m_ToPitchIndex = -1;			// -1 => uninitialized

	m_SemitonesChange = 0.0;

   m_FromFrequency = 0.0;		// 0.0 => uninitialized
   m_ToFrequency = 0.0;			// 0.0 => uninitialized

	m_PercentChange = 0.0;
}

wxString EffectChangePitch::GetEffectDescription() { 
   // Note: This is useful only after change amount has been set. 
   return wxString::Format(_("Applied effect: %s %.2f semitones"), 
                           (const char *)(this->GetEffectName()), 
									m_SemitonesChange); 
} 

bool EffectChangePitch::Init()
{
   mSoundTouch = NULL;
	return true;
}

// DeduceFrequency is Dominic's extremely cool trick (Vaughan sez so!) 
// to set deduce m_FromFrequency from the samples at the beginning of 
// the selection. Then we set some other params accordingly.
void EffectChangePitch::DeduceFrequencies()
{
   // As a neat trick, attempt to get the frequency of the note at the
   // beginning of the selection.
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   if (track) {
      const int windowSize = 1024;
      const int analyzeSize = 8192;
      const int numWindows = analyzeSize / windowSize;
      double trackStart = track->GetStartTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      longSampleCount start = track->TimeToLongSamples(t0);
      double rate = track->GetRate();
      float buffer[analyzeSize];
      float freq[windowSize/2];
      float freqa[windowSize/2];
      int i, j, argmax;
      int lag;

      for(j=0; j<windowSize/2; j++)
         freqa[j] = 0;

      track->Get((samplePtr) buffer, floatSample, start, analyzeSize);
      for(i=0; i<numWindows; i++) {
         ComputeSpectrum(buffer+i*windowSize, windowSize, windowSize/2,
                         (int)rate, windowSize, rate, freq, true);
         for(j=0; j<windowSize/2; j++)
            freqa[j] += freq[j];
      }
      argmax=0;
      for(j=1; j<windowSize/2; j++)
         if (freqa[j] > freqa[argmax])
            argmax = j;
      lag = (windowSize/2 - 1) - argmax;
      m_FromFrequency = rate / lag;
		m_ToFrequency = (m_FromFrequency * (100.0 + m_PercentChange)) / 100.0;

		// Now we can set the pitch control values. 
		m_FromPitchIndex = PitchIndex(Freq2Pitch(m_FromFrequency));
		m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);
		m_ToPitchIndex = PitchIndex(Freq2Pitch(m_ToFrequency));
   }
}

bool EffectChangePitch::PromptUser()
{
	this->DeduceFrequencies(); // Set frequency-related control values based on sample.

   ChangePitchDialog dlog(mParent, -1, _("Change Pitch"));
   dlog.m_FromPitchIndex = m_FromPitchIndex;
   dlog.m_bWantPitchDown = m_bWantPitchDown;
   dlog.m_ToPitchIndex = m_ToPitchIndex;
	dlog.m_SemitonesChange = m_SemitonesChange;
   dlog.m_FromFrequency = m_FromFrequency;
   dlog.m_ToFrequency = m_ToFrequency;
   dlog.m_PercentChange = m_PercentChange;
	//v Don't need to call TransferDataToWindow, although other Audacity dialogs 
	//v (from which I derived this one) do it, because ShowModal calls stuff that 
	//v eventually calls wxWindowBase::OnInitDialog, which calls TransferDataToWindow.
   //v		dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   m_FromPitchIndex = dlog.m_FromPitchIndex;
   m_bWantPitchDown = dlog.m_bWantPitchDown;
   m_ToPitchIndex = dlog.m_ToPitchIndex;
	m_SemitonesChange = dlog.m_SemitonesChange;
   m_FromFrequency = dlog.m_FromFrequency;
   m_ToFrequency = dlog.m_ToFrequency;
   m_PercentChange = dlog.m_PercentChange;

   mSoundTouch = new SoundTouch();
   mSoundTouch->setChannels(1);
   mSoundTouch->setPitchSemiTones((float)(m_SemitonesChange));

   return true;
}

//----------------------------------------------------------------------------
// ChangePitchDialog
//----------------------------------------------------------------------------

#define PERCENTCHANGE_MIN -99
#define PERCENTCHANGE_MAX 100 // warped above zero to actually go up to 400%
#define PERCENTCHANGE_SLIDER_WARP 1.30105 // warp power takes max from 100 to 400.

// event table for ChangePitchDialog

BEGIN_EVENT_TABLE(ChangePitchDialog, wxDialog)
    EVT_BUTTON(wxID_OK, ChangePitchDialog::OnOk)
    EVT_BUTTON(wxID_CANCEL, ChangePitchDialog::OnCancel)

    EVT_CHOICE(ID_CHOICE_FROMPITCH, ChangePitchDialog::OnChoice_FromPitch)
	 EVT_RADIOBOX(ID_RADIOBOX_PITCHUPDOWN, ChangePitchDialog::OnRadioBox_PitchUpDown)
    EVT_CHOICE(ID_CHOICE_TOPITCH, ChangePitchDialog::OnChoice_ToPitch)

    EVT_TEXT(ID_TEXT_SEMITONESCHANGE, ChangePitchDialog::OnText_SemitonesChange)

    EVT_TEXT(ID_TEXT_FROMFREQUENCY, ChangePitchDialog::OnText_FromFrequency)
    EVT_TEXT(ID_TEXT_TOFREQUENCY, ChangePitchDialog::OnText_ToFrequency)

    EVT_TEXT(ID_TEXT_PERCENTCHANGE, ChangePitchDialog::OnText_PercentChange)
    EVT_SLIDER(ID_SLIDER_PERCENTCHANGE, ChangePitchDialog::OnSlider_PercentChange)
END_EVENT_TABLE()


ChangePitchDialog::ChangePitchDialog(wxWindow * parent, 
												 wxWindowID id, const wxString & title, 
												 const wxPoint & position, const wxSize & size, 
												 long style)
: wxDialog(parent, id, title, position, size, style)
{
   m_bLoopDetect = false;

	m_FromPitchIndex = -1;		// -1 => uninitialized
	m_bWantPitchDown = false;
	m_ToPitchIndex = -1;			// -1 => uninitialized

	m_SemitonesChange = 0.0;

   m_FromFrequency = 0.0;		// 0.0 => uninitialized
   m_ToFrequency = 0.0;			// 0.0 => uninitialized

	m_PercentChange = 0.0;

	
	// CREATE THE CONTROLS PROGRAMMATICALLY.
	wxStaticText * pStaticText;

   wxBoxSizer * pBoxSizer_Dialog = new wxBoxSizer(wxVERTICAL);

	// heading
   pStaticText =
		new wxStaticText(this, ID_TEXT, _("Change Pitch without Changing Tempo"),
								wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Dialog->Add(pStaticText, 0, wxALIGN_CENTER | wxALL, 8);

   pStaticText =
		new wxStaticText(this, ID_TEXT, _("by Vaughan Johnson && Dominic Mazzoni"),
								wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Dialog->Add(pStaticText, 0, wxALIGN_CENTER | wxTOP | wxLEFT | wxRIGHT, 8);

   pStaticText =
		new wxStaticText(this, ID_TEXT, _("using SoundTouch, by Olli Parviainen"),
								wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Dialog->Add(pStaticText, 0, wxALIGN_CENTER | wxBOTTOM | wxLEFT | wxRIGHT, 8);

   pBoxSizer_Dialog->Add(0, 8, 0); // spacer


	// from/to pitch controls
   wxBoxSizer * pBoxSizer_Pitch = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText =
       new wxStaticText(this, ID_TEXT, _("Pitch:   from"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Pitch->Add(pStaticText, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

	const wxString strArray_PitchNames[] = 
		{"C", "C#/Db", "D", "D#/Eb", "E", "F", 
       "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B"};
	const int numChoicesPitchNames = 12;

   wxChoice * pChoice_FromPitch =
		new wxChoice(this, ID_CHOICE_FROMPITCH, wxDefaultPosition, wxSize(64, -1), 
							numChoicesPitchNames, strArray_PitchNames);
   pBoxSizer_Pitch->Add(pChoice_FromPitch, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

	const wxString strArray_RadioPitchUpDown[] = {_("up"), _("down")};
	wxRadioBox * pRadioBox_PitchUpDown = 
		new wxRadioBox(this, ID_RADIOBOX_PITCHUPDOWN, "", 
							wxDefaultPosition, wxDefaultSize, 
							2, strArray_RadioPitchUpDown, 1);
   pBoxSizer_Pitch->Add(pRadioBox_PitchUpDown, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);
   
	pStaticText =
       new wxStaticText(this, ID_TEXT, _("to"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Pitch->Add(pStaticText, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   wxChoice * pChoice_ToPitch =
		new wxChoice(this, ID_CHOICE_TOPITCH, wxDefaultPosition, wxSize(64, -1), 
							numChoicesPitchNames, strArray_PitchNames);
   pBoxSizer_Pitch->Add(pChoice_ToPitch, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_Pitch, 0, wxALIGN_CENTER | wxALL, 4);


	// semitones change controls
   wxBoxSizer * pBoxSizer_SemitonesChange = new wxBoxSizer(wxHORIZONTAL);
   pStaticText =
       new wxStaticText(this, ID_TEXT, _("Semitones (half-steps):"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_SemitonesChange->Add(pStaticText, 0, 
												wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   wxTextCtrl * pTextCtrl_SemitonesChange =
       new wxTextCtrl(this, ID_TEXT_SEMITONESCHANGE, "0.0", 
								wxDefaultPosition, wxSize(40, -1), 0,
								wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_SemitonesChange->Add(pTextCtrl_SemitonesChange, 0, 
												wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_SemitonesChange, 0, wxALIGN_CENTER | wxALL, 4);

	
	// from/to frequency controls
   pBoxSizer_Dialog->Add(0, 8, 0); // spacer
   wxBoxSizer * pBoxSizer_Frequency = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText =
       new wxStaticText(this, ID_TEXT, _("Frequency (Hz):   from"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Frequency->Add(pStaticText, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   wxTextCtrl * pTextCtrl_FromFrequency =
       new wxTextCtrl(this, ID_TEXT_FROMFREQUENCY, "", 
								wxDefaultPosition, wxSize(64, -1), 0,
								wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_Frequency->Add(pTextCtrl_FromFrequency, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pStaticText =
       new wxStaticText(this, ID_TEXT, _("to"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Frequency->Add(pStaticText, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   wxTextCtrl * pTextCtrl_ToFrequency =
       new wxTextCtrl(this, ID_TEXT_TOFREQUENCY, "", 
								wxDefaultPosition, wxSize(64, -1), 0,
								wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_Frequency->Add(pTextCtrl_ToFrequency, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_Frequency, 0, wxALIGN_CENTER | wxALL, 4);


	// percent change control
	
	// Group percent controls with spacers, 
	// rather than static box, so they don't look isolated.
   pBoxSizer_Dialog->Add(0, 8, 0); // spacer

   wxBoxSizer * pBoxSizer_PercentChange = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText =
		new wxStaticText(this, ID_TEXT, _("Percent Change:"),
								wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_PercentChange->Add(pStaticText, 0, 
											wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

	//v Override wxTextValidator to disallow negative values <= -100.0?
   wxTextCtrl * pTextCtrl_PercentChange =
       new wxTextCtrl(this, ID_TEXT_PERCENTCHANGE, "0.0", 
								wxDefaultPosition, wxSize(40, -1), 0,
								wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_PercentChange->Add(pTextCtrl_PercentChange, 0, 
											wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_PercentChange, 0, wxALIGN_CENTER | wxALL, 4);

   wxSlider * pSlider_PercentChange =
       new wxSlider(this, ID_SLIDER_PERCENTCHANGE, 
							0, PERCENTCHANGE_MIN, PERCENTCHANGE_MAX,
							wxDefaultPosition, wxSize(100, -1), wxSL_HORIZONTAL);
   pBoxSizer_Dialog->Add(pSlider_PercentChange, 1, 
									wxGROW | wxALIGN_CENTER | wxLEFT | wxRIGHT, 4);

   pBoxSizer_Dialog->Add(0, 8, 0); // spacer


	// OK & Cancel buttons
   pBoxSizer_Dialog->Add(0, 8, 0); // spacer

   wxBoxSizer * pBoxSizer_OK = new wxBoxSizer(wxHORIZONTAL);

   wxButton * pButton_OK =
       new wxButton(this, wxID_OK, _("OK"), wxDefaultPosition,
                    wxDefaultSize, 0);
   pButton_OK->SetDefault();
   pButton_OK->SetFocus();
   pBoxSizer_OK->Add(pButton_OK, 0, wxALIGN_CENTER | wxALL, 4);

   wxButton * pButton_Cancel =
       new wxButton(this, wxID_CANCEL, _("Cancel"), wxDefaultPosition,
                    wxDefaultSize, 0);
   pBoxSizer_OK->Add(pButton_Cancel, 0, wxALIGN_CENTER | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_OK, 0, wxALIGN_CENTER | wxALL, 8);


   this->SetAutoLayout(true);
   this->SetSizer(pBoxSizer_Dialog);
   pBoxSizer_Dialog->Fit(this);
   pBoxSizer_Dialog->SetSizeHints(this);
}

bool ChangePitchDialog::Validate()
{
   return true; 
}

bool ChangePitchDialog::TransferDataToWindow()
{
   m_bLoopDetect = true;

	// from/to pitch controls
	wxChoice * pChoice = this->GetChoice_FromPitch();
	if (pChoice) 
		pChoice->SetSelection(m_FromPitchIndex);

	this->Update_RadioBox_PitchUpDown();
	this->Update_Choice_ToPitch();


	// semitones change control
	this->Update_Text_SemitonesChange();


	// from/to frequency controls
	wxTextCtrl * pTextCtrl = this->GetTextCtrl_FromFrequency();
	if (pTextCtrl) {
		wxString str;
		if (m_FromFrequency > 0.0)
			str.Printf("%.3f", m_FromFrequency);
		else
			str = "";
		pTextCtrl->SetValue(str);
	}

	this->Update_Text_ToFrequency();

	
	// percent change controls
	this->Update_Text_PercentChange();
	this->Update_Slider_PercentChange();


   m_bLoopDetect = false;

	return true;
}

bool ChangePitchDialog::TransferDataFromWindow()
{
   double newDouble;
	wxChoice * pChoice;
	wxTextCtrl * pTextCtrl;
	wxString str;


	// from/to pitch controls
   pChoice = this->GetChoice_FromPitch();
	if (pChoice) 
		m_FromPitchIndex = pChoice->GetSelection(); 

	wxRadioBox * pRadioBox = this->GetRadioBox_PitchUpDown();
	if (pRadioBox)
		m_bWantPitchDown = (pRadioBox->GetSelection() == 1);

   pChoice = this->GetChoice_ToPitch();
	if (pChoice) 
		m_ToPitchIndex = pChoice->GetSelection();


	// semitones change control
	pTextCtrl = this->GetTextCtrl_SemitonesChange();
   if (pTextCtrl) {
      str = pTextCtrl->GetValue();
      str.ToDouble(&newDouble);
		m_SemitonesChange = newDouble;
	}


	// from/to frequency controls
   pTextCtrl = this->GetTextCtrl_FromFrequency();
   if (pTextCtrl) {
      str = pTextCtrl->GetValue();
      str.ToDouble(&newDouble);
		m_FromFrequency = newDouble;
	}

   pTextCtrl = this->GetTextCtrl_ToFrequency();
   if (pTextCtrl) {
      str = pTextCtrl->GetValue();
      str.ToDouble(&newDouble);
		m_ToFrequency = newDouble;
	}


	// percent change controls
   pTextCtrl = this->GetTextCtrl_PercentChange();
   if (pTextCtrl) {
      str = pTextCtrl->GetValue();
      str.ToDouble(&newDouble);
		m_PercentChange = newDouble;
	}

	// Ignore Slider_PercentChange because TextCtrl_PercentChange 
	// always tracks it & is more precise (decimal points).


   return true;
}


// calculations

void ChangePitchDialog::Calc_ToFrequency()
{
	m_ToFrequency = (m_FromFrequency * (100.0 + m_PercentChange)) / 100.0;
}

void ChangePitchDialog::Calc_ToPitchIndex()
{
	m_ToPitchIndex = (m_FromPitchIndex + 
							(int)(m_SemitonesChange + 
									// Round in the right direction.
									((m_bWantPitchDown ? -1.0 : 1.0) * 0.5))) 
							% 12;
}

void ChangePitchDialog::Calc_SemitonesChange_fromPitches()
{
	int sign = m_bWantPitchDown ? -1 : 1;
	m_SemitonesChange = sign * (((sign * (m_ToPitchIndex - m_FromPitchIndex)) + 12) % 12); 
}

void ChangePitchDialog::Calc_SemitonesChange_fromPercentChange()
{
	// Use m_PercentChange rather than m_FromFrequency & m_ToFrequency, because 
	// they start out uninitialized, but m_PercentChange is always valid.
	m_SemitonesChange = (12.0 * log((100.0 + m_PercentChange) / 100.0)) / log(2.0);
}

void ChangePitchDialog::Calc_PercentChange()
{
	m_PercentChange = 100.0 * (pow(2.0, (m_SemitonesChange / 12.0)) - 1.0);
}


// handlers

void ChangePitchDialog::OnChoice_FromPitch(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

	wxChoice * pChoice = this->GetChoice_FromPitch();
   if (pChoice) {
		m_FromPitchIndex = pChoice->GetSelection();

		this->Calc_ToPitchIndex();

		m_bLoopDetect = true;
		this->Update_Choice_ToPitch();
		m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnRadioBox_PitchUpDown(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

	wxRadioBox * pRadioBox = this->GetRadioBox_PitchUpDown();
	if (pRadioBox) {
		m_bWantPitchDown = (pRadioBox->GetSelection() == 1);

		this->Calc_SemitonesChange_fromPitches();
		this->Calc_PercentChange(); // Call *after* m_SemitonesChange is updated.
		this->Calc_ToFrequency(); // Call *after* m_PercentChange is updated.

		m_bLoopDetect = true;
		this->Update_Text_SemitonesChange();
		this->Update_Text_ToFrequency();
		this->Update_Text_PercentChange();
		this->Update_Slider_PercentChange();
		m_bLoopDetect = false;
	}
}

void ChangePitchDialog::OnChoice_ToPitch(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

	wxChoice * pChoice = this->GetChoice_ToPitch();
   if (pChoice) {
		m_ToPitchIndex = pChoice->GetSelection();

		this->Calc_SemitonesChange_fromPitches();
		this->Calc_PercentChange(); // Call *after* m_SemitonesChange is updated.
		this->Calc_ToFrequency(); // Call *after* m_PercentChange is updated.

		m_bLoopDetect = true;
		this->Update_Text_SemitonesChange();
		this->Update_Text_ToFrequency();
		this->Update_Text_PercentChange();
		this->Update_Slider_PercentChange();
		m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnText_SemitonesChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

	wxTextCtrl * pTextCtrl = this->GetTextCtrl_SemitonesChange();
	if (pTextCtrl) {
		wxString str = pTextCtrl->GetValue();
      double newValue;
      str.ToDouble(&newValue);
		m_SemitonesChange = newValue;

		this->Calc_PercentChange();
		this->Calc_ToFrequency(); // Call *after* m_PercentChange is updated.
		m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);
		this->Calc_ToPitchIndex(); // Call *after* m_bWantPitchDown is updated.

		m_bLoopDetect = true;
		this->Update_RadioBox_PitchUpDown();
		if (pTextCtrl->IsModified())
			pTextCtrl->SetFocus(); // See note at implementation of Update_RadioBox_PitchUpDown.
		this->Update_Choice_ToPitch();
		this->Update_Text_ToFrequency();
		this->Update_Text_PercentChange();
		this->Update_Slider_PercentChange();
		m_bLoopDetect = false;
	}
}

void ChangePitchDialog::OnText_FromFrequency(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

	wxTextCtrl * pTextCtrl = this->GetTextCtrl_FromFrequency();
	if (pTextCtrl) {
		wxString str = pTextCtrl->GetValue();
		double newDouble;
      str.ToDouble(&newDouble);
		m_FromFrequency = newDouble;

		m_FromPitchIndex = PitchIndex(Freq2Pitch(m_FromFrequency));
		this->Calc_ToFrequency();
		m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);
		this->Calc_ToPitchIndex(); // Call *after* m_bWantPitchDown is updated.

		m_bLoopDetect = true;
		this->Update_RadioBox_PitchUpDown();
		if (pTextCtrl->IsModified())
			pTextCtrl->SetFocus(); // See note at implementation of Update_RadioBox_PitchUpDown.
		this->Update_Choice_ToPitch();
		this->Update_Text_ToFrequency();
		m_bLoopDetect = false;
	}
}

void ChangePitchDialog::OnText_ToFrequency(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   wxTextCtrl * pTextCtrl = this->GetTextCtrl_ToFrequency();
   if (pTextCtrl) {
      wxString str = pTextCtrl->GetValue();
		double newDouble;
      str.ToDouble(&newDouble);
		m_ToFrequency = newDouble;

		m_PercentChange = (((double)(m_ToFrequency) * 100.0) / 
									(double)(m_FromFrequency)) - 100.0;

		this->Calc_SemitonesChange_fromPercentChange();
		this->Calc_ToPitchIndex(); // Call *after* m_SemitonesChange is updated.
		m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);

		m_bLoopDetect = true;
		this->Update_RadioBox_PitchUpDown();
		if (pTextCtrl->IsModified())
			pTextCtrl->SetFocus(); // See note at implementation of Update_RadioBox_PitchUpDown.
		this->Update_Choice_ToPitch();
		this->Update_Text_SemitonesChange();
		this->Update_Text_PercentChange();
		this->Update_Slider_PercentChange();
		m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnText_PercentChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   wxTextCtrl * pTextCtrl = this->GetTextCtrl_PercentChange();
   if (pTextCtrl) {
      wxString str = pTextCtrl->GetValue();
      double newValue;
      str.ToDouble(&newValue);
		m_PercentChange = newValue;

		this->Calc_SemitonesChange_fromPercentChange();
		this->Calc_ToPitchIndex(); // Call *after* m_SemitonesChange is updated.
		this->Calc_ToFrequency();
		m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);

      m_bLoopDetect = true;
		this->Update_RadioBox_PitchUpDown();
		if (pTextCtrl->IsModified())
			pTextCtrl->SetFocus(); // See note at implementation of Update_RadioBox_PitchUpDown.
		this->Update_Choice_ToPitch();
		this->Update_Text_SemitonesChange();
		this->Update_Text_ToFrequency();
		this->Update_Slider_PercentChange();
      m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnSlider_PercentChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

	wxSlider * pSlider = this->GetSlider_PercentChange();
	if (pSlider) {
		m_PercentChange = (double)(pSlider->GetValue()); 
		// Warp positive values to actually go up faster & further than negatives.
		if (m_PercentChange > 0.0)
			m_PercentChange = pow(m_PercentChange, PERCENTCHANGE_SLIDER_WARP);

		this->Calc_SemitonesChange_fromPercentChange();
		this->Calc_ToPitchIndex(); // Call *after* m_SemitonesChange is updated.
		this->Calc_ToFrequency();
		m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);

		m_bLoopDetect = true;
		this->Update_RadioBox_PitchUpDown();
		this->Update_Choice_ToPitch();
		this->Update_Text_SemitonesChange();
		this->Update_Text_ToFrequency();
		this->Update_Text_PercentChange();
	   m_bLoopDetect = false;
	}
}

void ChangePitchDialog::OnOk(wxCommandEvent & event)
{
   TransferDataFromWindow();
   
   if (Validate()) 
      EndModal(true);
   else 
      event.Skip();
}

void ChangePitchDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}


// helper fns

// NOTE: wxWindows ref (C:\wxWindows_2.4.0\docs\htmlhelp) says 
// wxRadioBox::SetSelection "does not cause a 
// wxEVT_COMMAND_RADIOBOX_SELECTED event to get emitted", but it 
// calls SetFocus, which sure as heck DOES select the radio button.
//
// So, any wxTextCtrl handler that calls Update_RadioBox_PitchUpDown 
// needs to call wxTextCtrl::SetFocus afterward, to return the 
// focus to the wxTextCtrl so the user can keep typing.
//
// Also, it turns out the wxTextCtrl handlers are sometimes 
// called before the dialog is displayed, so those SetFocus calls 
// need to be conditionalized on wxTextCtrl::IsModified.
void ChangePitchDialog::Update_RadioBox_PitchUpDown() 
{
	wxRadioBox * pRadioBox = this->GetRadioBox_PitchUpDown();
	if (pRadioBox)
		pRadioBox->SetSelection((int)(m_bWantPitchDown));
}

void ChangePitchDialog::Update_Choice_ToPitch() 
{
	wxChoice * pChoice = this->GetChoice_ToPitch();
	if (pChoice) 
		pChoice->SetSelection(m_ToPitchIndex);
}


void ChangePitchDialog::Update_Text_SemitonesChange()
{
	wxTextCtrl * pTextCtrl = this->GetTextCtrl_SemitonesChange();
	if (pTextCtrl) {
		wxString str;
		str.Printf("%.2f", m_SemitonesChange);
		pTextCtrl->SetValue(str);
	}
}

void ChangePitchDialog::Update_Text_ToFrequency() 
{
	wxTextCtrl * pTextCtrl = this->GetTextCtrl_ToFrequency();
	if (pTextCtrl) {
		wxString str;
		if (m_ToFrequency > 0.0)
			str.Printf("%.3f", m_ToFrequency);
		else
			str = "";
		pTextCtrl->SetValue(str);
	}
}


void ChangePitchDialog::Update_Text_PercentChange()
{
	wxTextCtrl * pTextCtrl = this->GetTextCtrl_PercentChange();
	if (pTextCtrl) {
		wxString str;
		str.Printf("%.1f", m_PercentChange);
		pTextCtrl->SetValue(str);
	}
}

void ChangePitchDialog::Update_Slider_PercentChange()
{
   wxSlider * pSlider = this->GetSlider_PercentChange();
   if (pSlider) {
		double unwarped = m_PercentChange;
		if (unwarped > 0.0)
			// Un-warp values above zero to actually go up to PERCENTCHANGE_MAX.
			unwarped = pow(m_PercentChange, (1.0 / PERCENTCHANGE_SLIDER_WARP));
		pSlider->SetValue((int)(unwarped + 0.5)); // Add 0.5 so trunc -> round.
	}
}


#endif // USE_SOUNDTOUCH
