/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangePitch.cpp

  Vaughan Johnson, Dominic Mazzoni
  
  Change Pitch effect, that allows raising or lowering 
  the pitch without changing the tempo.

**********************************************************************/

#include "../Audacity.h"

#if USE_SOUNDTOUCH

#include <math.h>

#include <SoundTouch.h>

#include <wx/intl.h>
#include <wx/valtext.h>

#include "ChangePitch.h"
#include "../WaveTrack.h"

//
// EffectChangePitch
//

EffectChangePitch::EffectChangePitch()
{
	m_FromPitch = 0; // indicates not yet set
	m_ToPitch = 0; // indicates not yet set

	m_SemitonesChange = 0.0;

   m_FromFrequency = 0;	
   m_ToFrequency = 0;	

	m_PercentChange = 0.0;
}

wxString EffectChangePitch::GetEffectDescription() { 
   // Note: This is useful only after change amount has been set. 
   return wxString::Format(_("Applied effect: %s %.1f%%"), 
                           (const char *)(this->GetEffectName()), 
									m_PercentChange); 
} 

bool EffectChangePitch::Init()
{
   mSoundTouch = NULL;
	return true;
}

bool EffectChangePitch::PromptUser()
{
   ChangePitchDialog dlog(mParent, -1, _("Change Pitch"));
   dlog.m_FromPitch = m_FromPitch;
   dlog.m_ToPitch = m_ToPitch;
	dlog.m_SemitonesChange = m_SemitonesChange;
   dlog.m_FromFrequency = m_FromFrequency;
   dlog.m_ToFrequency = m_ToFrequency;
   dlog.m_PercentChange = m_PercentChange;
   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   m_FromPitch = dlog.m_FromPitch;
   m_ToPitch = dlog.m_ToPitch;
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

    EVT_TEXT(ID_TEXT_FROMPITCH, ChangePitchDialog::OnText_FromPitch)
    EVT_TEXT(ID_TEXT_TOPITCH, ChangePitchDialog::OnText_ToPitch)

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

	m_FromPitch = 0; // indicates not yet set
	m_ToPitch = 0; // indicates not yet set

	m_SemitonesChange = 0.0;

   m_FromFrequency = 0;	
   m_ToFrequency = 0;	

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

   wxTextCtrl * pTextCtrl_FromPitch =
       new wxTextCtrl(this, ID_TEXT_FROMPITCH, _(""), 
								wxDefaultPosition, wxSize(40, -1), 0,
								wxTextValidator(wxFILTER_ALPHA));
   pBoxSizer_Pitch->Add(pTextCtrl_FromPitch, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pStaticText =
       new wxStaticText(this, ID_TEXT, _("to"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Pitch->Add(pStaticText, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   wxTextCtrl * pTextCtrl_ToPitch =
       new wxTextCtrl(this, ID_TEXT_TOPITCH, _(""), 
								wxDefaultPosition, wxSize(40, -1), 0,
								wxTextValidator(wxFILTER_ALPHA));
   pBoxSizer_Pitch->Add(pTextCtrl_ToPitch, 0, 
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
       new wxTextCtrl(this, ID_TEXT_SEMITONESCHANGE, _("0.0"), 
								wxDefaultPosition, wxSize(40, -1), 0,
								wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_SemitonesChange->Add(pTextCtrl_SemitonesChange, 0, 
												wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_SemitonesChange, 0, wxALIGN_CENTER | wxALL, 4);

	
	// from/to frequency controls
   wxBoxSizer * pBoxSizer_Frequency = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText =
       new wxStaticText(this, ID_TEXT, _("Frequency (Hz):   from"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Frequency->Add(pStaticText, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   wxTextCtrl * pTextCtrl_FromFrequency =
       new wxTextCtrl(this, ID_TEXT_FROMFREQUENCY, _(""), 
								wxDefaultPosition, wxSize(48, -1), 0,
								wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_Frequency->Add(pTextCtrl_FromFrequency, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pStaticText =
       new wxStaticText(this, ID_TEXT, _("to"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Frequency->Add(pStaticText, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   wxTextCtrl * pTextCtrl_ToFrequency =
       new wxTextCtrl(this, ID_TEXT_TOFREQUENCY, _(""), 
								wxDefaultPosition, wxSize(48, -1), 0,
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
       new wxTextCtrl(this, ID_TEXT_PERCENTCHANGE, _("0.0"), 
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

	wxString str;
	wxTextCtrl * pTextCtrl;

	// from/to Pitch controls
	pTextCtrl = this->GetTextCtrl_FromPitch();
	if (pTextCtrl) {
		if (m_FromPitch != 0)
			str.Printf(_("%d"), m_FromPitch);
		else
			str = _("");
		pTextCtrl->SetValue(str);
		pTextCtrl->Enable(false); //v Disable until it's working.
	}

	pTextCtrl = this->GetTextCtrl_ToPitch();
	if (pTextCtrl) {
		if (m_ToPitch != 0)
			str.Printf(_("%d"), m_ToPitch);
		else
			str = _("");
		pTextCtrl->SetValue(str);
		pTextCtrl->Enable(false); //v Disable until it's working.
	}


	// semitones change control
	pTextCtrl = this->GetTextCtrl_SemitonesChange();
	if (pTextCtrl) {
		str.Printf(_("%.2f"), m_SemitonesChange);
		pTextCtrl->SetValue(str);
	}


	// from/to frequency controls
	pTextCtrl = this->GetTextCtrl_FromFrequency();
	if (pTextCtrl) {
		if (m_FromFrequency != 0)
			str.Printf(_("%d"), m_FromFrequency);
		else
			str = _("");
		pTextCtrl->SetValue(str);
	}

	pTextCtrl = this->GetTextCtrl_ToFrequency();
	if (pTextCtrl) {
		if (m_ToFrequency != 0)
			str.Printf(_("%d"), m_ToFrequency);
		else
			str = _("");
		pTextCtrl->SetValue(str);
	}

	
	// percent change controls
	this->Update_Text_PercentChange();
	this->Update_Slider_PercentChange();


   m_bLoopDetect = false;

	return true;
}

bool ChangePitchDialog::TransferDataFromWindow()
{
   double newDouble;
   long newLong;
	wxString str;
	wxTextCtrl * pTextCtrl;


	// from/to Pitch controls
   pTextCtrl = this->GetTextCtrl_FromPitch();
   if (pTextCtrl) {
      str = pTextCtrl->GetValue();
      str.ToLong(&newLong);
		m_FromPitch = (int)(newLong);
	}

   pTextCtrl = this->GetTextCtrl_ToPitch();
   if (pTextCtrl) {
      str = pTextCtrl->GetValue();
      str.ToLong(&newLong);
		m_ToPitch = (int)(newLong);
	}


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
      str.ToLong(&newLong);
		m_FromFrequency = (int)(newLong);
	}

   pTextCtrl = this->GetTextCtrl_ToFrequency();
   if (pTextCtrl) {
      str = pTextCtrl->GetValue();
      str.ToLong(&newLong);
		m_ToFrequency = (int)(newLong);
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


// handler implementations for ChangePitchDialog

void ChangePitchDialog::OnText_FromPitch(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   wxTextCtrl * pTextCtrl_FromPitch = this->GetTextCtrl_FromPitch();
   if (pTextCtrl_FromPitch) {
      wxString str = pTextCtrl_FromPitch->GetValue();

		long newValue;
		str.ToLong(&newValue);
		m_FromPitch = (int)(newValue);

		m_bLoopDetect = true;

		//v Update FromFrequency first, then use that to calculate ToPitch?
		this->Update_Text_ToPitch();

		m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnText_ToPitch(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   wxTextCtrl * pTextCtrl_ToPitch = this->GetTextCtrl_ToPitch();
   if (pTextCtrl_ToPitch) {
      wxString str = pTextCtrl_ToPitch->GetValue();
		/* //v 
			long newValue;
			str.ToLong(&newValue);
			m_ToPitch = (int)(newValue);

			m_bLoopDetect = true;

			// If FromPitch has already been set, then there's a new percent change.
			if (m_FromPitch != 0) {
				m_PercentChange = (((double)(m_ToPitch) * 100.0) / (double)(m_FromPitch)) - 100.0;

				this->Update_Text_PercentChange();
				this->Update_Slider_PercentChange();

				this->Update_Text_ToFrequency();
			}
      
			m_bLoopDetect = false;
			*/
   }
}

void ChangePitchDialog::OnText_SemitonesChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

	wxTextCtrl * pTextCtrl_SemitonesChange = this->GetTextCtrl_SemitonesChange();
	if (pTextCtrl_SemitonesChange) {
		wxString str = pTextCtrl_SemitonesChange->GetValue();
      double newValue;
      str.ToDouble(&newValue);
		m_SemitonesChange = newValue;

		m_PercentChange = 100.0 * (pow(2.0, (m_SemitonesChange / 12.0)) - 1.0);

		m_bLoopDetect = true;

		this->Update_Text_ToPitch();

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

	wxTextCtrl * pTextCtrl_FromFrequency = this->GetTextCtrl_FromFrequency();
	if (pTextCtrl_FromFrequency) {
		wxString str = pTextCtrl_FromFrequency->GetValue();
		long newValue;
		str.ToLong(&newValue);
		m_FromFrequency = (unsigned int)newValue;

		m_bLoopDetect = true;

		//v update m_FromPitch
		this->Update_Text_ToPitch();

		this->Update_Text_ToFrequency();

		m_bLoopDetect = false;
	}
}

void ChangePitchDialog::OnText_ToFrequency(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   wxTextCtrl * pTextCtrl_ToFrequency = this->GetTextCtrl_ToFrequency();
   if (pTextCtrl_ToFrequency) {
      wxString str = pTextCtrl_ToFrequency->GetValue();
		double newValue;
		str.ToDouble(&newValue);
		m_ToFrequency = newValue;

		if (m_FromFrequency != 0) {
			m_PercentChange = (((double)(m_ToFrequency) * 100.0) / 
										(double)(m_FromFrequency)) - 100.0;

			m_bLoopDetect = true;

			this->Update_Text_ToPitch();
   
			this->Update_Text_SemitonesChange();

			this->Update_Text_PercentChange();
			this->Update_Slider_PercentChange();

			m_bLoopDetect = false;
		}
   }
}

void ChangePitchDialog::OnText_PercentChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   wxTextCtrl * pTextCtrl_PercentChange = this->GetTextCtrl_PercentChange();
   if (pTextCtrl_PercentChange) {
      wxString str = pTextCtrl_PercentChange->GetValue();
      double newValue;
      str.ToDouble(&newValue);
		m_PercentChange = newValue;

      m_bLoopDetect = true;

		this->Update_Text_ToPitch();

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

	wxSlider * slider = this->GetSlider_PercentChange();
	if (slider) {
		m_PercentChange = (double)(slider->GetValue()); 
		// Warp positive values to actually go up faster & further than negatives.
		if (m_PercentChange > 0.0)
			m_PercentChange = pow(m_PercentChange, PERCENTCHANGE_SLIDER_WARP);

	   m_bLoopDetect = true;

		this->Update_Text_ToPitch();

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

void ChangePitchDialog::Update_Text_ToPitch() 
// Use m_FromPitch & m_PercentChange to set new m_ToPitch & control.
{
   // Update m_ToPitch iff m_FromPitch has been set.
	if (m_FromPitch == 0) 
		return;

	/* //v 
	m_ToPitch = (unsigned int)((((double)(m_FromPitch) * 
											(100.0 + m_PercentChange)) / 100.0) + 
										0.5); // Add 0.5 so trunc -> round.
	wxTextCtrl * pTextCtrl_ToPitch = this->GetTextCtrl_ToPitch();
	if (pTextCtrl_ToPitch) {
		wxString str;
		str.Printf(_("%d"), m_ToPitch);
		pTextCtrl_ToPitch->SetValue(str);
	}
	*/
}

void ChangePitchDialog::Update_Text_SemitonesChange()
// Use m_PercentChange to set new m_SemitonesChange & control.
{
	// Use m_PercentChange rather than m_FromFrequency & m_ToFrequency, because 
	// they start out uninitialized, but m_PercentChange is always valid.
	m_SemitonesChange = (12.0 * log((100.0 + m_PercentChange) / 100.0)) / log(2.0);
	wxTextCtrl * pTextCtrl_SemitonesChange = this->GetTextCtrl_SemitonesChange();
	if (pTextCtrl_SemitonesChange) {
		wxString str;
		str.Printf(_("%.2f"), m_SemitonesChange);
		pTextCtrl_SemitonesChange->SetValue(str);
	}
}

void ChangePitchDialog::Update_Text_ToFrequency() 
// Use m_FromFrequency & m_PercentChange to set new m_ToFrequency & control.
{
   // Update m_ToFrequency iff m_FromFrequency has been set.
	if (m_FromFrequency == 0) 
		return;

	m_ToFrequency = (unsigned int)(((double)(m_FromFrequency) * 
												(100.0 + m_PercentChange)) / 100.0);
	wxTextCtrl * pTextCtrl_ToFrequency = this->GetTextCtrl_ToFrequency();
	if (pTextCtrl_ToFrequency) {
		wxString str;
		str.Printf(_("%d"), m_ToFrequency);
		pTextCtrl_ToFrequency->SetValue(str);
	}
}


void ChangePitchDialog::Update_Text_PercentChange()
{
	wxTextCtrl * pTextCtrl = this->GetTextCtrl_PercentChange();
	if (pTextCtrl) {
		wxString str;
		str.Printf(_("%.1f"), m_PercentChange);
		pTextCtrl->SetValue(str);
	}
}

void ChangePitchDialog::Update_Slider_PercentChange()
{
   wxSlider * slider = this->GetSlider_PercentChange();
   if (slider) {
		double unwarped = m_PercentChange;
		if (unwarped > 0.0)
			// Un-warp values above zero to actually go up to PERCENTCHANGE_MAX.
			unwarped = pow(m_PercentChange, (1.0 / PERCENTCHANGE_SLIDER_WARP));
		slider->SetValue((int)(unwarped + 0.5)); // Add 0.5 so trunc -> round.
	}
}


#endif // USE_SOUNDTOUCH
