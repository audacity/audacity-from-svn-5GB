/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeTempo.cpp

  Vaughan Johnson, Dominic Mazzoni
  
  Change Tempo effect provides speeding up or 
  slowing down tempo without changing pitch.

**********************************************************************/

#include "../Audacity.h" // for USE_SOUNDTOUCH

#if USE_SOUNDTOUCH

#include <math.h>

#include <wx/intl.h>
#include <wx/valtext.h>

#include <SoundTouch.h>

#include "ChangeTempo.h"

//
// EffectChangeTempo
//

EffectChangeTempo::EffectChangeTempo()
{
	m_PercentChange = 0.0;
	m_FromBPM = 0; // indicates not yet set
	m_ToBPM = 0; // indicates not yet set
   m_FromLength = 0.0;	
   m_ToLength = 0.0;	
}

wxString EffectChangeTempo::GetEffectDescription() { 
   // Note: This is useful only after change amount has been set. 
   return wxString::Format(_("Applied effect: %s %.1f%%"), 
                           (const char *)(this->GetEffectName()), 
									m_PercentChange); 
} 

bool EffectChangeTempo::Init()
{
	// The selection might have changed since the last time EffectChangeTempo 
	// was invoked, so recalculate the Length parameters.
	m_FromLength = mT1 - mT0;
	m_ToLength = (m_FromLength * 100.0) / (100.0 + m_PercentChange);

   mSoundTouch = NULL;

	return true;
}

bool EffectChangeTempo::PromptUser()
{
   ChangeTempoDialog dlog(mParent, -1, _("Change Tempo"));
   dlog.m_PercentChange = m_PercentChange;
   dlog.m_FromBPM = m_FromBPM;
   dlog.m_ToBPM = m_ToBPM;
   dlog.m_FromLength = m_FromLength;
   dlog.m_ToLength = m_ToLength;
	// Don't need to call TransferDataToWindow, although other 
	//	Audacity dialogs (from which I derived this one) do it, because 
	//	ShowModal calls stuff that eventually calls wxWindowBase::OnInitDialog, 
	//	which calls dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   m_PercentChange = dlog.m_PercentChange;
   m_FromBPM = dlog.m_FromBPM;
   m_ToBPM = dlog.m_ToBPM;
   m_ToLength = dlog.m_ToLength;

   mSoundTouch = new SoundTouch();
   mSoundTouch->setChannels(1);
   mSoundTouch->setTempoChange(m_PercentChange);

   return true;
}

//----------------------------------------------------------------------------
// ChangeTempoDialog
//----------------------------------------------------------------------------

#define PERCENTCHANGE_MIN -99
#define PERCENTCHANGE_MAX 100 // warped above zero to actually go up to 400%
#define PERCENTCHANGE_SLIDER_WARP 1.30105 // warp power takes max from 100 to 400.

// event table for ChangeTempoDialog

BEGIN_EVENT_TABLE(ChangeTempoDialog, wxDialog)
    EVT_BUTTON(wxID_OK, ChangeTempoDialog::OnOk)
    EVT_BUTTON(wxID_CANCEL, ChangeTempoDialog::OnCancel)

    EVT_TEXT(ID_TEXT_PERCENTCHANGE, ChangeTempoDialog::OnText_PercentChange)
    EVT_SLIDER(ID_SLIDER_PERCENTCHANGE, ChangeTempoDialog::OnSlider_PercentChange)
    EVT_TEXT(ID_TEXT_FROMBPM, ChangeTempoDialog::OnText_FromBPM)
    EVT_TEXT(ID_TEXT_TOBPM, ChangeTempoDialog::OnText_ToBPM)
    EVT_TEXT(ID_TEXT_TOLENGTH, ChangeTempoDialog::OnText_ToLength)
END_EVENT_TABLE()

ChangeTempoDialog::ChangeTempoDialog(wxWindow * parent, 
												 wxWindowID id, const wxString & title, 
												 const wxPoint & position, const wxSize & size, 
												 long style)
: wxDialog(parent, id, title, position, size, style)
{
   m_bLoopDetect = false;

	m_PercentChange = 0.0;
	m_FromBPM = 0; // indicates not yet set
	m_ToBPM = 0; // indicates not yet set
   m_FromLength = 0.0;	
   m_ToLength = 0.0;	

	
	// CREATE THE CONTROLS PROGRAMMATICALLY.
	wxStaticText * pStaticText;

   wxBoxSizer * pBoxSizer_Dialog = new wxBoxSizer(wxVERTICAL);

	// heading
   pStaticText =
		new wxStaticText(this, ID_TEXT, _("Change Tempo without Changing Pitch"),
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

	
	// percent change controls
	
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


	// from/to beats-per-minute controls
   wxBoxSizer * pBoxSizer_BPM = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText =
       new wxStaticText(this, ID_TEXT, _("Beats per Minute (BPM):   from"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_BPM->Add(pStaticText, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   wxTextCtrl * pTextCtrl_FromBPM =
       new wxTextCtrl(this, ID_TEXT_FROMBPM, "", 
								wxDefaultPosition, wxSize(40, -1), 0,
								wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_BPM->Add(pTextCtrl_FromBPM, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pStaticText =
       new wxStaticText(this, ID_TEXT, _("to"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_BPM->Add(pStaticText, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   wxTextCtrl * pTextCtrl_ToBPM =
       new wxTextCtrl(this, ID_TEXT_TOBPM, "", 
								wxDefaultPosition, wxSize(40, -1), 0,
								wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_BPM->Add(pTextCtrl_ToBPM, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_BPM, 0, wxALIGN_CENTER | wxALL, 4);


	// from/to length controls
   wxBoxSizer * pBoxSizer_Length = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText =
       new wxStaticText(this, ID_TEXT, _("Length (seconds):   from"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Length->Add(pStaticText, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   wxTextCtrl * pTextCtrl_FromLength =
       new wxTextCtrl(this, ID_TEXT_FROMLENGTH, "", 
								wxDefaultPosition, wxSize(48, -1), 
								wxTE_READONLY); // Read only because it's from the selection.
								// No validator because it's read only.
   pBoxSizer_Length->Add(pTextCtrl_FromLength, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pStaticText =
       new wxStaticText(this, ID_TEXT, _("to"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Length->Add(pStaticText, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   wxTextCtrl * pTextCtrl_ToLength =
       new wxTextCtrl(this, ID_TEXT_TOLENGTH, "", 
								wxDefaultPosition, wxSize(48, -1), 0,
								wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_Length->Add(pTextCtrl_ToLength, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_Length, 0, wxALIGN_CENTER | wxALL, 4);


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

bool ChangeTempoDialog::Validate()
{
   return true; 
}

bool ChangeTempoDialog::TransferDataToWindow()
{
   m_bLoopDetect = true;

	// percent change controls
	this->Update_Text_PercentChange();
	this->Update_Slider_PercentChange();


	// from/to BPM controls
	wxString str;

	wxTextCtrl * pTextCtrl = this->GetTextCtrl_FromBPM();
	if (pTextCtrl) {
		if (m_FromBPM != 0)
			str.Printf("%d", m_FromBPM);
		else
			str = "";
		pTextCtrl->SetValue(str);
	}

	pTextCtrl = this->GetTextCtrl_ToBPM();
	if (pTextCtrl) {
		if (m_ToBPM != 0)
			str.Printf("%d", m_ToBPM);
		else
			str = "";
		pTextCtrl->SetValue(str);
	}


	// from/to Length controls
	pTextCtrl = this->GetTextCtrl_FromLength();
	if (pTextCtrl) {
		str.Printf("%.2f", m_FromLength);
		pTextCtrl->SetValue(str);
		pTextCtrl->Enable(false); // Disable because the value comes from the user selection.
	}

	pTextCtrl = this->GetTextCtrl_ToLength();
	if (pTextCtrl) {
		str.Printf("%.2f", m_ToLength);
		pTextCtrl->SetValue(str);
	}

   m_bLoopDetect = false;

	return true;
}

bool ChangeTempoDialog::TransferDataFromWindow()
{
	wxString str;

	// percent change controls
   wxTextCtrl * pTextCtrl = this->GetTextCtrl_PercentChange();
   if (pTextCtrl) {
      str = pTextCtrl->GetValue();
      double newValue;
      str.ToDouble(&newValue);
		m_PercentChange = newValue;
	}

	// Ignore Slider_PercentChange because TextCtrl_PercentChange 
	// always tracks it & is more precise (decimal points).


	// from/to BPM controls
   long newLong;

   pTextCtrl = this->GetTextCtrl_FromBPM();
   if (pTextCtrl) {
      str = pTextCtrl->GetValue();
      str.ToLong(&newLong);
		m_FromBPM = (unsigned int)(newLong);
	}

   pTextCtrl = this->GetTextCtrl_ToBPM();
   if (pTextCtrl) {
      str = pTextCtrl->GetValue();
      str.ToLong(&newLong);
		m_ToBPM = (unsigned int)(newLong);
	}


	// from/to Length controls
   // Don't get TextCtrl_FromLength. It's disabled.
   pTextCtrl = this->GetTextCtrl_ToLength();
   if (pTextCtrl) {
      str = pTextCtrl->GetValue();
      str.ToLong(&newLong);
		m_ToLength = (int)(newLong);
	}

   return true;
}

// handler implementations for ChangeTempoDialog

void ChangeTempoDialog::OnText_PercentChange(wxCommandEvent & event)
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
		this->Update_Slider_PercentChange();
		this->Update_Text_ToBPM();
		this->Update_Text_ToLength();
      m_bLoopDetect = false;
   }
}

void ChangeTempoDialog::OnSlider_PercentChange(wxCommandEvent & event)
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
		this->Update_Text_PercentChange();
		this->Update_Text_ToBPM();
		this->Update_Text_ToLength();
	   m_bLoopDetect = false;
	}
}

void ChangeTempoDialog::OnText_FromBPM(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   wxTextCtrl * pTextCtrl_FromBPM = this->GetTextCtrl_FromBPM();
   if (pTextCtrl_FromBPM) {
      wxString str = pTextCtrl_FromBPM->GetValue();
      long newValue;
      str.ToLong(&newValue);
		m_FromBPM = (unsigned int)(newValue);

      m_bLoopDetect = true;

		this->Update_Text_ToBPM();

      m_bLoopDetect = false;
   }
}

void ChangeTempoDialog::OnText_ToBPM(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   wxTextCtrl * pTextCtrl_ToBPM = this->GetTextCtrl_ToBPM();
   if (pTextCtrl_ToBPM) {
      wxString str = pTextCtrl_ToBPM->GetValue();
      long newValue;
      str.ToLong(&newValue);
		m_ToBPM = (unsigned int)(newValue);

      m_bLoopDetect = true;

		// If FromBPM has already been set, then there's a new percent change.
		if (m_FromBPM != 0) {
			m_PercentChange = (((double)(m_ToBPM) * 100.0) / (double)(m_FromBPM)) - 100.0;

			this->Update_Text_PercentChange();
			this->Update_Slider_PercentChange();

			this->Update_Text_ToLength();
		}
      
      m_bLoopDetect = false;
   }
}

void ChangeTempoDialog::OnText_ToLength(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   wxTextCtrl * pTextCtrl_ToLength = this->GetTextCtrl_ToLength();
   if (pTextCtrl_ToLength) {
      wxString str = pTextCtrl_ToLength->GetValue();
      double newValue;
      str.ToDouble(&newValue);
		m_ToLength = newValue;

		m_PercentChange = ((m_FromLength * 100.0) / m_ToLength) - 100.0;

      m_bLoopDetect = true;

		this->Update_Text_PercentChange();
		this->Update_Slider_PercentChange();

		this->Update_Text_ToBPM();
      
      m_bLoopDetect = false;
   }
}

void ChangeTempoDialog::OnOk(wxCommandEvent & event)
{
   TransferDataFromWindow();
   
   if (Validate()) 
      EndModal(true);
   else 
      event.Skip();
}

void ChangeTempoDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}


// helper fns

void ChangeTempoDialog::Update_Text_PercentChange()
{
	wxTextCtrl * pTextCtrl = this->GetTextCtrl_PercentChange();
	if (pTextCtrl) {
		wxString str;
		str.Printf("%.1f", m_PercentChange);
		pTextCtrl->SetValue(str);
	}
}

void ChangeTempoDialog::Update_Slider_PercentChange()
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

void ChangeTempoDialog::Update_Text_ToBPM() 
// Use m_FromBPM & m_PercentChange to set new m_ToBPM & control.
{
   // Update ToBPM iff FromBPM has been set.
	if (m_FromBPM == 0) 
		return;

	m_ToBPM = (unsigned int)((((double)(m_FromBPM) * 
											(100.0 + m_PercentChange)) / 100.0) + 
										0.5); // Add 0.5 so trunc -> round.
	wxTextCtrl * pTextCtrl_ToBPM = this->GetTextCtrl_ToBPM();
	if (pTextCtrl_ToBPM) {
		wxString str;
		str.Printf("%d", m_ToBPM);
		pTextCtrl_ToBPM->SetValue(str);
	}
}

void ChangeTempoDialog::Update_Text_ToLength() 
// Use m_FromLength & m_PercentChange to set new m_ToLength & control.
{
	m_ToLength = (m_FromLength * 100.0) / (100.0 + m_PercentChange);
	wxTextCtrl * pTextCtrl_ToLength = this->GetTextCtrl_ToLength();
	if (pTextCtrl_ToLength) {
		wxString str;
		str.Printf("%.2f", m_ToLength);
		pTextCtrl_ToLength->SetValue(str);
	}
}

#endif // USE_SOUNDTOUCH
