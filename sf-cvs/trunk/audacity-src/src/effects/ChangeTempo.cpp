/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeTempo.cpp

  Vaughan Johnson, Dominic Mazzoni
  
  This class supports a Change Tempo effect, that allows speeding up or 
  slowing down tempo without changing pitch.

**********************************************************************/

#include <math.h>
#include <wx/intl.h>

#include "ChangeTempo.h"
#include "../Project.h"
#include "../WaveTrack.h"

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
	m_ToLength = (m_FromLength * 100.0) / (100.0 + (double)(m_PercentChange));
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
   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   m_PercentChange = dlog.m_PercentChange;
   m_FromBPM = dlog.m_FromBPM;
   m_ToBPM = dlog.m_ToBPM;
   m_ToLength = dlog.m_ToLength;

   return true;
}

bool EffectChangeTempo::ProcessSimpleMono(float * buffer, sampleCount len)
{
	//v WSOLA or Soundstretch?
   return true;
}

//----------------------------------------------------------------------------
// ChangeTempoDialog
//----------------------------------------------------------------------------

#define PERCENTCHANGE_MIN -99
#define PERCENTCHANGE_MAX 200

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

ChangeTempoDialog::ChangeTempoDialog(wxWindow * parent, wxWindowID id, const wxString & title, const wxPoint & position, const wxSize & size, long style):
wxDialog(parent, id, title, position, size, style)
{
   m_bLoopDetect = false;

	m_PercentChange = 0.0;
	m_FromBPM = 0; // indicates not yet set
	m_ToBPM = 0; // indicates not yet set
   m_FromLength = 0.0;	
   m_ToLength = 0.0;	

   MakeChangeTempoDialog(this, true, true);
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

	wxTextCtrl * pTextCtrl = GetTextCtrl_FromBPM();
	if (pTextCtrl) {
		if (m_FromBPM != 0)
			str.Printf("%d", m_FromBPM);
		else
			str = "";
		pTextCtrl->SetValue(str);
	}

	pTextCtrl = GetTextCtrl_ToBPM();
	if (pTextCtrl) {
		if (m_ToBPM != 0)
			str.Printf("%d", m_ToBPM);
		else
			str = "";
		pTextCtrl->SetValue(str);
	}


	// from/to Length controls
	pTextCtrl = GetTextCtrl_FromLength();
	if (pTextCtrl) {
		str.Printf("%.2f", m_FromLength);
		pTextCtrl->SetValue(str);
		// pTextCtrl->SetEditable(false); // wxTE_READONLY doesn't seem to work.
		pTextCtrl->Enable(false); // wxTE_READONLY doesn't seem to work.
	}

	pTextCtrl = GetTextCtrl_ToLength();
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
   wxTextCtrl * pTextCtrl = GetTextCtrl_PercentChange();
   if (pTextCtrl) {
      str = pTextCtrl->GetValue();
      double newDouble;
      str.ToDouble(&newDouble);
		m_PercentChange = newDouble;
	}

	// Ignore Slider_PercentChange because TextCtrl_PercentChange 
	// always tracks it & is more precise (decimal points).


	// from/to BPM controls
   long newLong;

   pTextCtrl = GetTextCtrl_FromBPM();
   if (pTextCtrl) {
      str = pTextCtrl->GetValue();
      str.ToLong(&newLong);
		m_FromBPM = (int)(newLong);
	}

   pTextCtrl = GetTextCtrl_ToBPM();
   if (pTextCtrl) {
      str = pTextCtrl->GetValue();
      str.ToLong(&newLong);
		m_ToBPM = (int)(newLong);
	}


	// from/to Length controls
   // Don't get TextCtrl_FromLength. It's read only.
   pTextCtrl = GetTextCtrl_ToLength();
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

   wxTextCtrl * pTextCtrl_PercentChange = GetTextCtrl_PercentChange();
   if (pTextCtrl_PercentChange) {
      wxString str = pTextCtrl_PercentChange->GetValue();
      double newValue;
      str.ToDouble(&newValue);
		m_PercentChange = newValue;

      m_bLoopDetect = true;

		this->Update_Slider_PercentChange();

      // Update ToBPM iff FromBPM has been set.
		if (m_FromBPM != 0) 
			this->Update_Text_ToBPM();

		this->Update_Text_ToLength();

      m_bLoopDetect = false;
   }
}

void ChangeTempoDialog::OnSlider_PercentChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

	wxSlider * slider = GetSlider_PercentChange();
	if (slider) {
		m_PercentChange = (float)(slider->GetValue());

	   m_bLoopDetect = true;

		this->Update_Text_PercentChange();

      // Update ToBPM iff FromBPM has been set.
		if (m_FromBPM != 0) 
			this->Update_Text_ToBPM();

		this->Update_Text_ToLength();

	   m_bLoopDetect = false;
	}
}

void ChangeTempoDialog::OnText_FromBPM(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   wxTextCtrl * pTextCtrl_FromBPM = GetTextCtrl_FromBPM();
   if (pTextCtrl_FromBPM) {
      wxString str = pTextCtrl_FromBPM->GetValue();
      long newValue;
      str.ToLong(&newValue);
		//v Disallow a negative value. Validator?
		m_FromBPM = (int)(newValue);

      m_bLoopDetect = true;

		this->Update_Text_ToBPM();

      m_bLoopDetect = false;
   }
}

void ChangeTempoDialog::OnText_ToBPM(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   wxTextCtrl * pTextCtrl_ToBPM = GetTextCtrl_ToBPM();
   if (pTextCtrl_ToBPM) {
      wxString str = pTextCtrl_ToBPM->GetValue();
      long newValue;
      str.ToLong(&newValue);
		m_ToBPM = (int)(newValue);

      m_bLoopDetect = true;

		// If FromBPM has already been set, then there's a new percent change.
		if (m_FromBPM != 0) {
			m_PercentChange = (((float)(m_ToBPM) * 100.0) / (float)(m_FromBPM)) - 100.0;

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

   wxTextCtrl * pTextCtrl_ToLength = GetTextCtrl_ToLength();
   if (pTextCtrl_ToLength) {
      wxString str = pTextCtrl_ToLength->GetValue();
      double newDouble;
      str.ToDouble(&newDouble);
		m_ToLength = newDouble;

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
	wxTextCtrl * pTextCtrl = GetTextCtrl_PercentChange();
	if (pTextCtrl) {
		wxString str;
		str.Printf("%.1f", m_PercentChange);
		pTextCtrl->SetValue(str);
	}
}

void ChangeTempoDialog::Update_Slider_PercentChange()
{
   wxSlider * slider = GetSlider_PercentChange();
   if (slider)
      slider->SetValue((int)(m_PercentChange + 0.5)); // Add 0.5 so trunc -> round.
}

void ChangeTempoDialog::Update_Text_ToBPM() 
// Use m_FromBPM & m_PercentChange to set new m_ToBPM & control.
{
	m_ToBPM = (unsigned int)((((float)(m_FromBPM) * 
											(100.0 + m_PercentChange)) / 100.0) + 
										0.5); // Add 0.5 so trunc -> round.
	wxTextCtrl * pTextCtrl_ToBPM = GetTextCtrl_ToBPM();
	if (pTextCtrl_ToBPM) {
		wxString str;
		str.Printf("%d", m_ToBPM);
		pTextCtrl_ToBPM->SetValue(str);
	}
}

void ChangeTempoDialog::Update_Text_ToLength() 
// Use m_FromLength & m_PercentChange to set new m_ToLength & control.
{
	m_ToLength = (m_FromLength * 100.0) / (100.0 + (double)(m_PercentChange));
	wxTextCtrl * pTextCtrl_ToLength = GetTextCtrl_ToLength();
	if (pTextCtrl_ToLength) {
		wxString str;
		str.Printf("%.2f", m_ToLength);
		pTextCtrl_ToLength->SetValue(str);
	}
}


wxSizer * MakeChangeTempoDialog(wxWindow * parent, bool call_fit, bool set_sizer)
{
   wxBoxSizer * pBoxSizer_Dialog = new wxBoxSizer(wxVERTICAL);

	// heading
   wxStaticText * pStaticText =
		new wxStaticText(parent, ID_TEXT, _("Change Tempo without Changing Pitch"),
								wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Dialog->Add(pStaticText, 0, wxALIGN_CENTER | wxALL, 8);


	// percent change controls
   wxBoxSizer * pBoxSizer_PercentChange = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText =
		new wxStaticText(parent, ID_TEXT, _("Percent Change:"),
								wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_PercentChange->Add(pStaticText, 0, 
											wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

	//v Use wxTextValidator for wxTextCtrl objects?
   wxTextCtrl * pTextCtrl_PercentChange =
       new wxTextCtrl(parent, ID_TEXT_PERCENTCHANGE, "0.0", wxDefaultPosition, 
                      wxSize(40, -1), 0);
   pBoxSizer_PercentChange->Add(pTextCtrl_PercentChange, 0, 
											wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);

   pBoxSizer_Dialog->Add(pBoxSizer_PercentChange, 0, wxALIGN_CENTER | wxALL, 5);

   wxSlider * pSlider_PercentChange =
       new wxSlider(parent, ID_SLIDER_PERCENTCHANGE, 
							0, PERCENTCHANGE_MIN, PERCENTCHANGE_MAX,
							wxDefaultPosition, wxSize(100, -1), wxSL_HORIZONTAL);
   pBoxSizer_Dialog->Add(pSlider_PercentChange, 1, 
									wxGROW | wxALIGN_CENTER | wxLEFT | wxRIGHT, 5);


	// from/to beats-per-minute controls
   wxBoxSizer * pBoxSizer_BPM = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText =
       new wxStaticText(parent, ID_TEXT, _("Beats per Minute (BPM):     From"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_BPM->Add(pStaticText, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

   wxTextCtrl * pTextCtrl_FromBPM =
       new wxTextCtrl(parent, ID_TEXT_FROMBPM, "", wxDefaultPosition,
                      wxSize(40, -1), 0);
   pBoxSizer_BPM->Add(pTextCtrl_FromBPM, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);

   pStaticText =
       new wxStaticText(parent, ID_TEXT, _("To"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_BPM->Add(pStaticText, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

   wxTextCtrl * pTextCtrl_ToBPM =
       new wxTextCtrl(parent, ID_TEXT_TOBPM, "", wxDefaultPosition,
                      wxSize(40, -1), 0);
   pBoxSizer_BPM->Add(pTextCtrl_ToBPM, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);

   pBoxSizer_Dialog->Add(pBoxSizer_BPM, 0, wxALIGN_CENTER | wxALL, 5);


	// from/to length controls
   wxBoxSizer * pBoxSizer_Length = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText =
       new wxStaticText(parent, ID_TEXT, _("Length (seconds):     From"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Length->Add(pStaticText, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

   wxTextCtrl * pTextCtrl_FromLength =
       new wxTextCtrl(parent, ID_TEXT_FROMLENGTH, "", wxDefaultPosition,
                      wxSize(48, -1), 0);
   pBoxSizer_Length->Add(pTextCtrl_FromLength, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL | 
										wxTE_READONLY, // readonly because it's from the selection
									5);

   pStaticText =
       new wxStaticText(parent, ID_TEXT, _("To"),
                        wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Length->Add(pStaticText, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

   wxTextCtrl * pTextCtrl_ToLength =
       new wxTextCtrl(parent, ID_TEXT_TOLENGTH, "", wxDefaultPosition,
                      wxSize(48, -1), 0);
   pBoxSizer_Length->Add(pTextCtrl_ToLength, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);

   pBoxSizer_Dialog->Add(pBoxSizer_Length, 0, wxALIGN_CENTER | wxALL, 5);


	// OK & Cancel buttons
   wxBoxSizer * pBoxSizer_OK = new wxBoxSizer(wxHORIZONTAL);

   wxButton * pButton_OK =
       new wxButton(parent, wxID_OK, _("OK"), wxDefaultPosition,
                    wxDefaultSize, 0);
   pButton_OK->SetDefault();
   pButton_OK->SetFocus();
   pBoxSizer_OK->Add(pButton_OK, 0, wxALIGN_CENTER | wxALL, 5);

   wxButton * pButton_Cancel =
       new wxButton(parent, wxID_CANCEL, _("Cancel"), wxDefaultPosition,
                    wxDefaultSize, 0);
   pBoxSizer_OK->Add(pButton_Cancel, 0, wxALIGN_CENTER | wxALL, 5);

   pBoxSizer_Dialog->Add(pBoxSizer_OK, 0, wxALIGN_CENTER | wxALL, 8);


   if (set_sizer) {
      parent->SetAutoLayout(true);
      parent->SetSizer(pBoxSizer_Dialog);
      if (call_fit) {
         pBoxSizer_Dialog->Fit(parent);
         pBoxSizer_Dialog->SetSizeHints(parent);
      }
   }

   return pBoxSizer_Dialog;
}


