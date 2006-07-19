/**********************************************************************

  Audacity: A Digital Audio Editor

  SelectionBar.cpp

  Copyright 2005 Dominic Mazzoni
  
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

*******************************************************************//**

\class SelectionBar
\brief (not quite a Toolbar) at foot of screen for setting and viewing the 
selection range.

*//****************************************************************//**

\class SelectionBarListener
\brief A parent class of SelectionBar, used to forward events to do 
with changes in the SelectionBar.

*//*******************************************************************/


#include "Audacity.h"

#include "SelectionBar.h"

#include "AudioIO.h"
#include "AColor.h"
#include "Prefs.h"

#include "widgets/TimeTextCtrl.h"

#include <wx/button.h>
#include <wx/combobox.h>
#include <wx/intl.h>
#include <wx/menu.h>
#include <wx/msgdlg.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/statline.h>
#include <wx/textdlg.h>

enum {
   SelectionBarFirstID = 2700,
   OnRateID,
   OnLengthRadioID,
   OnEndRadioID,
   OnLeftTimeID,
   OnRightTimeID
};

BEGIN_EVENT_TABLE(SelectionBar, wxPanel)
   EVT_SIZE(SelectionBar::OnSize)
   EVT_TEXT(OnLeftTimeID, SelectionBar::OnLeftTime)
   EVT_TEXT(OnRightTimeID, SelectionBar::OnRightTime)
   EVT_RADIOBUTTON(OnLengthRadioID, SelectionBar::OnLengthRadio)
   EVT_RADIOBUTTON(OnEndRadioID, SelectionBar::OnEndRadio)
   EVT_COMBOBOX(OnRateID, SelectionBar::OnRate)
   EVT_TEXT(OnRateID, SelectionBar::OnRate)
   EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, SelectionBar::OnUpdate)
END_EVENT_TABLE()

SelectionBar::SelectionBar(wxWindow * parent, wxWindowID id,
                           const wxPoint & pos,
                           const wxSize & size,
                           double rate,
                           SelectionBarListener * listener):
   wxPanel(parent, id, pos,  size, wxTAB_TRAVERSAL | wxNO_BORDER | wxFULL_REPAINT_ON_RESIZE ),
   mListener(listener), mRate(rate), 
   mStart(0.0), mEnd(0.0), mAudio(0.0),
   mModifyingSelection(false), mLeftTime(NULL)
{
   int i;

   SetLabel(_("Selection Bar"));
   SetName(_("Selection Bar"));

   // This will be inherited by all children:
   SetFont(wxFont(9, wxSWISS, wxNORMAL, wxNORMAL));

   wxFlexGridSizer *mainSizer;
   wxBoxSizer *hSizer;

   wxString formatName = gPrefs->Read(wxT("/SelectionFormat"), wxT(""));
   int formatIndex = 1;
   for(i=0; i<TimeTextCtrl::GetNumBuiltins(); i++)
      if (TimeTextCtrl::GetBuiltinName(i) == formatName)
         formatIndex = i;
   formatName = TimeTextCtrl::GetBuiltinName(formatIndex);
   wxString format = TimeTextCtrl::GetBuiltinFormat(formatIndex);

   mainSizer = new wxFlexGridSizer(9);

   //
   // Top row (mostly labels)
   //

   mainSizer->Add(new wxStaticText(this, -1, _("Project Rate (Hz):")),
               0, wxALL | wxALIGN_CENTER_VERTICAL, 1);

   mainSizer->Add(20, 10);

   mainSizer->Add(new wxStaticText(this, -1, _("Selection Start:")),
               0, wxALL | wxALIGN_CENTER_VERTICAL, 1);

   mainSizer->Add(20, 10);

   bool showSelectionLength = false;
   gPrefs->Read(wxT("/ShowSelectionLength"), &showSelectionLength);
   
   hSizer = new wxBoxSizer(wxHORIZONTAL);
   mRightEndButton = new wxRadioButton(this, OnEndRadioID, _("End"),
                                       wxDefaultPosition, wxDefaultSize,
                                       wxRB_GROUP);
   mRightEndButton->SetValue(!showSelectionLength);
   hSizer->Add(mRightEndButton,
               1, wxALL | wxALIGN_CENTER_VERTICAL, 0);
   mRightLengthButton = new wxRadioButton(this, OnLengthRadioID, _("Length"));
   mRightLengthButton->SetValue(showSelectionLength);
   hSizer->Add(mRightLengthButton,
               1, wxALL | wxALIGN_CENTER_VERTICAL, 0);
   #if defined(__WXMSW__)
      // Refer to Microsoft KB article 261192 for an explanation as
      // to why this is needed.  We've only experienced it under Win2k
      // so it's probably been fixed.  But, it doesn't hurt to have this
      // in for all versions.
      wxRadioButton* dummyButton = 
         new wxRadioButton(this, wxID_ANY, _("hidden"),
                           wxDefaultPosition, wxDefaultSize,
                           wxRB_GROUP);
      dummyButton->Disable();
      dummyButton->Hide();
   #endif
   mainSizer->Add(hSizer, 0, wxALL, 1);

   mainSizer->Add(20, 10);

   wxStaticText *stat = new wxStaticText(this, -1, _("Audio Position:"));
   mainSizer->Add(stat,
                  0, wxALL | wxALIGN_CENTER_VERTICAL, 1);

   mainSizer->Add(20, 10);

   mainSizer->Add(20, 10);

   //
   // Middle row (mostly time controls)
   //

   mRateBox = new wxComboBox(this, OnRateID,
                             wxT(""),
                             wxDefaultPosition, wxSize(80, -1));
   mRateBox->SetName(_("Project Rate (Hz):"));
   mRateBox->SetValue(wxString::Format(wxT("%d"), (int)mRate));
   UpdateRates(); // Must be done _after_ setting value on mRateBox!

   mainSizer->Add(mRateBox, 0, wxALL | wxALIGN_CENTER_VERTICAL, 1);

#if __WXMSW__ /* As of wx 2.6.2, wxStaticLine is broken for Windows*/
   mainSizer->Add( 1, 1 );
#else
   mainSizer->Add(new wxStaticLine(this, -1, wxDefaultPosition, wxDefaultSize,
                                   wxLI_VERTICAL),
                  0, wxEXPAND | wxALL, 1);
#endif

   mLeftTime = new TimeTextCtrl(this, OnLeftTimeID, format, 0.0, mRate);
   mLeftTime->SetName(_("Selection Start:"));
   mLeftTime->EnableMenu();
   mainSizer->Add(mLeftTime, 0, wxALL | wxALIGN_CENTER_VERTICAL, 1);

#if __WXMSW__ /* As of wx 2.6.2, wxStaticLine is broken for Windows*/
   mainSizer->Add( 1, 1 );
#else
   mainSizer->Add(new wxStaticLine(this, -1, wxDefaultPosition, wxDefaultSize,
                                   wxLI_VERTICAL),
                  0, wxEXPAND | wxALL, 1);
#endif

   mRightTime = new TimeTextCtrl(this, OnRightTimeID, format, 0.0, mRate);
   mRightTime->SetName(wxString(_("Selection ")) + (showSelectionLength ?
                                                   _("Length") :
                                                   _("End")));
   mRightTime->EnableMenu();
   mainSizer->Add(mRightTime, 0, wxALL | wxALIGN_CENTER_VERTICAL, 1);

#if __WXMSW__ /* As of wx 2.6.2, wxStaticLine is broken for Windows*/
   mainSizer->Add( 1, 1 );
#else
   mainSizer->Add(new wxStaticLine(this, -1, wxDefaultPosition, wxDefaultSize,
                                   wxLI_VERTICAL),
                  0, wxEXPAND | wxALL, 1);
#endif

   mAudioTime = new TimeTextCtrl(this, -1, format, 0.0, mRate);
   mAudioTime->SetName(_("Audio Position:"));
   mAudioTime->EnableMenu();
   mainSizer->Add(mAudioTime, 0, wxALL | wxALIGN_CENTER_VERTICAL, 1);

   mainSizer->Add(20, 10);

   //
   // Bottom row (buttons)
   //

#if 0

   mainSizer->Add(20, 10);
   mainSizer->Add(20, 10);

   hSizer = new wxBoxSizer(wxHORIZONTAL);

   wxButton *b;
   b = new wxButton(this, -1, _("Label"),
                    wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);
   mainSizer->Add(b, 0, wxALL | wxALIGN_CENTER_VERTICAL, 1);
   mainSizer->Add(20, 10);
   b = new wxButton(this, -1, _("Label"),
                    wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);
   mainSizer->Add(b, 0, wxALL | wxALIGN_CENTER_VERTICAL, 1);
   mainSizer->Add(20, 10);
   b = new wxButton(this, -1, _("Label"),
                    wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);
   mainSizer->Add(b, 0, wxALL | wxALIGN_CENTER_VERTICAL, 1);

#endif
   
   SetAutoLayout(true);
   SetSizer(mainSizer);
   Layout();

   mMainSizer = mainSizer;
}

SelectionBar::~SelectionBar()
{
}

void SelectionBar::OnSize(wxSizeEvent &evt)
{
   Refresh( true );

   evt.Skip();
}

void SelectionBar::ModifySelection()
{
   mStart = mLeftTime->GetTimeValue();
   double right = mRightTime->GetTimeValue();

   if (mRightEndButton->GetValue()) {
      if(mStart > right)
         mEnd = mStart;
      else
         mEnd = right;
   }
   else
      mEnd = mStart + right;

   mModifyingSelection = true;
   mListener->AS_ModifySelection(mStart, mEnd);
   mModifyingSelection = false;
}

void SelectionBar::OnLeftTime(wxCommandEvent &evt)
{
   ModifySelection();
}

void SelectionBar::OnRightTime(wxCommandEvent &evt)
{
   ModifySelection();
}

void SelectionBar::OnLengthRadio(wxCommandEvent &evt)
{
   gPrefs->Write(wxT("/ShowSelectionLength"), true);
   mRightTime->SetName(wxString(_("Selection Length")));

   ValuesToControls();
}

void SelectionBar::OnEndRadio(wxCommandEvent &evt)
{
   gPrefs->Write(wxT("/ShowSelectionLength"), false);
   mRightTime->SetName(wxString(_("Selection End")));

   ValuesToControls();
}

void SelectionBar::OnUpdate(wxCommandEvent &evt)
{
   int index = evt.GetInt();
   wxString formatName =  TimeTextCtrl::GetBuiltinName(index);
   wxString formatString = TimeTextCtrl::GetBuiltinFormat(index);

   gPrefs->Write(wxT("/SelectionFormat"), formatName);

   mLeftTime->SetFormatString(formatString);
   mRightTime->SetFormatString(formatString);
   mAudioTime->SetFormatString(formatString);

   Layout();
   Refresh(false);

   evt.Skip(false);
}

void SelectionBar::ValuesToControls()
{
   mLeftTime->SetTimeValue(mStart);

   if (mRightEndButton->GetValue())
      mRightTime->SetTimeValue(mEnd);
   else
      mRightTime->SetTimeValue(mEnd - mStart);

   mAudioTime->SetTimeValue(mAudio);
}

void SelectionBar::SetTimes(double start, double end, double audio)
{
   if (mModifyingSelection) {
      // This event is directly a result of the user typing a change,
      // so we ignore it - otherwise every time they type it changes the
      // field out from under them!
      return;
   }

   mStart = start;
   mEnd = end;
   mAudio = audio;

   ValuesToControls();
}

double SelectionBar::GetLeftTime()
{
   return mLeftTime->GetTimeValue();
}

double SelectionBar::GetRightTime()
{
   if (mRightEndButton->GetValue())
      return mRightTime->GetTimeValue();
   else
      return mRightTime->GetTimeValue() + mLeftTime->GetTimeValue();
}

void SelectionBar::SetField(const wxChar *msg, int fieldNum)
{
   if (fieldNum < 0 || fieldNum >= 10)
      return;

   if (mField[fieldNum] != msg) {
      mField[fieldNum] = msg;
      Refresh(false);
   }
}

void SelectionBar::SetRate(double rate)
{
   if (rate != mRate) {
      mRate = rate;
      mRateBox->SetValue(wxString::Format(wxT("%d"), (int)rate));
      mLeftTime->SetSampleRate(rate);
      mRightTime->SetSampleRate(rate);
      mAudioTime->SetSampleRate(rate);
   }
}

void SelectionBar::OnRate(wxCommandEvent & WXUNUSED(event))
{
   mRateBox->GetValue().ToDouble(&mRate);
   if (mLeftTime) {
      mLeftTime->SetSampleRate(mRate);
      mRightTime->SetSampleRate(mRate);
      mAudioTime->SetSampleRate(mRate);
      mListener->AS_SetRate(mRate);
   }
}

void SelectionBar::UpdateRates()
{
   wxArrayLong rates = AudioIO::GetSupportedSampleRates();
   wxString oldValue = mRateBox->GetValue();
   mRateBox->Clear();
   if (rates.GetCount() < 25) {
      unsigned int i;
      for(i=0; i<rates.GetCount(); i++)
         mRateBox->Append(wxString::Format(wxT("%d"), (int)rates[i]));
   }
   else {
      if (rates.Index(8000) != wxNOT_FOUND) mRateBox->Append(wxT("8000"));
      if (rates.Index(11025) != wxNOT_FOUND) mRateBox->Append(wxT("11025"));
      if (rates.Index(16000) != wxNOT_FOUND) mRateBox->Append(wxT("16000"));
      if (rates.Index(22050) != wxNOT_FOUND) mRateBox->Append(wxT("22050"));
      if (rates.Index(44100) != wxNOT_FOUND) mRateBox->Append(wxT("44100"));
      if (rates.Index(48000) != wxNOT_FOUND) mRateBox->Append(wxT("48000"));
      if (rates.Index(96000) != wxNOT_FOUND) mRateBox->Append(wxT("96000"));
   }
   mRateBox->SetValue(oldValue);
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
// arch-tag: 147df354-77ae-4620-a8e1-9598a695548b

