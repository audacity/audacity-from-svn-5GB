/**********************************************************************

  Audacity: A Digital Audio Editor

  SelectionBar.cpp

  Copyright 2005 Dominic Mazzoni
  
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

**********************************************************************/

#include "SelectionBar.h"

#include "AudioIO.h"
#include "AColor.h"
#include "Prefs.h"

#include "widgets/TimeTextCtrl.h"

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/combobox.h>
#include <wx/intl.h>
#include <wx/menu.h>
#include <wx/msgdlg.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/statline.h>
#include <wx/textdlg.h>

#define FORMAT_CHOICE_IS_COMBO_BOX 0

enum {
   SelectionBarFirstID = 2700,
   OnRateID,
   OnLengthRadioID,
   OnEndRadioID,
   OnFormatChoiceID,
   OnLeftTimeID,
   OnRightTimeID
};

BEGIN_EVENT_TABLE(SelectionBar, wxPanel)
   EVT_TEXT(OnLeftTimeID, SelectionBar::OnLeftTime)
   EVT_TEXT(OnRightTimeID, SelectionBar::OnRightTime)
   EVT_RADIOBUTTON(OnLengthRadioID, SelectionBar::OnLengthRadio)
   EVT_RADIOBUTTON(OnEndRadioID, SelectionBar::OnEndRadio)
   EVT_COMBOBOX(OnRateID, SelectionBar::OnRate)
   EVT_TEXT(OnRateID, SelectionBar::OnRate)
  #if FORMAT_CHOICE_IS_COMBO_BOX
   EVT_COMBOBOX(OnFormatChoiceID, SelectionBar::OnFormatChoice)
   EVT_TEXT(OnFormatChoiceID, SelectionBar::OnFormatChoice)
  #else
   EVT_CHOICE(OnFormatChoiceID, SelectionBar::OnFormatChoice)
  #endif
END_EVENT_TABLE()

SelectionBar::SelectionBar(wxWindow * parent, wxWindowID id,
                           const wxPoint & pos,
                           const wxSize & size,
                           double rate,
                           SelectionBarListener * listener):
   wxPanel(parent, id, pos,  size, wxFULL_REPAINT_ON_RESIZE),
   mListener(listener), mRate(rate),
   mStart(0.0), mEnd(0.0), mAudio(0.0),
   mModifyingSelection(false)
{
   // This will be inherited by all children:
   SetFont(wxFont(9, wxSWISS, wxNORMAL, wxNORMAL));

   wxFlexGridSizer *mainSizer;
   wxBoxSizer *hSizer;
   int i;

   wxString format = TimeTextCtrl::GetBuiltinFormat(1);

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
   mRightEndButton = new wxRadioButton(this, OnEndRadioID, _("End:"),
                                       wxDefaultPosition, wxDefaultSize,
                                       wxRB_GROUP);
   mRightEndButton->SetValue(!showSelectionLength);
   hSizer->Add(mRightEndButton,
               1, wxALL | wxALIGN_CENTER_VERTICAL, 0);
   mRightLengthButton = new wxRadioButton(this, OnLengthRadioID, _("Length:"));
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

   mLeftTime = new TimeTextCtrl(this, OnLeftTimeID, format, 0.0, 44100.0);
   mainSizer->Add(mLeftTime, 0, wxALL | wxALIGN_CENTER_VERTICAL, 1);

#if __WXMSW__ /* As of wx 2.6.2, wxStaticLine is broken for Windows*/
   mainSizer->Add( 1, 1 );
#else
   mainSizer->Add(new wxStaticLine(this, -1, wxDefaultPosition, wxDefaultSize,
                                   wxLI_VERTICAL),
                  0, wxEXPAND | wxALL, 1);
#endif

   mRightTime = new TimeTextCtrl(this, OnRightTimeID, format, 0.0, 44100.0);
   mainSizer->Add(mRightTime, 0, wxALL | wxALIGN_CENTER_VERTICAL, 1);

#if __WXMSW__ /* As of wx 2.6.2, wxStaticLine is broken for Windows*/
   mainSizer->Add( 1, 1 );
#else
   mainSizer->Add(new wxStaticLine(this, -1, wxDefaultPosition, wxDefaultSize,
                                   wxLI_VERTICAL),
                  0, wxEXPAND | wxALL, 1);
#endif

   mAudioTime = new TimeTextCtrl(this, -1, format, 0.0, 44100.0);
   mainSizer->Add(mAudioTime, 0, wxALL | wxALIGN_CENTER_VERTICAL, 1);

   mainSizer->Add(20, 10);

   mFormatChoice = NULL;

   wxString *choices = new wxString[TimeTextCtrl::GetNumBuiltins()];
   for(i=0; i<TimeTextCtrl::GetNumBuiltins(); i++)
      choices[i] = TimeTextCtrl::GetBuiltinName(i);

  #if FORMAT_CHOICE_IS_COMBO_BOX
   wxComboBox *box = new wxComboBox(this, OnFormatChoiceID, wxT(""),
                                    wxDefaultPosition, wxDefaultSize,
                                    TimeTextCtrl::GetNumBuiltins(),
                                    choices);
   box->SetWindowStyle(wxCB_READONLY);
   box->SetValue(TimeTextCtrl::GetBuiltinName(1));
   mFormatChoice = box;
  #else
   wxChoice *choice = new wxChoice(this, OnFormatChoiceID, 
                                    wxDefaultPosition, wxDefaultSize,
                                    TimeTextCtrl::GetNumBuiltins(),
                                    choices);
   choice->SetSelection(1);
   mFormatChoice = choice;   
  #endif
   delete [] choices;

   mainSizer->Add(mFormatChoice, 0, wxALL | wxALIGN_CENTER_VERTICAL, 1);

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

   //
   // Intercept focus events to include track panel in tab nagivation
   // (really should get real deal working...)
   //
   mRightEndButton->Connect( wxEVT_KILL_FOCUS, wxFocusEventHandler(SelectionBar::OnSetFocus));
   mRightLengthButton->Connect( wxEVT_KILL_FOCUS, wxFocusEventHandler(SelectionBar::OnSetFocus));
   mFormatChoice->Connect( wxEVT_KILL_FOCUS, wxFocusEventHandler(SelectionBar::OnKillFocus));
}

SelectionBar::~SelectionBar()
{
   mRightEndButton->Disconnect( wxEVT_KILL_FOCUS, wxFocusEventHandler(SelectionBar::OnSetFocus));
   mRightLengthButton->Disconnect( wxEVT_KILL_FOCUS, wxFocusEventHandler(SelectionBar::OnSetFocus));
   mFormatChoice->Disconnect( wxEVT_KILL_FOCUS, wxFocusEventHandler(SelectionBar::OnKillFocus));
}

bool SelectionBar::HasAnyFocus()
{
   wxWindow *focus = FindFocus();
   
   if (focus == this ||
       focus == mRightEndButton ||
       focus == mRightLengthButton ||
       focus == mFormatChoice ||
       focus == mRateBox)
      return true;

   if (mLeftTime->HasAnyFocus())
      return true;

   if (mRightTime->HasAnyFocus())
      return true;

   if (mAudioTime->HasAnyFocus())
      return true;

   return false;
}

void SelectionBar::ModifySelection()
{
   mStart = mLeftTime->GetTimeValue();
   double right = mRightTime->GetTimeValue();

   if (mRightEndButton->GetValue())
      mEnd = right;
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

   ValuesToControls();
}

void SelectionBar::OnEndRadio(wxCommandEvent &evt)
{
   gPrefs->Write(wxT("/ShowSelectionLength"), false);

   ValuesToControls();
}

void SelectionBar::OnFormatChoice(wxCommandEvent &evt)
{
   if (!mFormatChoice)
      return;

   #if FORMAT_CHOICE_IS_COMBO_BOX
   int index=0;
   wxString value = ((wxComboBox *)mFormatChoice)->GetValue();
   for(index=0; index<TimeTextCtrl::GetNumBuiltins(); index++)
      if (value == TimeTextCtrl::GetBuiltinName(index))
         break;
   if (index == TimeTextCtrl::GetNumBuiltins())
      return;
   #else
   int index = ((wxChoice *)mFormatChoice)->GetSelection();
   #endif

   wxString formatString = TimeTextCtrl::GetBuiltinFormat(index);

   mLeftTime->SetFormatString(formatString);
   mRightTime->SetFormatString(formatString);
   mAudioTime->SetFormatString(formatString);

   Layout();
   Refresh(false);
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
   }
}

void SelectionBar::OnRate(wxCommandEvent & WXUNUSED(event))
{
   mRateBox->GetValue().ToDouble(&mRate);
   mListener->AS_SetRate(mRate);
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


void SelectionBar::OnSetFocus(wxFocusEvent &evt)
{
   SelectionBar *sb = (SelectionBar *)((wxWindow *)evt.GetEventObject())->GetParent();
   wxWindow *n = evt.GetWindow();

   if (n == sb->mFormatChoice)
   {
      sb->mListener->AS_GiveFocus(false);
   }

   evt.Skip(true);
}

void SelectionBar::OnKillFocus(wxFocusEvent &evt)
{
   SelectionBar *sb = (SelectionBar *)((wxWindow *)evt.GetEventObject())->GetParent();
   wxWindow *n = evt.GetWindow();

   if ((n == sb->mRightEndButton) || (n == sb->mRightLengthButton))
   {
      sb->mListener->AS_GiveFocus(true);
   }

   evt.Skip(true);
}

void SelectionBar::TakeFocus(bool bForward)
{
   if (bForward)
   {
      SetFocus();
   }
   else
   {
      mFormatChoice->SetFocus();
   }
  
   return;
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

