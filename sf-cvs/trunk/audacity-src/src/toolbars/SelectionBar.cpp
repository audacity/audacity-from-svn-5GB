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


#include "../Audacity.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/button.h>
#include <wx/combobox.h>
#include <wx/intl.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/statline.h>
#include <wx/valtext.h>
#endif

#include "SelectionBar.h"

#include "../AudacityApp.h"
#include "../AudioIO.h"
#include "../AColor.h"
#include "../Prefs.h"
#include "../widgets/TimeTextCtrl.h"

IMPLEMENT_CLASS(SelectionBar, ToolBar);

enum {
   SelectionBarFirstID = 2700,
   OnRateID,
   OnLengthRadioID,
   OnEndRadioID,
   OnLeftTimeID,
   OnRightTimeID
};

BEGIN_EVENT_TABLE(SelectionBar, ToolBar)
   EVT_SIZE(SelectionBar::OnSize)
   EVT_TEXT(OnLeftTimeID, SelectionBar::OnLeftTime)
   EVT_TEXT(OnRightTimeID, SelectionBar::OnRightTime)
   EVT_RADIOBUTTON(OnLengthRadioID, SelectionBar::OnLengthRadio)
   EVT_RADIOBUTTON(OnEndRadioID, SelectionBar::OnEndRadio)
   EVT_COMBOBOX(OnRateID, SelectionBar::OnRate)
   EVT_TEXT(OnRateID, SelectionBar::OnRate)
   EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, SelectionBar::OnUpdate)
   EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, SelectionBar::OnRateCaptureKey)
END_EVENT_TABLE()

SelectionBar::SelectionBar()
: ToolBar(SelectionBarID, _("Selection"), wxT("Selection")),
  mStart(0.0), mEnd(0.0), mAudio(0.0),
  mModifyingSelection(false), mLeftTime(NULL)  
{
}

SelectionBar::~SelectionBar()
{
}

void SelectionBar::Create(wxWindow * parent)
{
   ToolBar::Create(parent);
}

void SelectionBar::Populate()
{
   int i;

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

   mainSizer = new wxFlexGridSizer(8);
   Add(mainSizer, 0, wxALIGN_CENTER_VERTICAL);

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

   mainSizer->Add(2, 10);

   //
   // Middle row (mostly time controls)
   //

   mRateBox = new wxComboBox(this, OnRateID,
                             wxT(""),
                             wxDefaultPosition, wxSize(80, -1));
   mRateBox->SetName(_("Project Rate (Hz):"));
   mRateBox->SetValidator(wxTextValidator(wxFILTER_NUMERIC));
   mRateBox->SetValue(wxString::Format(wxT("%d"), (int)mRate));
   UpdateRates(); // Must be done _after_ setting value on mRateBox!

   // We need to capture the SetFocus and KillFocus events to set up
   // for keyboard capture.  On Windows and GTK it's easy since the
   // combobox is presented as one control to hook into.
   wxWindow *ctrl = mRateBox;

#if defined(__WXMAC__)
   // The Mac uses a standard wxTextCtrl for the edit portion and that's
   // the control that gets the focus events.  So we have to find the
   // textctrl.
   wxWindowList kids = mRateBox->GetChildren();
   for (unsigned int i = 0; i < kids.GetCount(); i++) {
      wxClassInfo *ci = kids[i]->GetClassInfo();
      if (ci->IsKindOf(CLASSINFO(wxTextCtrl))) {
         ctrl = kids[i];
         break;
      }
   }
#endif

   ctrl->Connect(wxEVT_SET_FOCUS,
                 wxFocusEventHandler(SelectionBar::OnRateFocus),
                 NULL,
                 this);
   ctrl->Connect(wxEVT_KILL_FOCUS,
                 wxFocusEventHandler(SelectionBar::OnRateFocus),
                 NULL,
                 this);

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

   mainSizer->Layout();

   Layout();

   SetMinSize( GetSizer()->GetMinSize() );
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
   wxWindow *w = FindFocus();
   bool leftFocus = (w == mLeftTime);
   bool rightFocus = (w == mRightTime);
   bool audioFocus = (w == mAudioTime);
   
   evt.Skip(false);

   wxString formatName =  TimeTextCtrl::GetBuiltinName(index);
   wxString formatString = TimeTextCtrl::GetBuiltinFormat(index);

   gPrefs->Write(wxT("/SelectionFormat"), formatName);

   // ToolBar::ReCreateButtons() will get rid of our sizers and controls
   // so reset pointers first.
   mLeftTime =
   mRightTime =
   mAudioTime = NULL;

   mRightEndButton =
   mRightLengthButton = NULL;

   mRateBox = NULL;

   ToolBar::ReCreateButtons();

   ValuesToControls();

   if (leftFocus) {
      mLeftTime->SetFocus();
   }
   else if (rightFocus) {
      mRightTime->SetFocus();
   }
   else if (audioFocus) {
      mAudioTime->SetFocus();
   }

   Updated();
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
   if (mRate != 0.0 && mLeftTime) {
      mLeftTime->SetSampleRate(mRate);
      mRightTime->SetSampleRate(mRate);
      mAudioTime->SetSampleRate(mRate);
      mListener->AS_SetRate(mRate);
   }
}

void SelectionBar::UpdateRates()
{
   wxString oldValue = mRateBox->GetValue();
   mRateBox->Clear();
   for (int i = 0; i < AudioIO::NumStandardRates; i++) {
      mRateBox->Append(wxString::Format(wxT("%d"), AudioIO::StandardRates[i]));
   }
   mRateBox->SetValue(oldValue);
}

void SelectionBar::OnRateFocus(wxFocusEvent &event)
{
   wxCommandEvent e(EVT_CAPTURE_KEYBOARD);

   if (event.GetEventType() == wxEVT_KILL_FOCUS) {
      e.SetEventType(EVT_RELEASE_KEYBOARD);
   }
   e.SetEventObject(this);
   GetParent()->GetEventHandler()->ProcessEvent(e);

   Refresh(false);

   event.Skip();
}

void SelectionBar::OnRateCaptureKey(wxCommandEvent &event)
{
   wxKeyEvent *kevent = (wxKeyEvent *)event.GetEventObject();
   int keyCode = kevent->GetKeyCode();

   // Convert numeric keypad entries.
   if ((keyCode >= WXK_NUMPAD0) && (keyCode <= WXK_NUMPAD9))
      keyCode -= WXK_NUMPAD0 - '0';

   if (keyCode >= '0' && keyCode <= '9')
      return;

   event.Skip();

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

