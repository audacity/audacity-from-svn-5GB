/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeDialog.cpp

  Dominic Mazzoni

*******************************************************************//**

\class TimeDialog
\brief Dialog used to request a time value.

*//*******************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include "TimeDialog.h"

BEGIN_EVENT_TABLE(TimeDialog, wxDialog)
   EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, TimeDialog::OnUpdate)
END_EVENT_TABLE()

TimeDialog::TimeDialog(wxWindow *parent,
                               wxWindowID id,
                               const wxString &title,
                               const wxPoint &pos,
                               const wxSize &size,
                               long style,
                               const wxString &name)
: wxDialog(parent, id, title, pos, size, style, name),
  mFormat(wxT("seconds")),
  mRate(44100),
  mTime(0.0),
  mTimeCtrl(NULL)
{
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

void TimeDialog::PopulateOrExchange(ShuttleGui &S)
{
   S.SetBorder(5);
   S.StartVerticalLay(true);
   {
      S.StartStatic(_("Specify Time"), true);
      {
         mTimeCtrl = new
            TimeTextCtrl(this,
                         wxID_ANY,
                         TimeTextCtrl::GetBuiltinFormat(mFormat),
                         mTime,
                         mRate,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
         S.AddWindow(mTimeCtrl);
         mTimeCtrl->EnableMenu();
      }
      S.EndStatic();
   }
   S.EndVerticalLay();
   GetSizer()->Add(CreateButtonSizer(wxOK | wxCANCEL),
                   0,
                   wxCENTER | wxBOTTOM,
                   10);

   TransferDataToWindow();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();
}

bool TimeDialog::TransferDataToWindow()
{
   mTimeCtrl->SetFormatString(TimeTextCtrl::GetBuiltinFormat(mFormat));
   mTimeCtrl->SetSampleRate(mRate);
   mTimeCtrl->SetTimeValue(mTime);
   mTimeCtrl->SetFocus();

   return true;
}

bool TimeDialog::TransferDataFromWindow()
{
   mTime = mTimeCtrl->GetTimeValue();

   return true;
}

const double TimeDialog::GetTimeValue()
{
   return mTime;
}

void TimeDialog::SetFormatString(wxString formatString)
{
   mFormat = formatString;
   TransferDataToWindow();
}

void TimeDialog::SetSampleRate(double sampleRate)
{
   mRate = sampleRate;
   TransferDataToWindow();
}

void TimeDialog::SetTimeValue(double newTime)
{
   mTime = newTime;
   TransferDataToWindow();
}

void TimeDialog::OnUpdate(wxCommandEvent &event)
{
   Layout();
   Refresh();

   event.Skip(false);
}
