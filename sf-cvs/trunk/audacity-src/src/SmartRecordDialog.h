/**********************************************************************

  Audacity: A Digital Audio Editor

  SmartRecordDialog.h

  Copyright 2006 by Vaughan Johnson
  
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

**********************************************************************/

#ifndef __AUDACITY_SMARTRECORD_DIALOG__
#define __AUDACITY_SMARTRECORD_DIALOG__

#include <wx/dialog.h>
#include <wx/datectrl.h>
#include <wx/calctrl.h>

#include "widgets/TimeTextCtrl.h"

#include "ShuttleGui.h"

class SmartRecordDialog : public wxDialog
{
public:
   SmartRecordDialog(wxWindow* parent);
   ~SmartRecordDialog();

   void OnTimer(wxTimerEvent& event);

private:
   void OnDatePicker_Start(wxDateEvent& event);
   void OnTimeText_Start(wxCommandEvent& event);

   void OnDatePicker_End(wxDateEvent& event);
   void OnTimeText_End(wxCommandEvent& event);

   void OnTimeText_Duration(wxCommandEvent & event);

   void OnOK(wxCommandEvent& event);

   void PopulateOrExchange(ShuttleGui& S);
   bool TransferDataFromWindow();
   void UpdateDuration(); // Update m_TimeSpan_Duration and ctrl based on m_DateTime_Start and m_DateTime_End.
   void UpdateEnd(); // Update m_DateTime_End and ctrls based on m_DateTime_Start and m_TimeSpan_Duration.
   bool WaitForStart();

private:
   wxDateTime m_DateTime_Start;
   wxDateTime m_DateTime_End;
   wxTimeSpan m_TimeSpan_Duration;

   // controls
   wxDatePickerCtrl* m_pDatePickerCtrl_Start;
   TimeTextCtrl* m_pTimeTextCtrl_Start;

   wxDatePickerCtrl* m_pDatePickerCtrl_End;
   TimeTextCtrl* m_pTimeTextCtrl_End;

   TimeTextCtrl* m_pTimeTextCtrl_Duration;

   wxTimer m_timer;

   DECLARE_EVENT_TABLE();
};

#endif
