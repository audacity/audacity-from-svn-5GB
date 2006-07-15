/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeDialog.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TimeDialog__
#define __AUDACITY_TimeDialog__

#include <wx/defs.h>
#include <wx/dialog.h>
#include <wx/event.h>
#include <wx/string.h>

#include "widgets/TimeTextCtrl.h"

class ShuttleGui;

class TimeDialog:public wxDialog
{
 public:

   TimeDialog(wxWindow *parent,
                  wxWindowID id,
                  const wxString &title,
                  const wxPoint &pos = wxDefaultPosition,
                  const wxSize &size = wxDefaultSize,
                  long style = wxDEFAULT_DIALOG_STYLE,
                  const wxString &name = wxDialogNameStr);

   void SetFormatString(wxString formatString);
   void SetSampleRate(double sampleRate);
   void SetTimeValue(double newTime);
   const double GetTimeValue();

 private:

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

   void OnUpdate(wxCommandEvent &event);

 private:

   wxString mFormat;
   double mRate;
   double mTime;

   TimeTextCtrl *mTimeCtrl;

   DECLARE_EVENT_TABLE();
};

#endif
