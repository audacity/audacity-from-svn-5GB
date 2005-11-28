/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeTextCtrl.h

  Dominic Mazzoni

  See TimeTextCtrl.cpp for documentation on how to use the
  format string to specify how a TimeTextCtrl's fields are
  laid out.

**********************************************************************/

#ifndef __AUDACITY_TIME_TEXT_CTRL__
#define __AUDACITY_TIME_TEXT_CTRL__

#include <wx/defs.h>
#include <wx/event.h>
#include <wx/panel.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textctrl.h>

class TimeField;
WX_DECLARE_OBJARRAY(TimeField, TimeFieldArray);

class TimeTextCtrl: public wxPanel
{
 public:
   TimeTextCtrl(wxWindow *parent,
                wxWindowID id,
                wxString formatString = wxT(""),
                double timeValue = 0.0,
                double sampleRate = 44100,
                const wxPoint &pos = wxDefaultPosition,
                const wxSize &size = wxDefaultSize);

   bool HasAnyFocus();

   void SetFormatString(wxString formatString);
   void SetSampleRate(double sampleRate);
   void SetTimeValue(double newTime);
   const double GetTimeValue();

   wxString GetTimeString();

   static int  GetNumBuiltins();
   static wxString GetBuiltinName(int index);
   static wxString GetBuiltinFormat(int index);

 private:

   void OnText(wxCommandEvent &event);

   void ParseFormatString();

   void DeleteControls();
   void CreateControls();

   void ValueToControls();
   void ControlsToValue();

   void ComputeTextExtents();

   void PrintDebugInfo();

   double         mTimeValue;
   double         mSampleRate;
   wxString       mFormatString;

   bool           mModifyingText;

   wxString       mPrefix;
   wxStaticText  *mPrefixStaticText;

   TimeFieldArray mWholeFields;
   TimeFieldArray mFracFields;
   double         mScalingFactor;

   static int     sTextWidth[11];
   static int     sTextHeight;

   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_TIME_TEXT_CTRL__

// Indentation settings for Vim and Emacs.
// Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// 

