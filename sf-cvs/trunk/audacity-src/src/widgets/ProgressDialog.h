/**********************************************************************

  Audacity: A Digital Audio Editor

  ProgressDialog.h
  
  Leland Lucius

*************************************************************************/

#ifndef __AUDACITY_WIDGETS_PROGRESSDIALOG__
#define __AUDACITY_WIDGETS_PROGRESSDIALOG__

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/dialog.h>
#include <wx/gauge.h>
#include <wx/stattext.h>
#include <wx/utils.h>

enum
{
   eProgressCancelled = 0, //<! User says that whatever is happening is undesirable and shouldn't have happened at all
   eProgressSuccess,       //<! User says nothing, everything works fine, continue doing whatever we're doing
   eProgressFailed,        //<! Something has gone wrong, we should stop and cancel everything we did
   eProgressStopped        //<! Nothing is wrong, but user says we should stop now and leave things as they are now
};

enum ProgressDialogFlags
{
   pdlgEmptyFlags = 0x00000000,
   pdlgHideStopButton = 0x00000001,
   pdlgHideCancelButton = 0x00000002
} ;

////////////////////////////////////////////////////////////
/// ProgressDialog Class
////////////////////////////////////////////////////////////

class AUDACITY_DLL_API ProgressDialog:public wxDialog
{

 public:

   ProgressDialog(const wxString & title, const wxString & message = wxEmptyString, ProgressDialogFlags flags = pdlgEmptyFlags);
   virtual ~ProgressDialog();

   bool Show(bool show = true);

   int Update(int value, const wxString & message = wxEmptyString);
   int Update(double current, const wxString & message = wxEmptyString);
   int Update(double current, double total, const wxString & message = wxEmptyString);
   int Update(wxULongLong_t current, wxULongLong_t total, const wxString & message = wxEmptyString);
   int Update(wxLongLong current, wxLongLong total, const wxString & message = wxEmptyString);
   int Update(wxLongLong_t current, wxLongLong_t total, const wxString & message = wxEmptyString);
   int Update(int current, int total, const wxString & message = wxEmptyString);
   void SetMessage(const wxString & message);

 private:
   void OnCancel(wxCommandEvent & e);
   void OnStop(wxCommandEvent & e);
   void Beep();

 private:
   wxWindow *mHadFocus;
   wxGauge *mGauge;
   wxStaticText *mMessage;
   wxStaticText *mElapsed;
   wxStaticText *mRemaining;
   wxWindowDisabler *mDisable;

   wxLongLong_t mStartTime;
   wxLongLong_t mLastTime;
   wxLongLong_t mLastUpdate;
   int mLastValue;

   bool mCancel;
   bool mStop;
   
   DECLARE_EVENT_TABLE();
};

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 2f4ec75c-bdb7-4889-96d1-5d00abc41027
