/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApp.h

  Dominic Mazzoni

  This is the main source file for Audacity which handles
  initialization and termination by subclassing wxApp.

**********************************************************************/

#include <wx/app.h>

class wxLocale;

void QuitAudacity();

extern wxFrame *gParentFrame;

class AudacityApp:public wxApp {
 public:
   bool OnInit(void);
   int OnExit(void);

   void OnKey(wxKeyEvent& event);
   
   // These are currently only used on Mac OS, where it's
   // possible to have a menu bar but no windows open.  It doesn't
   // hurt any other platforms, though.
   // STM: Yes, it does hurt.  I'm not sure why
#ifdef __WXMAC__
   void OnMenuAbout(wxCommandEvent & event);
   void OnMenuNew(wxCommandEvent & event);
   void OnMenuOpen(wxCommandEvent & event);
   void OnMenuPreferences(wxCommandEvent & event);
   void OnMenuExit(wxCommandEvent & event);
#endif
   
 private:
   wxLocale *mLocale;
//   wxSingleInstanceChecker *mChecker;

 public:
    DECLARE_EVENT_TABLE()
};

extern AudacityApp & wxGetApp();
