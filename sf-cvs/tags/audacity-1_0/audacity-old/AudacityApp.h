/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApp.h

  Dominic Mazzoni

  This is the main source file for Audacity which handles
  initialization and termination by subclassing wxApp.

**********************************************************************/

#include <wx/app.h>

void QuitAudacity();

extern wxFrame *gParentFrame;

class AudacityApp:public wxApp {
 public:
   bool OnInit(void);
   int OnExit(void);
   
   void OnMenuAbout(wxCommandEvent & event);
   void OnMenuNew(wxCommandEvent & event);
   void OnMenuOpen(wxCommandEvent & event);
   void OnMenuPreferences(wxCommandEvent & event);
   void OnMenuExit(wxCommandEvent & event);

 private:
   void RunTest();

#ifdef __WXMSW__
   void *mSingleInstanceMutex;
#endif

#if 0
   wxSingleInstanceChecker *mChecker;
#endif

public:
    DECLARE_EVENT_TABLE()

};

extern AudacityApp & wxGetApp();
