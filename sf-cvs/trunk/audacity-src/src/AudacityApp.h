/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApp.h

  Dominic Mazzoni

  This is the main source file for Audacity which handles
  initialization and termination by subclassing wxApp.

**********************************************************************/

#include <wx/app.h>

class wxLocale;

void QuitAudacity(bool bForce);
void QuitAudacity();

extern wxFrame *gParentFrame;

extern bool gIsQuitting;

class AudacityApp:public wxApp {
 public:
   bool OnInit(void);
   int OnExit(void);

   int FilterEvent(wxEvent& event);

   int OnAllKeys(wxKeyEvent& event);

   // These are currently only used on Mac OS, where it's
   // possible to have a menu bar but no windows open.  It doesn't
   // hurt any other platforms, though.
   void OnMenuAbout(wxCommandEvent & event);
   void OnMenuNew(wxCommandEvent & event);
   void OnMenuOpen(wxCommandEvent & event);
   void OnMenuPreferences(wxCommandEvent & event);
   void OnMenuExit(wxCommandEvent & event);

   // A list of directories that should be searched
   // for Audacity files (plug-ins, help files, etc.).  On Unix
   // this will include the directory Audacity was installed into,
   // plus the current user's .audacity-files directory.  Additional
   // directories can be specified using the AUDACITY_PATH environment
   // variable.  On Windows or Mac OS, this will include the directory
   // which contains the Audacity program.  
   wxArrayString audacityPathList;

   // Default temp dir
   wxString defaultTempDir;

   // Useful functions for working with search paths
   static void AddUniquePathToPathList(wxString path,
                                       wxArrayString &pathList);
   static void AddMultiPathsToPathList(wxString multiPathString,
                                       wxArrayString &pathList);
   static void FindFilesInPathList(wxString pattern,
                                   wxArrayString pathList,
                                   int flags, // wxFILE, wxDIR, or 0
                                   wxArrayString &results);

 private:
   wxLocale *mLocale;

#ifdef __WXMSW__
   void *mSingleInstanceMutex;
#endif

 public:
    DECLARE_EVENT_TABLE()
};

extern AudacityApp & wxGetApp();
