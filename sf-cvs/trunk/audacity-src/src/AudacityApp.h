/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApp.h

  Dominic Mazzoni

  This is the main source file for Audacity which handles
  initialization and termination by subclassing wxApp.

**********************************************************************/

#ifndef __AUDACITY_APP__
#define __AUDACITY_APP__

#include <wx/app.h>
#include "ToolBar.h"

class wxLocale;
class wxSingleInstanceChecker;
class ToolBarStub;

void SaveWindowSize();

void QuitAudacity(bool bForce);
void QuitAudacity();

extern wxFrame *gParentFrame;

extern bool gIsQuitting;

class AudacityApp:public wxApp {
 public:
   virtual bool OnInit(void);
   virtual int OnExit(void);

//LDA - Until we have a better way to save/restore binary data.
   float* GetCleanSpeechNoiseGate() { return ms_CleanSpeechNoiseGate; }
   int    GetCleanSpeechNoiseGateExpectedCount() { return ms_CleanSpeechNoiseGateExpectedCount; }
   void   SetCleanSpeechNoiseGate(float* pNG) { ms_CleanSpeechNoiseGate = pNG; }
   void   SetCleanSpeechNoiseGateExpectedCount(int count) { ms_CleanSpeechNoiseGateExpectedCount = count; }

   virtual int FilterEvent(wxEvent& event);

   int OnAllKeys(wxKeyEvent& event);

   // These are currently only used on Mac OS, where it's
   // possible to have a menu bar but no windows open.  It doesn't
   // hurt any other platforms, though.
   void OnMenuAbout(wxCommandEvent & event);
   void OnMenuNew(wxCommandEvent & event);
   void OnMenuOpen(wxCommandEvent & event);
   void OnMenuPreferences(wxCommandEvent & event);
   void OnMenuExit(wxCommandEvent & event);
   void OnKeyEvent(wxKeyEvent & event );
   
   ToolBarStub * LoadToolBar( const wxString Name, bool bDefault, 
      wxWindow * pParent, enum ToolBarType tbt );
   // Most Recently Used File support (for all platforms).
   void OnMRUFile(wxCommandEvent &event);
// void OnMRUProject(wxCommandEvent &event);
   // Backend for above - returns true for success, false for failure
   bool MRUOpen(wxString fileName);

   #ifdef __WXMAC__
    // In response to Apple Events
    virtual void MacOpenFile(const wxString &fileName) ;
    virtual void MacPrintFile(const wxString &fileName) ;
    virtual void MacNewFile() ;
    virtual void MacReopenApp() ;
   #endif

	#if defined(__WXMSW__) && !defined(__WXUNIVERSAL__) && !defined(__CYGWIN__)
   void AssociateFileTypes(); 
	#endif

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

   wxSingleInstanceChecker *mChecker;

   bool InitTempDir();
   bool CreateSingleInstanceChecker(wxString dir);

//LDA - Until we have a better way to save/restore binary data.
   bool InitCleanSpeech();
   float* ms_CleanSpeechNoiseGate;
   int    ms_CleanSpeechNoiseGateExpectedCount;

//LDA - Keep track of where Presets are stored
   wxString mAppHomeDir; //lda
   wxString mPresetsDir; //lda

 public:
    DECLARE_EVENT_TABLE()
};

extern AudacityApp & wxGetApp();

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
// arch-tag: 31e7d5f1-bd9e-4348-bce1-6921effbd8e5
