/**********************************************************************

  Audacity: A Digital Audio Editor

  PrefsDialog.cpp

  Joshua Haberman
  James Crook

*******************************************************************//**

\class PrefsDialog
\brief Dialog that shows the current PrefsPanel in a tabbed divider.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/event.h>
#include <wx/font.h>
#include <wx/gdicmn.h>
#include <wx/intl.h>
#include <wx/listbox.h>
#include <wx/imaglist.h>
#include <wx/msgdlg.h>
#include <wx/notebook.h>
//#include <wx/choicebk.h>
#include <wx/listbook.h>
#include <wx/listctrl.h>
#include <wx/sizer.h>

#if wxCHECK_VERSION(2, 8, 4)
#include <wx/treebook.h>
#else
#include "../widgets/treebook.h"
#endif

#include "../Project.h"

#include "../Prefs.h"

#include "PrefsDialog.h"
#include "PrefsPanel.h"

#include "AudioIOPrefs.h"
#include "SmartRecordPrefs.h"
#include "DirectoriesPrefs.h"
#include "FileFormatPrefs.h"
#include "GUIPrefs.h"
#include "ThemePrefs.h"
#include "BatchPrefs.h"
#include "KeyConfigPrefs.h"
#include "QualityPrefs.h"
#include "SpectrumPrefs.h"
#include "MousePrefs.h"
#include "../Experimental.h"

//JKC: Experimental treebook, backported from wxWidgets 2.7.x
//#include "../widgets/treebook.h"


enum {
   CategoriesID = 1000
};

BEGIN_EVENT_TABLE(PrefsDialog, wxDialog)
   EVT_BUTTON(wxID_OK, PrefsDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, PrefsDialog::OnCancel)
END_EVENT_TABLE()

bool gPrefsDialogVisible = false;

// wxListBook doesn't let you use report mode for the list.
// We don't want images, we do want a single column, 
// so we work around the limitations by defining a thin and empty image.
static const char * TrialImage[] = {
"64 1 3 1",
"+	c #004010",
".	c None", // mask color = RGB:255,0,0
"#	c #00AA30",
"................................................................"};



PrefsDialog::PrefsDialog(wxWindow * parent):
   wxDialog(parent, -1, _("Audacity Preferences"), wxDefaultPosition,
         wxDefaultSize, wxDIALOG_MODAL | wxCAPTION | wxTHICK_FRAME)
{
#ifdef __WXMAC__
   mMacHiddenFrame = new wxFrame(NULL, -1, wxT(""), wxPoint(5000, 5000),
                        wxSize(100, 100));
   wxMenuBar *blankMenuBar = new wxMenuBar();
   mMacHiddenFrame->SetMenuBar(blankMenuBar);
   blankMenuBar->MacInstallMenuBar();
   mMacHiddenFrame->Show();
#endif

   gPrefsDialogVisible = true;

   wxRect rect = GetRect();
   if(rect.x < 0) rect.x = 0;
   if(rect.y < 0) rect.y = 0;
   SetSize(rect);

   wxBoxSizer *topSizer = new wxBoxSizer(wxVERTICAL);
// mCategories = new wxNotebook(this, -1, wxDefaultPosition, wxDefaultSize
// mCategories = new wxChoicebook(this, -1, wxDefaultPosition, wxDefaultSize
// mCategories = new wxListbook(this, -1, wxDefaultPosition, wxDefaultSize
   mCategories = new wxTreebook(this, -1, wxDefaultPosition, wxDefaultSize
#ifdef __WXGTK__
                                ,wxNB_LEFT
#endif
                                );

#if !wxCHECK_VERSION(2, 8, 4)
   // The list width is determined by the width of the images.
   // If you don't add some images the list will be too narrow.
   wxImageList *pImages = new wxImageList(64,1);
   wxBitmap bmpTrial(TrialImage);
   pImages->Add( bmpTrial );
   mCategories->SetImageList(pImages);
#endif

   //These two lines were an attempt to size the list correctly.
   //They don't work (in wxWidgets 2.6.1/XP)
//   wxListView * pList = mCategories->GetListView();
   // Can't use wxLC_REPORT because of limitations in wxListBook.
//   pList->SetWindowStyleFlag(wxLC_ICON | wxLC_SINGLE_SEL );//| wxLC_ALIGN_LEFT);
   //pList->SetMinSize( wxSize(300,100));
   
   topSizer->Add(mCategories, 1, wxGROW | wxALL, 0);

   /* All panel additions belong here */
   wxWindow *w;

   // Parameters are: AppPage( page, name, IsSelected, imageId)
   w = new AudioIOPrefs(mCategories);     mCategories->AddPage(w, w->GetName(),false,0);
   w = new QualityPrefs(mCategories);     mCategories->AddPage(w, w->GetName(),false,0);
   w = new FileFormatPrefs(mCategories);  mCategories->AddPage(w, w->GetName(),false,0);
   w = new GUIPrefs(mCategories);         mCategories->AddPage(w, w->GetName(),false,0);
   w = new SpectrumPrefs(mCategories);    mCategories->AddPage(w, w->GetName(),false,0);

   w = new DirectoriesPrefs(mCategories); mCategories->AddPage(w, w->GetName(),false,0);

#ifdef EXPERIMENTAL_SMART_RECORD
   w = new SmartRecordPrefs(mCategories); mCategories->AddPage(w, w->GetName(),false,0);
#endif

#ifdef EXPERIMENTAL_THEME_PREFS
   w = new ThemePrefs(mCategories);       mCategories->AddPage(w, w->GetName(),false,0);
#endif

//   w = new BatchPrefs(mCategories);       mCategories->AddPage(w, w->GetName(),false,0);
   w = new KeyConfigPrefs(mCategories);   mCategories->AddPage(w, w->GetName(),false,0);
   w = new MousePrefs(mCategories);       mCategories->AddPage(w, w->GetName(),false,0);

   long selected = gPrefs->Read(wxT("/Prefs/PrefsCategory"), 0L);
   if (selected < 0 || selected >= (int)mCategories->GetPageCount())
      mSelected = 0;


// This code to hide the roots of the tree doesn't work...
#if 0
   wxTreeCtrl * pTree = mCategories->GetTreeCtrl();
   long Style = pTree->GetWindowStyle();
   Style |= wxTR_HIDE_ROOT;
   pTree->SetWindowStyle(Style);
#endif

   mCategories->SetSelection(selected);

#if defined(__WXGTK__)
   mCategories->GetTreeCtrl()->EnsureVisible(mCategories->GetTreeCtrl()->GetRootItem());
#endif
   
   topSizer->Add(CreateStdButtonSizer(this, eCancelButton|eOkButton), 0, wxEXPAND);

   wxBoxSizer *outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 1, wxGROW|wxTOP, TOP_LEVEL_BORDER);

   SetAutoLayout(true);
   SetSizer(outSizer);
   outSizer->Fit(this);
   outSizer->SetSizeHints(this);

   #ifdef __MACOS9__
   // Until sizing works properly on the Mac
   SetSize(525, 350);
   #endif

   #ifdef __MACOSX__
   // Until sizing works properly on the Mac
   SetSize(620, 350);
   #endif

   #ifdef __WXMSW__
   // Because it looks nice (tm)   (you can see all the tabs at once)
   SetSize(525, 363);
   #endif

   // Center after all that resizing, but make sure it doesn't end up
   // off-screen
   CentreOnParent();

   #ifdef __WXMAC__
   wxPoint where = GetPosition();
   if (where.x < 2)
      where.x = 2;
   if (where.y < 44)
      where.y = 44;
   if (where != GetPosition())
      Move(where);
   #endif
}

void PrefsDialog::OnCancel(wxCommandEvent & event)
{
   int i;

   for (i = 0; i < (int)mCategories->GetPageCount(); i++) {
      ((PrefsPanel *) mCategories->GetPage(i))->Cancel();
   }

   EndModal(0);
}

void PrefsDialog::OnOK(wxCommandEvent & event)
{
   int i;
   unsigned int j;

   gPrefs->Write(wxT("/Prefs/PrefsCategory"), (long)mCategories->GetSelection());
   for (i = 0; i < (int)mCategories->GetPageCount(); i++) {
      PrefsPanel *panel = (PrefsPanel *) mCategories->GetPage(i);

      /* The dialog doesn't end until all the input is valid */
      if (!panel->Apply()) {
         mCategories->SetSelection(i);
         mSelected = i;
         return;
      }
   }

   // BG: Send all Audacity projects a preference update notification
   for(j = 0; j < gAudacityProjects.GetCount(); j++)
   {
      gAudacityProjects[j]->UpdatePrefs();
   }
   EndModal(0);
}

PrefsDialog::~PrefsDialog()
{
#ifdef __WXMAC__
   mMacHiddenFrame->Destroy();
#endif
   gPrefsDialogVisible = false;
}

void PrefsDialog::SelectPageByName(wxString pageName)
{
   int n = mCategories->GetPageCount();

   int i;

   for(i=0; i<n; i++)
      if (mCategories->GetPageText(i) == pageName) {
         mCategories->SetSelection(i);
         return;
      }
}


void PrefsDialog::ShowTempDirPage()
{
   SelectPageByName(_("Directories"));   
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
// arch-tag: b305b538-1d2e-4acf-a997-95023d10a7bd

