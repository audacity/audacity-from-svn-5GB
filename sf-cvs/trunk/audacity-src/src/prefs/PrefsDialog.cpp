/**********************************************************************

  Audacity: A Digital Audio Editor

  PrefsDialog.cpp

  Joshua Haberman
  James Crook

**********************************************************************/

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
#include <wx/listbook.h>
#include <wx/listctrl.h>
#include <wx/sizer.h>

#include "../Project.h"

#include "../Prefs.h"

#include "PrefsDialog.h"
#include "PrefsPanel.h"

#include "AudioIOPrefs.h"
#include "DirectoriesPrefs.h"
#include "FileFormatPrefs.h"
#include "GUIPrefs.h"
#include "ThemePrefs.h"
#include "BatchPrefs.h"
#include "KeyConfigPrefs.h"
#include "QualityPrefs.h"
#include "SpectrumPrefs.h"
#include "MousePrefs.h"


enum {
   CategoriesID = 1000
};

BEGIN_EVENT_TABLE(PrefsDialog, wxDialog)
   EVT_BUTTON(wxID_OK, PrefsDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, PrefsDialog::OnCancel)
END_EVENT_TABLE()

bool gPrefsDialogVisible = false;


#ifdef NOT_USED
// This is just a test image for the preference dialog image list.
// We'll replace it in due course with nicer images.
static const char * TrialImage[] = {
"64 5 3 1",
"+	c #004010",
".	c None", // mask color = RGB:255,0,0
"#	c #00AA30",
"###+.......+###.###+.......+###.###+.......+###.###+.......+###.",
"+###+.....+###+.+###+.....+###+.+###+.....+###+.+###+.....+###+.",
".+###+...+###+...+###+...+###+...+###+...+###+...+###+...+###+..",
"..+###+.+###+.....+###+.+###+.....+###+.+###+.....+###+.+###+...",
"...+###+###+.......+###+###+.......+###+###+.......+###+###+...."};
#endif

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
   mCategories = new wxListbook(this, -1, wxDefaultPosition, wxDefaultSize
#ifdef __WXGTK__
                                ,wxNB_LEFT
#endif
                                );

   // The list width is determined by the width of the images.
   // If you don't add some images the list will be too narrow.
   wxImageList *pImages = new wxImageList(64,1);
   wxBitmap bmpTrial(TrialImage);
   pImages->Add( bmpTrial );
   mCategories->SetImageList(pImages);

   //These two lines were an attempt to size the list correctly.
   //They don't work (in wxWidgets 2.6.1/XP)
   wxListView * pList = mCategories->GetListView();
   // Can't use wxLC_REPORT because of limitations in wxListBook.
   pList->SetWindowStyleFlag(wxLC_ICON | wxLC_SINGLE_SEL );//| wxLC_ALIGN_LEFT);
   //pList->SetMinSize( wxSize(300,100));



   topSizer->Add(mCategories, 1, wxGROW | wxALL, 0);

   /* All panel additions belong here */
   wxWindow *w;

   // Parameters are: AppPage( page, name, IsSelected, imageId)
   w = new AudioIOPrefs(mCategories);     mCategories->AddPage(w, w->GetName(),false,0);
   w = new QualityPrefs(mCategories);     mCategories->AddPage(w, w->GetName(),false,0);
   w = new FileFormatPrefs(mCategories);  mCategories->AddPage(w, w->GetName(),false,0);
   w = new SpectrumPrefs(mCategories);    mCategories->AddPage(w, w->GetName(),false,0);
   w = new DirectoriesPrefs(mCategories); mCategories->AddPage(w, w->GetName(),false,0);
   w = new GUIPrefs(mCategories);         mCategories->AddPage(w, w->GetName(),false,0);
#ifdef USE_THEME_PREFS
   w = new ThemePrefs(mCategories);       mCategories->AddPage(w, w->GetName(),false,0);
#endif
   w = new BatchPrefs(mCategories);       mCategories->AddPage(w, w->GetName(),false,0);
   w = new KeyConfigPrefs(mCategories);   mCategories->AddPage(w, w->GetName(),false,0);
   w = new MousePrefs(mCategories);       mCategories->AddPage(w, w->GetName(),false,0);

   long selected = gPrefs->Read(wxT("/Prefs/PrefsCategory"), 0L);
   if (selected < 0 || selected >= (int)mCategories->GetPageCount())
      mSelected = 0;

   mCategories->SetSelection(selected);

   mOK = new wxButton(this,
                      wxID_OK, _("OK"));

#ifndef TARGET_CARBON
   mOK->SetDefault();
   mOK->SetFocus();
#endif

   mCancel = new wxButton(this,
                          wxID_CANCEL,
                          _("Cancel"));

   wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);

   buttonSizer->Add(mCancel, 0, wxALL, 7);
   buttonSizer->Add(mOK, 0, wxALL, 7);
   
   topSizer->Add(buttonSizer, 0, wxALIGN_RIGHT);

   wxBoxSizer *outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 1, wxGROW|wxALL, TOP_LEVEL_BORDER);

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

