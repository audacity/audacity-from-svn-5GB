/**********************************************************************

  Audacity: A Digital Audio Editor

  ThemePrefs.cpp

  James Crook

  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

**********************************************************************/

#include <wx/wx.h>
#include "ThemePrefs.h"
#include "../ShuttleGui.h"
#include "../Prefs.h"
#include "../Theme.h"
#include "../Project.h"
#include "../ControlToolBar.h"


enum eThemePrefsIds {
   idLoadThemeCache=7000,
   idSaveThemeCache,
   idLoadThemeComponents,
   idSaveThemeComponents
};

BEGIN_EVENT_TABLE(ThemePrefs, wxPanel)
   EVT_BUTTON( idLoadThemeCache,      ThemePrefs::OnLoadThemeCache)
   EVT_BUTTON( idSaveThemeCache,      ThemePrefs::OnSaveThemeCache)
   EVT_BUTTON( idLoadThemeComponents, ThemePrefs::OnLoadThemeComponents)
   EVT_BUTTON( idSaveThemeComponents, ThemePrefs::OnSaveThemeComponents)
END_EVENT_TABLE()

ThemePrefs::ThemePrefs(wxWindow * parent) :
   PrefsPanel(parent)
{
   SetLabel(_("Theme"));         // Provide visual label
   SetName(_("Theme"));          // Provide audible label
   bLoadThemeAtStart = false;
   
   ShuttleGui S(this);
   Populate(S);
}

ThemePrefs::~ThemePrefs(void)
{
}

/// Update the preferences stored on disk.
/// Currently does nothing as Theme Preferences don't change.
bool ThemePrefs::Apply()
{
   /* Step 2: Write to gPrefs */
   gPrefs->SetPath(wxT("/Theme"));
//   gPrefs->Write(wxT("itemname"), mValue);
   gPrefs->SetPath(wxT("/"));
   return true;
}

void ThemePrefs::Populate( ShuttleGui & S)
{
   S.Init();
   S.StartHorizontalLay(wxEXPAND);
   S.StartStatic( wxT("Actions"));
   S.TieTickbox( wxT("Load Theme At Startup"), bLoadThemeAtStart );
   S.Id( idSaveThemeCache ).AddButton( wxT("Save Theme"));
   S.Id( idLoadThemeCache ).AddButton( wxT("Load Theme"));
   S.Id( idSaveThemeComponents ).AddButton( wxT("Save Components"))->Enable(false);
   S.Id( idLoadThemeComponents ).AddButton( wxT("Load Components"))->Enable(false);
   S.EndStatic();
   S.StartStatic( wxT("Info"), 1 );
   S.AddFixedText( 
      wxT("Themability is an experimental feature.\r\n\r\nTo try it out, click 'Save Theme' then find and modify\r\nthe images and colors in ImageCache.png using an\r\nimage editor such as the Gimp.\r\n\r\nClick 'Load Theme' to load the changed images\r\nand colors back into Audacity.")
      );
   S.AddFixedText( 
      wxT("Saving and loading components isn't implemented\r\nyet.  It will use separate files for each image.")
      );
   S.EndStatic();
}

void ThemePrefs::OnLoadThemeComponents(wxCommandEvent &event)
{
   int i=1;
}
void ThemePrefs::OnSaveThemeComponents(wxCommandEvent &event)
{

#if 0
   for(j = 0; j < gAudacityProjects.GetCount(); j++)
   {
      gAudacityProjects[j]->UpdatePrefs();
   }
#endif
}
void ThemePrefs::OnLoadThemeCache(wxCommandEvent &event)
{
   theTheme.ReadImageCache();
   wxLogDebug("OK So Far");
   AudacityProject *p = GetActiveProject();
   if( p->GetControlToolBar() )
   {
      p->GetControlToolBar()->ReCreateButtons();     
   }
}
void ThemePrefs::OnSaveThemeCache(wxCommandEvent &event)
{
   int i=4;
   theTheme.CreateImageCache();
}
