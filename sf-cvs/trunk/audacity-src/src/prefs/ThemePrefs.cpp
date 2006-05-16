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


enum eThemePrefsIds {
   idLoadTheme=7000,
   idSaveTheme,
   idLoadThemeCache,
   idSaveThemeCache
};

BEGIN_EVENT_TABLE(ThemePrefs, wxPanel)
   EVT_BUTTON( idLoadTheme, ThemePrefs::OnLoad)
   EVT_BUTTON( idSaveTheme, ThemePrefs::OnSave)
   EVT_BUTTON( idLoadThemeCache, ThemePrefs::OnLoadThemeCache)
   EVT_BUTTON( idSaveThemeCache, ThemePrefs::OnSaveThemeCache)
END_EVENT_TABLE()

ThemePrefs::ThemePrefs(wxWindow * parent) :
   PrefsPanel(parent)
{
   SetLabel(_("Theme"));         // Provide visual label
   SetName(_("Theme"));          // Provide audible label
   Populate();
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

void ThemePrefs::Populate()
{
   ShuttleGui S(this);
   S.Init();
   S.StartHorizontalLay(wxEXPAND);

   S.StartStatic( wxT("Actions"));
   S.Id( idSaveTheme ).AddButton( wxT("Save Theme"))->Enable(false);
   S.Id( idLoadTheme ).AddButton( wxT("Load Theme"))->Enable(false);
   S.Id( idSaveThemeCache ).AddButton( wxT("Save Theme Cache"));
   S.Id( idLoadThemeCache ).AddButton( wxT("Load Theme Cache"));
   S.EndStatic();

   S.StartStatic( wxT("Info"), 1 );
   S.AddFixedText( 
      wxT("Themability is an experimental feature.\r\n\r\nTo try it out, click 'Save Theme Cache' then find\r\nand modify the images and colors in ImageCache.png\r\nusing an image editor such as the Gimp.\r\n\r\nClick 'Load Theme Cache' to load the changed images\r\nand colors back into Audacity.")
      );
   S.EndStatic();
}

void ThemePrefs::OnLoad(wxCommandEvent &event)
{
   int i=1;
}
void ThemePrefs::OnSave(wxCommandEvent &event)
{
   int i=2;
}
void ThemePrefs::OnLoadThemeCache(wxCommandEvent &event)
{
   int i=3;
}
void ThemePrefs::OnSaveThemeCache(wxCommandEvent &event)
{
   int i=4;
   theTheme.RegisterImages();
   theTheme.CreateImageCache();
}
