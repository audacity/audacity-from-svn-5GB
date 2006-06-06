/**********************************************************************

  Audacity: A Digital Audio Editor

  ThemePrefs.cpp

  James Crook

  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

********************************************************************//*!

\class ThemePrefs
\brief Configures dynamic loading of 'Theme' icons and colours

Provides:
 - CheckBox for loading custom themes at startup.
 - Button to save current theme as a single png image.
 - Button to load theme from a single png image.
 - Button to save current theme to multiple png images.
 - Button to load theme from multiple png images.

*//********************************************************************/

#include <wx/wx.h>
#include "../Prefs.h"
#include "../Theme.h"
#include "../Project.h"
#include "../ControlToolBar.h"
#include "../ShuttleGui.h"
#include "ThemePrefs.h"

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
  
   Populate();
}

ThemePrefs::~ThemePrefs(void)
{
}

/// Creates the dialog and its contents.
void ThemePrefs::Populate( )
{
   // First any pre-processing for constructing the GUI.

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

/// Create the dialog contents, or exchange data with it.
void ThemePrefs::PopulateOrExchange( ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND,1);
   S.StartStatic( _("Actions"));
   {
      S.TieCheckBox( _("Load Theme At Startup"),  
         wxT("/Theme/LoadAtStart"), false);
      S.EnableCtrl(false);
      S.Id( idSaveThemeCache ).AddButton( _("Save Theme"));
      S.Id( idLoadThemeCache ).AddButton( _("Load Theme"));
      S.Id( idSaveThemeComponents ).AddButton( _("Save Components"));
      S.EnableCtrl(false);
      S.Id( idLoadThemeComponents ).AddButton( _("Load Components"));
      S.EnableCtrl(false);
   }
   S.EndStatic();
   S.StartStatic( _("Info"), 1 );
   {
      S.AddFixedText( 
         _("Themability is an experimental feature.\r\n\r\n"
         wxT("To try it out, click \"Save Theme\" then find and modify\r\n")
         wxT("the images and colors in ImageCache.png using an\r\n")
         wxT("image editor such as the Gimp.\r\n\r\n")
         wxT("Click \"Load Theme\" to load the changed images\r\n")
         wxT("and colors back into Audacity."))
         );
      S.AddFixedText( 
         _("Only the control toolbar and the colors on the \r\n"
         wxT("wavetrack are currently affected, even though the\r\n")
         wxT("image file shows other icons too.\r\n\r\n\r\n")


         wxT("Loading themes at start up isn't implemented yet.\r\n\r\n")

         wxT("Saving and loading components isn't implemented\r\n")
         wxT("yet.  This will use a separate file for each image.\r\n")
         wxT("Having a single image file is the normal way themes\r\n")
         wxT("will work.  However, when we change the layout\r\n")
         wxT("of the image file, we will want to save and then\r\n")
         wxT("load from individual files.\r\n"))

         );
   }
   S.EndStatic();
}

/// Update the preferences stored on disk.
bool ThemePrefs::Apply()
{
   ShuttleGui S( this, eIsSavingToPrefs );
   PopulateOrExchange( S );

   return true;
}

/// Load Theme from multiple png files.
void ThemePrefs::OnLoadThemeComponents(wxCommandEvent &event)
{
   int i=1;
}
/// Save Theme to multiple png files.
void ThemePrefs::OnSaveThemeComponents(wxCommandEvent &event)
{

}

/// Load Theme from single png file.
void ThemePrefs::OnLoadThemeCache(wxCommandEvent &event)
{
   theTheme.ReadImageCache();
   AudacityProject *p = GetActiveProject();
   if( p->GetControlToolBar() )
   {
      p->GetControlToolBar()->ReCreateButtons();     
   }
}

/// Save Theme to single png file.
void ThemePrefs::OnSaveThemeCache(wxCommandEvent &event)
{
   int i=4;
   theTheme.CreateImageCache();
}
