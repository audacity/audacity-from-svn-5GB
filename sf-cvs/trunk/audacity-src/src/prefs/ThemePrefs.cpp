/**********************************************************************

  Audacity: A Digital Audio Editor

  ThemePrefs.cpp

  James Crook

  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

********************************************************************//**

\class ThemePrefs
\brief Configures dynamic loading of 'Theme' icons and colours

Provides:
 - Button to save current theme as a single png image.
 - Button to load theme from a single png image.
 - Button to save current theme to multiple png images.
 - Button to load theme from multiple png images.
 - (Optional) Button to save theme as Cee data.
 - Button to read theme from default values in program.
 - CheckBox for loading custom themes at startup.

\see \ref Themability

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
   idSaveThemeComponents,
   idReadThemeInternal,
   idSaveThemeAsCode
};

BEGIN_EVENT_TABLE(ThemePrefs, wxPanel)
   EVT_BUTTON( idLoadThemeCache,      ThemePrefs::OnLoadThemeCache)
   EVT_BUTTON( idSaveThemeCache,      ThemePrefs::OnSaveThemeCache)
   EVT_BUTTON( idLoadThemeComponents, ThemePrefs::OnLoadThemeComponents)
   EVT_BUTTON( idSaveThemeComponents, ThemePrefs::OnSaveThemeComponents)
   EVT_BUTTON( idReadThemeInternal,   ThemePrefs::OnReadThemeInternal)
   EVT_BUTTON( idSaveThemeAsCode,     ThemePrefs::OnSaveThemeAsCode)
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
      S.Id( idSaveThemeCache ).AddButton( _("Save Theme"));
      S.Id( idLoadThemeCache ).AddButton( _("Load Theme"));
      S.Id( idSaveThemeComponents ).AddButton( _("Save Components"));
      S.Id( idLoadThemeComponents ).AddButton( _("Load Components"));

// This next button is only provided in Debug mode.
// It is for developers who are compiling Audacity themselves
// and who who wish to generate a new ThemeAsCeeCode.h and compile it in.
#ifdef __WXDEBUG__
//      S.Id( idSaveThemeAsCode ).AddButton( wxT("Save Code" ));
      S.Id( idSaveThemeAsCode ).AddButton( wxT("Output Sourcery" ));
#endif

      S.Id( idReadThemeInternal ).AddButton( _("Defaults" ));
      S.TieCheckBox( _("Load Theme At Startup"),  
         wxT("/Theme/LoadAtStart"), false);
   }
   S.EndStatic();
   S.StartStatic( _("Info"), 1 );
   {
      S.AddFixedText( 
         _("Themability is an experimental feature.\r\n\r\n"
         wxT("To try it out, click \"Save Theme\" then find and modify\r\n")
         wxT("the images and colors in ImageCacheVxx.png using an\r\n")
         wxT("image editor such as the Gimp.\r\n\r\n")
         wxT("Click \"Load Theme\" to load the changed images\r\n")
         wxT("and colors back into Audacity."))
         );
      S.AddFixedText( 
         _("[Only the control toolbar and the colors on the \r\n"
         wxT("wavetrack are currently affected, even though the\r\n")
         wxT("image file shows other icons too.]\r\n\r\n")

         wxT("Saving and loading components uses a separate file\r\n")
         wxT("for each image, but is otherwise the same idea.\r\n\r\n"))
         );

#ifdef __WXDEBUG__
      S.AddFixedText( 
         wxT("You have compiled Audacity with an extra button, \r\n")
         wxT("'Output Sourcery'.  This will save a C version of \r\n")
         wxT("the image cache that can be compiled in as a default.\r\n\r\n")
         );
#endif

      S.AddFixedText( 
         _("If 'Load Theme At Startup' is checked, then the Theme\r\n"
         wxT("will be loaded when the program starts up.\r\n")
         wxT("The component files do not play any part in this.\r\n\r\n"))
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
   theTheme.LoadComponents();
   theTheme.ApplyUpdatedImages();
}

/// Save Theme to multiple png files.
void ThemePrefs::OnSaveThemeComponents(wxCommandEvent &event)
{
   theTheme.SaveComponents();
}

/// Load Theme from single png file.
void ThemePrefs::OnLoadThemeCache(wxCommandEvent &event)
{
   theTheme.ReadImageCache();
   theTheme.ApplyUpdatedImages();
}

/// Save Theme to single png file.
void ThemePrefs::OnSaveThemeCache(wxCommandEvent &event)
{
   theTheme.CreateImageCache();
}

/// Read Theme from internal storage.
void ThemePrefs::OnReadThemeInternal( wxCommandEvent &event)
{
   theTheme.ReadThemeInternal();
   theTheme.ApplyUpdatedImages();
}

/// Save Theme as C source code.
void ThemePrefs::OnSaveThemeAsCode(wxCommandEvent &event)
{
   theTheme.SaveThemeAsCode();
}

