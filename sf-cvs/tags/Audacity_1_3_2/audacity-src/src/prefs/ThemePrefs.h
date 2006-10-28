/**********************************************************************

  Audacity: A Digital Audio Editor

  ThemePrefs.h

  James Crook

  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

**********************************************************************/

#ifndef __AUDACITY_THEME_PREFS__
#define __AUDACITY_THEME_PREFS__

#include "PrefsPanel.h"

class ShuttleGui;

class ThemePrefs : public PrefsPanel
{
public:
   ThemePrefs(wxWindow * parent);
   ~ThemePrefs(void);
   virtual bool Apply();

private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void OnLoadThemeComponents(wxCommandEvent &event);
   void OnSaveThemeComponents(wxCommandEvent &event);
   void OnLoadThemeCache(wxCommandEvent &event);
   void OnSaveThemeCache(wxCommandEvent &event);
   void OnReadThemeInternal( wxCommandEvent &event);
   void OnSaveThemeAsCode(wxCommandEvent &event);


   DECLARE_EVENT_TABLE();
};
#endif
