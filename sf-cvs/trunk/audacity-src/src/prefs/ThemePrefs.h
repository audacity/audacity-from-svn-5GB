/**********************************************************************

  Audacity: A Digital Audio Editor

  ThemePrefs.h

  James Crook

  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

**********************************************************************/

#pragma once
#include "PrefsPanel.h"

// ThemePrefs is a new feature (May 2006)
// To disable it, comment out the USE_THEME_PREFS #define.
#define USE_THEME_PREFS

class ShuttleGui;

class ThemePrefs :
   public PrefsPanel
{
public:
   ThemePrefs(wxWindow * parent);
   ~ThemePrefs(void);
   bool Apply();

   void Populate(ShuttleGui & S);
   void OnLoadThemeComponents(wxCommandEvent &event);
   void OnSaveThemeComponents(wxCommandEvent &event);
   void OnLoadThemeCache(wxCommandEvent &event);
   void OnSaveThemeCache(wxCommandEvent &event);

   bool bLoadThemeAtStart;

   DECLARE_EVENT_TABLE();
};
