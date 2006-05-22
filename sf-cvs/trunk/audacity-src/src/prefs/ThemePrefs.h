/**********************************************************************

  Audacity: A Digital Audio Editor

  ThemePrefs.h

  James Crook

  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

**********************************************************************/

#pragma once
#include "PrefsPanel.h"

// ThemePrefs is being developed first under Windows.
// In case it breaks Linux or mac builds, we only use it
// on Windows for now.  Adventurous people can
// try it out by defining USE_THEME_PREFS on other platforms
// if they so wish.  JKC: 16-May-2006.
#ifdef __WXMSW__
#define USE_THEME_PREFS
#endif

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
