/**********************************************************************

  Audacity: A Digital Audio Editor

  GUIPrefs.h

  Brian Gunlogson
  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_GUI_PREFS__
#define __AUDACITY_GUI_PREFS__

#include <wx/defs.h>

#include <wx/arrstr.h>
#include <wx/string.h>
#include <wx/window.h>

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class ShowPrefs:public PrefsPanel
{
 public:
   ShowPrefs(wxWindow * parent);
   ~ShowPrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
};

class TracksPrefs:public PrefsPanel
{
 public:
   TracksPrefs(wxWindow * parent);
   ~TracksPrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);

   wxArrayString mSoloCodes;
   wxArrayString mSoloChoices;
   wxArrayString mViewCodes;
   wxArrayString mViewChoices;
};

class GUIPrefs:public PrefsPanel 
{
 public:
   GUIPrefs(wxWindow * parent);
   ~GUIPrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);

   wxArrayString mLangCodes;
   wxArrayString mLangNames;

   wxArrayString mHtmlHelpCodes;
   wxArrayString mHtmlHelpChoices;

   wxArrayString mRangeCodes;
   wxArrayString mRangeChoices;
};

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
// arch-tag: 57018e2b-d264-4f93-bfa7-06752ebf631e

