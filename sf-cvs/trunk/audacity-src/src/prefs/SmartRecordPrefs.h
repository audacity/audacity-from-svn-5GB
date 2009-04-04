/**********************************************************************

  Audacity: A Digital Audio Editor

  SmartRecordPrefs.h

  Martyn Shaw

  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

**********************************************************************/

#ifndef __AUDACITY_SMARTRECORDING_PREFS__
#define __AUDACITY_SMARTRECORDING_PREFS__

#include <wx/defs.h>
#include <wx/window.h>

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class SmartRecordPrefs:public PrefsPanel
{
 public:
   SmartRecordPrefs(wxWindow * parent);
   ~SmartRecordPrefs(void);
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
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
// arch-tag: ccb794d2-45d5-4f7b-ba0c-6a4d2438ac93
