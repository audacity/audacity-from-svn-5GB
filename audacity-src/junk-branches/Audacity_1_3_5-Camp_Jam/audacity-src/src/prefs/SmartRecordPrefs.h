/**********************************************************************

  Audacity: A Digital Audio Editor

  SmartRecordPrefs.h

  Martyn Shaw

  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

**********************************************************************/

#ifndef __AUDACITY_SMARTRECORDING_PREFS__
#define __AUDACITY_SMARTRECORDING_PREFS__

#include "PrefsPanel.h"

class ShuttleGui;

class SmartRecordPrefs : public PrefsPanel
{
public:
   SmartRecordPrefs(wxWindow * parent);
   ~SmartRecordPrefs(void);
   virtual bool Apply();

private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);

//   DECLARE_EVENT_TABLE();
};
#endif
