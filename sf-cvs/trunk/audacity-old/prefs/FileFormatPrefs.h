/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormatPrefs.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_FILE_FORMAT_PREFS__
#define __AUDACITY_FILE_FORMAT_PREFS__

#include <wx/window.h>
#include <wx/statbox.h>
#include <wx/radiobut.h>
#include <wx/choice.h>

#include "PrefsPanel.h"

class FileFormatPrefs:public PrefsPanel {

 public:
   FileFormatPrefs(wxWindow * parent);
   ~FileFormatPrefs();
   bool Apply();

 private:
   wxRadioButton *mCopyOrEdit[2];
   int mNumFormats;
   wxChoice *mMP3Bitrate;

   // leave room for expansion. pointers are cheap
   wxRadioButton *mDefaultExportFormats[20];

};

#endif
