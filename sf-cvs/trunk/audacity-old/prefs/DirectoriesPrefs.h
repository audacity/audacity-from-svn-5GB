/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormatPrefs.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_DIRECTORIES_PREFS__
#define __AUDACITY_DIRECTORIES_PREFS__

#include <wx/window.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/string.h>

#include "PrefsPanel.h"

class DirectoriesPrefs:public PrefsPanel {

 public:
   DirectoriesPrefs(wxWindow * parent);
   ~DirectoriesPrefs();
   bool Apply();
   void SetTempDir(wxCommandEvent & event);

 private:
    wxString FormatSize(long size);
   wxStaticBox *mEnclosingBox;

   wxStaticText *mTempDirLabel;
   wxStaticText *mTempDir;
   wxButton *mChange;
   wxStaticText *mFreeSpaceLabel;
   wxStaticText *mFreeSpace;

 public:
    DECLARE_EVENT_TABLE()

};

#endif
