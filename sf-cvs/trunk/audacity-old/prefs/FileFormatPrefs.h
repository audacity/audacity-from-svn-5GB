/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormatPrefs.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_FILE_FORMAT_PREFS__
#define __AUDACITY_FILE_FORMAT_PREFS__

#include <wx/window.h>
#include <wx/statbox.h>
#include <wx/radiobox.h>

#include "PrefsPanel.h"

class FileFormatPrefs: public PrefsPanel {

public:
	FileFormatPrefs(wxWindow *parent);
	~FileFormatPrefs();
	bool Apply();

private:
	wxStaticBox *mEnclosingBox;

	wxRadioBox  *mCopyOrEdit;
	wxRadioBox  *mDefaultExportFormat;

};

#endif
