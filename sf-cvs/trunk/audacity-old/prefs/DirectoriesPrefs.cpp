/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormatPrefs.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/window.h>
#include <wx/statbox.h>
#include <wx/textdlg.h>
#include <wx/msgdlg.h>
#include <wx/filefn.h>
#include <wx/utils.h>
#include <wx/dirdlg.h>

#include "../Prefs.h"
#include "../DiskFunctions.h"
#include "DirectoriesPrefs.h"

enum {
	SetID = 1000
};

BEGIN_EVENT_TABLE(DirectoriesPrefs, wxPanel)
	EVT_BUTTON(SetID, DirectoriesPrefs::SetTempDir)
END_EVENT_TABLE()

DirectoriesPrefs::DirectoriesPrefs(wxWindow *parent):
	PrefsPanel(parent)
{
	wxString dir = gPrefs->Read("/Directories/TempDir", "");

	mEnclosingBox = new wxStaticBox(this,
									-1, 
									"Directories",
									wxPoint(0, 0),
									GetSize());

	gPrefs->SetPath("/Directories");
	
	mTempDirLabel	= new wxStaticText(this,
	                                   -1,
									   "Temp directory:",
									   wxPoint(PREFS_SIDE_MARGINS,
										       PREFS_TOP_MARGIN + 3));

	mTempDir		= new wxStaticText(this,
			                           -1,
									   dir,
									   wxPoint(100,
										       PREFS_TOP_MARGIN + 3));
	mSet            = new wxButton(this,
			                       SetID,
								   "Set",
								   wxPoint(250,
									       PREFS_TOP_MARGIN + 3),
	                               wxSize(50, 30));
	
	mFreeSpaceLabel = new wxStaticText(this,
			                           -1,
									   "Free Space",
									   wxPoint(PREFS_SIDE_MARGINS,
										       35));
	mFreeSpace      = new wxStaticText(this,
			                           -1,
									   FormatSize(GetFreeDiskSpace(dir)),
									   wxPoint(100,
										       35));
}

void DirectoriesPrefs::SetTempDir(wxCommandEvent& event)
{
	wxString dir;
	wxDirDialog dialog(this,
					   "Select Temporary Directory",
					   mTempDir->GetLabel());

	if(dialog.ShowModal() == wxID_CANCEL)
		return;

	dir = dialog.GetPath();
	
	/* TODO: make sure directory is writable */

	mTempDir->SetLabel(dir);
	mFreeSpace->SetLabel(FormatSize(GetFreeDiskSpace(dir)));
}


wxString DirectoriesPrefs::FormatSize(long size)
{
	wxString sizeStr;

	if(size == -1L)
		sizeStr = "Unable to determine";
	else
	{
		/* make it look nice, by formatting into k, MB, etc */
		if(size < 1024)
			sizeStr.sprintf("%d bytes", size);
		else if(size < 1024 * 1024)
			sizeStr.sprintf("%dkB", size / 1024);
		else if(size < 1024 * 1024 * 1024)
			sizeStr.sprintf("%dMB", size / (1024 * 1024));
		else
			sizeStr.sprintf("%dGB",
					size / (1024 * 1024 * 1024));
	}

	return sizeStr;
}


bool DirectoriesPrefs::Apply()
{
	wxString oldDir;
	
	gPrefs->SetPath("/Directories");
	oldDir = gPrefs->Read("TempDir", "none");
	gPrefs->Write("TempDir", mTempDir->GetLabel());

	if(mTempDir->GetLabel() != oldDir)
		wxMessageBox("Changes to temporary directory will not take effect until Audacity is restarted");

	return true;
}

DirectoriesPrefs::~DirectoriesPrefs()
{
	delete mEnclosingBox;
	delete mTempDirLabel;
	delete mTempDir;
	delete mSet;
	delete mFreeSpaceLabel;
	delete mFreeSpace;
}
