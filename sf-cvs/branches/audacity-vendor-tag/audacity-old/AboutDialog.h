/**********************************************************************

  Audacity: A Digital Audio Editor

  AboutDialog.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_ABOUT_DLG__
#define __AUDACITY_ABOUT_DLG__

#include "wx/wx.h"
#include "wx/setup.h"
#include "wx/dialog.h"

class AboutDialog: public wxDialog
{
DECLARE_DYNAMIC_CLASS(AboutDialog)

public:
    AboutDialog(wxWindow *parent, wxBitmap *bitmap,
				const wxString& message,
				const wxPoint& pos = wxDefaultPosition);

    void OnOK(wxCommandEvent& event);

DECLARE_EVENT_TABLE()
};

#endif


