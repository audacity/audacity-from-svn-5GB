/**********************************************************************

  Audacity: A Digital Audio Editor

  GUIPrefs.cpp

  Brian Gunlogson
  Joshua Haberman

**********************************************************************/

#include <math.h>

#include <wx/button.h>
#include <wx/dirdlg.h>
#include <wx/event.h>
#include <wx/filefn.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/checkbox.h>
#include <wx/utils.h>

#include "../Prefs.h"
#include "GUIPrefs.h"

BEGIN_EVENT_TABLE(GUIPrefs, wxPanel)
END_EVENT_TABLE()

GUIPrefs::GUIPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   bool autoscroll;
   gPrefs->Read("/GUI/AutoScroll", &autoscroll, false);

   topSizer = new wxStaticBoxSizer(
      new wxStaticBox(this, -1, _("GUI Settings")), wxVERTICAL );

   {
      mAutoscroll = new wxCheckBox(this, -1, _("Autoscroll Progress Indicator"));
      mAutoscroll->SetValue(autoscroll);
      topSizer->Add(mAutoscroll, 0, wxGROW|wxALL, 2);
   }

   SetAutoLayout(true);
   SetSizer(topSizer);

   topSizer->Fit(this);
   topSizer->SetSizeHints(this);

}

bool GUIPrefs::Apply()
{
   bool autoscroll = mAutoscroll->GetValue();

   gPrefs->Write("/GUI/AutoScroll", autoscroll);

   return true;
}

GUIPrefs::~GUIPrefs()
{
}
