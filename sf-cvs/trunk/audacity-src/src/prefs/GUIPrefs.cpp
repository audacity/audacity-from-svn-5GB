/**********************************************************************

  Audacity: A Digital Audio Editor

  GUIPrefs.cpp

  Brian Gunlogson
  Joshua Haberman

**********************************************************************/

#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/checkbox.h>

#include "../Prefs.h"
#include "GUIPrefs.h"

GUIPrefs::GUIPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   bool autoscroll;
   gPrefs->Read("/GUI/AutoScroll", &autoscroll, false);

   topSizer = new wxStaticBoxSizer(
      new wxStaticBox(this, -1, _("Interface")), wxVERTICAL );

   mAutoscroll = new wxCheckBox(this, -1, _("Autoscroll while playing"));
   mAutoscroll->SetValue(autoscroll);
   topSizer->Add(mAutoscroll, 0, wxGROW|wxALL, 2);

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
