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
   bool autoscroll, spectrogram;
   gPrefs->Read("/GUI/AutoScroll", &autoscroll, false);
   gPrefs->Read("/GUI/UpdateSpectrogram", &spectrogram, true);

   topSizer = new wxStaticBoxSizer(
      new wxStaticBox(this, -1, _("Interface")), wxVERTICAL );

   mAutoscroll = new wxCheckBox(this, -1, _("Autoscroll while playing"));
   mAutoscroll->SetValue(autoscroll);
   topSizer->Add(mAutoscroll, 0, wxGROW|wxALL, 2);

   mSpectrogram = new wxCheckBox(this, -1, _("Update spectrogram while playing (LEAVE CHECKED FOR NOW)"));
   mSpectrogram->SetValue(spectrogram);
   topSizer->Add(mSpectrogram, 0, wxGROW|wxALL, 2);

   SetAutoLayout(true);
   SetSizer(topSizer);

   topSizer->Fit(this);
   topSizer->SetSizeHints(this);

}

bool GUIPrefs::Apply()
{
   bool autoscroll = mAutoscroll->GetValue(), spectrogram = mSpectrogram->GetValue();

   gPrefs->Write("/GUI/AutoScroll", autoscroll);
   gPrefs->Write("/GUI/UpdateSpectrogram", spectrogram);

   return true;
}

GUIPrefs::~GUIPrefs()
{
}
