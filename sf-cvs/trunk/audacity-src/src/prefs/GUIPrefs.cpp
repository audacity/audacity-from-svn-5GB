/**********************************************************************

  Audacity: A Digital Audio Editor

  GUIPrefs.cpp

  Brian Gunlogson
  Joshua Haberman

**********************************************************************/

#include <wx/checkbox.h>
#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "../Prefs.h"
#include "GUIPrefs.h"

GUIPrefs::GUIPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   // Scrolling
   bool autoscroll, spectrogram;
   gPrefs->Read("/GUI/AutoScroll", &autoscroll, false);
   gPrefs->Read("/GUI/UpdateSpectrogram", &spectrogram, true);

   topSizer = new wxBoxSizer( wxVERTICAL );

   mAutoscroll = new wxCheckBox(this, -1, _("Autoscroll while playing"));
   mAutoscroll->SetValue(autoscroll);
   topSizer->Add(mAutoscroll, 0, wxGROW|wxALL, 2);

   mSpectrogram = new wxCheckBox(this, -1, _("Update spectrogram while playing"));
   mSpectrogram->SetValue(spectrogram);
   topSizer->Add(mSpectrogram, 0, wxGROW|wxALL, 2);

   // Locale
   wxString lang = gPrefs->Read("/Locale/Language", "en");
   mLocale = new wxTextCtrl(this, -1, lang);
   mLocaleLabel = new wxStaticText(this, -1, _("Language:"));

   wxFlexGridSizer *localeSizer = new wxFlexGridSizer( 0, 2, 0, 0 );
   localeSizer->Add(mLocaleLabel, 0, wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, 2);
   localeSizer->Add(mLocale, 1, wxGROW|wxALL|wxALIGN_CENTER_VERTICAL, 2 );
   topSizer->Add(localeSizer, 0, wxGROW|wxALL, 2);
   
   // Finish layout
   outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   SetAutoLayout(true);
   SetSizer(outSizer);

   outSizer->Fit(this);
   outSizer->SetSizeHints(this);
}

bool GUIPrefs::Apply()
{
   gPrefs->Write("/GUI/AutoScroll", mAutoscroll->GetValue());
   gPrefs->Write("/GUI/UpdateSpectrogram", mSpectrogram->GetValue());
   gPrefs->Write("/Locale/Language", mLocale->GetValue());

   return true;
}

GUIPrefs::~GUIPrefs()
{
}
