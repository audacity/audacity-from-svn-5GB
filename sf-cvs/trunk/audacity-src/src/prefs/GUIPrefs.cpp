/**********************************************************************

  Audacity: A Digital Audio Editor

  GUIPrefs.cpp

  Brian Gunlogson
  Joshua Haberman
  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "GUIPrefs.h"
#include "../Prefs.h"
#include "../Languages.h"
#include "../EditToolBar.h"


GUIPrefs::GUIPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   // Scrolling
   bool autoscroll, spectrogram, editToolBar,mixerToolBar, alwaysEnablePause;
   gPrefs->Read("/GUI/AutoScroll", &autoscroll, true);
   gPrefs->Read("/GUI/UpdateSpectrogram", &spectrogram, true);

   gPrefs->Read("/GUI/EnableEditToolBar", &editToolBar, true);
   gPrefs->Read("/GUI/EnableMixerToolBar", &mixerToolBar, true);
   gPrefs->Read("/GUI/AlwaysEnablePause", &alwaysEnablePause, false);

   topSizer = new wxBoxSizer( wxVERTICAL );
   
   //Auto-scrolling preference
   mAutoscroll = new wxCheckBox(this, -1, _("Autoscroll while playing"));
   mAutoscroll->SetValue(autoscroll);
   topSizer->Add(mAutoscroll, 0, wxGROW|wxALL, 2);

   //Always enable pause preference
   mAlwaysEnablePause = new wxCheckBox(this, -1, _("Always allow pausing"));
   mAlwaysEnablePause->SetValue(alwaysEnablePause);
   topSizer->Add(mAlwaysEnablePause, 0, wxGROW|wxALL, 2);

   //Spectrogram preference
   mSpectrogram = new wxCheckBox(this, -1, _("Update spectrogram while playing"));
   mSpectrogram->SetValue(spectrogram);
   topSizer->Add(mSpectrogram, 0, wxGROW|wxALL, 2);

   //Enable Edit toolbar preference

   mEditToolBar = new wxCheckBox(this, -1, _("Enable Edit Toolbar"));
   mEditToolBar->SetValue(editToolBar);
   topSizer->Add(mEditToolBar, 0, wxGROW|wxALL, 2);



   //Enable Mixer toolbar preference
   mMixerToolBar = new wxCheckBox(this, -1, _("Enable Mixer Toolbar"));
   mMixerToolBar->SetValue(mixerToolBar);
   topSizer->Add(mMixerToolBar, 0, wxGROW|wxALL, 2);


   // Quit Audacity when last window closes?


   // Enable/disable adjust selection edges


   // Locale
   GetLanguages(mLangCodes, mLangNames);
   int numLangs = mLangNames.GetCount();

   wxString currentLang = gPrefs->Read("/Locale/Language", "en");
   wxString *langArray = new wxString[numLangs];
   int i;
   for(i=0; i<numLangs; i++)
      langArray[i] = mLangNames[i];
   mLocale = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                          numLangs, langArray);
   delete[] langArray;
   for(i=0; i<numLangs; i++)
      if (mLangCodes[i] == currentLang)
         mLocale->SetSelection(i);

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
   gPrefs->Write("/GUI/AlwaysEnablePause", mAlwaysEnablePause->GetValue());

   //-------------------------------------------------------------
   //---------------------- Edit toolbar loading/unloading
   gPrefs->Write("/GUI/EnableEditToolBar", mEditToolBar->GetValue());
   bool enable = mEditToolBar->GetValue();
 
   if(gEditToolBarStub && !enable) {
      gEditToolBarStub->UnloadAll();    //Unload all docked EDIT toolbars
      delete gEditToolBarStub;
      gEditToolBarStub = NULL;
   }
   else if(!gEditToolBarStub && enable)
      {  
         gEditToolBarStub =  new ToolBarStub(gParentWindow, EditToolBarID);
         gEditToolBarStub->LoadAll();
      }
   //----------------------End of Edit toolbar loading/unloading
   //---------------------------------------------------------------


  //-------------------------------------------------------------
   //---------------------- Mixer toolbar loading/unloading
   gPrefs->Write("/GUI/EnableMixerToolBar", mMixerToolBar->GetValue());
   enable = mMixerToolBar->GetValue();
 
   if(gMixerToolBarStub && !enable) {
     gMixerToolBarStub->UnloadAll();    //Unload all docked MIXER toolbars
     delete gMixerToolBarStub;
      gMixerToolBarStub = NULL;
   }
   else if(!gMixerToolBarStub && enable)
      {  
	gMixerToolBarStub =  new ToolBarStub(gParentWindow, MixerToolBarID);
	gMixerToolBarStub->LoadAll();
      }
   //----------------------End of Mixer toolbar loading/unloading
   //---------------------------------------------------------------


   int localeIndex = mLocale->GetSelection();
   gPrefs->Write("/Locale/Language", mLangCodes[localeIndex]);


   return true;
}

GUIPrefs::~GUIPrefs()
{
}
