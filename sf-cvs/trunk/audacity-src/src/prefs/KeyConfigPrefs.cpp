/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyConfigPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni
  Brian Gunlogson

**********************************************************************/

#include <wx/statbox.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/radiobut.h>
#include <wx/choice.h>
#include <wx/intl.h>

#include "../Prefs.h"
#include "KeyConfigPrefs.h"

#define ID_CATEGORY_CHOICE 7001

#define NUM_CATEGORIES 3
wxString categories[] = { "File", "View", "Audio" };

BEGIN_EVENT_TABLE(KeyConfigPrefs, wxPanel)
   EVT_CHOICE(ID_CATEGORY_CHOICE,   KeyConfigPrefs::OnFormatChoice)
END_EVENT_TABLE()

KeyConfigPrefs::KeyConfigPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   /* Begin layout code... */

   topSizer = new wxStaticBoxSizer(
      new wxStaticBox(this, -1, _("Configure Keyboard")),
      wxVERTICAL );

   {
      wxStaticBoxSizer *defFormatSizer = new wxStaticBoxSizer(
         new wxStaticBox(this, -1, _("")),
         wxVERTICAL);

      defFormatSizer->Add(
               new wxStaticText(this, -1, _("Category:")), 0,
               wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

   #ifdef __WXMAC__
     // This is just to work around a wxChoice auto-sizing bug
     mSelectedCategory = new wxChoice(
        this, -1, wxDefaultPosition, wxSize(200,-1),
        NUM_CATEGORIES, categories);
   #else
     mSelectedCategory = new wxChoice(
        this, -1, wxDefaultPosition, wxDefaultSize,
        NUM_CATEGORIES, categories);
   #endif

      mSelectedCategory->SetSelection(0);

      defFormatSizer->Add(mSelectedCategory, 0,
                          wxALL, GENERIC_CONTROL_BORDER);

      topSizer->Add(
         defFormatSizer, 0, 
         wxALIGN_CENTER_VERTICAL|wxALL, TOP_LEVEL_BORDER );
   }

   /*
   {
      wxStaticBoxSizer *mp3SetupSizer = new wxStaticBoxSizer(
         new wxStaticBox(this, -1, _("MP3 Export Setup")),
         wxVERTICAL);

      {
         wxFlexGridSizer *mp3InfoSizer = new wxFlexGridSizer(0, 3, 0, 0);
         mp3InfoSizer->AddGrowableCol(1);

         mp3InfoSizer->Add(
            new wxStaticText(this, -1, _("MP3 Library Version:")), 0, 
            wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

         mMP3Version = new wxStaticText(this, -1, "CAPITAL LETTERS");
         SetMP3VersionText();

         mp3InfoSizer->Add(mMP3Version, 0,
            wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);
         
         mMP3FindButton = new wxButton(this, ID_MP3_FIND_BUTTON,
               _("Find Library"));
         
         mp3InfoSizer->Add(mMP3FindButton, 0,
                           wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

         if(GetMP3Exporter()->GetConfigurationCaps() & MP3CONFIG_BITRATE) {
            mp3InfoSizer->Add(
               new wxStaticText(this, -1, _("Bit Rate:")), 0,
               wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

            wxString bitrates[] = { "56", "96", "128", "192", "256", "320" };
            int numBitrates = 6;

            #ifdef __WXMAC__
            // This is just to work around a wxChoice auto-sizing bug
            mMP3Bitrate = new wxChoice(
               this, -1, wxDefaultPosition, wxSize(120,-1), numBitrates, bitrates);
            #else
            mMP3Bitrate = new wxChoice(
               this, -1, wxDefaultPosition, wxDefaultSize, numBitrates, bitrates);
            #endif

            mp3InfoSizer->Add(mMP3Bitrate, 0, 
               wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

            mMP3Bitrate->SetStringSelection(mp3BitrateString);
            if(mMP3Bitrate->GetSelection() == -1)
               mMP3Bitrate->SetStringSelection("128");

            if(!GetMP3Exporter()->ValidLibraryLoaded())
               mMP3Bitrate->Enable(false);
         }

         mp3SetupSizer->Add(
            mp3InfoSizer, 0, wxGROW|wxALL, 0);
      }

      topSizer->Add(
         mp3SetupSizer, 0,
         wxGROW|wxALIGN_CENTER_VERTICAL|wxALL, TOP_LEVEL_BORDER);
   }
   */

   SetAutoLayout(true);
   SetSizer(topSizer);

   topSizer->Fit(this);
   topSizer->SetSizeHints(this);
}

void KeyConfigPrefs::OnFormatChoice(wxCommandEvent& evt)
{
   int sel = mSelectedCategory->GetSelection();
}

bool KeyConfigPrefs::Apply()
{
   return true;
}

KeyConfigPrefs::~KeyConfigPrefs()
{
}
