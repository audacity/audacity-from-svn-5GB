/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormatPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni

**********************************************************************/

#include <wx/window.h>
#include <wx/statbox.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/msgdlg.h>
#include <wx/button.h>

#include "../ExportMP3.h"
#include "../FileFormats.h"
#include "../Prefs.h"
#include "../Track.h"
#include "FileFormatPrefs.h"

wxString gCopyOrEditOptions[] = {
   "Make a copy of the file to edit",
   "Edit the original in place"
};

#define ID_MP3_FIND_BUTTON         7001
#define ID_EXPORT_OPTIONS_BUTTON   7002

BEGIN_EVENT_TABLE(FileFormatPrefs, wxPanel)
   EVT_BUTTON(ID_MP3_FIND_BUTTON, FileFormatPrefs::OnMP3FindButton)
END_EVENT_TABLE()

FileFormatPrefs::FileFormatPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   /* Read existing config... */

   wxString copyEdit =
       gPrefs->Read("/FileFormats/CopyOrEditUncompressedData", "edit");

   int copyEditPos = 1; // Fall back to edit if it doesn't match anything else
   if (copyEdit.IsSameAs("copy", false))
      copyEditPos = 0;

   long mp3Bitrate = gPrefs->Read("/FileFormats/MP3Bitrate", 128);
   wxString mp3BitrateString = wxString::Format("%d", mp3Bitrate);

   wxString defaultFormat = gPCMFormats[gDefaultPCMFormat].name;
   wxString exportFormat =
       gPrefs->Read("/FileFormats/DefaultExportFormat", defaultFormat);

   int formatPos = gDefaultPCMFormat;
   int i;
   for (i = 0; i < gNumPCMFormats; i++)
      if (exportFormat.IsSameAs(gPCMFormats[i].name, false)) {
         formatPos = i;
         break;
      }

   /* Begin layout code... */

   topSizer = new wxStaticBoxSizer(
      new wxStaticBox(this, -1, "File Format Settings"),
      wxVERTICAL );

   {
      wxStaticBoxSizer *copyOrEditSizer = new wxStaticBoxSizer(
         new wxStaticBox(this, -1, "When importing uncompressed audio files"),
         wxVERTICAL );

      mCopyOrEdit[0] = new wxRadioButton(
         this, -1, "Make a copy of the file to edit",
         wxDefaultPosition, wxDefaultSize, wxRB_GROUP );
          
      copyOrEditSizer->Add( mCopyOrEdit[0], 0,
         wxGROW|wxLEFT | wxRIGHT, RADIO_BUTTON_BORDER );

      mCopyOrEdit[1] = new wxRadioButton(
         this, -1, "Edit the original in place",
         wxDefaultPosition, wxDefaultSize, 0 );

	  mCopyOrEdit[0]->SetValue(false);
	  mCopyOrEdit[1]->SetValue(false);
   
      copyOrEditSizer->Add( mCopyOrEdit[1], 0,
         wxGROW|wxLEFT | wxRIGHT, RADIO_BUTTON_BORDER );

      topSizer->Add( copyOrEditSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER );
   }

   {
      wxStaticBoxSizer *defFormatSizer = new wxStaticBoxSizer(
         new wxStaticBox(this, -1, "Uncompressed Export Format"),
         wxVERTICAL);

      wxString *formatStrings = new wxString[gNumPCMFormats];
      for(i=0; i<gNumPCMFormats; i++)
         formatStrings[i] = gPCMFormats[i].name;

      #ifdef __WXMAC__
        // This is just to work around a wxChoice auto-sizing bug
        mDefaultExportFormat = new wxChoice(
           this, -1, wxDefaultPosition, wxSize(200,-1),
           gNumPCMFormats, formatStrings);
      #else
        mDefaultExportFormat = new wxChoice(
           this, -1, wxDefaultPosition, wxDefaultSize,
           gNumPCMFormats, formatStrings);
      #endif
        mDefaultExportFormat->SetSelection(formatPos);

      delete[] formatStrings;

      mExportOptionsButton = new wxButton(this, ID_EXPORT_OPTIONS_BUTTON,
                                          "Export Options...");

      defFormatSizer->Add(mDefaultExportFormat, 0,
                          wxALL, GENERIC_CONTROL_BORDER);

      defFormatSizer->Add(mExportOptionsButton, 0,
                          wxALL, GENERIC_CONTROL_BORDER);

      topSizer->Add(
         defFormatSizer, 0, 
         wxGROW|wxALIGN_CENTER_VERTICAL|wxALL, TOP_LEVEL_BORDER );
   }

   {
      wxStaticBoxSizer *mp3SetupSizer = new wxStaticBoxSizer(
         new wxStaticBox(this, -1, "MP3 Export Setup"),
         wxVERTICAL);

      {
         wxFlexGridSizer *mp3InfoSizer = new wxFlexGridSizer(0, 3, 0, 0);
         mp3InfoSizer->AddGrowableCol(1);

         mp3InfoSizer->Add(
            new wxStaticText(this, -1, "MP3 Library Version:"), 0, 
            wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

         mMP3Version = new wxStaticText(this, -1, "CAPITAL LETTERS");
         SetMP3VersionText();

         mp3InfoSizer->Add(mMP3Version, 0,
            wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);
         
         mMP3FindButton = new wxButton(this, ID_MP3_FIND_BUTTON, "Find Library");
         
         mp3InfoSizer->Add(mMP3FindButton, 0,
                           wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

         if(GetMP3Exporter()->GetConfigurationCaps() & MP3CONFIG_BITRATE) {
            mp3InfoSizer->Add(
               new wxStaticText(this, -1, "Bit Rate:"), 0,
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


   SetAutoLayout(true);
   SetSizer(topSizer);

   topSizer->Fit(this);
   topSizer->SetSizeHints(this);

   /* set controls to match existing configuration... */

   mCopyOrEdit[copyEditPos]->SetValue(true);
}

void FileFormatPrefs::SetMP3VersionText()
{
   wxString versionString;
   bool doMP3 = true;
   
   doMP3 = GetMP3Exporter()->LoadLibrary();

   if (doMP3)
      doMP3 = GetMP3Exporter()->ValidLibraryLoaded();

   if(doMP3)
      versionString = GetMP3Exporter()->GetLibraryVersion();
   else
      versionString = "MP3 exporting plugin not found";
   
   mMP3Version->SetLabel(versionString);
}
         
void FileFormatPrefs::OnMP3FindButton(wxCommandEvent& evt)
{
   wxString oldPath = gPrefs->Read("/MP3/MP3LibPath", "");
 
   gPrefs->Write("/MP3/MP3LibPath", wxString(""));
   
   if (GetMP3Exporter()->FindLibrary(this))
      SetMP3VersionText();
   else {
      gPrefs->Write("/MP3/MP3LibPath", oldPath);
   }
   
   if(GetMP3Exporter()->GetConfigurationCaps() & MP3CONFIG_BITRATE)
      mMP3Bitrate->Enable(GetMP3Exporter()->ValidLibraryLoaded());
}

bool FileFormatPrefs::Apply()
{
   wxString copyEditString[] = { "copy", "edit" };

   int pos = mCopyOrEdit[0]->GetValue() ? 0 : 1;
   wxString copyOrEdit = copyEditString[pos];

   int format = mDefaultExportFormat->GetSelection();
   
   gPrefs->SetPath("/FileFormats");

   wxString originalExportFormat = gPrefs->Read("DefaultExportFormat", "");
   wxString defaultExportFormat = gPCMFormats[format].name;

   gPrefs->Write("CopyOrEditUncompressedData", copyOrEdit);
   gPrefs->Write("DefaultExportFormat", defaultExportFormat);

   if(GetMP3Exporter()->GetConfigurationCaps() & MP3CONFIG_BITRATE) {
      long bitrate;
      mMP3Bitrate->GetStringSelection().ToLong(&bitrate);
      gPrefs->Write("MP3Bitrate", bitrate);
   }
   
   if (originalExportFormat != defaultExportFormat) {
      // Force the menu bar to get recreated
      gMenusDirty++;
   }

   gPrefs->SetPath("/");
   return true;

}


FileFormatPrefs::~FileFormatPrefs()
{
}
