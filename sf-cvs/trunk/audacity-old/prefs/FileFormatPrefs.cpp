/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormatPrefs.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/window.h>
#include <wx/statbox.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/msgdlg.h>

#include "../Prefs.h"
#include "../Track.h"
#include "../ExportMP3.h"
#include "FileFormatPrefs.h"

wxString gCopyOrEditOptions[] = { "Make a copy of the file to edit",
   "Edit the original in place"
};
wxString gDefaultExportFormatOptions[] = { "AIFF",
#ifdef __WXMAC__
   "AIFF with track markers",
#endif
   "WAV",
   "IRCAM",
   "AU",
#ifdef USE_LIBVORBIS
   "Ogg Vorbis",
#endif
   "MP3",
   "***last"
};

FileFormatPrefs::FileFormatPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   /* Read existing config... */

   wxString copyEdit =
       gPrefs->Read("/FileFormats/CopyOrEditUncompressedData", "edit");

   int copyEditPos = 1; // Fall back to edit if it doesn't match anything else
   if (copyEdit.IsSameAs("copy", false))
      copyEditPos = 0;


   wxString defaultFormat =
       gPrefs->Read("/FileFormats/DefaultExportFormat", "WAV");

   mNumFormats = 1;
   while (gDefaultExportFormatOptions[mNumFormats] != "***last")
      mNumFormats++;

   long mp3Bitrate = gPrefs->Read("/FileFormats/MP3Bitrate", 128);
   wxString mp3BitrateString = wxString::Format("%d", mp3Bitrate);
   gMP3Exporter->LoadLibrary();

   int formatPos = 1;                     // Fall back to WAV
   for (int i = 0; i < mNumFormats; i++)
      if (defaultFormat.IsSameAs(gDefaultExportFormatOptions[i], false)) {
         /* Don't set it to MP3 if no MP3 library is loaded */
         if(gDefaultExportFormatOptions[i] == "MP3" && !gMP3Exporter->LoadLibrary())
            continue;

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
      mCopyOrEdit[0]->SetValue(false);
          
      copyOrEditSizer->Add( mCopyOrEdit[0], 0,
         wxGROW|wxLEFT | wxRIGHT, RADIO_BUTTON_BORDER );

      mCopyOrEdit[1] = new wxRadioButton(
         this, -1, "Edit the original in place",
         wxDefaultPosition, wxDefaultSize, 0 );
      mCopyOrEdit[1]->SetValue(false);
   
      copyOrEditSizer->Add( mCopyOrEdit[1], 0,
         wxGROW|wxLEFT | wxRIGHT, RADIO_BUTTON_BORDER );

      topSizer->Add( copyOrEditSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER );
   }


   {
      wxStaticBoxSizer *defFormatSizer = new wxStaticBoxSizer(
         new wxStaticBox(this, -1, "Export Format"),
         wxVERTICAL);

      mDefaultExportFormats[0] = new wxRadioButton(
         this, -1, gDefaultExportFormatOptions[0],
         wxDefaultPosition, wxDefaultSize, wxRB_GROUP);

      /* I have no idea why this is necessary, but it always gets set on
       * Windows if this isn't here! */
      mDefaultExportFormats[0]->SetValue(false);

      defFormatSizer->Add(
         mDefaultExportFormats[0], 0,
         wxGROW|wxLEFT | wxRIGHT, RADIO_BUTTON_BORDER);

      for(int i = 1; i < mNumFormats; i++) {
         mDefaultExportFormats[i] = new wxRadioButton(
            this, -1, gDefaultExportFormatOptions[i]);
         mDefaultExportFormats[i]->SetValue(false);

         if(gDefaultExportFormatOptions[i] == "MP3" && !gMP3Exporter->ValidLibraryLoaded())
            mDefaultExportFormats[i]->Enable(false);

         defFormatSizer->Add(
            mDefaultExportFormats[i], 
            0, wxGROW|wxLEFT | wxRIGHT, RADIO_BUTTON_BORDER);
      }

      topSizer->Add(
         defFormatSizer, 0, 
         wxGROW|wxALIGN_CENTER_VERTICAL|wxALL, TOP_LEVEL_BORDER );
   }

   {
      wxStaticBoxSizer *mp3SetupSizer = new wxStaticBoxSizer(
         new wxStaticBox(this, -1, "MP3 Export Setup"),
         wxVERTICAL);

      {
         wxString versionString;

         if(!gMP3Exporter->ValidLibraryLoaded())
            versionString = "MP3 exporting plugin not found";
         else
            versionString = gMP3Exporter->GetLibraryVersion();

         wxFlexGridSizer *mp3InfoSizer = new wxFlexGridSizer(0, 2, 0, 0);

         mp3InfoSizer->Add(
            new wxStaticText(this, -1, "MP3 Library Version:"), 0, 
            wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);


         mp3InfoSizer->Add(
            new wxStaticText(this, -1, versionString), 0,
            wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

         if(gMP3Exporter->GetConfigurationCaps() & MP3CONFIG_BITRATE) {
            mp3InfoSizer->Add(
               new wxStaticText(this, -1, "Bit Rate:"), 0,
               wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

            wxString bitrates[] = { "56", "96", "128", "192", "256", "320" };
            int numBitrates = 6;

            mMP3Bitrate = new wxChoice(
               this, -1, wxDefaultPosition, wxDefaultSize, numBitrates, bitrates);

            mp3InfoSizer->Add(mMP3Bitrate, 0, 
               wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

            mMP3Bitrate->SetStringSelection(mp3BitrateString);
            if(mMP3Bitrate->GetSelection() == -1)
               mMP3Bitrate->SetStringSelection("128");

            if(!gMP3Exporter->ValidLibraryLoaded())
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
   mDefaultExportFormats[formatPos]->SetValue(true);
}

bool FileFormatPrefs::Apply()
{
   int pos;
   wxString copyEditString[] = { "copy", "edit" };

   pos = mCopyOrEdit[0]->GetValue() ? 0 : 1;
   wxString copyOrEdit = copyEditString[pos];

   for(int i = 0; i < mNumFormats; i++)
      if(mDefaultExportFormats[i]->GetValue()) {
         pos = i;
         break;
      }

   wxString defaultExportFormat = gDefaultExportFormatOptions[pos];

   gPrefs->SetPath("/FileFormats");
   gPrefs->Write("CopyOrEditUncompressedData", copyOrEdit);
   gPrefs->Write("DefaultExportFormat", defaultExportFormat);

   if(gMP3Exporter->GetConfigurationCaps() & MP3CONFIG_BITRATE) {
      long bitrate;
      mMP3Bitrate->GetStringSelection().ToLong(&bitrate);
      gPrefs->Write("MP3Bitrate", bitrate);
   }

   gPrefs->SetPath("/");
   return true;

}


FileFormatPrefs::~FileFormatPrefs()
{
}
