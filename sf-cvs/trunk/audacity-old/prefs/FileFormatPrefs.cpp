/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormatPrefs.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/window.h>
#include <wx/statbox.h>
#include <wx/sizer.h>

#include "../Prefs.h"
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

   int formatPos = 1;                     // Fall back to WAV
   for (int i = 0; i < mNumFormats; i++)
      if (defaultFormat.IsSameAs(gDefaultExportFormatOptions[i], false)) {
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
         wxGROW|wxALL, RADIO_BUTTON_BORDER );

      mCopyOrEdit[1] = new wxRadioButton(
         this, -1, "Edit the original in place",
         wxDefaultPosition, wxDefaultSize, 0 );
   
      copyOrEditSizer->Add( mCopyOrEdit[1], 0,
         wxGROW|wxALL, RADIO_BUTTON_BORDER );

      topSizer->Add( copyOrEditSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER );
   }


   {
      wxStaticBoxSizer *defFormatSizer = new wxStaticBoxSizer(
         new wxStaticBox(this, -1, "Default Export Format"),
         wxVERTICAL);

      mDefaultExportFormats[0] = new wxRadioButton(
         this, -1, gDefaultExportFormatOptions[0],
         wxDefaultPosition, wxDefaultSize, wxRB_GROUP);

      defFormatSizer->Add(
         mDefaultExportFormats[0], 0,
         wxGROW|wxALL, RADIO_BUTTON_BORDER);

      for(int i = 1; i < mNumFormats; i++) {
         mDefaultExportFormats[i] = new wxRadioButton(
            this, -1, gDefaultExportFormatOptions[i]);
         defFormatSizer->Add(
            mDefaultExportFormats[i], 
            0, wxGROW|wxALL, RADIO_BUTTON_BORDER);
      }

      topSizer->Add(
         defFormatSizer, 0, 
         wxGROW|wxALIGN_CENTER_VERTICAL|wxALL, TOP_LEVEL_BORDER );
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
   gPrefs->SetPath("/");
   return true;

}


FileFormatPrefs::~FileFormatPrefs()
{
   delete mCopyOrEdit[0];
   delete mCopyOrEdit[1];

   for(int i = 0; i < mNumFormats; i++)
      delete mDefaultExportFormats[i];
}
