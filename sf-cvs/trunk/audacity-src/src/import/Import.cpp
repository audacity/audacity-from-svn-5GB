/**********************************************************************

  Audacity: A Digital Audio Editor

  Import.cpp

  Dominic Mazzoni

  This file contains a general function which will import almost
  any type of sampled audio file (i.e. anything except MIDI)
  and return the tracks that were imported.  This function just
  figures out which one to call; the actual importers are in
  ImportPCM, ImportMP3, ImportOGG, and ImportRawData.

**********************************************************************/

#include <wx/textctrl.h>
#include <wx/msgdlg.h>
#include <wx/string.h>
#include <wx/intl.h>
#include <wx/listimpl.cpp>

#include "../Audacity.h"

#include "Import.h"
#include "ImportPlugin.h"
#include "ImportPCM.h"
#include "ImportMP3.h"
#include "ImportOGG.h"
#include "ImportRaw.h"
#include "../Track.h"

WX_DEFINE_LIST(ImportPluginList);
WX_DEFINE_LIST(UnusableImportPluginList);

Importer::Importer()
{
   mImportPluginList = new ImportPluginList;
   mUnusableImportPluginList = new UnusableImportPluginList;

   // build the list of import plugin and/or unusableImporters.
   // order is significant.  If none match, they will all be tried
   // in the order defined here.
   GetPCMImportPlugin(mImportPluginList, mUnusableImportPluginList);
   GetOGGImportPlugin(mImportPluginList, mUnusableImportPluginList);
   GetMP3ImportPlugin(mImportPluginList, mUnusableImportPluginList);

   // TODO: others
}

Importer::~Importer()
{
   delete mImportPluginList;
   delete mUnusableImportPluginList;
}

void Importer::GetSupportedImportFormats(FormatList *formatList)
{
   ImportPluginList::Node *importPluginNode = mImportPluginList->GetFirst();
   while(importPluginNode)
   {
      ImportPlugin *importPlugin = importPluginNode->GetData();
      formatList->Append(new Format(importPlugin->GetPluginFormatDescription(),
                                    importPlugin->GetSupportedExtensions()));
      importPluginNode = importPluginNode->GetNext();
   }
}

// returns number of tracks imported
int Importer::Import(wxString fName,
                     TrackFactory *trackFactory,
                     Track *** tracks,
                     wxString &errorMessage,
                     progress_callback_t progressCallback,
                     void *userData)
{
   int numTracks = 0;

   // see if any of the plugins expect this extension and if so give
   // that plugins first dibs
   wxString extension = fName.AfterLast('.');
   ImportPluginList::Node *importPluginNode = mImportPluginList->GetFirst();
   while(importPluginNode)
   {
      ImportPlugin *plugin = importPluginNode->GetData();
      if( plugin->SupportsExtension(extension) )
      {
         mInFile = plugin->Open(fName);
         if( mInFile != NULL )
         {
            mInFile->SetProgressCallback(progressCallback, userData);
            if( mInFile->Import(trackFactory, tracks, &numTracks) == true )
            {
               // success!
               delete mInFile;
               return numTracks;
            }
            delete mInFile;
         }
      }
      importPluginNode = importPluginNode->GetNext();
   }

   // no importPlugin that recognized the extension succeeded.  However, the
   // file might be misnamed.  So this time we try all the importPlugins
   // in order and see if any of them can handle the file
   importPluginNode = mImportPluginList->GetFirst();
   while(importPluginNode)
   {
      ImportPlugin *plugin = importPluginNode->GetData();
      mInFile = plugin->Open(fName);
      if( mInFile != NULL )
      {
         mInFile->SetProgressCallback(progressCallback, userData);
         if( mInFile->Import(trackFactory, tracks, &numTracks) == true )
         {
            // success!
            delete mInFile;
            return numTracks;
         }
         delete mInFile;
      }
      importPluginNode = importPluginNode->GetNext();
   }

   // None of our plugins can handle this file.  It might be that
   // Audacity supports this format, but support was not compiled in.
   // If so, notify the user of this fact
   UnusableImportPluginList::Node *unusableImporterNode
      = mUnusableImportPluginList->GetFirst();
   while(unusableImporterNode)
   {
      UnusableImportPlugin *unusableImportPlugin = unusableImporterNode->GetData();
      if( unusableImportPlugin->SupportsExtension(extension) )
      {
         errorMessage = _("This version of Audacity was not compiled with ") +
                        unusableImportPlugin->GetPluginFormatDescription() +
                        _(" support.");
         return 0;
      }
      unusableImporterNode = unusableImporterNode->GetNext();
   }

   // we were not able to recognize the file type
   errorMessage = _("Audacity did not recognize the type "
                    "of this file.\n"
                    "If it is uncompressed, try importing it "
                    "using \"Import Raw\"" );
   return 0;
}

wxString Importer::GetFileDescription()
{
   return mInFile->GetFileDescription();
}


