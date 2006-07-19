/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportPCM.cpp

  Dominic Mazzoni

*//****************************************************************//**

\class PCMImportFileHandle
\brief An ImportFileHandle for PCM data

*//****************************************************************//**

\class PCMImportPlugin
\brief An ImportPlugin for PCM data

*//*******************************************************************/

#include "../Audacity.h"
#include "../AudacityApp.h"
#include "../Internat.h"
#include "ImportPCM.h"

#include <wx/string.h>
#include <wx/utils.h>
#include <wx/intl.h>
#include <wx/ffile.h>
#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/button.h>
#include <wx/stattext.h>

#include "sndfile.h"

#ifndef SNDFILE_1
#error Requires libsndfile 1.0 or higher
#endif

#include "../FileFormats.h"
#include "../Prefs.h"
#include "../WaveTrack.h"
#include "ImportPlugin.h"

// msmeyer: Created by wxDesigner
const int ID_TEXT = 10000;
const int ID_COPY = 10001;
const int ID_EDIT = 10002;
const int ID_ALWAYS = 10003;

wxSizer *EditWarningDialogResource( wxWindow *parent, bool call_fit, bool set_sizer )
{
    wxBoxSizer *item0 = new wxBoxSizer( wxVERTICAL );

    wxStaticText *item1 = new wxStaticText( parent, ID_TEXT, _("Audacity is currently set up not to make a copy when importing uncompressed audio files."), wxDefaultPosition, wxDefaultSize, 0 );
    item0->Add( item1, 0, wxGROW|wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxTOP, 5 );

    wxStaticText *item2 = new wxStaticText( parent, ID_TEXT, _("This speeds up file import and consumes less disk space."), wxDefaultPosition, wxDefaultSize, 0 );
    item0->Add( item2, 0, wxGROW|wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT, 5 );

    wxStaticText *item3 = new wxStaticText( parent, ID_TEXT, _("However, if you later delete or move the original file, your Audacity project will be lost, too."), wxDefaultPosition, wxDefaultSize, 0 );
    item0->Add( item3, 0, wxGROW|wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT, 5 );

    wxStaticText *item4 = new wxStaticText( parent, ID_TEXT, _("Is this what you want?"), wxDefaultPosition, wxDefaultSize, 0 );
    item0->Add( item4, 0, wxGROW|wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxBOTTOM, 5 );

    wxBoxSizer *item5 = new wxBoxSizer( wxHORIZONTAL );

    wxButton *item6 = new wxButton( parent, ID_COPY, _("No, make a copy while importing (safer)"), wxDefaultPosition, wxDefaultSize, 0 );
    item6->SetDefault();
    item5->Add( item6, 0, wxALIGN_CENTER|wxALL, 5 );

    wxButton *item7 = new wxButton( parent, ID_EDIT, _("Yes, import without making a copy"), wxDefaultPosition, wxDefaultSize, 0 );
    item5->Add( item7, 0, wxALIGN_CENTER|wxALL, 5 );

    item0->Add( item5, 0, wxALIGN_CENTER|wxALL, 5 );

    wxCheckBox *item8 = new wxCheckBox( parent, ID_ALWAYS, _("Always perform this action when importing uncompressed audio files"), wxDefaultPosition, wxDefaultSize, 0 );
    item0->Add( item8, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    if (set_sizer)
    {
        parent->SetSizer( item0 );
        if (call_fit)
            item0->SetSizeHints( parent );
    }
    
    return item0;
}

class EditWarningDialog: public wxDialog
{
public:
   EditWarningDialog(wxWindow* parent) :
      wxDialog(parent, -1, _("Warning"), wxDefaultPosition, wxDefaultSize,
      wxDEFAULT_DIALOG_STYLE & (~wxCLOSE_BOX)) // user must answer, no close box
   {
      EditWarningDialogResource(this, true, true);
   }
   
   void OnButtonCopy(wxCommandEvent& evt)
   {
      EndModal(ID_COPY);
   }
   
   void OnButtonEdit(wxCommandEvent& evt)
   {
      EndModal(ID_EDIT);
   }
   
   bool GetSaveChoice()
   {
      wxCheckBox* chkAlways = (wxCheckBox*)FindWindowById(ID_ALWAYS);
      return chkAlways->GetValue() != 0;
   }
   
   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(EditWarningDialog, wxDialog)
   EVT_BUTTON(ID_COPY, EditWarningDialog::OnButtonCopy)
   EVT_BUTTON(ID_EDIT, EditWarningDialog::OnButtonEdit)
END_EVENT_TABLE();

bool ShowEditWarning()
{
   bool showWarning = true;
   gPrefs->Read(wxT("/FileFormats/ShowEditWarning"), &showWarning, true);
      
   if (!showWarning)
   {
      // User doesn't want to be asked and always wants to edit
      return true;
   }
   
   EditWarningDialog dlg(gParentFrame);
   int result = dlg.ShowModal();
   
   if (dlg.GetSaveChoice())
   {
      // User ticked 'remember choice' checkbox
      if (result == ID_EDIT)
      {
         // User always wants to edit, so disable warning
         gPrefs->Write(wxT("/FileFormats/ShowEditWarning"), false);
      } else
      {
         // User always wants to copy, so remember this
         gPrefs->Write(wxT("/FileFormats/CopyOrEditUncompressedData"),
                       wxT("copy"));
      }
   }
   
   return result == ID_EDIT;
}

class PCMImportPlugin : public ImportPlugin
{
public:
   PCMImportPlugin():
      ImportPlugin(wxStringList())
   {
      sf_get_all_extensions(mExtensions);
   }

   ~PCMImportPlugin() { }

   wxString GetPluginFormatDescription();
   ImportFileHandle *Open(wxString Filename);
};


class PCMImportFileHandle : public ImportFileHandle
{
public:
   PCMImportFileHandle(wxString name, SNDFILE *file, SF_INFO info);
   ~PCMImportFileHandle();

   void SetProgressCallback(progress_callback_t function,
                            void *userData);
   wxString GetFileDescription();
   int GetFileUncompressedBytes();
   bool Import(TrackFactory *trackFactory, Track ***outTracks,
               int *outNumTracks);
private:
   wxString              mName;
   SNDFILE              *mFile;
   SF_INFO               mInfo;
   sampleFormat          mFormat;
   void                 *mUserData;
   progress_callback_t  mProgressCallback;
};

void GetPCMImportPlugin(ImportPluginList *importPluginList,
                        UnusableImportPluginList *unusableImportPluginList)
{
   importPluginList->Append(new PCMImportPlugin);
}

wxString PCMImportPlugin::GetPluginFormatDescription()
{
    return wxT("Uncompressed PCM Audio Files");
}

ImportFileHandle *PCMImportPlugin::Open(wxString filename)
{
   SF_INFO info;
   SNDFILE *file;

   memset(&info, 0, sizeof(info));

   #ifdef _UNICODE
      /* sf_open doesn't handle fn_Str() in Unicode build. May or may not actually work. */
      file = sf_open(FILENAME(filename).mb_str(), SFM_READ, &info);
   #else // ANSI
      file = sf_open(FILENAME(filename).fn_str(), SFM_READ, &info);
   #endif // Unicode/ANSI

   if (!file) {
      // TODO: Handle error
      //char str[1000];
      //sf_error_str((SNDFILE *)NULL, str, 1000);

      return NULL;
   }

   return new PCMImportFileHandle(filename, file, info);
}

PCMImportFileHandle::PCMImportFileHandle(wxString name,
                                         SNDFILE *file, SF_INFO info):
   mName(name),
   mFile(file),
   mInfo(info),
   mUserData(NULL),
   mProgressCallback(NULL)
{
   //
   // Figure out the format to use.
   //
   // In general, go with the user's preferences.  However, if
   // the file is higher-quality, go with a format which preserves
   // the quality of the original file.
   //
   
   mFormat = (sampleFormat)
      gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample);

   if (mFormat != floatSample &&
       sf_subtype_more_than_16_bits(mInfo.format))
      mFormat = floatSample;
}

void PCMImportFileHandle::SetProgressCallback(progress_callback_t progressCallback,
                                      void *userData)
{
   mProgressCallback = progressCallback;
   mUserData = userData;
}

wxString PCMImportFileHandle::GetFileDescription()
{
   return sf_header_name(mInfo.format);
}

int PCMImportFileHandle::GetFileUncompressedBytes()
{
   return mInfo.frames * mInfo.channels * SAMPLE_SIZE(mFormat);
}

bool PCMImportFileHandle::Import(TrackFactory *trackFactory,
                                 Track ***outTracks,
                                 int *outNumTracks)
{
   wxASSERT(mFile);

   *outNumTracks = mInfo.channels;
   WaveTrack **channels = new WaveTrack *[*outNumTracks];

   int c;
   for (c = 0; c < *outNumTracks; c++) {
      channels[c] = trackFactory->NewWaveTrack(mFormat);
      channels[c]->SetRate(mInfo.samplerate);

      if (*outNumTracks > 1)
         switch (c) {
         case 0:
            channels[c]->SetChannel(Track::LeftChannel);
            break;
         case 1:
            channels[c]->SetChannel(Track::RightChannel);
            break;
         default:
            channels[c]->SetChannel(Track::MonoChannel);
         }
   }

   if (*outNumTracks == 2) {
      channels[0]->SetLinked(true);
      channels[1]->SetTeamed(true);
   }

   sampleCount fileTotalFrames = (sampleCount)mInfo.frames;
   sampleCount maxBlockSize = channels[0]->GetMaxBlockSize();
   bool cancelled = false;
   
   wxString copyEdit =
       gPrefs->Read(wxT("/FileFormats/CopyOrEditUncompressedData"), wxT("edit"));

   // Fall back to "edit" if it doesn't match anything else
   bool doEdit = true;          
   if (copyEdit.IsSameAs(wxT("copy"), false))
      doEdit = false;
      
   // If the format is not seekable, we must use 'copy' mode,
   // because 'edit' mode depends on the ability to seek to an
   // arbitrary location in the file.
   if (!mInfo.seekable)
      doEdit = false;

   if (doEdit)
   {
      // Ask user if he really wants to edit, not copy the file
      doEdit = ShowEditWarning();
   }
   
   if (doEdit) {

      // If this mode has been selected, we form the tracks as
      // aliases to the files we're editing, i.e. ("foo.wav", 12000-18000)
      // instead of actually making fresh copies of the samples.

      for (sampleCount i = 0; i < fileTotalFrames; i += maxBlockSize) {
         sampleCount blockLen = maxBlockSize;
         if (i + blockLen > fileTotalFrames)
            blockLen = fileTotalFrames - i;

         for(c=0; c<(*outNumTracks); c++)
            channels[c]->AppendAlias(mName, i, blockLen, c);

         if( mProgressCallback )
            cancelled = mProgressCallback(mUserData,
                                          i*1.0 / fileTotalFrames);
         if (cancelled)
            break;
      }
   }
   else {
      // Otherwise, we're in the "copy" mode, where we read in the actual
      // samples from the file and store our own local copy of the
      // samples in the tracks.
      
      samplePtr srcbuffer = NewSamples(maxBlockSize * (*outNumTracks),
                                       mFormat);
      samplePtr buffer = NewSamples(maxBlockSize, mFormat);

      unsigned long framescompleted = 0;
      
      long block;
      do {
         block = maxBlockSize;
         
         if (mFormat == int16Sample)
            block = sf_readf_short(mFile, (short *)srcbuffer, block);
         else
            block = sf_readf_float(mFile, (float *)srcbuffer, block);
         
         if (block) {
            for(c=0; c<(*outNumTracks); c++) {
               if (mFormat==int16Sample) {
                  for(int j=0; j<block; j++)
                     ((short *)buffer)[j] =
                        ((short *)srcbuffer)[(*outNumTracks)*j+c];
               }
               else {
                  for(int j=0; j<block; j++)
                     ((float *)buffer)[j] =
                        ((float *)srcbuffer)[(*outNumTracks)*j+c];
               }
               
               channels[c]->Append(buffer, mFormat, block);
            }
            framescompleted += block;
         }

         if( mProgressCallback )
            cancelled = mProgressCallback(mUserData,
                                          framescompleted*1.0 /
                                          fileTotalFrames);
         if (cancelled)
            break;

      } while (block > 0);
   }

   if (cancelled) {
      for (c = 0; c < *outNumTracks; c++)
         delete channels[c];
      delete[] channels;

      return false;
   }
   else {
      *outTracks = new Track *[*outNumTracks];
      for(c = 0; c < *outNumTracks; c++) {
         channels[c]->Flush();
         (*outTracks)[c] = channels[c];
      }
      delete[] channels;

      return true;
   }
}

PCMImportFileHandle::~PCMImportFileHandle()
{
   sf_close(mFile);
}


#if 0

#include <wx/file.h>
#include <wx/string.h>
#include <wx/thread.h>
#include <wx/timer.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/intl.h>

#include "Import.h"
#include "ImportPCM.h"

#include "../FileFormats.h"
#include "../WaveTrack.h"
#include "../DirManager.h"
#include "../Prefs.h"

#include "sndfile.h"

bool IsPCM(wxString fName)
{
   wxFile testFile;
   testFile.Open(fName);
   if (!testFile.IsOpened())
      return false;
   testFile.Close();

   SF_INFO    info;
   SNDFILE   *fp;

   fp = sf_open_read(FILENAME(fName), &info);

   if (fp) {
      sf_close(fp);
      return true;
   }

   return false;
}


bool ImportPCM(wxWindow * parent,
               wxString fName, 
               WaveTrack ** channels[],
               int *numChannels,
               DirManager * dirManager)
{
   SF_INFO       info;
   SNDFILE      *fp;
   sampleFormat  format;

   fp = sf_open_read(FILENAME(fName), &info);

   if (!fp) {
      char str[1000];
      sf_error_str((SNDFILE *)NULL, str, 1000);
      wxMessageBox(LAT1CTOWX(str));

      return false;
   }

   wxString progressStr;
   wxString formatName = sf_header_name(info.format & SF_FORMAT_TYPEMASK);
   progressStr.Printf(_("Importing %s file..."),
                      formatName.c_str());

   *numChannels = info.channels;
   *channels = new WaveTrack*[*numChannels];

   if (info.pcmbitwidth > 16)
      format = floatSample;
   else
      format = int16Sample;

   int c;
   for(c=0; c<*numChannels; c++) {
      (*channels)[c] = new WaveTrack(dirManager, format);
      (*channels)[c]->SetRate(info.samplerate);
      (*channels)[c]->SetName(TrackNameFromFileName(fName));
      (*channels)[c]->SetChannel(Track::MonoChannel);
   }

   if (*numChannels == 2) {
      (*channels)[0]->SetChannel(Track::LeftChannel);
      (*channels)[1]->SetChannel(Track::RightChannel);
      (*channels)[0]->SetLinked(true);
      (*channels)[1]->SetTeamed(true);
   }

   sampleCount fileTotalFrames = (sampleCount)info.frames;
   sampleCount maxBlockSize = (*channels)[0]->GetMaxBlockSize();

   wxString copyEdit =
       gPrefs->Read(wxT("/FileFormats/CopyOrEditUncompressedData"), wxT("edit"));

   // Fall back to "edit" if it doesn't match anything else
   bool doEdit = true;          
   if (copyEdit.IsSameAs(wxT("copy"), false))
      doEdit = false;

   if (doEdit) {

      // If this mode has been selected, we form the tracks as
      // aliases to the files we're editing, i.e. ("foo.wav", 12000-18000)
      // instead of actually making fresh copies of the samples.

      wxProgressDialog *progress = NULL;
      wxYield();
      wxStartTimer();
      wxBusyCursor busy;

      bool cancelling = false;

      for (sampleCount i = 0; i < fileTotalFrames; i += maxBlockSize) {
         sampleCount blockLen = maxBlockSize;
         if (i + blockLen > fileTotalFrames)
            blockLen = fileTotalFrames - i;

         for(c=0; c<*numChannels; c++)
            (*channels)[c]->AppendAlias(fName, i, blockLen, c);

         if (!progress && wxGetElapsedTime(false) > 500) {
            progress =
                new wxProgressDialog(_("Import"), progressStr,
                                     1000,
                                     parent,
                                     wxPD_CAN_ABORT |
                                     wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
         }
         if (progress) {
            cancelling = !progress->Update((int)((i*1000.0)/fileTotalFrames));

            if (cancelling)
               i = fileTotalFrames;
         }
      }

      //printf(_("Time elapsed: %d\n"), wxGetElapsedTime());

      if (progress)
         delete progress;

      if (cancelling) {
         for(c=0; c<*numChannels; c++)
            delete (*channels)[c];
         delete[] (*channels);
         *channels = NULL;

         return false;
      }

      return true;
   }

   // Otherwise, we're in the "copy" mode, where we read in the actual
   // samples from the file and store our own local copy of the
   // samples in the tracks.

   samplePtr srcbuffer = NewSamples(maxBlockSize * (*numChannels),
                                    format);
   samplePtr buffer = NewSamples(maxBlockSize, format);

   sampleCount framescompleted = 0;

   wxProgressDialog *progress = NULL;
   wxYield();
   wxStartTimer();
   wxBusyCursor busy;

   bool cancelling = false;

   long block;
   do {
      block = maxBlockSize;

      if (format == int16Sample)
         block = sf_readf_short(fp, (short *)srcbuffer, block);
      else
         block = sf_readf_float(fp, (float *)srcbuffer, block);

      if (block) {
         for(c=0; c<(*numChannels); c++) {

            if (format==int16Sample) {
               if (info.pcmbitwidth == 8) {
                  for(int j=0; j<block; j++)
                     ((short *)buffer)[j] =
                        ((short *)srcbuffer)[(*numChannels)*j+c] << 8;
               }
               else {
                  for(int j=0; j<block; j++)
                     ((short *)buffer)[j] =
                        ((short *)srcbuffer)[(*numChannels)*j+c];
               }
            }
            else
               for(int j=0; j<block; j++)
                  ((float *)buffer)[j] =
                     ((float *)srcbuffer)[(*numChannels)*j+c];

            (*channels)[c]->Append(buffer, format, block);
         }

         framescompleted += block;
      }

      if (!progress && wxGetElapsedTime(false) > 500) {
         progress =
            new wxProgressDialog(_("Import"), progressStr,
                                  1000,
                                  parent,
                                  wxPD_CAN_ABORT |
                                  wxPD_REMAINING_TIME | 
                                  wxPD_AUTO_HIDE);
      }
      if (progress) {
         int progressvalue = (framescompleted > fileTotalFrames) ?
             fileTotalFrames : framescompleted;

         cancelling =
            !progress->Update((int)((progressvalue*1000.0)/fileTotalFrames));

         if (cancelling)
            block = 0;
      }
   } while (block > 0);

   sf_close(fp);

   //printf("Time elapsed: %d\n", wxGetElapsedTime());

   if (progress)
      delete progress;

   DeleteSamples(srcbuffer);
   DeleteSamples(buffer);

   if (cancelling) {
      for(c=0; c<*numChannels; c++)
         delete (*channels)[c];
      delete[] (*channels);
      *channels = NULL;

      return false;
   }

   return true;
}

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 2e9db06a-fd0b-4af3-badd-eeb8437067e7

