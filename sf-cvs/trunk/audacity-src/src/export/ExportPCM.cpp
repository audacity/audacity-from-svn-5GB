/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPCM.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/timer.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/string.h>
#include <wx/textctrl.h>
#include <wx/window.h>

#include "sndfile.h"

#include "../Audacity.h"
#include "../FileFormats.h"
#include "../Internat.h"
#include "../LabelTrack.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Tags.h"
#include "../Track.h"
#include "../WaveTrack.h"

#include "Export.h"

#ifdef __WXMAC__
#define __MOVIES__   /* Apple's Movies.h not compatible with Audacity */
/* #define __MACHELP__ */

#include <wx/mac/private.h>
# ifdef __UNIX__
#  include <CoreServices/CoreServices.h>
# else
# endif
#endif

//----------------------------------------------------------------------------
// Statics
//----------------------------------------------------------------------------

static int ReadExportFormatPref()
{
   return gPrefs->Read(wxT("/FileFormats/ExportFormat_SF1"),
#if defined(__WXMAC__)
                       (long int)(SF_FORMAT_AIFF | SF_FORMAT_PCM_16));
#else
                       (long int)(SF_FORMAT_WAV | SF_FORMAT_PCM_16));
#endif
}

static void WriteExportFormatPref(int format)
{
   gPrefs->Write(wxT("/FileFormats/ExportFormat_SF1"), (long int)format);
}

//----------------------------------------------------------------------------
// ExportPCMOptions Class
//----------------------------------------------------------------------------

#define ID_FORMAT_CHOICE           7101
#define ID_HEADER_CHOICE           7102
#define ID_ENCODING_CHOICE         7103

class ExportPCMOptions : public wxDialog
{
public:

   ExportPCMOptions(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnFormatChoice(wxCommandEvent & evt);
   void OnChoice(wxCommandEvent & event);
   void OnOK(wxCommandEvent& event);

private:

   void ValidateChoices();
   int GetFormat();

private:

   wxArrayString mFormatNames;
   wxArrayString mHeaderNames;
   wxArrayString mEncodingNames;
   wxChoice *mFormatChoice;
   wxChoice *mHeaderChoice;
   wxChoice *mEncodingChoice;
   wxButton *mOk;
   int mFormatFromChoice;
   int mHeaderFromChoice;
   int mEncodingFromChoice;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportPCMOptions, wxDialog)
   EVT_CHOICE(ID_FORMAT_CHOICE,   ExportPCMOptions::OnFormatChoice)
   EVT_CHOICE(ID_HEADER_CHOICE,   ExportPCMOptions::OnChoice)
   EVT_CHOICE(ID_ENCODING_CHOICE, ExportPCMOptions::OnChoice)
   EVT_BUTTON(wxID_OK,            ExportPCMOptions::OnOK)
END_EVENT_TABLE()

/// 
/// 
ExportPCMOptions::ExportPCMOptions(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify Uncompressed Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
{
   int format = ReadExportFormatPref();

   //----- Gather our strings used in choices.

   int i;
   int num = sf_num_simple_formats();
   int sel = num;
   for (i = 0; i < num; i++) {
      SF_FORMAT_INFO *info = sf_simple_format(i);
      mFormatNames.Add(LAT1CTOWX(info->name));
      if (format == info->format) {
         sel = i;
      }
   }
   mFormatNames.Add(_("Other..."));
   mFormatFromChoice = sel;

   num = sf_num_headers();
   sel = 0;
   for (i = 0; i < num; i++) {
      mHeaderNames.Add(sf_header_index_name(i));
      if ((format & SF_FORMAT_TYPEMASK) == sf_header_index_to_type(i)) {
         sel = i;
      }
   }
   mHeaderFromChoice = sel;

   num = sf_num_encodings();
   sel = 0;
   for (i = 0; i < num; i++) {
      mEncodingNames.Add(sf_encoding_index_name(i));
      if ((format & SF_FORMAT_SUBMASK) == sf_encoding_index_to_subtype(i)) {
         sel = i;
      }
   }
   mEncodingFromChoice = sel;

   ShuttleGui S(this, eIsCreatingFromPrefs);

   PopulateOrExchange(S);

   Layout();
   Fit();
   Center();
}

/// 
/// 
void ExportPCMOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, true);
   {
      S.StartStatic(_("Uncompressed Export Setup"), true);
      {
         S.StartMultiColumn(2, wxEXPAND);
         {
            S.SetStretchyCol(1);
            mFormatChoice = S.Id(ID_FORMAT_CHOICE)
               .AddChoice(_("Format:"),
                          mFormatNames[mFormatFromChoice],
                          &mFormatNames);
            mHeaderChoice = S.Id(ID_HEADER_CHOICE)
               .AddChoice(_("Header:"),
                          mHeaderNames[mHeaderFromChoice],
                          &mHeaderNames);
            mEncodingChoice = S.Id(ID_ENCODING_CHOICE)
               .AddChoice(_("Encoding:"),
                          mEncodingNames[mEncodingFromChoice],
                          &mEncodingNames);
         }
         S.EndMultiColumn();
         S.AddFixedText(_("(Not all combinations of headers and encodings are possible.)"));
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();
   mOk = (wxButton *)wxWindow::FindWindowById(wxID_OK, this);

   ValidateChoices();

   return;
}

///
///
void ExportPCMOptions::OnFormatChoice(wxCommandEvent & evt)
{
   int format = sf_simple_format(mFormatChoice->GetSelection())->format;
   int num;
   int i;

   num = sf_num_headers();
   for (i = 0; i < num; i++) {
      if ((format & SF_FORMAT_TYPEMASK) == sf_header_index_to_type(i)) {
         mHeaderChoice->SetSelection(i);
         break;
      }
   }

   num = sf_num_encodings();
   for (i = 0; i < num; i++) {
      if ((format & SF_FORMAT_SUBMASK) == sf_encoding_index_to_subtype(i)) {
         mEncodingChoice->SetSelection(i);
         break;
      }
   }

   ValidateChoices();
}

///
///
void ExportPCMOptions::OnChoice(wxCommandEvent & event)
{
   ValidateChoices();
}

/// 
/// 
void ExportPCMOptions::OnOK(wxCommandEvent& event)
{
   WriteExportFormatPref(GetFormat());

   EndModal(wxID_OK);

   return;
}

/// Calls a libsndfile library function to determine whether the user's
/// choice of sample encoding (e.g. pcm 16-bit or GSM 6.10 compression)
/// is compatible with their choice of file format (e.g. WAV, AIFF)
/// and enables/disables the OK button accordingly.
void ExportPCMOptions::ValidateChoices()
{
   SF_INFO info;
   memset(&info, 0, sizeof(info));
   info.frames = 0;
   info.samplerate = 44100;
   info.channels = 1;
   info.format = GetFormat();
   info.sections = 1;
   info.seekable = 0;

   mOk->Enable(sf_format_check(&info) ? true : false);

   bool enable = mFormatChoice->GetSelection() ==
                 (mFormatChoice->GetCount() - 1);

   mHeaderChoice->Enable(enable);
   mEncodingChoice->Enable(enable);
}

int ExportPCMOptions::GetFormat()
{
   return sf_header_index_to_type(mHeaderChoice->GetSelection()) |
          sf_encoding_index_to_subtype(mEncodingChoice->GetSelection());
}

//----------------------------------------------------------------------------
// ExportPCM Class
//----------------------------------------------------------------------------

class ExportPCM : public ExportPlugin
{
public:

   ExportPCM();
   void Destroy();

   // Required

   bool DisplayOptions(AudacityProject *project = NULL);
   bool Export(AudacityProject *project,
               int channels,
               wxString fName,
               bool selectedOnly,
               double t0,
               double t1,
               MixerSpec *mixerSpec = NULL,
               Tags *metadata = NULL); 

   // Overrides

   int GetMaxChannels();
   wxString GetExtension();
   wxArrayString GetExtensions();
   bool IsExtension(wxString & ext);

private:

   bool AddStrings(AudacityProject *project, SNDFILE *sf, Tags *tags);

};

ExportPCM::ExportPCM()
:  ExportPlugin()
{
   SetFormat(wxT("WAV"));
   SetCanMetaData(true);
   SetDescription(_("WAV, AIFF, and other uncompressed types"));
}

void ExportPCM::Destroy()
{
   delete this;
}

bool ExportPCM::Export(AudacityProject *project,
                       int numChannels,
                       wxString fName,
                       bool selectionOnly,
                       double t0,
                       double t1,
                       MixerSpec *mixerSpec,
                       Tags *metadata) 
{
   double       rate = project->GetRate();
   TrackList   *tracks = project->GetTracks();
   int          sf_format = ReadExportFormatPref();
   wxString     formatStr;
   SF_INFO      info;
   SNDFILE     *sf = NULL;
   int          err;

   formatStr = sf_header_name(sf_format & SF_FORMAT_TYPEMASK);

   // Use libsndfile to export file

   info.samplerate = (unsigned int)(rate + 0.5);
   info.frames = (unsigned int)((t1 - t0)*rate + 0.5);
   info.channels = numChannels;
   info.format = sf_format;
   info.sections = 1;
   info.seekable = 0;

   // If we can't export exactly the format they requested,
   // try the default format for that header type...
   if (!sf_format_check(&info))
      info.format = (info.format & SF_FORMAT_TYPEMASK);
   if (!sf_format_check(&info)) {
      wxMessageBox(_("Cannot export audio in this format."));
      return false;
   }

   sf = sf_open(OSFILENAME(fName), SFM_WRITE, &info);
   if (!sf) {
      wxMessageBox(wxString::Format(_("Cannot export audio to %s"),
                                    fName.c_str()));
      return false;
   }
   // Retrieve tags if not given a set
   if (metadata == NULL)
      metadata = project->GetTags();

   if (!AddStrings(project, sf, metadata)) { // meta data presence check
      sf_close(sf);
      return false;
   }

   sampleFormat format;
   if (sf_subtype_more_than_16_bits(info.format))
      format = floatSample;
   else
      format = int16Sample;

   int maxBlockLen = 44100 * 5;

   bool cancelling = false;

   int numWaveTracks;
   WaveTrack **waveTracks;
   tracks->GetWaveTracks(selectionOnly, &numWaveTracks, &waveTracks);
   Mixer *mixer = new Mixer(numWaveTracks, waveTracks,
                            tracks->GetTimeTrack(),
                            t0, t1,
                            info.channels, maxBlockLen, true,
                            rate, format, true, mixerSpec);

   GetActiveProject()->ProgressShow(wxFileName(fName).GetName(),
      selectionOnly ?
      wxString::Format(_("Exporting the selected audio as %s"),
                       formatStr.c_str()) :
      wxString::Format(_("Exporting the entire project as %s"),
                       formatStr.c_str()));

   while(!cancelling) {
      sampleCount numSamples = mixer->Process(maxBlockLen);

      if (numSamples == 0)
         break;
      
      samplePtr mixed = mixer->GetBuffer();

      if (format == int16Sample)
         sf_writef_short(sf, (short *)mixed, numSamples);
      else
         sf_writef_float(sf, (float *)mixed, numSamples);

      int progressvalue = int (1000 * ((mixer->MixGetCurrentTime()-t0) /
                                       (t1-t0)));
      cancelling = !GetActiveProject()->ProgressUpdate(progressvalue);
   }

   GetActiveProject()->ProgressHide();

   delete mixer;

   delete[] waveTracks;                            

   err = sf_close(sf);

   if (err) {
      char buffer[1000];
      sf_error_str(sf, buffer, 1000);
      wxMessageBox(wxString::Format
            /* i18n-hint: %s will be the error message from libsndfile */
                   (_("Error (file may not have been written): %s"),
                    buffer));
   }

#ifdef __WXMAC__

   FSSpec spec;

   wxMacFilename2FSSpec(fName, &spec);

   FInfo finfo;
   if (FSpGetFInfo(&spec, &finfo) == noErr) {
      finfo.fdType = sf_header_mactype(sf_format & SF_FORMAT_TYPEMASK);
      finfo.fdCreator = AUDACITY_CREATOR;

      FSpSetFInfo(&spec, &finfo);
   }
#endif

   return !cancelling;
}

bool ExportPCM::AddStrings(AudacityProject *project, SNDFILE *sf, Tags *tags)
{
   if (tags->HasTag(TAG_TITLE)) {
      sf_set_string(sf, SF_STR_TITLE, tags->GetTag(TAG_TITLE).mb_str(wxConvUTF8));
   }

   if (tags->HasTag(TAG_ARTIST)) {
      sf_set_string(sf, SF_STR_ARTIST, tags->GetTag(TAG_ARTIST).mb_str(wxConvUTF8));
   }

   if (tags->HasTag(TAG_COMMENTS)) {
      sf_set_string(sf, SF_STR_COMMENT, tags->GetTag(TAG_COMMENTS).mb_str(wxConvUTF8));
   }

   if (tags->HasTag(TAG_YEAR)) {
      sf_set_string(sf, SF_STR_DATE, tags->GetTag(TAG_YEAR).mb_str(wxConvUTF8));
   }

   if (tags->HasTag(wxT("Copyright"))) {
      sf_set_string(sf, SF_STR_COPYRIGHT, tags->GetTag(wxT("Copyright")).mb_str(wxConvUTF8));
   }

   if (tags->HasTag(wxT("Software"))) {
      sf_set_string(sf, SF_STR_SOFTWARE, tags->GetTag(wxT("Software")).mb_str(wxConvUTF8));
   }

   return true;
}

bool ExportPCM::DisplayOptions(AudacityProject *project)
{
   ExportPCMOptions od(project);

   od.ShowModal();

   return true;
}

int ExportPCM::GetMaxChannels()
{
   SF_INFO si;

   si.format = ReadExportFormatPref();
   si.samplerate = 0;
   si.channels = 0;

   do {
      si.channels++;
   } while (sf_format_check(&si));

   return si.channels - 1;
}

wxString ExportPCM::GetExtension()
{
   return sf_header_extension(ReadExportFormatPref());
}

wxArrayString ExportPCM::GetExtensions()
{
   return sf_get_all_extensions();
}

bool ExportPCM::IsExtension(wxString & ext)
{
   return ext.IsSameAs(GetExtension(), false);
}

//----------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------
ExportPlugin *New_ExportPCM()
{
   return new ExportPCM();
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: c1f32472-520f-4864-8086-3dba0d593e84
