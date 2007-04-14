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
#include "../Track.h"
#include "../WaveTrack.h"

#include "ExportPCM.h"

#ifdef __WXMAC__
#define __MOVIES__   /* Apple's Movies.h not compatible with Audacity */
/* #define __MACHELP__ */

#include <wx/mac/private.h>
# ifdef __UNIX__
#  include <CoreServices/CoreServices.h>
# else
# endif
#endif

static int ReadExportFormatPref()
{
   return gPrefs->Read(wxT("/FileFormats/ExportFormat_SF1"),
                       (long int)(SF_FORMAT_WAV | SF_FORMAT_PCM_16));
}

void WriteExportFormatPref(int format)
{
   gPrefs->Write(wxT("/FileFormats/ExportFormat_SF1"), (long int)format);
}

bool ExportPCM(AudacityProject *project,
               int numChannels, wxString fName,
               bool selectionOnly, double t0, double t1, MixerSpec *mixerSpec)
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

   GetActiveProject()->ProgressShow(_("E&xport"),
      selectionOnly ?
      wxString::Format(_("Exporting the selected audio as a %s file"),
                       formatStr.c_str()) :
      wxString::Format(_("Exporting the entire project as a %s file"),
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
                   (_("Error (file may not have been written): %hs"),
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

#define ID_FORMAT_CHOICE           7101
#define ID_HEADER_CHOICE           7102
#define ID_ENCODING_CHOICE         7103

class PCMOptionsDialog : public wxDialog
{
public:

   /// 
   /// 
   PCMOptionsDialog(wxWindow *parent)
   : wxDialog(NULL, wxID_ANY, wxString(_("Specify WAV Options")),
      wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
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

      GetSizer()->AddSpacer(5);
      Layout();
      Fit();
      Center();
   }

   /// 
   /// 
   void PopulateOrExchange(ShuttleGui & S)
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
      S.StartHorizontalLay(wxALIGN_CENTER, false);
      {
#if defined(__WXGTK20__) || defined(__WXMAC__)
         S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
         mOk = S.Id(wxID_OK).AddButton(_("&OK"));
#else
         mOk = S.Id(wxID_OK).AddButton(_("&OK"));
         S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
#endif
         mOk->SetDefault();
      }

      ValidateChoices();

      return;
   }

   ///
   ///
   void OnFormatChoice(wxCommandEvent & evt)
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
   void OnChoice(wxCommandEvent & event)
   {
      ValidateChoices();
   }

   /// 
   /// 
   void OnOK(wxCommandEvent& event)
   {
      WriteExportFormatPref(GetFormat());

      EndModal(wxID_OK);

      return;
   }

private:
   /// Calls a libsndfile library function to determine whether the user's
   /// choice of sample encoding (e.g. pcm 16-bit or GSM 6.10 compression)
   /// is compatible with their choice of file format (e.g. WAV, AIFF)
   /// and enables/disables the OK button accordingly.
   void ValidateChoices()
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

   int GetFormat()
   {
      return sf_header_index_to_type(mHeaderChoice->GetSelection()) |
             sf_encoding_index_to_subtype(mEncodingChoice->GetSelection());
   }

   wxArrayString mFormatNames;
   wxArrayString mHeaderNames;
   wxArrayString mEncodingNames;
   wxChoice *mFormatChoice;
   wxChoice *mHeaderChoice;
   wxChoice *mEncodingChoice;
   wxButton *mOk;
   int mFormat;
   int mFormatFromChoice;
   int mHeaderFromChoice;
   int mEncodingFromChoice;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(PCMOptionsDialog, wxDialog)
   EVT_CHOICE(ID_FORMAT_CHOICE,   PCMOptionsDialog::OnFormatChoice)
   EVT_CHOICE(ID_HEADER_CHOICE,   PCMOptionsDialog::OnChoice)
   EVT_CHOICE(ID_ENCODING_CHOICE, PCMOptionsDialog::OnChoice)
   EVT_BUTTON(wxID_OK,            PCMOptionsDialog::OnOK)
END_EVENT_TABLE()

bool ExportPCMOptions(AudacityProject *project)
{
   PCMOptionsDialog od(project);

   od.ShowModal();

   return true;
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

