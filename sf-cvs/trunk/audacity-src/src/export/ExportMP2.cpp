/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP2.cpp

  Joshua Haberman
  Markus Meyer

  Copyright 2002, 2003 Joshua Haberman.
  Copyright 2006 Markus Meyer
  Some portions may be Copyright 2003 Paolo Patruno.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*******************************************************************//**

\class MP2Exporter
\brief Class used to export MP2 files

*/

#include "../Audacity.h"

#ifdef USE_LIBTWOLAME

#include <wx/defs.h>
#include <wx/textctrl.h>
#include <wx/dynlib.h>
#include <wx/msgdlg.h>
#include <wx/utils.h>
#include <wx/progdlg.h>
#include <wx/timer.h>
#include <wx/window.h>
#include <wx/ffile.h>
#include <wx/log.h>
#include <wx/filedlg.h>
#include <wx/intl.h>

#include "ExportMP2.h"
#include "../Internat.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Tags.h"
#include "../WaveTrack.h"

#ifdef __WXMAC__
#define __MOVIES__
#include <wx/mac/private.h>
#ifdef __UNIX__
#include <CoreServices/CoreServices.h>
#else
#include <Files.h>
#endif
#endif

#define LIBTWOLAME_STATIC
#include "twolame.h"

bool ExportMP2(AudacityProject *project,
               int channels, wxString fName,
               bool selectionOnly, double t0, double t1, MixerSpec *mixerSpec)
{
   bool stereo = (channels == 2);
   long bitrate = gPrefs->Read(wxT("/FileFormats/MP2Bitrate"), 160);
   double rate = project->GetRate();
   TrackList *tracks = project->GetTracks();

   wxLogNull logNo;             /* temporarily disable wxWindows error messages */

   twolame_options *encodeOptions;
   encodeOptions = twolame_init();

   twolame_set_in_samplerate(encodeOptions, (int)(rate + 0.5));
   twolame_set_out_samplerate(encodeOptions, (int)(rate + 0.5));
   twolame_set_bitrate(encodeOptions, bitrate);
   twolame_set_num_channels(encodeOptions, stereo ? 2:1);

   if (twolame_init_params(encodeOptions) != 0)
   {
      wxMessageBox(_("Cannot export MP2 with this sample rate and bit rate"),
         _("Error"), wxICON_STOP);
      return false;
   }

   /* Put ID3 tags at beginning of file */
   /*lda Check ShowId3Dialog flag for CleanSpeech */
   bool showId3Dialog = project->GetShowId3Dialog();
   Tags *tags = project->GetTags();
   bool emptyTags = tags->IsEmpty();
   if (showId3Dialog && emptyTags) {
      if (!tags->ShowEditDialog(project,
                                _("Edit the ID3 tags for the MP3 file"),
                                true)) {
         return false;  // used selected "cancel"
      }
   }

   wxFFile outFile(fName, wxT("wb"));
   if (!outFile.IsOpened()) {
      wxMessageBox(_("Unable to open target file for writing"));
      return false;
   }

   char *id3buffer = NULL;
   int id3len;
   bool endOfFile;
   id3len = tags->ExportID3(&id3buffer, &endOfFile);
   if (!endOfFile)
     outFile.Write(id3buffer, id3len);

   // Values taken from the twolame simple encoder sample
   const int pcmBufferSize = 16384 / 2; // number of samples
   const int mp2BufferSize = 9216; // bytes

   // We allocate a buffer which is twice as big as the
   // input buffer, which should always be enough.
   // We have to multiply by 4 because one sample is 2 bytes wide!
   unsigned char* mp2Buffer = new unsigned char[mp2BufferSize];

   int numWaveTracks;
   WaveTrack **waveTracks;
   tracks->GetWaveTracks(selectionOnly, &numWaveTracks, &waveTracks);
   Mixer *mixer = new Mixer(numWaveTracks, waveTracks,
                            tracks->GetTimeTrack(),
                            t0, t1,
                            stereo? 2: 1, pcmBufferSize, true,
                            rate, int16Sample, true, mixerSpec);

   GetActiveProject()->ProgressShow(selectionOnly ?
      wxString::Format(_("Exporting selected audio at %d kbps"), bitrate) :
      wxString::Format(_("Exporting entire file at %d kbps"), bitrate),
      wxFileName(fName).GetName());

   bool cancelling = false;
   while(!cancelling) {
      sampleCount pcmNumSamples = mixer->Process(pcmBufferSize);

      if (pcmNumSamples == 0)
         break;
      
      short *pcmBuffer = (short *)mixer->GetBuffer();

      int mp2BufferNumBytes = twolame_encode_buffer_interleaved(
            encodeOptions,
            pcmBuffer,
            pcmNumSamples,
            mp2Buffer,
            mp2BufferSize);

      outFile.Write(mp2Buffer, mp2BufferNumBytes);

      int progressvalue = int (1000 * ((mixer->MixGetCurrentTime()-t0) /
                                          (t1-t0)));
      cancelling = !GetActiveProject()->ProgressUpdate(progressvalue);
   }

   GetActiveProject()->ProgressHide();

   delete mixer;

	int mp2BufferNumBytes = twolame_encode_flush(
      encodeOptions,
      mp2Buffer,
      mp2BufferSize);

   if (mp2BufferNumBytes > 0)
      outFile.Write(mp2Buffer, mp2BufferNumBytes);

   twolame_close(&encodeOptions);

   delete[] mp2Buffer;

   /* Write ID3 tag if it was supposed to be at the end of the file */
   
   if (endOfFile)
      outFile.Write(id3buffer, id3len);
   free(id3buffer);

   /* Close file */
   
   outFile.Close();

   /* MacOS: set the file type/creator so that the OS knows it's an MP2
      file which was created by Audacity */
      
#ifdef __WXMAC__
   FSSpec spec;
   wxMacFilename2FSSpec(fName, &spec);
   FInfo finfo;
   if (FSpGetFInfo(&spec, &finfo) == noErr) {
      finfo.fdType = 'MP2 ';
      finfo.fdCreator = AUDACITY_CREATOR;

      FSpSetFInfo(&spec, &finfo);
   }
#endif

   return !cancelling;
}

static int iBitrates[] = {
   16, 24, 32, 40, 48, 56, 64,
   80, 96, 112, 128, 160,
   192, 224, 256, 320 
};

class MP2OptionsDialog : public wxDialog
{
public:

   /// 
   /// 
   MP2OptionsDialog(wxWindow *parent)
   : wxDialog(NULL, wxID_ANY, wxString(_("Specify MP2 Options")),
      wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
   {
      ShuttleGui S(this, eIsCreatingFromPrefs);

      for (unsigned int i=0; i < (sizeof(iBitrates)/sizeof(int)); i++)
      {
         mBitRateNames.Add(wxString::Format(wxT("%i"),iBitrates[i]));
         mBitRateLabels.Add(iBitrates[i]);
      }

      PopulateOrExchange(S);
   }

   /// 
   /// 
   void PopulateOrExchange(ShuttleGui & S)
   {
      S.StartHorizontalLay(wxEXPAND, 0);
      {
         S.StartStatic(_("MP2 Export Setup"), 0);
         {
            S.StartTwoColumn();
            {
               S.TieChoice(_("Bit Rate:"), wxT("/FileFormats/MP2Bitrate"), 
                  160, mBitRateNames, mBitRateLabels);
            }
            S.EndTwoColumn();
         }
         S.EndStatic();
      }
      S.EndHorizontalLay();
      S.StartHorizontalLay(wxALIGN_CENTER, false);
      {
#if defined(__WXGTK20__) || defined(__WXMAC__)
         S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
         S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
#else
         S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
         S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
#endif
      }
      GetSizer()->AddSpacer(5);
      Layout();
      Fit();
      SetMinSize(GetSize());
      Center();

      return;
   }

   /// 
   /// 
   void OnOK(wxCommandEvent& event)
   {
      ShuttleGui S(this, eIsSavingToPrefs);
      PopulateOrExchange(S);

      wxDialog::OnOK(event);

      return;
   }

private:
   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(MP2OptionsDialog, wxDialog)
   EVT_BUTTON(wxID_OK, MP2OptionsDialog::OnOK)
END_EVENT_TABLE()

bool ExportMP2Options(AudacityProject *project)
{
   MP2OptionsDialog od(project);

   od.ShowModal();

   return true;
}

#endif // #ifdef USE_LIBTWOLAME

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: c6af56b1-37fa-4d95-b982-0a24b3a49c00

