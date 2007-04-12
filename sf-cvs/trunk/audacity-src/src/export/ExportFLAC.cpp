/**********************************************************************

Audacity: A Digital Audio Editor

ExportFLAC.cpp

Frederik M.J.V

This program is distributed under the GNU General Public License, version 2.
A copy of this license is included with this source.

Based on ExportOGG.cpp by:
Joshua Haberman

Portions from vorbis-tools, copyright 2000-2002 Michael Smith
<msmith@labyrinth.net.au>; Vorbize, Kenneth Arnold <kcarnold@yahoo.com>;
and libvorbis examples, Monty <monty@xiph.org>

**********************************************************************/

#include "../Audacity.h"

#ifdef USE_LIBFLAC

#include "ExportFLAC.h"

#include <wx/progdlg.h>
#include <wx/ffile.h>
#include <wx/log.h>
#include <wx/msgdlg.h>

#include <vorbis/vorbisenc.h>
#include "FLAC++/encoder.h"

#include "../Project.h"
#include "../Mix.h"
#include "../Prefs.h"

#include "../Internat.h"
#include "../Tags.h"

#define SAMPLES_PER_RUN 8192

bool ExportFLAC(AudacityProject *project,
                int numChannels, wxString fName,
                bool selectionOnly, double t0, double t1, MixerSpec *mixerSpec)
{
   double    rate    = project->GetRate();
   TrackList *tracks = project->GetTracks();
   
   wxLogNull logNo;            // temporarily disable wxWindows error messages 
   int       eos = 0;
   bool      cancelling = false;

   Tags *tags = project->GetTags();

   wxString bitDepthPref =
      gPrefs->Read(wxT("/FileFormats/FLACBitDepth"), wxT("16"));
   
   FLAC::Encoder::File *encoder= new FLAC::Encoder::File();
   encoder->set_filename(OSFILENAME(fName));
   encoder->set_channels(numChannels);
   encoder->set_sample_rate(int(rate + 0.5));

   tags->ExportFLACTags(encoder);
   
   sampleFormat format;
   if(bitDepthPref == wxT("24")){
   	format=int24Sample;
   	encoder->set_bits_per_sample(24);
   }else {//convert float to 16 bits*/
    	format=int16Sample;
   	encoder->set_bits_per_sample(16);
   }
   encoder->init();
   
   int numWaveTracks;
   WaveTrack **waveTracks;
   tracks->GetWaveTracks(selectionOnly, &numWaveTracks, &waveTracks);
   Mixer *mixer = new Mixer(numWaveTracks, waveTracks,
                            tracks->GetTimeTrack(),
                            t0, t1,
                            numChannels, SAMPLES_PER_RUN, false,
                            rate, format, true, mixerSpec);
   
   int i,j;
   i=j=0;
   FLAC__int32 **tmpsmplbuf = new FLAC__int32*[numChannels];
   for(i=0;i<numChannels;i++)
   	tmpsmplbuf[i]=(FLAC__int32*)calloc(SAMPLES_PER_RUN,sizeof(FLAC__int32));

   GetActiveProject()->ProgressShow(selectionOnly ?
                                    _("Exporting the selected audio as FLAC") :
                                    _("Exporting the entire project as FLAC"),
                                    wxFileName(fName).GetName());

   while(!cancelling && !eos) {
      sampleCount samplesThisRun = mixer->Process(SAMPLES_PER_RUN);
      if (samplesThisRun == 0) {//stop encoding
         break;
      }
      else {
         for(i=0;i<numChannels;i++){
            samplePtr mixed = mixer->GetBuffer(i);
            for(j=0;j<samplesThisRun;j++){
               if(format==int24Sample)
                  tmpsmplbuf[i][j]= ((int*)mixed)[j];
               else
                  tmpsmplbuf[i][j]= ((short*)mixed)[j];
            }
         }
         encoder->process(tmpsmplbuf,samplesThisRun);
      }
      int progressvalue = int (1000 * ((mixer->MixGetCurrentTime()-t0) /
                                       (t1-t0)));
      cancelling = !GetActiveProject()->ProgressUpdate(progressvalue);
   }
   encoder->finish();

   GetActiveProject()->ProgressHide();

   for(i=0;i<numChannels;i++)
   	free(tmpsmplbuf[i]);
   delete mixer;
   delete encoder;
   
   delete[] tmpsmplbuf;
   
   return !cancelling;
}

class FLACOptionsDialog : public wxDialog
{
public:

   /// 
   /// 
   FLACOptionsDialog(wxWindow *parent)
   : wxDialog(NULL, wxID_ANY, wxString(_("Specify FLAC Options")),
      wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
   {
      ShuttleGui S(this, eIsCreatingFromPrefs);

      PopulateOrExchange(S);
   }

   /// 
   /// 
   void PopulateOrExchange(ShuttleGui & S)
   {
      wxArrayString flacBitDepthNames, flacBitDepthLabels;
      flacBitDepthLabels.Add(wxT("16")); flacBitDepthNames.Add(_("16 bit"));
      flacBitDepthLabels.Add(wxT("24")); flacBitDepthNames.Add(_("24 bit"));

      S.StartHorizontalLay(wxEXPAND, 0);
      {
         S.StartStatic(_("FLAC Export Setup"), 0);
         {
            S.StartTwoColumn();
            {
               S.TieChoice(_("Bit depth:"), wxT("/FileFormats/FLACBitDepth"),
                           wxT("16"), flacBitDepthNames, flacBitDepthLabels);
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

      EndModal(wxID_OK);

      return;
   }

private:

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(FLACOptionsDialog, wxDialog)
   EVT_BUTTON(wxID_OK, FLACOptionsDialog::OnOK)
END_EVENT_TABLE()

bool ExportFLACOptions(AudacityProject *project)
{
   FLACOptionsDialog od(project);

   od.ShowModal();

   return true;
}

#endif // USE_LIBVORBIS


// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3


