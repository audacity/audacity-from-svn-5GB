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

#define SAMPLES_PER_RUN 8192

bool ExportFLAC(AudacityProject *project,
               int numChannels, wxString fName,
               bool selectionOnly, double t0, double t1, MixerSpec *mixerSpec)
{
   double    rate    = project->GetRate();
   wxWindow  *parent = project;
   TrackList *tracks = project->GetTracks();

   wxLogNull logNo;            // temporarily disable wxWindows error messages 
   int       eos = 0;
   bool      cancelling = false;
   
   wxString bitDepthPref =
      gPrefs->Read(wxT("/FileFormats/FLACBitDepth"), wxT("16"));

   FLAC::Encoder::File *encoder= new FLAC::Encoder::File();
   encoder->set_filename(fName.mb_str());
   encoder->set_channels(numChannels);
   encoder->set_sample_rate(int(rate + 0.5));
   sampleFormat format;
   if(bitDepthPref == wxT("24")){
   	format=int24Sample;
   	encoder->set_bits_per_sample(24);
   }else {//convert float to 16 bits*/
    	format=int16Sample;
   	encoder->set_bits_per_sample(16);
   }
   encoder->init();

   wxProgressDialog *progress = NULL;

   wxYield();
   wxStartTimer();

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
      if(progress) {
         int progressvalue = int (1000 * ((mixer->MixGetCurrentTime()-t0) /
                                          (t1-t0)));
         cancelling = !progress->Update(progressvalue);
      }
      else if(wxGetElapsedTime(false) > 500) {
            
         wxString message = selectionOnly ?
            _("Exporting the selected audio as FLAC") :
            _("Exporting the entire project as FLAC");

         progress = new wxProgressDialog(
               _("Export"),
               message,
               1000,
               parent,
               wxPD_CAN_ABORT | wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
      }
   }
   encoder->finish();
   for(i=0;i<numChannels;i++)
   	free(tmpsmplbuf[i]);
   delete mixer;
   delete encoder;

   if(progress)
      delete progress;

   delete[] tmpsmplbuf;

   return !cancelling;
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


