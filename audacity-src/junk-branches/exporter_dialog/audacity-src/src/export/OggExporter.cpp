/**********************************************************************

  Audacity: A Digital Audio Editor

  OggExporter.cpp

  (c) The Audacity Team

**********************************************************************/

#include "OggExporter.h"
#include "Exporter.h"


#include "../Audacity.h"
#include "../FileFormats.h"
#include "../LabelTrack.h"
#include "../Mix.h"
#include "../Project.h"
#include "../SampleFormat.h"
#include "../Track.h"
#include "../WaveTrack.h"

#include <vorbis/vorbisenc.h>



#include <wx/progdlg.h>
#include <wx/ffile.h>
#include <wx/log.h>
#include <wx/msgdlg.h>

#include <iostream>

OggExporter::OggExporter(AudacityProject * project, double t0, double t1,
			 bool exportSelection, int rate,
			 int channels, double quality):
  Exporter(project, t0, t1, exportSelection, rate, channels) 
{

  mQuality = quality;
  
}

OggExporter::~OggExporter()
{

}

bool OggExporter::Verify()
{
  return true;
}


bool OggExporter::Export(const wxString & filename)
{


  mFileName = filename;

   wxLogNull logNo;            // temporarily disable wxWindows error messages 
   bool      cancelling = false;

   wxFFile outFile(mFileName, "wb");

   if(!outFile.IsOpened()) {
      wxMessageBox(_("Unable to open target file for writing"));
      return false;
   }

   // All the Ogg and Vorbis encoding data
   ogg_stream_state stream;
   ogg_page         page;
   ogg_packet       packet;

   vorbis_info      info;
   vorbis_comment   comment;
   vorbis_dsp_state dsp;
   vorbis_block     block;

   // Encoding setup
   vorbis_info_init(&info);
   vorbis_encode_init_vbr(&info, mChannels, (int)(mInRate+.5), mQuality);

   vorbis_comment_init(&comment);
   // If we wanted to add comments, we would do it here

   // Set up analysis state and auxiliary encoding storage
   vorbis_analysis_init(&dsp, &info);
   vorbis_block_init(&dsp, &block);

   // Set up packet->stream encoder.  According to encoder example,
   // a random serial number makes it more likely that you can make
   // chained streams with concatenation.
   srand(time(NULL));
   ogg_stream_init(&stream, rand());

   // First we need to write the required headers:
   //    1. The Ogg bitstream header, which contains codec setup params
   //    2. The Vorbis comment header
   //    3. The bitstream codebook.
   //
   // After we create those our responsibility is complete, libvorbis will
   // take care of any other ogg bistream constraints (again, according
   // to the example encoder source)
   ogg_packet bitstream_header;
   ogg_packet comment_header;
   ogg_packet codebook_header;

   vorbis_analysis_headerout(&dsp, &comment, &bitstream_header, &comment_header,
         &codebook_header);

   // Place these headers into the stream
   ogg_stream_packetin(&stream, &bitstream_header);
   ogg_stream_packetin(&stream, &comment_header);
   ogg_stream_packetin(&stream, &codebook_header);

   // Flushing these headers now guarentees that audio data will
   // start on a new page, which apparently makes streaming easier
   ogg_stream_flush(&stream, &page);
   outFile.Write(page.header, page.header_len);
   outFile.Write(page.body, page.body_len);

   wxProgressDialog *progress = NULL;

   wxYield();
   wxStartTimer();

   int numWaveTracks;
   WaveTrack **waveTracks;
   TrackList *tracks = mProject->GetTracks();
   tracks->GetWaveTracks(mExportSelection, &numWaveTracks, &waveTracks);
   Mixer *mixer = new Mixer(numWaveTracks, waveTracks,
                            tracks->GetTimeTrack(),
                            m_t0, m_t1,
                            mChannels, SAMPLES_PER_RUN, false,
                            mInRate, floatSample);

   while(!cancelling) {
      sampleCount samplesThisRun = mixer->Process(SAMPLES_PER_RUN);

      if (samplesThisRun == 0)
         break;
      
      float **vorbis_buffer = vorbis_analysis_buffer(&dsp, SAMPLES_PER_RUN);
      
      float *left = (float *)mixer->GetBuffer(0);
      memcpy(vorbis_buffer[0], left, sizeof(float)*SAMPLES_PER_RUN);

      if(mChannels==2) {
         float *right = (float *)mixer->GetBuffer(1);
         memcpy(vorbis_buffer[1], right, sizeof(float)*SAMPLES_PER_RUN);
      }

      // tell the encoder how many samples we have
      vorbis_analysis_wrote(&dsp, samplesThisRun);

      // I don't understand what this call does, so here is the comment
      // from the example, verbatim:
      //
      //    vorbis does some data preanalysis, then divvies up blocks
      //    for more involved (potentially parallel) processing. Get
      //    a single block for encoding now
      while(vorbis_analysis_blockout(&dsp, &block) == 1) {

         // analysis, assume we want to use bitrate management
         vorbis_analysis(&block, NULL);
         vorbis_bitrate_addblock(&block);

         while(vorbis_bitrate_flushpacket(&dsp, &packet)) {

            // add the packet to the bitstream
            ogg_stream_packetin(&stream, &packet);
            int result = ogg_stream_pageout(&stream, &page);

            if(result != 0) {
               outFile.Write(page.header, page.header_len);
               outFile.Write(page.body, page.body_len);
            }
         }
      }

      if(progress) {
         int progressvalue = int (1000 * ((mixer->MixGetCurrentTime()-m_t0) /
                                          (m_t1-m_t0)));
         cancelling = !progress->Update(progressvalue);
      }
      else if(wxGetElapsedTime(false) > 500) {
            
         wxString message = mExportSelection ?
            _("Exporting the selected audio as Ogg Vorbis") :
            _("Exporting the entire project as Ogg Vorbis");

         progress = new wxProgressDialog(
               _("Export"),
               message,
               1000,
               mProject,
               wxPD_CAN_ABORT | wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
      }
   }

   delete mixer;

   outFile.Close();

   if(progress)
      delete progress;

   return true;

}
