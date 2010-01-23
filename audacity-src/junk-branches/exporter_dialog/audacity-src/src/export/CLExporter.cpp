/**********************************************************************

  Audacity: A Digital Audio Editor

  CLExporter.cpp

  (c) The Audacity Team

**********************************************************************/

#include "../Audacity.h"
#include "../FileFormats.h"
#include "../LabelTrack.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../SampleFormat.h"
#include "../Track.h"
#include "../WaveTrack.h"

#include "CLExporter.h"
#include "Exporter.h"

#include <wx/progdlg.h>


#include <iostream>

CLExporter::CLExporter(AudacityProject * project, double t0, double t1,
			 bool exportSelection, int rate,
			 int channels):
  Exporter(project, t0, t1, exportSelection, rate, channels)
{
  
}

CLExporter::~CLExporter()
{

}

bool CLExporter::Verify()
{
  return true;
}


bool CLExporter::Export(const wxString & filename)
{

  std::cout << "Exporting Command-line Format Not Implemented\n";
  mFileName = filename;
  int rate = int(mProject->GetRate() + 0.5);
   wxWindow *parent = mProject;
   TrackList *tracks = mProject->GetTracks();
   
   wxString command = gPrefs->Read("/FileFormats/ExternalProgramExportCommand", "lame - '%f'");
   command.Replace("%f", mFileName);

   /* establish parameters */
 
   unsigned long totalSamples = (unsigned long)((m_t1 - m_t0) * rate + 0.5);
   unsigned long sampleBytes = totalSamples * mChannels * SAMPLE_SIZE(int16Sample);

   /* fill up the wav header */
   wav_header header;
   header.riffID[0] = 'R';
   header.riffID[1] = 'I';
   header.riffID[2] = 'F';
   header.riffID[3] = 'F';
   header.riffType[0] = 'W';
   header.riffType[1] = 'A';
   header.riffType[2] = 'V';
   header.riffType[3] = 'E';
   header.lenAfterRiff = sampleBytes + 32;

   header.fmtID[0]  = 'f';
   header.fmtID[1]  = 'm';
   header.fmtID[2]  = 't';
   header.fmtID[3]  = ' ';
   header.formatChunkLen = 16;
   header.formatTag      = 1;
   header.channels       = mChannels;
   header.sampleRate     = rate;
   header.bitsPerSample  = SAMPLE_SIZE(int16Sample) * 8;
   header.blockAlign     = header.bitsPerSample * header.channels;
   header.avgBytesPerSec = header.sampleRate * header.blockAlign;

   header.dataID[0] = 'd';
   header.dataID[1] = 'a';
   header.dataID[2] = 't';
   header.dataID[3] = 'a';
   header.dataLen   = sampleBytes;

   FILE *pipe = popen(command.c_str(), "w");

   /* write the header */

   fwrite( &header, sizeof(wav_header), 1, pipe );

   sampleCount maxBlockLen = 44100 * 5;

   wxProgressDialog *progress = NULL;
   wxYield();
   wxStartTimer();
   wxBusyCursor busy;
   bool cancelling = false;

   int numWaveTracks;
   WaveTrack **waveTracks;
   tracks->GetWaveTracks(mExportSelection, &numWaveTracks, &waveTracks);
   Mixer *mixer = new Mixer(numWaveTracks, waveTracks,
                            tracks->GetTimeTrack(),
                            m_t0, m_t1,
                            mChannels, maxBlockLen, true,
                            rate, int16Sample);

   while(!cancelling) {
      sampleCount numSamples = mixer->Process(maxBlockLen);

      if (numSamples == 0)
         break;
      
      samplePtr mixed = mixer->GetBuffer();

      char *buffer = new char[numSamples * SAMPLE_SIZE(int16Sample) * mChannels];
      wxASSERT(buffer);

      // Byte-swapping is neccesary on big-endian machines, since
      // WAV files are little-endian
#if wxBYTE_ORDER == wxBIG_ENDIAN
      {
         short *buffer = (short*)mixed;
         for( int i = 0; i < numSamples; i++ )
            buffer[i] = wxINT16_SWAP_ON_BE(buffer[i]);
      }
#endif

      fwrite( mixed, numSamples * mChannels * SAMPLE_SIZE(int16Sample), 1, pipe );

      if (!progress && wxGetElapsedTime(false) > 500) {

         wxString message;

         if (mExportSelection)
            message = "Exporting the selected audio using command-line encoder";
         else
            message = "Exporting the entire project using command-line encoder";

         progress =
             new wxProgressDialog("Export",
                                  message,
                                  1000,
                                  parent,
                                  wxPD_CAN_ABORT |
                                  wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
      }
      if (progress) {
         int progressvalue = int (1000 * ((mixer->MixGetCurrentTime()-m_t0) /
                                          (m_t1-m_t0)));
         cancelling = !progress->Update(progressvalue);
      }

      delete[]buffer;
   }

   delete mixer;

   pclose( pipe );

   if(progress)
      delete progress;
   return true;
}
