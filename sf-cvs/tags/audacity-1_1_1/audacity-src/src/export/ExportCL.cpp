/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportCL.cpp

  Joshua Haberman

  This code allows Audacity to export data by piping it to an external
  program. Although it could in theory be made to work on Windows
  (and perhaps even Mac), it's not worth it to invest the effort because
  commandline encoders are rarely installed there.

**********************************************************************/

#ifdef __WXGTK__

#include <stdio.h>
#include <wx/progdlg.h>

#include "../Project.h"
#include "../Mix.h"
#include "../Prefs.h"

/* this structure combines the RIFF header, the format chunk, and the data
 * chunk header */
struct wav_header {
   /* RIFF header */
   char riffID[4];            /* "RIFF" */
   wxUint32 lenAfterRiff;     /* basically the file len - 8, or samples len + 32 */
   char riffType[4];          /* "WAVE" */
   
   /* format chunk */
   char fmtID[4];             /* "fmt " */
   wxUint32 formatChunkLen;   /* (format chunk len - first two fields) 16 in our case */
   wxUint16 formatTag;        /* 1 for PCM */
   wxUint16 channels;
   wxUint32 sampleRate;
   wxUint32 avgBytesPerSec;   /* sampleRate * blockAlign */
   wxUint16 blockAlign;       /* bitsPerSample * channels (assume bps % 8 = 0) */
   wxUint16 bitsPerSample;

   /* data chunk header */
   char dataID[4];            /* "data" */
   wxUint32 dataLen;          /* length of all samples in bytes */
};

bool ExportCL(AudacityProject *project, bool stereo, wxString fName,
              bool selectionOnly, double t0, double t1)
{
   int rate = int(project->GetRate() + 0.5);
   wxWindow *parent = project;
   TrackList *tracks = project->GetTracks();
   
   wxString command = gPrefs->Read("/FileFormats/ExternalProgramExportCommand", "lame - '%f'");
   command.Replace("%f", fName);

   /* establish parameters */
   int channels = stereo ? 2 : 1;
   unsigned long totalSamples = (unsigned long)((t1 - t0) * rate + 0.5);
   unsigned long sampleBytes = totalSamples * channels * SAMPLE_SIZE(int16Sample);
   double timeStep = 10.0;      // write in blocks of 10 secs

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
   header.channels       = channels;
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

   //sampleCount maxSamples = int (timeStep * rate + 0.5);

   wxProgressDialog *progress = NULL;
   wxYield();
   wxStartTimer();
   wxBusyCursor busy;
   bool cancelling = false;

   double t = t0;

   while (t < t1 && !cancelling) {

      double deltat = timeStep;
      if (t + deltat > t1)
         deltat = t1 - t;

      sampleCount numSamples = int (deltat * rate + 0.5);

      Mixer *mixer = new Mixer(channels, numSamples, true, rate, int16Sample);
      wxASSERT(mixer);
      mixer->Clear();

      char *buffer = new char[numSamples * SAMPLE_SIZE(int16Sample) * channels];
      wxASSERT(buffer);

      TrackListIterator iter(tracks);
      Track *tr = iter.First();
      while (tr) {
         if (tr->GetKind() == Track::Wave) {
            if (tr->GetSelected() || !selectionOnly) {
               if (tr->GetChannel() == Track::MonoChannel)
                  mixer->MixMono((WaveTrack *) tr, t, t + deltat);
               if (tr->GetChannel() == Track::LeftChannel)
                  mixer->MixLeft((WaveTrack *) tr, t, t + deltat);
               if (tr->GetChannel() == Track::RightChannel)
                  mixer->MixRight((WaveTrack *) tr, t, t + deltat);
            }
         }
         tr = iter.Next();
      }

      samplePtr mixed = mixer->GetBuffer();

      // Byte-swapping is neccesary on big-endian machines, since
      // WAV files are little-endian
#if wxBYTE_ORDER == wxBIG_ENDIAN
      {
         short *buffer = (short*)mixed;
         for( int i = 0; i < numSamples; i++ )
            buffer[i] = wxINT16_SWAP_ON_BE(buffer[i]);
      }
#endif

      fwrite( mixed, numSamples * channels * SAMPLE_SIZE(int16Sample), 1, pipe );

      t += deltat;

      if (!progress && wxGetElapsedTime(false) > 500) {

         wxString message;

         if (selectionOnly)
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
         cancelling =
             !progress->Update(int (((t - t0) * 1000) / (t1 - t0) + 0.5));
      }

      delete mixer;
      delete[]buffer;
   }

   pclose( pipe );

   if(progress)
      delete progress;
   return true;
}

#endif  /* __WXGTK__ */

