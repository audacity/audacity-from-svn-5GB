/**********************************************************************

  Audacity: A Digital Audio Editor

  CLExporter.h

  (c) The Audacity Team

**********************************************************************/

#ifndef __AUDACITY_CLEXPORTER__
#define __AUDACITY_CLEXPORTER__


/// This class includes routines to export the selected audio to
/// an file, supported by a command-line encoder.  Most useful on linux and
/// OSX.

#include "Exporter.h"
#include "../SampleFormat.h"
class wxString;
class AudacityProject;
class DirManager;
class WaveTrack;
      



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




class CLExporter: public Exporter
{
 public:
   CLExporter(AudacityProject * project, double t0, double t1,
	      bool exportSelection, int mRate, 
	      int channels);
  
  ~CLExporter();
  
  bool Export(const wxString &filename);
  bool Verify();
 private:
  
};

#endif
