/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPCM.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORTPCM__
#define __AUDACITY_EXPORTPCM__

#include "Exporter.h"
#include "../SampleFormat.h"
class wxString;
class AudacityProject;
class DirManager;
class WaveTrack;

//This class is a front-end to libsndfile  The following enumerated constants can
//be used to control the type of file produced; copied here for convenience
//These were originally in an anonymous enum in sndfile.h
//
//        SF_FORMAT_WAV                   = 0x010000,             /* Microsoft WAV format (little endian). */
//        SF_FORMAT_AIFF                  = 0x020000,             /* Apple/SGI AIFF format (big endian). */
//        SF_FORMAT_AU                    = 0x030000,             /* Sun/NeXT AU format (big endian). */
//        SF_FORMAT_RAW                   = 0x040000,             /* RAW PCM data. */
//        SF_FORMAT_PAF                   = 0x050000,             /* Ensoniq PARIS file format. */
//        SF_FORMAT_SVX                   = 0x060000,             /* Amiga IFF / SVX8 / SV16 format. */
//        SF_FORMAT_NIST                  = 0x070000,             /* Sphere NIST format. */
//        SF_FORMAT_VOC                   = 0x080000,             /* VOC files. */
//        SF_FORMAT_IRCAM                 = 0x0A0000,             /* Berkeley/IRCAM/CARL */
//        SF_FORMAT_W64                   = 0x0B0000,             /* Sonic Foundry's 64 bit RIFF/WAV */
//        SF_FORMAT_MAT4                  = 0x0C0000,             /* Matlab (tm) V4.2 / GNU Octave 2.0 */
//        SF_FORMAT_MAT5                  = 0x0D0000,             /* Matlab (tm) V5.0 / GNU Octave 2.1 */
//
//        /* Subtypes from here on. */
// 
//        SF_FORMAT_PCM_S8                = 0x0001,               /* Signed 8 bit data */
//        SF_FORMAT_PCM_16                = 0x0002,               /* Signed 16 bit data */
//        SF_FORMAT_PCM_24                = 0x0003,               /* Signed 24 bit data */
//        SF_FORMAT_PCM_32                = 0x0004,               /* Signed 32 bit data */
// 
//        SF_FORMAT_PCM_U8                = 0x0005,               /* Unsigned 8 bit data (WAV and RAW only) */
// 
//        SF_FORMAT_FLOAT                 = 0x0006,               /* 32 bit float data */
//        SF_FORMAT_DOUBLE                = 0x0007,               /* 64 bit float data */
 
//        SF_FORMAT_ULAW                  = 0x0010,               /* U-Law encoded. */
//        SF_FORMAT_ALAW                  = 0x0011,               /* A-Law encoded. */
//        SF_FORMAT_IMA_ADPCM             = 0x0012,               /* IMA ADPCM. */
//        SF_FORMAT_MS_ADPCM              = 0x0013,               /* Microsoft ADPCM. */
// 
//        SF_FORMAT_GSM610                = 0x0020,               /* GSM 6.10 encoding. */
//        SF_FORMAT_VOX_ADPCM             = 0x0021,               /* OKI / Dialogix ADPCM */
// 
//        SF_FORMAT_G721_32               = 0x0030,               /* 32kbs G721 ADPCM encoding. */
//        SF_FORMAT_G723_24               = 0x0031,               /* 24kbs G723 ADPCM encoding. */
//        SF_FORMAT_G723_40               = 0x0032,               /* 40kbs G723 ADPCM encoding. */
// 
//        SF_FORMAT_DWVW_12               = 0x0040,               /* 12 bit Delta Width Variable Word encoding. */
//        SF_FORMAT_DWVW_16               = 0x0041,               /* 16 bit Delta Width Variable Word encoding. */
//        SF_FORMAT_DWVW_24               = 0x0042,               /* 24 bit Delta Width Variable Word encoding. */
//        SF_FORMAT_DWVW_N                = 0x0043,               /* N bit Delta Width Variable Word encoding.
 
        /* Endian-ness options. */
 
//        SF_ENDIAN_FILE                  = 0x00000000,   /* Default file endian-ness. */
//        SF_ENDIAN_LITTLE                = 0x10000000,   /* Force little endian-ness. */
//        SF_ENDIAN_BIG                   = 0x20000000,   /* Force big endian-ness. */
//        SF_ENDIAN_CPU                   = 0x30000000,   /* Force CPU endian-ness. */
// 
//        SF_FORMAT_SUBMASK               = 0x0000FFFF,
//        SF_FORMAT_TYPEMASK              = 0x0FFF0000,
//        SF_FORMAT_ENDMASK               = 0x30000000
       


class PCMExporter: public Exporter
{
 public:
  PCMExporter(AudacityProject * project, double t0, double t1,
	      bool exportSelection, int outate, 
	      int channels, int format);
  
  ~PCMExporter();
 
  bool Export(const wxString &filename);
  bool Verify();
 private:
  int mSoundFileFormat;
};

#endif
