/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP3.cpp

  Joshua Haberman

  This just acts as an interface to LAME. A Lame dynamic library must
  be present

  The difficulty in our approach is that we are attempting to use LAME
  in a way it was not designed to be used. LAME's API is reasonably
  consistant, so if we were linking directly against it we could expect
  this code to work with a variety of different LAME versions. However,
  the data structures change from version to version, and so linking
  with one version of the header and dynamically linking against a
  different version of the dynamic library will not work correctly.

  The solution is to find the lowest common denominator between versions.
  The bare minimum of functionality we must use is this:
      1. Initialize the library.
      2. Set, at minimum, the following global options:
          i.  input sample rate
          ii. input channels
      3. Encode the stream
      4. Call the finishing routine

  Just so that it's clear that we're NOT free to use whatever features
  of LAME we like, I'm not including lame.h, but instead enumerating
  here the extent of functions and structures that we can rely on being
  able to import and use from a dynamic library.

  For the record, we aim to support LAME 3.70 on. Since LAME 3.70 was
  released in April of 2000, that should be plenty.


  Copyright 2002, 2003 Joshua Haberman.
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

\class MP3Exporter
\brief Class used to export MP3 files

*//*****************************************************************//**

\class MP3ExporterCleanup
\brief Tiny class that is used to clean up any allocated resources 
when the program exits.  A global instance of it is created, and its
destructor does the cleanup.

*//********************************************************************/

#include <wx/defs.h>

#include <wx/choice.h>
#include <wx/dynlib.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/mimetype.h>
#include <wx/msgdlg.h>
#include <wx/radiobut.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/timer.h>
#include <wx/utils.h>
#include <wx/window.h>

#include "../Audacity.h"
#include "../FileIO.h"
#include "../Internat.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Tags.h"
#include "../WaveTrack.h"
#include "../widgets/LinkingHtmlWindow.h"

#include "FileDialog.h"

#include "ExportMP3.h"

MP3Exporter *gMP3Exporter = NULL;

#define MODE_SET           0
#define MODE_VBR           1
#define MODE_ABR           2
#define MODE_CBR           3

#define CHANNEL_JOINT      0
#define CHANNEL_STEREO     1

#define QUALITY_0          0
#define QUALITY_1          1
#define QUALITY_2          2
#define QUALITY_3          3
#define QUALITY_4          4
#define QUALITY_5          5
#define QUALITY_6          6
#define QUALITY_7          7
#define QUALITY_8          8
#define QUALITY_9          9

#define ROUTINE_FAST       0
#define ROUTINE_STANDARD   1

#define PRESET_EXTREME     0
#define PRESET_STANDARD    1
#define PRESET_MEDIUM      2

// Note: The label field is what will be written to preferences and carries
//       no numerical significance.  It is simply a means to look up a value
//       in a table.
//
//       The entries should be listed in order you want them to appear in the
//       choice dropdown based on the name field.
typedef struct
{
   wxString name;
   int label;
} CHOICES;

static CHOICES fixRates[] =
{
   {  _("16 kbps"),  16    },
   {  _("24 kbps"),  24    },
   {  _("32 kbps"),  32    },
   {  _("40 kbps"),  40    },
   {  _("48 kbps"),  48    },
   {  _("56 kbps"),  56    },
   {  _("64 kbps"),  64    },
   {  _("80 kbps"),  80    },
   {  _("96 kbps"),  96    },
   {  _("112 kbps"), 112   },
   {  _("128 kbps"), 128   },
   {  _("160 kbps"), 160   },
   {  _("192 kbps"), 192   },
   {  _("224 kbps"), 224   },
   {  _("256 kbps"), 256   },
   {  _("320 kbps"), 320   }
};

static CHOICES varRates[] =
{
   {  _("220-260 kbps (Best Quality)"),   QUALITY_0   },
   {  _("200-250 kbps"),                  QUALITY_1   },
   {  _("170-210 kbps"),                  QUALITY_2   },
   {  _("155-195 kbps"),                  QUALITY_3   },
   {  _("145-185 kbps"),                  QUALITY_4   },
   {  _("110-150 kbps"),                  QUALITY_5   },
   {  _("95-135 kbps"),                   QUALITY_6   },
   {  _("80-120 kbps"),                   QUALITY_7   },
   {  _("65-105 kbps"),                   QUALITY_8   },
   {  _("45-85 kbps (Smaller files)"),    QUALITY_9   }
};

static CHOICES varModes[] =
{
   {  _("Fast"),     ROUTINE_FAST      },
   {  _("Standard"), ROUTINE_STANDARD  }
};

static CHOICES setRates[] =
{
   {  _("Extreme"),  PRESET_EXTREME    },
   {  _("Medium"),   PRESET_MEDIUM     },
   {  _("Standard"), PRESET_STANDARD   }
};

#if 0
// Debug routine from BladeMP3EncDLL.c in the libmp3lame distro
static void dump_config( 	lame_global_flags*	gfp )
{
	wxPrintf(wxT("\n\nLame_enc configuration options:\n"));
	wxPrintf(wxT("==========================================================\n"));

	wxPrintf(wxT("version                =%d\n"),lame_get_version( gfp ) );
	wxPrintf(wxT("Layer                  =3\n"));
	wxPrintf(wxT("mode                   ="));
	switch ( lame_get_mode( gfp ) )
	{
		case STEREO:       wxPrintf(wxT( "Stereo\n" )); break;
		case JOINT_STEREO: wxPrintf(wxT( "Joint-Stereo\n" )); break;
		case DUAL_CHANNEL: wxPrintf(wxT( "Forced Stereo\n" )); break;
		case MONO:         wxPrintf(wxT( "Mono\n" )); break;
		case NOT_SET:      /* FALLTROUGH */
		default:           wxPrintf(wxT( "Error (unknown)\n" )); break;
	}

	wxPrintf(wxT("Input sample rate      =%.1f kHz\n"), lame_get_in_samplerate( gfp ) /1000.0 );
	wxPrintf(wxT("Output sample rate     =%.1f kHz\n"), lame_get_out_samplerate( gfp ) /1000.0 );

	wxPrintf(wxT("bitrate                =%d kbps\n"), lame_get_brate( gfp ) );
	wxPrintf(wxT("Quality Setting        =%d\n"), lame_get_quality( gfp ) );

	wxPrintf(wxT("Low pass frequency     =%d\n"), lame_get_lowpassfreq( gfp ) );
	wxPrintf(wxT("Low pass width         =%d\n"), lame_get_lowpasswidth( gfp ) );

	wxPrintf(wxT("High pass frequency    =%d\n"), lame_get_highpassfreq( gfp ) );
	wxPrintf(wxT("High pass width        =%d\n"), lame_get_highpasswidth( gfp ) );

	wxPrintf(wxT("No short blocks        =%d\n"), lame_get_no_short_blocks( gfp ) );
	wxPrintf(wxT("Force short blocks     =%d\n"), lame_get_force_short_blocks( gfp ) );

	wxPrintf(wxT("de-emphasis            =%d\n"), lame_get_emphasis( gfp ) );
	wxPrintf(wxT("private flag           =%d\n"), lame_get_extension( gfp ) );

	wxPrintf(wxT("copyright flag         =%d\n"), lame_get_copyright( gfp ) );
	wxPrintf(wxT("original flag          =%d\n"),	lame_get_original( gfp ) );
	wxPrintf(wxT("CRC                    =%s\n"), lame_get_error_protection( gfp ) ? wxT("on") : wxT("off") );
	wxPrintf(wxT("Fast mode              =%s\n"), ( lame_get_quality( gfp ) )? wxT("enabled") : wxT("disabled") );
	wxPrintf(wxT("Force mid/side stereo  =%s\n"), ( lame_get_force_ms( gfp ) )?wxT("enabled"):wxT("disabled") );
	wxPrintf(wxT("Padding Type           =%d\n"), lame_get_padding_type( gfp ) );
	wxPrintf(wxT("Disable Reservoir      =%d\n"), lame_get_disable_reservoir( gfp ) );
	wxPrintf(wxT("Allow diff-short       =%d\n"), lame_get_allow_diff_short( gfp ) );
	wxPrintf(wxT("Interchannel masking   =%f\n"), lame_get_interChRatio( gfp ) );
	wxPrintf(wxT("Strict ISO Encoding    =%s\n"), ( lame_get_strict_ISO( gfp ) ) ?wxT("Yes"):wxT("No"));
	wxPrintf(wxT("Scale                  =%5.2f\n"), lame_get_scale( gfp ) );

	wxPrintf(wxT("VBR                    =%s, VBR_q =%d, VBR method ="),
					( lame_get_VBR( gfp ) !=vbr_off ) ? wxT("enabled"): wxT("disabled"),
		            lame_get_VBR_q( gfp ) );

	switch ( lame_get_VBR( gfp ) )
	{
		case vbr_off:	wxPrintf(wxT( "vbr_off\n" ));	break;
		case vbr_mt :	wxPrintf(wxT( "vbr_mt \n" ));	break;
		case vbr_rh :	wxPrintf(wxT( "vbr_rh \n" ));	break;
		case vbr_mtrh:	wxPrintf(wxT( "vbr_mtrh \n" ));	break;
		case vbr_abr: 
			wxPrintf(wxT( "vbr_abr (average bitrate %d kbps)\n"), lame_get_VBR_mean_bitrate_kbps( gfp ) );
		break;
		default:
			wxPrintf(wxT("error, unknown VBR setting\n"));
		break;
	}

	wxPrintf(wxT("Vbr Min bitrate        =%d kbps\n"), lame_get_VBR_min_bitrate_kbps( gfp ) );
	wxPrintf(wxT("Vbr Max bitrate        =%d kbps\n"), lame_get_VBR_max_bitrate_kbps( gfp ) );

	wxPrintf(wxT("Write VBR Header       =%s\n"), ( lame_get_bWriteVbrTag( gfp ) ) ?wxT("Yes"):wxT("No"));
	wxPrintf(wxT("VBR Hard min           =%d\n"), lame_get_VBR_hard_min( gfp ) );

	wxPrintf(wxT("ATH Only               =%d\n"), lame_get_ATHonly( gfp ) );
	wxPrintf(wxT("ATH short              =%d\n"), lame_get_ATHshort( gfp ) );
	wxPrintf(wxT("ATH no                 =%d\n"), lame_get_noATH( gfp ) );
	wxPrintf(wxT("ATH type               =%d\n"), lame_get_ATHtype( gfp ) );
	wxPrintf(wxT("ATH lower              =%f\n"), lame_get_ATHlower( gfp ) );
	wxPrintf(wxT("ATH aa                 =%d\n"), lame_get_athaa_type( gfp ) );
	wxPrintf(wxT("ATH aa  loudapprox     =%d\n"), lame_get_athaa_loudapprox( gfp ) );
	wxPrintf(wxT("ATH aa  sensitivity    =%f\n"), lame_get_athaa_sensitivity( gfp ) );

	wxPrintf(wxT("Experimental nspsytune =%d\n"), lame_get_exp_nspsytune( gfp ) );
	wxPrintf(wxT("Experimental X         =%d\n"), lame_get_experimentalX( gfp ) );
	wxPrintf(wxT("Experimental Y         =%d\n"), lame_get_experimentalY( gfp ) );
	wxPrintf(wxT("Experimental Z         =%d\n"), lame_get_experimentalZ( gfp ) );
}
#endif

#if defined(__WXMSW__)

#include "BladeMP3EncDLL.h"

class LameExporter : public MP3Exporter
{
public:
   LameExporter() : MP3Exporter()
   {
   }

   ~LameExporter()
   {
      FreeLibrary();
   }

   bool InitLibrary(wxString libpath)
   {
      if (!lame_lib.Load(libpath, wxDL_LAZY)) {
         return false;
      }

      beInitStream = (BEINITSTREAM) lame_lib.GetSymbol(wxT("beInitStream"));
      beEncodeChunk = (BEENCODECHUNK) lame_lib.GetSymbol(wxT("beEncodeChunk"));
      beDeinitStream = (BEDEINITSTREAM) lame_lib.GetSymbol(wxT("beDeinitStream"));
      beCloseStream = (BECLOSESTREAM) lame_lib.GetSymbol(wxT("beCloseStream"));
      beVersion = (BEVERSION) lame_lib.GetSymbol(wxT("beVersion"));

      if (!beInitStream ||
         !beEncodeChunk ||
         !beDeinitStream ||
         !beCloseStream ||
         !beVersion) {
         return false;
      }

      return true;
   }

   void FreeLibrary()
   {
      lame_lib.Unload();

      return;
   }

   wxString GetLibraryVersion()
   {
      BE_VERSION ver;

      if (!mLibraryLoaded) {
         return wxT("");
      }

      beVersion(&ver);

      return wxString::Format(wxT("LAME v%d.%d"), ver.byMajorVersion, ver.byMinorVersion);
   }

   int InitializeStream(int channels, int sampleRate)
   {
      if (!mLibraryLoaded) {
         return -1;
      }

      if (channels > 2) {
         return -1;
      }

      // Set config defaults to sane values
      memset(&mConf, 0, CURRENT_STRUCT_SIZE);
      mConf.dwConfig = BE_CONFIG_LAME;
      mConf.format.LHV1.dwStructVersion = CURRENT_STRUCT_VERSION;
      mConf.format.LHV1.dwStructSize = CURRENT_STRUCT_SIZE;
      mConf.format.LHV1.nPreset = LQP_NOPRESET;
      mConf.format.LHV1.dwMpegVersion = MPEG1;
      mConf.format.LHV1.bCRC = true;
      mConf.format.LHV1.bNoRes = true;
      mConf.format.LHV1.dwSampleRate = sampleRate;
      mConf.format.LHV1.dwBitrate = mBitrate;

      // Set the VBR quality or ABR/CBR bitrate
      switch (mMode) {
         case MODE_SET:
         {
            int preset;

            if (mRoutine == ROUTINE_FAST) {
               if (mQuality == PRESET_EXTREME) {
                  preset = LQP_FAST_EXTREME;
               }
               else if (mQuality == PRESET_STANDARD) {
                  preset = LQP_FAST_STANDARD;
               }
               else {
                  preset = 14;   // Not defined until 3.96
               }
            }
            else {
               if (mQuality == PRESET_EXTREME) {
                  preset = LQP_EXTREME;
               }
               else if (mQuality == PRESET_STANDARD) {
                  preset = LQP_STANDARD;
               }
               else {
                  preset = 13;   // Not defined until 3.96
               }
            }

             mConf.format.LHV1.nPreset = preset;
         }
         break;

         case MODE_VBR:
            mConf.format.LHV1.bEnableVBR = true;
            mConf.format.LHV1.nVbrMethod = mRoutine == ROUTINE_STANDARD ?
                                           VBR_METHOD_OLD :
                                           VBR_METHOD_NEW;
            mConf.format.LHV1.nVBRQuality = mQuality;
         break;

         case MODE_ABR:
            mConf.format.LHV1.bEnableVBR = true;
            mConf.format.LHV1.nVbrMethod = VBR_METHOD_ABR;
            mConf.format.LHV1.dwVbrAbr_bps = mBitrate * 1000;
         break;

         default:
            mConf.format.LHV1.bEnableVBR = false;
            mConf.format.LHV1.nVbrMethod = VBR_METHOD_NONE;
            mConf.format.LHV1.dwBitrate = mBitrate;
         break;
      }

      // Set the channel mode
      int mode;
      if (channels == 1) {
         mode = BE_MP3_MODE_MONO;
      }
      else if (mChannel == CHANNEL_JOINT) {
         mode = BE_MP3_MODE_JSTEREO;
      }
      else {
         mode = BE_MP3_MODE_STEREO;
      }
      mConf.format.LHV1.nMode = mode;

      if (beInitStream(&mConf, &mInSampleNum, &mOutBufferSize, &mStreamHandle)) {
         return -1;
      }

      mEncoding = true;

      return (mInSampleNum / channels); /* convert samples_total into samples_per_channel */
   }

   int GetOutBufferSize()
   {
      if (!mEncoding)
         return -1;

      return mOutBufferSize;
   }

   int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[])
   {
      if (!mEncoding) {
         return -1;
      }
      
      unsigned long bytes;
      if (beEncodeChunk(mStreamHandle, mInSampleNum, inbuffer, outbuffer, &bytes)) {
         return -1;
      }

      return bytes;
   }

   int EncodeRemainder(short int inbuffer[], int nSamples, unsigned char outbuffer[])
   {
      if (!mEncoding) {
         return -1;
      }

      unsigned long bytes;
      if (beEncodeChunk(mStreamHandle, nSamples, inbuffer, outbuffer, &bytes)) {
         return -1;
      }

      return bytes;
   }

   int EncodeBufferMono(short int inbuffer[], unsigned char outbuffer[])
   {
      return EncodeBuffer(inbuffer, outbuffer);
   }

   int EncodeRemainderMono(short int inbuffer[], int nSamples,
                     unsigned char outbuffer[])
   {
      return EncodeRemainder(inbuffer, nSamples, outbuffer);
   }

   int FinishStream(unsigned char outbuffer[])
   {
      if (!mEncoding) {
         return -1;
      }

      unsigned long bytes;
      if (beDeinitStream(mStreamHandle, outbuffer, &bytes)) {
         return -1;
      }

      if (beCloseStream(mStreamHandle)) {
         return -1;
      }

      mEncoding = false;

      return bytes;
   }

   void CancelEncoding()
   {
      if (!mEncoding) {
         return;
      }

      beCloseStream(mStreamHandle);

      return;
   }

   wxString GetLibraryPath()
   {
      return wxT("");
   }

   wxString GetLibraryName()
   {
      return wxT("lame_enc.dll");
   }
   
   wxString GetLibraryTypeString()
   {
      return _("Only lame_enc.dll|lame_enc.dll|Dynamically Linked Libraries (*.dll)|*.dll|All Files (*.*)|*");
   }
   
private:
   wxDynamicLibrary lame_lib;

   BE_CONFIG mConf;
   HBE_STREAM mStreamHandle;
   unsigned long mOutBufferSize;
   unsigned long mInSampleNum;

   /* function pointers to the symbols we get from the library */
   BEINITSTREAM beInitStream;
   BEENCODECHUNK beEncodeChunk;
   BEDEINITSTREAM beDeinitStream;
   BECLOSESTREAM beCloseStream;
   BEVERSION beVersion;
};

#else

#include <dlfcn.h>
#include "lame.h"

/* --------------------------------------------------------------------------*/

typedef lame_global_flags *lame_init_t(void);
typedef int lame_init_params_t(lame_global_flags*);
typedef const char* get_lame_version_t(void);

typedef int lame_encode_buffer_t (
      lame_global_flags* gf,
      const short int    buffer_l [],
      const short int    buffer_r [],
      const int          nsamples,
      unsigned char *    mp3buf,
      const int          mp3buf_size );

typedef int lame_encode_buffer_interleaved_t(
      lame_global_flags* gf,
      short int          pcm[],
      int                num_samples,   /* per channel */
      unsigned char*     mp3buf,
      int                mp3buf_size );

typedef int lame_encode_flush_t(
      lame_global_flags *gf,
      unsigned char*     mp3buf,
      int                size );

typedef int lame_close_t(lame_global_flags*);

typedef int lame_set_in_samplerate_t(lame_global_flags*, int);
typedef int lame_set_out_samplerate_t(lame_global_flags*, int);
typedef int lame_set_num_channels_t(lame_global_flags*, int );
typedef int lame_set_quality_t(lame_global_flags*, int);
typedef int lame_set_brate_t(lame_global_flags*, int);
typedef int lame_set_VBR_t(lame_global_flags *, vbr_mode);
typedef int lame_set_VBR_q_t(lame_global_flags *, int);
typedef int lame_set_VBR_min_bitrate_kbps_t(lame_global_flags *, int);
typedef int lame_set_mode_t(lame_global_flags *, MPEG_mode);
typedef int lame_set_preset_t(lame_global_flags *, int);
typedef int lame_set_error_protection_t(lame_global_flags *, int);

/* --------------------------------------------------------------------------*/

class LameExporter : public MP3Exporter
{
public:
   LameExporter() : MP3Exporter()
   {
      mLib = NULL;
      mGF = NULL;
   }

   ~LameExporter()
   {
      FreeLibrary();
   }

   bool InitLibrary(wxString libpath)
   {
      // Until wxWidgets supports Dynamic Libraries (dylib) on the Mac, we just use
      // dlopen() and friends to support nixes.

      mLib = dlopen(OSFILENAME(libpath), RTLD_LAZY);
      if (mLib == NULL) {
         return false;
      }

      // These strings should not be translated or unicode enabled

      lame_init = (lame_init_t *)
         dlsym(mLib, "lame_init");
      get_lame_version = (get_lame_version_t *)
         dlsym(mLib, "get_lame_version");
      lame_init_params = (lame_init_params_t *)
         dlsym(mLib, "lame_init_params");
      lame_encode_buffer = (lame_encode_buffer_t *)
         dlsym(mLib, "lame_encode_buffer");
      lame_encode_buffer_interleaved = (lame_encode_buffer_interleaved_t *)
         dlsym(mLib, "lame_encode_buffer_interleaved");
      lame_encode_flush = (lame_encode_flush_t *)
         dlsym(mLib, "lame_encode_flush");
      lame_close = (lame_close_t *)
         dlsym(mLib, "lame_close");

      lame_set_in_samplerate = (lame_set_in_samplerate_t *)
          dlsym(mLib, "lame_set_in_samplerate");
      lame_set_out_samplerate = (lame_set_out_samplerate_t *)
          dlsym(mLib, "lame_set_out_samplerate");
      lame_set_num_channels = (lame_set_num_channels_t *)
          dlsym(mLib, "lame_set_num_channels");
      lame_set_quality = (lame_set_quality_t *)
          dlsym(mLib, "lame_set_quality");
      lame_set_brate = (lame_set_brate_t *)
          dlsym(mLib, "lame_set_brate");
      lame_set_VBR = (lame_set_VBR_t *)
          dlsym(mLib, "lame_set_VBR");
      lame_set_VBR_q = (lame_set_VBR_q_t *)
          dlsym(mLib, "lame_set_VBR_q");
      lame_set_VBR_min_bitrate_kbps = (lame_set_VBR_min_bitrate_kbps_t *)
          dlsym(mLib, "lame_set_VBR_min_bitrate_kbps");
      lame_set_mode = (lame_set_mode_t *) 
          dlsym(mLib, "lame_set_mode");
      lame_set_preset = (lame_set_preset_t *)
          dlsym(mLib, "lame_set_preset");
      lame_set_error_protection = (lame_set_error_protection_t *)
          dlsym(mLib, "lame_set_error_protection");

      /* we assume that if all the symbols are found, it's a valid library */

      if (!lame_init ||
         !get_lame_version ||
         !lame_init_params ||
         !lame_encode_buffer ||
         !lame_encode_buffer_interleaved ||
         !lame_encode_flush ||
         !lame_close ||
         !lame_set_in_samplerate ||
         !lame_set_out_samplerate ||
         !lame_set_num_channels ||
         !lame_set_quality ||
         !lame_set_brate ||
         !lame_set_VBR ||
         !lame_set_VBR_q ||
         !lame_set_mode ||
         !lame_set_preset ||
         !lame_set_error_protection) {
         return false;
      }

      mGF = lame_init();

      return true;
   }

   void FreeLibrary()
   {
      if (mLib) {
         dlclose(mLib);
         mLib = NULL;
      }

      return;
   }

   wxString GetLibraryVersion()
   {
      if (!mLibraryLoaded) {
         return wxT("");
      }

      return wxString::Format(wxT("LAME %hs"), get_lame_version());
   }

   int InitializeStream(int channels, int sampleRate)
   {
      if (!mLibraryLoaded) {
         return -1;
      }

      lame_set_error_protection(mGF, true);
      lame_set_num_channels(mGF, channels);
      lame_set_in_samplerate(mGF, sampleRate);
      lame_set_out_samplerate(mGF, sampleRate);

      // Set the VBR quality or ABR/CBR bitrate
      switch (mMode) {
         case MODE_SET:
         {
            int preset;

            if (mRoutine == ROUTINE_FAST) {
               if (mQuality == PRESET_EXTREME) {
                  preset = EXTREME_FAST;
               }
               else if (mQuality == PRESET_STANDARD) {
                  preset = STANDARD_FAST;
               }
               else {
                  preset = 1007;    // Not defined until 3.96
               }
            }
            else {
               if (mQuality == PRESET_EXTREME) {
                  preset = EXTREME;
               }
               else if (mQuality == PRESET_STANDARD) {
                  preset = STANDARD;
               }
               else {
                  preset = 1006;    // Not defined until 3.96
               }
            }

            lame_set_preset(mGF, preset);
         }
         break;

         case MODE_VBR:
            lame_set_VBR(mGF, (mRoutine == ROUTINE_STANDARD ? vbr_rh : vbr_mtrh ));
            lame_set_VBR_q(mGF, mQuality);
         break;

         case MODE_ABR:
            lame_set_preset(mGF, mBitrate );
         break;

         default:
            lame_set_VBR(mGF, vbr_off);
            lame_set_brate(mGF, mBitrate);
         break;
      }

      // Set the channel mode
      MPEG_mode mode;
      if (channels == 1) {
         mode = MONO;
      }
      else if (mChannel == CHANNEL_JOINT) {
         mode = JOINT_STEREO;
      }
      else {
         mode = STEREO;
      }
      lame_set_mode(mGF, mode);

      lame_init_params(mGF);

#if 0
      dump_config(mGF);
#endif

      mEncoding = true;

      return mSamplesPerChunk;
   }

   int GetOutBufferSize()
   {
      if (!mEncoding)
         return -1;

      return mOutBufferSize;
   }

   int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[])
   {
      if (!mEncoding) {
         return -1;
      }

      return lame_encode_buffer_interleaved(mGF, inbuffer, mSamplesPerChunk,
         outbuffer, mOutBufferSize);
   }

   int EncodeRemainder(short int inbuffer[], int nSamples,
                     unsigned char outbuffer[])
   {
      if (!mEncoding) {
         return -1;
      }

      return lame_encode_buffer_interleaved(mGF, inbuffer, nSamples, outbuffer,
         mOutBufferSize);
   }

   int EncodeBufferMono(short int inbuffer[], unsigned char outbuffer[])
   {
      if (!mEncoding) {
         return -1;
      }

      return lame_encode_buffer(mGF, inbuffer,inbuffer, mSamplesPerChunk,
         outbuffer, mOutBufferSize);
   }

   int EncodeRemainderMono(short int inbuffer[], int nSamples,
                     unsigned char outbuffer[])
   {
      if (!mEncoding) {
         return -1;
      }

      return lame_encode_buffer(mGF, inbuffer, inbuffer, nSamples, outbuffer,
         mOutBufferSize);
   }

   int FinishStream(unsigned char outbuffer[])
   {
      if (!mEncoding) {
         return -1;
      }

      mEncoding = false;
      int result = lame_encode_flush(mGF, outbuffer, mOutBufferSize);
      lame_close(mGF);
      return result;
   }

   void CancelEncoding()
   {
      mEncoding = false;
   }

#if defined(__WXMAC__)

   wxString GetLibraryPath()
   {
      return wxT("/usr/local/lib");
   }

   wxString GetLibraryName()
   {
      return wxT("libmp3lame.dylib");
   }

   wxString GetLibraryTypeString()
   {
      return wxString(_("Only libmp3lame.dylib|libmp3lame.dylib|Dynamic Libraries (*.dylib)|*.dylib|All Files (*)|*"));
   }

#else

   wxString GetLibraryPath()
   {
      return wxT("/usr/lib");
   }

   wxString GetLibraryName()
   {
      return wxT("libmp3lame.so");
   }

   wxString GetLibraryTypeString()
   {
      return wxString(_("Only libmp3lame.so|libmp3lame.so|Primary Shared Object files (*.so)|*.so|Extended Libraries (*.so*)|*.so*|All Files (*)|*"));
   }

#endif

private:

   void *mLib;

   /* function pointers to the symbols we get from the library */
   lame_init_t* lame_init;
   lame_init_params_t* lame_init_params;
   lame_encode_buffer_t* lame_encode_buffer;
   lame_encode_buffer_interleaved_t* lame_encode_buffer_interleaved;
   lame_encode_flush_t* lame_encode_flush;
   lame_close_t* lame_close;
   get_lame_version_t* get_lame_version;
   
   lame_set_in_samplerate_t* lame_set_in_samplerate;
   lame_set_out_samplerate_t* lame_set_out_samplerate;
   lame_set_num_channels_t* lame_set_num_channels;
   lame_set_quality_t* lame_set_quality;
   lame_set_brate_t* lame_set_brate;
   lame_set_VBR_t* lame_set_VBR;
   lame_set_VBR_q_t* lame_set_VBR_q;
   lame_set_VBR_min_bitrate_kbps_t* lame_set_VBR_min_bitrate_kbps;
   lame_set_mode_t* lame_set_mode;
   lame_set_preset_t* lame_set_preset;
   lame_set_error_protection_t* lame_set_error_protection;

   lame_global_flags *mGF;

   static const int mSamplesPerChunk = 220500;
   static const int mOutBufferSize = int(1.25 * mSamplesPerChunk + 7200);
};
#endif

#define ID_BROWSE 5000
#define ID_DLOAD  5001

class FindDialog : public wxDialog
{
public:

   FindDialog(wxWindow *parent, wxString path, wxString name, wxString type)
   : wxDialog(parent, wxID_ANY, wxString(_("Locate Lame")))
   {
      ShuttleGui S(this, eIsCreating);

      mPath = path;
      mName = name;
      mType = type;

      mLibPath.Assign(mPath, mName);

      PopulateOrExchange(S);
   }

   void PopulateOrExchange(ShuttleGui & S)
   {
      wxString text;

      S.SetBorder(10);
      S.StartVerticalLay(true);
      {
         text.Printf(_("Audacity needs the file %s to create MP3s."), mName.c_str());
         S.AddTitle(text);

         S.SetBorder(3);
         S.StartHorizontalLay(wxALIGN_LEFT, true);
         {
            text.Printf(_("Location of %s:"), mName.c_str());
            S.AddTitle(text);
         }
         S.EndHorizontalLay();

         S.StartMultiColumn(2, wxEXPAND);
         S.SetStretchyCol(0);
         {
            if (mLibPath.GetFullPath().IsEmpty()) {
               text.Printf(_("To find %s, click here -->"), mName.c_str());
               mPathText = S.AddTextBox(wxT(""), text, 0);
            }
            else {
               mPathText = S.AddTextBox(wxT(""), mLibPath.GetFullPath(), 0);
            }
            S.Id(ID_BROWSE).AddButton(_("Browse..."), wxALIGN_RIGHT);
            S.AddVariableText(_("To get a free copy of Lame, click here -->"), true);
            S.Id(ID_DLOAD).AddButton(_("Download..."), wxALIGN_RIGHT);
         }
         S.EndMultiColumn();
         S.SetBorder(5);
         S.StartHorizontalLay(wxALIGN_BOTTOM | wxALIGN_CENTER, false);
         {
//            S.StartHorizontalLay(wxALIGN_CENTER, false);
            {
#if defined(__WXGTK20__) || defined(__WXMAC__)
               S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
               S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
#else
               S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
               S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
#endif
            }
//            S.EndHorizontalLay();
         }
         S.EndHorizontalLay();
      }
      S.EndVerticalLay();
      GetSizer()->AddSpacer(5);
      Layout();
      Fit();
      SetMinSize(GetSize());
      Center();

      return;
   }

   void OnBrowse(wxCommandEvent & event)
   {
      wxString question;
      /* i18n-hint: It's asking for the location of a file, for
         example, "Where is lame_enc.dll?" - you could translate
         "Where would I find the file %s" instead if you want. */
      question.Printf(_("Where is %s?"), mName.c_str());

      wxString path = FileSelector(question, 
                                   mLibPath.GetPath(),
                                   mLibPath.GetName(),
                                   wxT(""),
                                   mType,
                                   wxOPEN,
                                   this);
      if (!path.IsEmpty()) {
         mLibPath = path;
         mPathText->SetValue(path);
      }
   }

   void OnDownload(wxCommandEvent & event)
   {
      wxString page = wxT("http://audacity.sourceforge.net/lame");
      ::OpenInDefaultBrowser(page);
   }

   wxString GetLibPath()
   {
      return mLibPath.GetFullPath();
   }

private:

   wxFileName mLibPath;

   wxString mPath;
   wxString mName;
   wxString mType;

   wxTextCtrl *mPathText;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(FindDialog, wxDialog)
   EVT_BUTTON(ID_BROWSE, FindDialog::OnBrowse)
   EVT_BUTTON(ID_DLOAD,  FindDialog::OnDownload)
END_EVENT_TABLE()

// --------------------------------------------------------------------

MP3Exporter::MP3Exporter()
{
   mLibraryLoaded = false;
   mEncoding = false;

   if (gPrefs) {
      mLibPath = gPrefs->Read(wxT("/MP3/MP3LibPath"), wxT(""));
   }

   mBitrate = 128;
   mQuality = QUALITY_2;
   mChannel = CHANNEL_STEREO;
   mMode = MODE_CBR;
   mRoutine = ROUTINE_FAST;
}

MP3Exporter::~MP3Exporter()
{
}

bool MP3Exporter::FindLibrary(wxWindow *parent, bool showdialog)
{
   wxString path;
   wxString name;

   if (!mLibPath.IsEmpty()) {
      wxFileName fn = mLibPath;
      path = fn.GetPath();
      name = fn.GetFullName();
   }
   else {
      path = GetLibraryPath();
      name = GetLibraryName();
   }

   FindDialog fd(parent,
                 path,
                 name,
                 GetLibraryTypeString());

   if (fd.ShowModal() == wxID_CANCEL) {
      return false;
   }

   path = fd.GetLibPath();
   
   if (!::wxFileExists(path)) {
      return false;
   }

   mLibPath = path;
   gPrefs->Write(wxT("/MP3/MP3LibPath"), mLibPath);

   return true;
}

bool MP3Exporter::LoadLibrary(wxWindow *parent, bool askuser)
{
   wxLogNull logNo;

   if (ValidLibraryLoaded()) {
      FreeLibrary();
      mLibraryLoaded = false;
   }

   // First try loading it from a previously located path
   if (!mLibPath.IsEmpty()) {
      mLibraryLoaded = InitLibrary(mLibPath);
   }

   // If not successful, try loading using system search paths
   if (!ValidLibraryLoaded()) {
      mLibPath = GetLibraryName();
      mLibraryLoaded = InitLibrary(mLibPath);
   }

   // If not successful, try loading using compiled in path
   if (!ValidLibraryLoaded()) {
      wxFileName fn(GetLibraryPath(), GetLibraryName());
      mLibPath = fn.GetFullPath();
      mLibraryLoaded = InitLibrary(mLibPath);
   }

   // If not successful, must ask the user
   if (!ValidLibraryLoaded()) {
      if (askuser && FindLibrary(parent, true)) {
         mLibraryLoaded = InitLibrary(mLibPath);
      }
   }

   // Oh well, just give up
   if (!ValidLibraryLoaded()) {
      return false;
   }

   return true;
}

bool MP3Exporter::ValidLibraryLoaded()
{
   return mLibraryLoaded;
}

void MP3Exporter::SetMode(int mode)
{
   mMode = mode;
}

void MP3Exporter::SetBitrate(int rate)
{
   mBitrate = rate;
}

void MP3Exporter::SetQuality(int q, int r)
{
   mQuality = q;
   mRoutine = r;
}

void MP3Exporter::SetChannel(int mode)
{
   mChannel = mode;
}

MP3Exporter *GetMP3Exporter()
{
   if (!gMP3Exporter) {
      gMP3Exporter = new LameExporter();
   }
   
   return gMP3Exporter;
}

void ReleaseMP3Exporter()
{
   if (gMP3Exporter) {
      delete gMP3Exporter;
   }

   gMP3Exporter = NULL;
}

class MP3ExporterCleanup
{
public:
   MP3ExporterCleanup(){};
   ~MP3ExporterCleanup(){ ReleaseMP3Exporter();}
};

// Create instance of this cleanup class purely to clean up 
// the exporter.
// No one will reference this variable, but when the program
// exits it will clean up any allocated MP3 exporter.
MP3ExporterCleanup gMP3ExporterCleanup;

static int FindValue(CHOICES *choices, int cnt, int needle, int def)
{
   for (int i = 0; i < cnt; i++) {
      if (choices[i].label == needle) {
         return needle;
      }
   }

   return def;
}

bool ExportMP3(AudacityProject *project,
               int channels, wxString fName,
               bool selectionOnly, double t0, double t1, MixerSpec *mixerSpec)
{
   double rate = project->GetRate();
   wxWindow *parent = project;
   TrackList *tracks = project->GetTracks();
   MP3Exporter *exporter = GetMP3Exporter();

   if (!exporter->LoadLibrary(parent, true)) {
      wxMessageBox(_("Could not open MP3 encoding library!"));
      gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));

      return false;
   }

   if (!exporter->ValidLibraryLoaded()) {
      wxMessageBox(_("Not a valid or supported MP3 encoding library!"));      
      gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));
      
      return false;
   }
   
   /* Open file for writing */
   FileIO outFile(fName, FileIO::Output);
   if (!outFile.IsOpened()) {
      wxMessageBox(_("Unable to open target file for writing"));
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

   char *id3buffer = NULL;
   int id3len;
   bool endOfFile;
   id3len = tags->ExportID3(&id3buffer, &endOfFile);
   if (!endOfFile) {
     outFile.Write(id3buffer, id3len);
   }

   // Retrieve preferences
   int brate;
   int rmode;
   int vmode;
   int cmode;

   gPrefs->Read(wxT("/FileFormats/MP3Bitrate"), &brate, 128);
   gPrefs->Read(wxT("/FileFormats/MP3RateMode"), &rmode, MODE_CBR);
   gPrefs->Read(wxT("/FileFormats/MP3VarMode"), &vmode, ROUTINE_FAST);
   gPrefs->Read(wxT("/FileFormats/MP3ChannelMode"), &cmode, CHANNEL_STEREO);

   // Set the bitrate/quality and mode
   if (rmode == MODE_SET) {
      int q = FindValue(setRates, WXSIZEOF(setRates), brate, PRESET_STANDARD);
      int r = FindValue(varModes, WXSIZEOF(varModes), vmode, ROUTINE_FAST);
      exporter->SetMode(MODE_SET);
      exporter->SetQuality(q, r);
   }
   else if (rmode == MODE_VBR) {
      int q = FindValue(varRates, WXSIZEOF(varRates), brate, QUALITY_2);
      int r = FindValue(varModes, WXSIZEOF(varModes), vmode, ROUTINE_FAST);
      exporter->SetMode(MODE_VBR);
      exporter->SetQuality(q, r);
   }
   else if (rmode == MODE_ABR) {
      int r = FindValue(fixRates, WXSIZEOF(fixRates), brate, 128);
      exporter->SetMode(MODE_ABR);
      exporter->SetBitrate(r);
   }
   else {
      int r = FindValue(fixRates, WXSIZEOF(fixRates), brate, 128);
      exporter->SetMode(MODE_CBR);
      exporter->SetBitrate(r);
   }

   // Set the channel mode
   if (cmode == CHANNEL_JOINT) {
      exporter->SetChannel(CHANNEL_JOINT);
   }
   else {
      exporter->SetChannel(CHANNEL_STEREO);
   }

   sampleCount inSamples = exporter->InitializeStream(channels, int(rate + 0.5));

   bool cancelling = false;
   long bytes;

   int bufferSize = exporter->GetOutBufferSize();
   unsigned char *buffer = new unsigned char[bufferSize];
   wxASSERT(buffer);

   int numWaveTracks;
   WaveTrack **waveTracks;
   tracks->GetWaveTracks(selectionOnly, &numWaveTracks, &waveTracks);
   Mixer *mixer = new Mixer(numWaveTracks, waveTracks,
                            tracks->GetTimeTrack(),
                            t0, t1,
                            channels, inSamples, true,
                            rate, int16Sample, true, mixerSpec);

   if (rmode == MODE_CBR)
      GetActiveProject()->ProgressShow(selectionOnly ?
         wxString::Format(_("Exporting selected audio at %d kbps"), brate) :
         wxString::Format(_("Exporting entire file at %d kbps"), brate),
         wxFileName(fName).GetName());
   else
      GetActiveProject()->ProgressShow(selectionOnly ?
         wxString::Format(_("Exporting selected audio at quality %d"), brate) :
         wxString::Format(_("Exporting entire file at quality %d"), brate),
         wxFileName(fName).GetName());

   while (!cancelling) {
      sampleCount blockLen = mixer->Process(inSamples);

      if (blockLen == 0) {
         break;
      }
      
      short *mixed = (short *)mixer->GetBuffer();

      if (blockLen < inSamples) {
         if (channels > 1) {
            bytes = exporter->EncodeRemainder(mixed,  blockLen , buffer);
         }
         else {
            bytes = exporter->EncodeRemainderMono(mixed,  blockLen , buffer);
         }
      }
      else {
         if (channels > 1) {
            bytes = exporter->EncodeBuffer(mixed, buffer);
         }
         else {
            bytes = exporter->EncodeBufferMono(mixed, buffer);
         }
      }

      outFile.Write(buffer, bytes);

      int progressvalue = int (1000 * ((mixer->MixGetCurrentTime()-t0) /
                                          (t1-t0)));
      cancelling = !GetActiveProject()->ProgressUpdate(progressvalue);
   }

   GetActiveProject()->ProgressHide();

   delete mixer;

   bytes = exporter->FinishStream(buffer);

   if (bytes) {
      outFile.Write(buffer, bytes);
   }
   
   /* Write ID3 tag if it was supposed to be at the end of the file */
   
   if (endOfFile) {
      outFile.Write(id3buffer, id3len);
   }

   free(id3buffer);

   /* Close file */
   outFile.Close();
      
   delete[]buffer;
   
   return !cancelling;
}

#define ID_SET 7000
#define ID_VBR 7001
#define ID_ABR 7002
#define ID_CBR 7003

class MP3OptionsDialog : public wxDialog
{
public:

   /// 
   /// 
   MP3OptionsDialog(wxWindow *parent)
   : wxDialog(NULL, wxID_ANY, wxString(_("Specify MP3 Options")),
      wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
   {
      ShuttleGui S(this, eIsCreatingFromPrefs);

      PopulateOrExchange(S);
   }

   /// 
   /// 
   void PopulateOrExchange(ShuttleGui & S)
   {
      S.StartHorizontalLay(wxEXPAND, 0);
      {
         S.StartStatic(_("MP3 Export Setup"), 0);
         {
            S.StartMultiColumn(2, wxEXPAND);
            {
               S.SetStretchyCol(1);
               S.StartTwoColumn();
               {
                  S.AddPrompt(_("Bit Rate Mode:"));
                  S.StartHorizontalLay();
                  {
                     S.StartRadioButtonGroup(wxT("/FileFormats/MP3RateMode"), MODE_CBR);
                     {
                        mSET = S.Id(ID_SET).TieRadioButton(_("Preset"), MODE_SET);
                        mVBR = S.Id(ID_VBR).TieRadioButton(_("Variable"), MODE_VBR);
                        mABR = S.Id(ID_ABR).TieRadioButton(_("Average"), MODE_ABR);
                        mCBR = S.Id(ID_CBR).TieRadioButton(_("Constant"), MODE_CBR);
                     }
                     S.EndRadioButtonGroup();
                  }
                  S.EndHorizontalLay();

                  CHOICES *choices;
                  int cnt;
                  bool enable;
                  int defrate;
                  bool preset = false;

                  if (mSET->GetValue()) {
                     choices = setRates;
                     cnt = WXSIZEOF(setRates);
                     enable = true;
                     defrate = PRESET_STANDARD;
                     preset = true;
                  }
                  else if (mVBR->GetValue()) {
                     choices = varRates;
                     cnt = WXSIZEOF(varRates);
                     enable = true;
                     defrate = QUALITY_4;
                  }
                  else if (mABR->GetValue()) {
                     choices = fixRates;
                     cnt = WXSIZEOF(fixRates);
                     enable = false;
                     defrate = 128;
                  }
                  else {
                     mCBR->SetValue(true);
                     choices = fixRates;
                     cnt = WXSIZEOF(fixRates);
                     enable = false;
                     defrate = 128;
                  }

                  mRate = S.TieChoice(wxT("Quality"),
                                      wxT("/FileFormats/MP3Bitrate"), 
                                      defrate,
                                      GetNames(choices, cnt),
                                      GetLabels(choices, cnt));

                  mMode = S.TieChoice(_("Variable Mode:"),
                                      wxT("/FileFormats/MP3VarMode"), 
                                      ROUTINE_FAST,
                                      GetNames(varModes, WXSIZEOF(varModes)),
                                      GetLabels(varModes, WXSIZEOF(varModes)));
                  mMode->Enable(enable);

                  S.AddPrompt(_("Channel Mode:"));
                  S.StartTwoColumn();
                  {
                     S.StartRadioButtonGroup(wxT("/FileFormats/MP3ChannelMode"), CHANNEL_STEREO);
                     {
                        mJoint = S.TieRadioButton(_("Joint Stereo"), CHANNEL_JOINT);
                        mStereo = S.TieRadioButton(_("Stereo"), CHANNEL_STEREO);
#if defined(__WXMSW__)
                        mJoint->Enable(!preset);
                        mStereo->Enable(!preset);
#endif
                     }
                     S.EndRadioButtonGroup();
                  }
                  S.EndTwoColumn();
               }
               S.EndTwoColumn();
            }
            S.EndMultiColumn();
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

   /// 
   /// 
   void OnSET(wxCommandEvent& evt)
   {
      LoadNames(setRates, WXSIZEOF(setRates));

      mRate->SetSelection(2);
      mRate->Refresh();
      mMode->Enable(true);
#if defined(__WXMSW__)
      mJoint->Enable(false);
      mStereo->Enable(false);
#endif
   }

   /// 
   /// 
   void OnVBR(wxCommandEvent& evt)
   {
      LoadNames(varRates, WXSIZEOF(varRates));

      mRate->SetSelection(2);
      mRate->Refresh();
      mMode->Enable(true);
#if defined(__WXMSW__)
      mJoint->Enable(true);
      mStereo->Enable(true);
#endif
   }

   /// 
   /// 
   void OnABR(wxCommandEvent& evt)
   {
      LoadNames(fixRates, WXSIZEOF(fixRates));

      mRate->SetSelection(10);
      mRate->Refresh();
      mMode->Enable(false);
#if defined(__WXMSW__)
      mJoint->Enable(true);
      mStereo->Enable(true);
#endif
   }

   /// 
   /// 
   void OnCBR(wxCommandEvent& evt)
   {
      LoadNames(fixRates, WXSIZEOF(fixRates));

      mRate->SetSelection(10);
      mRate->Refresh();
      mMode->Enable(false);
#if defined(__WXMSW__)
      mJoint->Enable(true);
      mStereo->Enable(true);
#endif
   }

private:

   void LoadNames(CHOICES *choices, int count)
   {
      mRate->Clear();

      for (int i = 0; i < count; i++)
      {
         mRate->Append(choices[i].name);
      }
   }
   
   wxArrayString GetNames(CHOICES *choices, int count)
   {
      wxArrayString names;

      for (int i = 0; i < count; i++)
      {
         names.Add(choices[i].name);
      }

      return names;
   }

   wxArrayInt GetLabels(CHOICES *choices, int count)
   {
      wxArrayInt labels;

      for (int i = 0; i < count; i++)
      {
         labels.Add(choices[i].label);
      }

      return labels;
   }

private:

   wxRadioButton *mStereo;
   wxRadioButton *mJoint;
   wxRadioButton *mSET;
   wxRadioButton *mVBR;
   wxRadioButton *mABR;
   wxRadioButton *mCBR;
   wxChoice *mRate;
   wxChoice *mMode;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(MP3OptionsDialog, wxDialog)
   EVT_RADIOBUTTON(ID_SET,    MP3OptionsDialog::OnSET)
   EVT_RADIOBUTTON(ID_VBR,    MP3OptionsDialog::OnVBR)
   EVT_RADIOBUTTON(ID_ABR,    MP3OptionsDialog::OnABR)
   EVT_RADIOBUTTON(ID_CBR,    MP3OptionsDialog::OnCBR)
   EVT_BUTTON(wxID_OK,        MP3OptionsDialog::OnOK)
END_EVENT_TABLE()

bool ExportMP3Options(AudacityProject *project)
{
   MP3OptionsDialog od(project);

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
// arch-tag: c6af56b1-37fa-4d95-b982-0a24b3a49c00
