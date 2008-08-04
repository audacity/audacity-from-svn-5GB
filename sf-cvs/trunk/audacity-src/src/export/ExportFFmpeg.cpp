/**********************************************************************

   Audacity: A Digital Audio Editor

   ExportFFmpeg.cpp

   Audacity(R) is copyright (c) 1999-2008 Audacity Team.
   License: GPL v2.  See License.txt.

   LRN

******************************************************************//**

\class ExportFFmpegAC3Options
\brief Options dialog for FFmpeg exporting of AC3 format.

*//***************************************************************//**

\class ExportFFmpegWAVOptions
\brief Options dialog for FFmpeg exporting of WAV format.

*//***************************************************************//**

\class ExportFFmpegAACOptions
\brief Options dialog for FFmpeg exporting of AAC format.

*//***************************************************************//**

\class ExportFFmpegAMRNBOptions
\brief Options dialog for FFmpeg exporting of AMRNB format.

*//***************************************************************//**

\class ExportFFmpegAMRWBOptions
\brief Options dialog for FFmpeg exporting of AMRWB format.

*//***************************************************************//**

\class ExportFFmpegWMAOptions
\brief Options dialog for FFmpeg exporting of WMA format.

*//***************************************************************//**

\class ExportFFmpeg
\brief Controlling class for FFmpeg exporting.  Creates the options 
dialog of the appropriate type, adds tags and invokes the export 
function.

*//*******************************************************************/


#include "../Audacity.h"   // keep ffmpeg before wx because they interact
#include "../FFmpeg.h"     // and Audacity.h before FFmpeg for config*.h

#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/timer.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/string.h>
#include <wx/textctrl.h>
#include <wx/listbox.h>
#include <wx/window.h>
#include <wx/spinctrl.h>
#include <wx/combobox.h>

#include "../FileFormats.h"
#include "../Internat.h"
#include "../LabelTrack.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Tags.h"
#include "../Track.h"
#include "../WaveTrack.h"

#include "Export.h"

#if defined(USE_FFMPEG)

extern FFmpegLibs *FFmpegLibsInst;

enum FFmpegExposedFormat 
{
   FMT_PCMS16LEWAV,
   FMT_M4A,
   FMT_AC3,
   FMT_GSMAIFF,
   FMT_GSMMSWAV,
   FMT_AMRNB,
   FMT_AMRWB,
   FMT_WMA2,
   FMT_OTHER,
   FMT_LAST
};

struct ExposedFormat
{
   FFmpegExposedFormat fmtid;
   const wxChar *name;
   const wxChar *extension;
   int maxchannels;
   bool canmetadata;
   bool canutf8;
   const wxChar *description;
   CodecID codecid;
};

ExposedFormat fmts[] = 
{
   {FMT_PCMS16LEWAV, wxT("WAV"),       wxT("wav"),    255,  false,false,_("WAV Files (FFmpeg)"),                      CODEC_ID_PCM_S16LE},
   {FMT_M4A,         wxT("M4A"),       wxT("m4a"),    48,   true ,true ,_("M4A (AAC) Files (FFmpeg)"),                CODEC_ID_AAC},
   {FMT_AC3,         wxT("AC3"),       wxT("ac3"),    7,    false,false,_("AC3 Files (FFmpeg)"),                      CODEC_ID_AC3},
   {FMT_GSMAIFF,     wxT("GSMAIFF"),   wxT("aiff"),   1,    true ,true ,_("GSM-AIFF Files (FFmpeg)"),                 CODEC_ID_GSM},
   {FMT_GSMMSWAV,    wxT("GSMWAV"),    wxT("wav"),    1,    false,false,_("GSM-WAV (Microsoft) Files (FFmpeg)"),      CODEC_ID_GSM_MS},
   {FMT_AMRNB,       wxT("AMRNB"),     wxT("amr"),    1,    false,false,_("AMR (narrow band) Files (FFmpeg)"),        CODEC_ID_AMR_NB},
   {FMT_AMRWB,       wxT("AMRWB"),     wxT("amr"),    1,    false,false,_("AMR (wide band) Files (FFmpeg)"),          CODEC_ID_AMR_WB},
   {FMT_WMA2,        wxT("WMA"),       wxT("wma"),    2,    true ,false,_("WMA (version 2) Files (FFmpeg)"),          CODEC_ID_WMAV2},
   {FMT_OTHER,       wxT("FFMPEG"),    wxT("ffmpeg"), 255,  true ,true ,_("Custom FFmpeg Export"),                    CODEC_ID_NONE}
};

struct CompatibilityEntry
{
   const wxChar *fmt;
   CodecID codec;
};

CompatibilityEntry CompatibilityList[] = 
{
   { wxT("adts"), CODEC_ID_AAC },

   { wxT("aiff"), CODEC_ID_PCM_S16BE },
   { wxT("aiff"), CODEC_ID_PCM_S8 },
   { wxT("aiff"), CODEC_ID_PCM_S24BE },
   { wxT("aiff"), CODEC_ID_PCM_S32BE },
   { wxT("aiff"), CODEC_ID_PCM_ALAW },
   { wxT("aiff"), CODEC_ID_PCM_MULAW },
   { wxT("aiff"), CODEC_ID_MACE3 },
   { wxT("aiff"), CODEC_ID_MACE6 },
   { wxT("aiff"), CODEC_ID_GSM },
   { wxT("aiff"), CODEC_ID_ADPCM_G726 },
   { wxT("aiff"), CODEC_ID_PCM_S16LE },
   { wxT("aiff"), CODEC_ID_ADPCM_IMA_QT },
   { wxT("aiff"), CODEC_ID_QDM2 },

   { wxT("amr"), CODEC_ID_AMR_NB },
   { wxT("amr"), CODEC_ID_AMR_WB },

   { wxT("asf"), CODEC_ID_PCM_S16LE },
   { wxT("asf"), CODEC_ID_PCM_U8 },
   { wxT("asf"), CODEC_ID_PCM_S24LE },
   { wxT("asf"), CODEC_ID_PCM_S32LE },
   { wxT("asf"), CODEC_ID_ADPCM_MS },
   { wxT("asf"), CODEC_ID_PCM_ALAW },
   { wxT("asf"), CODEC_ID_PCM_MULAW },
   { wxT("asf"), CODEC_ID_WMAVOICE },
   { wxT("asf"), CODEC_ID_ADPCM_IMA_WAV },
   { wxT("asf"), CODEC_ID_ADPCM_YAMAHA },
   { wxT("asf"), CODEC_ID_TRUESPEECH },
   { wxT("asf"), CODEC_ID_GSM_MS },
   { wxT("asf"), CODEC_ID_ADPCM_G726 },
   { wxT("asf"), CODEC_ID_MP2 },
   { wxT("asf"), CODEC_ID_MP3 },
   { wxT("asf"), CODEC_ID_VOXWARE },
   { wxT("asf"), CODEC_ID_AAC },
   { wxT("asf"), CODEC_ID_WMAV1 },
   { wxT("asf"), CODEC_ID_WMAV2 },
   { wxT("asf"), CODEC_ID_WMAPRO },
   { wxT("asf"), CODEC_ID_ADPCM_CT },
   { wxT("asf"), CODEC_ID_ATRAC3 },
   { wxT("asf"), CODEC_ID_IMC },
   { wxT("asf"), CODEC_ID_AC3 },
   { wxT("asf"), CODEC_ID_DTS },
   { wxT("asf"), CODEC_ID_SONIC },
   { wxT("asf"), CODEC_ID_SONIC_LS },
   { wxT("asf"), CODEC_ID_FLAC },
   { wxT("asf"), CODEC_ID_ADPCM_SWF },
   { wxT("asf"), CODEC_ID_VORBIS },

   { wxT("au"), CODEC_ID_PCM_MULAW },
   { wxT("au"), CODEC_ID_PCM_S8 },
   { wxT("au"), CODEC_ID_PCM_S16BE },
   { wxT("au"), CODEC_ID_PCM_ALAW },

   { wxT("avi"), CODEC_ID_PCM_S16LE },
   { wxT("avi"), CODEC_ID_PCM_U8 },
   { wxT("avi"), CODEC_ID_PCM_S24LE },
   { wxT("avi"), CODEC_ID_PCM_S32LE },
   { wxT("avi"), CODEC_ID_ADPCM_MS },
   { wxT("avi"), CODEC_ID_PCM_ALAW },
   { wxT("avi"), CODEC_ID_PCM_MULAW },
   { wxT("avi"), CODEC_ID_WMAVOICE },
   { wxT("avi"), CODEC_ID_ADPCM_IMA_WAV },
   { wxT("avi"), CODEC_ID_ADPCM_YAMAHA },
   { wxT("avi"), CODEC_ID_TRUESPEECH },
   { wxT("avi"), CODEC_ID_GSM_MS },
   { wxT("avi"), CODEC_ID_ADPCM_G726 },
   { wxT("avi"), CODEC_ID_MP2 },
   { wxT("avi"), CODEC_ID_MP3 },
   { wxT("avi"), CODEC_ID_VOXWARE },
   { wxT("avi"), CODEC_ID_AAC },
   { wxT("avi"), CODEC_ID_WMAV1 },
   { wxT("avi"), CODEC_ID_WMAV2 },
   { wxT("avi"), CODEC_ID_WMAPRO },
   { wxT("avi"), CODEC_ID_ADPCM_CT },
   { wxT("avi"), CODEC_ID_ATRAC3 },
   { wxT("avi"), CODEC_ID_IMC },
   { wxT("avi"), CODEC_ID_AC3 },
   { wxT("avi"), CODEC_ID_DTS },
   { wxT("avi"), CODEC_ID_SONIC },
   { wxT("avi"), CODEC_ID_SONIC_LS },
   { wxT("avi"), CODEC_ID_FLAC },
   { wxT("avi"), CODEC_ID_ADPCM_SWF },
   { wxT("avi"), CODEC_ID_VORBIS },

   { wxT("crc"), CODEC_ID_NONE },

   { wxT("dv"), CODEC_ID_PCM_S16LE },

   { wxT("ffm"), CODEC_ID_NONE },

   { wxT("flv"), CODEC_ID_MP3 },
   { wxT("flv"), CODEC_ID_PCM_S8 },
   { wxT("flv"), CODEC_ID_PCM_S16BE },
   { wxT("flv"), CODEC_ID_PCM_S16LE },
   { wxT("flv"), CODEC_ID_ADPCM_SWF },
   { wxT("flv"), CODEC_ID_AAC },
   { wxT("flv"), CODEC_ID_NELLYMOSER },

   { wxT("framecrc"), CODEC_ID_NONE },

   { wxT("gxf"), CODEC_ID_PCM_S16LE },

   { wxT("matroska"), CODEC_ID_PCM_S16LE },
   { wxT("matroska"), CODEC_ID_PCM_U8 },
   { wxT("matroska"), CODEC_ID_PCM_S24LE },
   { wxT("matroska"), CODEC_ID_PCM_S32LE },
   { wxT("matroska"), CODEC_ID_ADPCM_MS },
   { wxT("matroska"), CODEC_ID_PCM_ALAW },
   { wxT("matroska"), CODEC_ID_PCM_MULAW },
   { wxT("matroska"), CODEC_ID_WMAVOICE },
   { wxT("matroska"), CODEC_ID_ADPCM_IMA_WAV },
   { wxT("matroska"), CODEC_ID_ADPCM_YAMAHA },
   { wxT("matroska"), CODEC_ID_TRUESPEECH },
   { wxT("matroska"), CODEC_ID_GSM_MS },
   { wxT("matroska"), CODEC_ID_ADPCM_G726 },
   { wxT("matroska"), CODEC_ID_MP2 },
   { wxT("matroska"), CODEC_ID_MP3 },
   { wxT("matroska"), CODEC_ID_VOXWARE },
   { wxT("matroska"), CODEC_ID_AAC },
   { wxT("matroska"), CODEC_ID_WMAV1 },
   { wxT("matroska"), CODEC_ID_WMAV2 },
   { wxT("matroska"), CODEC_ID_WMAPRO },
   { wxT("matroska"), CODEC_ID_ADPCM_CT },
   { wxT("matroska"), CODEC_ID_ATRAC3 },
   { wxT("matroska"), CODEC_ID_IMC },
   { wxT("matroska"), CODEC_ID_AC3 },
   { wxT("matroska"), CODEC_ID_DTS },
   { wxT("matroska"), CODEC_ID_SONIC },
   { wxT("matroska"), CODEC_ID_SONIC_LS },
   { wxT("matroska"), CODEC_ID_FLAC },
   { wxT("matroska"), CODEC_ID_ADPCM_SWF },
   { wxT("matroska"), CODEC_ID_VORBIS },

   { wxT("mmf"), CODEC_ID_ADPCM_YAMAHA },

   { wxT("mov"), CODEC_ID_PCM_S32BE }, //mov
   { wxT("mov"), CODEC_ID_PCM_S32LE },
   { wxT("mov"), CODEC_ID_PCM_S24BE },
   { wxT("mov"), CODEC_ID_PCM_S24LE },
   { wxT("mov"), CODEC_ID_PCM_S16BE },
   { wxT("mov"), CODEC_ID_PCM_S16LE },
   { wxT("mov"), CODEC_ID_PCM_S8 },
   { wxT("mov"), CODEC_ID_PCM_U8 },
   { wxT("mov"), CODEC_ID_PCM_MULAW },
   { wxT("mov"), CODEC_ID_PCM_ALAW },
   { wxT("mov"), CODEC_ID_ADPCM_IMA_QT },
   { wxT("mov"), CODEC_ID_MACE3 },
   { wxT("mov"), CODEC_ID_MACE6 },
   { wxT("mov"), CODEC_ID_MP3 },
   { wxT("mov"), CODEC_ID_AAC },
   { wxT("mov"), CODEC_ID_AMR_NB },
   { wxT("mov"), CODEC_ID_AMR_WB },
   { wxT("mov"), CODEC_ID_GSM },
   { wxT("mov"), CODEC_ID_ALAC },
   { wxT("mov"), CODEC_ID_QCELP },
   { wxT("mov"), CODEC_ID_QDM2 },
   { wxT("mov"), CODEC_ID_DVAUDIO },
   { wxT("mov"), CODEC_ID_WMAV2 },
   { wxT("mov"), CODEC_ID_ALAC },

   { wxT("mp3"), CODEC_ID_MP3 },

   { wxT("mpeg"), CODEC_ID_AC3 },
   { wxT("mpeg"), CODEC_ID_DTS },
   { wxT("mpeg"), CODEC_ID_PCM_S16BE },
   { wxT("mpeg"), CODEC_ID_MP2 },

   { wxT("vcd"), CODEC_ID_AC3 },
   { wxT("vcd"), CODEC_ID_DTS },
   { wxT("vcd"), CODEC_ID_PCM_S16BE },
   { wxT("vcd"), CODEC_ID_MP2 },

   { wxT("vob"), CODEC_ID_AC3 },
   { wxT("vob"), CODEC_ID_DTS },
   { wxT("vob"), CODEC_ID_PCM_S16BE },
   { wxT("vob"), CODEC_ID_MP2 },

   { wxT("svcd"), CODEC_ID_AC3 },
   { wxT("svcd"), CODEC_ID_DTS },
   { wxT("svcd"), CODEC_ID_PCM_S16BE },
   { wxT("svcd"), CODEC_ID_MP2 },

   { wxT("dvd"), CODEC_ID_AC3 },
   { wxT("dvd"), CODEC_ID_DTS },
   { wxT("dvd"), CODEC_ID_PCM_S16BE },
   { wxT("dvd"), CODEC_ID_MP2 },

   { wxT("nut"), CODEC_ID_PCM_S16LE },
   { wxT("nut"), CODEC_ID_PCM_U8 },
   { wxT("nut"), CODEC_ID_PCM_S24LE },
   { wxT("nut"), CODEC_ID_PCM_S32LE },
   { wxT("nut"), CODEC_ID_ADPCM_MS },
   { wxT("nut"), CODEC_ID_PCM_ALAW },
   { wxT("nut"), CODEC_ID_PCM_MULAW },
   { wxT("nut"), CODEC_ID_WMAVOICE },
   { wxT("nut"), CODEC_ID_ADPCM_IMA_WAV },
   { wxT("nut"), CODEC_ID_ADPCM_YAMAHA },
   { wxT("nut"), CODEC_ID_TRUESPEECH },
   { wxT("nut"), CODEC_ID_GSM_MS },
   { wxT("nut"), CODEC_ID_ADPCM_G726 },
   { wxT("nut"), CODEC_ID_MP2 },
   { wxT("nut"), CODEC_ID_MP3 },
   { wxT("nut"), CODEC_ID_VOXWARE },
   { wxT("nut"), CODEC_ID_AAC },
   { wxT("nut"), CODEC_ID_WMAV1 },
   { wxT("nut"), CODEC_ID_WMAV2 },
   { wxT("nut"), CODEC_ID_WMAPRO },
   { wxT("nut"), CODEC_ID_ADPCM_CT },
   { wxT("nut"), CODEC_ID_ATRAC3 },
   { wxT("nut"), CODEC_ID_IMC },
   { wxT("nut"), CODEC_ID_AC3 },
   { wxT("nut"), CODEC_ID_DTS },
   { wxT("nut"), CODEC_ID_SONIC },
   { wxT("nut"), CODEC_ID_SONIC_LS },
   { wxT("nut"), CODEC_ID_FLAC },
   { wxT("nut"), CODEC_ID_ADPCM_SWF },
   { wxT("nut"), CODEC_ID_VORBIS },

   { wxT("ogg"), CODEC_ID_VORBIS },
   { wxT("ogg"), CODEC_ID_FLAC },

   { wxT("ac3"), CODEC_ID_AC3 },

   { wxT("dts"), CODEC_ID_DTS },

   { wxT("flac"), CODEC_ID_FLAC },

   { wxT("RoQ"), CODEC_ID_ROQ_DPCM },

   { wxT("rm"), CODEC_ID_AC3 },

   { wxT("swf"), CODEC_ID_MP3 },

   { wxT("avm2"), CODEC_ID_MP3 },

   { wxT("voc"), CODEC_ID_PCM_U8 },

   { wxT("wav"), CODEC_ID_PCM_S16LE },
   { wxT("wav"), CODEC_ID_PCM_U8 },
   { wxT("wav"), CODEC_ID_PCM_S24LE },
   { wxT("wav"), CODEC_ID_PCM_S32LE },
   { wxT("wav"), CODEC_ID_ADPCM_MS },
   { wxT("wav"), CODEC_ID_PCM_ALAW },
   { wxT("wav"), CODEC_ID_PCM_MULAW },
   { wxT("wav"), CODEC_ID_WMAVOICE },
   { wxT("wav"), CODEC_ID_ADPCM_IMA_WAV },
   { wxT("wav"), CODEC_ID_ADPCM_YAMAHA },
   { wxT("wav"), CODEC_ID_TRUESPEECH },
   { wxT("wav"), CODEC_ID_GSM_MS },
   { wxT("wav"), CODEC_ID_ADPCM_G726 },
   { wxT("wav"), CODEC_ID_MP2 },
   { wxT("wav"), CODEC_ID_MP3 },
   { wxT("wav"), CODEC_ID_VOXWARE },
   { wxT("wav"), CODEC_ID_AAC },
   { wxT("wav"), CODEC_ID_WMAV1 },
   { wxT("wav"), CODEC_ID_WMAV2 },
   { wxT("wav"), CODEC_ID_WMAPRO },
   { wxT("wav"), CODEC_ID_ADPCM_CT },
   { wxT("wav"), CODEC_ID_ATRAC3 },
   { wxT("wav"), CODEC_ID_IMC },
   { wxT("wav"), CODEC_ID_AC3 },
   { wxT("wav"), CODEC_ID_DTS },
   { wxT("wav"), CODEC_ID_SONIC },
   { wxT("wav"), CODEC_ID_SONIC_LS },
   { wxT("wav"), CODEC_ID_FLAC },
   { wxT("wav"), CODEC_ID_ADPCM_SWF },
   { wxT("wav"), CODEC_ID_VORBIS },

   { NULL, CODEC_ID_NONE }
};

//----------------------------------------------------------------------------
// ExportFFmpegAC3Options Class
//----------------------------------------------------------------------------

static int iAC3BitRates[] = { 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 320000, 384000, 448000, 512000, 576000, 640000 };
static int iAC3SampleRates[] = { 32000, 44100, 48000, 0};

class ExportFFmpegAC3Options : public wxDialog
{
public:

   ExportFFmpegAC3Options(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

private:

   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;

   wxChoice *mBitRateChoice;
   wxButton *mOk;
   int mBitRateFromChoice;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportFFmpegAC3Options, wxDialog)
   EVT_BUTTON(wxID_OK,ExportFFmpegAC3Options::OnOK)
END_EVENT_TABLE()

ExportFFmpegAC3Options::ExportFFmpegAC3Options(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify AC3 Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   for (unsigned int i=0; i < (sizeof(iAC3BitRates)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(wxT("%i"),iAC3BitRates[i]/1000));
      mBitRateLabels.Add(iAC3BitRates[i]);
   }

   PopulateOrExchange(S);
}

/// 
/// 
void ExportFFmpegAC3Options::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.StartStatic(_("AC3 Export Setup"), 0);
      {
         S.StartTwoColumn();
         {
            S.TieChoice(_("Bit Rate:"), wxT("/FileFormats/AC3BitRate"), 
               160000, mBitRateNames, mBitRateLabels);
         }
         S.EndTwoColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

/// 
/// 
void ExportFFmpegAC3Options::OnOK(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}

//----------------------------------------------------------------------------
// ExportFFmpegWAVOptions Class
//----------------------------------------------------------------------------

static int iWAVSampleRates[] = { 48000, 44100, 32000 };

class ExportFFmpegWAVOptions : public wxDialog
{
public:

   ExportFFmpegWAVOptions(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

private:

   wxArrayString mSampleRateNames;
   wxArrayInt    mSampleRateLabels;

   wxChoice *mSampleRateChoice;
   wxButton *mOk;
   int mSampleRateFromChoice;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportFFmpegWAVOptions, wxDialog)
EVT_BUTTON(wxID_OK,            ExportFFmpegWAVOptions::OnOK)
END_EVENT_TABLE()

ExportFFmpegWAVOptions::ExportFFmpegWAVOptions(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify WAV Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   for (unsigned int i=0; i < (sizeof(iWAVSampleRates)/sizeof(int)); i++)
   {
      mSampleRateNames.Add(wxString::Format(wxT("%i"),iWAVSampleRates[i]));
      mSampleRateLabels.Add(iWAVSampleRates[i]);
   }


   PopulateOrExchange(S);
}

/// 
/// 
void ExportFFmpegWAVOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.StartStatic(_("WAV Export Setup"), 0);
      {
         S.StartTwoColumn();
         {
            S.TieChoice(_("Sample Rate:"), wxT("/FileFormats/WAVSampleRate"), 
               44100, mSampleRateNames, mSampleRateLabels);
         }
         S.EndTwoColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

/// 
/// 
void ExportFFmpegWAVOptions::OnOK(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}


//----------------------------------------------------------------------------
// ExportFFmpegAACOptions Class
//----------------------------------------------------------------------------

static int iAACProfileValues[] = { FF_PROFILE_AAC_LOW, FF_PROFILE_AAC_MAIN, /*FF_PROFILE_AAC_SSR,*/ FF_PROFILE_AAC_LTP };
static const wxChar *iAACProfileNames[] = { _("Low Complexity"), _("Main profile"), /*_("SSR"),*/ _("LTP") }; //SSR is not supported
static const int iAACSampleRates[] = { 7350, 8000, 11025, 12000, 16000, 22050, 24000, 32000, 44100, 38000, 64000, 88200, 0 };


class ExportFFmpegAACOptions : public wxDialog
{
public:

   ExportFFmpegAACOptions(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

private:

   wxSpinCtrl *mQualitySpin;
   wxButton *mOk;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportFFmpegAACOptions, wxDialog)
EVT_BUTTON(wxID_OK,ExportFFmpegAACOptions::OnOK)
END_EVENT_TABLE()

ExportFFmpegAACOptions::ExportFFmpegAACOptions(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify AAC Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   PopulateOrExchange(S);
}

/// 
/// 
void ExportFFmpegAACOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.StartStatic(_("AAC Export Setup"), 0);
      {
         S.StartTwoColumn();
         {
            S.TieSlider(wxT("Quality:"),wxT("/FileFormats/AACQuality"),250,500,-1);
         }
         S.EndTwoColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

/// 
/// 
void ExportFFmpegAACOptions::OnOK(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}


//----------------------------------------------------------------------------
// ExportFFmpegAMRNBOptions Class
//----------------------------------------------------------------------------

//8kHz for NB and 16kHz for WB.
static int iAMRNBBitRate[] = { 4750, 5150, 5900, 6700, 7400, 7950, 10200, 12200 };

class ExportFFmpegAMRNBOptions : public wxDialog
{
public:

   ExportFFmpegAMRNBOptions(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

private:

   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;

   wxChoice *mBitRateChoice;
   wxButton *mOk;
   int mBitRateFromChoice;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportFFmpegAMRNBOptions, wxDialog)
EVT_BUTTON(wxID_OK,ExportFFmpegAMRNBOptions::OnOK)
END_EVENT_TABLE()

ExportFFmpegAMRNBOptions::ExportFFmpegAMRNBOptions(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify AMR-NB Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   for (unsigned int i=0; i < (sizeof(iAMRNBBitRate)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(wxT("%i"),iAMRNBBitRate[i]/1000));
      mBitRateLabels.Add(iAMRNBBitRate[i]);
   }

   PopulateOrExchange(S);
}

/// 
/// 
void ExportFFmpegAMRNBOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.StartStatic(_("AMR-NB Export Setup"), 0);
      {
         S.StartTwoColumn();
         {
            S.TieChoice(_("Bit Rate:"), wxT("/FileFormats/AMRNBBitRate"), 
               12200, mBitRateNames, mBitRateLabels);
         }
         S.EndTwoColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

/// 
/// 
void ExportFFmpegAMRNBOptions::OnOK(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}

//----------------------------------------------------------------------------
// ExportFFmpegAMRWBOptions Class
//----------------------------------------------------------------------------

//8kHz for NB and 16kHz for WB.
static int iAMRWBBitRate[] = { 6600, 8850, 12650, 14250, 15850, 18250, 19850, 23050, 23850 };

class ExportFFmpegAMRWBOptions : public wxDialog
{
public:

   ExportFFmpegAMRWBOptions(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

private:

   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;

   wxChoice *mBitRateChoice;
   wxButton *mOk;
   int mBitRateFromChoice;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportFFmpegAMRWBOptions, wxDialog)
EVT_BUTTON(wxID_OK,ExportFFmpegAMRWBOptions::OnOK)
END_EVENT_TABLE()

ExportFFmpegAMRWBOptions::ExportFFmpegAMRWBOptions(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify AMR-WB Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   for (unsigned int i=0; i < (sizeof(iAMRWBBitRate)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(wxT("%i"),iAMRWBBitRate[i]/1000));
      mBitRateLabels.Add(iAMRWBBitRate[i]);
   }

   PopulateOrExchange(S);
}

/// 
/// 
void ExportFFmpegAMRWBOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.StartStatic(_("AMR-WB Export Setup"), 0);
      {
         S.StartTwoColumn();
         {
            S.TieChoice(_("Bit Rate:"), wxT("/FileFormats/AMRWBBitRate"), 
               23850, mBitRateNames, mBitRateLabels);
         }
         S.EndTwoColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

/// 
/// 
void ExportFFmpegAMRWBOptions::OnOK(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}

//----------------------------------------------------------------------------
// ExportFFmpegWMAOptions Class
//----------------------------------------------------------------------------

static int iWMABitRate[] = { 24634, 26012, 27734, 29457, 31524, 33764, 36348, 39448, 42894, 47028, 52024, 58225, 65805, 75624, 88716, 106976, 134539, 180189, 271835, 546598 };

class ExportFFmpegWMAOptions : public wxDialog
{
public:

   ExportFFmpegWMAOptions(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

private:

   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;

   wxChoice *mBitRateChoice;
   wxButton *mOk;
   int mBitRateFromChoice;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportFFmpegWMAOptions, wxDialog)
EVT_BUTTON(wxID_OK,ExportFFmpegWMAOptions::OnOK)
END_EVENT_TABLE()

ExportFFmpegWMAOptions::ExportFFmpegWMAOptions(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify WMA Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   for (unsigned int i=0; i < (sizeof(iWMABitRate)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(wxT("%i"),iWMABitRate[i]/1000));
      mBitRateLabels.Add(iWMABitRate[i]);
   }

   PopulateOrExchange(S);
}

/// 
/// 
void ExportFFmpegWMAOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.StartStatic(_("WMA Export Setup"), 0);
      {
         S.StartTwoColumn();
         {
            S.TieChoice(_("Bit Rate:"), wxT("/FileFormats/WMABitRate"), 
               180189, mBitRateNames, mBitRateLabels);
         }
         S.EndTwoColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

/// 
/// 
void ExportFFmpegWMAOptions::OnOK(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}


//----------------------------------------------------------------------------
// ExportFFmpegOptions Class
//----------------------------------------------------------------------------

enum FFmpegExportCtrlID {
   FEFirstID = 20000,
   FEFormatID,
   FECodecID,
   FEFormatLabelID,
   FECodecLabelID,
   FEFormatNameID,
   FECodecNameID,
   FELogLevelID,
   FEPresetID,
   FEBitrateID,
   FEQualityID,
   FESampleRateID,
   FELanguageID,
   FETagID,
   FECutoffID,
   FEFrameSizeID,
   FEBufSizeID,
   FEProfileID,
   FECompLevelID,
   FEUseLPCID,
   FELPCCoeffsID,
   FEMinPredID,
   FEMaxPredID,
   FEPredOrderID,
   FEMinPartOrderID,
   FEMaxPartOrderID,
   FEMuxRateID,
   FEPacketSizeID,
   FESavePresetID,
   FELoadPresetID,
   FEDeletePresetID,
   FEAllFormatsID,
   FEAllCodecsID,
   FEBitReservoirID,
   FELastID
};

struct ApplicableFor
{
   bool                 enable;
   FFmpegExportCtrlID   control;
   CodecID              codec;
   char                *format;
};


//Some controls (parameters they represent) are only applicable to a number
//of codecs or formats
ApplicableFor apptable[] = 
{
   {TRUE,FEQualityID,CODEC_ID_AAC,"any"},
   {TRUE,FEQualityID,CODEC_ID_MP3,"any"},
   {TRUE,FEQualityID,CODEC_ID_VORBIS,"any"},
   {FALSE,FEQualityID,CODEC_ID_NONE,"any"},

   {TRUE,FECutoffID,CODEC_ID_AC3,"any"},
   {TRUE,FECutoffID,CODEC_ID_AAC,"any"},
   {TRUE,FECutoffID,CODEC_ID_VORBIS,"any"},
   {FALSE,FECutoffID,CODEC_ID_NONE,"any"},

   {TRUE,FEFrameSizeID,CODEC_ID_FLAC,"any"},
   {FALSE,FEFrameSizeID,CODEC_ID_NONE,"any"},

   {TRUE,FEProfileID,CODEC_ID_AAC,"any"},
   {FALSE,FEProfileID,CODEC_ID_NONE,"any"},

   {TRUE,FECompLevelID,CODEC_ID_FLAC,"any"},
   {FALSE,FECompLevelID,CODEC_ID_NONE,"any"},

   {TRUE,FEUseLPCID,CODEC_ID_FLAC,"any"},
   {FALSE,FEUseLPCID,CODEC_ID_NONE,"any"},

   {TRUE,FELPCCoeffsID,CODEC_ID_FLAC,"any"},
   {FALSE,FELPCCoeffsID,CODEC_ID_NONE,"any"},

   {TRUE,FEMinPredID,CODEC_ID_FLAC,"any"},
   {FALSE,FEMinPredID,CODEC_ID_NONE,"any"},

   {TRUE,FEMaxPredID,CODEC_ID_FLAC,"any"},
   {FALSE,FEMaxPredID,CODEC_ID_NONE,"any"},

   {TRUE,FEPredOrderID,CODEC_ID_FLAC,"any"},
   {FALSE,FEPredOrderID,CODEC_ID_NONE,"any"},

   {TRUE,FEMinPartOrderID,CODEC_ID_FLAC,"any"},
   {FALSE,FEMinPartOrderID,CODEC_ID_NONE,"any"},

   {TRUE,FEMaxPartOrderID,CODEC_ID_FLAC,"any"},
   {FALSE,FEMaxPartOrderID,CODEC_ID_NONE,"any"},

   {TRUE,FEMuxRateID,CODEC_ID_NONE,"mpeg"},
   {TRUE,FEMuxRateID,CODEC_ID_NONE,"vcd"},
   {TRUE,FEMuxRateID,CODEC_ID_NONE,"vob"},
   {TRUE,FEMuxRateID,CODEC_ID_NONE,"svcd"},
   {TRUE,FEMuxRateID,CODEC_ID_NONE,"dvd"},
   {FALSE,FEMuxRateID,CODEC_ID_NONE,"any"},

   {TRUE,FEPacketSizeID,CODEC_ID_NONE,"mpeg"},
   {TRUE,FEPacketSizeID,CODEC_ID_NONE,"vcd"},
   {TRUE,FEPacketSizeID,CODEC_ID_NONE,"vob"},
   {TRUE,FEPacketSizeID,CODEC_ID_NONE,"svcd"},
   {TRUE,FEPacketSizeID,CODEC_ID_NONE,"dvd"},
   {FALSE,FEPacketSizeID,CODEC_ID_NONE,"any"},

   {TRUE,FELanguageID,CODEC_ID_NONE,"matroska"},
   {TRUE,FELanguageID,CODEC_ID_NONE,"mov"},
   {TRUE,FELanguageID,CODEC_ID_NONE,"3gp"},
   {TRUE,FELanguageID,CODEC_ID_NONE,"mp4"},
   {TRUE,FELanguageID,CODEC_ID_NONE,"psp"},
   {TRUE,FELanguageID,CODEC_ID_NONE,"3g2"},
   {TRUE,FELanguageID,CODEC_ID_NONE,"ipod"},
   {TRUE,FELanguageID,CODEC_ID_NONE,"mpegts"},
   {FALSE,FELanguageID,CODEC_ID_NONE,"any"},

   {TRUE,FEBitReservoirID,CODEC_ID_MP3,"any"},
   {TRUE,FEBitReservoirID,CODEC_ID_WMAV1,"any"},
   {TRUE,FEBitReservoirID,CODEC_ID_WMAV2,"any"},
   {FALSE,FEBitReservoirID,CODEC_ID_NONE,"any"},

   {FALSE,FFmpegExportCtrlID(0),CODEC_ID_NONE,NULL}
};

const wxChar *PredictionOrderMethodNames[] = { _("Estimate"), _("2-level"), _("4-level"), _("8-level"), _("Full search"), _("Log search")};


class ExportFFmpegOptions : public wxDialog
{
public:

   ExportFFmpegOptions(wxWindow *parent);
   ~ExportFFmpegOptions();
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);
   void OnFormatList(wxCommandEvent& event);
   void OnCodecList(wxCommandEvent& event);
   void OnAllFormats(wxCommandEvent& event);
   void OnAllCodecs(wxCommandEvent& event);
   void OnSavePreset(wxCommandEvent& event);
   void OnLoadPreset(wxCommandEvent& event);
   void OnDeletePreset(wxCommandEvent& event);
   void OnKeyDown(wxKeyEvent &event);

private:

   wxArrayString mPresetNames;
   wxArrayString mShownFormatNames;
   wxArrayString mShownFormatLongNames;
   wxArrayString mShownCodecNames;
   wxArrayString mShownCodecLongNames;
   wxArrayString mFormatNames;
   wxArrayString mFormatLongNames;
   wxArrayString mLogLevelNames;
   wxArrayInt    mLogLevelLabels;
   wxArrayString mCodecNames;
   wxArrayString mCodecLongNames;
   wxArrayString mProfileNames;
   wxArrayInt    mProfileLabels;
   wxArrayString mPredictionOrderMethodNames;;
   wxArrayInt    mPredictionOrderMethodLabels;

   wxChoice *mFormatChoice;
   wxChoice *mCodecChoice;

   wxListBox *mFormatList;
   wxListBox *mCodecList;

   wxStaticText *mFormatName;
   wxStaticText *mCodecName;

   wxChoice *mLogLevelChoice;
   wxChoice *mPresetChoice;
   wxComboBox *mPresetCombo;
   wxSpinCtrl *mBitrateSpin;
   wxSpinCtrl *mQualitySpin;
   wxSpinCtrl *mSampleRateSpin;
   wxTextCtrl *mLanguageText;
   wxTextCtrl *mTag;
   wxSpinCtrl *mCutoffSpin;
   wxCheckBox *mBitReservoirCheck;
   wxChoice *mProfileChoice;
   //wxSpinCtrl *mTrellisSpin; //trellis is only applicable for ADPCM...scrap it.
   wxSpinCtrl *mCompressionLevelSpin;
   wxSpinCtrl *mFrameSizeSpin;
   wxCheckBox *mUseLPCCheck;
   wxSpinCtrl *mLPCCoeffsPrecisionSpin;
   wxSpinCtrl *mMinPredictionOrderSpin;
   wxSpinCtrl *mMaxPredictionOrderSpin;
   wxChoice *mPredictionOrderMethodChoice;
   wxSpinCtrl *mMinPartitionOrderSpin;
   wxSpinCtrl *mMaxPartitionOrderSpin;
   wxSpinCtrl *mMuxRate;
   wxSpinCtrl *mPacketSize;

   wxButton *mOk;
   wxButton *mSavePreset;
   wxButton *mLoadPreset;
   wxButton *mDeletePreset;
   int mBitRateFromChoice;
   int mSampleRateFromChoice;

   wxArrayString *mPresets;

   void FindSelectedFormat(wxString **name, wxString **longname);
   void FindSelectedCodec(wxString **name, wxString **longname);
   void FetchFormatList();
   int FetchCompatibleFormatList(CodecID id, wxString *selfmt);
   void FetchCodecList();
   int FetchCompatibleCodecList(const wxChar *fmt, int id);
   void FetchPresetList();

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportFFmpegOptions, wxDialog)
EVT_BUTTON(wxID_OK,ExportFFmpegOptions::OnOK)
EVT_LISTBOX(FEFormatID,ExportFFmpegOptions::OnFormatList)
EVT_LISTBOX(FECodecID,ExportFFmpegOptions::OnCodecList)
EVT_BUTTON(FEAllFormatsID,ExportFFmpegOptions::OnAllFormats)
EVT_BUTTON(FEAllCodecsID,ExportFFmpegOptions::OnAllCodecs)
EVT_BUTTON(FESavePresetID,ExportFFmpegOptions::OnSavePreset)
EVT_BUTTON(FELoadPresetID,ExportFFmpegOptions::OnLoadPreset)
EVT_BUTTON(FEDeletePresetID,ExportFFmpegOptions::OnDeletePreset)
END_EVENT_TABLE()

ExportFFmpegOptions::~ExportFFmpegOptions()
{
   delete mPresets;
   
   DropFFmpegLibs();
}

ExportFFmpegOptions::ExportFFmpegOptions(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify Other Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PickFFmpegLibs();
   FFmpegLibsInst->LoadLibs(NULL,false);

   FetchPresetList();
   FetchFormatList();
   FetchCodecList();

   for (unsigned int i = 0; i < 10; i++)
   {
      mLogLevelLabels.Add(i);
      mLogLevelNames.Add(wxString::Format(wxT("%d"),i));
   }

   for (unsigned int i = 0; i < 6; i++)
   {
      mPredictionOrderMethodLabels.Add(i);
      mPredictionOrderMethodNames.Add(wxString::Format(wxT("%s"),PredictionOrderMethodNames[i]));
   }

   for (unsigned int i=0; i < (sizeof(iAACProfileValues)/sizeof(int)); i++)
   {
      mProfileNames.Add(wxString::Format(wxT("%s"),iAACProfileNames[i]));
      mProfileLabels.Add(iAACProfileValues[i]);
   }

   PopulateOrExchange(S);

   mFormatList->Select(mFormatList->FindString(gPrefs->Read(wxT("/FileFormats/FFmpegFormat"))));
   OnFormatList(wxCommandEvent());
   AVCodec *codec = FFmpegLibsInst->avcodec_find_encoder((CodecID)gPrefs->Read(wxT("/FileFormats/FFmpegCodec"),(long)CODEC_ID_NONE));
   if (codec != NULL) mCodecList->Select(mCodecList->FindString(wxString::FromUTF8(codec->name)));
   OnCodecList(wxCommandEvent());

}

///
///
void ExportFFmpegOptions::FetchPresetList()
{
   mPresets = new wxArrayString();
   wxString group;
   long dummy;
   if (gPrefs->GetFirstGroup(group,dummy))
   {
      do
      {
         wxString rest;
         if (group.StartsWith(wxT("FFmpegExportPreset_"), &rest))
         {
            mPresets->Add(rest);
         }
      } while (gPrefs->GetNextGroup(group,dummy));
   }
}

///
///
void ExportFFmpegOptions::FetchFormatList()
{
   AVOutputFormat *ofmt=NULL;
   ofmt = FFmpegLibsInst->av_oformat_next(ofmt);
   while (ofmt)
   {
      if (ofmt->audio_codec != CODEC_ID_NONE)
      {
         AVCodec *encoder = FFmpegLibsInst->avcodec_find_encoder(ofmt->audio_codec);
         if (encoder)
         {
            mFormatNames.Add(wxString::FromUTF8(ofmt->name));
            mFormatLongNames.Add(wxString::Format(wxT("%s - %s"),mFormatNames.Last().c_str(),wxString::FromUTF8(ofmt->long_name).c_str()));
         }
      }
      ofmt = FFmpegLibsInst->av_oformat_next(ofmt);
   }
   mShownFormatNames = mFormatNames;
   mShownFormatLongNames =  mFormatLongNames;
}

///
///
void ExportFFmpegOptions::FetchCodecList()
{
   AVCodec *codec=NULL;
   codec = FFmpegLibsInst->av_codec_next(codec);
   while (codec)
   {
      if (codec->type == CODEC_TYPE_AUDIO && codec->encode)
      {
         mCodecNames.Add(wxString::FromUTF8(codec->name));
         mCodecLongNames.Add(wxString::Format(wxT("%s - %s"),mCodecNames.Last().c_str(),wxString::FromUTF8(codec->long_name).c_str()));
      }
      codec = FFmpegLibsInst->av_codec_next(codec);
   }
   mShownCodecNames = mCodecNames;
   mShownCodecLongNames = mCodecLongNames;
}

/// 
/// 
void ExportFFmpegOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay(1);
   S.StartMultiColumn(5, wxEXPAND);
   {
      S.SetStretchyCol(1);
      mPresetCombo = S.Id(FEPresetID).AddCombo(_("Preset:"), gPrefs->Read(wxT("/FileFormats/FFmpegPreset"),wxEmptyString), mPresets);
      mLoadPreset = S.Id(FELoadPresetID).AddButton(_("Load Preset"));
      mSavePreset = S.Id(FESavePresetID).AddButton(_("Save Preset"));
      mDeletePreset = S.Id(FEDeletePresetID).AddButton(_("Delete Preset"));
   }
   S.EndMultiColumn();
   S.StartTwoColumn();
   {
      S.Id(FEFormatLabelID).AddVariableText(_("Format:"));
      mFormatName = S.Id(FEFormatNameID).AddVariableText(wxT(""));
      S.Id(FECodecLabelID).AddVariableText(_("Codec:"));
      mCodecName = S.Id(FECodecNameID).AddVariableText(wxT(""));
   }
   S.EndTwoColumn();
   S.AddVariableText(_("Not all formats and codecs are compatible. Some parameters (such as bitrate and samplerate) combinations are not compatible with some codecs too."),false);
   S.StartMultiColumn(3,wxEXPAND);
   {
      S.AddVariableText(_("Format selector:"),true);
      S.AddVariableText(_("Codec selector:"),true);
      S.AddVariableText(wxEmptyString);
      S.Id(FEAllFormatsID).AddButton(_("Show All Formats"));
      S.Id(FEAllCodecsID).AddButton(_("Show All Codecs"));
      S.AddVariableText(wxEmptyString);
      mFormatList = S.Id(FEFormatID).AddListBox(&mFormatNames);
      mCodecList = S.Id(FECodecID).AddListBox(&mCodecNames);
      mFormatList->DeselectAll();
      mCodecList->DeselectAll();

      S.StartStatic(wxT("Options"),0);
      {
         S.StartMultiColumn(4,wxALIGN_LEFT);
         {
            mLanguageText = S.Id(FELanguageID).TieTextBox(_("Language:"), wxT("/FileFormats/FFmpegLanguage"), wxEmptyString, 0);
            mLanguageText->SetToolTip(_("ISO 639 3-letter language code\nOptional\nempty - automatic"));

            mTag = S.Id(FETagID).TieTextBox(_("Tag:"), wxT("/FileFormats/FFmpegTag"), wxEmptyString, 0);
            mTag->SetToolTip(_("Codec tag (FOURCC)\nOptional\nempty - automatic"));

            mBitrateSpin = S.Id(FEBitrateID).TieSpinCtrl(_("Bit Rate:"), wxT("/FileFormats/FFmpegBitRate"), 0,1000000,0);
            mBitrateSpin->SetToolTip(_("Bit Rate (bits/second) - influences the resulting file size and quality\nSome codecs may only accept specific values (128k, 192k, 256k etc)\n0 - automatic\nRecommended - 192000"));

            mQualitySpin = S.Id(FEQualityID).TieSpinCtrl(_("Quality:"), wxT("/FileFormats/FFmpegQuality"), 0,500,0);
            mQualitySpin->SetToolTip(_("Overral quality, used differently by different codecs\nRequired for vorbis\n0 - automatic"));

            mSampleRateSpin = S.Id(FESampleRateID).TieSpinCtrl(_("Sample Rate:"), wxT("/FileFormats/FFmpegSampleRate"), 0,200000,0);
            mSampleRateSpin->SetToolTip(_("Sample rate (Hz)\n0 - don't change sample rate"));

            mCutoffSpin = S.Id(FECutoffID).TieSpinCtrl(_("Cutoff Bandwidth:"), wxT("/FileFormats/FFmpegCutOff"), 0,10000000,0);
            mCutoffSpin->SetToolTip(_("Audio cutoff bandwidth (Hz)\nOptional\n0 - automatic\n"));

            S.AddVariableText(wxT("Use Bit Reservoir"));
            S.Id(FEBitReservoirID).TieCheckBox(wxEmptyString, wxT("/FileFormats/FFmpegBitReservoir"), true);

            mProfileChoice = S.Id(FEProfileID).TieChoice(_("Profile:"), wxT("/FileFormats/FFmpegAACProfile"), 
               mProfileLabels[0], mProfileNames, mProfileLabels);


         }
         S.EndMultiColumn();
         S.StartStatic(wxT("FLAC options"),0);
         {
            S.StartMultiColumn(4);
            {
               mCompressionLevelSpin = S.Id(FECompLevelID).TieSpinCtrl(_("Compression Level:"), wxT("/FileFormats/FFmpegCompLevel"), 0,10,-1);
               mCompressionLevelSpin->SetToolTip(_("Compression level\nRequired for FLAC\n-1 - automatic\nmin - 0 (fast encoding, large output file)\nmax - 10 (slow encoding, small output file)"));

               mFrameSizeSpin =  S.Id(FEFrameSizeID).TieSpinCtrl(_("Frame Size:"), wxT("/FileFormats/FFmpegFrameSize"), 0,65535,0);
               mFrameSizeSpin->SetToolTip(_("Frame size\nOptional\n0 - default\nmin - 16\nmax - 65535"));

               mLPCCoeffsPrecisionSpin = S.Id(FELPCCoeffsID).TieSpinCtrl(_("LPC coefficients precision"), wxT("/FileFormats/FFmpegLPCCoefPrec"), 0,15,0);
               mLPCCoeffsPrecisionSpin->SetToolTip(_("LPC coefficients precision\nOptional\n0 - default\nmin - 1\nmax - 15"));

               mMinPredictionOrderSpin = S.Id(FEMinPredID).TieSpinCtrl(_("Minimal prediction order"), wxT("/FileFormats/FFmpegMinPredOrder"), -1,32,-1);
               mMinPredictionOrderSpin->SetToolTip(_("Minimal prediction order\nOptional\n-1 - default\nmin - 0\nmax - 32 (with LPC) or 4 (without LPC)"));

               mMaxPredictionOrderSpin = S.Id(FEMaxPredID).TieSpinCtrl(_("Maximal prediction order"), wxT("/FileFormats/FFmpegMaxPredOrder"), -1,32,-1);
               mMaxPredictionOrderSpin->SetToolTip(_("Maximal prediction order\nOptional\n-1 - default\nmin - 0\nmax - 32 (with LPC) or 4 (without LPC)"));

               mPredictionOrderMethodChoice = S.Id(FEPredOrderID).TieChoice(_("Prediction Order Method:"), wxT("/FileFormats/FFmpegPredOrderMethod"), 
                  mPredictionOrderMethodLabels[0], mPredictionOrderMethodNames, mPredictionOrderMethodLabels);

               mMinPartitionOrderSpin = S.Id(FEMinPartOrderID).TieSpinCtrl(_("Minimal partition order"), wxT("/FileFormats/FFmpegMinPartOrder"), -1,8,-1);
               mMinPartitionOrderSpin->SetToolTip(_("Minimal partition order\nOptional\n-1 - default\nmin - 0\nmax - 8"));

               mMaxPartitionOrderSpin = S.Id(FEMaxPartOrderID).TieSpinCtrl(_("Maximal partition order"), wxT("/FileFormats/FFmpegMaxPredOrder"), -1,8,-1);
               mMaxPartitionOrderSpin->SetToolTip(_("Maximal partition order\nOptional\n-1 - default\nmin - 0\nmax - 8"));

               S.AddVariableText(wxT("Use LPC"));
               S.Id(FEUseLPCID).TieCheckBox(wxEmptyString, wxT("/FileFormats/FFmpegUseLPC"), true);
               
            }
            S.EndMultiColumn();
         }
         S.EndStatic();
         S.StartStatic(wxT("MPEG container options"),0);
         {
            S.StartMultiColumn(4);
            {
               mMuxRate = S.Id(FEMuxRateID).TieSpinCtrl(_("Mux Rate:"), wxT("/FileFormats/FFmpegMuxRate"), 0,10000000,0);
               mMuxRate->SetToolTip(_("Maximum bit rate of the multiplexed stream\nOptional\n0 - default"));

               mPacketSize = S.Id(FEPacketSizeID).TieSpinCtrl(_("Packet Size:"), wxT("/FileFormats/FFmpegPacketSize"), 0,10000000,0);
               mPacketSize->SetToolTip(_("Packet size\nOptional\n0 - default"));
            }
            S.EndMultiColumn();
         }
         S.EndStatic();
      }
      S.EndStatic();
   }
   S.EndTwoColumn();
   S.EndVerticalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

///
///
void ExportFFmpegOptions::FindSelectedFormat(wxString **name, wxString **longname)
{
   wxArrayInt selections;
   int n = mFormatList->GetSelections(selections);
   if (n <= 0) return;
   wxString selfmt = mFormatList->GetString(selections[0]);
   int nFormat = mFormatNames.Index(selfmt.c_str());
   if (nFormat == wxNOT_FOUND) return;
   if (name != NULL) *name = &mFormatNames[nFormat];
   if (longname != NULL) *longname = &mFormatLongNames[nFormat];
   return;
}
///
///
void ExportFFmpegOptions::FindSelectedCodec(wxString **name, wxString **longname)
{
   wxArrayInt selections;
   int n = mCodecList->GetSelections(selections);
   if (n <= 0) return;
   wxString selcdc = mCodecList->GetString(selections[0]);
   int nCodec = mCodecNames.Index(selcdc.c_str());
   if (nCodec == wxNOT_FOUND) return;
   if (name != NULL) *name = &mCodecNames[nCodec];
   if (longname != NULL) *longname = &mCodecLongNames[nCodec];
}

///
///
int ExportFFmpegOptions::FetchCompatibleCodecList(const wxChar *fmt, int id)
{
   int index = -1;
   mShownCodecNames.Clear();
   mShownCodecLongNames.Clear();
   mCodecList->Clear();
   int found = 0;
   wxString str(fmt);
   for (int i = 0; CompatibilityList[i].fmt != NULL; i++)
   {
      if (str.Cmp(CompatibilityList[i].fmt) == 0)
      {
         found = 1;
         if (CompatibilityList[i].codec == CODEC_ID_NONE)
         {
            found = 2;
            break;
         }
         AVCodec *codec = FFmpegLibsInst->avcodec_find_encoder(CompatibilityList[i].codec);
         if (codec != NULL && (codec->type == CODEC_TYPE_AUDIO) && codec->encode)
         {
            if ((id >= 0) && codec->id == id) index = mShownCodecNames.GetCount();
            mShownCodecNames.Add(wxString::FromUTF8(codec->name));
            mShownCodecLongNames.Add(wxString::Format(wxT("%s - %s"),mShownCodecNames.Last().c_str(),wxString::FromUTF8(codec->long_name).c_str()));
         }
      }
   }
   if (found == 2)
   {
      AVCodec *codec = FFmpegLibsInst->av_codec_next(NULL);
      while (codec)
      {
         if (codec->type == CODEC_TYPE_AUDIO)
         {
            mShownCodecNames.Add(wxString::FromUTF8(codec->name));
            mShownCodecLongNames.Add(wxString::Format(wxT("%s - %s"),mShownCodecNames.Last().c_str(),wxString::FromUTF8(codec->long_name).c_str()));
            codec = FFmpegLibsInst->av_codec_next(codec);
         }
      }
   }
   else if (found == 0)
   {
      wxCharBuffer buf = str.ToUTF8();
      AVOutputFormat *format = FFmpegLibsInst->guess_format(buf,NULL,NULL);
      if (format != NULL)
      {
         AVCodec *codec = FFmpegLibsInst->avcodec_find_encoder(format->audio_codec);
         if (codec != NULL && (codec->type == CODEC_TYPE_AUDIO) && codec->encode)
         {
            if ((id >= 0) && codec->id == id) index = mShownCodecNames.GetCount();
            mShownCodecNames.Add(wxString::FromUTF8(codec->name));
            mShownCodecLongNames.Add(wxString::Format(wxT("%s - %s"),mShownCodecNames.Last().c_str(),wxString::FromUTF8(codec->long_name).c_str()));
         }
      }
   }
   mCodecList->Append(mShownCodecNames);
   return index;
}

///
///
int ExportFFmpegOptions::FetchCompatibleFormatList(CodecID id, wxString *selfmt)
{
   int index = -1;
   mShownFormatNames.Clear();
   mShownFormatLongNames.Clear();
   mFormatList->Clear();
   AVOutputFormat *ofmt = NULL;
   ofmt = FFmpegLibsInst->av_oformat_next(ofmt);
   wxArrayString FromList;
   for (int i = 0; CompatibilityList[i].fmt != NULL; i++)
   {
      if (CompatibilityList[i].codec == id)
      {
         if ((selfmt != NULL) && (selfmt->Cmp(CompatibilityList[i].fmt) == 0)) index = mShownFormatNames.GetCount();
         FromList.Add(CompatibilityList[i].fmt);
         mShownFormatNames.Add(CompatibilityList[i].fmt);
         AVOutputFormat *tofmt = FFmpegLibsInst->guess_format(wxString(CompatibilityList[i].fmt).ToUTF8(),NULL,NULL);
         if (tofmt != NULL) mShownFormatLongNames.Add(wxString::Format(wxT("%s - %s"),CompatibilityList[i].fmt,wxString::FromUTF8(tofmt->long_name).c_str()));
      }
   }
   while (ofmt)
   {
      if (ofmt->audio_codec == id)
      {
         wxString ofmtname = wxString::FromUTF8(ofmt->name);
         bool found = false;
         for (unsigned int i = 0; i < FromList.GetCount(); i++)
         {
            if (ofmtname.Cmp(FromList[i]) == 0)
            {
               found = true;
               break;
            }
         }
         if (!found)
         {
            if ((selfmt != NULL) && (selfmt->Cmp(wxString::FromUTF8(ofmt->name)) == 0)) index = mShownFormatNames.GetCount();
            mShownFormatNames.Add(wxString::FromUTF8(ofmt->name));
            mShownFormatLongNames.Add(wxString::Format(wxT("%s - %s"),mShownFormatNames.Last().c_str(),wxString::FromUTF8(ofmt->long_name).c_str()));
         }
      }
      ofmt = FFmpegLibsInst->av_oformat_next(ofmt);
   }
   mFormatList->Append(mShownFormatNames);
   return index;
}

///
///
void ExportFFmpegOptions::OnDeletePreset(wxCommandEvent& event)
{
   wxComboBox *preset = dynamic_cast<wxComboBox*>(FindWindowById(FEPresetID,this));
   wxString presetname = preset->GetValue();
   wxString groupname = wxString::Format(wxT("FFmpegExportPreset_%s"),presetname.c_str());
   gPrefs->DeleteGroup(groupname);
   mPresets->Remove(presetname.c_str());
}

///
///
void ExportFFmpegOptions::OnSavePreset(wxCommandEvent& event)
{
   wxComboBox *preset = dynamic_cast<wxComboBox*>(FindWindowById(FEPresetID,this));
   wxString name = preset->GetValue();
   wxString presetname = wxString::Format(wxT("/FFmpegExportPreset_%s/"),name.c_str());
   wxListBox *lb;
   wxSpinCtrl *sc;
   wxTextCtrl *tc;
   wxCheckBox *cb;
   wxChoice *ch;

   for (int id = FEFirstID; id < FELastID; id++)
   {
      wxWindow *wnd = FindWindowById(id,this);
      if (wnd != NULL)
      {
         wxString setting = wxString(presetname);
         switch(id)
         {
         case FEFormatID:
            lb = dynamic_cast<wxListBox*>(wnd);
            setting.append(wxT("FEFormatID"));
            if (lb->GetSelection() < 0) { wxMessageBox(wxT("Please select format before saving a profile")); return; }
            gPrefs->Write(setting,lb->GetString(lb->GetSelection()));
            break;
         case FECodecID:
            lb = dynamic_cast<wxListBox*>(wnd);
            setting.append(wxT("FECodecID"));
            if (lb->GetSelection() < 0) { wxMessageBox(wxT("Please select codec before saving a profile")); return; }
            gPrefs->Write(setting,lb->GetString(lb->GetSelection()));
            break;
         case FEFormatLabelID:
         case FECodecLabelID:
         case FEFormatNameID:
         case FECodecNameID:
         case FELogLevelID:
         case FEPresetID:
            break;
         case FEBitrateID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEBitrateID"));
            gPrefs->Write(setting,sc->GetValue());
            break;
         case FEQualityID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEQualityID"));
            gPrefs->Write(setting,sc->GetValue());
            break;
         case FESampleRateID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FESampleRateID"));
            gPrefs->Write(setting,sc->GetValue());
            break;
         case FELanguageID:
            tc = dynamic_cast<wxTextCtrl*>(wnd);
            setting.append(wxT("FELanguageID"));
            gPrefs->Write(setting,tc->GetValue());
            break;
         case FETagID:
            tc = dynamic_cast<wxTextCtrl*>(wnd);
            setting.append(wxT("FETagID"));
            gPrefs->Write(setting,tc->GetValue());
            break;
         case FECutoffID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FECutoffID"));
            gPrefs->Write(setting,sc->GetValue());
            break;
         case FEFrameSizeID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEFrameSizeID"));
            gPrefs->Write(setting,sc->GetValue());
            break;
         case FEBufSizeID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEBufSizeID"));
            gPrefs->Write(setting,sc->GetValue());
            break;
         case FEProfileID:
            ch = dynamic_cast<wxChoice*>(wnd);
            setting.append(wxT("FEProfileID"));
            gPrefs->Write(setting,ch->GetString(ch->GetSelection()));
            break;
         case FECompLevelID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FECompLevelID"));
            gPrefs->Write(setting,sc->GetValue());
            break;
         case FEUseLPCID:
            cb = dynamic_cast<wxCheckBox*>(wnd);
            setting.append(wxT("FEUseLPCID"));
            gPrefs->Write(setting,cb->GetValue());
            break;
         case FELPCCoeffsID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FELPCCoeffsID"));
            gPrefs->Write(setting,sc->GetValue());
            break;
         case FEMinPredID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEMinPredID"));
            gPrefs->Write(setting,sc->GetValue());
            break;
         case FEMaxPredID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEMaxPredID"));
            gPrefs->Write(setting,sc->GetValue());
            break;
         case FEPredOrderID:
            ch = dynamic_cast<wxChoice*>(wnd);
            setting.append(wxT("FEPredOrderID"));
            gPrefs->Write(setting,ch->GetString(ch->GetSelection()));
            break;
         case FEMinPartOrderID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEMinPartOrderID"));
            gPrefs->Write(setting,sc->GetValue());
            break;
         case FEMaxPartOrderID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEMaxPartOrderID"));
            gPrefs->Write(setting,sc->GetValue());
            break;
         case FEMuxRateID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEMuxRateID"));
            gPrefs->Write(setting,sc->GetValue());
            break;
         case FEPacketSizeID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEPacketSizeID"));
            gPrefs->Write(setting,sc->GetValue());
            break;
         case FEBitReservoirID:
            cb = dynamic_cast<wxCheckBox*>(wnd);
            setting.append(wxT("FEBitReservoirID"));
            gPrefs->Write(setting,cb->GetValue());
            break;
         case FESavePresetID:
         case FELoadPresetID:
         case FEDeletePresetID:
         case FEAllFormatsID:
         case FEAllCodecsID:
            break;
         }
      }
   }
   int index = mPresets->Index(name.c_str(),false);
   if (index == -1)
   {
      mPresets->Add(name);
      mPresetCombo->Clear();
      mPresetCombo->Append(*mPresets);
      mPresetCombo->Select(mPresets->Index(name,false));
   }
}

///
///
void ExportFFmpegOptions::OnLoadPreset(wxCommandEvent& event)
{
   wxComboBox *preset = dynamic_cast<wxComboBox*>(FindWindowById(FEPresetID,this));
   wxString presetname = preset->GetValue();
   wxString groupname = wxString::Format(wxT("FFmpegExportPreset_%s"),presetname.c_str());
   wxString settingprefix = wxString::Format(wxT("/FFmpegExportPreset_%s/"),presetname.c_str());
   wxString group;
   long dummy;
   bool found = false;
   if (gPrefs->GetFirstGroup(group,dummy))
   {
      do
      {
         wxString rest;
         if (group.CmpNoCase(groupname) == 0)
         {
            found = true;
            break;
         }
      } while (gPrefs->GetNextGroup(group,dummy));
   }
   if (!found)
   {
      wxMessageBox(wxString::Format(wxT("Preset '%s' does not exists"),presetname));
      return;
   }

   mShownFormatNames = mFormatNames;
   mShownFormatLongNames = mFormatLongNames;
   mFormatList->Clear();
   mFormatList->Append(mFormatNames);

   mShownCodecNames = mCodecNames;
   mShownCodecLongNames = mCodecLongNames;
   mCodecList->Clear();
   mCodecList->Append(mCodecNames);

   wxListBox *lb;
   wxSpinCtrl *sc;
   wxTextCtrl *tc;
   wxCheckBox *cb;
   wxChoice *ch;

   wxString readstr;
   long readlong;
   bool readbool;
   for (int id = FEFirstID; id < FELastID; id++)
   {
      wxWindow *wnd = FindWindowById(id,this);
      if (wnd != NULL)
      {
         wxString setting = wxString(settingprefix);
         switch(id)
         {
         case FEFormatID:
            lb = dynamic_cast<wxListBox*>(wnd);
            setting.append(wxT("FEFormatID"));
            gPrefs->Read(setting,&readstr,wxT("ogg"));
            readlong = lb->FindString(readstr);
            if (readlong > -1) lb->Select(readlong);
            break;
         case FECodecID:
            lb = dynamic_cast<wxListBox*>(wnd);
            setting.append(wxT("FECodecID"));
            gPrefs->Read(setting,&readstr,wxT("vorbis"));
            readlong = lb->FindString(readstr);
            if (readlong > -1) lb->Select(readlong);
            break;
         case FEFormatLabelID:
         case FECodecLabelID:
         case FEFormatNameID:
         case FECodecNameID:
         case FELogLevelID:
         case FEPresetID:
            break;
         case FEBitrateID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEBitrateID"));
            gPrefs->Read(setting,&readlong,0);
            sc->SetValue(readlong);
            break;
         case FEQualityID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEQualityID"));
            gPrefs->Read(setting,&readlong,0); sc->SetValue(readlong);
            break;
         case FESampleRateID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FESampleRateID"));
            gPrefs->Read(setting,&readlong,0); sc->SetValue(readlong);
            break;
         case FELanguageID:
            tc = dynamic_cast<wxTextCtrl*>(wnd);
            setting.append(wxT("FELanguageID"));
            gPrefs->Read(setting,&readstr,wxEmptyString);
            tc->SetValue(readstr);
            break;
         case FETagID:
            tc = dynamic_cast<wxTextCtrl*>(wnd);
            setting.append(wxT("FETagID"));
            gPrefs->Read(setting,&readstr,wxEmptyString);
            tc->SetValue(readstr);
            break;
         case FECutoffID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FECutoffID"));
            gPrefs->Read(setting,&readlong,0); sc->SetValue(readlong);
            break;
         case FEFrameSizeID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEFrameSizeID"));
            gPrefs->Read(setting,&readlong,0); sc->SetValue(readlong);
            break;
         case FEBufSizeID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEBufSizeID"));
            gPrefs->Read(setting,&readlong,0); sc->SetValue(readlong);
            break;
         case FEProfileID:
            ch = dynamic_cast<wxChoice*>(wnd);
            setting.append(wxT("FEProfileID"));
            gPrefs->Read(setting,&readstr,this->mProfileNames[0]);
            if (ch->FindString(readstr)) ch->Select(ch->FindString(readstr));
            break;
         case FECompLevelID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FECompLevelID"));
            gPrefs->Read(setting,&readlong,0); sc->SetValue(readlong);
            break;
         case FEUseLPCID:
            cb = dynamic_cast<wxCheckBox*>(wnd);
            setting.append(wxT("FEUseLPCID"));
            gPrefs->Read(setting,&readbool,true);
            cb->SetValue(readbool);
            break;
         case FELPCCoeffsID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FELPCCoeffsID"));
            gPrefs->Read(setting,&readlong,0); sc->SetValue(readlong);
            break;
         case FEMinPredID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEMinPredID"));
            gPrefs->Read(setting,&readlong,0); sc->SetValue(readlong);
            break;
         case FEMaxPredID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEMaxPredID"));
            gPrefs->Read(setting,&readlong,0); sc->SetValue(readlong);
            break;
         case FEPredOrderID:
            ch = dynamic_cast<wxChoice*>(wnd);
            setting.append(wxT("FEPredOrderID"));
            gPrefs->Read(setting,&readstr,this->mPredictionOrderMethodNames[0]);
            if (ch->FindString(readstr)) ch->Select(ch->FindString(readstr));
            break;
         case FEMinPartOrderID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEMinPartOrderID"));
            gPrefs->Read(setting,&readlong,0); sc->SetValue(readlong);
            break;
         case FEMaxPartOrderID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEMaxPartOrderID"));
            gPrefs->Read(setting,&readlong,0); sc->SetValue(readlong);
            break;
         case FEMuxRateID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEMuxRateID"));
            gPrefs->Read(setting,&readlong,0); sc->SetValue(readlong);
            break;
         case FEPacketSizeID:
            sc = dynamic_cast<wxSpinCtrl*>(wnd);
            setting.append(wxT("FEPacketSizeID"));
            gPrefs->Read(setting,&readlong,0); sc->SetValue(readlong);
            break;
         case FEBitReservoirID:
            cb = dynamic_cast<wxCheckBox*>(wnd);
            setting.append(wxT("FEBitReservoirID"));
            gPrefs->Read(setting,&readbool,true);
            cb->SetValue(readbool);
            break;
         case FESavePresetID:
         case FELoadPresetID:
         case FEDeletePresetID:
         case FEAllFormatsID:
         case FEAllCodecsID:
            break;
         }
      }
   }
}

///
///
void ExportFFmpegOptions::OnAllFormats(wxCommandEvent& event)
{
   mShownFormatNames = mFormatNames;
   mShownFormatLongNames = mFormatLongNames;
   mFormatList->Clear();
   mFormatList->Append(mFormatNames);
}

///
///
void ExportFFmpegOptions::OnAllCodecs(wxCommandEvent& event)
{
   mShownCodecNames = mCodecNames;
   mShownCodecLongNames = mCodecLongNames;
   mCodecList->Clear();
   mCodecList->Append(mCodecNames);
}

///
///
void ExportFFmpegOptions::OnFormatList(wxCommandEvent& event)
{
   wxString *selfmt = NULL;
   wxString *selfmtlong = NULL;
   FindSelectedFormat(&selfmt, &selfmtlong);
   if (selfmt == NULL)
   {
      return;
   }

   wxString *selcdc = NULL;
   wxString *selcdclong = NULL;
   FindSelectedCodec(&selcdc, &selcdclong);

   AVOutputFormat *fmt = FFmpegLibsInst->guess_format(selfmt->ToUTF8(),NULL,NULL);
   if (fmt == NULL)
   {
      //This shouldn't really happen
      mFormatName->SetLabel(wxString(wxT("Failed to guess format")));
      return;
   }
   mFormatName->SetLabel(wxString::Format(wxT("%s"),selfmtlong->c_str()));
   int selcdcid = -1;

   if (selcdc != NULL)
   {
      AVCodec *cdc = FFmpegLibsInst->avcodec_find_encoder_by_name(selcdc->ToUTF8());
      if (cdc != NULL)
      {
         selcdcid = cdc->id;
      }
   }
   int newselcdc = FetchCompatibleCodecList(selfmt->c_str(), selcdcid);
   if (newselcdc > 0) mCodecList->Select(newselcdc);
/*   else
   {
      mShownCodecNames.Clear();
      mShownCodecLongNames.Clear();
      mCodecList->Clear();
      
      CodecID id = fmt->audio_codec;
      AVCodec *cdc = FFmpegLibsInst->avcodec_find_encoder(id);
      if (cdc != NULL)
      {
         mShownCodecNames.Add(wxString::FromUTF8(cdc->name));
         mShownCodecLongNames.Add(wxString::Format(wxT("%s - %s"),mShownCodecNames.Last().c_str(),wxString::FromUTF8(cdc->long_name).c_str()));
         mCodecList->Append(mShownCodecNames);
         mCodecList->Select(0);
      }
   }*/
   int handled = -1;
   AVCodec *cdc = NULL;
   if (selcdc != NULL)
      cdc = FFmpegLibsInst->avcodec_find_encoder_by_name(selcdc->ToUTF8());
   for (int i = 0; apptable[i].control != 0; i++)
   {
      if (apptable[i].control != handled)
      {
         bool codec = false;
         bool format = false;
         if (apptable[i].codec == CODEC_ID_NONE) codec = true;
         else if (cdc != NULL && apptable[i].codec == cdc->id) codec = true;
         if (apptable[i].format == "any") format = true;
         else if (selfmt != NULL && selfmt->Cmp(wxString::FromUTF8(apptable[i].format)) == 0) format = true;
         if (codec && format)
         {
            handled = apptable[i].control;
            wxWindow *item = FindWindowById(apptable[i].control,this);
            if (item != NULL) item->Enable(apptable[i].enable);
         }
      }
   }
   return;
}

///
///
void ExportFFmpegOptions::OnCodecList(wxCommandEvent& event)
{
   wxString *selcdc = NULL;
   wxString *selcdclong = NULL;
   FindSelectedCodec(&selcdc, &selcdclong);
   if (selcdc == NULL)
   {
      return;
   }

   wxString *selfmt = NULL;
   wxString *selfmtlong = NULL;
   FindSelectedFormat(&selfmt, &selfmtlong);

   AVCodec *cdc = FFmpegLibsInst->avcodec_find_encoder_by_name(selcdc->ToUTF8());
   if (cdc == NULL)
   {
      //This shouldn't really happen
      mCodecName->SetLabel(wxString(wxT("Failed to find the codec")));
      return;
   }
   mCodecName->SetLabel(wxString::Format(wxT("[%d] %s"),cdc->id,selcdclong->c_str()));

   if (selfmt != NULL)
   {
      AVOutputFormat *fmt = FFmpegLibsInst->guess_format(selfmt->ToUTF8(),NULL,NULL);
      if (fmt == NULL)
      {
         selfmt = NULL;
         selfmtlong = NULL;
      }
   }

   int newselfmt = FetchCompatibleFormatList(cdc->id,selfmt);
   if (newselfmt > 0) mFormatList->Select(newselfmt);

   int handled = -1;
   AVOutputFormat *fmt = NULL;
   for (int i = 0; apptable[i].control != 0; i++)
   {
      if (apptable[i].control != handled)
      {
         bool codec = false;
         bool format = false;
         if (apptable[i].codec == CODEC_ID_NONE) codec = true;
         else if (cdc != NULL && apptable[i].codec == cdc->id) codec = true;
         if (apptable[i].format == "any") format = true;
         else if (selfmt != NULL && selfmt->Cmp(wxString::FromUTF8(apptable[i].format)) == 0) format = true;
         if (codec && format)
         {
            handled = apptable[i].control;
            wxWindow *item = FindWindowById(apptable[i].control,this);
            if (item != NULL) item->Enable(apptable[i].enable);
         }
      }
   }

   return;
}

///
///
void ExportFFmpegOptions::OnOK(wxCommandEvent& event)
{
   int selcdc = mCodecList->GetSelection();
   int selfmt = mFormatList->GetSelection();
   if (selcdc > -1) gPrefs->Write(wxT("/FileFormats/FFmpegCodec"),(long)FFmpegLibsInst->avcodec_find_encoder_by_name(mCodecList->GetString(selcdc).ToUTF8())->id);
   if (selfmt > -1) gPrefs->Write(wxT("/FileFormats/FFmpegFormat"),mFormatList->GetString(selfmt));
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}


//----------------------------------------------------------------------------
// ExportFFmpeg
//----------------------------------------------------------------------------

class ExportFFmpeg : public ExportPlugin
{
public:

   ExportFFmpeg();
   void Destroy();

   bool Init(const char *shortname, AudacityProject *project);
   bool InitCodecs(AudacityProject *project);
   bool AddTags(Tags *metadata);
   bool EncodeAudioFrame(int16_t *pFrame, int frameSize);
   bool Finalize();
   bool DisplayOptions(AudacityProject *project = NULL, int format = 0);
   bool CheckSampleRate(int rate, int lowrate, int highrate, const int *sampRates);
   int  AskResample(int bitrate, int rate, int lowrate, int highrate, const int *sampRates);
   bool Export(AudacityProject *project,
      int channels,
      wxString fName,
      bool selectedOnly,
      double t0,
      double t1,
      MixerSpec *mixerSpec = NULL,
      Tags *metadata = NULL,
      int subformat = 0);

private:

   AVFormatContext *	mEncFormatCtx;			// libavformat's context for our output file
   AVOutputFormat *	mEncFormatDesc;			// describes our output file to libavformat

   AVStream *		mEncAudioStream;			// the output audio stream (may remain NULL)
   AVCodecContext *	mEncAudioCodecCtx;		// the encoder for the output audio stream
   uint8_t *		mEncAudioEncodedBuf;		// buffer to hold frames encoded by the encoder
   int			mEncAudioEncodedBufSiz;		
   AVFifoBuffer		mEncAudioFifo;				// FIFO to write incoming audio samples into
   uint8_t *		mEncAudioFifoOutBuf;		// buffer to read _out_ of the FIFO into

   bool                  mCancelled;
   wxString              mName;

   int                   mSubFormat;
   int                   mBitRate;
   int                   mSampleRate;
   int                   mChannels;
   bool                  mSupportsUTF8;
};

ExportFFmpeg::ExportFFmpeg()
:  ExportPlugin()
{

   PickFFmpegLibs();
   FFmpegLibsInst->LoadLibs(NULL,false);
   int newfmt;

   for (newfmt = 0; newfmt < FMT_LAST; newfmt++)
   {
      //If codec is not compiled in, do not add codec
      if (!FFmpegLibsInst->ValidLibsLoaded() ||
         ( fmts[newfmt].codecid != CODEC_ID_NONE &&
         FFmpegLibsInst->avcodec_find_encoder(fmts[newfmt].codecid) != NULL ||
         fmts[newfmt].codecid == CODEC_ID_NONE )
         )
      {
         int fmtindex = AddFormat() - 1;
         SetFormat(fmts[newfmt].name,fmtindex);
         AddExtension(fmts[newfmt].extension,fmtindex);
         switch(newfmt)
         {
         case FMT_M4A:
            AddExtension(wxString(wxT("mov")),fmtindex);
            AddExtension(wxString(wxT("3gp")),fmtindex);
            AddExtension(wxString(wxT("mp4")),fmtindex);
            break;
         case FMT_WMA2:
            AddExtension(wxString(wxT("asf")),fmtindex);
            AddExtension(wxString(wxT("wmv")),fmtindex);
            break;
         default:
            break;
         }
         SetMaxChannels(fmts[newfmt].maxchannels,fmtindex);
         SetCanMetaData(fmts[newfmt].canmetadata,fmtindex);
         SetDescription(fmts[newfmt].description,fmtindex);
      }
   }

   mEncFormatCtx = NULL;			// libavformat's context for our output file
   mEncFormatDesc = NULL;			// describes our output file to libavformat
   mEncAudioStream = NULL;			// the output audio stream (may remain NULL)
   mEncAudioCodecCtx = NULL;		// the encoder for the output audio stream
   mEncAudioEncodedBuf = NULL;		// buffer to hold frames encoded by the encoder
   #define MAX_AUDIO_PACKET_SIZE (128 * 1024)
   mEncAudioEncodedBufSiz = 4*MAX_AUDIO_PACKET_SIZE;
   mEncAudioFifoOutBuf = NULL;		// buffer to read _out_ of the FIFO into
   mSampleRate = 0;
   mSupportsUTF8 = true;
}

void ExportFFmpeg::Destroy()
{
   DropFFmpegLibs();
   delete this;
}

bool ExportFFmpeg::Init(const char *shortname,AudacityProject *project)
{
   int err;
   FFmpegLibsInst->LoadLibs(NULL,true);

   if (!FFmpegLibsInst->ValidLibsLoaded()) return false;

   FFmpegLibsInst->av_log_set_callback(av_log_wx_callback);

   AVFormatParameters	fpOutFile;

   // See if libavformat has modules that can write our output format. If so, mEncFormatDesc
   // will describe the functions used to write the format (used internally by libavformat)
   // and the default video/audio codecs that the format uses.
   if ((mEncFormatDesc = FFmpegLibsInst->guess_format(shortname, OSFILENAME(mName), NULL)) == NULL)
   {
      wxLogMessage(wxT("FFmpeg : ERROR - Can't determine format description for file \"%s\"."), mName.c_str());
      return false;
   }

   // mEncFormatCtx is used by libavformat to carry around context data re our output file.
   if ((mEncFormatCtx = FFmpegLibsInst->av_alloc_format_context()) == NULL)
   {
      wxLogMessage(wxT("FFmpeg : ERROR - Can't allocate output format context."));
      return false;
   }

   // Initialise the output format context.
   mEncFormatCtx->oformat = mEncFormatDesc;
   wxString tName(mName);
   memcpy(mEncFormatCtx->filename,OSFILENAME(mName),strlen(OSFILENAME(mName))+1);
   
   if ((mEncAudioStream = FFmpegLibsInst->av_new_stream(mEncFormatCtx, 1)) == NULL)
   {
      wxLogMessage(wxT("FFmpeg : ERROR - Can't add audio stream to output file \"%s\"."), mName.c_str());
      return false;
   }

   mEncFormatCtx->timestamp = 0;

   // Open the output file.
   if (!(mEncFormatDesc->flags & AVFMT_NOFILE))
   {
      if ((err = FFmpegLibsInst->url_fopen(&mEncFormatCtx->pb, mEncFormatCtx->filename, URL_WRONLY)) < 0)
      {
         wxLogMessage(wxT("FFmpeg : ERROR - Can't open output file \"%s\" to write. Error code is %d."), mName.c_str(),err);
         return false;
      }
   }

   // Set default parameters on the format context.
   memset(&fpOutFile, 0, sizeof(AVFormatParameters));
   if ((err = FFmpegLibsInst->av_set_parameters(mEncFormatCtx, &fpOutFile)) < 0)
   {
      wxLogMessage(wxT("FFmpeg : ERROR - Can't set output parameters for output file \"%s\". Error code is %d."), mName.c_str(),err);
      return false;
   }

   mEncFormatCtx->preload   = (int)(0.5 * AV_TIME_BASE);
   mEncFormatCtx->max_delay = (int)(0.7 * AV_TIME_BASE);

   // Open the audio stream's codec and initialise any stream related data.
   if (!InitCodecs(project))
      return false;

   // Write headers to the output file.
   if ((err = FFmpegLibsInst->av_write_header(mEncFormatCtx)) < 0)
   {
      wxLogMessage(wxT("FFmpeg : ERROR - Can't write headers to output file \"%s\". Error code is %d."), mName.c_str(),err);

      return false;
   }

   return true;
}

bool ExportFFmpeg::CheckSampleRate(int rate, int lowrate, int highrate, const int *sampRates)
{
   if (rate < lowrate || rate > highrate) return false;
   for (int i = 0; sampRates[i] > 0; i++)
      if (rate == sampRates[i]) return true;
   return false;
}

bool ExportFFmpeg::InitCodecs(AudacityProject *project)
{
   AVCodec *	codec = NULL;

   // Configure the audio stream's codec context.
   mEncAudioCodecCtx = mEncAudioStream->codec;
  
   FFmpegLibsInst->avcodec_get_context_defaults(mEncAudioCodecCtx);

   mEncAudioCodecCtx->codec_id = fmts[mSubFormat].codecid;
   mEncAudioCodecCtx->codec_type = CODEC_TYPE_AUDIO;
   mEncAudioCodecCtx->codec_tag = FFmpegLibsInst->av_codec_get_tag(mEncFormatCtx->oformat->codec_tag,mEncAudioCodecCtx->codec_id);
   mSampleRate = project->GetRate();
   switch (mSubFormat)
   {
   case FMT_PCMS16LEWAV:
      break;
   case FMT_M4A:
      mEncAudioCodecCtx->bit_rate = 0;
      mEncAudioCodecCtx->bit_rate *= mChannels;
      mEncAudioCodecCtx->profile = FF_PROFILE_AAC_LOW;
      mEncAudioCodecCtx->cutoff = mSampleRate/2;
      mEncAudioCodecCtx->global_quality = gPrefs->Read(wxT("/FileFormats/AACQuality"),-1);
      if (!CheckSampleRate(mSampleRate,iAACSampleRates[0],iAACSampleRates[15],&iAACSampleRates[0]))
         mSampleRate = AskResample(mEncAudioCodecCtx->bit_rate,mSampleRate,iAACSampleRates[0],iAACSampleRates[15],&iAACSampleRates[0]);
      break;
   case FMT_AC3:
      mEncAudioCodecCtx->bit_rate = gPrefs->Read(wxT("/FileFormats/AC3BitRate"), 192000);
      if (!CheckSampleRate(mSampleRate,iAC3SampleRates[0],iAC3SampleRates[2],&iAC3SampleRates[0]))
         mSampleRate = AskResample(mEncAudioCodecCtx->bit_rate,mSampleRate,iAC3SampleRates[0],iAC3SampleRates[2],&iAC3SampleRates[0]);
      break;
   case FMT_GSMAIFF:
   case FMT_GSMMSWAV:
      mSampleRate = 8000;
      mEncAudioCodecCtx->bit_rate = 13000;
      break;
   case FMT_AMRNB:
      mSampleRate = 8000;
      mEncAudioCodecCtx->bit_rate = gPrefs->Read(wxT("/FileFormats/AMRNBBitRate"), 12200);
      break;
   case FMT_AMRWB:
      mSampleRate = 16000;
      mEncAudioCodecCtx->bit_rate = gPrefs->Read(wxT("/FileFormats/AMRWBBitRate"), 23850);
      break;
   case FMT_WMA2:
      mEncAudioCodecCtx->bit_rate = gPrefs->Read(wxT("/FileFormats/WMABitRate"), 198000);
      //Can't find any samplerate restrictions for WMA.
      break;
   case FMT_OTHER:
      memcpy(mEncAudioStream->language,gPrefs->Read(wxT("/FileFormats/FFmpegLanguage"),wxT("")).c_str(),4);
      mEncAudioCodecCtx->sample_rate = gPrefs->Read(wxT("/FileFormats/FFmpegSampleRate"),(long)0);
      if (mEncAudioCodecCtx->sample_rate != 0) mSampleRate = mEncAudioCodecCtx->sample_rate;
      mEncAudioCodecCtx->bit_rate = gPrefs->Read(wxT("/FileFormats/FFmpegBitRate"), (long)0);
      memcpy(&mEncAudioCodecCtx->codec_tag,gPrefs->Read(wxT("/FileFormats/FFmpegTag"),wxT("")).c_str(),4);
      mEncAudioCodecCtx->global_quality = gPrefs->Read(wxT("/FileFormats/FFmpegQuality"),(long)0);
      mEncAudioCodecCtx->cutoff = gPrefs->Read(wxT("/FileFormats/FFmpegCutOff"),(long)0);
      mEncAudioCodecCtx->use_lpc = gPrefs->Read(wxT("/FileFormats/FFmpegUseLPC"),true);
      mEncAudioCodecCtx->flags2 = 0;
      if (gPrefs->Read(wxT("/FileFormats/FFmpegBitReservoir"),true)) mEncAudioCodecCtx->flags2 |= CODEC_FLAG2_BIT_RESERVOIR;
      mEncAudioCodecCtx->use_lpc = gPrefs->Read(wxT("/FileFormats/FFmpegUseLPC"),true);
      mEncAudioCodecCtx->compression_level = gPrefs->Read(wxT("/FileFormats/FFmpegCompLevel"),-1);
      mEncAudioCodecCtx->frame_size = gPrefs->Read(wxT("/FileFormats/FFmpegFrameSize"),(long)0);
      mEncAudioCodecCtx->lpc_coeff_precision = gPrefs->Read(wxT("/FileFormats/FFmpegLPCCoefPrec"),(long)0);
      mEncAudioCodecCtx->min_prediction_order = gPrefs->Read(wxT("/FileFormats/FFmpegMinPredOrder"),(long)-1);
      mEncAudioCodecCtx->max_prediction_order = gPrefs->Read(wxT("/FileFormats/FFmpegMaxPredOrder"),(long)-1);
      mEncAudioCodecCtx->min_partition_order = gPrefs->Read(wxT("/FileFormats/FFmpegMinPartOrder"),(long)-1);
      mEncAudioCodecCtx->max_partition_order = gPrefs->Read(wxT("/FileFormats/FFmpegMaxPartOrder"),(long)-1);
      mEncAudioCodecCtx->prediction_order_method = gPrefs->Read(wxT("/FileFormats/FFmpegPredOrderMethod"),(long)0);
      mEncFormatCtx->mux_rate = gPrefs->Read(wxT("/FileFormats/FFmpegMuxRate"),(long)0);
      mEncFormatCtx->packet_size = gPrefs->Read(wxT("/FileFormats/FFmpegPacketSize"),(long)0);
      mEncAudioCodecCtx->codec_id = (CodecID)gPrefs->Read(wxT("/FileFormats/FFmpegCodec"), mEncFormatDesc->audio_codec);
      break;
   default:
      return false;
   }

   mEncAudioCodecCtx->sample_rate = mSampleRate;
   mEncAudioCodecCtx->channels = mChannels;
   mEncAudioCodecCtx->time_base.num = 0;
   mEncAudioCodecCtx->time_base.den = 1;
   mEncAudioCodecCtx->sample_fmt = SAMPLE_FMT_S16;
   mEncAudioCodecCtx->strict_std_compliance = FF_COMPLIANCE_STRICT;

   // Is the required audio codec compiled into libavcodec?
   if ((codec = FFmpegLibsInst->avcodec_find_encoder(mEncAudioCodecCtx->codec_id)) == NULL)
   {
      wxLogMessage(wxT("FFmpeg : ERROR - Can't find audio codec %d."),mEncAudioCodecCtx->codec_id);
      wxMessageBox(wxString::Format(wxT("FFmpeg cannot find audio codec %d.\nSupport for this codec is probably not compiled in."),mEncAudioCodecCtx->codec_id));
      return false;
   }

   if (mEncFormatCtx->oformat->flags & AVFMT_GLOBALHEADER)
   {
      mEncAudioCodecCtx->flags |= CODEC_FLAG_GLOBAL_HEADER;
      mEncAudioStream->codec->flags |= CODEC_FLAG_GLOBAL_HEADER;
   }

   // Open the codec.
   if (FFmpegLibsInst->avcodec_open(mEncAudioCodecCtx, codec) < 0) 
   {
      wxLogMessage(wxT("FFmpeg : ERROR - Can't open audio codec %d."),mEncAudioCodecCtx->codec_id);
      return false;
   }
 
   wxLogMessage(wxT("FFmpeg : Audio Output Codec Frame Size: %d samples."), mEncAudioCodecCtx->frame_size);

   if ((mEncAudioCodecCtx->codec_id >= CODEC_ID_PCM_S16LE) && (mEncAudioCodecCtx->codec_id <= CODEC_ID_PCM_DVD))
   {
      mEncAudioEncodedBufSiz = FF_MIN_BUFFER_SIZE;
   }
   // Allocate a buffer for the encoder to store encoded audio frames into.
   if ((mEncAudioEncodedBuf = (uint8_t*)FFmpegLibsInst->av_malloc(mEncAudioEncodedBufSiz)) == NULL)
   {
      wxLogMessage(wxT("FFmpeg : ERROR - Can't allocate buffer to hold encoded audio."));
      return false;
   }

   // The encoder may require a minimum number of raw audio samples for each encoding but we can't
   // guarantee we'll get this minimum each time an audio frame is decoded from the input file so 
   // we use a FIFO to store up incoming raw samples until we have enough for one call to the codec.
   FFmpegLibsInst->av_fifo_init(&mEncAudioFifo, 1024);

   // Allocate a buffer to read OUT of the FIFO into. The FIFO maintains its own buffer internally.
   if ((mEncAudioFifoOutBuf = (uint8_t*)FFmpegLibsInst->av_malloc(2*MAX_AUDIO_PACKET_SIZE)) == NULL)
   {
      wxLogMessage(wxT("FFmpeg : ERROR - Can't allocate buffer to read into from audio FIFO."));
      return false;
   }

   return true;
}

bool ExportFFmpeg::Finalize()
{
   int i, nEncodedBytes;

   // Flush the audio FIFO and encoder.
   for (;;)
   {
      AVPacket	pkt;
      int		nFifoBytes = FFmpegLibsInst->av_fifo_size(&mEncAudioFifo);	// any bytes left in audio FIFO?

      nEncodedBytes = 0;
      int		nAudioFrameSizeOut = mEncAudioCodecCtx->frame_size * mEncAudioCodecCtx->channels * sizeof(int16_t);
      if (mEncAudioCodecCtx->frame_size == 1) nAudioFrameSizeOut = mEncAudioEncodedBufSiz;

      // Flush the audio FIFO first if necessary. It won't contain a _full_ audio frame because
      // if it did we'd have pulled it from the FIFO during the last encodeAudioFrame() call - 
      // the encoder must support short/incomplete frames for this to work.
      if (nFifoBytes > 0)
      {
         memset(mEncAudioFifoOutBuf,0,nAudioFrameSizeOut);
         AVCodec *codec = mEncAudioCodecCtx->codec;
         if ((codec->capabilities & CODEC_CAP_SMALL_LAST_FRAME)
            || codec->id == CODEC_ID_FLAC
            || mEncAudioCodecCtx->frame_size == 1
            || gPrefs->Read(wxT("/FileFormats/OverrideSmallLastFrame"),(long)1)
            )
         {
            int nFrameSizeTmp = mEncAudioCodecCtx->frame_size;

            // The last frame is going to contain a smaller than usual number of samples.
            if (mEncAudioCodecCtx->frame_size != 1)
               mEncAudioCodecCtx->frame_size = nFifoBytes / (mEncAudioCodecCtx->channels * sizeof(int16_t));

            wxLogMessage(wxT("FFmpeg : Audio FIFO still contains %d bytes, writing short %d sample frame ..."), 
               nFifoBytes, mEncAudioCodecCtx->frame_size);

            // Pull the bytes out from the FIFO and feed them to the encoder.
            if (FFmpegLibsInst->av_fifo_read(&mEncAudioFifo, mEncAudioFifoOutBuf, nFifoBytes) == 0)
            {
               if (mEncAudioCodecCtx->frame_size != 1)
                  nEncodedBytes = FFmpegLibsInst->avcodec_encode_audio(mEncAudioCodecCtx, mEncAudioEncodedBuf, mEncAudioEncodedBufSiz, (int16_t*)mEncAudioFifoOutBuf);
               else
                  nEncodedBytes = FFmpegLibsInst->avcodec_encode_audio(mEncAudioCodecCtx, mEncAudioEncodedBuf, nFifoBytes, (int16_t*)mEncAudioFifoOutBuf);
            }

            mEncAudioCodecCtx->frame_size = nFrameSizeTmp;		// restore the native frame size
         }
      }

      // Now flush the encoder.
      if (nEncodedBytes <= 0)
         nEncodedBytes = FFmpegLibsInst->avcodec_encode_audio(mEncAudioCodecCtx, mEncAudioEncodedBuf, mEncAudioEncodedBufSiz, NULL);

      if (nEncodedBytes <= 0)			
         break;

      // Okay, we got a final encoded frame we can write to the output file.
      FFmpegLibsInst->av_init_packet(&pkt);

      pkt.stream_index = mEncAudioStream->index;
      pkt.data = mEncAudioEncodedBuf;
      pkt.size = nEncodedBytes;
      pkt.flags |= PKT_FLAG_KEY;

      // Set presentation time of frame (currently in the codec's timebase) in the stream timebase.
      if(mEncAudioCodecCtx->coded_frame && mEncAudioCodecCtx->coded_frame->pts != AV_NOPTS_VALUE)
         pkt.pts = FFmpegLibsInst->av_rescale_q(mEncAudioCodecCtx->coded_frame->pts, mEncAudioCodecCtx->time_base, mEncAudioStream->time_base);

      if (FFmpegLibsInst->av_interleaved_write_frame(mEncFormatCtx, &pkt) != 0)
      {
         wxLogMessage(wxT("FFmpeg : ERROR - Couldn't write last audio frame to output file."));
         break;
      }
   }

   // Close the codecs.
   if (mEncAudioStream != NULL)
      FFmpegLibsInst->avcodec_close(mEncAudioStream->codec);

   // Write any file trailers.
   FFmpegLibsInst->av_write_trailer(mEncFormatCtx);

   for (i = 0; i < (int)mEncFormatCtx->nb_streams; i++)
   {
      FFmpegLibsInst->av_freep(&mEncFormatCtx->streams[i]->codec);
      FFmpegLibsInst->av_freep(&mEncFormatCtx->streams[i]);
   }

   // Close the output file if we created it.
   if (!(mEncFormatDesc->flags & AVFMT_NOFILE))
      FFmpegLibsInst->url_fclose(mEncFormatCtx->pb);

   // Free any buffers or structures we allocated.
   FFmpegLibsInst->av_free(mEncFormatCtx);

   if (mEncAudioEncodedBuf != NULL)
      FFmpegLibsInst->av_free(mEncAudioEncodedBuf);

   if (mEncAudioFifoOutBuf != NULL)
      FFmpegLibsInst->av_free(mEncAudioFifoOutBuf);

   FFmpegLibsInst->av_fifo_free(&mEncAudioFifo);
   return true;
}

bool ExportFFmpeg::EncodeAudioFrame(int16_t *pFrame, int frameSize)
{
   AVPacket	pkt;
   int		nBytesToWrite = 0;
   uint8_t *	pRawSamples = NULL;
   int		nAudioFrameSizeOut = mEncAudioCodecCtx->frame_size * mEncAudioCodecCtx->channels * sizeof(int16_t);
   if (mEncAudioCodecCtx->frame_size == 1) nAudioFrameSizeOut = mEncAudioEncodedBufSiz;
   int      ret;

   nBytesToWrite = frameSize;
   pRawSamples  = (uint8_t*)pFrame;
   FFmpegLibsInst->av_fifo_realloc(&mEncAudioFifo, FFmpegLibsInst->av_fifo_size(&mEncAudioFifo) + frameSize);
   // Put the raw audio samples into the FIFO.
   ret = FFmpegLibsInst->av_fifo_generic_write(&mEncAudioFifo, pRawSamples, nBytesToWrite,NULL);
   wxASSERT(ret == nBytesToWrite);

   // Read raw audio samples out of the FIFO in nAudioFrameSizeOut byte-sized groups to encode.
   while ((ret = FFmpegLibsInst->av_fifo_size(&mEncAudioFifo)) >= nAudioFrameSizeOut)
   {
      ret = FFmpegLibsInst->av_fifo_read(&mEncAudioFifo, mEncAudioFifoOutBuf, nAudioFrameSizeOut);
      FFmpegLibsInst->av_init_packet(&pkt);

      pkt.size = FFmpegLibsInst->avcodec_encode_audio(mEncAudioCodecCtx, 
         mEncAudioEncodedBuf, mEncAudioEncodedBufSiz,		// out
         (int16_t*)mEncAudioFifoOutBuf);				// in
      if (mEncAudioCodecCtx->frame_size == 1) { wxASSERT(pkt.size == mEncAudioEncodedBufSiz); }
      if (pkt.size < 0)
      {
         wxLogMessage(wxT("FFmpeg : ERROR - Can't encode audio frame."));
         return false;
      }

      // Rescale from the codec time_base to the AVStream time_base.
      if (mEncAudioCodecCtx->coded_frame && mEncAudioCodecCtx->coded_frame->pts != AV_NOPTS_VALUE)
         pkt.pts = FFmpegLibsInst->av_rescale_q(mEncAudioCodecCtx->coded_frame->pts, mEncAudioCodecCtx->time_base, mEncAudioStream->time_base);
      //wxLogMessage(wxT("FFmpeg : (%d) Writing audio frame with PTS: %lld."), mEncAudioCodecCtx->frame_number, pkt.pts);

      pkt.stream_index = mEncAudioStream->index;
      pkt.data = mEncAudioEncodedBuf;
      pkt.flags |= PKT_FLAG_KEY;

      // Write the encoded audio frame to the output file.
      if ((ret = FFmpegLibsInst->av_write_frame(mEncFormatCtx, &pkt)) != 0)
      {
         wxLogMessage(wxT("FFmpeg : ERROR - Failed to write audio frame to file."));
         return false;
      }
   }
   return true;
}


bool ExportFFmpeg::Export(AudacityProject *project,
                       int channels, wxString fName,
                       bool selectionOnly, double t0, double t1, MixerSpec *mixerSpec, Tags *metadata, int subformat)
{
   mChannels = channels;
   if (channels > fmts[subformat].maxchannels)
   {
      wxLogMessage(wxT("Attempted to export %d channels, but max. channels = %d"),channels,fmts[subformat].maxchannels);
      wxMessageBox(wxString::Format(wxT("Attempted to export %d channels, but max. channels for selected output format is %d"),channels,fmts[subformat].maxchannels),wxT("Error"));
      return false;
   }
   mName = fName;
   mSubFormat = subformat;
   TrackList *tracks = project->GetTracks();
   bool ret = true;

   if (mSubFormat >= FMT_LAST) return false;
   
   wxString ext(fmts[mSubFormat].extension);
   if (mSubFormat == FMT_OTHER)
      ext = gPrefs->Read(wxT("/FileFormats/FFmpegFormat"),wxT("mka"));
   ret = Init(ext.ToAscii(),project);

   if (!ret) return false;

   if (metadata == NULL) metadata = project->GetTags();

   if (fmts[mSubFormat].canmetadata)
   {
      this->mSupportsUTF8 = fmts[mSubFormat].canutf8;
      AddTags(metadata);
   }

   int pcmBufferSize = 1024;
   int numWaveTracks;
   WaveTrack **waveTracks;
   tracks->GetWaveTracks(selectionOnly, &numWaveTracks, &waveTracks);
   Mixer *mixer = new Mixer(numWaveTracks, waveTracks,
      tracks->GetTimeTrack(),
      t0, t1,
      channels, pcmBufferSize, true,
      mSampleRate, int16Sample, true, mixerSpec);

   ProgressDialog *progress = new ProgressDialog(wxFileName(fName).GetName(),
      selectionOnly ?
      wxString::Format(_("Exporting selected audio as %s"), fmts[mSubFormat].description) :
   wxString::Format(_("Exporting entire file as %s"), fmts[mSubFormat].description));

   bool cancelling = false;

   while(!cancelling) {
      sampleCount pcmNumSamples = mixer->Process(pcmBufferSize);

      if (pcmNumSamples == 0)
         break;

      short *pcmBuffer = (short *)mixer->GetBuffer();

      EncodeAudioFrame(pcmBuffer,(pcmNumSamples)*sizeof(int16_t)*mChannels);

      cancelling = !progress->Update(mixer->MixGetCurrentTime()-t0, t1-t0);
   }

   delete progress;

   delete mixer;

   Finalize();

   return !cancelling;
}

void AddStringTagUTF8(char field[], int size, wxString value)
{
      memset(field,0,size);
      memcpy(field,value.ToUTF8(),strlen(value.ToUTF8()) > size -1 ? size -1 : strlen(value.ToUTF8()));
}

void AddStringTagANSI(char field[], int size, wxString value)
{
      memset(field,0,size);
      memcpy(field,value.mb_str(),strlen(value.mb_str()) > size -1 ? size -1 : strlen(value.mb_str()));
}

bool ExportFFmpeg::AddTags(Tags *tags)
{
   if (tags == NULL) return false;
   void (*AddStringTag)(char [], int, wxString);
   if (mSupportsUTF8)
      AddStringTag = AddStringTagUTF8;
   else
      AddStringTag = AddStringTagANSI;
   AddStringTag(mEncFormatCtx->author, sizeof(mEncFormatCtx->author), tags->GetTag(TAG_ARTIST));
   AddStringTag(mEncFormatCtx->album, sizeof(mEncFormatCtx->album), tags->GetTag(TAG_ALBUM));
   AddStringTag(mEncFormatCtx->comment, sizeof(mEncFormatCtx->comment), tags->GetTag(TAG_COMMENTS));
   AddStringTag(mEncFormatCtx->genre, sizeof(mEncFormatCtx->genre), tags->GetTag(TAG_GENRE));
   tags->GetTag(TAG_YEAR).ToLong((long*)&mEncFormatCtx->year);
   tags->GetTag(TAG_TRACK).ToLong((long*)&mEncFormatCtx->track);

   return true;
}

//----------------------------------------------------------------------------
// AskResample dialog
//----------------------------------------------------------------------------

int ExportFFmpeg::AskResample(int bitrate, int rate, int lowrate, int highrate, const int *sampRates)
{
   wxDialog d(NULL, wxID_ANY, wxString(_("Invalid sample rate")));
   wxChoice *choice;
   ShuttleGui S(&d, eIsCreating);
   wxString text;

   S.StartVerticalLay();
   {
      S.SetBorder(10);
      S.StartStatic(_("Resample"));
      {
         S.StartHorizontalLay(wxALIGN_CENTER, false);
         {
            if (bitrate == 0) {
               text.Printf(_("The project sample rate (%d) is not supported by the current output\nfile format.  "), rate);
            }
            else {
               text.Printf(_("The project sample rate (%d) and bit rate (%d kbps) combination is not\nsupported by the current output file format.  "), rate, bitrate/1024);
            }

            text += _("You may resample to one of the rates below.");
            S.AddTitle(text);
         }
         S.EndHorizontalLay();

         wxArrayString choices;
         wxString selected = wxT("");
         for (int i = 0; sampRates[i] > 0; i++)
         {
            int label = sampRates[i];
            if (label >= lowrate && label <= highrate)
            {
               wxString name = wxString::Format(wxT("%d"),label);
               choices.Add(name);
               if (label <= rate)
               {
                  selected = name;
               }
            }
         }

         if (selected.IsEmpty())
         {
            selected = choices[0];
         }

         S.StartHorizontalLay(wxALIGN_CENTER, false);
         {
            choice = S.AddChoice(_("Sample Rates"),
                                 selected,
                                 &choices);
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

      S.AddStandardButtons();
   }
   S.EndVerticalLay();

   d.Layout();
   d.Fit();
   d.SetMinSize(d.GetSize());
   d.Center();

   if (d.ShowModal() == wxID_CANCEL) {
      return 0;
   }

   return wxAtoi(choice->GetStringSelection());
}


bool ExportFFmpeg::DisplayOptions(AudacityProject *project, int format)
{
   if (format == FMT_PCMS16LEWAV)
   {
      ExportFFmpegWAVOptions od(project);
      od.ShowModal();
      return true;
   }
   else if (format == FMT_M4A)
   {
      ExportFFmpegAACOptions od(project);
      od.ShowModal();
      return true;
   }
   else if (format == FMT_AC3)
   {
      ExportFFmpegAC3Options od(project);
      od.ShowModal();
      return true;
   }
   else if ((format == FMT_GSMAIFF) || (format == FMT_GSMMSWAV))
   {
      wxMessageBox(wxT("No options for this format."));
      return true;
   }
   else if (format == FMT_AMRNB)
   {
      ExportFFmpegAMRNBOptions od(project);
      od.ShowModal();
      return true;
   }
   else if (format == FMT_AMRWB)
   {
      ExportFFmpegAMRWBOptions od(project);
      od.ShowModal();
      return true;
   }
   else if (format == FMT_WMA2)
   {
      ExportFFmpegWMAOptions od(project);
      od.ShowModal();
      return true;
   }
   else if (format == FMT_OTHER)
   {
      ExportFFmpegOptions od(project);
      od.ShowModal();
      return true;
   }

   return false;
}

//----------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------
ExportPlugin *New_ExportFFmpeg()
{
   return new ExportFFmpeg();
}

#endif
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
