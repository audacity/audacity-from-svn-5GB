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

\class ExportFFmpegMP3Options
\brief Options dialog for FFmpeg exporting of MP3 format.

*//***************************************************************//**

\class ExportFFmpegFLACOptions
\brief Options dialog for FFmpeg exporting of FLAC format.

*//***************************************************************//**

\class ExportFFmpegAACOptions
\brief Options dialog for FFmpeg exporting of AAC format.

*//***************************************************************//**

\class ExportFFmpegVorbisOptions
\brief Options dialog for FFmpeg exporting of Vorbis format.

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

//This violates some of the FFmpeg team recommendations (from riff.h),
//but i don't see any other ways to deal with compatibility problems
typedef struct AVCodecTag {
   int id;
   unsigned int tag;
} AVCodecTag;

enum FFmpegExposedFormat 
{
   FMT_PCMS16LEWAV,
   FMT_MP3,
   FMT_MP4,
   FMT_M4A,
   FMT_FLAC,
   FMT_AC3,
   FMT_AAC,
   FMT_OGGVORBIS,
   FMT_OGGFLAC,
   FMT_GSMAIFF,
   FMT_GSMMSWAV,
   FMT_AMRNB,
   FMT_AMRWB,
   FMT_WMA2,
//   FMT_RA3,
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
   const wxChar *description;
   CodecID codecid;
};

ExposedFormat fmts[] = 
{
   {FMT_PCMS16LEWAV, wxT("WAV"),       wxT("wav"),    255,  true,_("WAV Files (FFmpeg)"),                      CODEC_ID_PCM_S16LE},
   {FMT_MP3,         wxT("MP3"),       wxT("mp3"),    2,    true,_("MP3 Files (FFmpeg)"),                      CODEC_ID_MP3LAME},
   {FMT_MP4,         wxT("MP4"),       wxT("mp4"),    48,   true,_("MP4 (AAC) Files (FFmpeg)"),                CODEC_ID_AAC},
   {FMT_M4A,         wxT("M4A"),       wxT("m4a"),    48,   true,_("M4A (AAC) Files (FFmpeg)"),                CODEC_ID_AAC},
   {FMT_FLAC,        wxT("FLAC"),      wxT("flac"),   8,    true,_("FLAC Files (FFmpeg)"),                     CODEC_ID_FLAC},
   {FMT_AC3,         wxT("AC3"),       wxT("ac3"),    7,    true,_("AC3 Files (FFmpeg)"),                      CODEC_ID_AC3},
   {FMT_AAC,         wxT("AAC"),       wxT("aac"),    48,   true,_("AAC Files (FFmpeg)"),                      CODEC_ID_AAC},
   {FMT_OGGVORBIS,   wxT("OGG"),       wxT("ogg"),    2,    true,_("OGG Vorbis Files (FFmpeg)"),               CODEC_ID_VORBIS},
   {FMT_OGGFLAC,     wxT("OGG"),       wxT("ogg"),    255,  true,_("OGG FLAC Files (FFmpeg)"),                 CODEC_ID_FLAC},
   {FMT_GSMAIFF,     wxT("GSMAIFF"),   wxT("aiff"),   1,    true,_("GSM-AIFF Files (FFmpeg)"),                 CODEC_ID_GSM},
   {FMT_GSMMSWAV,    wxT("GSMWAV"),    wxT("wav"),    1,    true,_("GSM-WAV (Microsoft) Files (FFmpeg)"),      CODEC_ID_GSM_MS},
   {FMT_AMRNB,       wxT("AMRNB"),     wxT("amr"),    1,    true,_("AMR (narrow band) Files (FFmpeg)"),        CODEC_ID_AMR_NB},
   {FMT_AMRWB,       wxT("AMRWB"),     wxT("amr"),    1,    true,_("AMR (wide band) Files (FFmpeg)"),          CODEC_ID_AMR_WB},
   {FMT_WMA2,        wxT("WMA"),       wxT("wma"),    2,    true,_("WMA (version 2) Files (FFmpeg)"),          CODEC_ID_WMAV2},
//   {FMT_RA3,         wxT("RA3"),       wxT("ra"),     6,    true,_("Real Audio (version 3) Files (FFmpeg)"),   CODEC_ID_AC3}, //until i figure out how to export realplayer-compatible RMs
   {FMT_OTHER,       wxT("FFMPEG"),    wxT("ffmpeg"), 255,  true,_("Other FFmpeg-Compatible Files"),           CODEC_ID_NONE}
};

//----------------------------------------------------------------------------
// ExportFFmpegOptions Class
//----------------------------------------------------------------------------

enum FFmpegExportCtrlID {
   FEFormatID = 20001,
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
   FELevelID,
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
   FEAllFormatsID,
   FEAllCodecsID
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
   //\todo { flags bitexact }
   wxSpinCtrl *mCutoffSpin;
   //\todo { debug flags and flags2 reservoir }
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
   //\todo { fflags }

   wxButton *mOk;
   wxButton *mSavePreset;
   wxButton *mLoadPreset;
   int mBitRateFromChoice;
   int mSampleRateFromChoice;

   wxArrayString *mPresets;

   void FindSelectedFormat(wxString **name, wxString **longname);
   void FindSelectedCodec(wxString **name, wxString **longname);
   void FetchFormatList();
   int FetchCompatibleFormatList(CodecID id, wxString *selfmt);
   void FetchCodecList();
   int FetchCompatibleCodecList(const AVCodecTag **tags, int id);
   void FetchPresetList();

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportFFmpegOptions, wxDialog)
EVT_BUTTON(wxID_OK,ExportFFmpegOptions::OnOK)
EVT_LISTBOX(FEFormatID,ExportFFmpegOptions::OnFormatList)
EVT_LISTBOX(FECodecID,ExportFFmpegOptions::OnCodecList)
EVT_BUTTON(FEAllFormatsID,ExportFFmpegOptions::OnAllFormats)
EVT_BUTTON(FEAllCodecsID,ExportFFmpegOptions::OnAllCodecs)
END_EVENT_TABLE()

ExportFFmpegOptions::~ExportFFmpegOptions()
{
   DropFFmpegLibs();
}

ExportFFmpegOptions::ExportFFmpegOptions(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify Other Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
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

   PopulateOrExchange(S);
}

///
///
void ExportFFmpegOptions::FetchPresetList()
{
   mPresets = new wxArrayString();
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
   S.StartMultiColumn(4, wxEXPAND);
   {
      S.SetStretchyCol(1);
      wxString defpreset = gPrefs->Read(wxT("/FileFormats/FFmpegPreset"));
      //      mPresetChoice = S.Id(FEPresetID).TieChoice(_("Preset:"), wxT("/FileFormats/FFmpegPreset"), wxEmptyString, mPresetNames, mPresetNames);
      mPresetCombo = S.Id(FEPresetID).AddCombo(_("Preset:"), defpreset, mPresets);
      mLoadPreset = S.Id(FELoadPresetID).AddButton(_("Load Preset"));
      mSavePreset = S.Id(FESavePresetID).AddButton(_("Save Preset"));
   }
   S.EndMultiColumn();
   S.StartMultiColumn(2, wxEXPAND);
   {
      S.SetStretchyCol(1);
      S.Id(FEFormatLabelID).AddVariableText(wxT("Format:"));
      mFormatName = S.Id(FEFormatNameID).AddVariableText(wxT(""));
      S.Id(FECodecLabelID).AddVariableText(wxT("Codec:"));
      mCodecName = S.Id(FECodecNameID).AddVariableText(wxT(""));
   }
   S.EndMultiColumn();
   S.AddVariableText(wxT("Not all formats and codecs are compatible. Some parameters (such as bitrate and samplerate) combinations are not compatible with some codecs too."),false);
   S.StartMultiColumn(2,wxEXPAND);
   {
      S.SetStretchyCol(1);
      S.StartMultiColumn(2,wxEXPAND);
      {
         S.AddVariableText(wxT("Format selector:"));
         S.AddVariableText(wxT("Codec selector:"));
         S.Id(FEAllFormatsID).AddButton(_("Show All Formats"));
         S.Id(FEAllCodecsID).AddButton(_("Show All Codecs"));
         mFormatList = S.Id(FEFormatID).AddListBox(&mFormatNames);
         mCodecList = S.Id(FECodecID).AddListBox(&mCodecNames);
         mFormatList->DeselectAll();
         mCodecList->DeselectAll();
      }
      S.EndMultiColumn();
      S.StartStatic(wxT("Options"),1);
      {
         S.StartMultiColumn(6,wxALIGN_LEFT);
         {
            mLogLevelChoice = S.Id(FELogLevelID).TieChoice(_("Log Level:"), wxT("/FileFormats/FFmpegLogLevel"), 10, mLogLevelNames, mLogLevelLabels);
            mLogLevelChoice->SetToolTip(wxT("Log level - how much information should be displayed.\nmin - 0\nmax - 10"));

            mLanguageText = S.Id(FELanguageID).TieTextBox(_("Language:"), wxT("/FileFormats/FFmpegLanguage"), wxEmptyString, 0);
            mLanguageText->SetToolTip(wxT("ISO 639 3-letter language code\nOptional\nempty - automatic"));

            mTag = S.Id(FETagID).TieTextBox(_("Tag:"), wxT("/FileFormats/FFmpegTag"), wxEmptyString, 0);
            mTag->SetToolTip(wxT("Codec tag (FOURCC)\nOptional\nempty - automatic"));
         }
         S.EndMultiColumn();
         S.StartMultiColumn(6);
         {
            mBitrateSpin = S.Id(FEBitrateID).TieSpinCtrl(_("Bit Rate:"), wxT("/FileFormats/FFmpegBitRate"), 0,1000000,1);
            mBitrateSpin->SetToolTip(wxT("Bit Rate (bits/second) - influences the resulting file size and quality\nSome codecs may only accept specific values (128k, 192k, 256k etc)\n0 - automatic\nRecommended - 192000"));

            mQualitySpin = S.Id(FEQualityID).TieSpinCtrl(_("Quality:"), wxT("/FileFormats/FFmpegQuality"), 0,100,1);
            mQualitySpin->SetToolTip(wxT("Overral quality, used differently by different codecs\nRequired for vorbis\n0 - automatic"));

            mSampleRateSpin = S.Id(FESampleRateID).TieSpinCtrl(_("Sample Rate:"), wxT("/FileFormats/FFmpegSampleRate"), 0,200000,0);
            mSampleRateSpin->SetToolTip(wxT("Sample rate (Hz)\n0 - don't change sample rate"));

            mCutoffSpin = S.Id(FECutoffID).TieSpinCtrl(_("Cutoff Bandwidth:"), wxT("/FileFormats/FFmpegCutOff"), 0,10000000,0);
            mCutoffSpin->SetToolTip(wxT("Audio cutoff bandwidth (Hz)\nOptional\n0 - automatic\n"));
         }
         S.EndMultiColumn();
         S.StartStatic(wxT("FLAC options"),1);
         {
            S.StartMultiColumn(6);
            {
               S.Id(FEUseLPCID).TieCheckBox(_("Use LPC"), wxT("/FileFormats/FFmpegUseLPC"), 1);
               S.AddVariableText(wxEmptyString);

               mCompressionLevelSpin = S.Id(FECompLevelID).TieSpinCtrl(_("Compression Level:"), wxT("/FileFormats/FFmpegCompLevel"), 0,10,-1);
               mCompressionLevelSpin->SetToolTip(wxT("Compression level\nRequired for FLAC\n-1 - automatic\nmin - 0 (fast encoding, large output file)\nmax - 10 (slow encoding, small output file)"));

               mFrameSizeSpin =  S.Id(FEFrameSizeID).TieSpinCtrl(_("Frame Size:"), wxT("/FileFormats/FFmpegFrameSize"), 0,65535,0);
               mFrameSizeSpin->SetToolTip(wxT("Frame size\nOptional\n0 - default\nmin - 16\nmax - 65535"));

               mLPCCoeffsPrecisionSpin = S.Id(FELPCCoeffsID).TieSpinCtrl(_("LPC coefficients precision"), wxT("/FileFormats/FFmpegLPCCoefPrec"), 0,15,0);
               mLPCCoeffsPrecisionSpin->SetToolTip(wxT("LPC coefficients precision\nOptional\n0 - default\nmin - 1\nmax - 15"));

               mMinPredictionOrderSpin = S.Id(FEMinPredID).TieSpinCtrl(_("Minimal prediction order"), wxT("/FileFormats/FFmpegMinPredOrder"), -1,32,-1);
               mMinPredictionOrderSpin->SetToolTip(wxT("Minimal prediction order\nOptional\n-1 - default\nmin - 0\nmax - 32 (with LPC) or 4 (without LPC)"));

               mMaxPredictionOrderSpin = S.Id(FEMaxPredID).TieSpinCtrl(_("Maximal prediction order"), wxT("/FileFormats/FFmpegMaxPredOrder"), -1,32,-1);
               mMaxPredictionOrderSpin->SetToolTip(wxT("Maximal prediction order\nOptional\n-1 - default\nmin - 0\nmax - 32 (with LPC) or 4 (without LPC)"));

               mPredictionOrderMethodChoice = S.Id(FEPredOrderID).TieChoice(_("Prediction Order Method:"), wxT("/FileFormats/FFmpegPredOrderMethod"), 0, mPredictionOrderMethodNames, mPredictionOrderMethodLabels);

               mMinPartitionOrderSpin = S.Id(FEMinPartOrderID).TieSpinCtrl(_("Minimal partition order"), wxT("/FileFormats/FFmpegMinPartOrder"), -1,8,-1);
               mMinPartitionOrderSpin->SetToolTip(wxT("Minimal partition order\nOptional\n-1 - default\nmin - 0\nmax - 8"));

               mMaxPartitionOrderSpin = S.Id(FEMaxPartOrderID).TieSpinCtrl(_("Maximal partition order"), wxT("/FileFormats/FFmpegMaxPredOrder"), -1,8,-1);
               mMaxPartitionOrderSpin->SetToolTip(wxT("Maximal partition order\nOptional\n-1 - default\nmin - 0\nmax - 8"));
            }
            S.EndMultiColumn();
         }
         S.EndStatic();
         S.StartStatic(wxT("MPEG container options"),0);
         {
            S.StartMultiColumn(4);
            {
               mMuxRate = S.Id(FEMuxRateID).TieSpinCtrl(_("Mux Rate:"), wxT("/FileFormats/FFmpegMuxRate"), 0,10000000,0);
               mMuxRate->SetToolTip(wxT("Maximum bit rate of the multiplexed stream\nOptional\n0 - default"));

               mPacketSize = S.Id(FEPacketSizeID).TieSpinCtrl(_("Packet Size:"), wxT("/FileFormats/FFmpegPacketSize"), 0,10000000,0);
               mPacketSize->SetToolTip(wxT("Packet size\nOptional\n0 - default"));
            }
            S.EndMultiColumn();
         }
         S.EndStatic();
      }
      S.EndStatic();
   }
   S.EndMultiColumn();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

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

int ExportFFmpegOptions::FetchCompatibleFormatList(CodecID id, wxString *selfmt)
{
   int index = -1;
   mShownFormatNames.Clear();
   mShownFormatLongNames.Clear();
   mFormatList->Clear();
   AVOutputFormat *ofmt = NULL;
   ofmt = FFmpegLibsInst->av_oformat_next(ofmt);
   while (ofmt)
   {
      if (ofmt->audio_codec != CODEC_ID_NONE)
      {
         if (ofmt->audio_codec == id)
         {
            if ((selfmt != NULL) && (selfmt->Cmp(wxString::FromUTF8(ofmt->name)) == 0)) index = mShownFormatNames.GetCount();
            mShownFormatNames.Add(wxString::FromUTF8(ofmt->name));
            mShownFormatLongNames.Add(wxString::Format(wxT("%s - %s"),mShownFormatNames.Last().c_str(),wxString::FromUTF8(ofmt->long_name).c_str()));
         }
         else
         {
            const AVCodecTag **tags = ofmt->codec_tag;
            for (int i = 0; (tags != NULL) && (tags[i] != NULL); i++)
            {
               for (int j = 0; tags[i][j].id != NULL; j++)
               {
                  if (tags[i][j].id == id)
                  {
                     if ((selfmt != NULL) && (selfmt->Cmp(wxString::FromUTF8(ofmt->name)) == 0)) index = mShownFormatNames.GetCount();
                     mShownFormatNames.Add(wxString::FromUTF8(ofmt->name));
                     mShownFormatLongNames.Add(wxString::Format(wxT("%s - %s"),mShownFormatNames.Last().c_str(),wxString::FromUTF8(ofmt->long_name).c_str()));
                     break;
                  }
               }
            }
         }
      }
      ofmt = FFmpegLibsInst->av_oformat_next(ofmt);
   }
   mFormatList->Append(mShownFormatNames);
   return index;
}

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
int ExportFFmpegOptions::FetchCompatibleCodecList(const AVCodecTag **tags, int  id)
{
   int index = -1;
   mShownCodecNames.Clear();
   mShownCodecLongNames.Clear();
   mCodecList->Clear();
   for (int i = 0; (tags != NULL) && (tags[i] != NULL); i++)
   {
      for (int j = 0; tags[i][j].id != NULL; j++)
      {
         AVCodec *codec = FFmpegLibsInst->avcodec_find_encoder((CodecID)tags[i][j].id);
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

   AVOutputFormat *fmt = FFmpegLibsInst->guess_format(selfmt->mb_str(),NULL,NULL);
   if (fmt == NULL)
   {
      //This shouldn't really happen
      mFormatName->SetLabel(wxString(wxT("Failed to guess format")));
      return;
   }
   mFormatName->SetLabel(wxString::Format(wxT("%s"),selfmtlong->c_str()));
   const AVCodecTag **tags = fmt->codec_tag;
   int selcdcid = -1;

   if (tags != NULL)
   {
      if (selcdc != NULL)
      {
         AVCodec *cdc = FFmpegLibsInst->avcodec_find_encoder_by_name(selcdc->mb_str());
         if (cdc != NULL)
         {
            int i = 0;
            for(int i = 0;(tags != NULL) && (tags[i] != NULL); i++)
            {
               for (int j = 0; tags[i][j].id != NULL; j++)
               {
                  if (cdc->id == tags[i][j].id)
                     selcdcid = cdc->id;
               }
            }
         }
      }
      int newselcdc = FetchCompatibleCodecList(tags, selcdcid);
      if (newselcdc > 0) mCodecList->Select(newselcdc);
   }
   else
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
   }
   int handled = -1;
   AVCodec *cdc = NULL;
   if (selcdc != NULL)
      cdc = FFmpegLibsInst->avcodec_find_encoder_by_name(selcdc->mb_str());
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

   AVCodec *cdc = FFmpegLibsInst->avcodec_find_encoder_by_name(selcdc->mb_str());
   if (cdc == NULL)
   {
      //This shouldn't really happen
      mCodecName->SetLabel(wxString(wxT("Failed to find the codec")));
      return;
   }
   mCodecName->SetLabel(wxString::Format(wxT("[%d] %s"),cdc->id,selcdclong->c_str()));

   if (selfmt != NULL)
   {
      AVOutputFormat *fmt = FFmpegLibsInst->guess_format(selfmt->mb_str(),NULL,NULL);
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
   if (selcdc > -1) gPrefs->Write(wxT("/FileFormats/FFmpegCodec"),mCodecList->GetString(selcdc));
   if (selfmt > -1) gPrefs->Write(wxT("/FileFormats/FFmpegFormat"),mFormatList->GetString(selfmt));
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}

//----------------------------------------------------------------------------
// ExportFFmpegAC3Options Class
//----------------------------------------------------------------------------

static int iAC3BitRates[] = { 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 320000, 384000, 448000, 512000, 576000, 640000 };
static int iAC3SampleRates[] = { 48000, 44100, 32000 };


class ExportFFmpegAC3Options : public wxDialog
{
public:

   ExportFFmpegAC3Options(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

private:

   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;
   wxArrayString mSampleRateNames;
   wxArrayInt    mSampleRateLabels;

   wxChoice *mBitRateChoice;
   wxChoice *mSampleRateChoice;
   wxButton *mOk;
   int mBitRateFromChoice;
   int mSampleRateFromChoice;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportFFmpegAC3Options, wxDialog)
   EVT_BUTTON(wxID_OK,ExportFFmpegAC3Options::OnOK)
END_EVENT_TABLE()

ExportFFmpegAC3Options::ExportFFmpegAC3Options(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify AC3 Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   for (unsigned int i=0; i < (sizeof(iAC3BitRates)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(wxT("%i"),iAC3BitRates[i]/1000));
      mBitRateLabels.Add(iAC3BitRates[i]);
   }

   for (unsigned int i=0; i < (sizeof(iAC3SampleRates)/sizeof(int)); i++)
   {
      mSampleRateNames.Add(wxString::Format(wxT("%i"),iAC3SampleRates[i]));
      mSampleRateLabels.Add(iAC3SampleRates[i]);
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
            S.TieChoice(_("Sample Rate:"), wxT("/FileFormats/AC3SampleRate"), 
               48000, mSampleRateNames, mSampleRateLabels);
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
            wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
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
// ExportFFmpegMP3Options Class
//----------------------------------------------------------------------------

static int iMP3BitRates[] = { 32000, 40000, 48000, 56000, 64000, 80000, 96000,112000,128000,160000,192000,224000,256000,320000 };
static int iMP3SampleRates[] = { 48000, 44100, 32000 };


class ExportFFmpegMP3Options : public wxDialog
{
public:

   ExportFFmpegMP3Options(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

private:

   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;
   wxArrayString mSampleRateNames;
   wxArrayInt    mSampleRateLabels;

   wxChoice *mBitRateChoice;
   wxChoice *mSampleRateChoice;
   wxButton *mOk;
   int mBitRateFromChoice;
   int mSampleRateFromChoice;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportFFmpegMP3Options, wxDialog)
EVT_BUTTON(wxID_OK,ExportFFmpegMP3Options::OnOK)
END_EVENT_TABLE()

ExportFFmpegMP3Options::ExportFFmpegMP3Options(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify MP3 Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   for (unsigned int i=0; i < (sizeof(iMP3BitRates)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(wxT("%i"),iMP3BitRates[i]/1000));
      mBitRateLabels.Add(iMP3BitRates[i]);
   }

   for (unsigned int i=0; i < (sizeof(iMP3SampleRates)/sizeof(int)); i++)
   {
      mSampleRateNames.Add(wxString::Format(wxT("%i"),iMP3SampleRates[i]));
      mSampleRateLabels.Add(iMP3SampleRates[i]);
   }


   PopulateOrExchange(S);
}

/// 
/// 
void ExportFFmpegMP3Options::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.StartStatic(_("MP3 Export Setup"), 0);
      {
         S.StartTwoColumn();
         {
            S.TieChoice(_("Bit Rate:"), wxT("/FileFormats/MP3BitRate"), 
               160000, mBitRateNames, mBitRateLabels);
            S.TieChoice(_("Sample Rate:"), wxT("/FileFormats/MP3SampleRate"), 
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
void ExportFFmpegMP3Options::OnOK(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}

//----------------------------------------------------------------------------
// ExportFFmpegFLACOptions Class
//----------------------------------------------------------------------------

static int iFLACCompression[] = { 0,1,2,3,4,5,6,7,8 };

class ExportFFmpegFLACOptions : public wxDialog
{
public:

   ExportFFmpegFLACOptions(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

private:

   wxArrayString mCompressionNames;
   wxArrayInt    mCompressionLabels;

   wxChoice *mCompressionChoice;
   wxButton *mOk;
   int mCompressionFromChoice;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportFFmpegFLACOptions, wxDialog)
EVT_BUTTON(wxID_OK,ExportFFmpegFLACOptions::OnOK)
END_EVENT_TABLE()

ExportFFmpegFLACOptions::ExportFFmpegFLACOptions(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify FLAC Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   for (unsigned int i=0; i < (sizeof(iFLACCompression)/sizeof(int)); i++)
   {
      mCompressionNames.Add(wxString::Format(wxT("%i"),iFLACCompression[i]));
      mCompressionLabels.Add(iFLACCompression[i]);
   }

   PopulateOrExchange(S);
}

/// 
/// 
void ExportFFmpegFLACOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.StartStatic(_("FLAC Export Setup"), 0);
      {
         S.StartTwoColumn();
         {
            S.TieChoice(_("Compression Rate:"), wxT("/FileFormats/FLACCompression"), 
               5, mCompressionNames, mCompressionLabels);
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
void ExportFFmpegFLACOptions::OnOK(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}

//----------------------------------------------------------------------------
// ExportFFmpegAACOptions Class
//----------------------------------------------------------------------------

static int iAACBitRates[16];
static int iAACSampleRates[] = { 96000,88200,64000,48000,44100,32000,24000,22050,16000,12000,11025,8000,7350 };
static int iAACProfileValues[] = { FF_PROFILE_AAC_LOW, FF_PROFILE_AAC_MAIN, /*FF_PROFILE_AAC_SSR,*/ FF_PROFILE_AAC_LTP };
static const wxChar *iAACProfileNames[] = { _("Low Complexity"), _("Main profile"), /*_("SSR"),*/ _("LTP") }; //SSR is not supported

enum AACIDs
{
   AACSRChoice = 20000,
   AACBRChoice
};
/* Returns the maximum bitrate per channel for that sampling frequency */
#define FRAME_LEN 1024
unsigned int MaxAACBitrate(unsigned long sampleRate)
{
   /*
   *  Maximum of 6144 bit for a channel
   */
   return (unsigned int)(6144.0 * (double)sampleRate/(double)FRAME_LEN + .5);
}


class ExportFFmpegAACOptions : public wxDialog
{
public:

   ExportFFmpegAACOptions(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);
   void OnSampleRate(wxCommandEvent &event);

private:

   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;
   wxArrayString mSampleRateNames;
   wxArrayInt    mSampleRateLabels;
   wxArrayString mProfileNames;
   wxArrayInt    mProfileLabels;

   wxChoice *mBitRateChoice;
   wxChoice *mSampleRateChoice;
   wxChoice *mProfileChoice;
   wxButton *mOk;
   int mBitRateFromChoice;
   int mSampleRateFromChoice;
   int mProfileFromChoice;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportFFmpegAACOptions, wxDialog)
EVT_BUTTON(wxID_OK,ExportFFmpegAACOptions::OnOK)
EVT_CHOICE(AACSRChoice,ExportFFmpegAACOptions::OnSampleRate)
END_EVENT_TABLE()

ExportFFmpegAACOptions::ExportFFmpegAACOptions(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify AAC Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   long selsr = gPrefs->Read(wxT("/FileFormats/AACSampleRate"),44100);
   unsigned int mbr = MaxAACBitrate(selsr/2);

   for (unsigned int i=0; i < (sizeof(iAACBitRates)/sizeof(int)); i++)
   {
      iAACBitRates[i] = (i+1)*mbr/((sizeof(iAACBitRates)/sizeof(int)));
   }

   for (unsigned int i=0; i < (sizeof(iAACBitRates)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(wxT("%i"),iAACBitRates[i]/1024));
      mBitRateLabels.Add(iAACBitRates[i]);
   }

   for (unsigned int i=0; i < (sizeof(iAACSampleRates)/sizeof(int)); i++)
   {
      mSampleRateNames.Add(wxString::Format(wxT("%i"),iAACSampleRates[i]));
      mSampleRateLabels.Add(iAACSampleRates[i]);
   }

   for (unsigned int i=0; i < (sizeof(iAACProfileValues)/sizeof(int)); i++)
   {
      mProfileNames.Add(wxString::Format(wxT("%s"),iAACProfileNames[i]));
      mProfileLabels.Add(iAACProfileValues[i]);
   }


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
            S.Id(AACBRChoice).TieChoice(_("Bit Rate (per channel):"), wxT("/FileFormats/AACBitRate"), 
               98000, mBitRateNames, mBitRateLabels);
            S.Id(AACSRChoice).TieChoice(_("Sample Rate:"), wxT("/FileFormats/AACSampleRate"), 
               44100, mSampleRateNames, mSampleRateLabels);
            S.TieChoice(_("Profile:"), wxT("/FileFormats/AACProfile"), 
               FF_PROFILE_AAC_LOW, mProfileNames, mProfileLabels);
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
void ExportFFmpegAACOptions::OnSampleRate(wxCommandEvent &event)
{
   wxChoice *srchoice = (wxChoice*)this->FindWindowById(AACSRChoice);
   long selsr;
   srchoice->GetString(srchoice->GetSelection()).ToLong(&selsr);
   wxChoice *choice = (wxChoice*)this->FindWindowById(AACBRChoice);
   long selbr;
   selbr = choice->GetSelection();
   if (selbr >= 0)
      selbr = iAACBitRates[selbr];
   unsigned int mbr = MaxAACBitrate(selsr/2);
   for (unsigned int i=0; i < (sizeof(iAACBitRates)/sizeof(int)); i++)
   {
      iAACBitRates[i] = (i+1)*mbr/((sizeof(iAACBitRates)/sizeof(int)));
   }
   mBitRateNames.Clear();
   mBitRateLabels.Clear();
   for (unsigned int i=0; i < (sizeof(iAACBitRates)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(wxT("%i"),iAACBitRates[i]/1024));
      mBitRateLabels.Add(iAACBitRates[i]);
   }
   long mindiff = 1024*1024;
   long minindex = -1;
   for (unsigned int i=0; i < (sizeof(iAACBitRates)/sizeof(int)); i++)
   {
      choice->SetString(i,mBitRateNames[i]);
      //Retain previous choice if possible
      if (abs(iAACBitRates[i] - selbr) < mindiff)
      {
         mindiff = abs(iAACBitRates[i] - selbr);
         minindex = i;
      }
   }
   if (minindex != -1)
      choice->SetSelection(minindex);
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
// ExportFFmpegVorbisOptions Class
//----------------------------------------------------------------------------

static int iVorbisQuality[] = { -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

class ExportFFmpegVorbisOptions : public wxDialog
{
public:

   ExportFFmpegVorbisOptions(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

private:

   wxArrayString mQualityNames;
   wxArrayInt    mQualityLabels;

   wxChoice *mQualityChoice;
   wxButton *mOk;
   int mQualityFromChoice;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportFFmpegVorbisOptions, wxDialog)
EVT_BUTTON(wxID_OK,ExportFFmpegVorbisOptions::OnOK)
END_EVENT_TABLE()

ExportFFmpegVorbisOptions::ExportFFmpegVorbisOptions(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify Vorbis Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   for (unsigned int i=0; i < (sizeof(iVorbisQuality)/sizeof(int)); i++)
   {
      mQualityNames.Add(wxString::Format(wxT("%i"),iVorbisQuality[i]));
      mQualityLabels.Add(iVorbisQuality[i]);
   }

   PopulateOrExchange(S);
}

/// 
/// 
void ExportFFmpegVorbisOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.StartStatic(_("Vorbis Export Setup"), 0);
      {
         S.StartTwoColumn();
         {
            S.TieChoice(_("Quality:"), wxT("/FileFormats/VorbisQuality"), 
               5, mQualityNames, mQualityLabels);
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
void ExportFFmpegVorbisOptions::OnOK(wxCommandEvent& event)
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
            wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
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
            wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
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

static int iWMABitRate[] = { 24000, 32000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 320000, 384000 };

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
            wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
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
               192000, mBitRateNames, mBitRateLabels);
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
};

ExportFFmpeg::ExportFFmpeg()
:  ExportPlugin()
{
   int newfmt;

   for (newfmt = 0; newfmt < FMT_LAST; newfmt++)
   {
      AddFormat();
      SetFormat(fmts[newfmt].name,newfmt);
      AddExtension(fmts[newfmt].extension,newfmt);
      switch(newfmt)
      {
      case FMT_MP4:
         AddExtension(wxString(wxT("mov")),newfmt);
         AddExtension(wxString(wxT("3gp")),newfmt);
         AddExtension(wxString(wxT("m4a")),newfmt);
         break;
      case FMT_WMA2:
         AddExtension(wxString(wxT("asf")),newfmt);
         AddExtension(wxString(wxT("wmv")),newfmt);
         break;
      default:
         break;
      }
      SetMaxChannels(fmts[newfmt].maxchannels,newfmt);
      SetCanMetaData(fmts[newfmt].canmetadata,newfmt);
      SetDescription(fmts[newfmt].description,newfmt);
   }

   PickFFmpegLibs();

   mEncFormatCtx = NULL;			// libavformat's context for our output file
   mEncFormatDesc = NULL;			// describes our output file to libavformat
   mEncAudioStream = NULL;			// the output audio stream (may remain NULL)
   mEncAudioCodecCtx = NULL;		// the encoder for the output audio stream
   mEncAudioEncodedBuf = NULL;		// buffer to hold frames encoded by the encoder
   #define MAX_AUDIO_PACKET_SIZE (128 * 1024)
   mEncAudioEncodedBufSiz = 4*MAX_AUDIO_PACKET_SIZE;
   mEncAudioFifoOutBuf = NULL;		// buffer to read _out_ of the FIFO into
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

bool ExportFFmpeg::InitCodecs(AudacityProject *project)
{
   AVCodec *	codec = NULL;

   // Configure the audio stream's codec context.
   mEncAudioCodecCtx = mEncAudioStream->codec;
  
   FFmpegLibsInst->avcodec_get_context_defaults(mEncAudioCodecCtx);

   mEncAudioCodecCtx->codec_id = fmts[mSubFormat].codecid;
   mEncAudioCodecCtx->codec_type = CODEC_TYPE_AUDIO;
   mEncAudioCodecCtx->codec_tag = FFmpegLibsInst->av_codec_get_tag(mEncFormatCtx->oformat->codec_tag,mEncAudioCodecCtx->codec_id);

   switch (mSubFormat)
   {
   case FMT_PCMS16LEWAV:
      mSampleRate = project->GetRate();
      break;
   case FMT_MP3:
      mSampleRate = gPrefs->Read(wxT("/FileFormats/MP3SampleRate"), 48000);
      mEncAudioCodecCtx->bit_rate = gPrefs->Read(wxT("/FileFormats/MP3BitRate"), 192000);
      break;
   case FMT_MP4:
   case FMT_M4A:
   case FMT_AAC:
      mSampleRate = gPrefs->Read(wxT("/FileFormats/AACSampleRate"), 48000);
      mEncAudioCodecCtx->bit_rate = gPrefs->Read(wxT("/FileFormats/AACBitRate"), 98000);
      mEncAudioCodecCtx->bit_rate *= mChannels;
      mEncAudioCodecCtx->profile = gPrefs->Read(wxT("/FileFormats/AACProfile"),FF_PROFILE_AAC_LOW);
      mEncAudioCodecCtx->cutoff = mSampleRate/2;
      break;
   case FMT_FLAC:
   case FMT_OGGFLAC:
      mSampleRate = project->GetRate();
      mEncAudioCodecCtx->compression_level = gPrefs->Read(wxT("/FileFormats/FLACCompression"), 5);
      break;
   case FMT_AC3:
//   case FMT_RA3:
      mSampleRate = gPrefs->Read(wxT("/FileFormats/AC3SampleRate"), 48000);
      mEncAudioCodecCtx->bit_rate = gPrefs->Read(wxT("/FileFormats/AC3BitRate"), 192000);
      break;
   case FMT_OGGVORBIS:
      mSampleRate = project->GetRate();
      mEncAudioCodecCtx->global_quality = gPrefs->Read(wxT("/FileFormats/VorbisQuality"), 6);
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
      mSampleRate = gPrefs->Read(wxT("/FileFormats/WMASampleRate"), 48000);
      mEncAudioCodecCtx->bit_rate = gPrefs->Read(wxT("/FileFormats/WMABitRate"), 198000);
      break;
   case FMT_OTHER:
      memcpy(mEncAudioStream->language,gPrefs->Read(wxT("/FileFormats/FFmpegLanguage"),wxT("")).c_str(),4);
      mSampleRate = gPrefs->Read(wxT("/FileFormats/FFmpegSampleRate"),(long)0);
      mEncAudioCodecCtx->bit_rate = gPrefs->Read(wxT("/FileFormats/FFmpegBitRate"), (long)0);
      memcpy(&mEncAudioCodecCtx->codec_tag,gPrefs->Read(wxT("/FileFormats/FFmpegTag"),wxT("")).c_str(),4);
      mEncAudioCodecCtx->global_quality = gPrefs->Read(wxT("/FileFormats/FFmpegQuality"),(long)0);
      mEncAudioCodecCtx->cutoff = gPrefs->Read(wxT("/FileFormats/FFmpegCutOff"),(long)0);
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

   if (mSubFormat > 14) return false;
   
   wxString ext(fmts[mSubFormat].extension);
   if (mSubFormat == FMT_OTHER)
      ext = gPrefs->Read(wxT("/FileFormats/FFmpegFormat"),wxT("mkv"));
   ret = Init(ext.ToAscii(),project);

   if (!ret) return false;

   if (metadata == NULL) metadata = project->GetTags();

   AddTags(metadata);

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

bool ExportFFmpeg::AddTags(Tags *tags)
{
   if (tags == NULL) return false;
   {
      memset(mEncFormatCtx->author,0,sizeof(mEncFormatCtx->author));
      wxString tmp(tags->GetTag(TAG_ARTIST));
      const wxCharBuffer cstr = tmp.ToUTF8().data();
      strlen(cstr.data()) > sizeof(mEncFormatCtx->author) -1 ? sizeof(mEncFormatCtx->author) -1 : strlen(cstr.data());
   }

   {
      memset(mEncFormatCtx->album,0,sizeof(mEncFormatCtx->album));
      wxString tmp(tags->GetTag(TAG_ALBUM));
      const wxCharBuffer cstr = tmp.ToUTF8().data();
      strlen(cstr.data()) > sizeof(mEncFormatCtx->album) -1 ? sizeof(mEncFormatCtx->album) -1 : strlen(cstr.data());
   }

   {
      memset(mEncFormatCtx->comment,0,sizeof(mEncFormatCtx->comment));
      wxString tmp(tags->GetTag(TAG_COMMENTS));
      const wxCharBuffer cstr = tmp.ToAscii();
      strlen(cstr.data()) > sizeof(mEncFormatCtx->comment) -1 ? sizeof(mEncFormatCtx->comment) -1 : strlen(cstr.data());
   }

   {
      memset(mEncFormatCtx->genre,0,sizeof(mEncFormatCtx->genre));
      wxString tmp(tags->GetTag(TAG_GENRE));
      const wxCharBuffer cstr = tmp.ToAscii();
      strlen(cstr.data()) > sizeof(mEncFormatCtx->genre) -1 ? sizeof(mEncFormatCtx->genre) -1 : strlen(cstr.data());
   }

   tags->GetTag(TAG_YEAR).ToLong((long*)&mEncFormatCtx->year);
   tags->GetTag(TAG_TRACK).ToLong((long*)&mEncFormatCtx->track);

   return true;
}

bool ExportFFmpeg::DisplayOptions(AudacityProject *project, int format)
{
   if (format == FMT_PCMS16LEWAV)
   {
      ExportFFmpegWAVOptions od(project);
      od.ShowModal();
      return true;
   }
   else if (format == FMT_MP3)
   {
      ExportFFmpegMP3Options od(project);
      od.ShowModal();
      return true;
   }
   else if ((format == FMT_MP4) || (format == FMT_AAC) || (format == FMT_M4A))
   {
      ExportFFmpegAACOptions od(project);
      od.ShowModal();
      return true;
   }
   else if ((format == FMT_FLAC) || (format == FMT_OGGFLAC))
   {
      ExportFFmpegFLACOptions od(project);
      od.ShowModal();
      return true;
   }
   else if ((format == FMT_AC3)/* || (format == FMT_RA3)*/)
   {
      ExportFFmpegAC3Options od(project);
      od.ShowModal();
      return true;
   }
   else if (format == FMT_OGGVORBIS)
   {
      ExportFFmpegVorbisOptions od(project);
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
