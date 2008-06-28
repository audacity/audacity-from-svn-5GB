/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportFFmpeg.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

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
#include <wx/window.h>


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

#if defined (USE_FFMPEG)

extern FFmpegLibs *FFmpegLibsInst;

enum FFmpegExposedFormat 
{
  FMT_PCMS16LEWAV,
  FMT_MP3,
  FMT_MP4,
  FMT_FLAC,
  FMT_AC3,
  FMT_AAC,
  FMT_OGGVORBIS,
  FMT_OGGFLAC,
  FMT_GSMWAV,
  FMT_GSMMSWAV,
  FMT_AMRNB,
  FMT_AMRWB,
  FMT_WMA2,
  FMT_RA3,
  FMT_RA9,
  FMT_OTHER
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
   {FMT_PCMS16LEWAV, wxT("WAV"), wxT("wav"), 255, true,_("WAV Files (FFmpeg)"), CODEC_ID_PCM_S16LE},
   {FMT_MP3, wxT("MP3"), wxT("mp3"), 2, true,_("MP3 Files (FFmpeg)"), CODEC_ID_MP3LAME},
   {FMT_MP4, wxT("MP4"), wxT("mp4"), 48, true,_("MP4 (AAC) Files (FFmpeg)"), CODEC_ID_AAC},
   {FMT_FLAC, wxT("FLAC"), wxT("flac"), 8, true,_("FLAC Files (FFmpeg)"), CODEC_ID_FLAC},
   {FMT_AC3, wxT("AC3"), wxT("ac3"), 7, true,_("AC3 Files (FFmpeg)"), CODEC_ID_AC3},
   {FMT_AAC, wxT("AAC"), wxT("aac"), 48, true,_("AAC Files (FFmpeg)"), CODEC_ID_AAC},
   {FMT_OGGVORBIS, wxT("OGG"), wxT("ogg"), 2, true,_("OGG Vorbis Files (FFmpeg)"), CODEC_ID_VORBIS},
   {FMT_OGGFLAC, wxT("OGG"), wxT("ogg"), 255, true,_("OGG FLAC Files (FFmpeg)"), CODEC_ID_FLAC},
   {FMT_GSMWAV, wxT("WAV"), wxT("wav"), 1, true,_("GSM-WAV Files (FFmpeg)"), CODEC_ID_GSM}, //native GSM container exists, but muxer is not available (demuxing only)
   {FMT_GSMMSWAV, wxT("WAV"), wxT("wav"), 1, true,_("GSM-WAV (Microsoft) Files (FFmpeg)"), CODEC_ID_GSM_MS},
   {FMT_AMRNB, wxT("AMR"), wxT("amr"), 1, true,_("AMR (narrow band) Files (FFmpeg)"), CODEC_ID_AMR_NB},
   {FMT_AMRWB, wxT("AMR"), wxT("amr"), 1, true,_("AMR (wide band) Files (FFmpeg)"), CODEC_ID_AMR_WB},
   {FMT_WMA2, wxT("WMA"), wxT("wma"), 2, true,_("WMA (version 2) Files (FFmpeg)"), CODEC_ID_WMAV2},
   {FMT_RA3, wxT("RA"), wxT("ra"), 6, true,_("Real Audio (version 3) Files (FFmpeg)"), CODEC_ID_AC3},
   {FMT_RA9, wxT("RA"), wxT("ra"), 48, true,_("Real Audio (version 9) Files (FFmpeg)"), CODEC_ID_AAC},
   {FMT_OTHER, wxT("FFMPEG"), wxT("ffmpeg"), 255, true,_("Other FFmpeg-Compatible Files"), CODEC_ID_NONE}
};
/*
//----------------------------------------------------------------------------
// ExportFFmpegOptions Class
//----------------------------------------------------------------------------

class ExportFFmpegOptions : public wxDialog
{
public:

   ExportFFmpegOptions(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

private:

   wxArrayString mPresetNames;
   wxArrayString mFormatNames;
   wxArrayString mLogLevelNames;
   wxArrayInt    mLogLevelLabels;
   wxArrayString mCodecNames;
   wxArrayString mProfileNames;
   wxArrayInt    mProfileLabels;
   wxArrayString mPredictionOrderMethodNames;;
   wxArrayInt    mPredictionOrderMethodLabels;

   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;
   wxArrayString mSampleRateNames;
   wxArrayInt    mSampleRateLabels;

   wxChoice *mFormatChoice;
   wxChoice *mLogLevel;
   wxSpinCtrl *mBitrateSpin;
   wxSpinCtrl *mQualitySpin;
   wxSpinCtrl *mSampleRateSpin;
   wxChoice *mCodecChoice;
   wxTextCtrl *mLanguageText;
   wxTextCtrl *mTag;
   //\todo { flags bitexact }
   wxSpinCtrl *mCutoffSpin;
   wxSpinCtrl *mFrameSizeSpin;
   wxSpinCtrl *mBufSize;
   //\todo { debug flags and flags2 reservoir }
   wxChoice *mProfileChoice;
   wxSpinCtrl *mLevel;
   wxSpinCtrl *mCompressionLevelSpin;
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

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportFFmpegOptions, wxDialog)
EVT_BUTTON(wxID_OK,ExportFFmpegOptions::OnOK)
END_EVENT_TABLE()

ExportFFmpegOptions::ExportFFmpegOptions(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify Other Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   for (unsigned int i=0; i < (sizeof(iOtherBitRates)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(wxT("%i"),iOtherBitRates[i]/1000));
      mBitRateLabels.Add(iOtherBitRates[i]);
   }

   for (unsigned int i=0; i < (sizeof(iOtherSampleRates)/sizeof(int)); i++)
   {
      mSampleRateNames.Add(wxString::Format(wxT("%i"),iOtherSampleRates[i]));
      mSampleRateLabels.Add(iOtherSampleRates[i]);
   }


   PopulateOrExchange(S);
}

/// 
/// 
void ExportFFmpegOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.StartStatic(_("Other Export Setup"), 0);
      {
         S.StartTwoColumn();
         {
            S.TieChoice(_("Bit Rate:"), wxT("/FileFormats/OtherBitRate"), 
               160000, mBitRateNames, mBitRateLabels);
            S.TieChoice(_("Sample Rate:"), wxT("/FileFormats/OtherSampleRate"), 
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
void ExportFFmpegOptions::OnOK(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}*/


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

static int iAACBitRates[] = { 8*1024, 16*1024, 24*1024, 32*1024, 48*1024, 64*1024, 96*1024, 128*1024, 160*1024, 192*1024, 224*1024 };
static int iAACSampleRates[] = { 96000,88200,64000,48000,44100,32000,24000,22050,16000,12000,11025,8000,7350 };
static int iAACProfileValues[] = { FF_PROFILE_AAC_LOW, FF_PROFILE_AAC_MAIN, FF_PROFILE_AAC_SSR, FF_PROFILE_AAC_LTP };
static const wxChar *iAACProfileNames[] = { _("Low Complexity"), _("Main profile"), _("SSR"), _("LTP") };


class ExportFFmpegAACOptions : public wxDialog
{
public:

   ExportFFmpegAACOptions(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

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
END_EVENT_TABLE()

ExportFFmpegAACOptions::ExportFFmpegAACOptions(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify AAC Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   for (unsigned int i=0; i < (sizeof(iAACBitRates)/sizeof(int)); i++)
   {
      mBitRateNames.Add(wxString::Format(wxT("%i"),iAACBitRates[i]/1000));
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
            S.TieChoice(_("Bit Rate (per channel):"), wxT("/FileFormats/AACBitRate"), 
               98000, mBitRateNames, mBitRateLabels);
            S.TieChoice(_("Sample Rate:"), wxT("/FileFormats/AACSampleRate"), 
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

   for (newfmt = 0; newfmt < 15; newfmt++)
   {
      AddFormat();
      SetFormat(fmts[newfmt].name,newfmt);
      SetExtension(fmts[newfmt].extension,newfmt);
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
   mEncAudioEncodedBufSiz = AVCODEC_MAX_AUDIO_FRAME_SIZE;
   mEncAudioFifoOutBuf = NULL;		// buffer to read _out_ of the FIFO into
}

void ExportFFmpeg::Destroy()
{
   DropFFmpegLibs();
   delete this;
}

bool ExportFFmpeg::Init(const char *shortname,AudacityProject *project)
{
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
      if (FFmpegLibsInst->url_fopen(&mEncFormatCtx->pb, mEncFormatCtx->filename, URL_WRONLY) < 0)
      {
         wxLogMessage(wxT("FFmpeg : ERROR - Can't open output file \"%s\" to write."), mName.c_str());
         return false;
      }
   }

   // Set default parameters on the format context.
   memset(&fpOutFile, 0, sizeof(AVFormatParameters));
   if (FFmpegLibsInst->av_set_parameters(mEncFormatCtx, &fpOutFile) < 0)
   {
      wxLogMessage(wxT("FFmpeg : ERROR - Can't set output parameters for output file \"%s\"."), mName.c_str());
      return false;
   }

   mEncFormatCtx->preload   = (int)(0.5 * AV_TIME_BASE);
   mEncFormatCtx->max_delay = (int)(0.7 * AV_TIME_BASE);

   // Open the audio stream's codec and initialise any stream related data.
   if (!InitCodecs(project))
      return false;

   // Write headers to the output file.
   if (FFmpegLibsInst->av_write_header(mEncFormatCtx) < 0)
   {
      wxLogMessage(wxT("FFmpeg : ERROR - Can't write headers to output file \"%s\"."), mName.c_str());
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
   case FMT_AAC:
   case FMT_RA9:
      mSampleRate = gPrefs->Read(wxT("/FileFormats/AACSampleRate"), 48000);
      mEncAudioCodecCtx->bit_rate = gPrefs->Read(wxT("/FileFormats/AACBitRate"), 192000);
      break;
   case FMT_FLAC:
   case FMT_OGGFLAC:
      mSampleRate = project->GetRate();
      mEncAudioCodecCtx->compression_level = gPrefs->Read(wxT("/FileFormats/FLACCompression"), 5);
      break;
   case FMT_AC3:
   case FMT_RA3:
      mSampleRate = gPrefs->Read(wxT("/FileFormats/AC3SampleRate"), 48000);
      mEncAudioCodecCtx->bit_rate = gPrefs->Read(wxT("/FileFormats/AC3BitRate"), 192000);
      break;
   case FMT_OGGVORBIS:
      mSampleRate = project->GetRate();
      mEncAudioCodecCtx->global_quality = gPrefs->Read(wxT("/FileFormats/VorbisQuality"), 6);
      break;
   case FMT_GSMWAV:
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
   default:
      return false;
   }

   mEncAudioCodecCtx->sample_rate = mSampleRate;
   mEncAudioCodecCtx->channels = mChannels;
   mEncAudioCodecCtx->time_base.num = 0;
   mEncAudioCodecCtx->time_base.den = 1;
   mEncAudioCodecCtx->sample_fmt = SAMPLE_FMT_S16;

   // Is the required audio codec compiled into libavcodec?
   if ((codec = FFmpegLibsInst->avcodec_find_encoder(mEncAudioCodecCtx->codec_id)) == NULL)
   {
      wxLogMessage(wxT("FFmpeg : ERROR - Can't find audio codec %d."),mEncAudioCodecCtx->codec_id);
      return false;
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
   FFmpegLibsInst->av_fifo_init(&mEncAudioFifo, 2 * AVCODEC_MAX_AUDIO_FRAME_SIZE);

   // Allocate a buffer to read OUT of the FIFO into. The FIFO maintains its own buffer internally.
   if ((mEncAudioFifoOutBuf = (uint8_t*)FFmpegLibsInst->av_malloc(2 * AVCODEC_MAX_AUDIO_FRAME_SIZE)) == NULL)
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

      // Flush the audio FIFO first if necessary. It won't contain a _full_ audio frame because
      // if it did we'd have pulled it from the FIFO during the last encodeAudioFrame() call - 
      // the encoder must support short/incomplete frames for this to work.
      if (nFifoBytes > 0 && mEncAudioCodecCtx->codec->capabilities & CODEC_CAP_SMALL_LAST_FRAME)
      {
         int nFrameSizeTmp = mEncAudioCodecCtx->frame_size;

         // The last frame is going to contain a smaller than usual number of samples.
         mEncAudioCodecCtx->frame_size = nFifoBytes / (mEncAudioCodecCtx->channels * sizeof(int16_t));

         wxLogMessage(wxT("FFmpeg : Audio FIFO still contains %d bytes, writing short %d sample frame ..."), 
            nFifoBytes, mEncAudioCodecCtx->frame_size);

         // Pull the bytes out from the FIFO and feed them to the encoder.
         if (FFmpegLibsInst->av_fifo_read(&mEncAudioFifo, mEncAudioFifoOutBuf, nFifoBytes) == 0)
            nEncodedBytes = FFmpegLibsInst->avcodec_encode_audio(mEncAudioCodecCtx, mEncAudioEncodedBuf, mEncAudioEncodedBufSiz, (int16_t*)mEncAudioFifoOutBuf);

         mEncAudioCodecCtx->frame_size = nFrameSizeTmp;		// restore the native frame size
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
      else if (pkt.size == 0)
      {
         wxLogMessage(wxT("FFmpeg : Audio codec buffered samples ..."));
         return true;
      }

      // Rescale from the codec time_base to the AVStream time_base.
      if (mEncAudioCodecCtx->coded_frame && mEncAudioCodecCtx->coded_frame->pts != AV_NOPTS_VALUE)
         pkt.pts = FFmpegLibsInst->av_rescale_q(mEncAudioCodecCtx->coded_frame->pts, mEncAudioCodecCtx->time_base, mEncAudioStream->time_base);

      //wxLogMessage(wxT("FFmpeg : (%d) Writing audio frame with PTS: %lld."), mEncAudioCodecCtx->frame_number, pkt.pts);

      pkt.stream_index = 0;
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
   if (channels > fmts[subformat].maxchannels) return false;
   mName = fName;
   mSubFormat = subformat;
   TrackList *tracks = project->GetTracks();
   bool ret = true;

   if (mSubFormat > 14) return false;
   
   wxString ext(fmts[mSubFormat].extension);
   ret = Init(ext.ToAscii(),project);

   if (!ret) return false;

   if (metadata == NULL) metadata = project->GetTags();

   AddTags(metadata);

   int pcmBufferSize = 9216;
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
      const char *cstr = tmp.ToUTF8().data();
      strlen(cstr) > sizeof(mEncFormatCtx->author) -1 ? sizeof(mEncFormatCtx->author) -1 : strlen(cstr);
   }

   {
      memset(mEncFormatCtx->album,0,sizeof(mEncFormatCtx->album));
      wxString tmp(tags->GetTag(TAG_ALBUM));
      const char *cstr = tmp.ToUTF8().data();
      strlen(cstr) > sizeof(mEncFormatCtx->album) -1 ? sizeof(mEncFormatCtx->album) -1 : strlen(cstr);
   }

   {
      memset(mEncFormatCtx->comment,0,sizeof(mEncFormatCtx->comment));
      wxString tmp(tags->GetTag(TAG_COMMENTS));
      const char *cstr = tmp.ToAscii();
      strlen(cstr) > sizeof(mEncFormatCtx->comment) -1 ? sizeof(mEncFormatCtx->comment) -1 : strlen(cstr);
   }

   {
      memset(mEncFormatCtx->genre,0,sizeof(mEncFormatCtx->genre));
      wxString tmp(tags->GetTag(TAG_GENRE));
      const char *cstr = tmp.ToAscii();
      strlen(cstr) > sizeof(mEncFormatCtx->genre) -1 ? sizeof(mEncFormatCtx->genre) -1 : strlen(cstr);
   }

   tags->GetTag(TAG_YEAR).ToLong((long*)&mEncFormatCtx->year);
   tags->GetTag(TAG_TRACK).ToLong((long*)&mEncFormatCtx->track);

   return true;
}

bool ExportFFmpeg::DisplayOptions(AudacityProject *project, int format)
{

   if (format == 0)
   {
      ExportFFmpegWAVOptions od(project);
      od.ShowModal();
      return true;
   }
   else if (format == 1)
   {
      ExportFFmpegMP3Options od(project);
      od.ShowModal();
      return true;
   }
   else if ((format == 2) || (format == 5) || (format == 14))
   {
      ExportFFmpegAACOptions od(project);
      od.ShowModal();
      return true;
   }
   else if ((format == 3) || (format == 7))
   {
      ExportFFmpegFLACOptions od(project);
      od.ShowModal();
      return true;
   }
   else if ((format == 4) || (format == 13))
   {
      ExportFFmpegAC3Options od(project);
      od.ShowModal();
      return true;
   }
   else if (format == 6)
   {
      ExportFFmpegVorbisOptions od(project);
      od.ShowModal();
      return true;
   }
   else if ((format == 8) || (format == 9))
   {
      wxMessageBox(wxT("There is no options for GSM"));
      return true;
   }
   else if (format == 10)
   {
      ExportFFmpegAMRNBOptions od(project);
      od.ShowModal();
      return true;
   }
   else if (format == 11)
   {
      ExportFFmpegAMRWBOptions od(project);
      od.ShowModal();
      return true;
   }
   else if (format == 12)
   {
      ExportFFmpegWMAOptions od(project);
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
