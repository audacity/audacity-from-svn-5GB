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
#include "../float_cast.h"
#include "../FileIO.h"
#include "../Internat.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Tags.h"
#include "../WaveTrack.h"
#include "../widgets/LinkingHtmlWindow.h"

#include "FileDialog.h"

#include "Export.h"

#ifdef USE_LIBID3TAG 
#include <id3tag.h>
#endif

#ifdef _DEBUG
    #ifdef _MSC_VER
        #undef THIS_FILE
        static char*THIS_FILE= __FILE__;
        #define new new(_NORMAL_BLOCK, THIS_FILE, __LINE__)
    #endif
#endif

//----------------------------------------------------------------------------
// ExportMP3Options
//----------------------------------------------------------------------------

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

#define PRESET_INSANE      0
#define PRESET_EXTREME     1
#define PRESET_STANDARD    2
#define PRESET_MEDIUM      3

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
   /* i18n-hint: kbps is the bitrate of the MP3 file, kilobits per second*/
   {wxT(""),   8},
   {wxT(""),  16},
   {wxT(""),  24},
   {wxT(""),  32},
   {wxT(""),  40},
   {wxT(""),  48},
   {wxT(""),  56},
   {wxT(""),  64},
   {wxT(""),  80},
   {wxT(""),  96},
   {wxT(""), 112},
   {wxT(""), 128},
   {wxT(""), 144},
   {wxT(""), 160},
   {wxT(""), 192},
   {wxT(""), 224},
   {wxT(""), 256},
   {wxT(""), 320}
};

static CHOICES varRates[] =
{
   {wxT(""), QUALITY_0},
   {wxT(""), QUALITY_1},
   {wxT(""), QUALITY_2},
   {wxT(""), QUALITY_3},
   {wxT(""), QUALITY_4},
   {wxT(""), QUALITY_5},
   {wxT(""), QUALITY_6},
   {wxT(""), QUALITY_7},
   {wxT(""), QUALITY_8},
   {wxT(""), QUALITY_9},
};

static CHOICES varModes[] =
{
   {wxT(""), ROUTINE_FAST    },
   {wxT(""), ROUTINE_STANDARD}
};

static CHOICES setRates[] =
{
   {wxT(""), PRESET_INSANE  },
   {wxT(""), PRESET_EXTREME },
   {wxT(""), PRESET_STANDARD},
   {wxT(""), PRESET_MEDIUM  },
};

static CHOICES sampRates[] =
{
   {wxT(""),  8000     },
   {wxT(""), 11025    },
   {wxT(""), 12000    },
   {wxT(""), 16000    },
   {wxT(""), 22050    },
   {wxT(""), 24000    },
   {wxT(""), 32000    },
   {wxT(""), 44100    },
   {wxT(""), 48000    },
};

#define ID_SET 7000
#define ID_VBR 7001
#define ID_ABR 7002
#define ID_CBR 7003

void InitMP3_Statics()
{
   for (size_t i=0; i < WXSIZEOF(fixRates); i++)
   {
      fixRates[i].name = wxT("");
      fixRates[i].name << fixRates[i].label << wxT(" ") << _("kbps");
   }
   for (size_t i=0; i < WXSIZEOF(varRates); i++)
   {
      varRates[i].name = wxT("");
      varRates[i].name << i << wxT(", ");
   }
   varRates[0].name << wxT("220-260");
   varRates[1].name << wxT("200-250");
   varRates[2].name << wxT("170-210");
   varRates[3].name << wxT("155-195");
   varRates[4].name << wxT("145-185");
   varRates[5].name << wxT("110-150");
   varRates[6].name << wxT("95-135");
   varRates[7].name << wxT("80-120");
   varRates[8].name << wxT("65-105");
   varRates[9].name << wxT("45-85");
   for (size_t i=0; i < WXSIZEOF(varRates); i++)
      varRates[i].name << wxT(" ") << _("kbps");
   varRates[0].name << wxT(" ") << _("(Best Quality)");
   varRates[9].name << wxT(" ") << _("(Smaller files)");

   varModes[0].name = _("Fast");
   varModes[1].name = _("Standard");

   for (size_t i=0; i < WXSIZEOF(setRates); i++)
      setRates[i].name = wxT("");
   setRates[0].name << _("Insane"  ) << wxT(", ") << 320;
   setRates[1].name << _("Extreme" ) << wxT(", ") << 220 << wxT("-") << 260;
   setRates[2].name << _("Standard") << wxT(", ") << 170 << wxT("-") << 210;
   setRates[3].name << _("Medium"  ) << wxT(", ") << 145 << wxT("-") << 185;
   for (size_t i=0; i < WXSIZEOF(setRates); i++)
      setRates[i].name << wxT(" ") << _("kbps");

   for (size_t i=0; i < WXSIZEOF(sampRates); i++)
   {
      sampRates[i].name = wxT("");
      sampRates[i].name << sampRates[i].label;
   }
}

class ExportMP3Options : public wxDialog
{
public:

   ExportMP3Options(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);
   void OnSET(wxCommandEvent& evt);
   void OnVBR(wxCommandEvent& evt);
   void OnABR(wxCommandEvent& evt);
   void OnCBR(wxCommandEvent& evt);
   void LoadNames(CHOICES *choices, int count);
   wxArrayString GetNames(CHOICES *choices, int count);
   wxArrayInt GetLabels(CHOICES *choices, int count);

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

BEGIN_EVENT_TABLE(ExportMP3Options, wxDialog)
   EVT_RADIOBUTTON(ID_SET,    ExportMP3Options::OnSET)
   EVT_RADIOBUTTON(ID_VBR,    ExportMP3Options::OnVBR)
   EVT_RADIOBUTTON(ID_ABR,    ExportMP3Options::OnABR)
   EVT_RADIOBUTTON(ID_CBR,    ExportMP3Options::OnCBR)
   EVT_BUTTON(wxID_OK,        ExportMP3Options::OnOK)
END_EVENT_TABLE()

/// 
/// 
ExportMP3Options::ExportMP3Options(wxWindow *parent)
:  wxDialog(NULL, wxID_ANY,
            wxString(_("Specify MP3 Options")),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxSTAY_ON_TOP)
{
   InitMP3_Statics();
   ShuttleGui S(this, eIsCreatingFromPrefs);

   PopulateOrExchange(S);
}

/// 
/// 
void ExportMP3Options::PopulateOrExchange(ShuttleGui & S)
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

               mRate = S.TieChoice(_("Quality"),
                                   wxT("/FileFormats/MP3Bitrate"), 
                                   defrate,
                                   GetNames(choices, cnt),
                                   GetLabels(choices, cnt));

               mMode = S.TieChoice(_("Variable Speed:"),
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

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

/// 
/// 
void ExportMP3Options::OnOK(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   EndModal(wxID_OK);

   return;
}

/// 
/// 
void ExportMP3Options::OnSET(wxCommandEvent& evt)
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
void ExportMP3Options::OnVBR(wxCommandEvent& evt)
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
void ExportMP3Options::OnABR(wxCommandEvent& evt)
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
void ExportMP3Options::OnCBR(wxCommandEvent& evt)
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

void ExportMP3Options::LoadNames(CHOICES *choices, int count)
{
   mRate->Clear();

   for (int i = 0; i < count; i++)
   {
      mRate->Append(choices[i].name);
   }
}

wxArrayString ExportMP3Options::GetNames(CHOICES *choices, int count)
{
   wxArrayString names;

   for (int i = 0; i < count; i++) {
      names.Add(choices[i].name);
   }

   return names;
}

wxArrayInt ExportMP3Options::GetLabels(CHOICES *choices, int count)
{
   wxArrayInt labels;

   for (int i = 0; i < count; i++) {
      labels.Add(choices[i].label);
   }

   return labels;
}

//----------------------------------------------------------------------------
// FindDialog
//----------------------------------------------------------------------------

#define ID_BROWSE 5000
#define ID_DLOAD  5001

class FindDialog : public wxDialog
{
public:

   FindDialog(wxWindow *parent, wxString path, wxString name, wxString type)
   :  wxDialog(parent, wxID_ANY, wxString(_("Locate Lame")))
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

         S.AddStandardButtons();
      }
      S.EndVerticalLay();

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

//----------------------------------------------------------------------------
// MP3Exporter
//----------------------------------------------------------------------------

class MP3Exporter
{
public:

   MP3Exporter();
   virtual ~MP3Exporter();

   bool FindLibrary(wxWindow *parent);
   bool LoadLibrary(wxWindow *parent, bool askuser);
   bool ValidLibraryLoaded();

   /* These global settings keep state over the life of the object */
   void SetMode(int mode);
   void SetBitrate(int rate);
   void SetQuality(int q, int r);
   void SetChannel(int mode);

   /* Virtual methods that must be supplied by library interfaces */

   /* initialize the library interface */
   virtual bool InitLibrary(wxString libpath) = 0;
   virtual void FreeLibrary() = 0;

   /* get library info */
   virtual wxString GetLibraryVersion() = 0;
   virtual wxString GetLibraryName() = 0;
   virtual wxString GetLibraryPath() = 0;
   virtual wxString GetLibraryTypeString() = 0;

   /* returns the number of samples PER CHANNEL to send for each call to EncodeBuffer */
   virtual int InitializeStream(int channels, int sampleRate) = 0;

   /* In bytes. must be called AFTER InitializeStream */
   virtual int GetOutBufferSize() = 0;

   /* returns the number of bytes written. input is interleaved if stereo*/
   virtual int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]) = 0;
   virtual int EncodeRemainder(short int inbuffer[], int nSamples,
                               unsigned char outbuffer[]) = 0;

   virtual int EncodeBufferMono(short int inbuffer[], unsigned char outbuffer[]) = 0;
   virtual int EncodeRemainderMono(short int inbuffer[], int nSamples,
                                   unsigned char outbuffer[]) = 0;

   virtual int FinishStream(unsigned char outbuffer[]) = 0;
   virtual void CancelEncoding() = 0;

protected:

   wxString mLibPath;

   bool mLibraryLoaded;
   bool mEncoding;

   int mMode;
   int mBitrate;
   int mQuality;
   int mRoutine;
   int mChannel;
};

//----------------------------------------------------------------------------
// MP3Exporter
//----------------------------------------------------------------------------

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

bool MP3Exporter::FindLibrary(wxWindow *parent)
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
      if (askuser && FindLibrary(parent)) {
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

//----------------------------------------------------------------------------
// LameExporter
//----------------------------------------------------------------------------

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
      mConf.format.LHV1.dwReSampleRate = sampleRate;
      mConf.format.LHV1.dwBitrate = mBitrate;

      // Set the VBR quality or ABR/CBR bitrate
      switch (mMode) {
         case MODE_SET:
         {
            int preset;

            if (mQuality == PRESET_INSANE) {
               preset = LQP_INSANE;
            }
            else if (mRoutine == ROUTINE_FAST) {
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
   /* note these values are for Windows only - Mac and Unix have their own
    * sections elsewhere */
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

#else //__WXMSW__

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
typedef int lame_set_disable_reservoir_t(lame_global_flags *, int);
typedef int lame_set_padding_type_t(lame_global_flags *, Padding_type);
typedef int lame_set_bWriteVbrTag_t(lame_global_flags *, int);

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
#endif //!__WXMSW__ 

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
      lame_set_disable_reservoir = (lame_set_disable_reservoir_t *)
          dlsym(mLib, "lame_set_disable_reservoir");
      lame_set_padding_type = (lame_set_padding_type_t *)
          dlsym(mLib, "lame_set_padding_type");
      lame_set_bWriteVbrTag = (lame_set_bWriteVbrTag_t *)
          dlsym(mLib, "lame_set_bWriteVbrTag");
      
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
         !lame_set_error_protection ||
         !lame_set_disable_reservoir ||
         !lame_set_padding_type ||
         !lame_set_bWriteVbrTag) {
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

      if (channels > 2) {
         return -1;
      }

      lame_set_error_protection(mGF, true);
      lame_set_num_channels(mGF, channels);
      lame_set_in_samplerate(mGF, sampleRate);
      lame_set_out_samplerate(mGF, sampleRate);
      lame_set_disable_reservoir(mGF, true);
      lame_set_padding_type(mGF, PAD_NO);
      lame_set_bWriteVbrTag(mGF, false);

      // Set the VBR quality or ABR/CBR bitrate
      switch (mMode) {
         case MODE_SET:
         {
            int preset;

            if (mQuality == PRESET_INSANE) {
               preset = INSANE;
            }
            else if (mRoutine == ROUTINE_FAST) {
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

      int rc = lame_init_params(mGF);
      if (rc < 0) {
         return rc;
      }

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
   /* this section is for all *nixes including Mac OS X, but not windows */
#if defined(__WXMAC__)
   /* values for Mac OS X */

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

#else //!__WXMAC__
   /* Values for Linux / Unix systems */
   wxString GetLibraryPath()
   {
      return wxT(LIBDIR);
   }

   wxString GetLibraryName()
   {
      return wxT("libmp3lame.so.0");
   }

   wxString GetLibraryTypeString()
   {
      return wxString(_("Only libmp3lame.so.0|libmp3lame.so.0|Primary Shared Object files (*.so)|*.so|Extended Libraries (*.so*)|*.so*|All Files (*)|*"));
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
   lame_set_disable_reservoir_t *lame_set_disable_reservoir;
   lame_set_padding_type_t *lame_set_padding_type;
   lame_set_bWriteVbrTag_t *lame_set_bWriteVbrTag;
   
   lame_global_flags *mGF;

   static const int mSamplesPerChunk = 220500;
   // See lame.h/lame_encode_buffer() for further explanation
   // As coded here, this should be the worst case.
   static const int mOutBufferSize = 
      mSamplesPerChunk * (320 / 8) / 8 + 4 * 1152 * (320 / 8) / 8 + 512;
};
#endif

//----------------------------------------------------------------------------
// ExportMP3
//----------------------------------------------------------------------------

class ExportMP3 : public ExportPlugin
{
public:

   ExportMP3();
   void Destroy();

   // Required

   bool DisplayOptions(AudacityProject *project = NULL);
   bool Export(AudacityProject *project,
               int channels,
               wxString fName,
               bool selectedOnly,
               double t0,
               double t1,
               MixerSpec *mixerSpec = NULL,
               Tags *metadata = NULL);

private:

   int FindValue(CHOICES *choices, int cnt, int needle, int def);
   wxString FindName(CHOICES *choices, int cnt, int needle);
   int AskResample(int bitrate, int rate, int lowrate, int highrate);
   int AddTags(AudacityProject *project, char **buffer, bool *endOfFile, Tags *tags);

};

ExportMP3::ExportMP3()
:  ExportPlugin()
{
   InitMP3_Statics();
   SetFormat(wxT("MP3"));
   SetExtension(wxT("mp3"));
   SetMaxChannels(2);
   SetCanMetaData(true);
   SetDescription(_("MP3 Files"));
}

void ExportMP3::Destroy()
{
   delete this;
}

bool ExportMP3::Export(AudacityProject *project,
                       int channels,
                       wxString fName,
                       bool selectionOnly,
                       double t0,
                       double t1,
                       MixerSpec *mixerSpec,
                       Tags *metadata)
{
   int rate = lrint(project->GetRate());
   wxWindow *parent = project;
   TrackList *tracks = project->GetTracks();
   LameExporter exporter;

   if (!exporter.LoadLibrary(parent, true)) {
      wxMessageBox(_("Could not open MP3 encoding library!"));
      gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));

      return false;
   }

   if (!exporter.ValidLibraryLoaded()) {
      wxMessageBox(_("Not a valid or supported MP3 encoding library!"));      
      gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));
      
      return false;
   }
   
   // Retrieve preferences
   int highrate = 48000;
   int lowrate = 8000;
   int bitrate = 0;
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
      exporter.SetMode(MODE_SET);
      exporter.SetQuality(q, r);
   }
   else if (rmode == MODE_VBR) {
      int q = FindValue(varRates, WXSIZEOF(varRates), brate, QUALITY_2);
      int r = FindValue(varModes, WXSIZEOF(varModes), vmode, ROUTINE_FAST);
      exporter.SetMode(MODE_VBR);
      exporter.SetQuality(q, r);
   }
   else if (rmode == MODE_ABR) {
      bitrate = FindValue(fixRates, WXSIZEOF(fixRates), brate, 128);
      exporter.SetMode(MODE_ABR);
      exporter.SetBitrate(bitrate);

      if (bitrate > 160) {
         lowrate = 32000;
      }
      else if (bitrate < 32 || bitrate == 144) {
         highrate = 24000;
      }
   }
   else {
      bitrate = FindValue(fixRates, WXSIZEOF(fixRates), brate, 128);
      exporter.SetMode(MODE_CBR);
      exporter.SetBitrate(bitrate);

      if (bitrate > 160) {
         lowrate = 32000;
      }
      else if (bitrate < 32 || bitrate == 144) {
         highrate = 24000;
      }
   }

   // Verify sample rate
   if (FindName(sampRates, WXSIZEOF(sampRates), rate).IsEmpty() ||
      (rate < lowrate) || (rate > highrate)) {
      rate = AskResample(bitrate, rate, lowrate, highrate);
      if (rate == 0) {
         return false;
      }
   }

   // Set the channel mode
   if (cmode == CHANNEL_JOINT) {
      exporter.SetChannel(CHANNEL_JOINT);
   }
   else {
      exporter.SetChannel(CHANNEL_STEREO);
   }

   sampleCount inSamples = exporter.InitializeStream(channels, rate);
   if (((int)inSamples) < 0) {
      wxMessageBox(_("Unable to initialize MP3 stream"));
      return false;
   }

   // Put ID3 tags at beginning of file
   if (metadata == NULL)
      metadata = project->GetTags();

   // Open file for writing
   FileIO outFile(fName, FileIO::Output);
   if (!outFile.IsOpened()) {
      wxMessageBox(_("Unable to open target file for writing"));
      return false;
   }

   char *id3buffer = NULL;
   int id3len;
   bool endOfFile;
   id3len = AddTags(project, &id3buffer, &endOfFile, metadata);
   if (id3len && !endOfFile) {
     outFile.Write(id3buffer, id3len);
   }

   bool cancelling = false;
   long bytes;

   int bufferSize = exporter.GetOutBufferSize();
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

   wxString title;
   if (rmode == MODE_SET) {
      title.Printf(selectionOnly ?
                   _("Exporting selected audio with %s preset") :
                   _("Exporting entire file with %s preset"),
                   FindName(setRates, WXSIZEOF(setRates), brate).c_str());
   }
   else if (rmode == MODE_VBR) {
      title.Printf(selectionOnly ?
                   _("Exporting selected audio with VBR quality %s") :
                   _("Exporting entire file with VBR quality %s"),
                   FindName(varRates, WXSIZEOF(setRates), brate).c_str());
   }
   else {
      title.Printf(selectionOnly ?
                   _("Exporting selected audio at %d Kbps") :
                   _("Exporting entire file at %d Kbps"),
                   brate);
   }
   GetActiveProject()->ProgressShow(wxFileName(fName).GetName(), title);

   while (!cancelling) {
      sampleCount blockLen = mixer->Process(inSamples);

      if (blockLen == 0) {
         break;
      }
      
      short *mixed = (short *)mixer->GetBuffer();

      if (blockLen < inSamples) {
         if (channels > 1) {
            bytes = exporter.EncodeRemainder(mixed,  blockLen , buffer);
         }
         else {
            bytes = exporter.EncodeRemainderMono(mixed,  blockLen , buffer);
         }
      }
      else {
         if (channels > 1) {
            bytes = exporter.EncodeBuffer(mixed, buffer);
         }
         else {
            bytes = exporter.EncodeBufferMono(mixed, buffer);
         }
      }

      if (bytes < 0) {
         wxString msg;
         msg.Printf(_("Error %d returned from MP3 encoder"), bytes);
         wxMessageBox(msg);
         break;
      }

      outFile.Write(buffer, bytes);

      int progressvalue = int (1000 * ((mixer->MixGetCurrentTime()-t0) /
                                          (t1-t0)));
      cancelling = !GetActiveProject()->ProgressUpdate(progressvalue);
   }

   GetActiveProject()->ProgressHide();

   delete mixer;

   bytes = exporter.FinishStream(buffer);

   if (bytes) {
      outFile.Write(buffer, bytes);
   }
   
   /* Write ID3 tag if it was supposed to be at the end of the file */
   
   if (id3len && endOfFile) {
      outFile.Write(id3buffer, id3len);
   }

   if (id3buffer) {
      free(id3buffer);
   }

   /* Close file */
   outFile.Close();

   delete[]buffer;
   
   return !cancelling;
}

bool ExportMP3::DisplayOptions(AudacityProject *project)
{
   ExportMP3Options od(project);

   od.ShowModal();

   return true;
}

int ExportMP3::FindValue(CHOICES *choices, int cnt, int needle, int def)
{
   for (int i = 0; i < cnt; i++) {
      if (choices[i].label == needle) {
         return needle;
      }
   }

   return def;
}

wxString ExportMP3::FindName(CHOICES *choices, int cnt, int needle)
{
   for (int i = 0; i < cnt; i++) {
      if (choices[i].label == needle) {
         return choices[i].name.BeforeFirst(wxT(','));
      }
   }

   return wxT("");
}

int ExportMP3::AskResample(int bitrate, int rate, int lowrate, int highrate)
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
               text.Printf(_("The project sample rate (%d) is not supported by the MP3\nfile format.  "), rate);
            }
            else {
               text.Printf(_("The project sample rate (%d) and bit rate (%d kbps) combination is not\nsupported by the MP3 file format.  "), rate, bitrate);
            }

            text += _("You may resample to one of the rates below.");
            S.AddTitle(text);
         }
         S.EndHorizontalLay();

         wxArrayString choices;
         wxString selected = wxT("");
         for (size_t i = 0; i < WXSIZEOF(sampRates); i++) {
            int label = sampRates[i].label;
            if (label >= lowrate && label <= highrate) {
               choices.Add(sampRates[i].name);
               if (label <= rate) {
                  selected = sampRates[i].name;
               }
            }
         }

         if (selected.IsEmpty()) {
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

// returns buffer len; caller frees
int ExportMP3::AddTags(AudacityProject *project, char **buffer, bool *endOfFile, Tags *tags)
{
#ifdef USE_LIBID3TAG 
   struct id3_tag *tp = id3_tag_new();

   bool v2 = tags->GetID3V2();

   wxString n, v;
   for (bool cont = tags->GetFirst(n, v); cont; cont = tags->GetNext(n, v)) {
      char *name = "TXXX";

      if (n.CmpNoCase(TAG_TITLE) == 0) {
         name = ID3_FRAME_TITLE;
      }
      else if (n.CmpNoCase(TAG_ARTIST) == 0) {
         name = ID3_FRAME_ARTIST;
      }
      else if (n.CmpNoCase(TAG_ALBUM) == 0) {
         name = ID3_FRAME_ALBUM;
      }
      else if (n.CmpNoCase(TAG_YEAR) == 0) {
         name = ID3_FRAME_YEAR;
      }
      else if (n.CmpNoCase(TAG_GENRE) == 0) {
         name = ID3_FRAME_GENRE;
         if (!v2) {
            v.Printf(wxT("%d"), tags->GetGenre(v));
         }
      }
      else if (n.CmpNoCase(TAG_COMMENTS) == 0) {
         name = ID3_FRAME_COMMENT;
      }
      else if (n.CmpNoCase(TAG_TRACK) == 0) {
         name = ID3_FRAME_TRACK;
      }

      struct id3_frame *frame = id3_frame_new(name);

      if (v2) {
         if (!n.IsAscii() || !v.IsAscii()) {
            id3_field_settextencoding(id3_frame_field(frame, 0), ID3_FIELD_TEXTENCODING_UTF_16);
         }
         else {
            id3_field_settextencoding(id3_frame_field(frame, 0), ID3_FIELD_TEXTENCODING_ISO_8859_1);
         }
      }

      id3_ucs4_t *ucs4 =
         id3_utf8_ucs4duplicate((id3_utf8_t *) (const char *) v.mb_str(wxConvUTF8));

      if (strcmp(name, ID3_FRAME_COMMENT) == 0) {
         // A hack to get around iTunes not recognizing the comment.  The
         // language defaults to XXX and, since it's not a valid language,
         // iTunes just ignores the tag.  So, either set it to a valid language
         // (which one???) or just clear it.  Unfortunately, there's no supported
         // way of clearing the field, so do it directly.
         id3_field *f = id3_frame_field(frame, 1);
         memset(f->immediate.value, 0, sizeof(f->immediate.value));
         id3_field_setfullstring(id3_frame_field(frame, 3), ucs4);
      }
      else if (strcmp(name, "TXXX") == 0) {
         id3_field_setstring(id3_frame_field(frame, 2), ucs4);
         free(ucs4);

         ucs4 = id3_utf8_ucs4duplicate((id3_utf8_t *) (const char *) n.mb_str(wxConvUTF8));
           
         id3_field_setstring(id3_frame_field(frame, 1), ucs4);
      }
      else {
         id3_field_setstrings(id3_frame_field(frame, 1), 1, &ucs4);
      }

      free(ucs4);

      id3_tag_attachframe(tp, frame);
   }

   if (v2) {
      tp->options &= (~ID3_TAG_OPTION_COMPRESSION); // No compression

      // If this version of libid3tag supports it, use v2.3 ID3
      // tags instead of the newer, but less well supported, v2.4
      // that libid3tag uses by default.
      #ifdef ID3_TAG_HAS_TAG_OPTION_ID3V2_3
      tp->options |= ID3_TAG_OPTION_ID3V2_3;
      #endif

      *endOfFile = false;
   }
   else {
      tp->options |= ID3_TAG_OPTION_ID3V1;
      *endOfFile = true;
   }

   id3_length_t len;
   
   len = id3_tag_render(tp, 0);
   *buffer = (char *)malloc(len);
   len = id3_tag_render(tp, (id3_byte_t *)*buffer);

   id3_tag_delete(tp);

   return len;
#else //ifdef USE_LIBID3TAG 
   return 0;
#endif
}

//----------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------
ExportPlugin *New_ExportMP3()
{
   return new ExportMP3();
}

//----------------------------------------------------------------------------
// Return library version
//----------------------------------------------------------------------------

wxString GetMP3Version(wxWindow *parent, bool prompt)
{
   LameExporter exporter;
   wxString versionString = _("MP3 exporting plugin not found");

   if (prompt) {
      exporter.FindLibrary(parent);
   }

   if (exporter.LoadLibrary(parent, false)) {
      versionString = exporter.GetLibraryVersion();
   }

   return versionString;
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
