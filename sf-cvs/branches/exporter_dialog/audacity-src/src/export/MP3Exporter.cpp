/**********************************************************************

  Audacity: A Digital Audio Editor

  MP3Exporter.cpp

  (c) Joshua Haberman and The Audacity Team
 
   See MP3Exporter.h for detailed description of class.

**********************************************************************/

#include <wx/defs.h>
#include <wx/textctrl.h>
#include <wx/dynlib.h>
#include <wx/msgdlg.h>
#include <wx/utils.h>
#include <wx/progdlg.h>
#include <wx/timer.h>
#include <wx/window.h>
#include <wx/ffile.h>
#include <wx/log.h>
#include <wx/filedlg.h>
#include <wx/intl.h>

#include "../Audacity.h"
#include "MP3Exporter.h"
#include "Exporter.h"

#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Tags.h"
#include "../WaveTrack.h"
#include "../FileFormats.h"
#include "../LabelTrack.h"
#include "../SampleFormat.h"
#include "../Track.h"
#include "../WaveTrack.h"


#ifdef __WXMAC__
#define __MOVIES__  /* Apple's Movies.h not compatible with Audacity */
/* #define __MACHELP__ */

#include <wx/mac/private.h>
# ifdef __UNIX__
#  include <CoreServices/CoreServices.h>
# else
#  include <Files.h>
# endif
#endif

#include <iostream>


MP3Exporter::MP3Exporter(AudacityProject * project, double t0, double t1,
			 bool exportSelection, int outrate,
			 int channels):
  Exporter(project, t0, t1, exportSelection, outrate, channels)

{
  if (gPrefs)
    mLibPath = gPrefs->Read("/MP3/MP3LibPath", "");
  
}


MP3Exporter::~MP3Exporter()
{

}

bool MP3Exporter::Verify()
{
  return ValidLibraryLoaded();
}


bool MP3Exporter::Export(const wxString & filename)
{


 
  mFileName = filename;

  TrackList *tracks = mProject->GetTracks();

 
  // if (!success)
  //  return false;

  //  success = LoadLibrary();
  //  if (!success) {
  //   wxMessageBox(_("Could not open MP3 encoding library!"));
  //  gPrefs->Write("/MP3/MP3LibPath", wxString(""));
    
  //  return false;
  //  }

  
  bool success = ValidLibraryLoaded();   
  if(!success) {
    wxMessageBox(_("Not a valid or supported MP3 encoding library!"));      
    gPrefs->Write("/MP3/MP3LibPath", wxString(""));
    
    return false;
  }

 
  /* Open file for writing */

  wxFFile outFile(mFileName, "wb");
  if (!outFile.IsOpened()) {
    wxMessageBox(_("Unable to open target file for writing"));
    return false;
  }
   
  /* Put ID3 tags at beginning of file */
   
  Tags *tags = mProject->GetTags();
  if (!tags->ShowEditDialog(mProject, _("Edit the ID3 tags for the MP3 file")))
    return false;  // used selected "cancel"

  char *id3buffer = NULL;
  int id3len;
  bool endOfFile;
  id3len = tags->ExportID3(&id3buffer, &endOfFile);
  if (!endOfFile)
    outFile.Write(id3buffer, id3len);

  /* Export MP3 using DLL */
 
  sampleCount inSamples = InitializeStream();

  wxProgressDialog *progress = NULL;
  wxYield();
  wxStartTimer();
  wxBusyCursor busy;
  bool cancelling = false;
  long bytes;

  int bufferSize = GetOutBufferSize();
  unsigned char *buffer = new unsigned char[bufferSize];
  wxASSERT(buffer);

  int numWaveTracks;
  WaveTrack **waveTracks;
  tracks->GetWaveTracks(mExportSelection, &numWaveTracks, &waveTracks);

  
  Mixer *mixer = new Mixer(numWaveTracks, waveTracks,
			   tracks->GetTimeTrack(),
			   m_t0, m_t1,
			   mChannels, inSamples, true,
			   mInRate, int16Sample);

  while(!cancelling) {

    sampleCount blockLen = mixer->Process(inSamples);

    if (blockLen == 0)
      break;
      
    short *mixed = (short *)mixer->GetBuffer();

    if(blockLen < inSamples)
      bytes = EncodeRemainder(mixed, blockLen, buffer);
    else
      bytes = EncodeBuffer(mixed, buffer);

    outFile.Write(buffer, bytes);


    //If exporting takes more than 500 ms, create a dialog to monitor progress.
    //This will only get created if one doesn't already exist.
    if (!progress && wxGetElapsedTime(false) > 500) 
      {
	
	wxString message;
	
	if (mExportSelection)
	  message =
	    wxString::Format(_("Exporting the selected audio as an mp3"));
	else
	  message =
	    wxString::Format(_("Exporting the entire project as an mp3"));
	
	progress =
	  new wxProgressDialog(_("Export"),
			       message,
			       1000,
			       mProject,
			       wxPD_CAN_ABORT |
			       wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
      }

    if (progress) {
      int progressvalue = int (1000 * ((mixer->MixGetCurrentTime()-m_t0) /
				       (m_t1-m_t0)));
      cancelling = !progress->Update(progressvalue);
    }

  }

  delete mixer;

  bytes = FinishStream(buffer);

  if (bytes)
    outFile.Write(buffer, bytes);
   
  /* Write ID3 tag if it was supposed to be at the end of the file */
   
  if (endOfFile)
    outFile.Write(id3buffer, id3len);
  free(id3buffer);

  /* Close file */
   
  outFile.Close();
      
  /* MacOS: set the file type/creator so that the OS knows it's an MP3
     file which was created by Audacity */
      
#ifdef __WXMAC__
  FSSpec spec;
  wxMacFilename2FSSpec(fName, &spec);
  FInfo finfo;
  if (FSpGetFInfo(&spec, &finfo) == noErr) {
    finfo.fdType = 'MP3 ';
    finfo.fdCreator = AUDACITY_CREATOR;

    FSpSetFInfo(&spec, &finfo);
  }
#endif

  if (progress)
    delete progress;

  delete[]buffer;
   
  return true;
}



/// FindLibrary looks in the preferences to locate the mp3 library.
/// 
bool MP3Exporter::FindLibrary(wxWindow *parent)
{
  mLibPath = gPrefs->Read("/MP3/MP3LibPath", "");

  if (mLibPath=="" || !::wxFileExists(mLibPath)) {
   
    int action = wxMessageBox(GetLibraryMessage(),
			      _("Export MP3"),
			      wxYES_NO | wxICON_EXCLAMATION,
			      parent);

    if (action == wxYES) {
      wxString question;
      question.Printf(_("Where is %s?"), (const char *)GetLibraryName());
      mLibPath = wxFileSelector(question, 
				GetLibraryPath(),        // Path
				GetLibraryName(),        // Name
				"",      // Extension
				GetLibraryTypeString(),
				wxOPEN, parent);
         
      if (mLibPath == "") {
	mLibPath = "";
	gPrefs->Write("/MP3/MP3LibPath", mLibPath);
         
	return false;
      }
         
      wxString path, baseName, extension;
      ::wxSplitPath(mLibPath, &path, &baseName, &extension);
         
      if (extension != "")
	baseName += "." + extension;
         
      if (baseName.CmpNoCase(GetLibraryName())) {
         
	wxString question;
	question.Printf(_("Audacity was expecting a library named \"%s\".  "
			  "Are you sure you want to attempt to export MP3 "
			  "files using \"%s\"?"),
			(const char *)GetLibraryName(),
			(const char *)baseName);

	int action = wxMessageBox(question,
				  _("Export MP3"),
				  wxYES_NO | wxICON_EXCLAMATION,
				  parent);
            
	if (action != wxYES) {
	  mLibPath = "";
	  gPrefs->Write("/MP3/MP3LibPath", mLibPath);
            
	  return false;
	}
      }
    }
    else {
      mLibPath = "";
      gPrefs->Write("/MP3/MP3LibPath", mLibPath);
            
      return false;
    }
      
    gPrefs->Write("/MP3/MP3LibPath", mLibPath);
  }
   
  return true;
}


/* --------------------------------------------------------------------------*/
/* --------------------------------------------------------------------------*/
/* --------------------------------------------------------------------------*/
//The following code is compiled only on WXGTK (linux bsd etc.)
#ifdef __WXGTK__

PlatformMP3Exporter::PlatformMP3Exporter(AudacityProject * project, double t0, double t1,
					 bool exportSelection, int outrate, int channels, int bitrate, int quality):
  MP3Exporter(project, t0, t1, exportSelection, outrate, channels),
  mGF(NULL),
  mLibraryLoaded(false),
  mEncoding(false)
{

  if(LoadLibrary())
    {
      SetBitRate(bitrate);
      SetQuality(quality);
    }


}

PlatformMP3Exporter::~PlatformMP3Exporter()
{
}

wxString PlatformMP3Exporter::GetLibraryPath()
{
  return "/usr/lib";
}

wxString PlatformMP3Exporter::GetLibraryName()
{
  return "libmp3lame.so";
}
         
wxString PlatformMP3Exporter::GetLibraryTypeString()
{
  return wxString(_("Only libmp3lame.so|libmp3lame.so|Primary Shared Object files (*.so)|*.so|Extended Libraries (*.so*)|*.so*|All Files (*)|*"));
}
         
wxString PlatformMP3Exporter::GetLibraryMessage()
{
  return _("Audacity does not export MP3 files directly, but instead uses the \n"
	   "freely available LAME library to handle MP3 file encoding.  You must \n"
	   "obtain libmp3lame.so separately, either by downloading it or building \n"
	   "it from the sources, and then locate the file for Audacity.  You only \n"
	   "need to do this once.\n\n"
	   "Would you like to locate libmp3lame.so now?");
}

bool  PlatformMP3Exporter::LoadLibrary()
{
  
  //BG: I was unable to test the wxDynamicLibrary code on this platform
  
  if (wxFileExists(mLibPath))
    {
      if(lame_enc_lib.IsLoaded())
	{
	  lame_enc_lib.Unload();
	}
      
      if(!lame_enc_lib.Load(mLibPath))
	{
	  return false;
	}
    }
  else
    return false;
  
  /* get function pointers from the shared library */
  
  lame_init = (lame_init_t *)lame_enc_lib.GetSymbol("lame_init");
  get_lame_version = (get_lame_version_t *)lame_enc_lib.GetSymbol("get_lame_version");
  lame_init_params = 
    (lame_init_params_t *) lame_enc_lib.GetSymbol("lame_init_params");
  lame_encode_buffer_interleaved =
    (lame_encode_buffer_interleaved_t *) lame_enc_lib.GetSymbol("lame_encode_buffer_interleaved");
  lame_encode_flush =  (lame_encode_flush_t *) lame_enc_lib.GetSymbol("lame_encode_flush");
  lame_close = (lame_close_t *) lame_enc_lib.GetSymbol("lame_close");
  lame_close = (lame_close_t *) lame_enc_lib.GetSymbol("lame_close");
  lame_set_in_samplerate =
    (lame_set_in_samplerate_t *) lame_enc_lib.GetSymbol("lame_set_in_samplerate");
  lame_set_num_channels =
    (lame_set_num_channels_t *) lame_enc_lib.GetSymbol("lame_set_num_channels");
  lame_set_quality = (lame_set_quality_t *) lame_enc_lib.GetSymbol("lame_set_quality");
  lame_get_quality = (lame_get_quality_t *) lame_enc_lib.GetSymbol("lame_get_quality");
  lame_set_brate =  (lame_set_brate_t *) lame_enc_lib.GetSymbol("lame_set_brate");
  lame_get_brate =  (lame_get_brate_t *) lame_enc_lib.GetSymbol("lame_get_brate");

  /* we assume that if all the symbols are found, it's a valid library */
  
  if (!lame_init ||
      !get_lame_version ||
      !lame_init_params ||
      !lame_encode_buffer_interleaved ||
      !lame_encode_flush ||
      !lame_close ||
      !lame_set_in_samplerate ||
      !lame_set_num_channels ||
      !lame_set_quality ||
      !lame_set_brate) 
    {
      return false;
    }
  
  mGF = lame_init();
  mLibraryLoaded = true;
  return true;
}

bool PlatformMP3Exporter::ValidLibraryLoaded() 
{
  return mLibraryLoaded;
}

wxString PlatformMP3Exporter::GetLibraryVersion() {
  if(!mLibraryLoaded) return "";
  
  return wxString::Format("LAME %s", get_lame_version());
}

int PlatformMP3Exporter::InitializeStream()
{
  if(!mLibraryLoaded) return -1;
  
  lame_set_num_channels(mGF, mChannels);
  lame_set_in_samplerate(mGF,(int)(mInRate+.5));
  
  lame_init_params(mGF);
  
  mEncoding = true;
  return mSamplesPerChunk;
}

int PlatformMP3Exporter::GetOutBufferSize() {
  return mOutBufferSize;
}

int PlatformMP3Exporter::EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]) {
  if(!mEncoding) return -1;
  
  return lame_encode_buffer_interleaved(mGF, inbuffer, mSamplesPerChunk,
					outbuffer, mOutBufferSize);
}

int PlatformMP3Exporter::EncodeRemainder(short int inbuffer[], int nSamples,
					 unsigned char outbuffer[]) {
  return lame_encode_buffer_interleaved(mGF, inbuffer, nSamples, outbuffer,
					mOutBufferSize);
}

int PlatformMP3Exporter::FinishStream(unsigned char outbuffer[]) {
  mEncoding = false;
  int result = lame_encode_flush(mGF, outbuffer, mOutBufferSize);
  lame_close(mGF);
  return result;
}

void PlatformMP3Exporter::CancelEncoding() { mEncoding = false; }

int PlatformMP3Exporter::GetConfigurationCaps() { return MP3CONFIG_BITRATE|MP3CONFIG_QUALITY; }

int PlatformMP3Exporter::GetQualityVariance() { return 10; }


void PlatformMP3Exporter::SetBitRate(int rate)
{
  lame_set_brate(mGF, rate); 
}


int PlatformMP3Exporter::GetBitRate() { return lame_get_quality(mGF); }

void PlatformMP3Exporter::SetQuality(int quality) { lame_set_quality(mGF, quality); }
int PlatformMP3Exporter::GetQuality() { return lame_get_quality(mGF); }



/* --------------------------------------------------------------------------*/
/* --------------------------------------------------------------------------*/
//The following code is compiled only on Mac OSX 
#elif defined(__MACOSX__)
         
PlatformMP3Exporter::PlatformMP3Exporter(AudacityProject * project, double t0, double t1,
					 bool exportSelection, int outrate,
					 int channels, int bitrate, int quality):
  MP3Exporter(project, t0, t1, exportSelection, outrate, channels),
  mGF(NULL),
  mLibraryLoaded(false),
  mEncoding(false),

{

  if(LoadLibrary())
    {
      SetBitRate(bitrate);
      //      SetQuality(quality);
    }

}
         
PlatformMP3Exporter::~PlatformMP3Exporter()
{ 
  if(lame_enc_lib.IsLoaded())
    {
      lame_enc_lib.Unload();
    }
}

wxString PlatformMP3Exporter::GetLibraryPath()
{
  return "";
}


void PlatformMP3Exporter::MakePString(unsigned char *p, const char *c)
{
  int len = strlen(c);
  for(int i=len; i>=1; i--)
    p[i] = (unsigned char)c[i-1];

  p[0] = (unsigned char)len;
}

// MachOFunctionPointerForCFMFunctionPointer(void *cfmfp)
//
// Borrowed from the Apple Sample Code file "CFM_MachO_CFM.c"
// This function allocates a block of CFM glue code which contains
// the instructions to call CFM routines

void *PlatformMP3Exporter::NewMachOFromCFM(void *cfmfp)
{
  if (cfmfp == 0)
    return 0;
  
  UInt32 CFMTemplate[6] = {0x3D800000, 0x618C0000, 0x800C0000,
			   0x804C0004, 0x7C0903A6, 0x4E800420};
  UInt32 *mfp = (UInt32*)NewPtr(sizeof(CFMTemplate));
  
  mfp[0] = CFMTemplate[0] | ((UInt32)cfmfp >> 16);
  mfp[1] = CFMTemplate[1] | ((UInt32)cfmfp & 0xFFFF);
  mfp[2] = CFMTemplate[2];
  mfp[3] = CFMTemplate[3];
  mfp[4] = CFMTemplate[4];
  mfp[5] = CFMTemplate[5];
  MakeDataExecutable(mfp, sizeof(CFMTemplate));
  
  return(mfp);
}



wxString PlatformMP3Exporter::GetLibraryName()
{
  return "LameLib";
}
         
wxString PlatformMP3Exporter::GetLibraryTypeString()
{
  return wxString(_("Only LameLib|LameLib|All Files (*)|*"));
}
         
wxString PlatformMP3Exporter::GetLibraryMessage()
{
  // Must be <= 255 characters on Mac
  return _("Audacity does not export MP3 files directly, but instead uses LAME, "
	   "an MP3 exporting library available separately.  See the documentation "
	   "for more information.\n\n"
	   "Would you like to locate LameLib now?");
}

bool  PlatformMP3Exporter::LoadLibrary() {
  FSSpec spec;
  OSErr err;
  CFragConnectionID connID;
  Ptr mainAddr;
  Str255 errMsg;
  Str255 name;
  CFragSymbolClass symClass;

  if (!wxFileExists(mLibPath))
    return false;

  wxMacFilename2FSSpec(mLibPath, &spec);

  name[0] = 0;
  err = GetDiskFragment(&spec, 0, kCFragGoesToEOF,
			name,
			kPrivateCFragCopy,
			&connID,
			&mainAddr,
			errMsg);

  if (err) {
    printf("GetDiskFragment: err=%d\n", err);
    return false;
  }

  MakePString(name, "lame_init");
  FindSymbol(connID, name, &mainAddr, &symClass);
  lame_init = NewMachOFromCFM(mainAddr);

  MakePString(name, "get_lame_version");
  FindSymbol(connID, name, &mainAddr, &symClass);
  get_lame_version = NewMachOFromCFM(mainAddr);

  MakePString(name, "lame_init_params");
  FindSymbol(connID, name, &mainAddr, &symClass);
  lame_init_params = NewMachOFromCFM(mainAddr);

  MakePString(name, "lame_encode_buffer_interleaved");
  FindSymbol(connID, name, &mainAddr, &symClass);
  lame_encode_buffer_interleaved = NewMachOFromCFM(mainAddr);

  MakePString(name, "lame_encode_flush");
  FindSymbol(connID, name, &mainAddr, &symClass);
  lame_encode_flush = NewMachOFromCFM(mainAddr);

  MakePString(name, "lame_close");
  FindSymbol(connID, name, &mainAddr, &symClass);
  lame_close = NewMachOFromCFM(mainAddr);

  MakePString(name, "lame_set_in_samplerate");
  FindSymbol(connID, name, &mainAddr, &symClass);
  lame_set_in_samplerate = NewMachOFromCFM(mainAddr);

  MakePString(name, "lame_set_num_channels");
  FindSymbol(connID, name, &mainAddr, &symClass);
  lame_set_num_channels = NewMachOFromCFM(mainAddr);

  MakePString(name, "lame_set_quality");
  FindSymbol(connID, name, &mainAddr, &symClass);
  lame_set_quality = NewMachOFromCFM(mainAddr);

  MakePString(name, "lame_get_quality");
  FindSymbol(connID, name, &mainAddr, &symClass);
  lame_get_quality = NewMachOFromCFM(mainAddr);

  MakePString(name, "lame_set_brate");
  FindSymbol(connID, name, &mainAddr, &symClass);
  lame_set_brate = NewMachOFromCFM(mainAddr);

  MakePString(name, "lame_get_brate");
  FindSymbol(connID, name, &mainAddr, &symClass);
  lame_get_brate = NewMachOFromCFM(mainAddr);


  /* we assume that if all the symbols are found, it's a valid library */

  if (!lame_init ||
      !get_lame_version ||
      !lame_init_params ||
      !lame_encode_buffer_interleaved ||
      !lame_encode_flush ||
      !lame_close ||
      !lame_set_in_samplerate ||
      !lame_set_num_channels ||
      !lame_set_quality ||
      !lame_set_brate) {
    return false;
  }

  mGF = lame_init();
  mLibraryLoaded = true;
  return true;
}

bool PlatformMP3Exporter::ValidLibraryLoaded() { return mLibraryLoaded; }

wxString GetLibraryVersion() {
  if(!mLibraryLoaded) return "";

  return wxString::Format("LAME %s", get_lame_version());
}

int PlatformMP3Exporter::InitializeStream()
{
  if(!mLibraryLoaded) return -1;

  lame_set_num_channels(mGF, mChannels);
  lame_set_in_samplerate(mGF,mInRate);

  lame_init_params(mGF);

  mEncoding = true;
  return mSamplesPerChunk;
}

int PlatformMP3Exporter::GetOutBufferSize() {
  return mOutBufferSize;
}

int PlatformMP3Exporter::EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]) {
  if(!mEncoding) return -1;

  return lame_encode_buffer_interleaved(mGF, inbuffer, mSamplesPerChunk,
					outbuffer, mOutBufferSize);
}

int PlatformMP3Exporter::EncodeRemainder(short int inbuffer[], int nSamples,
					 unsigned char outbuffer[]) {
  return lame_encode_buffer_interleaved(mGF, inbuffer, nSamples, outbuffer,
					mOutBufferSize);
}

int PlatformMP3Exporter::FinishStream(unsigned char outbuffer[]) {
  mEncoding = false;
  int result = lame_encode_flush(mGF, outbuffer, mOutBufferSize);
  lame_close(mGF);
  return result;
}

void PlatformMP3Exporter::CancelEncoding() { mEncoding = false; }

int PlatformMP3Exporter::GetConfigurationCaps() { return MP3CONFIG_BITRATE|MP3CONFIG_QUALITY; }

int PlatformMP3Exporter::GetQualityVariance() { return 10; }

void PlatformMP3Exporter::SetBitRate(int rate) { lame_set_brate(mGF, rate); }
int PlatformMP3Exporter::GetBitRate() { return lame_get_quality(mGF); }

void PlatformMP3Exporter::SetQuality(int quality) { lame_set_quality(mGF, quality); }
int PlatformMP3Exporter::GetQuality() { return lame_get_quality(mGF); }



/* --------------------------------------------------------------------------*/
/* --------------------------------------------------------------------------*/
/* --------------------------------------------------------------------------*/
//The following code is compiled only on classic MacOS 


#elif defined(__WXMAC__)


PlatformMP3Exporter::PlatformMP3Exporter(AudacityProject * project, double t0, double t1,
					 bool exportSelection, int outrate,
					 int channels, int bitrate, int quality):
  MP3Exporter(project, t0, t1, exportSelection, outrate, channels),
  mGF(NULL),
  mLibraryLoaded(false),
  mEncoding(false)


{
    if(LoadLibrary())
    {
      SetBitRate(bitrate);
      //      SetQuality(quality);
    }

}

PlatformMP3Exporter::~PlatformMP3Exporter()
{
}
wxString  PlatformMP3Exporter::GetLibraryPath()
{
  return "";
}

wxString  PlatformMP3Exporter::GetLibraryName()
{
  return "LAMELib";
}
         
wxString  PlatformMP3Exporter::GetLibraryTypeString()
{
  return _("Only LAMELib|LAMELib|Shared Libraries (*)|*");
}
         
wxString  PlatformMP3Exporter::GetLibraryMessage()
{
  // Must be <= 255 characters on Mac
  return _("Audacity does not export MP3 files directly, but instead uses LAME, "
	   "an MP3 exporting library available separately.  See the documentation "
	   "for more information.\n\n"
	   "Would you like to locate LameLib now?");
}

bool   PlatformMP3Exporter::LoadLibrary() {
  wxLogNull logNo;

  //BG: I was unable to test the wxDynamicLibrary code on this platform

  if (wxFileExists(mLibPath))
    {
      if(lame_enc_lib.IsLoaded())
	{
	  lame_enc_lib.Unload();
	}

      if(!lame_enc_lib.Load(mLibPath))
	{
	  return false;
	}
    }
  else
    return false;

  lame_init = (lame_init_t *) lame_enc_lib.GetSymbol("lame_init");

  lame_version = (lame_version_t *) lame_enc_lib.GetSymbol("lame_version");
            
  get_lame_version =
    (get_lame_version_t *) lame_enc_lib.GetSymbol("get_lame_version");

  lame_init_params = 
    (lame_init_params_t *) lame_enc_lib.GetSymbol("lame_init_params");

  lame_encode_buffer =
    (lame_encode_buffer_t *) lame_enc_lib.GetSymbol("lame_encode_buffer");
  lame_encode_finish =
    (lame_encode_finish_t *) lame_enc_lib.GetSymbol("lame_encode_finish");

  if (!lame_init ||
      !lame_init_params ||
      !lame_encode_buffer ||
      !(lame_version || get_lame_version) ||
      !lame_encode_finish) {
    return false;
  }

  mGF = new lame_global_flags;
  lame_init(mGF);
  mLibraryLoaded = true;
  return true;
}

bool  PlatformMP3Exporter::ValidLibraryLoaded() { return mLibraryLoaded; }

wxString GetLibraryVersion() {
  if(!mLibraryLoaded) return "";

  if(get_lame_version)
    return get_lame_version();
  else {
    lame_version(mGF, mVersion);
    return mVersion;
  }
}

int  PlatformMP3Exporter::InitializeStream()
{
  if(!mLibraryLoaded) return -1;

  mGF->num_channels = mChannels;
  mGF->in_samplerate = (double)mInRate;
  mGF->out_samplerate =  (double)mOutRate;
  mGF->num_samples = 0;
         
  if (channels == 1)
    mGF->mode = 3;  // mono
  else
    mGF->mode = 1;  // joint stereo

  lame_init_params(mGF);
         
  mLeftBuffer = new short[mSamplesPerChunk];
  mRightBuffer = new short[mSamplesPerChunk];

  mEncoding = true;
  return mSamplesPerChunk;
}

int  PlatformMP3Exporter::GetOutBufferSize() {
  return mOutBufferSize;
}
      
int  PlatformMP3Exporter::GetConfigurationCaps() {
  return MP3CONFIG_BITRATE;
}

int  PlatformMP3Exporter::EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]) {
  if(!mEncoding) return -1;
         
  if (mGF->num_channels == 2) {
    for(int i=0; i<mSamplesPerChunk; i++) {
      mLeftBuffer[i] = inbuffer[2*i];
      mRightBuffer[i] = inbuffer[2*i+1];
    }

    return lame_encode_buffer(mGF, mLeftBuffer, mRightBuffer, mSamplesPerChunk,
			      outbuffer, mOutBufferSize);
  }
  else {
    return lame_encode_buffer(mGF, inbuffer, inbuffer, mSamplesPerChunk,
			      outbuffer, mOutBufferSize);
  }
}

int  PlatformMP3Exporter::EncodeRemainder(short int inbuffer[], int nSamples,
					  unsigned char outbuffer[]) {

  if (mGF->num_channels == 2) {
    for(int i=0; i<nSamples; i++) {
      mLeftBuffer[i] = inbuffer[2*i];
      mRightBuffer[i] = inbuffer[2*i+1];
    }

    return lame_encode_buffer(mGF, mLeftBuffer, mRightBuffer, nSamples, outbuffer,
			      mOutBufferSize);
  }
  else {
    return lame_encode_buffer(mGF, inbuffer, inbuffer, nSamples,
			      outbuffer, mOutBufferSize);
  }
}

int  PlatformMP3Exporter::FinishStream(unsigned char outbuffer[]) {
  mEncoding = false;
  int result = lame_encode_finish(mGF, outbuffer, mOutBufferSize);
         
  delete[] mLeftBuffer;
  delete[] mRightBuffer;
         
  return result;
}

void  PlatformMP3Exporter::CancelEncoding() { mEncoding = false; }

int  PlatformMP3Exporter::GetQualityVariance() { return -1; }

void  PlatformMP3Exporter::SetBitRate(int rate) { }
int  PlatformMP3Exporter::GetBitRate() { return -1; }

void  PlatformMP3Exporter::SetQuality(int quality) { }
int  PlatformMP3Exporter::GetQuality() { return -1; }


/* --------------------------------------------------------------------------*/
/* --------------------------------------------------------------------------*/
/* --------------------------------------------------------------------------*/
//The following code is compiled only on  Win32

//--------------------------------------------------------
#elif defined(__WXMSW__)

#include "BladeMP3EncDLL.h"

PlatformMP3Exporter::PlatformMP3Exporter(AudacityProject * project, double t0, double t1,
					 bool exportSelection, int outrate,
					 int channels, int bitrate, int quality):
  MP3Exporter(project, t0, t1, exportSelection, outrate, channels)
  mGF(NULL),
  mLibraryLoaded(false),
  mEncoding(false)


{

  if(LoadLibrary())
    {
      SetBitRate(bitrate);
      //      SetQuality(quality);
    }

  /* Set all the config defaults to sane values */

  memset(&mConf, 0, sizeof(BE_CONFIG));
  //      mConf.dwConfig = BE_CONFIG_LAME;
  //      mConf.format.LHV1.dwStructVersion = 1;
  //      mConf.format.LHV1.dwStructSize = sizeof(BE_CONFIG);
  //      mConf.format.LHV1.dwReSampleRate = 0;
  //      mConf.format.LHV1.dwBitrate = 128;
  //mConf.format.LHV1.dwMaxBitrate = 128;
  //      mConf.format.LHV1.nPreset = LQP_HIGH_QUALITY;
  //	  mConf.format.LHV1.dwMpegVersion = MPEG1;
  //      mConf.format.LHV1.bCopyright = false;
  //      mConf.format.LHV1.bCRC = true;
  //      mConf.format.LHV1.bOriginal = false;
  //      mConf.format.LHV1.bPrivate = false;
  //      mConf.format.LHV1.bWriteVBRHeader = false;
  //      mConf.format.LHV1.bEnableVBR = true;
  //      mConf.format.LHV1.nVBRQuality = 2;
  //      mConf.format.LHV1.dwVbrAbr_bps = -1;
  //      mConf.format.LHV1.bNoRes = true;
  mConf.dwConfig = BE_CONFIG_MP3;
  mConf.format.mp3.wBitrate = 128;
  mConf.format.mp3.bCopyright = false;
  mConf.format.mp3.bCRC = true;
  mConf.format.mp3.bOriginal = false;
  mConf.format.mp3.bPrivate = false;
      
  mDefaultRate = 128;

}

PlatformMP3Exporter::~PlatformMP3Exporter()
{
}

wxString PlatformMP3Exporter::GetLibraryPath()
{
  return "";
}

wxString PlatformMP3Exporter::GetLibraryName()
{
  return "lame_enc.dll";
}
   
wxString PlatformMP3Exporter::GetLibraryTypeString()
{
  return _("Only lame_enc.dll|lame_enc.dll|Dynamically Linked Libraries (*.dll)|*.dll|All Files (*.*)|*");
}
   
wxString PlatformMP3Exporter::GetLibraryMessage()
{
  return _("Audacity does not export MP3 files directly, but instead uses the\n"
	   "freely available LAME library to handle MP3 file encoding.  You must\n"
	   "obtain lame_enc.dll separately, by downloading the LAME MP3 encoder,"
	   "and then locate this file for Audacity.  You only need to do this once.\n\n"
	   "Would you like to locate lame_enc.dll now?");
}


bool  PlatformMP3Exporter::LoadLibrary() {
  wxLogNull logNo;

  if (wxFileExists(mLibPath))
    {
      if(lame_enc_lib.IsLoaded())
	{
	  lame_enc_lib.Unload();
	}

      if(!lame_enc_lib.Load(mLibPath))
	{
	  return false;
	}
    }
  else
    return false;

  beInitStream = (BEINITSTREAM)lame_enc_lib.GetSymbol("beInitStream");
  beEncodeChunk = (BEENCODECHUNK)lame_enc_lib.GetSymbol("beEncodeChunk");
  beDeinitStream = (BEDEINITSTREAM)lame_enc_lib.GetSymbol("beDeinitStream");
  beCloseStream = (BECLOSESTREAM)lame_enc_lib.GetSymbol("beCloseStream");
  beVersion = (BEVERSION)lame_enc_lib.GetSymbol("beVersion");

  if(!beInitStream   ||
     !beEncodeChunk  ||
     !beDeinitStream ||
     !beCloseStream  ||
     !beVersion)
    return false;

  beVersion(&mVersion);
  mLibraryLoaded = true;
  return true;
}

bool PlatformMP3Exporter::ValidLibraryLoaded() { return mLibraryLoaded; }

wxString GetLibraryVersion() {
  BE_VERSION ver;

  if(!mLibraryLoaded)
    return "";

  beVersion(&ver);

  return wxString::Format("LAME v%d.%d", ver.byMajorVersion, ver.byMinorVersion);
}

int PlatformMP3Exporter::InitializeStream() 
{

  if(!mLibraryLoaded)
    return -1;

  //int modes[] = { 0, BE_MP3_MODE_MONO, BE_MP3_MODE_STEREO };
  //mConf.format.LHV1.dwSampleRate = sampleRate;
  //mConf.format.LHV1.nMode = modes[channels];
  int modes[] = { 0, BE_MP3_MODE_MONO, BE_MP3_MODE_STEREO };
  mConf.format.mp3.byMode = modes[mChannels];
  mConf.format.mp3.dwSampleRate = mOutRate;
  mConf.format.mp3.wBitrate = mDefaultRate;

  beInitStream(&mConf, &mInSampleNum, &mOutBufferSize, &mStreamHandle);

  mEncoding = true;

  if(mChannels == 2) {
    return(mInSampleNum / 2); /* convert samples_total into samples_per_channel */
  }
  else {
    return (mInSampleNum);
  }

}

int PlatformMP3Exporter::GetOutBufferSize() {
  if (!mEncoding)
    return -1;

  return mOutBufferSize;
}

int PlatformMP3Exporter::EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]) {
  if(!mEncoding)
    return -1;
      
  unsigned long bytes;
  beEncodeChunk(mStreamHandle, mInSampleNum, inbuffer, outbuffer, &bytes);

  return bytes;
}

int PlatformMP3Exporter::EncodeRemainder(short int inbuffer[], int nSamples, unsigned char outbuffer[]) {
  if(!mEncoding)
    return -1;

  unsigned long bytes;
  beEncodeChunk(mStreamHandle, nSamples, inbuffer, outbuffer, &bytes);

  return bytes;
}

int PlatformMP3Exporter::FinishStream(unsigned char outbuffer[]) {
  if(!mEncoding)
    return -1;

  unsigned long bytes;
  beDeinitStream(mStreamHandle, outbuffer, &bytes);
  beCloseStream(mStreamHandle);

  mEncoding = false;
  return bytes;
}

void PlatformMP3Exporter::CancelEncoding() {
  beCloseStream(mStreamHandle);
}

int PlatformMP3Exporter::GetQualityVariance() { return -1; }

int PlatformMP3Exporter::GetConfigurationCaps() {
  return MP3CONFIG_BITRATE;
}
   
void PlatformMP3Exporter::SetBitRate(int rate) { 
  mDefaultRate = rate;
}

int PlatformMP3Exporter::GetBitRate() {
  return mDefaultRate;
}

void PlatformMP3Exporter::SetQuality(int quality) { }
int PlatformMP3Exporter::GetQuality() { return -1; }


#endif  

