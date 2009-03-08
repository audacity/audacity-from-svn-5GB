/**********************************************************************

Audacity: A Digital Audio Editor

FFmpeg.cpp

Audacity(R) is copyright (c) 1999-2008 Audacity Team.
License: GPL v2.  See License.txt.

******************************************************************//**

\class FFmpegLibs
\brief Class used to dynamically load FFmpeg libraries

*//*******************************************************************/



#include "Audacity.h"	// for config*.h
#include "FFmpeg.h"
#include "AudacityApp.h"

#ifdef _DEBUG
   #ifdef _MSC_VER
      #undef THIS_FILE
      static char*THIS_FILE= __FILE__;
      #define new new(_NORMAL_BLOCK, THIS_FILE, __LINE__)
   #endif
#endif

#if !defined(USE_FFMPEG)
/// FFmpeg support may or may not be compiled in,
/// but Preferences dialog requires this function nevertheless
wxString GetFFmpegVersion(wxWindow *parent)
{
   return wxString(_("FFmpeg support not compiled in"));
}

#else

/** This pointer to the shared object has global scope and is used to track the
 * singleton object which wraps the FFmpeg codecs */
FFmpegLibs *FFmpegLibsInst = NULL;

FFmpegLibs *PickFFmpegLibs()
{
   if (FFmpegLibsInst != NULL)
   {
      FFmpegLibsInst->refcount++;
      return FFmpegLibsInst;
   }
   else
   {
      FFmpegLibsInst = new FFmpegLibs();
      return FFmpegLibsInst;
   }
}

void DropFFmpegLibs()
{
   if (FFmpegLibsInst != NULL)
   {
      FFmpegLibsInst->refcount--;
      if (FFmpegLibsInst->refcount == 0)
      {
         delete FFmpegLibsInst;
         FFmpegLibsInst = NULL;
      }
   }
}

bool LoadFFmpeg(bool showerror)
{
   PickFFmpegLibs();
   if (FFmpegLibsInst->ValidLibsLoaded())
   {
     DropFFmpegLibs();
     return true;
   }
   if (!FFmpegLibsInst->LoadLibs(NULL,showerror))
   {
      DropFFmpegLibs();
      gPrefs->Write(wxT("/FFmpeg/Enabled"), false);
      return false;
   }
   else
   {
      gPrefs->Write(wxT("/FFmpeg/Enabled"), true);
      return true;
   }
}

/** Called during Audacity start-up to try and load the ffmpeg libraries */
void FFmpegStartup()
{
   bool enabled = false;
   gPrefs->Read(wxT("/FFmpeg/Enabled"),&enabled);
   // 'false' means that no errors should be shown whatsoever
   if (enabled && !LoadFFmpeg(false))
   {
     wxMessageBox(_("FFmpeg was configured in Preferences and successfully loaded before, \
                      \nbut this time Audacity failed to load it at startup. \
                      \n\nYou may want to go back to Preferences > Import/Export and re-configure it."),
                  _("FFmpeg startup failed"));
   }
}

wxString GetFFmpegVersion(wxWindow *parent)
{
   PickFFmpegLibs();

   wxString versionString = _("FFmpeg library not found");

   if (FFmpegLibsInst->ValidLibsLoaded()) {
      versionString = FFmpegLibsInst->GetLibraryVersion();
   }

   DropFFmpegLibs();

   return versionString;
}

void av_log_wx_callback(void* ptr, int level, const char* fmt, va_list vl)
{
   //Most of this stuff is taken from FFmpeg tutorials and FFmpeg itself
   int av_log_level = AV_LOG_WARNING;
   AVClass* avc = ptr ? *(AVClass**)ptr : NULL;
   if (level > av_log_level)
      return;
   wxString printstring(wxT(""));

   if (avc) {
      printstring.Append(wxString::Format(wxT("[%s @ %p] "), wxString::FromUTF8(avc->item_name(ptr)).c_str(), avc));
   }

   wxString frm(fmt,wxConvLibc);
#if defined(__WXMSW__)
   frm.Replace(wxT("%t"),wxT("%i"),true); //TODO: on Windows vprintf won't handle %t, and probably some others. Investigate.
#endif
#if defined(wxUSE_UNICODE)
   // String comes with %s format field and a value in value list is ascii char*. Thus in Unicode configurations
   // we have to convert %s to %S.
   frm.Replace(wxT("%s"),wxT("%S"),true);
#endif
   printstring.Append(wxString::FormatV(frm,vl));
   wxString cpt;
   switch (level)
   {
   case 0: cpt = wxT("Error"); break;
   case 1: cpt = wxT("Info"); break;
   case 2: cpt = wxT("Debug"); break;
   default: cpt = wxT("Log"); break;
   }
   wxLogMessage(wxT("%s: %s"),cpt.c_str(),printstring.c_str());
}

#if defined(__WXMSW__)
//======================= UTF8-aware uri protocol for FFmpeg
// Code is from ffmpeg-users mailing list mostly

static int ufile_open(URLContext *h, const char *filename, int flags)
{
    int access;
    int fd;

    FFmpegLibsInst->av_strstart(filename, "ufile:", &filename);

    /// 4096 should be enough for a path name
    wchar_t wfilename[4096];
    int nChars = MultiByteToWideChar(
        CP_UTF8,
        MB_ERR_INVALID_CHARS,
        filename,
        -1,    // string is NULL terminated
        wfilename,
        sizeof(wfilename) / sizeof(*wfilename)
        );

    if(nChars <= 0) {
        return AVERROR(ENOENT);
    }

    if (flags & URL_RDWR) {
        access = _O_CREAT | _O_TRUNC | _O_RDWR;
    } else if (flags & URL_WRONLY) {
        access = _O_CREAT | _O_TRUNC | _O_WRONLY;
    } else {
        access = _O_RDONLY;
    }
#ifdef O_BINARY
    access |= O_BINARY;
#endif
    fd = _wopen(wfilename, access, 0666);
    h->priv_data = (void *)(size_t)fd;
    if (fd < 0) {
        const int err = AVERROR(ENOENT);
        assert (err < 0);
        return err;
    }
    return 0;
}

static int ufile_read(URLContext *h, unsigned char *buf, int size)
{
    int fd = (size_t)h->priv_data;
    int nBytes = _read(fd, buf, size);
    return nBytes;
}

static int ufile_write(URLContext *h, unsigned char *buf, int size)
{
    int fd = (size_t)h->priv_data;
    int nBytes = _write(fd, buf, size);
    return nBytes;
}

#if LIBAVFORMAT_VERSION_MAJOR >= 52
static int64_t ufile_seek(URLContext *h, int64_t pos, int whence)
#else
static offset_t ufile_seek(URLContext *h, offset_t pos, int whence)
#endif
{
    //assert(whence == SEEK_SET || whence == SEEK_CUR || whence == SEEK_END);
    const int fd = (size_t)h->priv_data;
    const __int64 nBytes = _lseeki64(fd, pos, whence);
    return nBytes;
}

static int ufile_close(URLContext *h)
{
    int fd = (size_t)h->priv_data;
    if(fd >= 0) {
        return _close(fd);
    }

    return 0;
}

URLProtocol ufile_protocol = {
    "ufile",
    ufile_open,
    ufile_read,
    ufile_write,
    ufile_seek,
    ufile_close,
};

int modify_file_url_to_utf8(char* buffer, size_t buffer_size, const char* url)
{
    strncpy(buffer, "ufile:", buffer_size);
    strncat(buffer, url, buffer_size);
    return 0;
}

int modify_file_url_to_utf8(char* buffer, size_t buffer_size, const wchar_t* url)
{
    static const char ufile[] = "ufile:";
    strncpy(buffer, ufile, buffer_size);

    /// convert Unicode to multi-byte string
    int result = WideCharToMultiByte(CP_UTF8,
        0, url, -1, buffer + (sizeof(ufile) - 1),
        buffer_size - sizeof(ufile),
        NULL, NULL);
    if (result <= 0)  
        return -1;

    return 0;
}
#endif


class FFmpegNotFoundDialog;

//----------------------------------------------------------------------------
// FindFFmpegDialog
//----------------------------------------------------------------------------

#define ID_FFMPEG_BROWSE 5000
#define ID_FFMPEG_DLOAD  5001

/// Allows user to locate libav* libraries
class FindFFmpegDialog : public wxDialog
{
public:

   FindFFmpegDialog(wxWindow *parent, wxString path, wxString name, wxString type)
      :  wxDialog(parent, wxID_ANY, wxString(_("Locate FFmpeg")))
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
         text.Printf(_("Audacity needs the file %s to import and export audio via FFmpeg."), mName.c_str());
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
            S.Id(ID_FFMPEG_BROWSE).AddButton(_("Browse..."), wxALIGN_RIGHT);
            S.AddVariableText(_("To get a free copy of FFmpeg, click here -->"), true);
            S.Id(ID_FFMPEG_DLOAD).AddButton(_("Download"), wxALIGN_RIGHT);
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
         wxFD_OPEN | wxRESIZE_BORDER,
         this);
      if (!path.IsEmpty()) {
         mLibPath = path;
         mPathText->SetValue(path);
      }
   }

   void OnDownload(wxCommandEvent & event)
   {
      wxString page = wxT("http://www.audacityteam.org/manual/index.php?title=FAQ:Installation_and_Plug-Ins%23installffmpeg");
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

BEGIN_EVENT_TABLE(FindFFmpegDialog, wxDialog)
   EVT_BUTTON(ID_FFMPEG_BROWSE, FindFFmpegDialog::OnBrowse)
   EVT_BUTTON(ID_FFMPEG_DLOAD,  FindFFmpegDialog::OnDownload)
END_EVENT_TABLE()


//----------------------------------------------------------------------------
// FFmpegNotFoundDialog
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(FFmpegNotFoundDialog, wxDialog)
   EVT_BUTTON(wxID_OK, FFmpegNotFoundDialog::OnOk)
END_EVENT_TABLE()


//----------------------------------------------------------------------------
// FFmpegLibs
//----------------------------------------------------------------------------

FFmpegLibs::FFmpegLibs()
{
   mLibsLoaded = false;
   refcount = 1;
   avformat = avcodec = avutil = NULL;
   if (gPrefs) {
      mLibAVFormatPath = gPrefs->Read(wxT("/FFmpeg/FFmpegLibPath"), wxT(""));
   }

}

FFmpegLibs::~FFmpegLibs()
{
   FreeLibs();
};

bool FFmpegLibs::FindLibs(wxWindow *parent)
{
   wxString path;
   wxString name;

   wxLogMessage(wxT("Looking for FFmpeg libraries..."));
   if (!mLibAVFormatPath.IsEmpty()) {
      wxLogMessage(wxT("mLibAVFormatPath is not empty, = %s"),mLibAVFormatPath.c_str());
      wxFileName fn = mLibAVFormatPath;
      path = fn.GetPath();
      name = fn.GetFullName();
   }
   else {
      path = GetLibAVFormatPath();
      name = GetLibAVFormatName();
      wxLogMessage(wxT("mLibAVFormatPath is empty, starting with path '%s', name '%s'"),path.c_str(),name.c_str());
   }

   FindFFmpegDialog fd(parent,
      path,
      name,
      GetLibraryTypeString());

   if (fd.ShowModal() == wxID_CANCEL) {
      wxLogMessage(wxT("User canceled the dialog. Failed to find libraries."));
      return false;
   }

   path = fd.GetLibPath();

   wxLogMessage(wxT("User-specified path = %s"),path.c_str());
   if (!::wxFileExists(path)) {
      wxLogMessage(wxT("User-specified file doesn't exist! Failed to find libraries."));
      return false;
   }
   wxLogMessage(wxT("User-specified file exists. Success."));
   mLibAVFormatPath = path;
   gPrefs->Write(wxT("/FFmpeg/FFmpegLibPath"), mLibAVFormatPath);

   return true;
}

bool FFmpegLibs::LoadLibs(wxWindow *parent, bool showerr)
{

   wxLogMessage(wxT("Trying to load FFmpeg libraries"));
   if (ValidLibsLoaded()) {
      wxLogMessage(wxT("Libraries already loaded - freeing"));
      FreeLibs();
   }

   // First try loading it from a previously located path
   if (!mLibAVFormatPath.IsEmpty()) {
      wxLogMessage(wxT("mLibAVFormatPath is not empty, = %s. Loading from it."),mLibAVFormatPath.c_str());
      mLibsLoaded = InitLibs(mLibAVFormatPath,showerr);
   }

   // If not successful, try loading it from default path
   if (!mLibsLoaded && !GetLibAVFormatPath().IsEmpty()) {
      wxFileName fn(GetLibAVFormatPath(), GetLibAVFormatName());
      wxString path = fn.GetFullPath();
      wxLogMessage(wxT("Trying to load from default path %s."),path.c_str());
      mLibsLoaded = InitLibs(path,showerr);
      if (mLibsLoaded) {
         mLibAVFormatPath = path;
      }
   }

   // If not successful, try loading using system search paths
   if (!ValidLibsLoaded()) {
      wxString path = GetLibAVFormatName();
      wxLogMessage(wxT("Trying to load from system paths. File name is %s"),path.c_str());
      mLibsLoaded = InitLibs(path,showerr);
      if (mLibsLoaded) {
         mLibAVFormatPath = path;
      }
   }

   // If libraries aren't loaded - nag user about that
   /*
   if (!ValidLibsLoaded())
   {
      wxLogMessage(wxT("Failed to load libraries altogether."));
      int dontShowDlg;
      FFmpegNotFoundDialog *dlg;
      gPrefs->Read(wxT("/FFmpeg/NotFoundDontShow"),&dontShowDlg,0);
      if ((dontShowDlg == 0) && (showerr))
      {
          dlg = new FFmpegNotFoundDialog(NULL);
          dlg->ShowModal();
          delete dlg;
      }
   }
   */
   // Oh well, just give up
   if (!ValidLibsLoaded()) {
      if (showerr) wxMessageBox(_("Failed to find compatible FFmpeg libraries"));
      return false;
   }

   wxLogMessage(wxT("Libraries loaded successfully!"));
   return true;
}

bool FFmpegLibs::ValidLibsLoaded()
{
   return mLibsLoaded;
}

bool FFmpegLibs::InitLibs(wxString libpath_format, bool showerr)
{
   // Initially we don't know where are the avcodec and avutl libs
   wxDynamicLibrary *codec = NULL;
   wxDynamicLibrary *util = NULL;
   wxFileName name(libpath_format);

   wxLogWindow* mLogger = wxGetApp().mLogger;

   bool gotError = false;

   wxString syspath;
   bool pathfix = false;

   FreeLibs();

#if defined(__WXMSW__)
   wxLogMessage(wxT("Looking up PATH..."));
   // First take PATH environment variable (store it's content)
   if (wxGetEnv(wxT("PATH"),&syspath))
   {
      wxLogMessage(wxT("PATH = %s"),syspath.c_str());
      wxString fmtdirsc = wxPathOnly(libpath_format) + wxT(";");
      wxString scfmtdir = wxT(";") + wxPathOnly(libpath_format);
      wxString fmtdir = wxPathOnly(libpath_format);
      wxLogMessage(wxT("Checking that %s is in PATH..."),fmtdir.c_str());
      // If the directory, where libavformat is, is not in PATH - add it
      if (!syspath.Contains(fmtdirsc) && !syspath.Contains(scfmtdir) && !syspath.Contains(fmtdir))
      {
         wxLogMessage(wxT("not in PATH!"));
         if (syspath.Last() == wxT(';'))
         {
            wxLogMessage(wxT("Appending %s ..."),fmtdir.c_str());
            syspath.Append(fmtdirsc);
         }
         else
         {
            wxLogMessage(wxT("Appending %s ..."),scfmtdir.c_str());
            syspath.Append(scfmtdir);
         }

         if (wxSetEnv(wxT("PATH"),syspath.c_str()))
         {
            // Remember to change PATH back to normal after we're done
            pathfix = true;
         }
         else
         {
            wxLogMessage(wxT("wxSetEnv(%s) failed."),syspath.c_str());
         }
      }
      else
      {
         wxLogMessage(wxT("in PATH."));
      }
   }
   else
   {
      wxLogMessage(wxT("PATH does not exist."));
   }
#endif

   //Load libavformat
   avformat = new wxDynamicLibrary();
   wxLogMessage(wxT("Loading avformat from %s"),libpath_format.c_str());
   if (showerr)
      mLogger->SetActiveTarget(NULL);
   gotError = !avformat->Load(libpath_format, wxDL_LAZY);
   if (showerr)
      mLogger->SetActiveTarget(mLogger);

   if (!gotError) {
      if (avformat->HasSymbol(wxT("av_free"))) {
         util = avformat;
      }
      if (avformat->HasSymbol(wxT("avcodec_init"))) {
         codec = avformat;
      }
   }

   if (!util) {
      name.SetFullName(GetLibAVUtilName());
      avutil = util =  new wxDynamicLibrary();
      wxLogMessage(wxT("Loading avutil from %s"),name.GetFullPath().c_str());
      if (showerr)
         mLogger->SetActiveTarget(NULL);
      util->Load(name.GetFullPath(), wxDL_LAZY);
      if (showerr)
         mLogger->SetActiveTarget(mLogger);
   }

   if (!codec) {
      name.SetFullName(GetLibAVCodecName());
      avcodec = codec = new wxDynamicLibrary();
      wxLogMessage(wxT("Loading avcodec from %s"),name.GetFullPath().c_str());
      if (showerr)
         mLogger->SetActiveTarget(NULL);
      codec->Load(name.GetFullPath(), wxDL_LAZY);
      if (showerr)
         mLogger->SetActiveTarget(mLogger);
   }

   if (!avformat->IsLoaded()) {
      if (showerr)
         mLogger->SetActiveTarget(NULL);
      gotError = !avformat->Load(libpath_format, wxDL_LAZY);
      if (showerr)
         mLogger->SetActiveTarget(mLogger);
   }

   //Return PATH to normal
   if ( pathfix )
   {
      wxString oldpath = syspath.BeforeLast(wxT(';'));
      wxLogMessage(wxT("Returning PATH to normal..."));
      wxSetEnv(wxT("PATH"),oldpath.c_str());
   }

   if (gotError) {
      wxLogMessage(wxT("Failed to load FFmpeg libs"));
      FreeLibs();
      return false;
   }

   wxLogMessage(wxT("Importing symbols..."));
   INITDYN(avformat,av_register_all);
   INITDYN(avformat,av_open_input_file);
   INITDYN(avformat,av_find_stream_info);
   INITDYN(avformat,av_read_frame);
   INITDYN(avformat,av_seek_frame);
   INITDYN(avformat,av_close_input_file);
   INITDYN(avformat,av_index_search_timestamp);
   INITDYN(avformat,av_write_header);
   INITDYN(avformat,av_interleaved_write_frame);
   INITDYN(avformat,av_write_frame);
   INITDYN(avformat,av_iformat_next);
   INITDYN(avformat,av_oformat_next);
   INITDYN(avformat,av_set_parameters);
#if LIBAVFORMAT_VERSION_MAJOR < 53
   INITDYN(avformat,register_protocol);
   av_register_protocol = register_protocol;
#else
   INITDYN(avformat,av_register_protocol);
#endif
   INITDYN(avformat,url_fopen);
   INITDYN(avformat,url_fclose);
   INITDYN(avformat,url_fsize);
   INITDYN(avformat,av_new_stream);
   INITDYN(avformat,av_alloc_format_context);
   INITDYN(avformat,guess_format);
   INITDYN(avformat,av_write_trailer);
   INITDYN(avformat,av_init_packet);
   INITDYN(avformat,av_codec_get_id);
   INITDYN(avformat,av_codec_get_tag);
   INITDYN(avformat,avformat_version);

   INITDYN(codec,avcodec_init);
   INITDYN(codec,avcodec_find_encoder);
   INITDYN(codec,avcodec_find_encoder_by_name);
   INITDYN(codec,avcodec_find_decoder);
   INITDYN(codec,avcodec_find_decoder_by_name);
   INITDYN(codec,avcodec_string);
   INITDYN(codec,avcodec_get_context_defaults);
   INITDYN(codec,avcodec_alloc_context);
   INITDYN(codec,avcodec_get_frame_defaults);
   INITDYN(codec,avcodec_alloc_frame);
   INITDYN(codec,avcodec_open);
   INITDYN(codec,avcodec_decode_audio2);
   INITDYN(codec,avcodec_encode_audio);
   INITDYN(codec,avcodec_close);
   INITDYN(codec,avcodec_register_all);
   INITDYN(codec,avcodec_flush_buffers);
   INITDYN(codec,av_get_bits_per_sample);
   INITDYN(codec,av_get_bits_per_sample_format);
   INITDYN(codec,avcodec_version);
   INITDYN(codec,av_fast_realloc);
   INITDYN(codec,av_codec_next);

   INITDYN(util,av_free);
   INITDYN(util,av_log_set_callback);
   INITDYN(util,av_log_default_callback);
   INITDYN(util,av_fifo_init);
   INITDYN(util,av_fifo_free);
   INITDYN(util,av_fifo_read);
   INITDYN(util,av_fifo_size);
   INITDYN(util,av_fifo_generic_write);
   INITDYN(util,av_fifo_realloc);
   INITDYN(util,av_malloc);
   INITDYN(util,av_freep);
   INITDYN(util,av_rescale_q);
   INITDYN(util,av_strstart);
   INITDYN(util,avutil_version);

   //FFmpeg initialization
   wxLogMessage(wxT("All symbols loaded successfully. Initializing the library."));
   this->avcodec_init();
   this->avcodec_register_all();
   this->av_register_all();
   
   wxLogMessage(wxT("Retrieving library version."));
   int avcver = this->avcodec_version();
   int avfver = this->avformat_version();
   int avuver = this->avutil_version();
   mAVCodecVersion = wxString::Format(wxT("%d.%d.%d"),avcver >> 16 & 0xFF, avcver >> 8 & 0xFF, avcver & 0xFF);
   mAVFormatVersion = wxString::Format(wxT("%d.%d.%d"),avfver >> 16 & 0xFF, avfver >> 8 & 0xFF, avfver & 0xFF);
   mAVUtilVersion = wxString::Format(wxT("%d.%d.%d"),avuver >> 16 & 0xFF, avuver >> 8 & 0xFF, avuver & 0xFF);

   wxLogMessage(wxT("AVCodec version 0x%06x - %s (built against 0x%06x - %s)"),avcver,mAVCodecVersion.c_str(),LIBAVCODEC_VERSION_INT,wxString::FromUTF8(AV_STRINGIFY(LIBAVCODEC_VERSION)).c_str());
   wxLogMessage(wxT("AVFormat version 0x%06x - %s (built against 0x%06x - %s)"),avfver,mAVFormatVersion.c_str(),LIBAVFORMAT_VERSION_INT,wxString::FromUTF8(AV_STRINGIFY(LIBAVFORMAT_VERSION)).c_str());
   wxLogMessage(wxT("AVUtil version 0x%06x - %s (built against 0x%06x - %s)"),avuver,mAVUtilVersion.c_str(),LIBAVUTIL_VERSION_INT,wxString::FromUTF8(AV_STRINGIFY(LIBAVUTIL_VERSION)).c_str());

   int avcverdiff = (avcver >> 16 & 0xFF) - int(LIBAVCODEC_VERSION_MAJOR);
   int avfverdiff = (avfver >> 16 & 0xFF) - int(LIBAVFORMAT_VERSION_MAJOR);
   int avuverdiff = (avuver >> 16 & 0xFF) - int(LIBAVUTIL_VERSION_MAJOR);
   wxLogMessage(wxT("AVCodec version mismatch is %d"),avcverdiff);
   wxLogMessage(wxT("AVFormat version mismatch is %d"),avfverdiff);
   wxLogMessage(wxT("AVUtil version mismatch is %d"),avuverdiff);
   //make sure that header and library major versions are the same
   if (avcverdiff != 0 || avfverdiff != 0 || avuverdiff != 0)
   {
      wxLogMessage(wxT("Version mismatch! Libraries are unusable."));
      return false;
   }

   av_register_protocol(&ufile_protocol);

   return true;
}

void FFmpegLibs::FreeLibs()
{
   if (avformat != NULL) {
      delete avformat;
      avformat = NULL;
   }

   if (avcodec != NULL) {
      delete avcodec;
      avcodec = NULL;
   }

   if (avutil != NULL) {
      delete avutil;
      avutil = NULL;
   }

   mLibsLoaded = false;

   return;
}

#endif //USE_FFMPEG
