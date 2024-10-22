/**********************************************************************

Audacity: A Digital Audio Editor

FFmpeg.cpp

Audacity(R) is copyright (c) 1999-2009 Audacity Team.
License: GPL v2.  See License.txt.

******************************************************************//**

\class FFmpegLibs
\brief Class used to dynamically load FFmpeg libraries

*//*******************************************************************/

// Store function pointers here when including FFmpeg.h
#define DEFINE_FFMPEG_POINTERS

#include "Audacity.h"	// for config*.h
#include "FFmpeg.h"
#include "AudacityApp.h"
#include "FileNames.h"
#include "Internat.h"

#include <wx/file.h>

#ifdef _DEBUG
   #ifdef _MSC_VER
      #undef THIS_FILE
      static char*THIS_FILE= __FILE__;
      #define new new(_NORMAL_BLOCK, THIS_FILE, __LINE__)
   #endif
#endif

#define UFILE_PROTOCOL "ufile"

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
      gPrefs->Flush();
      return false;
   }
   else
   {
      gPrefs->Write(wxT("/FFmpeg/Enabled"), true);
      gPrefs->Flush();
      return true;
   }
}

/** Called during Audacity start-up to try and load the ffmpeg libraries */
void FFmpegStartup()
{
   bool enabled = false;
   gPrefs->Read(wxT("/FFmpeg/Enabled"),&enabled);
   // 'false' means that no errors should be shown whatsoever
   if (!LoadFFmpeg(false))
   {
      if (enabled)
      {
         wxMessageBox(_("FFmpeg was configured in Preferences and successfully loaded before, \
                        \nbut this time Audacity failed to load it at startup. \
                        \n\nYou may want to go back to Preferences > Libraries and re-configure it."),
                      _("FFmpeg startup failed"));
      }
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
#ifdef EXPERIMENTAL_OD_FFMPEG
//if the decoding happens thru OD then this gets called from a non main thread, which means wxLogDebug
//will crash.  
//TODO:find some workaround for the log.  perhaps use ODManager as a bridge. for now just print
   if(!wxThread::IsMain())
      printf("%s: %s\n",(char*)cpt.char_str(),(char*)printstring.char_str());
   else
#endif
      wxLogDebug(wxT("%s: %s"),cpt.c_str(),printstring.c_str());
}

//======================= Unicode aware uri protocol for FFmpeg
// Code inspired from ffmpeg-users mailing list sample

static int ufile_open(URLContext *h, const char *filename, int flags)
{
   wxString name(strchr(filename, ':') + 1, wxConvUTF8);
   wxFile *f;
   wxFile::OpenMode mode;

   f = new wxFile;
   if (!f) {
      return AVERROR(ENOMEM);
   }

   // LLL:  These really should be logical AND tests, but on 2011/04/28, the URL_ open flags
   //       changed in the FFmpeg source to values that were not compatible with previous
   //       values.
   //
   //       Since Audacity doesn't use any other open flags (there aren't any others defined
   //       anyway), making equality tests works for older and new FFmpeg headers.
   if (flags == URL_RDWR) {
      mode = wxFile::read_write;
   } else if (flags == URL_WRONLY) {
      mode = wxFile::write;
   } else {
      mode = wxFile::read;
   }

   if (!f->Open(name, mode)) {
      delete f;
      return AVERROR(ENOENT);
   }

   h->priv_data = (void *)f;

   return 0;
}

static int ufile_read(URLContext *h, unsigned char *buf, int size)
{
   return (int) ((wxFile *) h->priv_data)->Read(buf, size);
}

#if LIBAVFORMAT_VERSION_INT < AV_VERSION_INT(52, 68, 0)
static int ufile_write(URLContext *h, unsigned char *buf, int size)
#else
static int ufile_write(URLContext *h, const unsigned char *buf, int size)
#endif
{
   return (int) ((wxFile *) h->priv_data)->Write(buf, size);
}

static int64_t ufile_seek(URLContext *h, int64_t pos, int whence)
{
   wxSeekMode mode = wxFromStart;

#if !defined(AVSEEK_FORCE)
#define AVSEEK_FORCE 0
#endif

   switch (whence & ~AVSEEK_FORCE)
   {
   case (SEEK_SET):
     mode = wxFromStart;
     break;
   case (SEEK_CUR):
     mode = wxFromCurrent;
     break;
   case (SEEK_END):
     mode = wxFromEnd;
     break;
   case (AVSEEK_SIZE):
     return ((wxFile *) h->priv_data)->Length();
   }

   return ((wxFile *) h->priv_data)->Seek(pos, mode);
}

static int ufile_close(URLContext *h)
{
   wxFile *f = (wxFile *) h->priv_data;

   if (f) {
      f->Close();
      delete f;
   }

    return 0;
}

URLProtocol ufile_protocol = {
    UFILE_PROTOCOL,
    ufile_open,
    ufile_read,
    ufile_write,
    ufile_seek,
    ufile_close,
};

// Open a file with a (possibly) Unicode filename
int ufile_fopen(AVIOContext **s, const wxString & name, int flags)
{
   wxString url(wxString(wxT(UFILE_PROTOCOL)) + wxT(":") + name);
   URLContext *h;
   int err;

   // Open the file using our custom protocol and passing the (possibly) Unicode
   // filename.  We convert the name to UTF8 here and it will be converted back
   // to original encoding in ufile_open().  This allows us to support Unicode
   // filenames even though FFmpeg does not.
   err = url_open(&h, (const char *) url.ToUTF8(), flags);
   if (err < 0) {
      return err;
   }

   // Associate the file with a context
   err = url_fdopen(s, h);
   if (err < 0) {
      url_close(h);
      return err;
   }

   return 0;
}


// Size of probe buffer, for guessing file type from file contents
#define PROBE_BUF_MIN 2048
#define PROBE_BUF_MAX (1<<20)

// Detect type of input file and open it if recognized. Routine
// based on the av_open_input_file() libavformat function.
int ufile_fopen_input(AVFormatContext **ic_ptr, wxString & name)
{
   wxFileName f(name);
   wxCharBuffer fname;
   const char *filename;
   AVProbeData pd;
   AVIOContext *pb = NULL;
   AVInputFormat *fmt = NULL;
   AVInputFormat *fmt1;
   int probe_size;
   int err;

   // Create a dummy file name using the extension from the original
   f.SetName(wxT(UFILE_PROTOCOL));
   fname = f.GetFullName().mb_str();
   filename = (const char *) fname;

   // Initialize probe data...go ahead and preallocate the maximum buffer size.
   pd.filename = filename;
   pd.buf_size = 0;
   pd.buf = (unsigned char *) av_malloc(PROBE_BUF_MAX + AVPROBE_PADDING_SIZE);
   if (pd.buf == NULL) {
      err = AVERROR(ENOMEM);
      goto fail;
   }

   // Open the file to prepare for probing
   if ((err = ufile_fopen(&pb, name, URL_RDONLY)) < 0) {
      goto fail;
   }

   for (probe_size = PROBE_BUF_MIN; probe_size <= PROBE_BUF_MAX && !fmt; probe_size <<= 1) {
      int score_max = probe_size < PROBE_BUF_MAX ? AVPROBE_SCORE_MAX / 4 : 0;

      // Read up to a "probe_size" worth of data
      pd.buf_size = avio_read(pb, pd.buf, probe_size);

      // AWD: with zero-length input files buf_size can come back negative;
      // this causes problems so we might as well just fail
      if (pd.buf_size < 0) {
         err = AVERROR_INVALIDDATA;
         goto fail;
      }

      // Clear up to a "AVPROBE_PADDING_SIZE" worth of unused buffer
      memset(pd.buf + pd.buf_size, 0, AVPROBE_PADDING_SIZE);

      // Reposition file for succeeding scan
      if (avio_seek(pb, 0, SEEK_SET) < 0) {
         err = AVERROR(EIO);
         goto fail;
      }

      // Scan all input formats
      fmt = NULL;
      for (fmt1 = av_iformat_next(NULL); fmt1 != NULL; fmt1 = av_iformat_next(fmt1)) {
         int score = 0;

         // Ignore the ones that are not file based
         if (fmt1->flags & AVFMT_NOFILE) {
            continue;
         }

         // If the format can probe the file then try that first
         if (fmt1->read_probe) {
            score = fmt1->read_probe(&pd);
         }
         // Otherwize, resort to extension matching if available
         else if (fmt1->extensions) {
            if (av_match_ext(filename, fmt1->extensions)) {
               score = 50;
            }
         }

         // Remember this format if it scored higher than a previous match
         if (score > score_max) {
            score_max = score;
            fmt = fmt1;
         }
         else if (score == score_max) {
            fmt = NULL;
         }
      }
   }

   // Didn't find a suitable format, so bail
   if (!fmt) {
      err = AVERROR(EILSEQ);
      goto fail;
   }

   // And finally, attempt to associate an input stream with the file
   err = av_open_input_stream(ic_ptr, pb, filename, fmt, NULL);
   if (err) {
      goto fail;
   }

   // Done with the probe buffer
   av_freep(&pd.buf);

   return 0;

fail:
   if (pd.buf) {
      av_freep(&pd.buf);
   }

   if (pb) {
      avio_close(pb);
   }

   *ic_ptr = NULL;

   return err;
}

streamContext *import_ffmpeg_read_next_frame(AVFormatContext* formatContext,
                                             streamContext** streams,
                                             unsigned int numStreams)
{
   streamContext *sc = NULL;
   AVPacket pkt;

   if (av_read_frame(formatContext,&pkt) < 0)
   {
      return NULL;
   }

   // Find a stream to which this frame belongs to
   for (unsigned int i = 0; i < numStreams; i++)
   {
      if (streams[i]->m_stream->index == pkt.stream_index)
         sc = streams[i];
   }

   // Off-stream packet. Don't panic, just skip it.
   // When not all streams are selected for import this will happen very often.
   if (sc == NULL)
   {
      av_free_packet(&pkt);
      return (streamContext*)1;
   }

   // Copy the frame to the stream context
   memcpy(&sc->m_pkt, &pkt, sizeof(AVPacket));

   sc->m_pktValid = 1;
   sc->m_pktDataPtr = pkt.data;
   sc->m_pktRemainingSiz = pkt.size;

   return sc;
}

int import_ffmpeg_decode_frame(streamContext *sc, bool flushing)
{
   int      nBytesDecoded;          
   wxUint8 *pDecode = sc->m_pktDataPtr;
   int      nDecodeSiz = sc->m_pktRemainingSiz;

   sc->m_frameValid = 0;

   if (flushing)
   {
      // If we're flushing the decoders we don't actually have any new data to decode.
      pDecode = NULL;
      nDecodeSiz = 0;
   }
   else
   {
      if (!sc->m_pktValid || (sc->m_pktRemainingSiz <= 0))
      {
         //No more data
         return -1;
      }
   }

   sc->m_samplefmt = sc->m_codecCtx->sample_fmt;
   sc->m_samplesize = av_get_bits_per_sample_format(sc->m_samplefmt) / 8;

   unsigned int newsize = FFMAX(sc->m_pkt.size * sc->m_samplesize, AVCODEC_MAX_AUDIO_FRAME_SIZE);
   // Reallocate the audio sample buffer if it's smaller than the frame size.
   if (newsize > sc->m_decodedAudioSamplesSiz )
   {
      if (sc->m_decodedAudioSamples)
      {
         av_free(sc->m_decodedAudioSamples);
      }

      sc->m_decodedAudioSamples = (uint8_t *) av_malloc(newsize);
      sc->m_decodedAudioSamplesSiz = newsize;

      if (sc->m_decodedAudioSamples == NULL)
      {
         //Can't allocate bytes
         return -1;
      }
   }


   sc->m_decodedAudioSamplesValidSiz = sc->m_decodedAudioSamplesSiz;

#if LIBAVCODEC_VERSION_INT > AV_VERSION_INT(52, 25, 0)
   // avcodec_decode_audio3() expects the size of the output buffer as the 3rd parameter but
   // also returns the number of bytes it decoded in the same parameter.
   AVPacket avpkt;
   av_init_packet(&avpkt);
   avpkt.data = pDecode;
   avpkt.size = nDecodeSiz;
   nBytesDecoded =
      avcodec_decode_audio3(sc->m_codecCtx,
                            (int16_t *)sc->m_decodedAudioSamples,    // out
                            &sc->m_decodedAudioSamplesValidSiz,      // in/out
                            &avpkt);                                 // in
#else
   // avcodec_decode_audio2() expects the size of the output buffer as the 3rd parameter but
   // also returns the number of bytes it decoded in the same parameter.
   nBytesDecoded =
      avcodec_decode_audio2(sc->m_codecCtx, 
                            (int16_t *) sc->m_decodedAudioSamples,   // out
                            &sc->m_decodedAudioSamplesValidSiz,      // in/out
                            pDecode,                                 // in
                            nDecodeSiz);                             // in
#endif
   if (nBytesDecoded < 0)
   {
      // Decoding failed. Don't stop.
      return -1;
   }

   // We may not have read all of the data from this packet. If so, the user can call again.
   // Whether or not they do depends on if m_pktRemainingSiz == 0 (they can check).
   sc->m_pktDataPtr += nBytesDecoded;
   sc->m_pktRemainingSiz -= nBytesDecoded;

   // At this point it's normally safe to assume that we've read some samples. However, the MPEG
   // audio decoder is broken. If this is the case then we just return with m_frameValid == 0
   // but m_pktRemainingSiz perhaps != 0, so the user can call again.
   if (sc->m_decodedAudioSamplesValidSiz > 0)
   {
      sc->m_frameValid = 1;
   }
   return 0;
}



/*******************************************************/

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
         text.Printf(_("Audacity needs the file '%s' to import and export audio via FFmpeg."), mName.c_str());
         S.AddTitle(text);

         S.SetBorder(3);
         S.StartHorizontalLay(wxALIGN_LEFT, true);
         {
            text.Printf(_("Location of '%s':"), mName.c_str());
            S.AddTitle(text);
         }
         S.EndHorizontalLay();

         S.StartMultiColumn(2, wxEXPAND);
         S.SetStretchyCol(0);
         {
            if (mLibPath.GetFullPath().IsEmpty()) {
               text.Printf(_("To find '%s', click here -->"), mName.c_str());
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
      "Where would I find the file '%s'?" instead if you want. */
      question.Printf(_("Where is '%s'?"), mName.c_str());

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
      wxLogMessage(wxT("mLibAVFormatPath ('%s') is not empty."), mLibAVFormatPath.c_str());
      wxFileName fn = mLibAVFormatPath;
      path = fn.GetPath();
      name = fn.GetFullName();
   }
   else {
      path = GetLibAVFormatPath();
      name = GetLibAVFormatName();
      wxLogMessage(wxT("mLibAVFormatPath is empty, starting with path '%s', name '%s'."), 
                  path.c_str(), name.c_str());
   }

   FindFFmpegDialog fd(parent,
                        path,
                        name,
                        GetLibraryTypeString());

   if (fd.ShowModal() == wxID_CANCEL) {
      wxLogMessage(wxT("User canceled the dialog. Failed to find FFmpeg libraries."));
      return false;
   }

   path = fd.GetLibPath();

   wxLogMessage(wxT("User-specified path = '%s'"), path.c_str());
   if (!::wxFileExists(path)) {
      wxLogError(wxT("User-specified file does not exist. Failed to find FFmpeg libraries."));
      return false;
   }
   wxLogMessage(wxT("User-specified FFmpeg file exists. Success."));
   mLibAVFormatPath = path;
   gPrefs->Write(wxT("/FFmpeg/FFmpegLibPath"), mLibAVFormatPath);
   gPrefs->Flush();

   return true;
}

bool FFmpegLibs::LoadLibs(wxWindow *parent, bool showerr)
{
#if defined(DISABLE_DYNAMIC_LOADING_FFMPEG)
   mLibsLoaded = InitLibs(wxEmptyString, showerr);
   return mLibsLoaded;
#endif

   wxLogMessage(wxT("Trying to load FFmpeg libraries..."));
   if (ValidLibsLoaded()) {
      wxLogMessage(wxT("FFmpeg libraries are already loaded."));
      FreeLibs();
   }

   // First try loading it from a previously located path
   if (!mLibAVFormatPath.IsEmpty()) {
      wxLogMessage(wxT("mLibAVFormatPath ('%s') is not empty. Loading from it."),mLibAVFormatPath.c_str());
      mLibsLoaded = InitLibs(mLibAVFormatPath,showerr);
   }

   // If not successful, try loading it from default path
   if (!mLibsLoaded && !GetLibAVFormatPath().IsEmpty()) {
      wxFileName fn(GetLibAVFormatPath(), GetLibAVFormatName());
      wxString path = fn.GetFullPath();
      wxLogMessage(wxT("Trying to load FFmpeg libraries from default path, '%s'."), path.c_str());
      mLibsLoaded = InitLibs(path,showerr);
      if (mLibsLoaded) {
         mLibAVFormatPath = path;
      }
   }
   
#if defined(__WXMAC__)
   // If not successful, try loading it from legacy path
   if (!mLibsLoaded && !GetLibAVFormatPath().IsEmpty()) {
      wxFileName fn(wxT("/usr/local/lib/audacity"), GetLibAVFormatName());
      wxString path = fn.GetFullPath();
      wxLogMessage(wxT("Trying to load FFmpeg libraries from legacy path, '%s'."), path.c_str());
      mLibsLoaded = InitLibs(path,showerr);
      if (mLibsLoaded) {
         mLibAVFormatPath = path;
      }
   }
#endif

   // If not successful, try loading using system search paths
   if (!ValidLibsLoaded()) {
      wxString path = GetLibAVFormatName();
      wxLogMessage(wxT("Trying to load FFmpeg libraries from system paths. File name is '%s'."), path.c_str());
      mLibsLoaded = InitLibs(path,showerr);
      if (mLibsLoaded) {
         mLibAVFormatPath = path;
      }
   }

   // If libraries aren't loaded - nag user about that
   /*
   if (!ValidLibsLoaded())
   {
      wxLogError(wxT("Failed to load libraries altogether."));
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
      wxString msg = _("Failed to find compatible FFmpeg libraries.");
      if (showerr) 
         wxMessageBox(msg);
      wxLogError(msg);
      return false;
   }

   wxLogMessage(wxT("FFmpeg libraries loaded successfully."));
   return true;
}

bool FFmpegLibs::ValidLibsLoaded()
{
   return mLibsLoaded;
}

bool FFmpegLibs::InitLibs(wxString libpath_format, bool showerr)
{
#if !defined(DISABLE_DYNAMIC_LOADING_FFMPEG)
   FreeLibs();

#if defined(__WXMSW__)
   wxString syspath;
   bool pathfix = false;

   wxLogMessage(wxT("Looking up PATH environment variable..."));
   // First take PATH environment variable and store its content.
   if (wxGetEnv(wxT("PATH"),&syspath))
   {
      wxLogMessage(wxT("PATH = '%s'"), syspath.c_str());
      wxString fmtdirsc = wxPathOnly(libpath_format) + wxT(";");
      wxString scfmtdir = wxT(";") + wxPathOnly(libpath_format);
      wxString fmtdir = wxPathOnly(libpath_format);
      wxLogMessage(wxT("Checking that '%s' is in PATH..."), fmtdir.c_str());
      // If the directory, where libavformat is, is not in PATH - add it
      if (!syspath.Contains(fmtdirsc) && !syspath.Contains(scfmtdir) && !syspath.Contains(fmtdir))
      {
         wxLogWarning(wxT("FFmpeg directory is not in PATH."), fmtdir.c_str());
         if (syspath.Last() == wxT(';'))
         {
            wxLogMessage(wxT("Temporarily appending '%s' to PATH..."), fmtdir.c_str());
            syspath.Append(fmtdirsc);
         }
         else
         {
            wxLogMessage(wxT("Temporarily appending '%s' to PATH..."), scfmtdir.c_str());
            syspath.Append(scfmtdir);
         }

         if (wxSetEnv(wxT("PATH"),syspath.c_str()))
            // Remember to change PATH back to normal after we're done
            pathfix = true;
         else
            wxLogSysError(wxT("Setting PATH via wxSetEnv('%s') failed."),syspath.c_str());
      }
      else
      {
         wxLogMessage(wxT("FFmpeg directory is in PATH."));
      }
   }
   else
   {
      wxLogSysError(wxT("PATH does not exist."));
   }
#endif

   //Load libavformat
   // Initially we don't know where are the avcodec and avutl libs
   wxDynamicLibrary *codec = NULL;
   wxDynamicLibrary *util = NULL;
   wxFileName name(libpath_format);
   bool gotError = false;

   // Check for a monolithic avformat
   avformat = new wxDynamicLibrary();
   wxLogMessage(wxT("Checking for monolithic avformat from '%s'."), name.GetFullPath().c_str());
   gotError = !avformat->Load(name.GetFullPath(), wxDL_LAZY);

   // Verify it really is monolithic
   if (!gotError) {
      wxFileName actual;

      actual = FileNames::PathFromAddr(avformat->GetSymbol(wxT("avutil_version")));
      if (actual.GetFullPath().IsSameAs(name.GetFullPath())) {
         actual = FileNames::PathFromAddr(avformat->GetSymbol(wxT("avcodec_version")));
         if (actual.GetFullPath().IsSameAs(name.GetFullPath())) {
             util = avformat;
             codec = avformat;
         }
      }

      if (util == NULL || codec == NULL) {
         wxLogMessage(wxT("avformat not monolithic"));
         avformat->Unload();
         util = NULL;
         codec = NULL;
      }
      else {
         wxLogMessage(wxT("avformat is monolithic"));
      }
   }

   if (!util) {
      name.SetFullName(GetLibAVUtilName());
      avutil = util = new wxDynamicLibrary();
      wxLogMessage(wxT("Loading avutil from '%s'."), name.GetFullPath().c_str());
      util->Load(name.GetFullPath(), wxDL_LAZY);
   }

   if (!codec) {
      name.SetFullName(GetLibAVCodecName());
      avcodec = codec = new wxDynamicLibrary();
      wxLogMessage(wxT("Loading avcodec from '%s'."), name.GetFullPath().c_str());
      codec->Load(name.GetFullPath(), wxDL_LAZY);
   }

   if (!avformat->IsLoaded()) {
      name.SetFullName(libpath_format);
      wxLogMessage(wxT("Loading avformat from '%s'."), name.GetFullPath().c_str());
      gotError = !avformat->Load(name.GetFullPath(), wxDL_LAZY);
   }

#if defined(__WXMSW__)
   //Return PATH to normal
   if ( pathfix )
   {
      wxString oldpath = syspath.BeforeLast(wxT(';'));
      wxLogMessage(wxT("Returning PATH to previous setting..."));
      wxSetEnv(wxT("PATH"),oldpath.c_str());
   }
#endif

   if (gotError) {
      wxLogError(wxT("Failed to load FFmpeg libraries."));
      FreeLibs();
      return false;
   }

   // Show the actual libraries loaded
   if (avutil) {
      wxLogMessage(wxT("Actual avutil path %s"),
                 FileNames::PathFromAddr(avutil->GetSymbol(wxT("avutil_version"))).c_str());
   }
   if (avcodec) {
      wxLogMessage(wxT("Actual avcodec path %s"),
                 FileNames::PathFromAddr(avcodec->GetSymbol(wxT("avcodec_version"))).c_str());
   }
   if (avformat) {
      wxLogMessage(wxT("Actual avformat path %s"),
                 FileNames::PathFromAddr(avformat->GetSymbol(wxT("avformat_version"))).c_str());
   }

   wxLogMessage(wxT("Importing symbols..."));
   FFMPEG_INITDYN(avformat, av_register_all);
   FFMPEG_INITDYN(avformat, av_find_stream_info);
   FFMPEG_INITDYN(avformat, av_read_frame);
   FFMPEG_INITDYN(avformat, av_seek_frame);
   FFMPEG_INITDYN(avformat, av_close_input_file);
   FFMPEG_INITDYN(avformat, av_write_header);
   FFMPEG_INITDYN(avformat, av_interleaved_write_frame);
   FFMPEG_INITDYN(avformat, av_iformat_next);
   FFMPEG_INITDYN(avformat, av_oformat_next);
   FFMPEG_INITDYN(avformat, av_set_parameters);
   FFMPEG_INITDYN(avformat, url_open_protocol);
   FFMPEG_INITDYN(avformat, url_open);
   FFMPEG_INITDYN(avformat, url_fdopen);
   FFMPEG_INITDYN(avformat, url_close);
   FFMPEG_INITDYN(avformat, url_fseek);
   FFMPEG_INITDYN(avformat, url_fclose);
   FFMPEG_INITDYN(avformat, av_new_stream);
   FFMPEG_INITDYN(avformat, avformat_alloc_context);
   FFMPEG_INITDYN(avformat, av_write_trailer);
   FFMPEG_INITDYN(avformat, av_codec_get_tag);
   FFMPEG_INITDYN(avformat, avformat_version);
   FFMPEG_INITDYN(avformat, av_open_input_stream);
   FFMPEG_INITDYN(avformat, av_metadata_get);

   FFMPEG_INITALT(avformat, av_register_protocol2, av_register_protocol);
   FFMPEG_INITALT(avformat, avio_read, get_buffer);
   FFMPEG_INITALT(avformat, avio_seek, url_fseek);
   FFMPEG_INITALT(avformat, avio_close, url_fclose);
   FFMPEG_INITALT(avformat, av_metadata_set2, av_metadata_set);
   FFMPEG_INITALT(avformat, av_guess_format, guess_format);
   FFMPEG_INITALT(avformat, av_match_ext, match_ext);

#if LIBAVCODEC_VERSION_INT > AV_VERSION_INT(52, 58, 0)
   FFMPEG_INITDYN(avcodec, av_init_packet);
#else
   FFMPEG_INITDYN(avformat, av_init_packet);
#endif

#if LIBAVFORMAT_VERSION_INT > AV_VERSION_INT(52, 31, 0)
   FFMPEG_INITDYN(avcodec, av_free_packet);
#endif
   FFMPEG_INITDYN(avcodec, avcodec_init);
   FFMPEG_INITDYN(avcodec, avcodec_find_encoder);
   FFMPEG_INITDYN(avcodec, avcodec_find_encoder_by_name);
   FFMPEG_INITDYN(avcodec, avcodec_find_decoder);
   FFMPEG_INITDYN(avcodec, avcodec_get_context_defaults);
   FFMPEG_INITDYN(avcodec, avcodec_open);
#if LIBAVCODEC_VERSION_INT > AV_VERSION_INT(52, 25, 0)
   FFMPEG_INITDYN(avcodec, avcodec_decode_audio3);
#else
   FFMPEG_INITDYN(avcodec, avcodec_decode_audio2);
#endif
   FFMPEG_INITDYN(avcodec, avcodec_encode_audio);
   FFMPEG_INITDYN(avcodec, avcodec_close);
   FFMPEG_INITDYN(avcodec, avcodec_register_all);
   FFMPEG_INITDYN(avcodec, avcodec_version);
   FFMPEG_INITDYN(avcodec, av_fast_realloc);
   FFMPEG_INITDYN(avcodec, av_codec_next);

   FFMPEG_INITALT(avcodec, av_get_bits_per_sample_format, av_get_bits_per_sample_fmt);

   FFMPEG_INITDYN(avutil, av_free);
   FFMPEG_INITDYN(avutil, av_log_set_callback);
   FFMPEG_INITDYN(avutil, av_log_default_callback);
#if LIBAVUTIL_VERSION_INT > AV_VERSION_INT(49, 15, 0)
   FFMPEG_INITDYN(avutil, av_fifo_alloc);
#else
   FFMPEG_INITDYN(avutil, av_fifo_init);
#endif
   FFMPEG_INITDYN(avutil, av_fifo_generic_read);
   FFMPEG_INITDYN(avutil, av_fifo_realloc2);
   FFMPEG_INITDYN(avutil, av_fifo_free);
   FFMPEG_INITDYN(avutil, av_fifo_size);
   FFMPEG_INITDYN(avutil, av_malloc);
   FFMPEG_INITDYN(avutil, av_fifo_generic_write);
   FFMPEG_INITDYN(avutil, av_freep);
   FFMPEG_INITDYN(avutil, av_rescale_q);
   FFMPEG_INITDYN(avutil, avutil_version);

   wxLogMessage(wxT("All symbols loaded successfully. Initializing the library."));
#endif

   //FFmpeg initialization
   avcodec_init();
   avcodec_register_all();
   av_register_all();
   
   wxLogMessage(wxT("Retrieving FFmpeg library version numbers:"));
   int avfver = avformat_version();
   int avcver = avcodec_version();
   int avuver = avutil_version();
   mAVCodecVersion = wxString::Format(wxT("%d.%d.%d"),avcver >> 16 & 0xFF, avcver >> 8 & 0xFF, avcver & 0xFF);
   mAVFormatVersion = wxString::Format(wxT("%d.%d.%d"),avfver >> 16 & 0xFF, avfver >> 8 & 0xFF, avfver & 0xFF);
   mAVUtilVersion = wxString::Format(wxT("%d.%d.%d"),avuver >> 16 & 0xFF, avuver >> 8 & 0xFF, avuver & 0xFF);

   wxLogMessage(wxT("   AVCodec version 0x%06x - %s (built against 0x%06x - %s)"), 
                  avcver, mAVCodecVersion.c_str(), LIBAVCODEC_VERSION_INT, 
                  wxString::FromUTF8(AV_STRINGIFY(LIBAVCODEC_VERSION)).c_str());
   wxLogMessage(wxT("   AVFormat version 0x%06x - %s (built against 0x%06x - %s)"), 
                  avfver, mAVFormatVersion.c_str(), LIBAVFORMAT_VERSION_INT, 
                  wxString::FromUTF8(AV_STRINGIFY(LIBAVFORMAT_VERSION)).c_str());
   wxLogMessage(wxT("   AVUtil version 0x%06x - %s (built against 0x%06x - %s)"), 
                  avuver,mAVUtilVersion.c_str(), LIBAVUTIL_VERSION_INT, 
                  wxString::FromUTF8(AV_STRINGIFY(LIBAVUTIL_VERSION)).c_str());

   int avcverdiff = (avcver >> 16 & 0xFF) - int(LIBAVCODEC_VERSION_MAJOR);
   int avfverdiff = (avfver >> 16 & 0xFF) - int(LIBAVFORMAT_VERSION_MAJOR);
   int avuverdiff = (avuver >> 16 & 0xFF) - int(LIBAVUTIL_VERSION_MAJOR);
   if (avcverdiff != 0)
      wxLogError(wxT("AVCodec version mismatch = %d"), avcverdiff);
   if (avfverdiff != 0)
      wxLogError(wxT("AVFormat version mismatch = %d"), avfverdiff);
   if (avuverdiff != 0)
      wxLogError(wxT("AVUtil version mismatch = %d"), avuverdiff);
   //make sure that header and library major versions are the same
   if (avcverdiff != 0 || avfverdiff != 0 || avuverdiff != 0)
   {
      wxLogError(wxT("Version mismatch. FFmpeg libraries are unusable."));
      return false;
   }

#if defined(DISABLE_DYNAMIC_LOADING_FFMPEG) && (LIBAVFORMAT_VERSION_INT < AV_VERSION_INT(52, 69, 0))
   av_register_protocol(&ufile_protocol);
#else
   av_register_protocol2(&ufile_protocol, sizeof(ufile_protocol));
#endif

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
