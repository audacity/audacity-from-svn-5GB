/**********************************************************************

Audacity: A Digital Audio Editor

FFmpeg.h

Audacity(R) is copyright (c) 1999-2008 Audacity Team.
License: GPL v2.  See License.txt.

******************************************************************//**

Describes shared object that is used to access FFmpeg libraries.

*//*******************************************************************/

#if !defined(__AUDACITY_FFMPEG__)
#define __AUDACITY_FFMPEG__

/* FFmpeg is written in C99. It uses many types from stdint.h. Because we are
 * compiling this using a C++ compiler we have to put it in extern "C".
 * __STDC_CONSTANT_MACROS is defined to make <stdint.h> behave like it
 * is actually being compiled with a C99 compiler. This only works if these
 * headers get to stdint.h before anyone else does, otherwise it doesn't get
 * re-processed and doesn't work properly.
 * The symptoms are that INT64_C is not a valid type, which tends to break
 * somewhere down in the implementations using this file */
/* In order to be able to compile this file when ffmpeg is not available we
 * need access to the value of USE_FFMPEG, which means config*.h needs to come
 * in before this file. The suggest way to achieve this is by including
 * Audacity.h */

#if defined(USE_FFMPEG)
	extern "C" {
	#ifdef _STDINT_H
   /* stdint.h has already been included. That's likely to break ffmpeg headers
	* as described above so we issue a warning */
	#warning "stdint.h included before ffmpeg headers, this may well not compile"
	#endif
   #if !defined(__STDC_CONSTANT_MACROS)
	#define __STDC_CONSTANT_MACROS
   #endif
	#include <libavcodec/avcodec.h>
	#include <libavformat/avformat.h>
	#include <libavutil/fifo.h>
	}
#endif

#include "Audacity.h"
/* rather earlier than normal, but pulls in config*.h and other program stuff
 * we need for the next bit */
#include <wx/string.h>
#include <wx/dynlib.h>
#include <wx/log.h>			// for wxLogNull
#include <wx/msgdlg.h>		// for wxMessageBox
#include <wx/utils.h>
#include "../widgets/LinkingHtmlWindow.h"
#include "FileDialog.h"
#include "ShuttleGui.h"
#include "../Prefs.h"
#include <wx/checkbox.h>
#include <wx/textctrl.h>

// if you needed them, any other audacity header files would go here

/* These defines apply whether or not ffmpeg is available */
#define INITDYN(w,f) if ((*(void**)&this->f=(void*)w->GetSymbol(wxT(#f))) == NULL) { wxLogMessage(wxT("Failed to load symbol ") wxT(#f)); return false; };

/// Callback function to catch FFmpeg log messages.
/// Uses wxLogMessage.
void av_log_wx_callback(void* ptr, int level, const char* fmt, va_list vl);

//----------------------------------------------------------------------------
// Get FFmpeg library version
//----------------------------------------------------------------------------
wxString GetFFmpegVersion(wxWindow *parent);

/* from here on in, this stuff only applies when ffmpeg is available */
#if defined(USE_FFMPEG)

//----------------------------------------------------------------------------
// Attempt to load and enable/disable FFmpeg at startup
//----------------------------------------------------------------------------
void FFmpegStartup();

bool LoadFFmpeg(bool showerror);

/// If Audacity failed to load libav*, this dialog
/// shows up and tells user about that. It will pop-up
/// again and again until it is disabled.
class FFmpegNotFoundDialog : public wxDialog
{
public:

   FFmpegNotFoundDialog(wxWindow *parent)
      :  wxDialog(parent, wxID_ANY, wxString(_("FFmpeg not found")))
   {
      ShuttleGui S(this, eIsCreating);
      PopulateOrExchange(S);
   }

   void PopulateOrExchange(ShuttleGui & S)
   {
      wxString text;

      S.SetBorder(10);
      S.StartVerticalLay(true);
      {
         S.AddFixedText(wxT(
"Audacity attempted to use FFmpeg libraries to import an audio file,\n\
but libraries were not found.\n\
If you want to use the FFmpeg import feature, please go to Preferences->Import/Export\n\
and tell Audacity where to look for the libraries."
         ));

         int dontShowDlg = 0;
         gPrefs->Read(wxT("/FFmpeg/NotFoundDontShow"),&dontShowDlg,0);
         mDontShow = S.AddCheckBox(wxT("Do not show this warning again"),dontShowDlg ? wxT("true") : wxT("false"));

         S.AddStandardButtons(eOkButton);
      }
      S.EndVerticalLay();

      Layout();
      Fit();
      SetMinSize(GetSize());
      Center();

      return;
   }

   void OnOk(wxCommandEvent & event)
   {
      if (mDontShow->GetValue())
      {
         gPrefs->Write(wxT("/FFmpeg/NotFoundDontShow"),1);
      }
      this->EndModal(0);
   }

private:

   wxCheckBox *mDontShow;

   DECLARE_EVENT_TABLE()
};

/// Manages liabv* libraries - loads/unloads libraries, imports symbols.
/// Only one instance of this class should exist at each given moment.
/// function definitions are taken from FFmpeg headers manually,
/// eventually (at next major FFmpeg version change) we'll have to review
/// them and update if necessary.
class FFmpegLibs
{
public:
   FFmpegLibs();
   ~FFmpegLibs();

   void              (*av_log_set_callback)           (void (*)(void*, int, const char*, va_list));
   void              (*av_log_default_callback)       (void* ptr, int level, const char* fmt, va_list vl);
   void              (*av_free)                       (void *ptr);
   unsigned          (*avcodec_version)               (void);
   unsigned          (*avformat_version)              (void);
   unsigned          (*avutil_version)                (void);
   void              (*avcodec_init)                  (void);
   AVCodec*          (*avcodec_find_encoder)          (enum CodecID id);
   AVCodec*          (*avcodec_find_encoder_by_name)  (const char *name);
   AVCodec*          (*avcodec_find_decoder)          (enum CodecID id);
   AVCodec*          (*avcodec_find_decoder_by_name)  (const char *name);
   enum CodecID      (*av_codec_get_id)               (const struct AVCodecTag **tags, unsigned int tag);
   unsigned int      (*av_codec_get_tag)              (const struct AVCodecTag **tags, enum CodecID id);
   void              (*avcodec_string)                (char *buf, int buf_size, AVCodecContext *enc, int encode);
   void              (*avcodec_get_context_defaults)  (AVCodecContext *s);
   AVCodecContext*   (*avcodec_alloc_context)         (void);
   void              (*avcodec_get_frame_defaults)    (AVFrame *pic);
   AVFrame*          (*avcodec_alloc_frame)           (void);
   int               (*avcodec_open)                  (AVCodecContext *avctx, AVCodec *codec);
   int               (*avcodec_decode_audio2)         (AVCodecContext *avctx, int16_t *samples, int *frame_size_ptr, const uint8_t *buf, int buf_size);
   int               (*avcodec_encode_audio)          (AVCodecContext *avctx, uint8_t *buf, int buf_size, const short *samples);
   int               (*avcodec_close)                 (AVCodecContext *avctx);
   void              (*avcodec_register_all)          (void);
   void              (*avcodec_flush_buffers)         (AVCodecContext *avctx);
   int               (*av_get_bits_per_sample)        (enum CodecID codec_id);
   int               (*av_get_bits_per_sample_format) (enum SampleFormat sample_fmt);
   void*             (*av_fast_realloc)               (void *ptr, unsigned int *size, unsigned int min_size);
   int               (*av_open_input_file)            (AVFormatContext **ic_ptr, const char *filename, AVInputFormat *fmt, int buf_size, AVFormatParameters *ap);
   void              (*av_register_all)               (void);
   int               (*av_find_stream_info)           (AVFormatContext *ic);
   int               (*av_read_frame)                 (AVFormatContext *s, AVPacket *pkt);
   int               (*av_seek_frame)                 (AVFormatContext *s, int stream_index, int64_t timestamp, int flags);
   int               (*av_close_input_file)           (AVFormatContext *s);
   int               (*av_index_search_timestamp)     (AVStream *st, int64_t timestamp, int flags);
   int               (*av_write_header)               (AVFormatContext *s);
   AVInputFormat*    (*av_iformat_next)               (AVInputFormat *f);
   AVOutputFormat*   (*av_oformat_next)               (AVOutputFormat *f);
   AVCodec*          (*av_codec_next)                 (AVCodec *c);
   int               (*av_set_parameters)             (AVFormatContext *s, AVFormatParameters *ap);
   int               (*url_fopen)                     (ByteIOContext **s, const char *filename, int flags);
   int               (*url_fclose)                    (ByteIOContext *s);
   int               (*url_fsize)                     (ByteIOContext *s);
   AVStream*         (*av_new_stream)                 (AVFormatContext *s, int id);
   AVFormatContext*  (*av_alloc_format_context)       (void);
   AVOutputFormat*   (*guess_format)                  (const char *short_name, const char *filename, const char *mime_type);
   int               (*av_write_trailer)              (AVFormatContext *s);
   int               (*av_interleaved_write_frame)    (AVFormatContext *s, AVPacket *pkt);
   int               (*av_write_frame)    (AVFormatContext *s, AVPacket *pkt);
   void              (*av_init_packet)                (AVPacket *pkt);
   int               (*av_fifo_init)                  (AVFifoBuffer *f, int size);
   void              (*av_fifo_free)                  (AVFifoBuffer *f);
   int               (*av_fifo_read)                  (AVFifoBuffer *f, uint8_t *buf, int buf_size);
   int               (*av_fifo_size)                  (AVFifoBuffer *f);
   int               (*av_fifo_generic_write)         (AVFifoBuffer *f, void *src, int size, int (*func)(void*, void*, int));
   void              (*av_fifo_realloc)                (AVFifoBuffer *f, unsigned int size);
   void*             (*av_malloc)                     (unsigned int size);
   void              (*av_freep)                      (void *ptr);
   int64_t           (*av_rescale_q)                  (int64_t a, AVRational bq, AVRational cq);

   ///! Finds libav* libraries
   ///\return true if found, false if not found
   bool FindLibs(wxWindow *parent);
   ///! Loads libav* libraries
   ///\param showerr - controls whether or not to show an error dialog if libraries cannot be loaded
   ///\return true if loaded, false if not loaded
   bool LoadLibs(wxWindow *parent, bool showerr);
   ///! Checks if libraries are loaded
   ///\return true if libraries are loaded, false otherwise
   bool ValidLibsLoaded();

   ///! Initializes the libraries. Call after LoadLibs (when ValidLibsLoaded returns true)
   ///\param libpath_codec - full file path to the libavformat library
   ///\param showerr - controls whether or not to show an error dialog if libraries cannot be loaded
   ///\return true if initialization completed without errors, false otherwise
   /// do not call (it is called by FindLibs automatically)
   bool InitLibs(wxString libpath_codec, bool showerr);

   ///! Frees (unloads) loaded libraries
   void FreeLibs();

   ///! Returns library version as string
   ///\return libavformat library version or empty string?
   wxString GetLibraryVersion()
   {
      return wxString::Format(wxT("F(%s),C(%s),U(%s)"),mAVFormatVersion.c_str(),mAVCodecVersion.c_str(),mAVUtilVersion.c_str());
   }

#if defined(__WXMSW__)
   /* Library names and file filters for Windows only */

   wxString GetLibraryTypeString()
   {
      return _("Only avformat.dll|*avformat*.dll|Dynamically Linked Libraries (*.dll)|*.dll|All Files (*.*)|*");
   }

   wxString GetLibAVFormatPath()
   {
      return wxT("");
   }

   wxString GetLibAVFormatName()
   {
      return (wxT("avformat-") wxT(AV_STRINGIFY(LIBAVFORMAT_VERSION_MAJOR)) wxT(".dll"));
   }
#elif defined(__WXMAC__)
   /* Library names and file filters for Mac OS only */
   wxString GetLibraryTypeString()
   {
      return _("Only libavformat.*.dylib|libavformat.*.dylib|Dynamically Linked Libraries (*.dylib)|*.dylib|All Files (*)|*");
   }

   wxString GetLibAVFormatPath()
   {
      return wxT("");
   }

   wxString GetLibAVFormatName()
   {
      return (wxT("libavformat.") wxT(AV_STRINGIFY(LIBAVFORMAT_VERSION_MAJOR)) 
					  wxT(".dylib"));
   }
#else
   /* Library names and file filters for other platforms, basically Linux and
	* other *nix platforms */
   wxString GetLibraryTypeString()
   {
      return _("Only libavformat.so|libavformat.so*|Dynamically Linked Libraries (*.so*)|*.so*|All Files (*)|*");
   }

   wxString GetLibAVFormatPath()
   {
      return wxT("");
   }

   wxString GetLibAVFormatName()
   {
      return (wxT("libavformat.so.") wxT(AV_STRINGIFY(LIBAVFORMAT_VERSION_MAJOR)));
   }

#endif // (__WXMAC__) || (__WXMSW__)

   /// Ugly reference counting. I thought of using wxStuff for that,
   /// but decided that wx reference counting is not useful, since
   /// there's no data sharing - object is shared because libraries are.
   int refcount;

private:

   ///! Stored path to libavformat library
   wxString mLibAVFormatPath;

   ///! Stored library version
   wxString mAVCodecVersion;
   wxString mAVFormatVersion;
   wxString mAVUtilVersion;
   
   ///! wx interfaces for dynamic libraries
   wxDynamicLibrary *avformat;
   wxDynamicLibrary *avcodec;
   wxDynamicLibrary *avutil;

   ///! true if libavformat has internal static linkage, false otherwise
   bool mStatic;

   ///! true if libraries are loaded, false otherwise
   bool mLibsLoaded;
};

///! Helper function - creates FFmpegLibs object if it does not exists
///! or just increments reference count if it does
///! It is usually called by constructors or initializators
FFmpegLibs *PickFFmpegLibs();

///! Helper function - destroys FFmpegLibs object if there is no need for it
///! anymore, or just decrements it's reference count
void        DropFFmpegLibs();

#endif // USE_FFMPEG
#endif // __AUDACITY_FFMPEG__

