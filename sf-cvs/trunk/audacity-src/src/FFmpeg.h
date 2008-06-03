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

#include "../Audacity.h"	// pulls in config*.h and other program stuff
#include <Experimental.h>
#if defined(USE_FFMPEG)
#include <wx/dynlib.h>
#include <wx/log.h>			// for wxLogNull
#include <wx/msgdlg.h>		// for wxMessageBox

#define __STDC_CONSTANT_MACROS

extern "C" {
#include <avcodec.h>
#include <avformat.h>
}

#define INITDYN(w,f) if ((*(void**)&this->f=(void*)w.GetSymbol(wxT(#f))) == NULL) return false

class FFmpegLibs
{
public:
   FFmpegLibs(bool askuser);
   ~FFmpegLibs();

   void              (*av_log_set_callback)           (void (*)(void*, int, const char*, va_list));
   void              (*av_log_default_callback)       (void* ptr, int level, const char* fmt, va_list vl);
   void              (*av_free)                       (void *ptr);
   unsigned          (*avcodec_version)               (void);
   void              (*avcodec_init)                  (void);
   AVCodec*          (*avcodec_find_encoder)          (enum CodecID id);
   AVCodec*          (*avcodec_find_encoder_by_name)  (const char *name);
   AVCodec*          (*avcodec_find_decoder)          (enum CodecID id);
   AVCodec*          (*avcodec_find_decoder_by_name)  (const char *name);
   void              (*avcodec_string)                (char *buf, int buf_size, AVCodecContext *enc, int encode);
   void              (*avcodec_get_context_defaults)  (AVCodecContext *s) ;
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
   int               (*av_interleaved_write_frame)    (AVFormatContext *s, AVPacket *pkt);
   AVInputFormat*    (*av_iformat_next)               (AVInputFormat *f);

   bool LoadLibs(wxWindow *parent, bool showerr);
   bool ValidLibsLoaded();

   /* initialize the library interface */
   bool InitLibs(wxString libpath_codec, wxString libpath_format, wxString libpath_util, bool showerr);
   void FreeLibs();

   /* note these values are for Windows only - Mac and Unix have their own
   * sections elsewhere */
   //\todo { Section for Mac and *nix }
   wxString GetLibAVCodecName()
   {
      return wxT("avcodec-51.dll");
   }

   wxString GetLibAVFormatName()
   {
      return wxT("avformat-52.dll");
   }

   wxString GetLibAVUtilName()
   {
      return wxT("avutil-49.dll");
   }

   //Ugly reference counting. I thought of using wxStuff for that,
   //but decided that wx reference counting is not useful, since
   //there's no data sharing - object is shared because libraries are.
   int refcount;

private:

   wxString mLibAVCodecPath;
   wxString mLibAVFormatPath;
   wxString mLibAVUtilPath;

   wxDynamicLibrary avcodec;
   wxDynamicLibrary avformat;
   wxDynamicLibrary avutil;

   bool mLibsLoaded;
};
#endif //FFMPEG_INTEGRATION
#endif
