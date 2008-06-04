/**********************************************************************

Audacity: A Digital Audio Editor

FFmpeg.cpp

Audacity(R) is copyright (c) 1999-2008 Audacity Team.
License: GPL v2.  See License.txt.

******************************************************************//**

\class FFmpegLibs
\brief Class used to dynamically load FFmpeg libraries

*//*******************************************************************/
#include "FFmpeg.h"		// brings config*.h in with it

#ifdef _DEBUG
   #ifdef _MSC_VER
      #undef THIS_FILE
      static char*THIS_FILE= __FILE__;
      #define new new(_NORMAL_BLOCK, THIS_FILE, __LINE__)
   #endif
#endif

#if defined(USE_FFMPEG)
//----------------------------------------------------------------------------
// FFmpegLibs
//----------------------------------------------------------------------------

//shared object
FFmpegLibs *FFmpegLibsInst = NULL;

FFmpegLibs::FFmpegLibs(bool showerr)
{
   mLibsLoaded = false;

   refcount = 1;
}

FFmpegLibs::~FFmpegLibs()
{

};

bool FFmpegLibs::LoadLibs(wxWindow *parent, bool showerr)
{
   wxLogNull logNo;

   if (ValidLibsLoaded()) {
      FreeLibs();
      mLibsLoaded = false;
   }

   // Try loading using system search paths
   if (!ValidLibsLoaded()) {
      mLibAVCodecPath = GetLibAVCodecName();
      mLibAVFormatPath = GetLibAVFormatName();
      mLibAVUtilPath = GetLibAVUtilName();
      mLibsLoaded = InitLibs(mLibAVCodecPath, mLibAVFormatPath, mLibAVUtilPath,showerr);
   }

   // Oh well, just give up
   if (!ValidLibsLoaded()) {
      return false;
   }

   return true;
}

bool FFmpegLibs::ValidLibsLoaded()
{
   return mLibsLoaded;
}

bool FFmpegLibs::InitLibs(wxString libpath_codec, wxString libpath_format, wxString libpath_util, bool showerr)
{

   bool gotError = false;
#if defined(__WXMSW__)
   unsigned int erm = SetErrorMode(showerr ? 0 : SEM_FAILCRITICALERRORS);
#endif

   if (!avformat.IsLoaded() && !gotError)
   {
      gotError = !avformat.Load(libpath_format, wxDL_LAZY);
   }

   if (!avcodec.IsLoaded() && !gotError)
   {
      gotError = !avcodec.Load(libpath_codec, wxDL_LAZY);
   }

   if (!avutil.IsLoaded() && !gotError)
   {
      gotError = !avutil.Load(libpath_util, wxDL_LAZY);
   }

   if ( gotError )
   {
      if ( showerr ) wxMessageBox(wxSysErrorMsg());
#if defined(__WXMSW__)
      SetErrorMode(erm);
#endif
      return false;
   }

   INITDYN(avformat,av_register_all);
   INITDYN(avformat,av_open_input_file);
   INITDYN(avformat,av_find_stream_info);
   INITDYN(avformat,av_read_frame);
   INITDYN(avformat,av_seek_frame);
   INITDYN(avformat,av_close_input_file);
   INITDYN(avformat,av_index_search_timestamp);
   INITDYN(avformat,av_write_header);
   INITDYN(avformat,av_interleaved_write_frame);
   INITDYN(avformat,av_iformat_next);

   INITDYN(avcodec,avcodec_init);
   INITDYN(avcodec,avcodec_find_encoder);
   INITDYN(avcodec,avcodec_find_encoder_by_name);
   INITDYN(avcodec,avcodec_find_decoder);
   INITDYN(avcodec,avcodec_find_decoder_by_name);
   INITDYN(avcodec,avcodec_string);
   INITDYN(avcodec,avcodec_get_context_defaults);
   INITDYN(avcodec,avcodec_alloc_context);
   INITDYN(avcodec,avcodec_get_frame_defaults);
   INITDYN(avcodec,avcodec_alloc_frame);
   INITDYN(avcodec,avcodec_open);
   INITDYN(avcodec,avcodec_decode_audio2);
   INITDYN(avcodec,avcodec_encode_audio);
   INITDYN(avcodec,avcodec_close);
   INITDYN(avcodec,avcodec_register_all);
   INITDYN(avcodec,avcodec_flush_buffers);
   INITDYN(avcodec,av_get_bits_per_sample);
   INITDYN(avcodec,av_get_bits_per_sample_format);
   INITDYN(avcodec,avcodec_version);
   INITDYN(avcodec,av_fast_realloc);

   INITDYN(avutil,av_free);
   INITDYN(avutil,av_log_set_callback);
   INITDYN(avutil,av_log_default_callback);

#if defined(__WXMSW__)
   SetErrorMode(erm);
#endif

   this->avcodec_init();
   this->avcodec_register_all();
   this->av_register_all();
   return true;
}

void FFmpegLibs::FreeLibs()
{
   avcodec.Unload();
   avformat.Unload();
   avutil.Unload();
   return;
}
#endif //USE_FFMPEG
