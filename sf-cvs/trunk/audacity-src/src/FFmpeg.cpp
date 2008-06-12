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

void av_log_wx_callback(void* ptr, int level, const char* fmt, va_list vl)
{
   int av_log_level = AV_LOG_WARNING;
   AVClass* avc = ptr ? *(AVClass**)ptr : NULL;
   if (level > av_log_level)
      return;
   wxString printstring(wxT(""));

   if (avc) {
      printstring.Append(wxString::Format(wxT("[%s @ %p] "), wxString::FromUTF8(avc->item_name(ptr)).c_str(), avc));
   }

   wxString frm(fmt,wxConvLibc);
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

//shared object
FFmpegLibs *FFmpegLibsInst = NULL;

FFmpegLibs::FFmpegLibs(bool showerr)
{
   mLibsLoaded = false;
   mStatic = false;
   refcount = 1;
   avformat = avcodec = avutil = NULL;
}

FFmpegLibs::~FFmpegLibs()
{
   if (avformat) delete avformat;
   if (!mStatic)
   {
      if (avcodec) delete avcodec;
      if (avutil) delete avutil;
   }
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
      mLibAVFormatPath = GetLibAVFormatName();
      mLibsLoaded = InitLibs(mLibAVFormatPath,showerr);
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

bool FFmpegLibs::InitLibs(wxString libpath_format, bool showerr)
{
   //Initially we don't know where's the avcodec and avutl libs
   wxString libpath_codec(wxT(""));
   wxString libpath_util(wxT(""));

   bool gotError = false;

#if defined(__WXMSW__)
   //On Windows force system to show error messages (as they are
   //more informative than wxMessages).
   unsigned int erm = SetErrorMode(showerr ? 0 : SEM_FAILCRITICALERRORS);
#endif

   avformat = new wxDynamicLibrary();
   if (!avformat->IsLoaded() && !gotError)
   {
      gotError = !avformat->Load(libpath_format, wxDL_LAZY);
   }

   //avformat loaded successfully?
   if (!gotError)
   {
      //Get the list of all loaded modules and it's length
      wxDynamicLibraryDetailsArray loaded = avformat->ListLoaded();
      int loadsize = loaded.size();
      for (int i = 0; i < loadsize; i++)
      {
         _wxObjArraywxDynamicLibraryDetailsArray litem = loaded.Item(i);
         //Get modules' path and base name
         wxString libpath = litem.GetPath();
         wxString libname = litem.GetName();
         //Match name against a pattern to find avcodec and avutil
#if defined(__WXMSW__)
         if (libname.Matches(wxT("*avcodec*.dll*")))
#else
         if (libname.Matches(wxT("*avcodec*.so*")))
#endif
         {
            wxLogMessage(wxT("Found avcodec: %s"),libpath.c_str());
            libpath_codec = libpath;
         }
#if defined(__WXMSW__)
         else if (libname.Matches(wxT("*avutil*.dll*")))
#else
         else if (libname.Matches(wxT("*avutil*.so*")))
#endif
         {
            wxLogMessage(wxT("Found avutil: %s"),libpath.c_str());
            libpath_util = libpath;
         }
      }
      //avformat loaded all right. If it didn't linked two other
      //libs to itself in process, then it's statically linked.
      //"or" operator ensures that we won't count misnamed statically linked
      //avformat library as dynamic one.
      if ((libpath_codec.CompareTo(wxT("")) == 0)
         || (libpath_util.CompareTo(wxT("")) == 0))
      {
         mStatic = true;
      }
      else mStatic = false;
   }

   if (!mStatic)
   {
      //Load other two libs
      avcodec = new wxDynamicLibrary();
      if (!avcodec->IsLoaded() && !gotError)
      {
         gotError = !avcodec->Load(libpath_codec, wxDL_LAZY);
      }
      avutil = new wxDynamicLibrary();
      if (!avutil->IsLoaded() && !gotError)
      {
         gotError = !avutil->Load(libpath_util, wxDL_LAZY);
      }
   }

   if ( gotError )
   {
      if ( showerr ) wxMessageBox(wxSysErrorMsg());
#if defined(__WXMSW__)
      //On Windows - return error mode to normal
      SetErrorMode(erm);
#endif
      return false;
   }

   if (mStatic)
   {
      //If it's static library, load everything from it
      avcodec = avformat;
      avutil = avformat;
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
   INITDYN(avformat,av_write_frame);
   INITDYN(avformat,av_iformat_next);
   INITDYN(avformat,av_set_parameters);
   INITDYN(avformat,url_fopen);
   INITDYN(avformat,url_fclose);
   INITDYN(avformat,url_fsize);
   INITDYN(avformat,av_new_stream);
   INITDYN(avformat,av_alloc_format_context);
   INITDYN(avformat,guess_format);
   INITDYN(avformat,av_write_trailer);
   INITDYN(avformat,av_init_packet);

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
   INITDYN(avutil,av_fifo_init);
   INITDYN(avutil,av_fifo_free);
   INITDYN(avutil,av_fifo_read);
   INITDYN(avutil,av_fifo_size);
   INITDYN(avutil,av_fifo_generic_write);
   INITDYN(avutil,av_malloc);
   INITDYN(avutil,av_freep);
   INITDYN(avutil,av_rescale_q);

#if defined(__WXMSW__)
   //Return error mode to normal
   SetErrorMode(erm);
#endif

   //FFmpeg initialization
   this->avcodec_init();
   this->avcodec_register_all();
   this->av_register_all();
   return true;
}

void FFmpegLibs::FreeLibs()
{
   if (avformat) avformat->Unload();
   if (!mStatic)
   {
      if (avcodec) avcodec->Unload();
      if (avutil) avutil->Unload();
   }
   return;
}
#endif //USE_FFMPEG
