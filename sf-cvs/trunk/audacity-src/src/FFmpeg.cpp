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
wxString GetFFmpegVersion(wxWindow *parent, bool prompt)
{
   return wxString(wxT("FFmpeg support not compiled in"));
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


wxString GetFFmpegVersion(wxWindow *parent, bool prompt)
{
   PickFFmpegLibs();

   wxString versionString = _("FFmpeg library is not found");

   if (prompt) {
      FFmpegLibsInst->FindLibs(parent);
   }

   if (FFmpegLibsInst->LoadLibs(parent, false)) {
      versionString = FFmpegLibsInst->GetLibraryVersion();
   }

   DropFFmpegLibs();

   return versionString;
}

void av_log_wx_callback(void* ptr, int level, const char* fmt, va_list vl)
{
   //Most of this stuff is taked from FFmpeg tutorials and FFmpeg itself
   int av_log_level = AV_LOG_WARNING;
   AVClass* avc = ptr ? *(AVClass**)ptr : NULL;
   if (level > av_log_level)
      return;
   wxString printstring(wxT(""));

   if (avc) {
      printstring.Append(wxString::Format(wxT("[%s @ %p] "), wxString::FromUTF8(avc->item_name(ptr)).c_str(), avc));
   }

   wxString frm(fmt,wxConvLibc);
#if defined(wxMSW)
   frm.Replace(wxT("%t"),wxT("%i"),true); //TODO: on Windows vprintf won't handle %t, and probably some others. Investigate.
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
            S.Id(ID_FFMPEG_DLOAD).AddButton(_("Download..."), wxALIGN_RIGHT);
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
         wxOPEN | wxRESIZE_BORDER,
         this);
      if (!path.IsEmpty()) {
         mLibPath = path;
         mPathText->SetValue(path);
      }
   }

   void OnDownload(wxCommandEvent & event)
   {
      wxString page = wxT("http://audacityteam.org/wiki/index.php?title=FFmpeg");
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
"Audacity attempted to load FFmpeg libraries\n\
to either import or export an audio file,\n\
but libraries were not found.\n\
If you want to use the FFmpeg import/export feature,\n\
please go to Preferences->Import/Export\n\
and tell Audacity where to look for the libraries."
         ));

         mDontShow = S.AddCheckBox(wxT("Do not show this warning again"),wxT("false"));

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

BEGIN_EVENT_TABLE(FFmpegNotFoundDialog, wxDialog)
   EVT_BUTTON(wxID_OK, FFmpegNotFoundDialog::OnOk)
END_EVENT_TABLE()


//----------------------------------------------------------------------------
// FFmpegLibs
//----------------------------------------------------------------------------

FFmpegLibs::FFmpegLibs()
{
   mLibsLoaded = false;
   mStatic = false;
   refcount = 1;
   avformat = avcodec = avutil = NULL;
   if (gPrefs) {
      mLibAVFormatPath = gPrefs->Read(wxT("/FFmpeg/FFmpegLibPath"), wxT(""));
   }

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
      wxLogMessage(wxT("User-specified file doesn't exists! Failed to find libraries."));
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
      mLibsLoaded = false;
   }

   // First try loading it from a previously located path
   if (!mLibAVFormatPath.IsEmpty()) {
      wxLogMessage(wxT("mLibAVFormatPath is not empty, = %s. Loading from it."),mLibAVFormatPath.c_str());
      mLibsLoaded = InitLibs(mLibAVFormatPath,showerr);
   }

   // If not successful, try loading using system search paths
   if (!ValidLibsLoaded()) {
      mLibAVFormatPath = GetLibAVFormatName();
      wxLogMessage(wxT("Trying to load from PATH. File name is %s"),mLibAVFormatPath.c_str());
      mLibsLoaded = InitLibs(mLibAVFormatPath,showerr);
   }

   // If libraries aren't loaded - nag user about that
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

   // Oh well, just give up
   if (!ValidLibsLoaded()) {
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
   wxString libpath_codec(wxT(""));
   wxString libpath_util(wxT(""));

   bool gotError = false;

#if defined(__WXMSW__)
   // On Windows force system to show error messages (as they are
   // more informative than wxMessages).
   unsigned int erm = SetErrorMode(showerr ? 0 : SEM_FAILCRITICALERRORS);
#endif

   wxString syspath;
   bool pathfix = false;
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
      wxLogMessage(wxT("PATH does not exists."));
   }

   //Load libavformat
   avformat = new wxDynamicLibrary();
   if (!avformat->IsLoaded() && !gotError)
   {
      wxLogMessage(wxT("Loading avformat from %s"),libpath_format.c_str());
      gotError = !avformat->Load(libpath_format, wxDL_LAZY);
   }

   //avformat loaded successfully?
   if (!gotError)
   {
      wxLogMessage(wxT("avformat loaded successfully. Acquiring the list of modules."));
      //Get the list of all loaded modules and it's length
      wxDynamicLibraryDetailsArray loaded = avformat->ListLoaded();
      int loadsize = loaded.size();
      wxLogMessage(wxT("List acquired and consists of %d modules"),loadsize);
      for (int i = 0; i < loadsize; i++)
      {
         _wxObjArraywxDynamicLibraryDetailsArray litem = loaded.Item(i);
         //Get modules' path and base name
         wxString libpath = litem.GetPath();
         wxString libname = litem.GetName();
         wxLogMessage(wxT("Item %d: path=%s , name=%s"),i,libpath.c_str(),libname.c_str());
         //Match name against a pattern to find avcodec and avutil
         ///\todo own sections for Mac and *nix
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
      //avformat library as a dynamic one.
      if ((libpath_codec.CompareTo(wxT("")) == 0)
         || (libpath_util.CompareTo(wxT("")) == 0))
      {
         wxLogMessage(wxT("The avformat library happened to be statically linked"));
         mStatic = true;
      }
      else mStatic = false;
   }

   if (!mStatic)
   {
      wxLogMessage(wxT("Trying to load avcodec and avutil"));
      //Load other two libs
      avcodec = new wxDynamicLibrary();
      if (!avcodec->IsLoaded() && !gotError)
      {
         wxLogMessage(wxT("Loading avcodec from %s"),libpath_codec.c_str());
         gotError = !avcodec->Load(libpath_codec, wxDL_LAZY);
      }
      avutil = new wxDynamicLibrary();
      if (!avutil->IsLoaded() && !gotError)
      {
         wxLogMessage(wxT("Loading avutil from %s"),libpath_util.c_str());
         gotError = !avutil->Load(libpath_util, wxDL_LAZY);
      }
   }

   //Return PATH to normal
   if ( pathfix )
   {
      wxString oldpath = syspath.BeforeLast(wxT(';'));
      wxLogMessage(wxT("Returning PATH to normal..."));
      wxSetEnv(wxT("PATH"),oldpath.c_str());
   }

   if ( gotError )
   {
      wxLogMessage(wxT("Failed to load either avcodec or avutil"));
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
   INITDYN(avcodec,av_codec_next);

   INITDYN(avutil,av_free);
   INITDYN(avutil,av_log_set_callback);
   INITDYN(avutil,av_log_default_callback);
   INITDYN(avutil,av_fifo_init);
   INITDYN(avutil,av_fifo_free);
   INITDYN(avutil,av_fifo_read);
   INITDYN(avutil,av_fifo_size);
   INITDYN(avutil,av_fifo_generic_write);
   INITDYN(avutil,av_fifo_realloc);
   INITDYN(avutil,av_malloc);
   INITDYN(avutil,av_freep);
   INITDYN(avutil,av_rescale_q);

#if defined(__WXMSW__)
   //Return error mode to normal
   SetErrorMode(erm);
#endif

   //FFmpeg initialization
   wxLogMessage(wxT("All symbols loaded successfully. Initializing the library."));
   this->avcodec_init();
   this->avcodec_register_all();
   this->av_register_all();
   
   wxLogMessage(wxT("Retrieving library version."));
   int ver = this->avcodec_version();
   mVersion = wxString::Format(wxT("%d.%d-%d"),ver >> 16 & 0xFF, ver >> 8 & 0xFF, ver & 0xFF);
   wxLogMessage(wxT("Version is 0x%06x, which means %s"),ver,mVersion.c_str());
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
