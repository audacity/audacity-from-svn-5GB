/**********************************************************************

Audacity: A Digital Audio Editor

ImportGStreamer.cpp

Copyright 2008  LRN
Based on ImportFFmpeg.cpp by LRN
Licensed under the GNU General Public License v2 or later

*//****************************************************************//**

\class GStreamerImportFileHandle
\brief An ImportFileHandle for GStreamer data

*//****************************************************************//**

\class GStreamerImportPlugin
\brief An ImportPlugin for GStreamer data

*//*******************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include "../Audacity.h"	// needed before GStreamer.h
#include "../GStreamerLoader.h"		// which brings in gst.h
#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#include <wx/window.h>
#endif

#define DESC _("GStreamer-compatible files")

#if defined(USE_GSTREAMER)
// all the includes live here by default
#include "Import.h"
#include "ImportGStreamer.h"
#include "../Tags.h"
#include "../Internat.h"
#include "../WaveTrack.h"
#include "ImportPlugin.h"
extern "C" {
#include <gio/gio.h>
#include <gst/app/gstappsink.h>
}
#include "stdint.h" //for int16_t

// A stucture, we create and maintain one for each audio stream
struct GStreamContext {
//   gchar      *mBinSinkName; // Name of a sink pad, that receives audio for this stream
   gchar      *mAConvName;   // Name of an audio converter element that serves this stream
   gchar      *mASinkName;   // Name of an AppSink element that passes this stream to Audacity
   GstElement *mConv;        // Audio converter
   GstElement *mSink;        // Application sink
   bool        mUse;         // True if this stream should be imported, False if it should not be
   WaveTrack **mChannels;    // Array of WaveTrack pointers, one for each channel
   gint        mNumChannels; // Number of channels
   gdouble     mSampleRate;  // Sample rate
   gint        mWidth;       // Alignment
   gint        mDepth;       // Sample resolution
};

class GStreamerImportFileHandle;

/// A representative of GStreamer loader in
/// the Audacity import plugin list
class GStreamerImportPlugin : public ImportPlugin
{
public:
   GStreamerImportPlugin():
      ImportPlugin(wxArrayString())
      {
         if (GStreamerInst != NULL)
         {
           mExtensions = GStreamerInst->GetExtensions();
         }
         else
         {
           // It should be an empty list, but i've noticed that empty list doesn't work here
           mExtensions.Add(wxT("wav"));
         }
      }

   ~GStreamerImportPlugin() { }

   wxString GetPluginFormatDescription();

   ///! Probes the file and opens it if appropriate
   ImportFileHandle *Open(wxString Filename);
};

///! Does actual import, returned by GStreamerImportPlugin::Open
class GStreamerImportFileHandle : public ImportFileHandle
{

public:
   GStreamerImportFileHandle(const wxString & name);
   ~GStreamerImportFileHandle();

   ///! Format initialization
   ///\return true if successful, false otherwise
   bool Init();

   wxString GetFileDescription();
   int GetFileUncompressedBytes();

   ///! Imports audio
   ///\return import status (see Import.cpp)
   int Import(TrackFactory *trackFactory, Track ***outTracks,
      int *outNumTracks, Tags *tags);

   ///! Called when pad-added signal comes in from UriDecodeBin (as GstElement)
   ///\param uridecodebin - element that sent the signal
   ///\param pad - source pad of uridecodebin that will be serving the data
   void OnNewStream(GstElement *uridecodebin, GstPad *pad);

   ///! Called when unknown-pad signal comes in from UriDecodeBin
   ///\param uridecodebin - element that sent the signal
   ///\param pad - source pad of uridecodebin that will not be serving any data
   ///\param caps - information about the stream
   void OnUnknownStream(GstElement *uridecodebin, GstPad *pad, GstCaps *caps);

   ///! Adds new stream context
   ///\return pointer to newly-allocated context
   GStreamContext *AddNewStream();

   ///! Removes stream context
   ///\param index - index of a context in context array
   void DeleteStream(guint index);

   ///! Called when a message comes through GStreamer message bus
   ///\param bus - a bus that served the message
   ///\param message - a message
   ///\return true if we want to continue to watch the bus, false otherwise
   gboolean OnBusMessage(GstBus *bus, GstMessage *message);

   ///! Called when new data comes in from the pipeline
   ///\param sc - stream context
   ///\param buffer - GStreamer Audio Buffer
   ///\return eImportSuccess if everything is OK
   int WriteData(GStreamContext *sc, GstBuffer *buffer);

   ///! Called by the pipeline when GST_MESSAGE_TAG arrives
   ///\param list - list of tags
   ///\param tag - a tag in the list
   void WriteTags(const GstTagList *list, const gchar *tag);

   ///! Called by Import.cpp
   ///\return number of readable audio streams in the file
   wxInt32 GetStreamCount()
   {
      return mScs->len;
   }
   
   ///! Called by Import.cpp
   ///\return array of strings - descriptions of the streams
   wxArrayString *GetStreamInfo()
   {
      return mStreamInfo;
   }

   ///! Called by Import.cpp
   ///\param StreamID - index of the stream in mStreamInfo and mScs arrays
   ///\param Use - true if this stream should be imported, false otherwise
   void SetStreamUsage(wxInt32 StreamID, bool Use)
   {
      if ((guint)StreamID < mScs->len)
      {
         GStreamContext *c = (GStreamContext*)g_ptr_array_index(mScs,StreamID);
         c->mUse = Use;
      }
   }

private:

   GstElement           *mPipeline;      //!< GStreamer pipeline
   GstBus               *mBus;           //!< Message bus
   GstElement           *mDec;           //!< uridecodebin element
//   GMainLoop            *mGLoop;         //!< GLib main loop
//   GstElement           *mAudioBin;      //!< Audio bin that contains audio converters and audio sinks
   GstStaticCaps         mStaticCaps;    //!< Decribes our input capabilities

   GPtrArray            *mScs;           //!< Array of pointers to stream contexts.
   wxArrayString        *mStreamInfo;    //!< Array of stream descriptions. Length is the same as mScs

   bool                  mCancelled;     //!< True if importing was canceled by user
   wxString              mName;          //!< Source name

   TrackFactory         *mTrackFactory;  //!< Used to save Audacity track factory
   Track              ***mOutTracks;     //!< Tracks to be passed back to Audacity
   int                  *mOutNumTracks;  //!< Number of tracks to be passed back to Audacity
   Tags                  mTags;          //!< Tags to be passed back to Audacity
};

void GetGStreamerImportPlugin(ImportPluginList *importPluginList,
                           UnusableImportPluginList *unusableImportPluginList)
{
   importPluginList->Append(new GStreamerImportPlugin);
}

wxString GStreamerImportPlugin::GetPluginFormatDescription()
{
   return DESC;
}

ImportFileHandle *GStreamerImportPlugin::Open(wxString filename)
{
   if (!GStreamerInst->Loaded())
      return NULL;

   GStreamerImportFileHandle *handle = new GStreamerImportFileHandle(filename);

   // Open the file for import  
   bool success = handle->Init();
   if (!success) {
      delete handle;
      return NULL;
   }

   return handle;
}

GStreamerImportFileHandle::GStreamerImportFileHandle(const wxString & name)
:  ImportFileHandle(name)
{

   mStreamInfo = new wxArrayString();
   mCancelled = false;
   mName = name;
   mTrackFactory = NULL;

   mPipeline = mDec = NULL;
//   mAudioBin = NULL;
   mScs = g_ptr_array_new();
//   mGLoop = NULL;

   memset(&mStaticCaps,0,sizeof(mStaticCaps));
   mStaticCaps.string = (               // We are guaranteed to get audio in following format:
        "audio/x-raw-int, "             // Audaciy can handle floats too, but ints should be ok.
        "signed = (boolean) { TRUE }, " // I don't think that Audacity supports unsigned ints.
        "width = (int) { 16, 32 }, "    // 2- (for 16-bit depth) or 4-byte (for 24-bit depth) alignment.
        "depth = (int) { 16, 24 }, "    // 16- and 24-bit depth. 32-bit ints are not supported.
        "rate = (int) [ 1, MAX ], "     // AFAIK Audacity supports any sample rate.
        "channels = (int) [ 1, MAX ], " // Same here - Audacity supports any number of channel.
        "endianness = (int) BYTE_ORDER" // I think 'BYTE_ORDER' means 'native'. Right?
        );
}

GStreamerImportFileHandle::~GStreamerImportFileHandle()
{
   delete mStreamInfo;
   if (mBus != NULL)
     gst_object_unref(mBus);
   while (mScs->len > 0)
     DeleteStream(0);
   g_ptr_array_free(mScs,TRUE);
   if (mPipeline != NULL)
   {
      gst_element_set_state(mPipeline, GST_STATE_NULL);
      gst_object_unref(GST_OBJECT(mPipeline));
   }
}

void GStreamerWriteTags(const GstTagList *list, const gchar *tag, gpointer user_data)
{
   GStreamerImportFileHandle *handle = (GStreamerImportFileHandle*)user_data;
   handle->WriteTags(list, tag);
}

void GStreamerImportFileHandle::WriteTags(const GstTagList *list, const gchar *tag)
{
   guint valuecount = gst_tag_list_get_tag_size(list, tag);
   gchar *strvalue = NULL;
   GType tagtype = gst_tag_get_type(tag);
   const gchar *typetitle = g_type_name(tagtype);
   if (tagtype == G_TYPE_STRING)
      gst_tag_list_get_string(list, tag, &strvalue);
   else if (tagtype == G_TYPE_UINT)
   {
      guint uintvalue;
      if (gst_tag_list_get_uint(list, tag, &uintvalue))
        strvalue = g_strdup_printf("%d", uintvalue);
   }
   else if (tagtype == GST_TYPE_DATE)
   {
      GDate *datevalue;
      if (gst_tag_list_get_date(list, tag, &datevalue))
      {
        strvalue = g_new0(char,100);
        g_date_strftime(strvalue, 100, "%Y", datevalue);
      }
   }
   wxString wxstrvalue = wxString::FromUTF8(strvalue);
   if (g_utf8_collate(tag, GST_TAG_TITLE) == 0)
      mTags.SetTag(TAG_TITLE, wxstrvalue);
   else if (g_utf8_collate(tag, GST_TAG_ARTIST) == 0)
      mTags.SetTag(TAG_ARTIST, wxstrvalue);
   else if (g_utf8_collate(tag, GST_TAG_ALBUM) == 0)
      mTags.SetTag(TAG_ALBUM, wxstrvalue);
   else if (g_utf8_collate(tag, GST_TAG_TRACK_NUMBER) == 0)
      mTags.SetTag(TAG_TRACK, wxstrvalue);
   else if (g_utf8_collate(tag, GST_TAG_DATE) == 0)
      mTags.SetTag(TAG_YEAR, wxstrvalue);
   else if (g_utf8_collate(tag, GST_TAG_GENRE) == 0)
      mTags.SetTag(TAG_GENRE, wxstrvalue);
   else if (g_utf8_collate(tag, GST_TAG_COMMENT) == 0)
      mTags.SetTag(TAG_COMMENTS, wxstrvalue);
   else
      mTags.SetTag(wxString::FromUTF8(tag), wxstrvalue);
   if (strvalue)
     g_free(strvalue);
}

gboolean GStreamerImportFileHandle::OnBusMessage(GstBus *bus, GstMessage *message)
{
   if (message != NULL && message->src != NULL)
   {
      gchar *objname = gst_object_get_name(message->src);
      if (objname != NULL)
      {
        wxLogMessage(wxT("GStreamer: Got message %s from object %s"), wxString::FromUTF8(GST_MESSAGE_TYPE_NAME(message)).c_str() ,wxString::FromUTF8(objname).c_str());
      }
      g_free(objname);
   }
   else
     if (message)
        wxLogMessage(wxT("GStreamer: Got message %s"), wxString::FromUTF8(GST_MESSAGE_TYPE_NAME(message)).c_str());

   GError *err;
   gchar *debug;

   switch (GST_MESSAGE_TYPE(message))
   {
   case GST_MESSAGE_ERROR:
      gst_message_parse_error(message, &err, &debug);
      wxLogMessage(wxT("GStreamer Error: %s - %s"), wxString::FromUTF8(err->message).c_str(), wxString::FromUTF8(debug).c_str());
      g_error_free(err);
      g_free(debug);
      return FALSE;
      break;
   case GST_MESSAGE_WARNING:
      gst_message_parse_warning(message, &err, &debug);
      wxLogMessage(wxT("GStreamer Warning: %s - %s"), wxString::FromUTF8(err->message).c_str(), wxString::FromUTF8(debug).c_str());
      g_error_free(err);
      g_free(debug);
      break;
   case GST_MESSAGE_INFO:
      gst_message_parse_info(message, &err, &debug);
      wxLogMessage(wxT("GStreamer Info: %s - %s"), wxString::FromUTF8(err->message).c_str(), wxString::FromUTF8(debug).c_str());
      g_error_free(err);
      g_free(debug);
      break;
   case GST_MESSAGE_STATE_CHANGED:
      GstState oldstate, newstate, pending;
      gst_message_parse_state_changed(message, &oldstate, &newstate, &pending);
      wxLogMessage(wxT("GStreamer State: %d -> %d ... %d"), oldstate, newstate, pending);
      break;
   case GST_MESSAGE_ASYNC_DONE:
      wxLogMessage(wxT("GStreamer: async done"));
      return FALSE;
      break;
   case GST_MESSAGE_EOS:
      wxLogMessage(wxT("GStreamer: end of stream"));
      gst_element_set_state(mPipeline, GST_STATE_NULL);
      return FALSE;
      break;
   case GST_MESSAGE_TAG:
      GstTagList *tags;
      tags = NULL;
      gst_message_parse_tag(message, &tags);
      gst_tag_list_foreach(tags, GStreamerWriteTags, (gpointer)this);
      break;
   default:
      /* unhandled message */
      break;
   }
 
  /* we want to be notified again the next time there is a message
    * on the bus, so returning TRUE (FALSE means we want to stop watching
    * for messages on the bus and our callback should not be called again)
    */
   return TRUE;
}

gboolean GStreamerBusCallback(GstBus *bus, GstMessage *message, gpointer data)
{
   GStreamerImportFileHandle *handle = (GStreamerImportFileHandle*)data;
   return handle->OnBusMessage(bus, message);
}

void GStreamerNewStreamCallback(GstElement *uridecodebin, GstPad *pad, gpointer data)
{
   GStreamerImportFileHandle *handle = (GStreamerImportFileHandle*)data;
   handle->OnNewStream(uridecodebin, pad);
}

void GStreamerUnknownStreamCallback(GstElement *uridecodebin, GstPad *pad, GstCaps *caps, gpointer data)
{
   GStreamerImportFileHandle *handle = (GStreamerImportFileHandle*)data;
   handle->OnUnknownStream(uridecodebin, pad, caps);
}

// this "gint" is really GstAutoplugSelectResult enum, but we may not have gstplay-enum.h, so just use gint
gint GStreamerAutoplugSelectCallback(GstElement *element, GstPad *pad, GstCaps *caps, GstElementFactory *factory, gpointer data)
{
   // Check factory class
   const gchar *fclass = gst_element_factory_get_klass(factory);
   gboolean quit = FALSE;
   // Skip video decoding
   if (g_strrstr(fclass,"Video"))
      return 2;
   return 0;
}

void GStreamerImportFileHandle::OnNewStream(GstElement *uridecodebin, GstPad *pad)
{
   GstCaps *caps;
   GstStructure *str;

   caps = gst_pad_get_caps(pad);
   str = gst_caps_get_structure(caps, 0);

   // Dump info
   GString *strinfo = g_string_new(NULL);
   gst_structure_foreach(str, LogStructure, strinfo);
   // Check stream type
   const gchar *name = gst_structure_get_name(str);
   // Only accept audio streams with integer samples
   // TODO: instead of g_strrstr use caps_subset or caps_intersect with caps from mStaticCaps
   gboolean quit = FALSE;
   if (!g_strrstr(name, "audio") || !g_strrstr(name, "raw-int"))
   {
      g_string_free(strinfo, TRUE);
      quit = TRUE;
   }
   gst_caps_unref (caps);
   if (quit)
     return;

   // Create unique name
   gchar *mAConvName = g_strdup_printf("aconv-%d",mScs->len);
   // Create an audioconv   
   GstElement *mConv = gst_element_factory_make("audioconvert", mAConvName);
   // Get it's sink pad
   GstPad *convpad = gst_element_get_static_pad(mConv, "sink");
   // Add it to the pipeline
   gst_bin_add(GST_BIN(mPipeline), mConv);

   GstPadLinkReturn ret = gst_pad_link(pad, convpad);
   gst_object_unref(convpad);
   if (ret)
   {
      // TODO: insert wxLogMessage here
      g_print("Failed to link uridecodebin to audioconvert - %d\n",ret);
      g_free(mAConvName);
      gst_bin_remove(GST_BIN(mPipeline), mConv);
      gst_object_unref(mConv);
      g_string_free(strinfo, TRUE);
      return;
   }

   gchar *mASinkName = g_strdup_printf("asink-%d",mScs->len);
   GstElement *mSink = gst_element_factory_make("appsink", mASinkName);
   // Set up sink properties
   caps = gst_static_caps_get(&mStaticCaps);
   gst_app_sink_set_caps(GST_APP_SINK(mSink), caps);          //!< NULL by default? (only accept data that matches caps)
   gst_caps_unref(caps);
   gst_base_sink_set_sync(GST_BASE_SINK(mSink), FALSE);       //!< TRUE by default (sync to the clock)
   gst_app_sink_set_drop(GST_APP_SINK(mSink), FALSE);         //!< FALSE by default (don't drop buffers when queue is full)
   gst_app_sink_set_emit_signals(GST_APP_SINK(mSink), FALSE); //!< FALSE by default (work in blocking mode)
   gst_app_sink_set_max_buffers(GST_APP_SINK(mSink), 10);     //!< 0 by default, which means unlimited buffering

   gst_bin_add(GST_BIN(mPipeline), mSink);

   if (!gst_element_link(mConv, mSink))
   {
      g_free(mAConvName);
      gst_bin_remove(GST_BIN(mPipeline), mConv);
      gst_object_unref(mConv);
      g_free(mASinkName);
      gst_bin_remove(GST_BIN(mPipeline), mSink);
      gst_object_unref(mSink);
      g_string_free(strinfo, TRUE);
   }

   // Run newly added elements
   GstStateChangeReturn statechange;
   statechange = gst_element_set_state(mConv, GST_STATE_PAUSED);
   statechange = gst_element_set_state(mSink, GST_STATE_PAUSED);
   
   // Allocate memory
   GStreamContext *c = g_new0(GStreamContext,1);
   c->mAConvName = mAConvName;
   c->mASinkName = mASinkName;
   c->mConv = mConv;
   c->mSink = mSink;
   // Add new stream context to context array
   g_ptr_array_add(mScs,c);

   // Add stream info
   wxString sinfo;
   // TODO: filter the strinfo somehow, becaue it may have a lot of unnecessary elements
   sinfo.Printf(wxT("Index[%02x] %s"),mScs->len - 1, wxString::FromUTF8(strinfo->str).c_str());
   g_string_free(strinfo, TRUE);
   mStreamInfo->Add(sinfo);
}

// For unknown streams just dump info
void GStreamerImportFileHandle::OnUnknownStream(GstElement *uridecodebin, GstPad *pad, GstCaps *caps)
{
   GstStructure *str;
   str = gst_caps_get_structure(caps, 0);
   gst_structure_foreach(str, LogStructure, NULL);
}

// Free memory occupied by the stream and remove it
void GStreamerImportFileHandle::DeleteStream(guint index)
{
   if (index < mScs->len)
   {
      GStreamContext *str = (GStreamContext*)g_ptr_array_index(mScs,index);
      g_free(str->mAConvName);
      g_free(str->mASinkName);
      gst_bin_remove(GST_BIN(mPipeline), str->mConv);
      gst_bin_remove(GST_BIN(mPipeline), str->mSink);
      gst_object_unref(GST_OBJECT(str->mConv));
      gst_object_unref(GST_OBJECT(str->mSink));
      g_ptr_array_remove(mScs,(gpointer)str);
      g_free(str);
   }
}

// Pipeline initialization
bool GStreamerImportFileHandle::Init()
{
   // Create main loop
   //mGLoop = g_main_loop_new(NULL, FALSE);

   GstPad *audiopad = NULL;

   // Create a pipeline
   mPipeline = gst_pipeline_new("pipeline");
   // Get it's bus an add a message watch to it
   mBus = gst_pipeline_get_bus(GST_PIPELINE(mPipeline));   

   // Set up source location
   gchar *name_utf8 = g_strdup(mName.ToUTF8());
   // TODO: handle various URIs, not just file URIs. Yes, we can import radio streams through http!
   gchar *name_with_proto_utf8 = g_strdup_printf("file:///%s",name_utf8);
   g_free(name_utf8);
   // FIXME: it is possible that g_filename_from_utf8 should be used to convert uri to filesystem encoding
/*
   GError *err = NULL;
   gchar *name_filesystem = g_filename_from_utf8(name_with_proto_utf8, -1, NULL, NULL, &err);
   g_free(name_with_proto_utf8);
   if (err != NULL)
   {
      return false;
   }
*/
   
   // Create uridecodebin and set up signal handlers
   mDec = gst_element_factory_make("uridecodebin", "decoder");
   g_signal_connect(mDec, "pad-added", G_CALLBACK(GStreamerNewStreamCallback), (gpointer)this);
   g_signal_connect(mDec, "unknown-type", G_CALLBACK(GStreamerUnknownStreamCallback), (gpointer)this);
   //g_signal_connect(mDec, "autoplug-continue", G_CALLBACK(GStreamerAutoplugContinueCallback), (gpointer)this);
   //g_signal_connect(mDec, "autoplug-factories", G_CALLBACK(GStreamerAutoplugFactoriesCallback), (gpointer)this);
   g_signal_connect(mDec, "autoplug-select", G_CALLBACK(GStreamerAutoplugSelectCallback), (gpointer)this);
   
   

   g_object_set(G_OBJECT(mDec), "uri", name_with_proto_utf8, NULL);
   //g_free(name_filesystem);

   // Add everythin into the pipeline
   gst_bin_add_many(GST_BIN(mPipeline), mDec, NULL);

   // Run the pipeline
   GstStateChangeReturn statechange = gst_element_set_state(mPipeline, GST_STATE_PAUSED);

   while (true)
   {
      GstMessage *msg = gst_bus_pop(mBus);
      if (msg)
      {
        if (!OnBusMessage(mBus,msg))
          break;
      }
   }

   return true;
}

wxString GStreamerImportFileHandle::GetFileDescription()
{
   return DESC;
}

int GStreamerImportFileHandle::GetFileUncompressedBytes()
{
   // TODO: Get Uncompressed byte count. Is it possible for GStreamer?
   return 0;
}

int GStreamerImportFileHandle::Import(TrackFactory *trackFactory,
              Track ***outTracks,
              int *outNumTracks,
              Tags *tags)
{
   CreateProgress();

   // Not necessary anymore, but i decided to keep it
   mTrackFactory = trackFactory;
   mOutTracks = outTracks;
   mOutNumTracks = outNumTracks;

   GstStateChangeReturn statechange = gst_element_set_state(mPipeline, GST_STATE_PLAYING);
   if (statechange == GST_STATE_CHANGE_FAILURE)
      return eImportFailed;

   // This is the heart of the importing process
   // The result of Import() to be returend. It will be something other than zero if user canceled or some error appears.
   int res = eImportSuccess;
   // Read next frame.
   gboolean doIt = TRUE;
   while (doIt && res == eImportSuccess)
   {
      doIt = FALSE;
      // Keep an eye on any messages we could have
      GstMessage *msg = gst_bus_pop(mBus);
      while (msg != NULL)
      {
        OnBusMessage(mBus,msg);
        msg = gst_bus_pop(mBus);
      }
      for (guint i = 0; i < mScs->len && (res == eImportSuccess || res == eImportStopped); i++)
      {
         GStreamContext *c = (GStreamContext *)g_ptr_array_index(mScs, i);
         if (!c->mUse) continue;
         GstBuffer *incbuf = gst_app_sink_pull_buffer(GST_APP_SINK(c->mSink));
         if (incbuf)
         {
           doIt = TRUE;
           res = WriteData(c, incbuf);
         }
         gst_buffer_unref(incbuf);
      }
   }

   // Something bad happened - destroy everything!
   if (res == eImportFailed)
   {
      for (guint s = 0; s < mScs->len; s++)
      {
         GStreamContext *c = (GStreamContext *)g_ptr_array_index(mScs, s);
         delete[] c->mChannels;
      }

      if (mCancelled)
         return eImportCancelled;
      else
         return eImportFailed;
   }

   *outNumTracks = 0;
   for (guint s = 0; s < mScs->len; s++)
   {
      GStreamContext *c = (GStreamContext*)g_ptr_array_index(mScs,s);
      if (c->mUse)
        *outNumTracks += c->mNumChannels;
   }

   // Create new tracks
   *outTracks = new Track *[*outNumTracks];

   // Copy audio from mChannels to newly created tracks (destroying mChannels elements in process)
   int trackindex = 0;
   for (guint s = 0; s < mScs->len; s++)
   {
      GStreamContext *c = (GStreamContext*)g_ptr_array_index(mScs,s);
      if (c->mUse)
      {
         for(int ch = 0; ch < c->mNumChannels; ch++)
         {
            c->mChannels[ch]->Flush();
            (*outTracks)[trackindex++] = c->mChannels[ch];
         }
         delete[] c->mChannels;
         c->mChannels = NULL;
         c->mUse = false;
      }
   }

   // Pray that copycon works correctly
   *tags = mTags;

   return eImportSuccess;
}

int GStreamerImportFileHandle::WriteData(GStreamContext *sc,GstBuffer *buffer)
{
   if (this->mTrackFactory == NULL) return eImportFailed;
   size_t pos = 0;

   int updateResult = eImportSuccess;

   gpointer data = GST_BUFFER_DATA(buffer);
   guint length = GST_BUFFER_SIZE(buffer);
   if (sc->mNumChannels == 0)
   {
      GstPad *pad = gst_element_get_static_pad(sc->mSink,"sink");
      if (pad != NULL)
      {
         GstCaps *caps = gst_pad_get_negotiated_caps(pad);
         if (caps != NULL)
         {
            gint capcount = gst_caps_get_size(caps);
            GstStructure *str = gst_caps_get_structure(caps, 0);
            gst_structure_foreach(str, LogStructure, NULL);
            gint channels = -1;
            gst_structure_get_int(str, "channels", &channels);
            gint rate = 0;
            gst_structure_get_int(str, "rate", &rate);
            sc->mNumChannels = channels;
            sc->mSampleRate = (double)rate;
            gst_structure_get_int(str, "width", &sc->mWidth);
            gst_structure_get_int(str, "depth", &sc->mDepth);
            gst_caps_unref(caps);
            if (channels <= 0) sc->mUse = false;
         }
         gst_object_unref(pad);
      }
      else sc->mUse = false;
   }
   if (sc->mChannels == NULL)
   {
      sc->mChannels = new WaveTrack *[sc->mNumChannels];
      int ch;
      for (ch = 0; ch < sc->mNumChannels; ch++)
      {
         sampleFormat sfmt = int16Sample;
         switch(sc->mDepth)
         {
         case 16:
           sfmt = int16Sample;
           break;
         case 24:
           sfmt = int24Sample;
           break;
         default:
           ;// TODO: Error? We do not support 32-bit int samples, do we?
         }
         sc->mChannels[ch] = mTrackFactory->NewWaveTrack(sfmt, sc->mSampleRate);
         if (sc->mNumChannels == 2)
         {
            switch (ch)
            {
            case 0:
               sc->mChannels[ch]->SetChannel(Track::LeftChannel);
               sc->mChannels[ch]->SetLinked(true);
               break;
            case 1:
               sc->mChannels[ch]->SetChannel(Track::RightChannel);
               sc->mChannels[ch]->SetTeamed(true);
               break;
            }
         }
         else
         {
            sc->mChannels[ch]->SetChannel(Track::MonoChannel);
         }
      }
   }
   
   // Allocate the buffer to store audio.
   int nChannels = sc->mNumChannels;
   int16_t **tmp16 = NULL;
   int32_t **tmp32 = NULL;
   // For 16-bit samples use 16-bit output buffer
   if (sc->mWidth == 16)
   {
      tmp16 = (int16_t**)malloc(sizeof(int16_t*)*nChannels);
      for (int chn = 0; chn < nChannels; chn++)
      {
         tmp16[chn] = (int16_t*)malloc(sizeof(int16_t) * length/sizeof(int16_t)/nChannels);
      }
   }
   // For 24- and 32-bit samples use 32-bit output buffer
   else if (sc->mWidth == 24 || sc->mWidth == 32)
   {
      tmp32 = (int32_t**)malloc(sizeof(int32_t*)*nChannels);
      for (int chn = 0; chn < nChannels; chn++)
      {
         tmp32[chn] = (int32_t*)malloc(sizeof(int32_t) * length/sizeof(int32_t)/nChannels);
      }      
   }

   // Separate the channels
   int16_t *int16_data = NULL;
   int32_t *int32_data = NULL;
   int index = 0;
   // Samples may be 16-, 24- or 32-bit, but input buffer's width is independant
   // I'm assuming that we'll never get width=16 and depth=24 or 32, that's impossible
   // Or width=32 and depth=16, that's stupid
   if (sc->mWidth == 16)
   {
      int16_data = (int16_t*)data;
      for (pos = 0; pos < length/sizeof(int16_t);)
      {
         for (int chn = 0; chn < nChannels; chn++)
         {
            tmp16[chn][index] = int16_data[pos];
            pos++;
         }
         index++;
      }
   }
   else if (sc->mWidth == 24 || sc->mWidth == 32)
   {
      int32_data = (int32_t*)data;
      for (pos = 0; pos < length/sizeof(int32_t);)
      {
         for (int chn = 0; chn < nChannels; chn++)
         {
            tmp32[chn][index] = int32_data[pos];
            pos++;
         }
         index++;
      }
   }

   // Write audio into WaveTracks
   if (sc->mDepth == 16)
   {
      for (int chn = 0; chn < nChannels; chn++)
      {
         sc->mChannels[chn]->Append((samplePtr)tmp16[chn],int16Sample,index);
         free(tmp16[chn]);
      }
      free(tmp16);
   }
   else if (sc->mDepth == 24 || sc->mDepth == 32)
   {
      for (int chn = 0; chn < nChannels; chn++)
      {
         sc->mChannels[chn]->Append((samplePtr)tmp32[chn],int24Sample,index);
         free(tmp32[chn]);
      }
      free(tmp32);
   }
   
   // Try to update the progress indicator (and see if user wants to stop or cancel)
   GstFormat fmt = GST_FORMAT_TIME;
   gint64 tpos, tlen;
   if (gst_element_query_position (mPipeline, &fmt, &tpos) &&
       gst_element_query_duration (mPipeline, &fmt, &tlen) )
   {
      updateResult = mProgress->Update((wxLongLong)tpos, (wxLongLong)tlen);
      if (updateResult != 1)
      {
         if (updateResult == 0)
         {
           mCancelled = true;
           updateResult = eImportCancelled;
         }
         else
           updateResult = eImportStopped;
         GstStateChangeReturn statechange = gst_element_set_state(mPipeline, GST_STATE_NULL);
      }
      else
        updateResult = eImportSuccess;
   }
   
   return updateResult;
}

#endif //USE_GSTREAMER
