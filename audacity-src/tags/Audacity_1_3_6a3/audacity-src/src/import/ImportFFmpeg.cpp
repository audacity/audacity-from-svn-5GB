/**********************************************************************

Audacity: A Digital Audio Editor

ImportFFmpeg.cpp

Copyright 2008  LRN
Based on ImportFLAC.cpp by Sami Liedes and transcode_sample.c by ANYwebcam Pty Ltd
Licensed under the GNU General Public License v2 or later

*//****************************************************************//**

\class FFmpegImportFileHandle
\brief An ImportFileHandle for FFmpeg data

*//****************************************************************//**

\class FFmpegImportPlugin
\brief An ImportPlugin for FFmpeg data

*//*******************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include "../Audacity.h"	// needed before FFmpeg.h
#include "../FFmpeg.h"		// which brings in avcodec.h, avformat.h
#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#include <wx/window.h>
#endif


#define DESC _("FFmpeg-compatible files")

//TODO: remove non-audio extensions
static const wxChar *exts[] =
{   
   wxT("4xm"),
   wxT("MTV"),
   wxT("roq"),
   wxT("aac"),
   wxT("ac3"),
   wxT("aif"),
   wxT("aiff"),
   wxT("afc"),
   wxT("aifc"),
   wxT("al"),
   wxT("amr"),
   wxT("apc"),
   wxT("ape"),
   wxT("apl"),
   wxT("mac"),
   wxT("asf"),
   wxT("wmv"),
   wxT("wma"),
   wxT("au"),
   wxT("avi"),
   wxT("avs"),
   wxT("bethsoftvid"),
   wxT("c93"),
   wxT("302"),
   wxT("daud"),
   wxT("dsicin"),
   wxT("dts"),
   wxT("dv"),
   wxT("dxa"),
   wxT("ea"),
   wxT("cdata"),
   wxT("ffm"),
   wxT("film_cpk"),
   wxT("flac"),
   wxT("flic"),
   wxT("flv"),
   wxT("gif"),
   wxT("gxf"),
   wxT("idcin"),
   wxT("image2"),
   wxT("image2pipe"),
   wxT("cgi"),
   wxT("ipmovie"),
   wxT("nut"),
   wxT("lmlm4"),
   wxT("m4v"),
   wxT("mkv"),
   wxT("mm"),
   wxT("mmf"),
   wxT("mov"),
   wxT("mp4"),
   wxT("m4a"),
   wxT("3gp"),
   wxT("3g2"),
   wxT("mj2"),
   wxT("mp3"),
   wxT("mpc"),
   wxT("mpc8"),
   wxT("mpg"),
   wxT("mpeg"),
   wxT("ts"),
   wxT("mpegtsraw"),
   wxT("mpegvideo"),
   wxT("msnwctcp"),
   wxT("ul"),
   wxT("mxf"),
   wxT("nsv"),
   wxT("nuv"),
   wxT("ogg"),
   wxT("psxstr"),
   wxT("pva"),
   wxT("redir"),
   wxT("rl2"),
   wxT("rm"),
   wxT("ra"),
   wxT("rtsp"),
   wxT("s16be"),
   wxT("sw"),
   wxT("s8"),
   wxT("sb"),
   wxT("sdp"),
   wxT("shn"),
   wxT("siff"),
   wxT("vb"),
   wxT("son"),
   wxT("smk"),
   wxT("sol"),
   wxT("swf"),
   wxT("thp"),
   wxT("tiertexseq"),
   wxT("tta"),
   wxT("txd"),
   wxT("u16be"),
   wxT("uw"),
   wxT("ub"),
   wxT("u8"),
   wxT("vfwcap"),
   wxT("vmd"),
   wxT("voc"),
   wxT("wav"),
   wxT("wc3movie"),
   wxT("wsaud"),
   wxT("wsvqa"),
   wxT("wv")
};

#if defined(USE_FFMPEG)
// all the includes live here by default
#include "Import.h"
#include "ImportFFmpeg.h"
#include "../Tags.h"
#include "../Internat.h"
#include "../WaveTrack.h"
#include "ImportPlugin.h"

extern FFmpegLibs *FFmpegLibsInst;

class FFmpegImportFileHandle;

typedef struct _streamContext
{
   bool                 m_use;                           // TRUE = this stream will be loaded into Audacity
   AVStream            *m_stream;		      	         // an AVStream * in gDecFormatCtx->streams[]
   AVCodecContext      *m_codecCtx;			               // pointer to m_stream->codec

   AVPacket             m_pkt;				               // the last AVPacket we read for this stream
   int                  m_pktValid;			               // is m_pkt valid?
   uint8_t             *m_pktDataPtr;		           	   // pointer into m_pkt.data
   int                  m_pktRemainingSiz;	

   int64_t			      m_pts;			              	   // the current presentation time of the input stream
   int64_t			      m_ptsOffset;		    	         // packets associated with stream are relative to this

   int                  m_frameValid;		               // is m_decodedVideoFrame/m_decodedAudioSamples valid?
   int16_t             *m_decodedAudioSamples;           // decoded audio samples stored here
   unsigned int			m_decodedAudioSamplesSiz;        // current size of m_decodedAudioSamples
   int			         m_decodedAudioSamplesValidSiz;   // # valid bytes in m_decodedAudioSamples
   int                  m_initialchannels;               // number of channels allocated when we begin the importing. Assumes that number of channels doesn't change on the fly.
} streamContext;

class FFmpegImportPlugin : public ImportPlugin
{
public:
   FFmpegImportPlugin():
      ImportPlugin(wxArrayString(WXSIZEOF(exts),exts))
      {
         
      }

   ~FFmpegImportPlugin() { }

   wxString GetPluginFormatDescription();
   ImportFileHandle *Open(wxString Filename);
};

class FFmpegImportFileHandle : public ImportFileHandle
{

public:
   FFmpegImportFileHandle(const wxString & name);
   ~FFmpegImportFileHandle();

   bool Init();
   bool InitCodecs();


   wxString GetFileDescription();
   int GetFileUncompressedBytes();
   int Import(TrackFactory *trackFactory, Track ***outTracks,
      int *outNumTracks, Tags *tags);

   streamContext* ReadNextFrame();
   int DecodeFrame(streamContext *sc, bool flushing);
   int WriteData(streamContext *sc);
   void WriteMetadata(AVFormatContext *avf, Tags *tags);
   wxInt32 GetStreamCount()
   {
      return mNumStreams;
   }

   wxArrayString *GetStreamInfo()
   {
      return mStreamInfo;
   }

   void SetStreamUsage(wxInt32 StreamID, bool Use)
   {
      if (StreamID < mNumStreams)
         mScs[StreamID]->m_use = Use;
   }

private:

   AVFormatContext      *mFormatContext;
   int                   mNumStreams; // mNumstreams is less or equal to mFormatContext->nb_streams
   streamContext       **mScs;
   wxArrayString        *mStreamInfo;

   bool                  mCancelled;
   wxString              mName;
   void                 *mUserData;
   uint64_t              mNumSamples;
   uint64_t              mSamplesDone;
   WaveTrack           ***mChannels;
};


void GetFFmpegImportPlugin(ImportPluginList *importPluginList,
                           UnusableImportPluginList *unusableImportPluginList)
{
   importPluginList->Append(new FFmpegImportPlugin);
}


wxString FFmpegImportPlugin::GetPluginFormatDescription()
{
   return DESC;
}

ImportFileHandle *FFmpegImportPlugin::Open(wxString filename)
{

   // Open the file for import
   FFmpegImportFileHandle *handle = new FFmpegImportFileHandle(filename);

   bool success = handle->Init();
   if (!success) {
      delete handle;
      return NULL;
   }

   return handle;
}


FFmpegImportFileHandle::FFmpegImportFileHandle(const wxString & name)
:  ImportFileHandle(name)
{
   PickFFmpegLibs();

   mStreamInfo = new wxArrayString();
   mFormatContext = NULL;
   mNumStreams = 0;
   mScs = NULL;
   mCancelled =false;
   mName = name;
   mUserData = NULL;
   mNumSamples = 0;
   mSamplesDone = 0;
   mChannels = NULL;
}

bool FFmpegImportFileHandle::Init()
{
   FFmpegLibsInst->LoadLibs(NULL,false);

   if (!FFmpegLibsInst->ValidLibsLoaded()) return false;

   FFmpegLibsInst->av_log_set_callback(av_log_wx_callback);

   int err = FFmpegLibsInst->av_open_input_file(&mFormatContext,OSFILENAME(mName),NULL,0, NULL);
   if (err < 0)
   {
      wxLogMessage(wxT("FFmpeg : av_open_input_file() failed for file %s"),mName.c_str());
      return false;
   }

   err = FFmpegLibsInst->av_find_stream_info(mFormatContext);
   if (err < 0)
   {
      wxLogMessage(wxT("FFmpeg : av_find_stream_info() failed for file %s"),mName.c_str());
      return false;
   }

   if (mFormatContext->start_time != AV_NOPTS_VALUE)
   {
      //TODO: handle this situation (generate <start_time> ms of slience?)
      wxLogMessage(wxT("start_time = %d, that would be %d milliseconds."),mFormatContext->start_time,(mFormatContext->start_time/AV_TIME_BASE*1000));
      wxLogMessage(wxT("start_time support is not implemented in FFmpeg import plugin. Patches are welcome."));
   }

   InitCodecs();
   return true;
}

bool FFmpegImportFileHandle::InitCodecs()
{
   mScs = (streamContext**)malloc(sizeof(streamContext**)*mFormatContext->nb_streams);
   for (unsigned int i = 0; i < mFormatContext->nb_streams; i++)
   {
      if (mFormatContext->streams[i]->codec->codec_type == CODEC_TYPE_AUDIO)
      {
         streamContext *sc = new streamContext;
         memset(sc,0,sizeof(*sc));

         sc->m_stream = mFormatContext->streams[i];
         sc->m_codecCtx = sc->m_stream->codec;

         if (sc->m_stream->start_time != AV_NOPTS_VALUE)
         {
            //TODO: same as before
            wxLogMessage(wxT("start_time = %d, that would be %d milliseconds."),sc->m_stream->start_time,(sc->m_stream->start_time/AV_TIME_BASE*1000));
            wxLogMessage(wxT("start_time support is not implemented in FFmpeg import plugin. Patches are welcome."));
         }

         AVCodec *codec = FFmpegLibsInst->avcodec_find_decoder(sc->m_codecCtx->codec_id);
         if (codec == NULL)
         {
            wxLogMessage(wxT("FFmpeg : avcodec_find_decoder() failed. Index[%02d], Codec[%02x - %s]"),i,sc->m_codecCtx->codec_id,sc->m_codecCtx->codec_name);
            //FFmpeg can't decode this stream, skip it
            delete sc;
            continue;
         }
         if (codec->type != sc->m_codecCtx->codec_type)
         {
            wxLogMessage(wxT("FFmpeg : Codec type mismatch, skipping. Index[%02d], Codec[%02x - %s]"),i,sc->m_codecCtx->codec_id,sc->m_codecCtx->codec_name);
            //Non-audio codec reported as audio? Nevertheless, we don't need THIS.
            delete sc;
            continue;
         }

         if (FFmpegLibsInst->avcodec_open(sc->m_codecCtx, codec) < 0)
         {
            wxLogMessage(wxT("FFmpeg : avcodec_open() failed. Index[%02d], Codec[%02x - %s]"),i,sc->m_codecCtx->codec_id,sc->m_codecCtx->codec_name);
            //Can't open decoder - skip this stream
            delete sc;
            continue;
         }

         wxString strinfo;
         strinfo.Printf(_("Index[%02x] Codec[%S], Language[%S], Duration[%d]"),sc->m_stream->id,codec->name,sc->m_stream->language,sc->m_stream->duration);
         mStreamInfo->Add(strinfo);
         mScs[mNumStreams++] = sc;
      }
      //for video and unknown streams do nothing
   }
   return true;
}

wxString FFmpegImportFileHandle::GetFileDescription()
{
   return DESC;
}


int FFmpegImportFileHandle::GetFileUncompressedBytes()
{
   // TODO: Get Uncompressed byte count.
   return 0;
}


int FFmpegImportFileHandle::Import(TrackFactory *trackFactory,
              Track ***outTracks,
              int *outNumTracks,
              Tags *tags)
{

   CreateProgress();

   for (int i = 0; i < mNumStreams;)
   {
      if (!mScs[i]->m_use)
      {
         delete mScs[i];
         for (int j = i; j < mNumStreams - 1; j++)
         {
            mScs[j] = mScs[j+1];
         }
         mNumStreams--;
      }
      else i++;
   }

   mChannels = new WaveTrack **[mNumStreams];
   mNumSamples = 0;

   for (int s = 0; s < mNumStreams; s++)
   {
      mNumSamples += mScs[s]->m_stream->nb_frames;
      mScs[s]->m_initialchannels = mScs[s]->m_stream->codec->channels;
      mChannels[s] = new WaveTrack *[mScs[s]->m_stream->codec->channels];
      int c;
      for (c = 0; c < mScs[s]->m_stream->codec->channels; c++)
      {
         mChannels[s][c] = trackFactory->NewWaveTrack(int16Sample, mScs[s]->m_stream->codec->sample_rate);

         if (mScs[s]->m_stream->codec->channels == 2)
         {
            switch (c)
            {
            case 0:
               mChannels[s][c]->SetChannel(Track::LeftChannel);
               mChannels[s][c]->SetLinked(true);
               break;
            case 1:
               mChannels[s][c]->SetChannel(Track::RightChannel);
               mChannels[s][c]->SetTeamed(true);
               break;
            }
         }
         else
         {
            mChannels[s][c]->SetChannel(Track::MonoChannel);
         }
      }
   }

   streamContext *sc = NULL;
   int res = 0;
   while ((sc = ReadNextFrame()) != NULL && (res == 0))
   {
      if (sc != (streamContext*)1)
      {
         while (sc->m_pktRemainingSiz > 0 && (res == 0))
         {
            if (DecodeFrame(sc,false) < 0)
               break;


            if (sc->m_frameValid)
               res = WriteData(sc);

         }

         if (sc->m_pktValid)
         {
            av_free_packet(&sc->m_pkt);
            sc->m_pktValid = 0;
         }    
      }
   }

   // Flush the decoders.
   if ((mNumStreams != 0) && (res == 0))
   {
      for (int i = 0; i < mNumStreams; i++)
      {
         if (DecodeFrame(mScs[i], 1) == 0)
         {
            WriteData(mScs[i]);

            if (mScs[i]->m_pktValid)
            {
               av_free_packet(&mScs[i]->m_pkt);
               mScs[i]->m_pktValid = 0;
            }				
         }
      }
   }

   if (res)
   {
      for (int s = 0; s < mNumStreams; s++)
      {
         delete[] mChannels[s];
      }
      delete[] mChannels;

      if (mCancelled)
         return eImportCancelled;
      else
         return eImportFailed;
   }

   *outNumTracks = 0;
   for (int s = 0; s < mNumStreams; s++)
   {
      *outNumTracks += mScs[s]->m_stream->codec->channels;
   }

   *outTracks = new Track *[*outNumTracks];

   int trackindex = 0;
   for (int s = 0; s < mNumStreams; s++)
   {
      for(int c = 0; c < mScs[s]->m_stream->codec->channels; c++)
      {
         mChannels[s][c]->Flush();
         (*outTracks)[trackindex++] = mChannels[s][c];
      }
      delete[] mChannels[s];
   }
   delete[] mChannels;


   WriteMetadata(mFormatContext,tags);

   return eImportSuccess;
}

streamContext *FFmpegImportFileHandle::ReadNextFrame()
{
   streamContext *sc = NULL;
   AVPacket pkt;

   if (FFmpegLibsInst->av_read_frame(mFormatContext,&pkt) < 0)
   {
      return NULL;
   }

   for (int i = 0; i < mNumStreams; i++)
   {
      if (mScs[i]->m_stream->index == pkt.stream_index)
         sc = mScs[i];
   }

   //Off-stream packet. Don't panic, just skip it.
   //When not all streams are selected for import this will happen very often.
   if (sc == NULL)
   {
      av_free_packet(&pkt);
      return (streamContext*)1;
   }

   memcpy(&sc->m_pkt, &pkt, sizeof(AVPacket));

   sc->m_pktValid = 1;
   sc->m_pktDataPtr = pkt.data;
   sc->m_pktRemainingSiz = pkt.size;

   return sc;
}

int FFmpegImportFileHandle::DecodeFrame(streamContext *sc, bool flushing)
{
   int		nBytesDecoded;			
   wxUint8 *pDecode = sc->m_pktDataPtr;
   int		nDecodeSiz = sc->m_pktRemainingSiz;

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

   // Reallocate the audio sample buffer if it's smaller than the frame size.
   if (!flushing)
   {
      // av_fast_realloc() will only reallocate the buffer if m_decodedAudioSamplesSiz is 
      // smaller than third parameter. It also returns new size in m_decodedAudioSamplesSiz
      //\warning { for some reason using the following macro call right in the function call
      //causes Audacity to crash in some unknown place. With "newsize" it works fine }
      int newsize = FFMAX(sc->m_pkt.size*sizeof(*sc->m_decodedAudioSamples), AVCODEC_MAX_AUDIO_FRAME_SIZE);
      sc->m_decodedAudioSamples = (int16_t*)FFmpegLibsInst->av_fast_realloc(sc->m_decodedAudioSamples, 
         &sc->m_decodedAudioSamplesSiz,
         newsize
         );

      if (sc->m_decodedAudioSamples == NULL)
      {
         //Can't allocate bytes
         return -1;
      }
   }

   // avcodec_decode_audio2() expects the size of the output buffer as the 3rd parameter but
   // also returns the number of bytes it decoded in the same parameter.
   sc->m_decodedAudioSamplesValidSiz = sc->m_decodedAudioSamplesSiz;
   nBytesDecoded = FFmpegLibsInst->avcodec_decode_audio2(sc->m_codecCtx, 
      sc->m_decodedAudioSamples,		// out
      &sc->m_decodedAudioSamplesValidSiz,	// in/out
      pDecode, nDecodeSiz);				// in

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

int FFmpegImportFileHandle::WriteData(streamContext *sc)
{

   size_t pos = 0;
   int streamid = -1;
   for (int i = 0; i < mNumStreams; i++)
   {
      if (mScs[i] == sc)
      {
         streamid = i;
         break;
      }
   }
   if (streamid == -1)
   {
      return 1;
   }

   int nChannels = sc->m_stream->codec->channels < sc->m_initialchannels ? sc->m_stream->codec->channels : sc->m_initialchannels;
   int16_t **tmp = (int16_t**)malloc(sizeof(short*)*nChannels);
   for (int chn = 0; chn < nChannels; chn++)
   {
      tmp[chn] = (int16_t*)malloc(sizeof(int16_t)*sc->m_decodedAudioSamplesValidSiz/sizeof(int16_t)/nChannels);
   }

   int index = 0;
   while (pos < sc->m_decodedAudioSamplesValidSiz/sizeof(int16_t))
   {
      for (int chn=0; chn < nChannels; chn++)
      {
         tmp[chn][index] = sc->m_decodedAudioSamples[pos];
         pos++;
      }
      index++;
   }

   for (int chn=0; chn < nChannels; chn++)
   {
      mChannels[streamid][chn]->Append((samplePtr)tmp[chn],int16Sample,index);
      free(tmp[chn]);
   }

   free(tmp);
   
   int tsize = FFmpegLibsInst->url_fsize(mFormatContext->pb);
   if (!mProgress->Update((wxLongLong)this->mFormatContext->pb->pos, (wxLongLong)(tsize > 0 ? tsize : 1))) {
      mCancelled = true;
      return 1;
   }

   return 0;
}

void FFmpegImportFileHandle::WriteMetadata(AVFormatContext *avf,Tags *tags)
{

   tags->Clear();


   tags->SetTag(TAG_TITLE,wxString((const wxChar*)avf->title));
   tags->SetTag(TAG_ARTIST,wxString((const wxChar*)avf->author));
   //tags->SetTag(TAG_COPYRIGHT,avf->copyright);
   tags->SetTag(TAG_COMMENTS,wxString((const wxChar*)avf->comment));
   tags->SetTag(TAG_ALBUM,wxString((const wxChar*)avf->album));
   tags->SetTag(TAG_YEAR,avf->year);
   tags->SetTag(TAG_TRACK,avf->track);
   tags->SetTag(TAG_GENRE,wxString((const wxChar*)avf->genre));

}


FFmpegImportFileHandle::~FFmpegImportFileHandle()
{
   if (FFmpegLibsInst->ValidLibsLoaded())
   {
      if (mFormatContext) FFmpegLibsInst->av_close_input_file(mFormatContext);
      FFmpegLibsInst->av_log_set_callback(FFmpegLibsInst->av_log_default_callback);
   }

   for (int i = 0; i < mNumStreams; i++)
   {
      if (mScs[i]->m_decodedAudioSamples != NULL)
         FFmpegLibsInst->av_free(mScs[i]->m_decodedAudioSamples);

      delete mScs[i];
   }

   DropFFmpegLibs();
}

#endif //USE_FFMPEG
