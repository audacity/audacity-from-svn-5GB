/*
 *  ODDecodeFlacTask.cpp
 *  Audacity
 *
 *  Created by apple on 8/10/08.
 *  Copyright 2008 __MyCompanyName__. All rights reserved.
 *
 */

#include "ODDecodeFlacTask.h"

#include <wx/string.h>
#include <wx/utils.h>
#include <wx/file.h>
#include <wx/ffile.h>

#ifdef USE_LIBID3TAG 
extern "C" {
#include <id3tag.h>
}
#endif

#include "../Tags.h"

#define FLAC_HEADER "fLaC"

#define DESC _("FLAC files")

static const wxChar *exts[] =
{
   wxT("flac"),
   wxT("flc")
};


void ODFLACFile::metadata_callback(const FLAC__StreamMetadata *metadata)
{
   switch (metadata->type)
   {
      case FLAC__METADATA_TYPE_VORBIS_COMMENT:
         for (FLAC__uint32 i = 0; i < metadata->data.vorbis_comment.num_comments; i++) {
            mComments.Add(UTF8CTOWX((char *)metadata->data.vorbis_comment.comments[i].entry));
         }
      break;

      case FLAC__METADATA_TYPE_STREAMINFO:
         mDecoder->mSampleRate=metadata->data.stream_info.sample_rate;
         mDecoder->mNumChannels=metadata->data.stream_info.channels;
         mDecoder->mBitsPerSample=metadata->data.stream_info.bits_per_sample;
         mDecoder->mNumSamples=metadata->data.stream_info.total_samples;

         if (mDecoder->mBitsPerSample<=16) {
            if (mDecoder->mFormat<int16Sample) {
               mDecoder->mFormat=int16Sample;
            }
         } else if (mDecoder->mBitsPerSample<=24) {
            if (mDecoder->mFormat<int24Sample) {
               mDecoder->mFormat=int24Sample;
            }
         } else {
            mDecoder->mFormat=floatSample;
         }
         mDecoder->mStreamInfoDone=true;
      break;
	  // handle the other types we do nothing with to avoid a warning
	  case FLAC__METADATA_TYPE_PADDING:	// do nothing with padding
	  case FLAC__METADATA_TYPE_APPLICATION:	// no idea what to do with this
	  case FLAC__METADATA_TYPE_SEEKTABLE:	// don't need a seektable here
	  case FLAC__METADATA_TYPE_CUESHEET:	// convert this to labels?
	  case FLAC__METADATA_TYPE_PICTURE:		// ignore pictures
	  case FLAC__METADATA_TYPE_UNDEFINED:	// do nothing with this either
	  break;
   }
}

void ODFLACFile::error_callback(FLAC__StreamDecoderErrorStatus status)
{
   mWasError = true;
   
   /*
   switch (status)
   {
   case FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC:
      wxPrintf(wxT("Flac Error: Lost sync\n"));
      break;
   case FLAC__STREAM_DECODER_ERROR_STATUS_FRAME_CRC_MISMATCH:
      wxPrintf(wxT("Flac Error: Crc mismatch\n"));
      break;
   case FLAC__STREAM_DECODER_ERROR_STATUS_BAD_HEADER:
      wxPrintf(wxT("Flac Error: Bad Header\n"));
      break;
   default:
      wxPrintf(wxT("Flac Error: Unknown error code\n"));
      break;
   }*/
}


//the inside of the read loop.
FLAC__StreamDecoderWriteStatus ODFLACFile::write_callback(const FLAC__Frame *frame,
							  const FLAC__int32 * const buffer[])
{
   short *tmp=new short[frame->header.blocksize];

   for (unsigned int chn=0; chn<mDecoder->mNumChannels; chn++) {
      if (frame->header.bits_per_sample == 16) {
         for (unsigned int s=0; s<frame->header.blocksize; s++) {
            tmp[s]=buffer[chn][s];
         }

         mDecoder->mChannels[chn]->Append((samplePtr)tmp,
                  int16Sample,
                  frame->header.blocksize);
      }
      else {
         mDecoder->mChannels[chn]->Append((samplePtr)buffer[chn],
                  int24Sample,
                  frame->header.blocksize);
      }
   }

   delete [] tmp;

   mDecoder->mSamplesDone += frame->header.blocksize;

//   mDecoder->mUpdateResult = mDecoder->mProgress->Update((wxULongLong_t) mDecoder->mSamplesDone, mDecoder->mNumSamples != 0 ? (wxULongLong_t)mDecoder->mNumSamples : 1);

   if (mDecoder->mUpdateResult != eProgressSuccess)
   {
      return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
   }

   return FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
}

//--Decoder stuff:
   ///Decodes the samples for this blockfile from the real file into a float buffer.  
   ///This is file specific, so subclasses must implement this only.
   ///the buffer was defined like
   ///samplePtr sampleData = NewSamples(mLen, floatSample);
   ///this->ReadData(sampleData, floatSample, 0, mLen);
   ///This class should call ReadHeader() first, so it knows the length, and can prepare 
   ///the file object if it needs to. 
void ODFlacDecoder::Decode(samplePtr data, sampleFormat format, sampleCount start, sampleCount len)
{
	//TODO: Do
}
   
///Read header.  Subclasses must override.  Probably should save the info somewhere.
///Ideally called once per decoding of a file.  This complicates the task because 
///returns true if the file exists and the header was read laright.
bool ODFlacDecoder::ReadHeader()
{
	//this will call the metadata_callback when it is done.
}  



///Creates an ODFileDecoder that decodes a file of filetype the subclass handles.
//
//compare to FLACImportPlugin::Open(wxString filename)
ODFileDecoder* ODDecodeFlacTask::CreateFileDecoder(const wxString & fileName)
{
	// First check if it really is a FLAC file

   int cnt;
   wxFile binaryFile;
   if (!binaryFile.Open(fileName)) {
      return NULL; // File not found
   }

#ifdef USE_LIBID3TAG
   // Skip any ID3 tags that might be present
   id3_byte_t query[ID3_TAG_QUERYSIZE];
   cnt = binaryFile.Read(query, sizeof(query));
   cnt = id3_tag_query(query, cnt);
   binaryFile.Seek(cnt);
#endif   

   char buf[5];
   cnt = binaryFile.Read(buf, 4);
   binaryFile.Close();

   if (cnt == wxInvalidOffset || strncmp(buf, FLAC_HEADER, 4) != 0) {
      // File is not a FLAC file
      return NULL; 
   }
   
   // Open the file for import
   ODFlacDecoder *decoder = new ODFlacDecoder(fileName);


   bool success = decoder->Init();
   if (!success) {
      delete decoder;
      return NULL;
   }

   return decoder;

}

