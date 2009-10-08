/*
 *  ODDecodeFlacTask.cpp
 *  Audacity
 *
 *  Created by apple on 8/10/08.
 *  Copyright 2008 __MyCompanyName__. All rights reserved.
 *
 */

#include "ODDecodeFlacTask.h"

#include "../Prefs.h"
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

ODDecodeFlacTask::~ODDecodeFlacTask()
{
}


ODTask* ODDecodeFlacTask::Clone()
{
   ODDecodeFlacTask* clone = new ODDecodeFlacTask;
   clone->mDemandSample=GetDemandSample();

   //TODO: do we need to add the decoders and blockfiles?  
   //this can be tested when the track is copied and pasted.  I don't think we need to
   //because the blockfiles are not copies, and the decoders are created from scratch 
   //with CreateOrGetMatchingDecoder()
   return clone;
   
}


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

   
   for (unsigned int s=0; s<frame->header.blocksize && mDecoder->mDecodeBufferWritePosition+s<mDecoder->mDecodeBufferLen; s++) {
      mDecoder->mDecodeBuffer[s+mDecoder->mDecodeBufferWritePosition]=buffer[mDecoder->mTargetChannel][s];
   }
   mDecoder->mDecodeBufferWritePosition+=frame->header.blocksize;
/*      
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
*/

   mDecoder->mSamplesDone += frame->header.blocksize;
   return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
   
//   mDecoder->mUpdateResult = mDecoder->mProgress->Update((wxULongLong_t) mDecoder->mSamplesDone, mDecoder->mNumSamples != 0 ? (wxULongLong_t)mDecoder->mNumSamples : 1);
/*
   if (mDecoder->mUpdateResult != eProgressSuccess)
   {
      return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
   }

   return FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
   */
}


//--Decoder stuff:
   ///Decodes the samples for this blockfile from the real file into a float buffer.  
   ///This is file specific, so subclasses must implement this only.
   ///the buffer was defined like
   ///samplePtr sampleData = NewSamples(mLen, floatSample);
   ///this->ReadData(sampleData, floatSample, 0, mLen);
   ///This class should call ReadHeader() first, so it knows the length, and can prepare 
   ///the file object if it needs to. 
void ODFlacDecoder::Decode(samplePtr & data, sampleFormat & format, sampleCount start, sampleCount len, unsigned int channel)
{

   //we need to lock this so the target stays fixed over the seek/write callback.
   mFlacFileLock.Lock();

   mDecodeBufferWritePosition=0;
   mDecodeBufferLen = len;
	//TODO: Do
   data = NewSamples(len, mFormat);
   mDecodeBuffer=data;
   format = mFormat;
   
   mTargetChannel=channel;
   
   if(!mFile->seek_absolute(start))
   {
   //TODO:Warn!
      mFlacFileLock.Unlock();
      return;
   }   
   
   //read with call back until len is satisfied.
   unsigned int flacDecoderBlocksize = mFile->get_blocksize();
   
   //we want to write TO the data parameter.
   for(sampleCount samples = start;samples<start+len;samples+=flacDecoderBlocksize)
   {
      mFile->process_single();
   }
   
   mFlacFileLock.Unlock();
   //insert into blockfile and
   //calculate summary happen in ODDecodeBlockFile::WriteODDecodeBlockFile, where this method is also called.
}
   
///Read header.  Subclasses must override.  Probably should save the info somewhere.
///Ideally called once per decoding of a file.  This complicates the task because 
///returns true if the file exists and the header was read alright.

//Note:we are not using LEGACY_FLAC defs (see ImportFlac.cpp FlacImportFileHandle::Init()
//this code is based on that function.
bool ODFlacDecoder::ReadHeader()
{
   mFormat = (sampleFormat)
      gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample);
   if(mFile) 
      delete mFile;
   mFile = new ODFLACFile(this);


   if (!mHandle.Open(mFName, wxT("rb"))) {
      return false;
   }

   // Even though there is an init() method that takes a filename, use the one that
   // takes a file handle because wxWidgets can open a file with a Unicode name and
   // libflac can't (under Windows).
   //
   // Responsibility for closing the file is passed to libflac.
   // (it happens when mFile->finish() is called)
   bool result = mFile->init(mHandle.fp())?true:false;
   mHandle.Detach();

   if (result != FLAC__STREAM_DECODER_INIT_STATUS_OK) {
      return false;
   }

	//this will call the metadata_callback when it is done   
   mFile->process_until_end_of_metadata();
   // not necessary to check state, error callback will catch errors, but here's how:
   if (mFile->get_state() > FLAC__STREAM_DECODER_READ_FRAME) {
      return false;
   }

   if (!mFile->is_valid() || mFile->get_was_error()) {
      // This probably is not a FLAC file at all
      return false;
   }
   return true;

}  

ODFLACFile* ODFlacDecoder::GetFlacFile()
{
   return mFile;
}

ODFlacDecoder::~ODFlacDecoder(){
   if(mFile) 
   {
      mFile->finish();
      delete mFile;
   }
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

   
   mDecoders.push_back(decoder);
   return decoder;

}

