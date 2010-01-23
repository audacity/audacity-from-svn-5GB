/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportQT.cpp

  Joshua Haberman

  Handles importing MPEG-4 audio files, including AAC and Apple Lossless,
  on Mac OS X.  Could be modified to support QuickTime importing on
  Windows, too.

**********************************************************************/

#include "../Audacity.h"
#include "../Tags.h"
#include "ImportQT.h"
#include "ImportPlugin.h"
#include "wx/intl.h"

#define DESC _("QuickTime files")

static const wxChar *exts[] =
{
   wxT("mov"),
   wxT("aac"),
   wxT("m4a")
};

#ifndef USE_QUICKTIME

void GetQTImportPlugin(ImportPluginList *importPluginList,
                       UnusableImportPluginList *unusableImportPluginList)
{
   UnusableImportPlugin* qtIsUnsupported =
      new UnusableImportPlugin(DESC, wxArrayString(WXSIZEOF(exts), exts));

   unusableImportPluginList->Append(qtIsUnsupported);
}

#else /* USE_QUICKTIME */

#define Track XTrack

// gives us routines to convert between filenames and FSSpec
#ifndef WIN32
#include <wx/mac/private.h>
#endif
#include <wx/msgdlg.h>

// I'm not sure what __MACH__ is important for, this list of headers is
// copied verbatim from the Apple sample code
#ifdef __MACH__
   #include <Carbon/Carbon.h>
   #include <QuickTime/QuickTime.h>
#else
   #include <ConditionalMacros.h>
   #include <Movies.h>
   #include <QuickTimeComponents.h>
   #include <Sound.h>
   #include <Folders.h>
   #include <ToolUtils.h>
   #include <Gestalt.h>
   #include <Navigation.h>
#endif

#undef Track

#include "../WaveTrack.h"

// Prototype for the callback
static pascal Boolean
SoundConverterFillBufferCallback(SoundComponentDataPtr *outData, void *userData);


Media GetMediaFromMovie(Movie mov);

// Structure used to pass data to the sound converter fill buffer callback
struct CallbackData
{
   ExtendedSoundComponentData compData;
   Handle hSource; // source media buffer
   Media sourceMedia; // sound media identifier
   TimeValue getMediaAtThisTime;
   TimeValue sourceDuration;
   UInt32 maxBufferSize;
   Boolean isThereMoreSource;
   Boolean isSourceVBR;
};

class QTImportPlugin : public ImportPlugin
{
public:
   QTImportPlugin():
      ImportPlugin(wxArrayString(WXSIZEOF(exts), exts))
   {
   }

   ~QTImportPlugin() { }

   wxString GetPluginFormatDescription();
   ImportFileHandle *Open(wxString Filename);
};

class QTImportFileHandle : public ImportFileHandle
{
public:
   QTImportFileHandle(Movie movie, Media media):
      mMovie(movie),
      mMedia(media),
      mProgressCallback(NULL),
      mUserData(NULL)
   {
   }
   ~QTImportFileHandle() { }

   wxString GetFileDescription();
   int GetFileUncompressedBytes();

   void SetProgressCallback(progress_callback_t function,
                            void *userData);

   bool Import(TrackFactory *trackFactory, Track ***outTracks,
               int *outNumTracks, Tags *tags);
private:
   void AddMetadata(Tags *tags);

   Movie mMovie;
   Media mMedia;
   progress_callback_t mProgressCallback;
   void *mUserData;
};


void GetQTImportPlugin(ImportPluginList *importPluginList,
                       UnusableImportPluginList *unusableImportPluginList)
{
   importPluginList->Append(new QTImportPlugin);
}


wxString QTImportPlugin::GetPluginFormatDescription()
{
   return DESC;
}

ImportFileHandle *QTImportPlugin::Open(wxString Filename)
{
   FSSpec inFile;
   FInfo fileInfo;
   Movie theMovie = 0;
   Media theMedia;

   // Make sure QuickTime is initialized
   //::EnterMovies();

#ifdef WIN32
   char* specFilename = strdup(Filename.GetData());
   if (FSMakeFSSpec(0,0,c2pstr(specFilename), &inFile) != noErr)
   {
       free(specFilename);
       return false;
   }
   free(specFilename);
#else
   wxMacFilename2FSSpec(Filename, &inFile);
#endif

   OSErr err = FSpGetFInfo(&inFile, &fileInfo);

   if(err != noErr)
      return NULL;

   if (kQTFileTypeSystemSevenSound == fileInfo.fdType)
   {
      // TODO: handle this case.  it is special because system seven sounds cannot
      // be opened in place
   }
   else
   {
      short theRefNum;
      short theResID = 0;  // we want the first movie
      Boolean wasChanged;

      // open the movie file
      err = OpenMovieFile(&inFile, &theRefNum, fsRdPerm);
      if(err != noErr)
         return NULL;

      // instantiate the movie
      err = NewMovieFromFile(&theMovie, theRefNum, &theResID, NULL, newMovieActive, &wasChanged);
      CloseMovieFile(theRefNum);
      if(err != noErr)
         return NULL;
   }


   // get and return the sound track media
   theMedia = GetMediaFromMovie(theMovie);
   if(theMedia == NULL)
      return NULL;

   return new QTImportFileHandle(theMovie, theMedia);
}


wxString QTImportFileHandle::GetFileDescription()
{
   return DESC;
}

int QTImportFileHandle::GetFileUncompressedBytes()
{
   return 0;
}

void QTImportFileHandle::SetProgressCallback(progress_callback_t function,
                            void *userData)
{
   mProgressCallback = function;
   mUserData = userData;
}

bool QTImportFileHandle::Import(TrackFactory *trackFactory, Track ***outTracks,
                                  int *outNumTracks, Tags *tags)
{
   OSErr err = noErr;

   //
   // Determine the file format.
   //

   // GetMediaSampleDescription takes a SampleDescriptionHandle, but apparently
   // if the media is a sound (which presumably we know it is) then it will treat
   // it as a SoundDescriptionHandle (which in addition to the format of single
   // samples, also tells you sample rate, number of channels, etc.)
   // Pretty messed up interface, if you ask me.
   SoundDescriptionHandle soundDescription = (SoundDescriptionHandle)NewHandle(0);
   GetMediaSampleDescription(mMedia, 1, (SampleDescriptionHandle)soundDescription);

   // If this is a compressed format, it may have out-of-stream compression
   // parameters that need to be passed to the sound converter.  We retrieve
   // these in the form of an audio atom.  To do this, however we have to
   // get the data by way of a handle, then copy it manually from the handle to
   // the atom.  These interfaces get worse all the time!
   Handle decompressionParamsHandle = NewHandle(0);
   AudioFormatAtomPtr decompressionParamsAtom = NULL;
   err = GetSoundDescriptionExtension(soundDescription, &decompressionParamsHandle,
                                      siDecompressionParams);
   if(err == noErr)
   {
      // this stream has decompression parameters.  copy from the handle to the atom.
      int paramsSize = GetHandleSize(decompressionParamsHandle);
      HLock(decompressionParamsHandle);
      decompressionParamsAtom = (AudioFormatAtomPtr)NewPtr(paramsSize);
      //err = MemError();
      BlockMoveData(*decompressionParamsHandle, decompressionParamsAtom, paramsSize);
      HUnlock(decompressionParamsHandle);
   }

   if(decompressionParamsHandle)
      DisposeHandle(decompressionParamsHandle);

   //
   // Now we set up a sound converter to decompress the data if it is compressed.
   //

   SoundComponentData inputFormat;
   SoundComponentData outputFormat;
   SoundConverter     soundConverter = NULL;

   inputFormat.flags       = outputFormat.flags       = 0;
   inputFormat.sampleCount = outputFormat.sampleCount = 0;
   inputFormat.reserved    = outputFormat.reserved    = 0;
   inputFormat.buffer      = outputFormat.buffer      = NULL;
   inputFormat.numChannels = outputFormat.numChannels = (*soundDescription)->numChannels;
   inputFormat.sampleSize  = outputFormat.sampleSize  = (*soundDescription)->sampleSize;
   inputFormat.sampleRate  = outputFormat.sampleRate  = (*soundDescription)->sampleRate;

   inputFormat.format = (*soundDescription)->dataFormat;
   outputFormat.format = kSoundNotCompressed;

   err = SoundConverterOpen(&inputFormat, &outputFormat, &soundConverter);

   //
   // Create the Audacity WaveTracks to house the new data
   //

   WaveTrack **channels = new WaveTrack *[outputFormat.numChannels];

   // determine sample format

   sampleFormat format;
   int bytesPerSample;

   // TODO: do we know for sure that 24 and 32 bit samples are the same kind
   // of 24 and 32 bit samples we expect?
   switch(outputFormat.sampleSize) {
      case 16:
         format = int16Sample;
         bytesPerSample = 2;
         break;

      case 24:
         format = int24Sample;
         bytesPerSample = 3;
         break;

      case 32:
         format = floatSample;
         bytesPerSample = 4;
         break;

      default:
         printf("I can't import a %d-bit file!\n", outputFormat.sampleSize);
         return false;
   }

   int c;
   for (c = 0; c < outputFormat.numChannels; c++)
   {
      channels[c] = trackFactory->NewWaveTrack(format);
      channels[c]->SetRate(outputFormat.sampleRate / 65536.0);

      if(outputFormat.numChannels == 2)
      {
         if(c == 0)
         {
            channels[c]->SetChannel(Track::LeftChannel);
            channels[c]->SetLinked(true);
         }
         else if(c == 1)
         {
            channels[c]->SetChannel(Track::RightChannel);
         }
      }
   }

   //
   // Give the converter the decompression atom.
   //

   // (judging from the sample code, it's OK if the atom is NULL, which
   // it will be if there was no decompression information)

   err = SoundConverterSetInfo(soundConverter, siDecompressionParams, decompressionParamsAtom);
   if(err == siUnknownInfoType)
   {
      // the decompressor didn't need the decompression atom, but that's ok.
      err = noErr;
   }

   // Tell the converter we're cool with VBR audio
   SoundConverterSetInfo(soundConverter, siClientAcceptsVBR, Ptr(true));

   //
   // Determine buffer sizes and allocate output buffer
   //

   int inputBufferSize = 655360;
   int outputBufferSize = 524288;
   char *outputBuffer = new char[outputBufferSize];

   //
   // Populate the structure of data that is passed to the callback
   //

   CallbackData cbData;
   memset(&cbData.compData, 0, sizeof(ExtendedSoundComponentData));

   cbData.isSourceVBR        = ((*soundDescription)->compressionID == variableCompression);
   cbData.sourceMedia        = mMedia;
   cbData.getMediaAtThisTime = 0;
   cbData.sourceDuration     = GetMediaDuration(mMedia);
   cbData.isThereMoreSource  = true;
   cbData.maxBufferSize      = inputBufferSize;

   // allocate source media buffer
   cbData.hSource            = NewHandle((long)cbData.maxBufferSize);
   MoveHHi(cbData.hSource);
   HLock(cbData.hSource);

   cbData.compData.desc = inputFormat;
   cbData.compData.desc.buffer = (BytePtr)*cbData.hSource;

   cbData.compData.desc.flags = kExtendedSoundData;
   cbData.compData.extendedFlags = kExtendedSoundBufferSizeValid |
                                   kExtendedSoundSampleCountNotValid;
   if(cbData.isSourceVBR)
      cbData.compData.extendedFlags |= kExtendedSoundCommonFrameSizeValid;

   cbData.compData.bufferSize = 0; // filled in during callback

   // this doesn't make sense to me, but it is taken from sample code
   cbData.compData.recordSize = sizeof(ExtendedSoundComponentData);


   //
   // Begin the Conversion
   //

   err = SoundConverterBeginConversion(soundConverter);

   SoundConverterFillBufferDataUPP fillBufferUPP;
   fillBufferUPP = NewSoundConverterFillBufferDataUPP(SoundConverterFillBufferCallback);

   bool done = false;
   bool cancelled = false;
   sampleCount samplesSinceLastCallback = 0;
   UInt32 outputFrames;
   UInt32 outputBytes;
   UInt32 outputFlags;

#define SAMPLES_PER_CALLBACK 10000

   while(!done && !cancelled)
   {
      err = SoundConverterFillBuffer(soundConverter,    // a sound converter
                                     fillBufferUPP,     // the callback
                                     &cbData,           // refCon passed to FillDataProc
                                     outputBuffer,      // the buffer to decompress into
                                     outputBufferSize,  // size of that buffer
                                     &outputBytes,      // number of bytes actually output
                                     &outputFrames,     // number of frames actually output
                                     &outputFlags);     // fillbuffer retured advisor flags
      if (err)
         break;

      if((outputFlags & kSoundConverterHasLeftOverData) == false)
         done = true;

      for(c = 0; c < outputFormat.numChannels; c++)
         channels[c]->Append(outputBuffer + (c*bytesPerSample),
                             format,
                             outputFrames,
                             outputFormat.numChannels);

      samplesSinceLastCallback += outputFrames;
      if( samplesSinceLastCallback > SAMPLES_PER_CALLBACK )
      {
         if( mProgressCallback )
            cancelled = mProgressCallback(mUserData,
                                          (float)cbData.getMediaAtThisTime /
                                          cbData.sourceDuration);
         samplesSinceLastCallback -= SAMPLES_PER_CALLBACK;
      }
   }

   HUnlock(cbData.hSource);

   // Flush any remaining data to the output buffer.
   // It appears that we have no way of telling this routine how big the output
   // buffer is!  We had better hope that there isn't more data left than
   // the buffer is big.
   SoundConverterEndConversion(soundConverter, outputBuffer, &outputFrames, &outputBytes);

   for(c = 0; c < outputFormat.numChannels; c++)
   {
       channels[c]->Append(outputBuffer + (c*bytesPerSample),
                           format,
                           outputFrames,
                          outputFormat.numChannels);
      channels[c]->Flush();
   }

   bool res = (!cancelled && err == noErr);

   //
   // Extract any metadata
   //
   if (res) {
      AddMetadata(tags);
   }

   delete[] outputBuffer;
   DisposeHandle(cbData.hSource);
   SoundConverterClose(soundConverter);
   DisposeMovie(mMovie);

   if (!res) {
      for (c = 0; c < outputFormat.numChannels; c++)
         delete channels[c];
      delete[] channels;

      return false;
   }

   *outNumTracks = outputFormat.numChannels;
   *outTracks = new Track *[outputFormat.numChannels];
   for(c = 0; c < outputFormat.numChannels; c++)
         (*outTracks)[c] = channels[c];
      delete[] channels;

      return true;
   }

static const struct
{
   OSType key;
   wxChar *name;
}
names[] =
{
   {  kQTMetaDataCommonKeyAuthor,         wxT("Author")           },
   {  kQTMetaDataCommonKeyComment,        TAG_COMMENTS            },
   {  kQTMetaDataCommonKeyCopyright,      wxT("Copyright")        },
   {  kQTMetaDataCommonKeyDirector,       wxT("Director")         },
   {  kQTMetaDataCommonKeyDisplayName,    wxT("Full Name")        },
   {  kQTMetaDataCommonKeyInformation,    wxT("Information")      },
   {  kQTMetaDataCommonKeyKeywords,       wxT("Keywords")         },
   {  kQTMetaDataCommonKeyProducer,       wxT("Producer")         },
   {  kQTMetaDataCommonKeyAlbum,          TAG_ALBUM               },
   {  kQTMetaDataCommonKeyArtist,         TAG_ARTIST              },
   {  kQTMetaDataCommonKeyChapterName,    wxT("Chapter")          },
   {  kQTMetaDataCommonKeyComposer,       wxT("Composer")         },
   {  kQTMetaDataCommonKeyDescription,    wxT("Description")      },
   {  kQTMetaDataCommonKeyGenre,          TAG_GENRE               },
   {  kQTMetaDataCommonKeyOriginalFormat, wxT("Original Format")  },
   {  kQTMetaDataCommonKeyOriginalSource, wxT("Original Source")  },
   {  kQTMetaDataCommonKeyPerformers,     wxT("Performers")       },
   {  kQTMetaDataCommonKeySoftware,       wxT("Software")         },
   {  kQTMetaDataCommonKeyWriter,         wxT("Writer")           },
};

void QTImportFileHandle::AddMetadata(Tags *tags)
{
   QTMetaDataRef metaDataRef = NULL;
   OSErr err; 

   err = QTCopyMovieMetaData(mMovie, &metaDataRef);
   if (err != noErr) {
      return;
   }

   for (int i = 0; i < WXSIZEOF(names); i++) {
      QTMetaDataItem item = kQTMetaDataItemUninitialized;
      OSType key = names[i].key;

      err = QTMetaDataGetNextItem(metaDataRef,
                                  kQTMetaDataStorageFormatWildcard,
                                  kQTMetaDataItemUninitialized,
                                  kQTMetaDataKeyFormatCommon,
                                  (const UInt8 *) &names[i].key,
                                  sizeof(names[i].key),
                                  &item);
      if (err != noErr) {
         continue;
      }

      if (item == kQTMetaDataItemUninitialized) {
         continue;
      }

      QTPropertyValuePtr outValPtr = nil;
      QTPropertyValueType outPropType;
      ByteCount outPropValueSize;
      ByteCount outPropValueSizeUsed = 0;
      UInt32 outPropFlags;
      UInt32 dataType;
      
      // Get data type
      err =  QTMetaDataGetItemProperty(metaDataRef,
                                       item,
                                       kPropertyClass_MetaDataItem,
                                       kQTMetaDataItemPropertyID_DataType,
                                       sizeof(dataType),
                                       &dataType,
                                       &outPropValueSizeUsed);
      if (err != noErr) {
         continue;
      }

      // Get the data length
      err = QTMetaDataGetItemPropertyInfo(metaDataRef,
                                          item,
                                          kPropertyClass_MetaDataItem,
                                          kQTMetaDataItemPropertyID_Value,
                                          &outPropType,
                                          &outPropValueSize,
                                          &outPropFlags );
      if (err != noErr) {
         continue;
      }

      // Alloc memory for it
      outValPtr = malloc(outPropValueSize);
      
      // Retrieve the data
      err =  QTMetaDataGetItemProperty(metaDataRef,
                                       item,
                                       kPropertyClass_MetaDataItem,
                                       kQTMetaDataItemPropertyID_Value,
                                       outPropValueSize,
                                       outValPtr,
                                       &outPropValueSizeUsed);
      if (err != noErr) {
         free(outValPtr);
         continue;
      }

      wxString v = wxT("");

      switch (dataType)
      {
         case kQTMetaDataTypeUTF8:
            v = wxString((char *)outValPtr, wxConvUTF8);
         break;
         case kQTMetaDataTypeUTF16BE:
         {
            wxMBConvUTF16BE conv;
            v = wxString((char *)outValPtr, conv);
         }
         break;
      }

      if (!v.IsEmpty()) {
         tags->SetTag(names[i].name, v);
      }

      free(outValPtr);
   }      

   // we are done so release our metadata object
   QTMetaDataRelease(metaDataRef);

   return;
}

static pascal Boolean
SoundConverterFillBufferCallback(SoundComponentDataPtr *outData, void *userData)
{
   CallbackData *pFillData = (CallbackData*)userData;

   OSErr err;

   // if after getting the last chunk of data the total time is over the duration, we're done
   if (pFillData->getMediaAtThisTime >= pFillData->sourceDuration)
   {
      pFillData->isThereMoreSource = false;
      pFillData->compData.desc.buffer = NULL;
      pFillData->compData.desc.sampleCount = 0;
      pFillData->compData.bufferSize = 0;
      pFillData->compData.commonFrameSize = 0;
   }

   if (pFillData->isThereMoreSource) {

      long    sourceBytesReturned;
      long    numberOfSamples;
      TimeValue sourceReturnedTime, durationPerSample;

      // in calling GetMediaSample, we'll get a buffer that consists of equal
      // sized frames - the degenerate case is only 1 frame -- for
      // non-self-framed vbr formats (like AAC in QT 6.0) we need to provide
      // some more framing information - either the frameCount, frameSizeArray
      // pair or commonFrameSize field must be valid -- because we always get
      // equal sized frames, we use commonFrameSize and set the
      // kExtendedSoundCommonFrameSizeValid flag -- if there is only 1 frame
      // then (common frame size == media sample size), if there are multiple
      // frames, then (common frame size == media sample size / number of
      // frames).

      HUnlock(pFillData->hSource);

      err = GetMediaSample(
               pFillData->sourceMedia,   // media to retrieve data from
               pFillData->hSource,       // where to put the data
               pFillData->maxBufferSize, // maximum number of bytes to be returned
               &sourceBytesReturned,     // number of bytes actually returned
               pFillData->getMediaAtThisTime,  // starting time of the sample to be retrieved (must be in Media's TimeScale)
               &sourceReturnedTime,      // actual time of the returned sample data
               &durationPerSample,       // duration of each sample in the media
               NULL,                     // sample description of the returned data
               NULL,                     // index value to the sample description
               0,                        // maximum number of samples to be returned (0 to use a value that is appropriate for the media)
               &numberOfSamples,         // number of samples it actually returned
               NULL);                    // flags that describe the sample

      if ((noErr != err) || (sourceBytesReturned == 0)) {
         pFillData->isThereMoreSource = false;
         pFillData->compData.desc.buffer = NULL;
         pFillData->compData.desc.sampleCount = 0;
         pFillData->compData.bufferSize = 0;
         pFillData->compData.commonFrameSize = 0;

         if(sourceBytesReturned == 0)
            wxMessageBox(wxT("There were simply no bytes returned!"));
         if(err != noErr)
            wxMessageBox(wxString::Format(wxT("Error returned: %d"), err));
         if ((err != noErr) && (sourceBytesReturned > 0))
            wxMessageBox(wxT("GetMediaSample - Failed in FillBufferDataProc"));
      }

      pFillData->getMediaAtThisTime = sourceReturnedTime + (durationPerSample * numberOfSamples);

      // (the following comment is out of the example code.  I would never call
      // a sound conversion routine "studly."  -JH
      //
      // we've specified kExtendedSoundSampleCountNotValid and the 'studly'
      // Sound Converter will take care of sampleCount for us, so while this is
      // not required we fill out all the information we have to simply
      // demonstrate how this would be done sampleCount is the number of PCM
      // samples
      pFillData->compData.desc.sampleCount = numberOfSamples * durationPerSample;

      // kExtendedSoundBufferSizeValid was specified - make sure this field is
      // filled in correctly
      pFillData->compData.bufferSize = sourceBytesReturned;

      // for VBR audio we specified the kExtendedSoundCommonFrameSizeValid flag
      // - make sure this field is filled in correctly
      if (pFillData->isSourceVBR)
         pFillData->compData.commonFrameSize = sourceBytesReturned / numberOfSamples;
   }

   // set outData to a properly filled out ExtendedSoundComponentData struct
   *outData = (SoundComponentDataPtr)&pFillData->compData;

   return (pFillData->isThereMoreSource);
}

#endif

// Indentation settings for Vim and Emacs.
// Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
