/**********************************************************************

  Audacity: A Digital Audio Editor

  SimpleBlockFile.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/ffile.h>
#include <wx/utils.h>

#include "SimpleBlockFile.h"
#include "../FileFormats.h"

#include "sndfile.h"

// The AU formats we care about
enum {
   AU_SAMPLE_FORMAT_16 = 3,
   AU_SAMPLE_FORMAT_24 = 4,
   AU_SAMPLE_FORMAT_FLOAT = 6,
};

typedef struct {
   wxUint32 magic;      // magic number
   wxUint32 dataOffset; // byte offset to start of audio data
   wxUint32 dataSize;   // data length, in bytes (optional)
   wxUint32 encoding;   // data encoding enumeration
   wxUint32 sampleRate; // samples per second
   wxUint32 channels;   // number of interleaved channels
} auHeader;

/// Constructs a SimpleBlockFile based on sample data and writes
/// it to disk.
///
/// @param baseFileName The filename to use, but without an extension.
///                     This constructor will add the appropriate
///                     extension (.au in this case).
/// @param sampleData   The sample data to be written to this block.
/// @param sampleLen    The number of samples to be written to this block.
/// @param format       The format of the given samples.
SimpleBlockFile::SimpleBlockFile(wxFileName baseFileName,
                                 samplePtr sampleData, sampleCount sampleLen,
                                 sampleFormat format):
   BlockFile(wxFileName(baseFileName.GetFullPath() + ".au"), sampleLen)
{
   // Open and write the file
   wxFFile file;

   if( !file.Open((const wxChar *) mFileName.GetFullPath(), "wb") )
       // Throw an exception?
       return;

   auHeader header;

   // AU files can be either big or little endian.  Which it is is
   // determined implicitly by the byte-order of the magic 0x2e736e64
   // (.snd).  We want it to be native-endian, so we write the magic
   // to memory and then let it write that to a file in native
   // endianness
   header.magic = 0x2e736e64;

   // We store the summary data at the end of the header, so the data
   // offset is the length of the summary data plus the length of the header
   header.dataOffset = sizeof(auHeader) + mSummaryInfo.totalSummaryBytes;

   // dataSize is optional, and we opt out
   header.dataSize = 0xffffffff;

   switch(format) {
      case int16Sample:
         header.encoding = AU_SAMPLE_FORMAT_16;
         break;

      case int24Sample:
         header.encoding = AU_SAMPLE_FORMAT_24;
         break;

      case floatSample:
         header.encoding = AU_SAMPLE_FORMAT_FLOAT;
         break;
   }

   // TODO: don't fabricate
   header.sampleRate = 44100;

   // BlockFiles are always mono
   header.channels = 1;

   // Write the file
   void *summaryData = BlockFile::CalcSummary(sampleData, sampleLen, format);

   file.Write(&header, sizeof(header));
   file.Write(summaryData, mSummaryInfo.totalSummaryBytes);

   if( format == int24Sample )
   {
      // we can't write the buffer directly to disk, because 24-bit samples
      // on disk need to be packed, not padded to 32 bits like they are in
      // memory
      for( int i = 0; i < sampleLen; i++ )
         file.Write(&sampleData[i], 3);
   }
   else
   {
      // for all other sample formats we can write straight from the buffer
      // to disk
      file.Write(sampleData, sampleLen * SAMPLE_SIZE(format));
    }
}

/// Construct a SimpleBlockFile memory structure that will point to an
/// existing block file.  This file must exist and be a valid block file.
///
/// @param existingFile The disk file this SimpleBlockFile should use.
SimpleBlockFile::SimpleBlockFile(wxFileName existingFile,
                                 float min, float max, float rms):
   BlockFile(existingFile, 0)
{
   // Since we gave BlockFile a bogus parameter for sampleLen, we need
   // to update mSummaryInfo with real values.  We do this by opening
   // the file with libsndfile and retrieving its length.

   if( !existingFile.FileExists() )
      // throw an exception?
      ;

   SF_INFO info;
   SNDFILE *sf = sf_open(existingFile.GetFullPath(), SFM_READ, &info);

   SummaryInfo realInfo(info.frames);
   mSummaryInfo = realInfo;

   sf_close(sf);

   mMin = min;
   mMax = max;
   mRMS = rms;
}

SimpleBlockFile::~SimpleBlockFile()
{
}

/// Read the summary section of the disk file.
///
/// @param *data The buffer to write the data to.  It must be at least
/// mSummaryinfo.totalSummaryBytes long.
bool SimpleBlockFile::ReadSummary(void *data)
{
   wxFFile file;

   if( !file.Open((const wxChar *) mFileName.GetFullPath(), "rb") )
      return false;

   // The offset is just past the au header
   if( !file.Seek(sizeof(auHeader)) )
      return false;

   int read = (int)file.Read(data, (size_t)mSummaryInfo.totalSummaryBytes);

   return (read == mSummaryInfo.totalSummaryBytes);
}

/// Read the data portion of the block file using libsndfile.  Convert it
/// to the given format if it is not already.
///
/// @param data   The buffer where the data will be stored
/// @param format The format the data will be stored in
/// @param start  The offset in this block file
/// @param len    The number of samples to read
int SimpleBlockFile::ReadData(samplePtr data, sampleFormat format,
                        sampleCount start, sampleCount len)
{
   SF_INFO info;

   memset(&info, 0, sizeof(info));
   SNDFILE *sf = sf_open(mFileName.GetFullPath(), SFM_READ, &info);

   if (!sf)
      return 0;

   sf_seek(sf, start, SEEK_SET);
   samplePtr buffer = NewSamples(len, floatSample);

   int framesRead = 0;

   if (format == int16Sample &&
       !sf_subtype_more_than_16_bits(info.format)) {
      // Special case: if the file is in 16-bit (or less) format,
      // and the calling method wants 16-bit data, go ahead and
      // read 16-bit data directly.  This is a pretty common
      // case, as most audio files are 16-bit.
      framesRead = sf_readf_short(sf, (short *)data, len);
   }
   else {
      // Otherwise, let libsndfile handle the conversion and
      // scaling, and pass us normalized data as floats.  We can
      // then convert to whatever format we want.
      framesRead = sf_readf_float(sf, (float *)buffer, len);
      CopySamples(buffer, floatSample,
                  (samplePtr)data, format, framesRead);
   }

   DeleteSamples(buffer);

   sf_close(sf);

   return framesRead;
}

void SimpleBlockFile::SaveXML(int depth, wxFFile &xmlFile)
{
   for(int i = 0; i < depth; i++)
      xmlFile.Write("\t");
   xmlFile.Write("<simpleblockfile ");
   xmlFile.Write(wxString::Format("filename='%s' ", mFileName.GetFullName().c_str()));
   xmlFile.Write(wxString::Format("min='%f' ", mMin));
   xmlFile.Write(wxString::Format("max='%f' ", mMax));
   xmlFile.Write(wxString::Format("rms='%f'", mRMS));
   xmlFile.Write("/>\n");
}

/// static
BlockFile *SimpleBlockFile::BuildFromXML(wxString projDir, const char **attrs)
{
   wxFileName fileName;
   float min=0, max=0, rms=0;

   while(*attrs)
   {
       const char *attr =  *attrs++;
       const char *value = *attrs++;

       if( !strcmp(attr, "filename") )
          fileName.Assign(projDir, value);
       if( !strcmp(attr, "min") )
          min = atoi(value);
       if( !strcmp(attr, "max") )
          max = atoi(value);
       if( !strcmp(attr, "rms") )
          rms = atoi(value);
   }

   return new SimpleBlockFile(fileName, min, max, rms);
}

/// Create a copy of this BlockFile, but using a different disk file.
///
/// @param newFileName The name of the new file to use.
BlockFile *SimpleBlockFile::Copy(wxFileName newFileName)
{
   BlockFile *newBlockFile = new SimpleBlockFile(newFileName,
                                                 mMin, mMax, mRMS);

   return newBlockFile;
}

int SimpleBlockFile::GetSpaceUsage()
{
   wxFFile dataFile(mFileName.GetFullPath());
   return dataFile.Length();
}

