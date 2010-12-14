/**********************************************************************

  Audacity: A Digital Audio Editor

  PCMAliasBlockFile.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/utils.h>
#include <wx/wxchar.h>

#include <sndfile.h>

#include "PCMAliasBlockFile.h"
#include "../FileFormats.h"
#include "../Internat.h"

PCMAliasBlockFile::PCMAliasBlockFile(wxFileName fileName,
                     wxFileName aliasedFile, sampleCount aliasStart,
                     sampleCount aliasLen, int aliasChannel):
   AliasBlockFile(fileName, aliasedFile, aliasStart, aliasLen, aliasChannel)
{
   AliasBlockFile::WriteSummary();
}

PCMAliasBlockFile::PCMAliasBlockFile(wxFileName existingFileName,
                     wxFileName aliasedFile, sampleCount aliasStart,
                     sampleCount aliasLen, int aliasChannel,
                     float min, float max, float rms):
   AliasBlockFile(existingFileName, aliasedFile, aliasStart, aliasLen,
                  aliasChannel, min, max, rms)
{
}

PCMAliasBlockFile::~PCMAliasBlockFile()
{
}

/// Reads the specified data from the aliased file, using libsndfile,
/// and converts it to the given sample format.
///
/// @param data   The buffer to read the sample data into.
/// @param format The format to convert the data into
/// @param start  The offset within the block to begin reading
/// @param len    The number of samples to read
int PCMAliasBlockFile::ReadData(samplePtr data, sampleFormat format,
                                sampleCount start, sampleCount len)
{
   SF_INFO info;

   memset(&info, 0, sizeof(info));

   SNDFILE *sf = sf_open(mAliasedFullPath, SFM_READ, &info);
   if (!sf)
      return 0;

   sf_seek(sf, mAliasStart + start, SEEK_SET);
   samplePtr buffer = NewSamples(len * info.channels, floatSample);

   int framesRead = 0;

   if (format == int16Sample &&
       !sf_subtype_more_than_16_bits(info.format)) {
      // Special case: if the file is in 16-bit (or less) format,
      // and the calling method wants 16-bit data, go ahead and
      // read 16-bit data directly.  This is a pretty common
      // case, as most audio files are 16-bit.
      framesRead = sf_readf_short(sf, (short *)buffer, len);
      for (int i = 0; i < framesRead; i++)
         ((short *)data)[i] =
            ((short *)buffer)[(info.channels * i) + mAliasChannel];
   }
   else {
      // Otherwise, let libsndfile handle the conversion and
      // scaling, and pass us normalized data as floats.  We can
      // then convert to whatever format we want.
      framesRead = sf_readf_float(sf, (float *)buffer, len);
      float *bufferPtr = &((float *)buffer)[mAliasChannel];
      CopySamples((samplePtr)bufferPtr, floatSample,
                  (samplePtr)data, format,
                  framesRead, true, info.channels);
   }

   DeleteSamples(buffer);

   sf_close(sf);

   return framesRead;
}

/// Construct a new PCMAliasBlockFile based on this one, but writing
/// the summary data to a new file.
///
/// @param newFileName The filename to copy the summary data to.
BlockFile *PCMAliasBlockFile::Copy(wxFileName newFileName)
{
   BlockFile *newBlockFile = new PCMAliasBlockFile(newFileName,
                                                   mAliasedFileName, mAliasStart,
                                                   mLen, mAliasChannel,
                                                   mMin, mMax, mRMS);

   return newBlockFile;
}

void PCMAliasBlockFile::SaveXML(int depth, wxFFile &xmlFile)
{
   for(int i = 0; i < depth; i++)
      xmlFile.Write("\t");
   xmlFile.Write("<pcmaliasblockfile ");
   xmlFile.Write(wxString::Format("summaryfile='%s' ",
                                  XMLTagHandler::XMLEsc(mFileName.GetFullName()).c_str()));
   xmlFile.Write(wxString::Format("aliasfile='%s' ",
                                  XMLTagHandler::XMLEsc(mAliasedFileName.GetFullPath()).c_str()));
   xmlFile.Write(wxString::Format("aliasstart='%d' ", mAliasStart));
   xmlFile.Write(wxString::Format("aliaslen='%d' ", mLen));
   xmlFile.Write(wxString::Format("aliaschannel='%d' ", mAliasChannel));
   xmlFile.Write(wxString::Format("min='%s' ",
            Internat::ToString(mMin).c_str()));
   xmlFile.Write(wxString::Format("max='%s' ",
            Internat::ToString(mMax).c_str()));
   xmlFile.Write(wxString::Format("rms='%s'",
            Internat::ToString(mRMS).c_str()));
   xmlFile.Write("/>\n");
}

BlockFile *PCMAliasBlockFile::BuildFromXML(wxString projDir, const char **attrs)
{
   wxFileName summaryFileName;
   wxFileName aliasFileName;
   int aliasStart=0, aliasLen=0, aliasChannel=0;
   float min=0, max=0, rms=0;
   long nValue;

   while(*attrs)
   {
       const char *attr =  *attrs++;
       const char *value = *attrs++;
       if (!value) 
          break;

       const wxString strValue = value;
       if( !wxStricmp(attr, "summaryfile") )
       {
         if (!XMLValueChecker::IsGoodFileName(strValue, projDir))
            return NULL;
         summaryFileName.Assign(projDir, strValue);
       }
       else if( !wxStricmp(attr, "aliasfile") )
       {
         if (XMLValueChecker::IsGoodPathName(strValue))
            aliasFileName.Assign(strValue);
         else if (XMLValueChecker::IsGoodFileName(strValue, projDir))
            // Allow fallback of looking for the file name, located in the data directory.
            aliasFileName.Assign(projDir, strValue);
         else 
            return NULL;
       }
       else if (XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue)) 
       { // integer parameters
         if( !wxStricmp(attr, "aliasstart") )
            aliasStart = nValue;
         else if( !wxStricmp(attr, "aliaslen") )
            aliasLen = nValue;
         else if( !wxStricmp(attr, "aliaschannel") )
            aliasChannel = nValue;
         else if( !wxStricmp(attr, "min") )
            min = nValue;
         else if( !wxStricmp(attr, "max") )
            max = nValue;
         else if( !wxStricmp(attr, "rms") )
            rms = nValue;
       }
   }

   if (!XMLValueChecker::IsGoodFileName(summaryFileName.GetFullName(), 
                                         summaryFileName.GetPath(wxPATH_GET_VOLUME)) || 
         !XMLValueChecker::IsGoodFileName(aliasFileName.GetFullName(), 
                                          aliasFileName.GetPath(wxPATH_GET_VOLUME)) ||
         (aliasStart < 0) || (aliasLen <= 0) || 
         !XMLValueChecker::IsValidChannel(aliasChannel) || (rms < 0.0))
      return NULL;

   return new PCMAliasBlockFile(summaryFileName, aliasFileName,
                                aliasStart, aliasLen, aliasChannel,
                                min, max, rms);
}

