/**********************************************************************

  Audacity: A Digital Audio Editor

  PCMAliasBlockFile.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/utils.h>
#include <wx/wxchar.h>
#include <wx/log.h>

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

   if(!mAliasedFileName.IsOk()){ // intentionally silenced 
      memset(data,0,SAMPLE_SIZE(format)*len);
      return len;
   }

   wxLogNull *silence=0;
   if(mSilentAliasLog)silence= new wxLogNull();

   memset(&info, 0, sizeof(info));

   #ifdef _UNICODE
      /* sf_open doesn't handle fn_Str() in Unicode build. May or may not actually work. */
      SNDFILE *sf=sf_open(FILENAME(mAliasedFileName.GetFullPath()).mb_str(), 
                        SFM_READ, &info);
   #else // ANSI
      SNDFILE *sf=sf_open(FILENAME(mAliasedFileName.GetFullPath()).fn_str(), 
                        SFM_READ, &info);
   #endif // Unicode/ANSI
   
   if (!sf){
      
      memset(data,0,SAMPLE_SIZE(format)*len);

      if(silence) delete silence;
      mSilentAliasLog=TRUE;

      return len;
   }

   if(silence) delete silence;
   mSilentAliasLog=FALSE;

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
      xmlFile.Write(wxT("\t"));
   xmlFile.Write(wxT("<pcmaliasblockfile "));
   xmlFile.Write(wxString::Format(wxT("summaryfile='%s' "),
                                  XMLTagHandler::XMLEsc(mFileName.GetFullName()).c_str()));
   xmlFile.Write(wxString::Format(wxT("aliasfile='%s' "),
                                  XMLTagHandler::XMLEsc(mAliasedFileName.GetFullPath()).c_str()));
   xmlFile.Write(wxString::Format(wxT("aliasstart='%d' "), mAliasStart));
   xmlFile.Write(wxString::Format(wxT("aliaslen='%d' "), mLen));
   xmlFile.Write(wxString::Format(wxT("aliaschannel='%d' "), mAliasChannel));
   xmlFile.Write(wxString::Format(wxT("min='%s' "),
            Internat::ToString(mMin).c_str()));
   xmlFile.Write(wxString::Format(wxT("max='%s' "),
            Internat::ToString(mMax).c_str()));
   xmlFile.Write(wxString::Format(wxT("rms='%s'"),
            Internat::ToString(mRMS).c_str()));
   xmlFile.Write(wxT("/>\n"));
}

BlockFile *PCMAliasBlockFile::BuildFromXML(DirManager &dm, const wxChar **attrs)
{
   wxFileName summaryFileName;
   wxFileName aliasFileName;
   int aliasStart=0, aliasLen=0, aliasChannel=0;
   float min=0, max=0, rms=0;

   while(*attrs)
   {
       const wxChar *attr =  *attrs++;
       const wxChar *value = *attrs++;

       if( !wxStricmp(attr, wxT("summaryfile")) )
	  dm.AssignFile(summaryFileName,value,FALSE);
       if( !wxStricmp(attr, wxT("aliasfile")) )
          aliasFileName.Assign(value);
       if( !wxStricmp(attr, wxT("aliasstart")) )
          aliasStart = wxAtoi(value);
       if( !wxStricmp(attr, wxT("aliaslen")) )
          aliasLen = wxAtoi(value);
       if( !wxStricmp(attr, wxT("aliaschannel")) )
          aliasChannel = wxAtoi(value);
       if( !wxStricmp(attr, wxT("min")) )
          min = wxAtoi(value);
       if( !wxStricmp(attr, wxT("max")) )
          max = wxAtoi(value);
       if( !wxStricmp(attr, wxT("rms")) )
          rms = wxAtoi(value);
   }

   return new PCMAliasBlockFile(summaryFileName, aliasFileName,
                                aliasStart, aliasLen, aliasChannel,
                                min, max, rms);
}

void PCMAliasBlockFile::Recover(void)
{
   WriteSummary();
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 7afeef28-696c-40c6-9558-c1134ac04a66


