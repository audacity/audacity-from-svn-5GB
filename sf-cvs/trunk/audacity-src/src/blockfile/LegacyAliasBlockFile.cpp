/**********************************************************************

  Audacity: A Digital Audio Editor

  LegacyAliasBlockFile.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/utils.h>
#include <wx/wxchar.h>

#include <sndfile.h>

#include "LegacyAliasBlockFile.h"
#include "LegacyBlockFile.h"
#include "../FileFormats.h"

LegacyAliasBlockFile::LegacyAliasBlockFile(wxFileName fileName,
                                           wxFileName aliasedFile,
                                           sampleCount aliasStart,
                                           sampleCount aliasLen,
                                           int aliasChannel,
                                           sampleCount summaryLen,
                                           bool noRMS):
   PCMAliasBlockFile(fileName, aliasedFile, aliasStart, aliasLen,
                     aliasChannel, 0.0, 0.0, 0.0)
{
   sampleFormat format;

   if (noRMS)
      format = int16Sample;
   else
      format = floatSample;

   ComputeLegacySummaryInfo(fileName,
                            summaryLen, format,
                            &mSummaryInfo, noRMS, FALSE,
                            &mMin, &mMax, &mRMS);
}

LegacyAliasBlockFile::~LegacyAliasBlockFile()
{
}

/// Construct a new LegacyAliasBlockFile based on this one, but writing
/// the summary data to a new file.
///
/// @param newFileName The filename to copy the summary data to.
BlockFile *LegacyAliasBlockFile::Copy(wxFileName newFileName)
{
   BlockFile *newBlockFile =
      new LegacyAliasBlockFile(newFileName,
                               mAliasedFileName, mAliasStart,
                               mLen, mAliasChannel,
                               mSummaryInfo.totalSummaryBytes,
                               mSummaryInfo.fields < 3);

   return newBlockFile;
}

void LegacyAliasBlockFile::SaveXML(int depth, wxFFile &xmlFile)
{
   for(int i = 0; i < depth; i++)
      xmlFile.Write("\t");
   xmlFile.Write("<legacyblockfile ");
   xmlFile.Write("alias='1' ");
   xmlFile.Write(wxString::Format("name='%s' ",
                                  XMLTagHandler::XMLEsc(mFileName.GetFullName()).c_str()));
   xmlFile.Write(wxString::Format("aliaspath='%s' ",
                                  XMLTagHandler::XMLEsc(mAliasedFileName.GetFullPath()).c_str()));
   xmlFile.Write(wxString::Format("aliasstart='%d' ", mAliasStart));
   xmlFile.Write(wxString::Format("aliaslen='%d' ", mLen));
   xmlFile.Write(wxString::Format("aliaschannel='%d' ", mAliasChannel));
   xmlFile.Write(wxString::Format("summarylen='%d' ", mSummaryInfo.totalSummaryBytes));
   if (mSummaryInfo.fields < 3)
      xmlFile.Write(wxString::Format("norms='1' "));
   xmlFile.Write("/>\n");
}

BlockFile *LegacyAliasBlockFile::BuildFromXML(wxString projDir, const char **attrs)
{
   wxFileName summaryFileName;
   wxFileName aliasFileName;
   
   int aliasStart=0, aliasLen=0, aliasChannel=0;
   int summaryLen=0;
   bool noRMS = false;

   while(*attrs)
   {
       const char *attr =  *attrs++;
       const char *value = *attrs++;

       if( !wxStricmp(attr, "name") )
          summaryFileName.Assign(projDir, value, "");
       if( !wxStricmp(attr, "aliaspath") )
          aliasFileName.Assign(value);
       if( !wxStricmp(attr, "aliasstart") )
          aliasStart = atoi(value);
       if( !wxStricmp(attr, "aliaslen") )
          aliasLen = atoi(value);
       if( !wxStricmp(attr, "aliaschannel") )
          aliasChannel = atoi(value);
       if( !wxStricmp(attr, "summarylen") )
          summaryLen = atoi(value);
       if( !wxStricmp(attr, "norms") )
          noRMS = atoi(value)?true:false;
   }

   return new LegacyAliasBlockFile(summaryFileName, aliasFileName,
                                   aliasStart, aliasLen, aliasChannel,
                                   summaryLen, noRMS);
}

// regenerates the summary info, doesn't deal with missing alias files
void LegacyAliasBlockFile::Recover(){
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
// arch-tag: b5ab502b-a641-4013-b4fd-1dea05d9b3fd




