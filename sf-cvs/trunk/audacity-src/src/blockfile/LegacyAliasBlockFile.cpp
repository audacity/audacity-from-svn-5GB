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
                                           sampleCount summaryLen):
   PCMAliasBlockFile(fileName, aliasedFile, aliasStart, aliasLen,
                     aliasChannel, 0.0, 0.0, 0.0)
{
   ComputeLegacySummaryInfo(fileName,
                            summaryLen, floatSample,
                            &mSummaryInfo,
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
                               mSummaryInfo.totalSummaryBytes);

   return newBlockFile;
}

void LegacyAliasBlockFile::SaveXML(int depth, wxFFile &xmlFile)
{
   for(int i = 0; i < depth; i++)
      xmlFile.Write("\t");
   xmlFile.Write("<legacyblockfile ");
   xmlFile.Write("alias='1' ");
   xmlFile.Write(wxString::Format("name='%s' ", mFileName.GetFullName().c_str()));
   xmlFile.Write(wxString::Format("aliaspath='%s' ", mAliasedFileName.GetFullPath().c_str()));
   xmlFile.Write(wxString::Format("aliasstart='%d' ", mAliasStart));
   xmlFile.Write(wxString::Format("aliaslen='%d' ", mLen));
   xmlFile.Write(wxString::Format("aliaschannel='%d' ", mAliasChannel));
   xmlFile.Write(wxString::Format("summarylen='%d' ", mSummaryInfo.totalSummaryBytes));
   xmlFile.Write("/>\n");
}

BlockFile *LegacyAliasBlockFile::BuildFromXML(wxString projDir, const char **attrs)
{
   wxFileName summaryFileName;
   wxFileName aliasFileName;
   int aliasStart=0, aliasLen=0, aliasChannel=0;
   int summaryLen=0;

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
   }

   return new LegacyAliasBlockFile(summaryFileName, aliasFileName,
                                   aliasStart, aliasLen, aliasChannel,
                                   summaryLen);
}

