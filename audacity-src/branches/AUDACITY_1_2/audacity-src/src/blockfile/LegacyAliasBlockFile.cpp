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
                            &mSummaryInfo, noRMS,
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
   long nValue;

   while(*attrs)
   {
       const char *attr =  *attrs++;
       const char *value = *attrs++;

      if (!value)
         break;

      const wxString strValue = value;
       if( !wxStricmp(attr, "name") )
       {
         if (!XMLValueChecker::IsGoodFileName(strValue, projDir))
            return NULL;
         summaryFileName.Assign(projDir, strValue, "");
       }
       else if ( !wxStricmp(attr, "aliaspath") )
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
       if( !wxStricmp(attr, "aliaslen") )
            aliasLen = nValue;
       if( !wxStricmp(attr, "aliaschannel") )
            aliasChannel = nValue;
       if( !wxStricmp(attr, "summarylen") )
            summaryLen = nValue;
       if( !wxStricmp(attr, "norms") )
            noRMS = (nValue != 0);
   }
   }

   if (!XMLValueChecker::IsGoodFileName(summaryFileName.GetFullName(), 
                                         summaryFileName.GetPath(wxPATH_GET_VOLUME)) || 
         !XMLValueChecker::IsGoodFileName(aliasFileName.GetFullName(), 
                                          aliasFileName.GetPath(wxPATH_GET_VOLUME)) ||
         (aliasStart < 0) || (aliasLen <= 0) || 
         !XMLValueChecker::IsValidChannel(aliasChannel) || (summaryLen <= 0))
      return NULL;

   return new LegacyAliasBlockFile(summaryFileName, aliasFileName,
                                   aliasStart, aliasLen, aliasChannel,
                                   summaryLen, noRMS);
}

