/**********************************************************************

  Audacity: A Digital Audio Editor

  SilentBlockFile.cpp

  Joshua Haberman

**********************************************************************/

#include "SilentBlockFile.h"
#include "../FileFormats.h"

SilentBlockFile::SilentBlockFile(sampleCount sampleLen):
   BlockFile(wxFileName(), sampleLen)
{
}

SilentBlockFile::~SilentBlockFile()
{
}

bool SilentBlockFile::ReadSummary(void *data)
{
   memset(data, 0, (size_t)mSummaryInfo.totalSummaryBytes);
   return true;
}

int SilentBlockFile::ReadData(samplePtr data, sampleFormat format,
                              sampleCount start, sampleCount len)
{
   ClearSamples(data, format, 0, len);

   return len;
}

void SilentBlockFile::SaveXML(int depth, wxFFile &xmlFile)
{
   for(int i = 0; i < depth; i++)
      xmlFile.Write("\t");
   xmlFile.Write("<silentblockfile ");
   xmlFile.Write(wxString::Format("len='%d' ", mLen));
   xmlFile.Write("/>\n");
}

/// static
BlockFile *SilentBlockFile::BuildFromXML(wxString projDir, const char **attrs)
{
   long nValue;
   sampleCount len = 0;

   while(*attrs)
   {
       const char *attr =  *attrs++;
       const char *value = *attrs++;

       if (!value)
         break;

       const wxString strValue = value;
       if( !strcmp(attr, "len") && 
            XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue)) 
          len = nValue;
   }

   if (len <= 0)
      return NULL;

   return new SilentBlockFile(len);
}

/// Create a copy of this BlockFile
BlockFile *SilentBlockFile::Copy(wxFileName newFileName)
{
   BlockFile *newBlockFile = new SilentBlockFile(mLen);

   return newBlockFile;
}

int SilentBlockFile::GetSpaceUsage()
{
   return 0;
}

