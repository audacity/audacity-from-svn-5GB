/**********************************************************************

   Audacity: A Digital Audio Editor

   Branding.h

   Vaughan Johnson, November 2006

   Project-specific branding (brand name, website URL, logo, color scheme)

  
**********************************************************************/

#include "Branding.h"
#include "Project.h"

#include <wx/msgdlg.h>

Branding::Branding()
{
   m_strBrandName = "";
   m_strBrandURL = "";
   m_strBrandColorScheme = "";
}

bool Branding::HandleXMLTag(const char *tag, const char **attrs) 
{
   if (strcmp(tag, "branding")) return false;

   // loop through attrs, null-terminated list of attribute-value pairs
   while (*attrs) {
      const char *attr = *attrs++;
      const char *value = *attrs++;

      if (!value) 
         break;

      if (!strcmp(attr, "brandname") && XMLValueChecker::IsGoodString(value)) 
         m_strBrandName = value;
      else if (!strcmp(attr, "url") && XMLValueChecker::IsGoodString(value)) 
         m_strBrandURL = value;
      else if (!strcmp(attr, "logofilename")) 
      {
         // Logo file is supposed to be stored in the project data directory.
         wxString strDirName = GetActiveProject()->GetDirManager()->GetProjectDataDir();
         if (XMLValueChecker::IsGoodFileName(value, strDirName)) 
         {
            // Store full thing, not just file name, so don't need to add path again.
            m_BrandLogoFileName.Assign(strDirName, value);
            m_BrandLogoFileName.Normalize(wxPATH_NORM_ABSOLUTE | wxPATH_NORM_LONG);
         } 
         else
         {
            wxLogWarning(wxT("Could not open branding logo file: %s"), value);
            return false;
         }
      }
      else if (!strcmp(attr, "colorscheme") && XMLValueChecker::IsGoodString(value)) 
         m_strBrandColorScheme = value;
   } // while

   return true; 
}

void Branding::WriteXML(int depth, FILE *fp)
{
   int i;

   for(i=0; i<depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "<branding ");
   fprintf(fp, "brandname=\"%s\" ", XMLEsc(m_strBrandName).c_str());
   fprintf(fp, "url=\"%s\" ", XMLEsc(m_strBrandURL).c_str());
   fprintf(fp, "logofilename=\"%s\" ", XMLEsc(m_BrandLogoFileName.GetFullName()).c_str());
   fprintf(fp, "colorscheme=\"%s\" ", XMLEsc(m_strBrandColorScheme).c_str());
   fprintf(fp, "/>\n"); // XML shorthand for childless tag
}

