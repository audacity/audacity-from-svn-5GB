/**********************************************************************

   Audacity: A Digital Audio Editor

   Branding.h

   Vaughan Johnson, November 2006

   Project-specific branding (brand name, website URL, logo, color scheme)

  
**********************************************************************/

#pragma once

#include "Audacity.h"
#include "xml/XMLTagHandler.h"

#include <wx/filename.h>

class Branding : public XMLTagHandler
{
public:
   Branding();

   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual XMLTagHandler *HandleXMLChild(const char *tag) { return NULL; }; //v
   virtual void WriteXML(int depth, FILE *fp);

   wxString GetBrandName() { return m_strBrandName; }
   wxString GetBrandURL() { return m_strBrandURL; }
   wxFileName GetBrandLogoFileName() { return m_BrandLogoFileName; }
   wxString GetBrandColorScheme() { return m_strBrandColorScheme; }

private:
   wxString m_strBrandName;
   wxString m_strBrandURL;
   wxFileName m_BrandLogoFileName; // Store full thing, not just file name, so don't need to add path again.
   wxString m_strBrandColorScheme;
};
