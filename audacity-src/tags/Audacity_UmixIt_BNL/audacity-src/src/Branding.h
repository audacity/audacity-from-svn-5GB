/**********************************************************************

   Audacity: A Digital Audio Editor

   Branding.h

   Vaughan Johnson, November 2006

   Project-specific branding (brand name, website URL, logo, color scheme)

  
**********************************************************************/

#pragma once

#include "Audacity.h"
#include "xml/xmltaghandler.h"

class Branding : public XMLTagHandler
{
public:
   Branding();

   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual XMLTagHandler *HandleXMLChild(const char *tag) { return NULL; }; //v
   virtual void WriteXML(int depth, FILE *fp);

   wxString GetBrandName() { return m_strBrandName; }
   wxString GetBrandURL() { return m_strBrandURL; }
   wxString GetBrandLogoFilename() { return m_strBrandLogoFilename; }
   wxString GetBrandColorScheme() { return m_strBrandColorScheme; }

public:
   wxString m_strBrandName;
   wxString m_strBrandURL;
   wxString m_strBrandLogoFilename;
   wxString m_strBrandColorScheme;
};
