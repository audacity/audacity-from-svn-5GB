/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLTagHandler.h

  Dominic Mazzoni

  This class is an interface which should be implemented by
  classes which wish to be able to load and save themselves
  using XML files.

**********************************************************************/

#include "XMLTagHandler.h"

#include "../Audacity.h"
#include "../Internat.h"

#include <wx/defs.h>

// See http://www.w3.org/TR/REC-xml for reference
wxString XMLTagHandler::XMLEsc(wxString s)
{
   wxString result;
   int len = s.Length();

   for(int i=0; i<len; i++) {
      wxChar c = s.GetChar(i);

      switch (c) {
         case wxT('\''):
            result += wxT("&apos;");
            break;
         case wxT('"'):
            result += wxT("&quot;");
            break;
         case wxT('&'):
            result += wxT("&amp;");
            break;
         case wxT('<'):
            result += wxT("&lt;");
            break;
         case wxT('>'):
            result += wxT("&gt;");
            break;
         default:
            result += c;
      }
   }

   return Internat::LocalToUTF8(result);
}

bool XMLTagHandler::ReadXMLTag(const char *tag, const char **attrs)
{
   wxArrayString tmp_attrs;

   while (*attrs) {
      const char *s = *attrs++;
      tmp_attrs.Add(UTF8CTOWX(s));
   }

// JKC: Previously the next line was:
// const char **out_attrs = new char (const char *)[tmp_attrs.GetCount()+1];
// however MSVC doesn't like the constness in this position, so this is now 
// added by a cast after creating the array of pointers-to-non-const chars.
   const wxChar **out_attrs = (const wxChar**)new wxChar *[tmp_attrs.GetCount()+1];
   for (size_t i=0; i<tmp_attrs.GetCount(); i++) {
      out_attrs[i] = tmp_attrs[i].c_str();
   }
   out_attrs[tmp_attrs.GetCount()] = 0;

   bool result = HandleXMLTag(UTF8CTOWX(tag).c_str(), out_attrs);

   delete[] out_attrs;
   return result;
}

void XMLTagHandler::ReadXMLEndTag(const char *tag)
{
   HandleXMLEndTag(UTF8CTOWX(tag).c_str());
}

XMLTagHandler *XMLTagHandler::ReadXMLChild(const char *tag)
{
   return HandleXMLChild(UTF8CTOWX(tag).c_str());
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
// arch-tag: 6aabae58-19bd-4b3a-aa6c-08432a7e106e

