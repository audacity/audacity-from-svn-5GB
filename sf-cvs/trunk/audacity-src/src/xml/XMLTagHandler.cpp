/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLTagHandler.h

  Dominic Mazzoni

  This class is an interface which should be implemented by
  classes which wish to be able to load and save themselves
  using XML files.

**********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/string.h>

#include "XMLTagHandler.h"

// See http://www.w3.org/TR/REC-xml for reference
wxString XMLTagHandler::XMLEsc(wxString s)
{
   int len = s.Length();
   int i;
   char c;
   wxString result;

   for(i=0; i<len; i++) {
      c = s[i];

      if (c < 32 || c>127 ||
          c=='\'' || c=='"' ||
          c=='&' || c=='<' || c=='>') {
         result += wxString::Format("&#x%02X;", (int)c);
      }
      else
         result += c;
   }

   return result;
}
