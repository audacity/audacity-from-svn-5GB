/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLTagHandler.cpp

  Dominic Mazzoni
  Vaughan Johnson (IsGood*FromXML)

  This class is an interface which should be implemented by
  classes which wish to be able to load and save themselves
  using XML files.

**********************************************************************/

#include "XMLTagHandler.h"

#include "../Audacity.h"
#include "../Internat.h"

#include <wx/defs.h>
#include <wx/filename.h>

// "Good" means the name is well-formed and names an existing file.
// These are functions rather than methods because some non-descendants of XMLTagHandler need it. //vvvvv necessarily?
bool IsGoodFileString(wxString str)
{
   // Test against corrupt filenames per security vulnerability report by NGS for UmixIt.
   wxString intlStrFileName = FILENAME(str);
   size_t len = intlStrFileName.Length();
   return ((len <= 128) && // FILENAME_MAX is 260 in MSVC, but inconsistent across platforms, sometimes huge.
            (intlStrFileName.Find('\0') == len) && // No null characters except terminator.
            (intlStrFileName.Find(wxFileName::GetPathSeparator()) == -1)); // No path separator characters. //vvv (this won't work on CVS HEAD)
}

bool IsGoodSubdirNameFromXML(const wxString strSubdirName, const wxString strDirName /* = "" */)
{
   // Test strSubdirName.
   if (!IsGoodFileString(strSubdirName)) return false;

   // Test the corresponding wxFileName.
   wxFileName fileName(FILENAME(strDirName), FILENAME(strSubdirName));
   return (fileName.IsOk() && fileName.DirExists());
}

bool IsGoodFileNameFromXML(const wxString strFileName, const wxString strDirName /* = "" */)
{
   // Test strFileName.
   if (!IsGoodFileString(strFileName)) return false;

   // Test the corresponding wxFileName.
   wxFileName fileName(FILENAME(strDirName), FILENAME(strFileName));
   return (fileName.IsOk() && fileName.FileExists());
}

bool IsGoodPathNameFromXML(const wxString strPathName)
{
   // Test strPathName.
   wxString intlStrPathName = FILENAME(strPathName);
   if ((intlStrPathName.Find('\0') < intlStrPathName.Length())) // No null characters.
      return false;

   // Test the corresponding wxFileName.
   wxFileName fileName(intlStrPathName);
   return IsGoodFileNameFromXML(fileName.GetFullName(), fileName.GetPath(wxPATH_GET_VOLUME));
}


// See http://www.w3.org/TR/REC-xml for reference
wxString XMLTagHandler::XMLEsc(wxString s)
{
   wxString result;
   int len = s.Length();

   for(int i=0; i<len; i++) {
      wxChar c = s.GetChar(i);

      switch (c) {
         case '\'':
            result += "&apos;";
            break;
         case '"':
            result += "&quot;";
            break;
         case '&':
            result += "&amp;";
            break;
         case '<':
            result += "&lt;";
            break;
         case '>':
            result += "&gt;";
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
      tmp_attrs.Add(Internat::UTF8ToLocal(s));
   }

// JKC: Previously the next line was:
// const char **out_attrs = new char (const char *)[tmp_attrs.GetCount()+1];
// however MSVC doesn't like the constness in this position, so this is now 
// added by a cast after creating the array of pointers-to-non-const chars.
   const char **out_attrs = (const char**)new char *[tmp_attrs.GetCount()+1];
   for (size_t i=0; i<tmp_attrs.GetCount(); i++) {
      out_attrs[i] = tmp_attrs[i].c_str();
   }
   out_attrs[tmp_attrs.GetCount()] = 0;

   bool result = HandleXMLTag(Internat::UTF8ToLocal(tag), out_attrs);

   delete[] out_attrs;
   return result;
}

void XMLTagHandler::ReadXMLEndTag(const char *tag)
{
   HandleXMLEndTag(Internat::UTF8ToLocal(tag));
}

XMLTagHandler *XMLTagHandler::ReadXMLChild(const char *tag)
{
   return HandleXMLChild(Internat::UTF8ToLocal(tag));
}
