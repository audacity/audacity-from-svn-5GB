/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLTagHandler.cpp

  Dominic Mazzoni
  Vaughan Johnson

  The XMLTagHandler class is an interface which should be implemented by
  classes which wish to be able to load and save themselves
  using XML files.

  The XMLValueChecker class implements static bool methods for checking 
  input values from XML files.

**********************************************************************/

#include "XMLTagHandler.h"

#include "../Audacity.h"
#include "../Internat.h"

#ifdef _WIN32
   #include <windows.h>
#endif

#include <wx/defs.h>
#include <wx/filename.h>

#include "../SampleFormat.h"
#include "../Track.h"

bool XMLValueChecker::IsGoodString(const wxString str)
{
   size_t len = str.Length();
   int nullIndex = str.Find('\0');
   if ((len < 2048) && // Shouldn't be any reason for longer strings, except intentional file corruption.
         (nullIndex > -1) && // _Should_ always find it, at string terminator.
         (nullIndex == len)) // No null characters except terminator.
      return true;
   else
      return false; // good place for a breakpoint
}

// "Good" means the name is well-formed and names an existing file or folder.
bool XMLValueChecker::IsGoodFileName(const wxString strFileName, const wxString strDirName /* = "" */)
{
   // Test strFileName.
   if (!IsGoodFileString(strFileName)) 
      return false;

   #ifdef _WIN32
      if (strFileName.Length() + 1 + strDirName.Length() > MAX_PATH)
         return false;
   #endif

   // Test the corresponding wxFileName.
   wxFileName fileName(FILENAME(strDirName), FILENAME(strFileName));
   return (fileName.IsOk() && fileName.FileExists());
}

bool XMLValueChecker::IsGoodSubdirName(const wxString strSubdirName, const wxString strDirName /* = "" */)
{
   // Test strSubdirName. 
   // Note this prevents path separators, and relative path to parents (strDirName), 
   // so fixes vulnerability #3 in the NGS report for UmixIt, 
   // where an attacker could craft an AUP file with relative pathnames to get to system files, for example.
   if (!IsGoodFileString(strSubdirName) || (strSubdirName == ".") || (strSubdirName == ".."))
      return false;

   #ifdef _WIN32
      if (strSubdirName.Length() + 1 + strDirName.Length() > MAX_PATH)
         return false;
   #endif

   // Test the corresponding wxFileName.
   wxFileName fileName(FILENAME(strDirName), FILENAME(strSubdirName));
   return (fileName.IsOk() && fileName.DirExists());
}

bool XMLValueChecker::IsGoodPathName(const wxString strPathName)
{
   // Test the corresponding wxFileName.
   wxFileName fileName(FILENAME(strPathName));
   return XMLValueChecker::IsGoodFileName(fileName.GetFullName(), fileName.GetPath(wxPATH_GET_VOLUME));
}

bool XMLValueChecker::IsGoodFileString(wxString str)
{
   return (IsGoodString(str) && 
            !str.IsEmpty() && 
            (str.Length() <= 260) && // FILENAME_MAX is 260 in MSVC, but inconsistent across platforms, sometimes huge.
            (str.Find(wxFileName::GetPathSeparator()) == -1)); // No path separator characters.
}

bool XMLValueChecker::IsGoodInt(const wxString strInt)
{
   if (!IsGoodString(strInt))
      return false;

   // Check that the value won't overflow.
   // Signed long: -2,147,483,648 to +2,147,483,647, i.e., -2^31 to 2^31-1
   // We're strict about disallowing spaces and commas, and requiring minus sign to be first char for negative.
   const size_t lenMAXABS = strlen("2147483647");
   const size_t lenStrInt = strInt.Length();

   unsigned long nTest;
   wxString strTest;

   if (lenStrInt > (lenMAXABS + 1))
      return false;
   else if ((lenStrInt == (lenMAXABS + 1)) && (strInt[0] == '-'))
   {
      const unsigned long digitsMAXABS[] = {2, 1, 4, 7, 4, 8, 3, 6, 4, 8};
      for (unsigned int i = 0; i < lenMAXABS; i++) {
         strTest = strInt[i+1];
         if (!strTest.ToULong(&nTest) || (nTest > digitsMAXABS[i]))
            return false;
      }
   }
   else if (lenStrInt == lenMAXABS)
   {
      const unsigned long digitsMAXABS[] = {2, 1, 4, 7, 4, 8, 3, 6, 4, 7};
      for (unsigned int i = 0; i < lenMAXABS; i++) {
         strTest = strInt[i];
         if (!strTest.ToULong(&nTest) || (nTest > digitsMAXABS[i]))
            return false;
      }
   }
   return true;
}

bool XMLValueChecker::IsValidChannel(const int nValue)
{
   return (nValue >= Track::LeftChannel) && (nValue <= Track::MonoChannel);
}

bool XMLValueChecker::IsValidSampleFormat(const int nValue)
{
   return (nValue == int16Sample) || (nValue == int24Sample) || (nValue == floatSample);
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
