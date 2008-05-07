/**********************************************************************

  Audacity: A Digital Audio Editor

  Internat.cpp

  Markus Meyer
  Dominic Mazzoni (Mac OS X code)

*******************************************************************//*!

\class Internat
\brief Internationalisation support.

This class is used to help internationalisation and in general
compatibility with different locales and character sets.
It deals mostly with converting numbers, but also has important
functions to convert to/from UTF-8, which is used in XML files
and on Mac OS X for the filesystem.

*//*******************************************************************/


#include "Internat.h"

#include <wx/log.h>
#include <wx/intl.h>
#include <wx/filename.h>

#ifdef __WXMAC__
#include <wx/mac/private.h>
#endif

#include <locale.h>
#include <math.h> // for pow()

// in order for the static member variables to exist, they must appear here
// (_outside_) the class definition, in order to be allocated some storage.
// Otherwise, you get link errors.

wxChar Internat::mDecimalSeparator = wxT('.'); // default
wxMBConv *Internat::mConvLocal = NULL;
wxString Internat::forbid;
wxArrayString Internat::exclude;


#ifdef __WXMAC__
void *Internat::mTECToUTF = NULL;
void *Internat::mTECFromUTF = NULL;
#endif

void Internat::Init()
{
   // Save decimal point character
   struct lconv * localeInfo = localeconv();
   if (localeInfo)
      mDecimalSeparator = wxString(localeInfo->decimal_point, wxConvLocal).GetChar(0);

//   wxLogDebug(wxT("Decimal separator set to '%c'"), mDecimalSeparator);

   #ifndef __WXMAC__
   // Set up character-set conversion for UTF-8 input and output.
   mConvLocal = new wxCSConv(wxLocale::GetSystemEncodingName());
   #else
   // Set up a special converter to/from the Mac-specific local
   // encoding (usually MacRoman)
   OSStatus status = noErr;
   TECObjectRef ec;

   TextEncoding MacEncoding = GetApplicationTextEncoding();
   TextEncoding UTF8 = CreateTextEncoding(kTextEncodingUnicodeDefault,
                                          kUnicodeNoSubset,
                                          kUnicodeUTF8Format);
   status = TECCreateConverter(&ec, MacEncoding, UTF8);
   if (status == noErr)
      mTECToUTF = (void *)ec;
   else
      mTECToUTF = NULL;
   
   status = TECCreateConverter(&ec, UTF8, MacEncoding);
   if (status == noErr)
      mTECFromUTF = (void *)ec;
   else
      mTECFromUTF = NULL;

   #endif

   // Setup list of characters that aren't allowed in file names
   wxFileName tmpFile;
   forbid = tmpFile.GetForbiddenChars();
   for(unsigned int i=0; i < forbid.Length(); i++)
      exclude.Add( forbid.Mid(i, 1) );

}

// JKC: Added to fix a memory leak.
void Internat::CleanUp()
{
#ifndef __WXMAC__
   if( mConvLocal != NULL )
   {
      delete mConvLocal;
   }
#endif
}

wxChar Internat::GetDecimalSeparator()
{
   return mDecimalSeparator;
}

bool Internat::CompatibleToDouble(const wxString& stringToConvert, double* result)
{
   // Regardless of the locale, always respect comma _and_ point
   wxString s = stringToConvert;
   s.Replace(wxT(","), wxString(GetDecimalSeparator()));
   s.Replace(wxT("."), wxString(GetDecimalSeparator()));
   return s.ToDouble(result);
}

double Internat::CompatibleToDouble(const wxString& stringToConvert)
{
   double result = 0;
   Internat::CompatibleToDouble(stringToConvert, &result);
   return result;
}

wxString Internat::ToString(double numberToConvert,
                     int digitsAfterDecimalPoint /* = -1 */)
{
   wxString result = ToDisplayString(
      numberToConvert, digitsAfterDecimalPoint);

   result.Replace(wxString(GetDecimalSeparator()), wxT("."));

   return result;
}

wxString Internat::ToDisplayString(double numberToConvert,
                     int digitsAfterDecimalPoint /* = -1 */)
{
   wxString decSep = wxString(GetDecimalSeparator());
   wxString result;

   if (digitsAfterDecimalPoint == -1)
   {
      result.Printf(wxT("%f"), numberToConvert);
      
      // Not all libcs respect the decimal separator, so always convert
      // any dots found to the decimal separator
      result.Replace(wxT("."), decSep);
      
      if (result.Find(decSep) != -1)
      {
         // Strip trailing zeros
         int pos = result.Length() - 1;
         while (pos > 0 && result.GetChar(pos) == wxT('0'))
            pos--;
         if (result.GetChar(pos) == decSep)
            pos--; // strip point before empty fractional part
         result = result.Left(pos+1);
      }
   } else
   {
      wxString format;
      format.Printf(wxT("%%.%if"), digitsAfterDecimalPoint);
      result.Printf(format, numberToConvert);
      
      // Not all libcs respect the decimal separator, so always convert
      // any dots found to the decimal separator
      result.Replace(wxT("."), decSep);
   }

   return result;
}

wxString Internat::FormatSize(wxLongLong size)
{
   /* wxLongLong contains no built-in conversion to double */
   double dSize = size.GetHi() * pow(2.0, 32);  // 2 ^ 32
   dSize += size.GetLo();

   return FormatSize(dSize);
}

wxString Internat::FormatSize(double size)
{
   wxString sizeStr;

   if (size == -1)
      sizeStr = _("Unable to determine");
   else {
      /* make it look nice, by formatting into k, MB, etc */
      if (size < 1024.0)
         sizeStr = ToDisplayString(size) + wxT(" ") + _("bytes");
      else if (size < 1024.0 * 1024.0) {
         sizeStr = ToDisplayString(size / 1024.0, 1) + wxT(" ") + _("KB");
      }
      else if (size < 1024.0 * 1024.0 * 1024.0) {
         sizeStr = ToDisplayString(size / (1024.0 * 1024.0), 1) + wxT(" ") + _("MB");
      }
      else {
         sizeStr = ToDisplayString(size / (1024.0 * 1024.0 * 1024.0), 1) + wxT(" ") + _("GB");
      }
   }

   return sizeStr;
}

#ifdef __WXMAC__IGNORE

// wxMac 2.4.x doesn't support converting to/from Mac encodings yet,
// so we use Mac OS X-specific code

wxString MacConvertString(TECObjectRef ec,
                          wxString input)
{
   OSStatus status = noErr;
   ByteCount byteOutLen;
   ByteCount byteInLen = input.Length();
   ByteCount byteBufferLen = byteInLen * 8 + 1;
   char* buf = new char[byteBufferLen] ;

   status = TECConvertText(ec,
                           (ConstTextPtr)input.c_str(),
                           byteInLen,
                           &byteInLen,
                           (TextPtr)buf,
                           byteBufferLen,
                           &byteOutLen);

   if (status != noErr) {
      delete[] buf;
      return input;
   }

   buf[byteOutLen] = 0;

   wxString result = wxString(buf, wxConvLocal);
   delete[] buf;

   return result;
}

wxString Internat::LocalToUTF8(const wxString &s)
{
   if (!mTECToUTF)
      return s;

   return MacConvertString((TECObjectRef)mTECToUTF, s);
}

wxString Internat::UTF8ToLocal(const wxString &s)
{
   if (!mTECFromUTF)
      return s;

   return MacConvertString((TECObjectRef)mTECFromUTF, s);
}

wxString Internat::ToFilename(const wxString &s)
{
   return LocalToUTF8(s);
}

wxString Internat::FromFilename(const wxString &s)
{
   return UTF8ToLocal(s);
}

#else

wxString Internat::LocalToUTF8(const wxString &s)
{
#if defined(__WXMAC__)
   return wxString(s, wxConvUTF8);
#else
   return wxString(s.wc_str(*mConvLocal), wxConvUTF8);
#endif
}

wxString Internat::UTF8ToLocal(const wxString &s)
{
#if defined(__WXMAC__)
   return wxString(s, wxConvLocal);
#else
   return wxString(s.wc_str(wxConvUTF8), *mConvLocal);
#endif
}

wxString Internat::ToFilename(const wxString &s)
{
   return s;
}

wxString Internat::FromFilename(const wxString &s)
{
   return s;
}

#endif

wxString Internat::SanitiseFilename(const wxString &name, const wxString &sub)
{
   wxString temp = name;
   for(unsigned i=0; i<exclude.Count(); i++)
   {
      if(temp.Contains(exclude.Item(i)))
      {
         temp.Replace(exclude.Item(i),sub);
      }
   }
   return temp;
}

wxString Internat::StripAccelerators(const wxString &s)
{
   wxString result;
   result.Alloc(s.Length());
   for(size_t i = 0; i < s.Length(); i++) {
      if (s[i] == '\t')
         break;
      if (s[i] != '&' && s[i] != '.')
         result += s[i];
   }
   return result;
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
// arch-tag: b6467fbe-b2f1-407c-bb61-da934ee307dc

