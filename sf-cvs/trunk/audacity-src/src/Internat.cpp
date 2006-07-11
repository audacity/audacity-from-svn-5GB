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

#ifdef __WXMAC__
#include <wx/mac/private.h>
#endif

#include <locale.h>

wxChar Internat::mDecimalSeparator = wxT('.'); // default
wxMBConv *Internat::mConvLocal = 0;

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
   double result;
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

#ifdef __WXMAC__

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
   return wxString(s.wc_str(*mConvLocal), wxConvUTF8);
}

wxString Internat::UTF8ToLocal(const wxString &s)
{
   return wxString(s.wc_str(wxConvUTF8), *mConvLocal);
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

