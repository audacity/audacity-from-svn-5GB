/**********************************************************************

  Audacity: A Digital Audio Editor

  Internat.cpp

  Markus Meyer
  Dominic Mazzoni (Mac OS X code)

**********************************************************************/

#include "Internat.h"

#include <wx/log.h>
#include <wx/intl.h>

#ifdef __WXMAC__
#include <wx/mac/private.h>
#endif

#include <locale.h>

wxChar Internat::mDecimalSeparator = '.'; // default
wxMBConv *Internat::mConvLocal = 0;

#ifdef __WXMAC__
void *Internat::mTECToUTF = NULL;
void *Internat::mTECFromUTF = NULL;
#endif

void Internat::Init()
{
   // Set up character-set conversion for UTF-8 input and output.
   mConvLocal = new wxCSConv(wxLocale::GetSystemEncodingName());

   // There is no way to check the 'default' (rather than the current
   // decimal separator character), so we set the 'default' number locale
   // explicitely, then reset it to "C", because Nyquist and other
   // parts of Audacity expect printf() etc. expect the "C" locale for
   // numbers.

   // Set default numeric locale
   setlocale(LC_NUMERIC, "");

   // Save decimal point character
   struct lconv * localeInfo = localeconv();
   if (localeInfo)
      mDecimalSeparator = localeInfo->decimal_point[0];

   wxLogDebug("Decimal separator set to '%c'", mDecimalSeparator);

   // Reset to C numeric locale
   setlocale(LC_NUMERIC, "C");

   #ifdef __WXMAC__
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

wxChar Internat::GetDecimalSeparator()
{
   return mDecimalSeparator;
}

bool Internat::ToDouble(const wxString& stringToConvert, double* result)
{
   wxString s = stringToConvert;

   wxChar decimalSeparator = GetDecimalSeparator();

   if (decimalSeparator != '.')
      s.Replace(wxString(decimalSeparator), ".");

   return s.ToDouble(result);
}

double Internat::ToDouble(const wxString& stringToConvert)
{
   double result;
   Internat::ToDouble(stringToConvert, &result);
   return result;
}

bool Internat::CompatibleToDouble(const wxString& stringToConvert, double* result)
{
   // Regardless of the locale, always respect comma _and_ point
   wxString s = stringToConvert;
   s.Replace(",", ".");

   return s.ToDouble(result);
}

double Internat::CompatibleToDouble(const wxString& stringToConvert)
{
   double result;
   Internat::CompatibleToDouble(stringToConvert, &result);
   return result;
}

wxString Internat::ToString(double numberToConvert,
                     int digitsAfterDecimalPoint /* = -1 */,
		     wxChar decimalSeparatorChar /* = 0 */)
{
   wxString format, result;

   if (digitsAfterDecimalPoint == -1)
      format = "%f";
   else
      format.Printf("%%.%if", digitsAfterDecimalPoint);

   result.Printf(format, numberToConvert);

   if (decimalSeparatorChar == 0)
      decimalSeparatorChar = GetDecimalSeparator();

   if (decimalSeparatorChar != '.')
      result.Replace(".", wxString(decimalSeparatorChar));

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

   wxString result = wxString(buf);
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

