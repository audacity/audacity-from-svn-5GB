/**********************************************************************

  Audacity: A Digital Audio Editor

  Internat.cpp

  Markus Meyer

**********************************************************************/

#include "Internat.h"
#include "wx/log.h"

#include <locale.h>

wxChar Internat::mDecimalSeparator = '.'; // default

void Internat::Init()
{
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

