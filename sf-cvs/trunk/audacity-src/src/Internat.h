/**********************************************************************

  Audacity: A Digital Audio Editor

  Internat.h

  Markus Meyer

**********************************************************************/

#ifndef __AUDACITY_INTERNAT__
#define __AUDACITY_INTERNAT__

#include <wx/string.h>

// This class is used to help internationalisation
// Currently, it deals mostly with converting numbers.
class Internat
{
public:
   // Initialize internationalisation support. Call this once at
   // program start.
   static void Init();

   // Get the decimal separator for the current locale. Normally, this is
   // a decimal point ('.'), but e.g. Germany uses a comma (',').
   static wxChar GetDecimalSeparator();

   // Convert a string to a number. This function will only accept the
   // decimal separator for the current locale. Returns 'true' on success,
   // and 'false' if an error occurs.
   static bool ToDouble(const wxString& stringToConvert, double* result);

   // Function version of above.
   static double ToDouble(const wxString& stringToConvert);

   // Convert a string to a number. This function will accept BOTH point
   // and comma as a decimal separator, regardless of the current locale.
   // Returns 'true' on success, and 'false' if an error occurs.
   static bool CompatibleToDouble(const wxString& stringToConvert, double* result);

   // Function version of above.
   static double CompatibleToDouble(const wxString& stringToConvert);

   // Convert a number to a string, using the given decimal separator.
   // The default is to use the decimal separator for the current locale.
   static wxString ToString(double numberToConvert,
                     int digitsAfterDecimalPoint = -1,
		     wxChar decimalSeparatorChar = 0);

   // Convert strings to and from UTF-8 (used for XML files).
   static wxString UTF8ToLocal(const wxString &s);
   static wxString LocalToUTF8(const wxString &s);

private:
   static wxChar mDecimalSeparator;
   static wxMBConv *mConvLocal;
};

#endif
