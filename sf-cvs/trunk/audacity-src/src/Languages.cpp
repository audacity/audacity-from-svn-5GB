/**********************************************************************

  Audacity: A Digital Audio Editor

  Languages.cpp

  Dominic Mazzoni

**********************************************************************/

#include "Languages.h"

void GetLanguages(wxArrayString &langCodes, wxArrayString &langNames)
{
   //
   // TODO:
   //
   // This is just a placeholder until somebody writes code
   // that actually searches for locale files...
   //

   langCodes.Add("bg"); langNames.Add("Balgarski");
   langCodes.Add("da"); langNames.Add("Dansk");
   langCodes.Add("de"); langNames.Add("Deutch");
   langCodes.Add("en"); langNames.Add("English");
   langCodes.Add("es"); langNames.Add("Español");
   langCodes.Add("fr"); langNames.Add("Français");
   langCodes.Add("it"); langNames.Add("Italiano");
   langCodes.Add("hu"); langNames.Add("Magyar");
   langCodes.Add("nl"); langNames.Add("Nederlands");
   langCodes.Add("pl"); langNames.Add("Polski");
   langCodes.Add("ru"); langNames.Add("Russky");
}
