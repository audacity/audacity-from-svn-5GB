/**********************************************************************

  Audacity: A Digital Audio Editor

  Languages.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/intl.h>

#include "Languages.h"

#include "Audacity.h"
#include "AudacityApp.h"

void GetLanguages(wxArrayString &langCodes, wxArrayString &langNames)
{
   //
   // TODO:
   //
   // This is a placeholder until the code which
   // actually searches for locale files (below)
   // is finished...
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
   langCodes.Add("sl"); langNames.Add("Slovenscina");
}

#if 0

// This code doesn't work yet

   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxArrayString results;

   langCodes.Add("en"); langNames.Add("English");

   wxGetApp().AddUniquePathToPathList(wxString::Format("%s/share/locale",
                                                       INSTALL_PREFIX),
                                      audacityPathList);

   wxGetApp().FindFilesInPathList("*/audacity.mo",
                                  audacityPathList,
                                  wxFILE,
                                  results);

   wxGetApp().FindFilesInPathList("*/LC_MESSAGES/audacity.mo",
                                  audacityPathList,
                                  wxFILE,
                                  results);

   for(unsigned int i=0; i<results.GetCount(); i++) {
      wxString path = ::wxPathOnly(results[i]);
      if (path.Length() > 12 && path.Right(11)=="LC_MESSAGES")
         path = path.Left(path.Length()-12);
      wxString langCode = path.AfterLast(wxFILE_SEP_PATH);
      //wxLanguageInfo *info = wxLocale::GetLanguageInfo(langCode);
      printf("Found %s\n", (const char *)langCode);
   }
#endif

