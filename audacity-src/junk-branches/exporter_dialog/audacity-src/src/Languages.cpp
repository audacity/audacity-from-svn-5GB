/**********************************************************************

  Audacity: A Digital Audio Editor

  Languages.cpp

  Dominic Mazzoni

  Figure out what translations are installed and return a list
  of language codes (like "es", "fr", or "pt-br") and corresponding
  language names (like "Español", "Français", and "Português").
  We use our own list of translations of language names (i.e.
  "Français" instead of "French") but we fallback on the language
  name in wxWindows if we don't have it listed.

  This code is designed to work well with all of the current
  languages, but adapt to any language that wxWindows supports.
  Other languages will only be supported if they're added to
  the database using wxLocale::AddLanguage.

  But for the most part, this means that somebody could add a new
  translation and have it work immediately.

**********************************************************************/

#include <wx/defs.h>
#include <wx/hashmap.h>
#include <wx/intl.h>

#include "Languages.h"

#include "Audacity.h"
#include "AudacityApp.h"

WX_DECLARE_STRING_HASH_MAP(wxString, LangHash);

bool TranslationExists(wxArrayString &audacityPathList, wxString code)
{
   wxArrayString results;   
   wxGetApp().FindFilesInPathList(wxString::Format("%s/audacity.mo",
                                                   (const char *)code),
                                  audacityPathList,
                                  wxFILE,
                                  results);
   
   wxGetApp().FindFilesInPathList(wxString::Format("%s/LC_MESSAGES/audacity.mo",
                                                   (const char *)code),
                                  audacityPathList,
                                  wxFILE,
                                  results);
   
   return (results.GetCount() > 0);
}

wxString GetSystemLanguageCode()
{
   wxArrayString langCodes;
   wxArrayString langNames;

   GetLanguages(langCodes, langNames);
   int sysLang = wxLocale::GetSystemLanguage();
   const wxLanguageInfo *info = wxLocale::GetLanguageInfo(sysLang);
   
   if (info) {
      wxString fullCode = info->CanonicalName;
      if (fullCode.Length() < 2)
         return "en";

      wxString code = fullCode.Left(2);
      unsigned int i;

      for(i=0; i<langCodes.GetCount(); i++) {
         if (langCodes[i] == fullCode)
            return fullCode;

         if (langCodes[i] == code)
            return code;
      }
   }

   return "en";
}

void GetLanguages(wxArrayString &langCodes, wxArrayString &langNames)
{
   wxArrayString tempNames;
   wxArrayString tempCodes;
   LangHash localLanguageName;
   LangHash reverseHash;

   localLanguageName["bg"] = "Balgarski";
   localLanguageName["ca"] = "Catalan";
   localLanguageName["da"] = "Dansk";
   localLanguageName["de"] = "Deutsch";
   localLanguageName["en"] = "English";
   localLanguageName["es"] = "Español";
   localLanguageName["fr"] = "Français";
   localLanguageName["it"] = "Italiano";
   localLanguageName["hu"] = "Magyar";
   localLanguageName["mk"] = "Makedonski";
   localLanguageName["nl"] = "Nederlands";
   localLanguageName["pl"] = "Polski";
   localLanguageName["pt"] = "Português";
   localLanguageName["ru"] = "Russky";
   localLanguageName["sl"] = "Slovenscina";

   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxGetApp().AddUniquePathToPathList(wxString::Format("%s/share/locale",
                                                       INSTALL_PREFIX),
                                      audacityPathList);
   wxString lastCode = "";

   int i;
   for(i=wxLANGUAGE_UNKNOWN; i<wxLANGUAGE_USER_DEFINED;i++) {
      const wxLanguageInfo *info = wxLocale::GetLanguageInfo(i);

      if (!info)
         continue;

      wxString fullCode = info->CanonicalName;
      wxString code = fullCode.Left(2);
      wxString name = info->Description;
      bool found = false;

      if (localLanguageName[fullCode] != "") {
         name = localLanguageName[fullCode];
      }
      if (localLanguageName[code] != "") {
         name = localLanguageName[code];
      }

      if (fullCode.Length() < 2)
         continue;

      if (TranslationExists(audacityPathList, fullCode)) {
         tempCodes.Add(fullCode);
         tempNames.Add(name);
         found = true;
      }

      if (code != lastCode) {
         if (TranslationExists(audacityPathList, code)) {
            tempCodes.Add(code);
            tempNames.Add(name);
            found = true;
         }

         if (code == "en" && !found) {
            tempCodes.Add(code);
            tempNames.Add(name);
            found = true;
         }
      }

      lastCode = code;
   }

   // Sort

   unsigned int j;
   for(j=0; j<tempNames.GetCount(); j++)
      reverseHash[tempNames[j]] = tempCodes[j];

   tempNames.Sort();

   for(j=0; j<tempNames.GetCount(); j++) {
      langNames.Add(tempNames[j]);
      langCodes.Add(reverseHash[tempNames[j]]);
   }
}

