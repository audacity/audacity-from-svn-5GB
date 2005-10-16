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
   wxGetApp().FindFilesInPathList(wxString::Format(wxT("%s/audacity.mo"),
                                                   code.c_str()),
                                  audacityPathList,
                                  wxFILE,
                                  results);
   
   wxGetApp().FindFilesInPathList(wxString::Format(wxT("%s/LC_MESSAGES/audacity.mo"),
                                                   code.c_str()),
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
         return wxT("en");

      wxString code = fullCode.Left(2);
      unsigned int i;

      for(i=0; i<langCodes.GetCount(); i++) {
         if (langCodes[i] == fullCode)
            return fullCode;

         if (langCodes[i] == code)
            return code;
      }
   }

   return wxT("en");
}

void GetLanguages(wxArrayString &langCodes, wxArrayString &langNames)
{
   wxArrayString tempNames;
   wxArrayString tempCodes;
   LangHash localLanguageName;
   LangHash reverseHash;

   // MM: Use only ASCII characters here to avoid unnecessary
   //     problems with charset conversions on Linux systems
   localLanguageName[wxT("bg")] = wxT("Balgarski");
   localLanguageName[wxT("ca")] = wxT("Catalan");
   localLanguageName[wxT("da")] = wxT("Dansk");
   localLanguageName[wxT("de")] = wxT("Deutsch");
   localLanguageName[wxT("en")] = wxT("English");
   localLanguageName[wxT("es")] = wxT("Espanol");
   localLanguageName[wxT("fi")] = wxT("Suomi");
   localLanguageName[wxT("fr")] = wxT("Francais");
   localLanguageName[wxT("it")] = wxT("Italiano");
   localLanguageName[wxT("ja")] = wxT("Nihongo");
   localLanguageName[wxT("hu")] = wxT("Magyar");
   localLanguageName[wxT("mk")] = wxT("Makedonski");
   localLanguageName[wxT("nl")] = wxT("Nederlands");
   localLanguageName[wxT("nb")] = wxT("Norsk");
   localLanguageName[wxT("pl")] = wxT("Polski");
   localLanguageName[wxT("pt")] = wxT("Portugues");
   localLanguageName[wxT("ru")] = wxT("Russky");
   localLanguageName[wxT("sl")] = wxT("Slovenscina");
   localLanguageName[wxT("sv")] = wxT("Svenska");

   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxGetApp().AddUniquePathToPathList(wxString::Format(wxT("%s/share/locale"),
                                                       wxT(INSTALL_PREFIX)),
                                      audacityPathList);
   wxString lastCode = wxT("");

   int i;
   for(i=wxLANGUAGE_UNKNOWN; i<wxLANGUAGE_USER_DEFINED;i++) {
      const wxLanguageInfo *info = wxLocale::GetLanguageInfo(i);

      if (!info)
         continue;

      wxString fullCode = info->CanonicalName;
      wxString code = fullCode.Left(2);
      wxString name = info->Description;
      bool found = false;

      if (localLanguageName[fullCode] != wxT("")) {
         name = localLanguageName[fullCode];
      }
      if (localLanguageName[code] != wxT("")) {
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

         if (code == wxT("en") && !found) {
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


// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: f4fe3d35-5ae6-4ca4-b151-293551801fa1

