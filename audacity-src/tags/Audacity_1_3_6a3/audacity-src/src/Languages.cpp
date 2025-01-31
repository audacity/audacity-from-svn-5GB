/**********************************************************************

  Audacity: A Digital Audio Editor

  Languages.cpp

  Dominic Mazzoni


*******************************************************************//*!

\file Languages.cpp
\brief Determine installed languages.

  Figure out what translations are installed and return a list
  of language codes (like "es", "fr", or "pt-br") and corresponding
  language names (like "Espa�ol", "Fran�ais", and "Portugu�s").
  We use our own list of translations of language names (i.e.
  "Fran�ais" instead of "French") but we fallback on the language
  name in wxWindows if we don't have it listed.

  This code is designed to work well with all of the current
  languages, but adapt to any language that wxWindows supports.
  Other languages will only be supported if they're added to
  the database using wxLocale::AddLanguage.

  But for the most part, this means that somebody could add a new
  translation and have it work immediately.

*//*******************************************************************/


#include "Audacity.h"

#include <wx/defs.h>
#include <wx/hashmap.h>
#include <wx/intl.h>

#include "Languages.h"

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
   LangHash tempHash;

   // MM: Use only ASCII characters here to avoid problems with
   //     charset conversion on Linux platforms
   localLanguageName[wxT("ar")] = wxT("Arabic");
   localLanguageName[wxT("bg")] = wxT("Balgarski");
   localLanguageName[wxT("ca")] = wxT("Catalan");
   localLanguageName[wxT("cs")] = wxT("Czech");
   localLanguageName[wxT("da")] = wxT("Dansk");
   localLanguageName[wxT("de")] = wxT("Deutsch");
   localLanguageName[wxT("el")] = wxT("Ellinika");
   localLanguageName[wxT("en")] = wxT("English");
   localLanguageName[wxT("es")] = wxT("Espanol");
   localLanguageName[wxT("eu")] = wxT("Euskara");
   localLanguageName[wxT("fi")] = wxT("Suomi");
   localLanguageName[wxT("fr")] = wxT("Francais");
   localLanguageName[wxT("ga")] = wxT("Gaeilge");
   localLanguageName[wxT("it")] = wxT("Italiano");
   localLanguageName[wxT("ja")] = wxT("Nihongo");
   localLanguageName[wxT("lo")] = wxT("Lao"); // aka Laothian
   localLanguageName[wxT("lt")] = wxT("Lietuviu");
   localLanguageName[wxT("he")] = wxT("Hebrew");
   localLanguageName[wxT("hu")] = wxT("Magyar");
   localLanguageName[wxT("mk")] = wxT("Makedonski");
   localLanguageName[wxT("nl")] = wxT("Nederlands");
   localLanguageName[wxT("nb")] = wxT("Norsk");
   localLanguageName[wxT("pl")] = wxT("Polski");
   localLanguageName[wxT("pt")] = wxT("Portugues");
//   localLanguageName[wxT("pt_BR")] = wxT("Portugues (Brazil)");
//   only if we have to distinguish two sorts of portuguese
   localLanguageName[wxT("ru")] = wxT("Russky");
   localLanguageName[wxT("sl")] = wxT("Slovenscina");
   localLanguageName[wxT("sv")] = wxT("Svenska");
   localLanguageName[wxT("tr")] = wxT("Turkce");
   localLanguageName[wxT("uk")] = wxT("Ukrainska");
   localLanguageName[wxT("vi")] = wxT("Vietnamese");
   localLanguageName[wxT("zh")] = wxT("Chinese (Simplified)");
   localLanguageName[wxT("zh_TW")] = wxT("Chinese (Traditional)");

   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxGetApp().AddUniquePathToPathList(wxString::Format(wxT("%s/share/locale"),
                                                       wxT(INSTALL_PREFIX)),
                                      audacityPathList);
   int i;
   for(i=wxLANGUAGE_UNKNOWN; i<wxLANGUAGE_USER_DEFINED;i++) {
      const wxLanguageInfo *info = wxLocale::GetLanguageInfo(i);

      if (!info)
         continue;

      wxString fullCode = info->CanonicalName;
      wxString code = fullCode.Left(2);
      wxString name = info->Description;

      // Logic: Languages codes are sometimes hierarchical, with a
      // general language code and then a subheading.  For example,
      // zh_TW for Traditional Chinese and zh_CN for Simplified
      // Chinese - but just zh for Chinese in general.  First we look
      // for the full code, like zh_TW.  If that doesn't exist, we
      // look for a code corresponding to the first two letters.
      // Note that if the language for a fullCode exists but we only
      // have a name for the short code, we will use the short code's
      // name but associate it with the full code.  This allows someone
      // to drop in a new language and still get reasonable behavior.

      if (fullCode.Length() < 2)
         continue;

      if (localLanguageName[code] != wxT("")) {
         name = localLanguageName[code];
      }
      if (localLanguageName[fullCode] != wxT("")) {
         name = localLanguageName[fullCode];
      }

      if (TranslationExists(audacityPathList, fullCode)) {
         code = fullCode;
      }

      if (tempHash[code] != wxT(""))
         continue;

      if (TranslationExists(audacityPathList, code) || code==wxT("en")) {
         tempCodes.Add(code);
         tempNames.Add(name);
         tempHash[code] = name;

         /* for debugging */
         wxLogDebug(wxT("code=%s name=%s fullCode=%s name=%s -> %s\n"),
                code.c_str(), localLanguageName[code].c_str(),
                fullCode.c_str(), localLanguageName[fullCode].c_str(),
                name.c_str());
      }
   }


   // JKC: Adding language for simplified audacity.
   {
      wxString code;
      wxString name;
      code = wxT("en-simple");
      name = wxT("Simplified");
      if (TranslationExists(audacityPathList, code) ) {
         tempCodes.Add(code);
         tempNames.Add(name);
         tempHash[code] = name;

         /* for debugging
         printf(wxT("code=%s name=%s fullCode=%s name=%s -> %s\n"),
                code.c_str(), localLanguageName[code].c_str(),
                fullCode.c_str(), localLanguageName[fullCode].c_str(),
                name.c_str());
         */
      }
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

