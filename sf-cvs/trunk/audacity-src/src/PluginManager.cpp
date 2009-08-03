/**********************************************************************

  Audacity: A Digital Audio Editor

  PluginManager.cpp

  Leland Lucius

*******************************************************************//*!

\file PluginManager.cpp
\brief

*//*******************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/dynarray.h>
#include <wx/dynlib.h>
#include <wx/filename.h>
#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>
#include <wx/wfstream.h>

#include "FileNames.h"
#include "xml/XMLFileReader.h"
#include "xml/XMLWriter.h"

#include "PluginManager.h"

#include <wx/arrimpl.cpp>

///////////////////////////////////////////////////////////////////////////////
//
// PluginManager
//
///////////////////////////////////////////////////////////////////////////////

static PluginManager mInstance;

PluginManager::PluginManager()
{
   mConfig = NULL;
   mDirty = false;
}

PluginManager::~PluginManager()
{
   if (mConfig) {
      Close();
   }
}

PluginManager & PluginManager::Get(bool refresh)
{
   return mInstance;
}

void PluginManager::Open()
{
   Close();

   wxFileName name(FileNames::PluginsCache());
   wxFile file;

   if (!::wxFileExists(FileNames::PluginsCache())) {
      file.Create(FileNames::PluginsCache());
      file.Close();
   }

   wxFileInputStream stream(FileNames::PluginsCache());

   mConfig = new wxFileConfig(stream);

   file.Close();
}

void PluginManager::Close()
{
   if (mConfig && IsDirty()) {

      wxFile file(FileNames::PluginsCache(), wxFile::write);
      if (!file.IsOpened()) {
         wxLogDebug(wxT("Couldn't open plugins cache for write"));
         return;
      }

      wxFileOutputStream stream(file);

      SetDirty(!mConfig->Save(stream));

      file.Close();
   }

   if (mConfig && !IsDirty()) {
      delete mConfig;
      mConfig = NULL;
   }
}

bool PluginManager::IsDirty()
{
   return mDirty;
}

void PluginManager::SetDirty(bool dirty)
{
   mDirty = dirty;
}

wxString PluginManager::Read(const wxString & key, const wxString & def)
{
   if (mConfig) {
      return mConfig->Read(key, def);
   }
   
   return def;
}

long PluginManager::Read(const wxString & key, long def)
{
   if (mConfig) {
      return mConfig->Read(key, def);
   }
   
   return def;
}

void PluginManager::Write(const wxString & key, const wxString & val)
{
   if (mConfig) {
      mConfig->Write(key, val);

      SetDirty();
   }

   return;
}

void PluginManager::Write(const wxString & key, long val)
{
   if (mConfig) {
      mConfig->Write(key, val);

      SetDirty();
   }
   
   return;
}

bool PluginManager::HasType(const wxString & type)
{
   if (!mConfig) {
      return false;;
   }

   wxString path;

   path.Printf(wxT("/%s"), type.c_str());

   return mConfig->HasGroup(path);
}

void PluginManager::PurgeType(const wxString & type)
{
   if (!mConfig) {
      return;
   }

   wxString path;

   path.Printf(wxT("/%s"), type.c_str());

   mConfig->DeleteGroup(path);
}

int PluginManager::GetPluginCount(const wxString & type)
{
   if (!mConfig) {
      return 0;
   }

   wxString path = mConfig->GetPath();

   mConfig->SetPath(wxString::Format(wxT("/%s"), type.c_str()));

   int cnt = mConfig->GetNumberOfGroups();

   mConfig->SetPath(path);

   return cnt;
}

wxString PluginManager::GetPlugin(const wxString & type, int index)
{
   if (!mConfig) {
      return wxEmptyString;
   }

   wxString path;

   path.Printf(wxT("/%s/%d"), type.c_str(), index);
   if (!mConfig->HasGroup(path)) {
      return wxEmptyString;
   }

   mConfig->SetPath(path);

   path = mConfig->Read(wxT("PluginPath"), wxEmptyString);

   mConfig->SetPath(wxT("private"));

   return path;
}

wxString PluginManager::GetFirstPlugin(const wxString & type)
{
   if (!mConfig) {
      return wxEmptyString;
   }

   wxString path;

   mCurrentIndex = -1;

   do {
      path.Printf(wxT("/%s/%d"), type.c_str(), ++mCurrentIndex);
      if (!mConfig->HasGroup(path)) {
         return wxEmptyString;
      }

      mConfig->SetPath(path);
   } while (!mConfig->Read(wxT("Enabled"), (bool) true));

   path = mConfig->Read(wxT("PluginPath"), wxEmptyString);

   mConfig->SetPath(wxT("private"));

   return path;
}

wxString PluginManager::GetNextPlugin(const wxString & type)
{
   if (!mConfig) {
      return wxEmptyString;
   }

   wxString path;

   do {
      path.Printf(wxT("/%s/%d"), type.c_str(), ++mCurrentIndex);
      if (!mConfig->HasGroup(path)) {
         return wxEmptyString;
      }

      mConfig->SetPath(path);
   } while (!mConfig->Read(wxT("Enabled"), (bool) true));

   path = mConfig->Read(wxT("PluginPath"), wxEmptyString);

   mConfig->SetPath(wxT("private"));

   return path;
}

bool PluginManager::IsRegistered(const wxString & type, const wxString & path)
{
   int cnt = GetPluginCount(type);

   for (int i = 0; i < cnt; i++) {
      wxString registered = GetPlugin(type, i);
      if (registered == path) {
         return true;
      }
   }

   return false;
}

void PluginManager::RegisterPlugin(const wxString & type, const wxString & path)
{
   if (IsRegistered(type, path)) {
      return;
   }

   mConfig->SetPath(wxString::Format(wxT("/%s/%d"), type.c_str(), GetPluginCount(type)));

   mConfig->Write(wxT("PluginPath"), path);
   SetDirty();

   mConfig->SetPath(wxT("private"));

   return;
}

bool PluginManager::IsPluginEnabled(const wxString & type, const wxString & path)
{
   int cnt = GetPluginCount(type);

   for (int i = 0; i < cnt; i++) {
      wxString registered = GetPlugin(type, i);
      if (registered == path) {
         mConfig->SetPath(wxT(".."));
         bool enabled = mConfig->Read(wxT("Enabled"), true) != false;
         mConfig->SetPath(wxT("private"));
         return enabled;
      }
   }

   return true;
}

void PluginManager::EnablePlugin(const wxString & type, const wxString & path, bool enable)
{
   int cnt = GetPluginCount(type);

   for (int i = 0; i < cnt; i++) {
      wxString registered = GetPlugin(type, i);
      if (registered == path) {
         mConfig->SetPath(wxT(".."));
         mConfig->Write(wxT("Enabled"), enable);
         SetDirty(true);
         mConfig->SetPath(wxT("private"));
         break;
      }
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
