/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandsMenu.cpp

  Brian Gunlogson

  This class creates menus, but does not assign them to anything.

**********************************************************************/

#include <wx/tokenzr.h>

#include "CommandsMenu.h"

#ifdef __WXGTK__
#define MAX_MENU_LEN 20
#define MAX_SUBMENU_LEN 10
#else
#define MAX_MENU_LEN 1000
#define MAX_SUBMENU_LEN 1000
#endif

CommandsMenu::CommandsMenu() : mCurrentID(0), mCurrentMenu(NULL)
{
}

CommandsMenu::~CommandsMenu()
{
   //WARNING: This removes menubars that could still be assigned to windows!
   PurgeData();
}

void CommandsMenu::PurgeData()
{
   unsigned int i;

   //remove all menu items, one by one
   //hopefully this fixes memory leaks on non-windows platforms
   //walk through the menu bar list
   for(i = 0; i < mMenuBarList.GetCount(); i++)
   {
      CommandsMenuMenuArray mnuMainArray;
      //remove individual menus
      for(unsigned int c = 0; c < mMenuBarList[i]->menubar->GetMenuCount(); c++)
      {
         CommandsMenuMenuArray mnuSubArray;
         wxMenu *mnuTmp = mMenuBarList[i]->menubar->GetMenu(c);

         if(!mnuTmp)
            continue;

         mnuMainArray.Add(mnuTmp);

         //Remove all items, store submenus for deletion, and delete all other items
         //WARNING: I doubt separators are deleted using this method.
         //Do they need to be deleted?
         for(int j = 1; j <= mCurrentID; j++)
         {
            wxMenu *mnuTmpParent;

            wxMenuItem *mnuItem = mnuTmp->FindItem(j, &mnuTmpParent);

            if(!mnuItem)
               continue;

            wxMenu *mnuSubMenu = mnuItem->GetSubMenu();

            if(mnuSubMenu)
               mnuSubArray.Add(mnuSubMenu);

            mnuTmpParent->Delete(mnuItem);
         }

         //Delete submenus
         while(mnuSubArray.GetCount())
         {
            delete mnuSubArray[0];
            mnuSubArray.RemoveAt(0);
         }

         //clear the menu array
         WX_CLEAR_ARRAY(mnuSubArray);
         mnuSubArray.Clear();
      }

      //Delete menus
      while(mnuMainArray.GetCount())
      {
         mMenuBarList[i]->menubar->Remove(0);
         delete mnuMainArray[0];
         mnuMainArray.RemoveAt(0);
      }

      //clear the menu array
      WX_CLEAR_ARRAY(mnuMainArray);
      mnuMainArray.Clear();
   }

   //clear the menu bar list
   for(i = 0; i < mMenuBarList.GetCount(); i++)
      delete mMenuBarList[i]->menubar;
   WX_CLEAR_ARRAY(mMenuBarList);
   mMenuBarList.Clear();

   //clear the other arrays
   WX_CLEAR_ARRAY(mSubMenuList);
   mSubMenuList.Clear();

   WX_CLEAR_ARRAY(mIdentifierNameList);
   mIdentifierNameList.Clear();

   //reset other variables
   mCurrentMenu = NULL;
   mCurrentID = 0;
}

void CommandsMenu::AddMenuBar(wxString &sMenu)
{
   MenuBarListEntry *tmpEntry = new MenuBarListEntry;

   tmpEntry->menubar = new wxMenuBar();
   tmpEntry->name = sMenu;

   mMenuBarList.Add(tmpEntry);
}

wxMenuBar * CommandsMenu::GetMenuBar(wxString &sMenu)
{
   for(unsigned int i = 0; i < mMenuBarList.GetCount(); i++)
   {
      if(!mMenuBarList[i]->name.Cmp(sMenu))
         return mMenuBarList[i]->menubar;
   }

   return NULL;
}

wxMenuBar * CommandsMenu::CurrentMenuBar()
{
   if(mMenuBarList.IsEmpty())
      return NULL;

   return mMenuBarList[mMenuBarList.GetCount()-1]->menubar;
}

void CommandsMenu::BeginMenu(wxString &tName)
{
   wxMenu *tmpMenu = new wxMenu();

   mCurrentMenu = tmpMenu;

   CurrentMenuBar()->Append(mCurrentMenu, tName);
}

void CommandsMenu::EndMenu()
{
   mCurrentMenu = NULL;
}

void CommandsMenu::BeginSubMenu(wxString &tName)
{
   SubMenuListEntry *tmpEntry = new SubMenuListEntry;

   tmpEntry->menu = new wxMenu();
   tmpEntry->name = tName;

   mSubMenuList.Add(tmpEntry);
}

void CommandsMenu::EndSubMenu()
{
   size_t submenu_count = mSubMenuList.GetCount()-1;

   //Save the submenu's information
   SubMenuListEntry *tmpSubMenu = mSubMenuList[submenu_count];

   //Pop off the new submenu so CurrentMenu returns the parent of the submenu
   mSubMenuList.RemoveAt(submenu_count);

   //Add the submenu to the current menu
   CurrentMenu()->Append(GetUniqueIdentifier(), tmpSubMenu->name, tmpSubMenu->menu, tmpSubMenu->name);

   delete tmpSubMenu;
}

wxMenu * CommandsMenu::CurrentSubMenu()
{
   if(mSubMenuList.IsEmpty())
      return NULL;

   return mSubMenuList[mSubMenuList.GetCount()-1]->menu;
}

wxMenu * CommandsMenu::CurrentMenu()
{
   if(!mCurrentMenu)
      return NULL;

   wxMenu * tmpCurrentSubMenu = CurrentSubMenu();

   if(!tmpCurrentSubMenu)
   {
      return mCurrentMenu;
   }

   return tmpCurrentSubMenu;
}

void CommandsMenu::AddItem(wxString &tName, wxString &sFunctions, wxString &sKeys)
{
   wxString finalName = AppendComboString(tName, sKeys);

   CurrentMenu()->Append(GetUniqueIdentifier(sFunctions, sKeys), finalName);
}

void CommandsMenu::AddSeparator()
{
   CurrentMenu()->AppendSeparator();
}

void CommandsMenu::AddDynamicItem(wxString &sName)
{
   int effType = -1;
   EffectArray *effs;

   //determine what dynamic item to add
   if(!sName.Cmp("GeneratePlugins"))
      effType = INSERT_EFFECT;
   else if(!sName.Cmp("EffectPlugins"))
      effType = PROCESS_EFFECT;
   else if(!sName.Cmp("AnalyzePlugins"))
      effType = ANALYZE_EFFECT;
   else
      return;

   effs = Effect::GetEffects(BUILTIN_EFFECT | effType);
   AppendEffects(effs, sName, false);
   delete effs;

   effs = Effect::GetEffects(PLUGIN_EFFECT | effType);
   if (effs->GetCount() > 0)
   {
      AddSeparator();
      AppendEffects(effs, sName, true);
   }
   delete effs;

   if(!CurrentMenu()->GetMenuItemCount())
   {
      //Add disabled None item
      AddItem(wxString(_("None")), wxString(""), wxString(""));
      CurrentMenu()->Enable(mCurrentID, false);
   }
}

void CommandsMenu::AppendEffect(int idEffect, wxString &sName, wxString &sType)
{
   AddItem(sName, wxString::Format("%i@%s@Effect", idEffect, sType.c_str()), wxString(""));
}

void CommandsMenu::AppendEffects(EffectArray *effs, wxString &sType, bool spill)
{
   unsigned int currentLen = CurrentMenu()->GetMenuItemCount();
   unsigned int effLen = effs->GetCount();
   unsigned int i;

   if (!spill || currentLen + effLen <= MAX_MENU_LEN)
   {
      for(i=0; i<effLen; i++)
         AppendEffect((*effs)[i]->GetID(), (*effs)[i]->GetEffectName(), sType);
      return;
   }

   // There were too many effects in this menu.  Put the
   // extras (plug-ins) in submenus.

   wxString label;
   int listnum = 1;

   BeginSubMenu(wxString::Format("Plugins #%i", listnum));

   for (i=0; i<effLen; i++) {
      AppendEffect((*effs)[i]->GetID(), (*effs)[i]->GetEffectName(), sType);

      if(((i+1) % MAX_SUBMENU_LEN) == 0 && i != (effLen - 1))
      {
         EndSubMenu();
         listnum++;
         BeginSubMenu(wxString::Format("Plugins #%i", listnum));
      }

   }

   EndSubMenu();
}

//Gets an unused identifier, currently used for menus only
//WARNING: Does this conflict with the identifiers set for controls/windows?
//If it does, a workarround may be to keep controls below wxID_LOWEST
//and keep menus above wxID_HIGHEST
int CommandsMenu::GetUniqueIdentifier(wxString &sFunctions, wxString &sKeys)
{
   mCurrentID++;

   //Skip the reserved identifiers used by wxWindows
   if((mCurrentID >= wxID_LOWEST) && (mCurrentID <= wxID_HIGHEST))
      mCurrentID = wxID_HIGHEST+1;

   if(sFunctions.Length())
      SetIdentifierData(mCurrentID, sFunctions, sKeys);

   return mCurrentID;
}

void CommandsMenu::SetIdentifierData(int nID, wxString &sFunctions, wxString &sKeys)
{
   IdentifierNameListEntry *tmpEntry = new IdentifierNameListEntry;

   tmpEntry->id = nID;
   tmpEntry->functions = sFunctions;
   tmpEntry->keys = sKeys;
   tmpEntry->menu = CurrentMenu();

   mIdentifierNameList.Add(tmpEntry);
}

wxMenu * CommandsMenu::GetMenuFromIdentifier(int nID)
{
   for(unsigned int i = 0; i < mIdentifierNameList.GetCount(); i++)
   {
      if(mIdentifierNameList[i]->id == nID)
         return mIdentifierNameList[i]->menu;
   }

   return NULL;
}

int CommandsMenu::GetIdentifiersFromFunction(wxString &sFunction, bool bReset)
{
   static unsigned int uIndex = 0;

   if(bReset)
      uIndex = 0;

   for(unsigned int i = uIndex; i < mIdentifierNameList.GetCount(); i++)
   {
      uIndex = i+1;

      wxStringTokenizer tFunctions(mIdentifierNameList[i]->functions, ":");
      while(tFunctions.HasMoreTokens())
      {
         wxString token = tFunctions.GetNextToken();

         if(!token.Cmp(sFunction))
            return mIdentifierNameList[i]->id;
      }
   }

   return -1;
}

int CommandsMenu::GetIdentifierFromFunctions(wxString &sFunctions)
{
   for(unsigned int i = 0; i < mIdentifierNameList.GetCount(); i++)
   {
      if(!mIdentifierNameList[i]->functions.Cmp(sFunctions))
         return mIdentifierNameList[i]->id;
   }

   return -1;
}

wxString CommandsMenu::GetFunctionsFromIdentifier(int nID)
{
   for(unsigned int i = 0; i < mIdentifierNameList.GetCount(); i++)
   {
      if(mIdentifierNameList[i]->id == nID)
         return mIdentifierNameList[i]->functions;
   }

   return "";
}

wxString CommandsMenu::GetKeysFromIdentifier(int nID)
{
   for(unsigned int i = 0; i < mIdentifierNameList.GetCount(); i++)
   {
      if(mIdentifierNameList[i]->id == nID)
         return mIdentifierNameList[i]->keys;
   }

   return "";
}

int CommandsMenu::GetIdentifierFromKey(wxString &sKey)
{
   for(unsigned int i = 0; i < mIdentifierNameList.GetCount(); i++)
   {
      wxStringTokenizer tKeys(mIdentifierNameList[i]->keys, ":");
      while(tKeys.HasMoreTokens())
      {
         wxString token = tKeys.GetNextToken();

         if(!token.Cmp(sKey))
            return mIdentifierNameList[i]->id;
      }
   }

   return -1;
}

int CommandsMenu::GetIdentifierFromKeys(wxString &sKeys)
{
   return GetIdentifierFromKey(GetFirstKey(sKeys));
}

wxString CommandsMenu::GetFirstKey(wxString &sKeys)
{
   wxStringTokenizer tKeys(sKeys, ":");
   if(tKeys.HasMoreTokens())
   {
      return tKeys.GetNextToken();
   }

   return "";
}

wxString CommandsMenu::AppendComboString(wxString &tName, wxString &sKeys)
{
   //see if tName contains tabs
   if((tName.Find('\t') != -1) || (tName.Find("\\t") != -1))
      return tName;

   wxString firstKey = GetFirstKey(sKeys);

   if(!firstKey)
      return tName;

   return (tName+"\t"+firstKey);
}
