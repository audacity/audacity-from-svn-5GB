/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandsMenu.h

  Brian Gunlogson

  This class creates menus, but does not assign them to anything.

**********************************************************************/

#include <wx/string.h>
#include <wx/dynarray.h>
#include <wx/menu.h>

#include "../effects/Effect.h"

class wxListCtrl;

struct MenuBarListEntry
{
   wxString name;
   wxMenuBar *menubar;
};

struct SubMenuListEntry
{
   wxString name;
   wxMenu *menu;
};

struct IdentifierNameListEntry
{
   wxString functions;
   wxString keys;
   int id;
   wxMenu *menu;
};

WX_DEFINE_ARRAY(MenuBarListEntry *, MenuBarList);
WX_DEFINE_ARRAY(SubMenuListEntry *, SubMenuList);
WX_DEFINE_ARRAY(IdentifierNameListEntry *, IdentifierNameList);
WX_DEFINE_ARRAY(wxMenu *, CommandsMenuMenuArray);

class CommandsMenu
{
public:
   CommandsMenu();
   ~CommandsMenu();

   void PurgeData();

   void AddMenuBar(wxString sMenu);
   wxMenuBar * GetMenuBar(wxString sMenu);
   wxMenuBar * CurrentMenuBar();

   void BeginMenu(wxString tName);
   void EndMenu();

   void BeginSubMenu(wxString tName);
   void EndSubMenu();
   wxMenu * CurrentSubMenu();

   wxMenu * CurrentMenu();

   void AddItem(wxString tName, wxString sFunctions, wxString sKeys);
   void AddSeparator();
   void AddDynamicItem(wxString sName);
   void AppendEffect(int idEffect, wxString sName, wxString sType);
   void AppendEffects(EffectArray *effs, wxString sType, bool spill);

   int GetUniqueIdentifier(wxString sFunctions = "", wxString sKeys = "");
   void SetIdentifierData(int nID, wxString sFunctions = "", wxString sKeys = "");

   wxMenu * GetMenuFromIdentifier(int nID);

   int GetIdentifiersFromFunction(wxString sFunction, bool bReset = false);
   int GetIdentifierFromFunctions(wxString sFunctions);
   wxString GetFunctionsFromIdentifier(int nID);

   wxString GetKeysFromIdentifier(int nID);
   int GetIdentifierFromKey(wxString sKey);
   int GetIdentifierFromKeys(wxString sKeys);

   wxString GetFirstKey(wxString sKeys);

   wxString AppendComboString(wxString tName, wxString sKeys);

   void FillKeyBindingsList(wxListCtrl * pList);


private:
   MenuBarList mMenuBarList;
   SubMenuList mSubMenuList;
   IdentifierNameList mIdentifierNameList;
   int mCurrentID;
   wxMenu * mCurrentMenu;
};
