/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandManager.h

  Brian Gunlogson
  Dominic Mazzoni

**********************************************************************/

#include <wx/string.h>
#include <wx/dynarray.h>
#include <wx/menu.h>
#include <wx/hashmap.h>

#include "../effects/Effect.h"

class CommandFunctor
{
public:
   virtual void operator()(int index = 0) = 0;
};

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

struct CommandListEntry
{
   int id;
   wxString name;
   wxString key;
   wxString label;
   wxMenu *menu;
   CommandFunctor *callback;
   bool multi;
   int index;
};

WX_DEFINE_ARRAY(MenuBarListEntry *, MenuBarList);
WX_DEFINE_ARRAY(SubMenuListEntry *, SubMenuList);
WX_DEFINE_ARRAY(CommandListEntry *, CommandList);

WX_DECLARE_STRING_HASH_MAP(CommandListEntry *, CommandNameHash);
WX_DECLARE_HASH_MAP(int, CommandListEntry *, wxIntegerHash, wxIntegerEqual, CommandIDHash);

class CommandManager
{
 public:

   //
   // Constructor / Destructor
   //

   CommandManager();
   ~CommandManager();

   void PurgeData();

   //
   // Creating menus and adding commands
   //

   wxMenuBar *AddMenuBar(wxString sMenu);

   void BeginMenu(wxString tName);
   void EndMenu();

   void BeginSubMenu(wxString tName);
   void EndSubMenu();

   void AddItem(wxString name, wxString label, CommandFunctor *callback);
   void AddItemList(wxString name, wxArrayString labels,
                    CommandFunctor *callback, bool plugins = false);

   void AddSeparator();

   // A command doesn't actually appear in a menu but might have a
   // keyboard shortcut.
   void AddCommand(wxString name, wxString label, CommandFunctor *callback);

   //
   // Modifying menus
   //

   void Enable(wxString name, bool enabled);
   void Modify(wxString name, wxString newLabel);

   //
   // Executing commands
   //

   bool HandleMenuID(int id);

   bool HandleKey(wxKeyEvent &evt);

 protected:

   wxMenuBar * CurrentMenuBar();
   wxMenuBar * GetMenuBar(wxString sMenu);
   wxMenu * CurrentSubMenu();
   wxMenu * CurrentMenu();

   int NewIdentifier(wxString name, wxString label, wxMenu *menu,
                     CommandFunctor *callback, bool multi, int index);

   wxString GetKey(wxString label);

   /*
   void AddDynamicItem(wxString sName);
   void AppendEffect(int idEffect, wxString sName, wxString sType);
   void AppendEffects(EffectArray *effs, wxString sType, bool spill);


   wxMenu * GetMenuFromIdentifier(int nID);

   int GetIdentifiersFromFunction(wxString sFunction, bool bReset = false);
   int GetIdentifierFromFunctions(wxString sFunctions);
   wxString GetFunctionsFromIdentifier(int nID);

   wxString GetKeysFromIdentifier(int nID);
   int GetIdentifierFromKey(wxString sKey);

   */

private:
   MenuBarList  mMenuBarList;
   SubMenuList  mSubMenuList;
   CommandList  mCommandList;
   CommandNameHash  mCommandNameHash;
   CommandNameHash  mCommandKeyHash;
   CommandIDHash  mCommandIDHash;
   int mCurrentID;
   wxMenu * mCurrentMenu;
};
