/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandManager.cpp

  Brian Gunlogson
  Dominic Mazzoni

*******************************************************************//**

\class CommandManager
\brief CommandManager implements a system for organizing all user-callable
  commands.  
  
  It creates and manages a menu bar with a command
  associated with each item, and managing other commands callable
  by keyboard shortcuts.

  Commands are implemented by overriding an abstract functor class.
  See Menus.cpp for an example use.

  Menus or submenus containing lists of items can be added at once,
  with a single function (functor) to be called when any of the
  items is selected, with the index number of the selection as the
  parameter.  This is useful for dynamic menus (effects) and
  submenus containing a list of choices (selection formats).

  Menu items can be enabled or disabled individually, groups of
  "multi-items" can be enabled or disabled all at once, or entire
  sets of commands can be enabled or disabled all at once using
  flags.  The flags should be a bitfield stored in a 32-bit
  integer but can be whatever you want.  You specify both the
  desired values of the flags, and the set of flags relevant to
  a particular command, by using a combination of a flags parameter
  and a mask parameter.  Any flag set to 0 in the mask parameter is
  the same as "don't care".  Any command whose mask is set to zero
  will not be affected by enabling/disabling by flags.

*//****************************************************************//**

\class CommandFunctor
\brief CommandFunctor is a very small class that works with 
CommandManager.  It holds the callback for one command.

*//****************************************************************//**

\class MenuBarListEntry
\brief MenuBarListEntry is a structure used by CommandManager.

*//****************************************************************//**

\class SubMenuListEntry
\brief SubMenuListEntry is a structure used by CommandManager.

*//****************************************************************//**

\class CommandListEntry
\brief CommandListEntry is a structure used by CommandManager.

*//****************************************************************//**

\class MenuBarList
\brief List of MenuBarListEntry.

*//****************************************************************//**

\class SubMenuList
\brief List of SubMenuListEntry.

*//****************************************************************//**

\class CommandList
\brief List of CommandListEntry.

*//******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/hash.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/log.h>

#include "../Prefs.h"
#include "../Project.h"

#include "CommandManager.h"

#include "Keyboard.h"
#include "../effects/Effect.h"

// On wxGTK, there may be many many many plugins, but the menus don't automatically
// allow for scrolling, so we build sub-menus.  If the menu gets longer than
// MAX_MENU_LEN, we put things in submenus that have MAX_SUBMENU_LEN items in them.
// 
#ifdef __WXGTK__
#define MAX_MENU_LEN 20
#define MAX_SUBMENU_LEN 15
#else
#define MAX_MENU_LEN 1000
#define MAX_SUBMENU_LEN 1000
#endif

///
///  Standard Constructor
///
CommandManager::CommandManager():
   mCurrentID(0),
   mHiddenID(0),
   mCurrentMenu(NULL),
   mOpenMenu(NULL),
   mDefaultFlags(0),
   mDefaultMask(0)
{
}

///
///  Class Destructor.  Includes PurgeData, which removes
///  menubars
CommandManager::~CommandManager()
{
   //WARNING: This removes menubars that could still be assigned to windows!
   PurgeData();
}

void CommandManager::PurgeData()
{
   // Delete callback functors BEFORE clearing mCommandList!
   // These are the items created within 'FN()'
   size_t i;
   CommandFunctor * pCallback = NULL;
   for(i=0; i<mCommandList.GetCount(); i++)
   {
      CommandListEntry *tmpEntry = mCommandList[i];
      // JKC: We only want to delete each callbacks once.
      // AddItemList() may have inserted the same callback 
      // several times over.
      if( tmpEntry->callback != pCallback )
      {
         pCallback = tmpEntry->callback;
         delete pCallback;
      }
   }

   // mCommandList contains pointers to CommandListEntrys
   // mMenuBarList contains pointers to MenuBarListEntrys.
   // mSubMenuList contains pointers to SubMenuListEntrys
   WX_CLEAR_ARRAY( mCommandList );
   WX_CLEAR_ARRAY( mMenuBarList );
   WX_CLEAR_ARRAY( mSubMenuList );

   mCommandNameHash.clear();
   mCommandKeyHash.clear();
   mCommandIDHash.clear();

   mCurrentMenu = NULL;
   mCurrentID = 0;
}


///
/// Makes a new menubar for placement on the top of a project
/// Names it according to the passed-in string argument.
///
/// If the menubar already exists, simply returns it.
wxMenuBar *CommandManager::AddMenuBar(wxString sMenu)
{
   wxMenuBar *menuBar = GetMenuBar(sMenu);
   if (menuBar)
      return menuBar;

   MenuBarListEntry *tmpEntry = new MenuBarListEntry;

   tmpEntry->menubar = new wxMenuBar();
   tmpEntry->name = sMenu;

   mMenuBarList.Add(tmpEntry);

   return tmpEntry->menubar;
}


///
/// Retrieves the menubar based on the name given in AddMenuBar(name) 
///
wxMenuBar * CommandManager::GetMenuBar(wxString sMenu)
{
   for(unsigned int i = 0; i < mMenuBarList.GetCount(); i++)
   {
      if(!mMenuBarList[i]->name.Cmp(sMenu))
         return mMenuBarList[i]->menubar;
   }

   return NULL;
}


///
/// Retrieve the 'current' menubar; either NULL or the
/// last on in the mMenuBarList.
wxMenuBar * CommandManager::CurrentMenuBar()
{
   if(mMenuBarList.IsEmpty())
      return NULL;

   return mMenuBarList[mMenuBarList.GetCount()-1]->menubar;
}


///
/// This makes a new menu and adds it to the 'CurrentMenuBar'
///
/// If the menu already exists, all of the items in it are
/// cleared instead.
///
void CommandManager::BeginMenu(wxString tName)
{
   

   wxMenu *tmpMenu = new wxMenu();

   mCurrentMenu = tmpMenu;

   CurrentMenuBar()->Append(mCurrentMenu, tName);
}


///
/// This ends a menu by setting the pointer to it
/// to NULL.  It is still attached to the CurrentMenuBar()
void CommandManager::EndMenu()
{
   mCurrentMenu = NULL;
}



///
/// This starts a new submenu, and names it according to
/// the function's argument.
wxMenu* CommandManager::BeginSubMenu(wxString tName)
{
   SubMenuListEntry *tmpEntry = new SubMenuListEntry;

   tmpEntry->menu = new wxMenu();
   tmpEntry->name = tName;

   mSubMenuList.Add(tmpEntry);
   
   return(tmpEntry->menu);
}


///
/// This function is called after the final item of a SUBmenu is added.
/// Submenu items are added just like regular menu items; they just happen
/// after BeginSubMenu() is called but before EndSubMenu() is called.
void CommandManager::EndSubMenu()
{
   size_t submenu_count = mSubMenuList.GetCount()-1;

   //Save the submenu's information
   SubMenuListEntry *tmpSubMenu = mSubMenuList[submenu_count];

   //Pop off the new submenu so CurrentMenu returns the parent of the submenu
   mSubMenuList.RemoveAt(submenu_count);

   //Add the submenu to the current menu
   CurrentMenu()->Append(0, tmpSubMenu->name, tmpSubMenu->menu, tmpSubMenu->name);

   delete tmpSubMenu;
}


///
/// This returns the 'Current' Submenu, which is the one at the
///  end of the mSubMenuList (or NULL, if it doesn't exist).
wxMenu * CommandManager::CurrentSubMenu()
{
   if(mSubMenuList.IsEmpty())
      return NULL;

   return mSubMenuList[mSubMenuList.GetCount()-1]->menu;
}

///
/// This returns the current menu that we're appending to - note that
/// it could be a submenu if BeginSubMenu was called and we haven't
/// reached EndSubMenu yet.
wxMenu * CommandManager::CurrentMenu()
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

///
/// Add a menu item to the current menu.  When the user selects it, the
/// given functor will be called
void CommandManager::AddItem(wxString name, wxString label,
                             CommandFunctor *callback)
{
   int ID = NewIdentifier(name, label, CurrentMenu(), callback, false, 0, 0);

   // Replace the accel key with the one from the preferences
   label = label.BeforeFirst(wxT('\t'));

   // This is a very weird hack.  Under GTK, menu labels are totally
   // linked to accelerators the first time you create a menu item
   // with that label and can't be changed.  This causes all sorts of
   // problems.  As a workaround, we create each menu item with a
   // made-up name (just an ID number string) but with the accelerator
   // we want, then immediately change the label to the correct string.
   // -DMM
   mHiddenID++;
   wxString dummy, newLabel;
   dummy.Printf(wxT("%s%08d"), label.c_str(), mHiddenID);
   newLabel = label;

   bool shortcut = false;

   if (mCommandIDHash[ID]->key.Length() > 0)
      shortcut = true;
   
   // Mac OS X fixes
  #ifdef __WXMAC__
   if (newLabel.Length() > 0 && newLabel[0] == wxT('&'))
      newLabel = newLabel.Right(newLabel.Length()-1);

   if (shortcut == true &&
       (mCommandIDHash[ID]->key.Length() < 5 ||
        mCommandIDHash[ID]->key.Left(5) != wxT("Ctrl+")))
      shortcut = false;
  #endif
   
   if (shortcut) {
      dummy = dummy + wxT("\t") + mCommandIDHash[ID]->key;
   }

   CurrentMenu()->Append(ID, dummy);
   CurrentMenu()->SetLabel(ID, newLabel);
}

///
/// Add a list of menu items to the current menu.  When the user selects any
/// one of these, the given functor will be called
/// with its position in the list as the index number.
/// When you call Enable on this command name, it will enable or disable
/// all of the items at once.
void CommandManager::AddItemList(wxString name, wxArrayString labels,
                                 CommandFunctor *callback,
                                 bool plugins /*= false*/)
{
   unsigned int i;

   #ifndef __WXGTK__
   plugins = false;
   #endif

   if (CurrentMenu()->GetMenuItemCount() + labels.GetCount() < MAX_MENU_LEN)
      plugins = false;

   if (!plugins) {
      for(i=0; i<labels.GetCount(); i++) {
         int ID = NewIdentifier(name, labels[i], CurrentMenu(), callback,
                                true, i, labels.GetCount());
         CurrentMenu()->Append(ID, labels[i]);
      }
      return;
   }

   wxString label;
   unsigned int effLen = labels.GetCount();
   int listnum = 1;
   int tmpmax = MAX_SUBMENU_LEN  < effLen? MAX_SUBMENU_LEN: effLen;

   //The first submenu starts at 1.
   BeginSubMenu(wxString::Format(_("Plugins 1 to %i"), tmpmax));

   for(i=0; i<effLen; i++) {
      int ID = NewIdentifier(name, labels[i], CurrentMenu(), callback,
                             true, i, effLen);

      CurrentMenu()->Append(ID, labels[i]);
     
      if(((i+1) % MAX_SUBMENU_LEN) == 0 && i != (effLen - 1)) {
         EndSubMenu();
         listnum++;
         
         //This label the plugins by number in the submenu title (1 to 15, 15 to 30, etc.)
         tmpmax = i + MAX_SUBMENU_LEN  < effLen? 1 + i + MAX_SUBMENU_LEN: effLen;
         BeginSubMenu(wxString::Format(_("Plugins %i to %i"),i+2,tmpmax));
      }
   }
   EndSubMenu();
}

///
/// Add a command that doesn't appear in a menu.  When the key is pressed, the
/// given function pointer will be called (via the CommandManagerListener)
void CommandManager::AddCommand(wxString name, wxString label,
                                CommandFunctor *callback)
{
   NewIdentifier(name, label, NULL, callback, false, 0, 0);
}

void CommandManager::AddSeparator()
{
   CurrentMenu()->AppendSeparator();
}

int CommandManager::NextIdentifier(int ID)
{
   ID++;

   //Skip the reserved identifiers used by wxWindows
   if((ID >= wxID_LOWEST) && (ID <= wxID_HIGHEST))
      ID = wxID_HIGHEST+1;

   return ID;
}

///Given all of the information for a command, comes up with a new unique
///ID, adds it to a list, and returns the ID.
///WARNING: Does this conflict with the identifiers set for controls/windows?
///If it does, a workaround may be to keep controls below wxID_LOWEST
///and keep menus above wxID_HIGHEST
int CommandManager::NewIdentifier(wxString name, wxString label, wxMenu *menu,
                                  CommandFunctor *callback,
                                  bool multi, int index, int count)
{
   CommandListEntry *tmpEntry = new CommandListEntry;
   
   // wxMac 2.5 and higher will do special things with the
   // Preferences, Exit (Quit), and About menu items,
   // if we give them the right IDs.
   // Otherwise we just pick increasing ID numbers for each new
   // command.  Note that the name string we are comparing
   // ("About", "Preferences") is the internal command name
   // (untranslated), not the label that actually appears in the
   // menu (which might be translated).

   mCurrentID = NextIdentifier(mCurrentID);
   tmpEntry->id = mCurrentID;

  #ifdef __WXMAC__
   if (name == wxT("Preferences"))
      tmpEntry->id = wxID_PREFERENCES;
   else if (name == wxT("Exit"))
      tmpEntry->id = wxID_EXIT;
   else if (name == wxT("About"))
      tmpEntry->id = wxID_ABOUT;
  #endif
  
   tmpEntry->name = name;
   tmpEntry->label = label;
   tmpEntry->menu = menu;
   tmpEntry->callback = callback;
   tmpEntry->multi = multi;
   tmpEntry->index = index;
   tmpEntry->count = count;
   tmpEntry->key = GetKey(label);
   tmpEntry->defaultKey = GetKey(label);
   tmpEntry->flags = mDefaultFlags;
   tmpEntry->mask = mDefaultMask;
   tmpEntry->enabled = true;

   // Key from preferences overridse the default key given
   gPrefs->SetPath(wxT("/NewKeys"));
   if (gPrefs->HasEntry(name)) {
      tmpEntry->key = gPrefs->Read(name, GetKey(label));
   }
   gPrefs->SetPath(wxT("/"));
   
   mCommandList.Add(tmpEntry);
   mCommandIDHash[tmpEntry->id] = tmpEntry;   

   if (index==0 || !multi)
      mCommandNameHash[name] = tmpEntry;

   if (tmpEntry->key != wxT(""))
      mCommandKeyHash[tmpEntry->key] = tmpEntry;

   return tmpEntry->id;
}

wxString CommandManager::GetKey(wxString label)
{
   int loc = -1;

   loc = label.Find(wxT('\t'));
   if (loc == -1)
      loc = label.Find(wxT("\\t"));

   if (loc == -1)
      return wxT("");

   return label.Right(label.Length() - (loc+1));
}

///Enables or disables a menu item based on its name (not the
///label in the menu bar, but the name of the command.)
///If you give it the name of a multi-item (one that was
///added using AddItemList(), it will enable or disable all
///of them at once
void CommandManager::Enable(CommandListEntry *entry, bool enabled)
{
   // Don't do anything if the command's enabled state
   // is already the same
   if (entry->enabled == enabled)
      return;

   entry->enabled = enabled;

   if (!entry->menu)
      return;
      
   entry->menu->Enable(entry->id, enabled);
   if (entry->multi) {
      int i;
      int ID = entry->id;
      
      for(i=1; i<entry->count; i++) {
         ID = NextIdentifier(ID);
         wxMenuItem *item = entry->menu->FindItem(ID);
         if (item) {
            item->Enable(enabled);
         } else {
            wxLogDebug(wxT("Warning: Menu entry with id %i in %s not found"),
                ID, (const wxChar*)entry->name);
         }
      }
   }
}

void CommandManager::Enable(wxString name, bool enabled)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry || !entry->menu) {
      //printf("WARNING: Unknown command enabled: '%s'\n", (const char *)name.mb_str());
      return;
   }

   Enable(entry, enabled);
}

void CommandManager::EnableUsingFlags(wxUint32 flags, wxUint32 mask)
{
   unsigned int i;

   for(i=0; i<mCommandList.GetCount(); i++) {
      CommandListEntry *entry = mCommandList[i];
      if (entry->multi && entry->index != 0)
         continue;

      wxUint32 combinedMask = (mask & entry->mask);
      if (combinedMask) {
         bool enable = ((flags & combinedMask) ==
                        (entry->flags & combinedMask));
         Enable(entry, enable);
      }
   }
}

///Changes the label text of a menu item
void CommandManager::Modify(wxString name, wxString newLabel)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (entry && entry->menu) {
      newLabel = newLabel.BeforeFirst(wxT('\t'));
      if (!entry->key.IsEmpty())
         newLabel = newLabel + wxT("\t") + entry->key;
      entry->label = newLabel;
      entry->menu->SetLabel(entry->id, newLabel);
   }
}

void CommandManager::HandleMenuOpen(wxMenuEvent &evt)
{
   // Ensure we have a menu and that it's a top-level menu.
   wxMenu *m = evt.GetMenu();
   if (!m || m->GetParent())
      return;

   // Windows does not send a CLOSE event if you move from one 
   // top-level menu to another, so simulate it.
   if (mOpenMenu) {
      wxMenuEvent dummy;
      HandleMenuClose(dummy);
   }

   // Remember this menu
   mOpenMenu = m;

   // Turn on the accelerators
   ToggleAccels(m, true);

   return;
}

void CommandManager::HandleMenuClose(wxMenuEvent &evt)
{
   // This can happen when if the Windows system menu is used   
   if (mOpenMenu == NULL)
      return;

   // GetMenu() under Windows will always return NULL.  And on other
   // platforms we must ensure we are a top-level menu.
   wxMenu *m = evt.GetMenu();
   if (m && m->GetParent())
      return;

   // Turn off the accelerators
   ToggleAccels(mOpenMenu, false);

   // Forget about it
   mOpenMenu = NULL;

   return;
}
 
void CommandManager::ToggleAccels(wxMenu *m, bool show)
{
   // Add the top-level menu to the stack;
   wxArrayPtrVoid stack;
   stack.Add(m);

   // Process all sub-menus in this tree
   while (!stack.IsEmpty()) {

      // Pop the bottom entry
      m = (wxMenu *) stack.Item(0);
      stack.RemoveAt(0);

      // Retrieve menuitem info for this menu
      wxMenuItemList mil = m->GetMenuItems();
      int iCnt = m->GetMenuItemCount();
      int iNdx;

      // Iterate all menuitems at this level
      for (iNdx = 0; iNdx < iCnt; iNdx++) {

         // Retrieve the menuitem
         wxMenuItem *mi = mil.Item(iNdx)->GetData();
         if (!mi)
            continue;

         // Stack the menu if this item represents a submenu
         if (mi->IsSubMenu()) {
            stack.Add(mi->GetSubMenu());
            continue;
         }

         // Retrieve the command entry for this item
         CommandListEntry *entry = mCommandIDHash[mi->GetId()];
         if (!entry)
            continue;

         // Rebuild the label based on whether the accelerator should
         // be shown.
         wxString label = entry->label.BeforeFirst(wxT('\t'));
         if (show && !entry->key.IsEmpty()) {
            label = label + wxT("\t") + entry->key;
         }

         // Set the new label
         mi->SetText( label );
      }
   }

   return;
}
 
/// HandleCommandEntry() takes a CommandListEntry and executes it
/// returning true iff successful.  If you pass any flags,
///the command won't be executed unless the flags are compatible
///with the command's flags.
bool CommandManager::HandleCommandEntry(CommandListEntry * entry, wxUint32 flags, wxUint32 mask)
{
   if (!entry)
      return false;

   wxUint32 combinedMask = (mask & entry->mask);
   if (combinedMask) {
      bool allowed = ((flags & combinedMask) ==
                      (entry->flags & combinedMask));
      if (!allowed)
         return false;
   }

   (*(entry->callback))(entry->index);

   return true;
}

///Call this when a menu event is received.
///If it matches a command, it will call the appropriate
///CommandManagerListener function.  If you pass any flags,
///the command won't be executed unless the flags are compatible
///with the command's flags.
bool CommandManager::HandleMenuID(int id, wxUint32 flags, wxUint32 mask)
{
   CommandListEntry *entry = mCommandIDHash[id];
   return HandleCommandEntry( entry, flags, mask );
}

///Call this when a key event is received.
///If it matches a command, it will call the appropriate
///CommandManagerListener function.  If you pass any flags,
///the command won't be executed unless the flags are compatible
///with the command's flags.
bool CommandManager::HandleKey(wxKeyEvent &evt, wxUint32 flags, wxUint32 mask)
{
   wxString keyStr = KeyEventToKeyString(evt);
	CommandListEntry *entry = mCommandKeyHash[keyStr];
   return HandleCommandEntry( entry, flags, mask );
}

/// HandleTextualCommand() allows us a limitted version of script/batch
/// behavior, since we can get from a string command name to the actual
/// code to run.
bool CommandManager::HandleTextualCommand(wxString & Str, wxUint32 flags, wxUint32 mask)
{
   unsigned int i;

   // Linear search for now...
   for(i=0; i<mCommandList.GetCount(); i++) {
      if (!mCommandList[i]->multi)
      {
         if( Str.IsSameAs( mCommandList[i]->name ))
         {
            return HandleCommandEntry( mCommandList[i], flags, mask);
         }
      }
   }
   // Not one of the singleton commands.
   // We could/should try all the list-style commands.
   // instead we only try the effects.
   EffectArray *effects;
   AudacityProject * proj;
   proj = GetActiveProject();
   if( !proj )
      return false;

   int effectFlags = ALL_EFFECTS | CONFIGURED_EFFECT;
   effects = Effect::GetEffects(effectFlags);
   for(i=0; i<effects->GetCount(); i++) {
      wxString effectName = (*effects)[i]->GetEffectName();
      if( Str.IsSameAs( effectName ))
      {
         return proj->OnEffect( effectFlags, (*effects)[i] ); 
      }
   }
   return false;
}

void CommandManager::GetAllCommandNames(wxArrayString &names,
                                        bool includeMultis)
{
   unsigned int i;

   for(i=0; i<mCommandList.GetCount(); i++) {
      if (includeMultis || !mCommandList[i]->multi)
         names.Add(mCommandList[i]->name);
   }
}

wxString CommandManager::GetLabelFromName(wxString name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry)
      return wxT("");

   return entry->label;
}

wxString CommandManager::GetKeyFromName(wxString name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry)
      return wxT("");
 
   return entry->key;
}

wxString CommandManager::GetDefaultKeyFromName(wxString name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry)
      return wxT("");
 
   return entry->defaultKey;
}

bool CommandManager::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (!wxStrcmp(tag, wxT("audacitykeyboard"))) {
      mXMLKeysRead = 0;
   }

   if (!wxStrcmp(tag, wxT("command"))) {
      wxString name;
      wxString key;

      while(*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;
         
         if (!value)
            break;
         
         if (!wxStrcmp(attr, wxT("name")))
            name = value;
         if (!wxStrcmp(attr, wxT("key")))
            key = value;
      }

      if (mCommandNameHash[name]) {
         mCommandNameHash[name]->key = key;
         mXMLKeysRead++;
      }
   }

   return true;
}

void CommandManager::HandleXMLEndTag(const wxChar *tag)
{
   if (!wxStrcmp(tag, wxT("audacitykeyboard"))) {
      wxMessageBox(wxString::Format(_("Loaded %d keyboard shortcuts\n"),
                                    mXMLKeysRead),
                   _("Loading keyboard shortcuts"),
                   wxOK | wxCENTRE);
   }
}

XMLTagHandler *CommandManager::HandleXMLChild(const wxChar *tag)
{
   return this;
}

void CommandManager::WriteXML(XMLWriter &xmlFile)
{
   int i;
   unsigned int j;

   xmlFile.StartTag(wxT("audacitykeyboard"));
   xmlFile.WriteAttr(wxT("audacityversion"), wxT(AUDACITY_VERSION_STRING));

   for(j=0; j<mCommandList.GetCount(); j++) {
      if (!mCommandList[j]->multi) {
         
         wxString label = mCommandList[j]->label;
         label = wxMenuItem::GetLabelFromText(label.BeforeFirst(wxT('\t')));
         
         xmlFile.StartTag(wxT("command"));
         xmlFile.WriteAttr(wxT("name"), mCommandList[j]->name);
         xmlFile.WriteAttr(wxT("label"), label);
         xmlFile.WriteAttr(wxT("key"), mCommandList[j]->key);
         xmlFile.EndTag(wxT("command"));
      }
   }
   
   xmlFile.EndTag(wxT("audacitykeyboard"));
}

void CommandManager::SetDefaultFlags(wxUint32 flags, wxUint32 mask)
{
   mDefaultFlags = flags;
   mDefaultMask = mask;
}

void CommandManager::SetCommandFlags(wxString name,
                                     wxUint32 flags, wxUint32 mask)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (entry) {
      entry->flags = flags;
      entry->mask = mask;
   }
}

void CommandManager::SetCommandFlags(const wxChar **names,
                                     wxUint32 flags, wxUint32 mask)
{
   const wxChar **nptr = names;
   while(*nptr) {
      SetCommandFlags(wxString(*nptr), flags, mask);
      nptr++;
   }
}

void CommandManager::SetCommandFlags(wxUint32 flags, wxUint32 mask, ...)
{
   va_list list;
   va_start(list, mask); 
   for(;;) {
      const wxChar *name = va_arg(list, const wxChar *);
      if (!name)
         break;
      SetCommandFlags(wxString(name), flags, mask);
   }
   va_end(list);
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
// arch-tag: 7202d707-9bf3-4735-bdf4-a45b7e004d9a

