/**********************************************************************

  Audacity: A Digital Audio Editor

  Commands.cpp

  Brian Gunlogson

  This class creates menus, determines keybindings, and a few other things.

**********************************************************************/

#include <wx/tokenzr.h>
#include <wx/dirdlg.h>
#include <wx/filename.h>

#include "../Project.h"
#include "../Prefs.h"

#include "Commands.h"

#define AUDACITY_COMMANDS_CALLBACK_POINTERS
#include "CommandsCallback.h"
#undef AUDACITY_COMMANDS_CALLBACK_POINTERS

wxString gCommandsCfgLocation;

Commands::Commands()
{
   wxString oldCommandsCfgLocation;
   bool bFalse = false;

   //See if we need to change the location of commands.cfg
   if(gPrefs->Read("/DeleteCmdCfgLocation", &bFalse))
   {
      if(gPrefs->HasEntry("/QDeleteCmdCfgLocation"))
         gPrefs->DeleteEntry("/QDeleteCmdCfgLocation");

      gPrefs->DeleteEntry("/DeleteCmdCfgLocation");

      oldCommandsCfgLocation = gPrefs->Read("/CmdCfgLocation", "");

      if(gPrefs->HasEntry("/CmdCfgLocation"))
         gPrefs->DeleteEntry("/CmdCfgLocation");
   }

   //Figure out where to put commands.cfg
   gCommandsCfgLocation = gPrefs->Read("/CmdCfgLocation", "");

   if(!gCommandsCfgLocation.Length())
   {
      gCommandsCfgLocation = wxDirSelector(_("Choose the location for commands.cfg"), wxGetHomeDir());
      if(!gCommandsCfgLocation.Length())
      {
         gCommandsCfgLocation = wxGetHomeDir();
      }

      wxFileName CommandsCfgFilename("commands.cfg");
      CommandsCfgFilename.PrependDir(gCommandsCfgLocation);

      gCommandsCfgLocation = CommandsCfgFilename.GetFullPath();
      gPrefs->Write("/CmdCfgLocation", gCommandsCfgLocation);
   }

   //Copy old file to new location
   if(oldCommandsCfgLocation.Length())
   {
      ::wxRenameFile(oldCommandsCfgLocation, gCommandsCfgLocation);
   }
}

Commands::~Commands()
{
}

// Begin forewarder functions

wxMenuBar * Commands::GetMenuBar(wxString sMenu)
{
   return mCommandsReader.GetCommandsMenu()->GetMenuBar(sMenu);
}

// End forewarder functions

bool Commands::Initialize()
{
   if(!wxFileExists(gCommandsCfgLocation))
   {
      if(!Reset())
         return false;
   }

   return Parse();
}

bool Commands::Parse()
{
   return mCommandsReader.ParseFile();
}

bool Commands::Reset()
{
   return mCommandsWriter.WriteXML();
}

bool Commands::AssignDefaults()
{
   if(!Reset())
      return false;

   return Reparse();
}

bool Commands::Reparse()
{
   mCommandsReader.GetCommandsMenu()->PurgeData();
   return Parse();
}

bool Commands::ProcessKeyCombo(wxString sKeyCombo)
{
   return ExecuteFunctionsList(mCommandsReader.GetCommandsMenu()->GetFunctionsFromIdentifier(mCommandsReader.GetCommandsMenu()->GetIdentifierFromKey(sKeyCombo)));
}

bool Commands::HandleMenuEvent(wxEvent &event)
{
   AudacityProject *audacityPrj = GetActiveProject();

   if(!audacityPrj)
   {
      return false;
   }

   if(!audacityPrj->IsActive())
   {
      return false;
   }

   int id = event.GetId();
   wxString functionsString = mCommandsReader.GetCommandsMenu()->GetFunctionsFromIdentifier(id);

   //Check if it is an effect (will have the form *@*@Effect (where * is a wildcard))
   if(!functionsString.Right(7).Cmp("@Effect"))
   {
      //it appears to be an effect
      long effectIndex = -1;
      functionsString.Left(functionsString.Find('@')).ToLong(&effectIndex);

      return audacityPrj->ProcessEffectEvent(effectIndex);
   }

   return ExecuteFunctionsList(functionsString);
}

bool Commands::ExecuteFunctionsList(wxString sFunctions)
{
   AudacityProject *audacityPrj = GetActiveProject();

   if(!audacityPrj)
   {
      return false;
   }

   if(!audacityPrj->IsActive())
   {
      return false;
   }

   wxStringTokenizer tFunctions(sFunctions, ":");
   while (tFunctions.HasMoreTokens())
   {
      wxString token = tFunctions.GetNextToken();
      audEventFunction token_callback = LookupCallbackByName(token);
      //good 'ol MSVC++ gave me an internal compiler error, maybe it was something I did?
      void *tokenICE = &token_callback;

      if(!tokenICE)
         return false;

      (audacityPrj->*((audEventFunction) (token_callback))) ();
   }

   return true;
}

audEventFunction Commands::LookupCallbackByName(wxString sCallback)
{
   for(int i = 0; i < NUM_CALLBACK_FUNCTIONS; i++)
   {
      if(!sCallback.Cmp(callback_function_strings[i]))
         return callback_function_pointers[i];
   }

   return NULL;
}

void Commands::ChangeText(wxString sMenuBarName, wxString sFunctions, wxString sNewText)
{
   wxMenuBar *ChangingMenuBar = GetMenuBar(sMenuBarName);
   if(!ChangingMenuBar)
      return;

   int ChangingID = mCommandsReader.GetCommandsMenu()->GetIdentifierFromFunctions(sFunctions);

   wxMenuItem *ChangingMenuItem = ChangingMenuBar->FindItem(ChangingID);
   if(!ChangingMenuItem)
      return;

   wxString finalNewText = mCommandsReader.GetCommandsMenu()->AppendComboString(sNewText, mCommandsReader.GetCommandsMenu()->GetKeysFromIdentifier(ChangingID));

   /**********************************************************
   WARNING: This might cause problems on non-windows platforms
   **********************************************************/
   ChangingMenuItem->SetText(finalNewText);
}

void Commands::EnableItemsByFunction(wxString sMenuBarName, wxString sFunction, bool bEnable)
{
   wxMenuBar *ChangingMenuBar = GetMenuBar(sMenuBarName);
   if(!ChangingMenuBar)
      return;

   int EnableID = mCommandsReader.GetCommandsMenu()->GetIdentifiersFromFunction(sFunction, true);

   while(EnableID != -1)
   {
      mCommandsReader.GetCommandsMenu()->GetMenuFromIdentifier(EnableID)->Enable(EnableID, bEnable);
      EnableID = mCommandsReader.GetCommandsMenu()->GetIdentifiersFromFunction(sFunction);
   }
}
