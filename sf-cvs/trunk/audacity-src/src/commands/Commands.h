/**********************************************************************

  Audacity: A Digital Audio Editor

  Commands.h

  Brian Gunlogson

  This class creates menus, determines keybindings, and a few other things.

**********************************************************************/

#ifndef __AUDACITY_COMMANDS__
#define __AUDACITY_COMMANDS__

class AudacityProject;

#include "CommandsWriter.h"
#include "CommandsReader.h"

typedef void (AudacityProject::*audEventFunction)();

class Commands
{
public:
   Commands();
   ~Commands();

   //forewarder functions
   wxMenuBar * GetMenuBar(wxString sMenu);

   bool Initialize();
   bool Parse();
   bool Reset();
   bool AssignDefaults();
   bool Reparse();

   bool ProcessKeyCombo(wxString sKeyCombo);
   bool HandleMenuEvent(wxEvent &event);

   void ChangeText(wxString sMenuBarName, wxString sFunctions, wxString sNewText);
   void EnableItemsByFunction(wxString sMenuBarName, wxString sFunction, bool bEnable);

private:
   bool ExecuteFunctionsList(wxString sFunctions);
   audEventFunction LookupCallbackByName(wxString sCallback);

   CommandsWriter mCommandsWriter;
   CommandsReader mCommandsReader;
};

#endif
