/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dan Horgan

******************************************************************//**

\file ScriptCommandRelay.h
\brief Contains declarations for ScriptCommandRelay

*//*******************************************************************/

#ifndef __SCRIPTCOMMANDRELAY__
#define __SCRIPTCOMMANDRELAY__

#include "../Audacity.h"
#include <wx/string.h>

class CommandHandler;

extern "C" {

typedef int (*tpExecScriptServerFunc)( wxString * pIn);
typedef int (*tpRegScriptServerFunc)(tpExecScriptServerFunc pFn);
typedef int (*tpScriptServerResponseFunc)( wxString * pOut);

class ScriptCommandRelay
{
   private:
      static CommandHandler *sCmdHandler;
      static tpRegScriptServerFunc sScriptFn;
      static tpScriptServerResponseFunc sScriptOutFn;

   public:

      static void SetRegScriptServerFunc(tpRegScriptServerFunc scriptFn);
      static void SetScriptServerResponseFunc(tpScriptServerResponseFunc scriptOutFn);
      static void SetCommandHandler(CommandHandler &ch);

      static void Run();

      AUDACITY_DLL_API static int ExecCommand( wxString * pIn );

      static void SendResponse(wxString &pOut);
};
} // End 'extern C'
#endif /* End of include guard: __SCRIPTCOMMANDRELAY__ */

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: TBD
