/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dan Horgan

******************************************************************//**

\file CommandTargets
\brief Contains classes for 'command output targets' - objects which can
receive output from a command. For instance, a progress target might pass the
information to a GUI ProgressDialog. Using abstract targets means the command
objects needn't be concerned with what happens to the information.

*//*******************************************************************/

#ifndef __COMMANDTARGETS__
#define __COMMANDTARGETS__

#include <wx/string.h>
#include <wx/msgdlg.h>
#include <wx/statusbr.h>
#include "widgets/ProgressDialog.h"

/// Interface for objects that can receive command progress information
class CommandProgressTarget
{
public:
   virtual ~CommandProgressTarget() {}
   virtual void Update(double completed) = 0;
};

/// Used to ignore a command's progress updates
class NullProgressTarget : public CommandProgressTarget
{
public:
   virtual ~NullProgressTarget() {}
   virtual void Update(double completed) {}
};

/// Sends command progress information to a ProgressDialog
class GUIProgressTarget : public CommandProgressTarget
{
private:
   ProgressDialog &mProgress;
public:
   GUIProgressTarget(ProgressDialog &pd)
      : mProgress(pd)
   {}
   virtual void Update(double completed)
   {
      mProgress.Update(completed);
   }
};

/// Interface for objects that can receive (string) messages from a command
class CommandMessageTarget
{
public:
   virtual ~CommandMessageTarget() {}
   virtual void Update(wxString message) = 0;
};

/// Used to ignore a command's message updates
class NullMessageTarget : public CommandMessageTarget
{
public:
   virtual ~NullMessageTarget() {}
   virtual void Update(wxString message) {}
};

/// Displays messages from a command in a wxMessageBox
class MessageBoxTarget : public CommandMessageTarget
{
public:
   virtual ~MessageBoxTarget() {}
   virtual void Update(wxString message)
   {
      wxMessageBox(message);
   }
};

/// Displays messages from a command in a wxStatusBar
class StatusBarTarget : public CommandMessageTarget
{
private:
   wxStatusBar &mStatus;
public:
   StatusBarTarget(wxStatusBar &sb)
      : mStatus(sb)
   {}
   virtual void Update(wxString message)
   {
      mStatus.SetStatusText(message);
   }
};

// By default, we ignore progress updates but display all other messages directly
class TargetFactory
{
public:
   static CommandProgressTarget *ProgressDefault()
   {
      return CreateProgressTarget<NullProgressTarget>();
   }

   static CommandMessageTarget *MessageDefault()
   {
      return CreateMessageTarget<MessageBoxTarget>();
   }

   template<typename T>
   static CommandProgressTarget *CreateProgressTarget()
   {
      return (new T);
   }

   template<typename T>
   static CommandMessageTarget *CreateMessageTarget()
   {
      return (new T);
   }

};

/// Used to aggregate the various output targets a command may have
// Assumes responsibility for pointers passed into it
class CommandOutputTarget
{
private:
   CommandProgressTarget *mProgressTarget;
   CommandMessageTarget *mStatusTarget;
   CommandMessageTarget *mErrorTarget;
public:
   CommandOutputTarget(CommandProgressTarget *pt = TargetFactory::ProgressDefault(),
                       CommandMessageTarget  *st = TargetFactory::MessageDefault(),
                       CommandMessageTarget  *et = TargetFactory::MessageDefault())
      : mProgressTarget(pt), mStatusTarget(st), mErrorTarget(et)
   { }
   ~CommandOutputTarget()
   {
      delete mProgressTarget;
      delete mStatusTarget;
      delete mErrorTarget;
   }
   void Progress(double completed)
   {
      if (mProgressTarget)
         mProgressTarget->Update(completed);
   }
   void Status(wxString status)
   {
      if (mStatusTarget)
         mStatusTarget->Update(status);
   }
   void Error(wxString message)
   {
      if (mErrorTarget)
         mErrorTarget->Update(message);
   }
};

#endif /* End of include guard: __COMMANDTARGETS__ */

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
