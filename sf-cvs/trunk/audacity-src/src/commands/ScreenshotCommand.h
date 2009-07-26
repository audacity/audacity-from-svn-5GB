/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dominic Mazzoni
   Dan Horgan

**********************************************************************/

#ifndef __SCREENSHOTCOMMAND__
#define __SCREENSHOTCOMMAND__

#include "Command.h"

class wxWindow;
class wxTopLevelWindow;
class wxCommandEvent;
class wxRect;
class ToolManager;

class ScreenshotCommand : public Command
{
private:
   // May need to ignore the screenshot dialog
   wxWindow *mIgnore;

   bool mBackground;
   wxColour mBackColor;

   wxString MakeFileName(wxString path, wxString basename);

   wxRect GetBackgroundRect();
   void Capture(wxString basename,
         wxWindow *window,
         int x, int y, int width, int height,
         bool bg = false);
   void CaptureToolbar(ToolManager *man, int type, wxString name);
   void CaptureDock(wxWindow *win, wxString fileName);

public:
   wxTopLevelWindow *GetFrontWindow(AudacityProject *project);
   ScreenshotCommand(wxString cmdName, 
                     const ParamMap &signature,
                     CommandOutputTarget *output)
      : Command(cmdName, signature, output), 
         mIgnore(NULL), mBackground(false)
   { }
   ScreenshotCommand(CommandOutputTarget *output, wxWindow *ignore = NULL)
      : Command(BuildName(), BuildSignature(), output), mIgnore(ignore)
   { }
   bool Apply(CommandExecutionContext context);
   static ParamMap BuildSignature();
   static wxString BuildName();
};

#endif /* End of include guard: __SCREENSHOTCOMMAND__ */

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
