/**********************************************************************

  Audacity: A Digital Audio Editor

  
  ToolsToolbar.h
 
  Dominic Mazzoni
  Shane T. Mueller

**********************************************************************/

#ifndef __AUDACITY_TOOLS_TOOLBAR__
#define __AUDACITY_TOOLS_TOOLBAR__

#include "ToolBar.h"

class AButton;
class ToolsToolBar;
class ToolBar;
class AudacityProject;

class wxImage;
class wxSize;
class wxPoint;

// Code duplication warning: these apparently need to be in the
// same order as the enum in ToolsToolBar.cpp

enum {
   selectTool,
   envelopeTool,
   drawTool,
   zoomTool,
   slideTool,
   multiTool,
   numTools,

   firstTool = selectTool,
   lastTool = multiTool
};

class ToolsToolBar:public ToolBar {
public:
   ToolsToolBar() {};
   ToolsToolBar(wxWindow * parent);
   virtual ~ToolsToolBar();

   void UpdatePrefs();

   void OnTool(wxCommandEvent & evt);

   void SetCurrentTool(int tool, bool show);

   //These interrogate the state of the buttons or controls.
   int GetCurrentTool();
   bool GetSelectToolDown();
   bool GetZoomToolDown();
   bool GetEnvelopeToolDown();
   bool GetSlideToolDown();
   bool GetDrawToolDown();
   bool GetMultiToolDown();

   const wxChar * GetMessageForTool( int ToolNumber );

   void Populate();
   virtual void Repaint( wxDC *dc )  {};
   virtual void EnableDisableButtons() {};

   DECLARE_EVENT_TABLE()
   ;
private:
   void RegenerateToolsTooltips();
   wxImage *MakeToolImage(wxImage * tool, wxImage * mask, int style);
   AButton *MakeTool( teBmps eTool, int id, const wxChar *label);

   AButton *mTool[numTools];
   wxGridSizer *mToolSizer;
   int mCurrentTool;
};

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: bb2858b8-2c70-48df-9d72-bcdef94be4e3

