/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolBar.h

  Dominic Mazzoni
  Shane T. Mueller

  This file contains three classes: ToolBarStub, ToolBarFrame, and ToolBar.


  For each new toolbar, a global instance of ToolBarStub should be created
  in AudacityApp.cpp.  ToolBarStub contains some information about the ToolBar,
  and contains ToolBarFrame, which is the floating (hidden or visable) toolbar.
  ToolBar is a base class that is used to base new toolbars on (cf ControlToolBar.h) 
  To create a new toolbar, a new class that inherits this class should be created,
  the class should be given an ID in enum ToolBarType, and accessor functions in
  Menus.h and Menus.cpp should be written. Type-specific
  switch statements in ToolBarFrame::ToolBarFrame, AudacityProject::OnMouseEvent(),
  AudacityProject::LoadToolBar() and AudacityProject::UnloadToolBar() should
  be made to handle the new toolbar type.

  The toolbars in each window are not tied directly to a ToolBarStub, although 
  there are some methods in ToolBarStub that will load and unload toolbars from
  all project windows.

  If you want to determine what type of child toolbar something is, you must 
  use the WX_CREATE_DYNAMIC_CLASS() and WX_INSTANTIATE_DYNAMIC_CLASS() macros.
  Then, you can use wxDynamicCast(parent*, child_class_type) to cast a base class
  instantiation into the child class (if it really is a child).



**********************************************************************/

#ifndef __AUDACITY_TOOLBAR__
#define __AUDACITY_TOOLBAR__

#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/minifram.h>
#include <wx/object.h>
#include "widgets/AButton.h"

class ToolBarStub;
class ToolBarFrame;

class wxImage;
class wxSize;
class wxPoint;
class AudacityProject;

enum ToolBarType {
   NoneID,
   ControlToolBarID,
   EditToolBarID,
   DummyToolBarID
};

////////////////////////////////////////////////////////////
/// class ToolBar
////////////////////////////////////////////////////////////

class ToolBar:public wxWindow {

 public:
   ToolBar() {
   };
   ToolBar(wxWindow * parent);
   ToolBar(wxWindow * parent, wxWindowID id,
           const wxPoint & pos, const wxSize & size);

   virtual ~ ToolBar();
   virtual int GetHeight() {
      return GetSize().y;
   };
   virtual wxString GetTitle() {
      return mTitle;
   };
   enum ToolBarType GetType() {
      return mType;
   };
   virtual void OnKeyEvent(wxKeyEvent & event);
   virtual void OnPaint(wxPaintEvent & event);
   wxSize GetIdealSize() {
      return mIdealSize;
   };
 protected:

   virtual AButton * MakeButton(wxImage * up,
                                wxImage * down,
                                wxImage * hilite,
                                const char **foreground,
                                const char **disabledfg,
                                const char **alpha,
                                wxWindowID id,
                                wxPoint placement, 
                                wxSize size,
                                int xoff, int yoff);

   wxImage *OverlayImage(wxImage * background,
                         wxImage * foreground,
                         wxImage * mask, int xoff, int yoff);
 
   wxImage *ChangeImageColour(wxImage * srcImage, wxColour & dstColour);
   wxImage *ChangeImageColour(wxImage * srcImage,
                              wxColour & srcColour,
                              wxColour & dstColour);

   void SetButton(bool down, AButton* button);

   wxBrush mBackgroundBrush;
   wxPen mBackgroundPen;
   enum ToolBarType mType;
   wxString mTitle;

   //These keep track of how big the tool bar "should" be.
   wxSize mIdealSize;
};



////////////////////////////////////////////////////////////
/// class ToolBarStub
////////////////////////////////////////////////////////////

class ToolBarStub {

 public:
   ToolBarStub(wxWindow * Parent, enum ToolBarType tbt);
   ~ToolBarStub();

   //These methods load/unload/float/unfloat the children
   //toolbars from EVERY window.
   void LoadAll();
   void UnloadAll();
   void ShowWindowedToolBar(wxPoint * where = NULL);
   void HideWindowedToolBar();
   bool IsToolBarLoaded(AudacityProject *);
   bool GetWindowedStatus() {return mWindowedStatus; };
   bool GetLoadedStatus() { return mLoadedStatus;  };
   void SetLoadedStatus(bool status) { mLoadedStatus = status; };
   void SetWindowedStatus(bool status){ mWindowedStatus = status;};
   enum ToolBarType GetType() { return mType;  };
   int GetHeight() {return mSize.y;};
   ToolBar *GetToolBar();

 protected:
   ToolBarFrame * mToolBarFrame;
   enum ToolBarType mType;
   wxString mTitle;
   wxSize mSize;                //Intended size of toolbar
   bool mWindowedStatus;        //Whether the toolbar is floating or embedded
   bool mLoadedStatus;          //Whether the toolbar is loaded (visible) or not

};


////////////////////////////////////////////////////////////
/// class ToolBarFrame
////////////////////////////////////////////////////////////


class ToolBarFrame:public wxMiniFrame {
 public:

   ToolBarFrame(wxWindow * parent, ToolBar * tb, const wxString & title,
                const wxPoint & pos);
    ToolBarFrame(wxWindow * parent, ToolBar * TB);
    ToolBarFrame(wxWindow * parent, enum ToolBarType tbt);
    virtual ~ ToolBarFrame();

   void OnCloseWindow(wxCloseEvent & event);
   ToolBar *GetToolBar() {
      return mToolBar;
   };

 protected:
   ToolBar * mToolBar;
 public:
   DECLARE_EVENT_TABLE()
};



#endif
