/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolBar.h

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_TOOLBAR__
#define __AUDACITY_TOOLBAR__

#include <wx/defs.h>
#include <wx/panel.h>
#include <wx/sizer.h>

#include <../Theme.h>

class wxDC;
class wxEraseEvent;
class wxMouseEvent;
class wxObject;
class wxPaintEvent;
class wxPoint;
class wxSize;
class wxSizeEvent;
class wxString;
class wxWindow;

class AButton;
class Grabber;
class ToolDock;

////////////////////////////////////////////////////////////
/// class ToolBar
////////////////////////////////////////////////////////////

//
// Custom event
//
DECLARE_EVENT_TYPE(EVT_TOOLBAR_UPDATED, -1);

//
// Height of a single line toolbar
//
#define toolbarSingle 27

//
// Size of border around toolbars
//
#define toolbarGap 1

//
// ToolBar IDs
//
enum
{
   NoBarID = -1,
   ControlBarID,
   ToolsBarID,
   MeterBarID,
   MixerBarID,
   EditBarID,
   TranscriptionBarID,
   SelectionBarID,
   DeviceBarID,
   ToolBarCount
};
  
class ToolBar:public wxPanel
{

 public:

   ToolBar(int type, const wxString & label, bool resizable = false);
   virtual ~ToolBar();

   virtual void Create(wxWindow *parent);
   virtual void EnableDisableButtons() = 0;
   virtual void ReCreateButtons();

   void SetDocked(bool dock, bool pushed);

   int GetType();
   wxString GetTitle();
   wxString GetLabel();
   ToolDock *GetDock();

   bool Expose( bool show = true );

   bool IsResizable();
   bool IsVisible();
   bool IsDocked();

 protected:

   AButton *MakeButton(teBmps eUp,
                       teBmps eDown,
                       teBmps eHilite,
                       teBmps eStandard,
                       teBmps eDisabled,
                       wxWindowID id,
                       wxPoint placement,
                       bool processdownevents, 
                       wxSize size);

   void SetButton(bool down, AButton *button);

   void MakeMacRecoloredImage(teBmps eBmpOut, teBmps eBmpIn);
   void MakeRecoloredImage(teBmps eBmpOut, teBmps eBmpIn);
   void MakeButtonBackgroundsLarge();
   void MakeButtonBackgroundsSmall();

   wxBoxSizer *GetSizer();

   void Add(wxWindow *window,
            int proportion = 0,
            int flag = wxALIGN_TOP,
            int border = 0,
            wxObject *userData = NULL);

   void Add(wxSizer *sizer,
             int proportion = 0,
             int flag = 0,
             int border = 0,
             wxObject *userData = NULL);

   void Add(int width,
            int height,
            int proportion = 0,
            int flag = 0,
            int border = 0,
            wxObject *userData = NULL);

   void AddSpacer(int size = 14);
   void AddStretchSpacer(int prop = 1);

   void Detach(wxWindow *window);
   void Detach(wxSizer *sizer);

   void Updated();

   virtual void Populate() = 0;
   virtual void Repaint(wxDC *dc) = 0;

   void OnErase(wxEraseEvent & event);
   void OnPaint(wxPaintEvent & event);
   void OnLeftDown(wxMouseEvent & event);
   void OnLeftUp(wxMouseEvent & event);
   void OnMotion(wxMouseEvent & event);

 private:

   void Init(wxWindow *parent, int type, const wxString & title, const wxString & label);

   wxWindow *mParent;
   
   Grabber *mGrabber;
   wxBoxSizer *mHSizer;
   wxSizerItem *mSpacer;

   wxPoint mResizeStart;

   wxString mTitle;
   wxString mLabel;
   int mType;

   bool mVisible;
   bool mDocked;
   bool mResizable;

 public:

   DECLARE_CLASS(ToolBar);
   DECLARE_EVENT_TABLE();
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
// arch-tag: 5a2a21f8-6c9e-45a4-8718-c26cad5cfe65

