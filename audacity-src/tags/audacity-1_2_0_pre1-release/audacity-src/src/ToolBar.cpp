/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolBar.cpp
  
  Dominic Mazzoni
  Shane T. Mueller

  See ToolBar.h for details.

**********************************************************************/  

#include "ToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif  /*  */

#ifndef WX_PRECOMP
#include <wx/defs.h>
#include <wx/brush.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/intl.h>
#include <wx/settings.h>
#endif  /*  */


#include <wx/image.h>

#include "Audacity.h"
#include "widgets/AButton.h"
#include "widgets/ASlider.h"
#include "ControlToolBar.h"
#include "EditToolBar.h"
#include "ImageManipulation.h"
#include "MixerToolBar.h"
#include "Project.h"


////////////////////////////////////////////////////////////
/// Methods for ToolBarStub
////////////////////////////////////////////////////////////


/// ToolBarStub Constructer. Requires a ToolBarType.
/// Whenever a ToolBarStub is around, there will be a floating
/// ToolBarFrame.  It may be hidden or unhidden.
ToolBarStub::ToolBarStub(wxWindow * Parent, enum ToolBarType tbt) 
{
   //Create a frame with a toolbar of type tbt inside it
   mToolBarFrame = new ToolBarFrame(Parent, tbt);

   //Get the newly-created toolbar to get some info from it.
   ToolBar * tempTB = mToolBarFrame->GetToolBar();

   mType = tbt;
   mTitle = tempTB->GetTitle();
   mSize = tempTB->GetSize();
   mWindowedStatus = false;
   mLoadedStatus = true;
} 


/// ToolBarStub destructer
ToolBarStub::~ToolBarStub() 
{
   if (mToolBarFrame) {
      delete mToolBarFrame;
      mToolBarFrame = NULL;
   }
}


/// This will add a new toolbar to all project windows, 
/// even if one of that type already exists.
void ToolBarStub::LoadAll() 
{
   wxUpdateUIEvent evt;
   
   //Add the toolbar to each Window 
   int len = gAudacityProjects.GetCount();
   for (int i = 0; i < len; i++) {
      gAudacityProjects[i]->LoadToolBar(mType);
      gAudacityProjects[i]->OnUpdateMenus(evt);
   }
} 


/// This will unload the toolbar from all windows
/// If more than one toolbar of this type exists in a window, this
/// will unload them all
void ToolBarStub::UnloadAll() 
{
   wxUpdateUIEvent evt;
   int len = gAudacityProjects.GetCount();
   for (int i = 0; i < len; i++)
   {
      gAudacityProjects[i]->UnloadToolBar(mType);
      gAudacityProjects[i]->OnUpdateMenus(evt);
   }
} 


/// This will make the floating ToolBarFrame appear at the specified location
void ToolBarStub::ShowWindowedToolBar(wxPoint * where /* = NULL */ ) 
{
   if (!mWindowedStatus) {
      
      //Move the frame to the mouse position
      if (where)
         {
            mToolBarFrame->Move(*where);
         }
      
      //Show the new window
      mToolBarFrame->Show();
   }

   mWindowedStatus = true;
}


/// This will make the floating ToolBarFrame disappear (but it will still exist).
void ToolBarStub::HideWindowedToolBar() 
{
   if (mWindowedStatus) {
      mToolBarFrame->Hide();
      mWindowedStatus = false;
   }
}

// To Iconize a windowed toolbar we just hide it,
// To de-Iconize a windowed toolbar we just show it. 
void ToolBarStub::Iconize(bool bIconize) 
{
   if (mWindowedStatus) {
      if( bIconize )
         mToolBarFrame->Hide();
      else
         mToolBarFrame->Show();
   }
}


/// This finds out if a ToolBar of this type is loaded in
/// a given project window
bool ToolBarStub::IsToolBarLoaded(AudacityProject * p) 
{
   return p->IsToolBarLoaded(mType);
}


/// This will return a pointer to the ToolBar inside the member ToolBarFrame
ToolBar * ToolBarStub::GetToolBar() 
{
   return mToolBarFrame ? mToolBarFrame->GetToolBar() : NULL;
}


////////////////////////////////////////////////////////////
/// Methods for ToolBar
////////////////////////////////////////////////////////////

/// Constructor for ToolBar. Should be used by children toolbars
/// to instantiate the initial parameters of the toolbar.
ToolBar::ToolBar(wxWindow * parent, wxWindowID id, const wxPoint & pos, const
      wxSize & size) : wxWindow(parent, id, pos, size)
{
   //Set some default values that should be overridden
   mTitle = "Audacity Toolbar";

   wxColour backgroundColour =
      wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);

   mBackgroundBrush.SetColour(backgroundColour);
   mBackgroundPen.SetColour(backgroundColour);

   mBackgroundBitmap = NULL;
   mBackgroundHeight = 0;
   mBackgroundWidth = 0;

   mIdealSize = wxSize(300, size.y);
} 


//  Alternate constructor for Toolbar.  Should probably not be used,  
//  except to create a dummy ToolBar.
ToolBar::ToolBar(wxWindow * parent):wxWindow(parent, -1, wxPoint(1, 1),
              wxSize(300, 20)) 
{
   //Set some default values that should be overridden
   mTitle = "Audacity Toolbar";

   wxColour backgroundColour =
      wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);

   mBackgroundBrush.SetColour(backgroundColour);
   mBackgroundPen.SetColour(backgroundColour);
   mIdealSize = wxSize(300, 20);
} 


ToolBar::~ToolBar()
{
   if (mBackgroundBitmap)
      delete mBackgroundBitmap;

}



   ///This is a generic function that will make a button, given
   ///generic button images.
   ///  Parameters:
   ///
   /// up:           An image of the blank button in its "up" state
   /// down:         An image of the blank button in its "down" state
   /// hilite        An image of the blank button with the hilite halo
   /// foreground:   A color bitmap of the icon on the button when its
   ///                 enabled--Can be a solid field.
   /// disabledfg:   A color bitmap of the icon when its disabled--can be a solid field.
   /// alpha:        A greyscale mask that determines how much of the
   ///                 foreground shows through
   /// id            Button ID
   /// placement     Location on the toolbar
   /// processdownevents      boolean that determine whether the button will process events
   ///                        if it is in the down position (and pop up when clicked in the down position)
   /// xoff          x-offset to place icon pixmaps at, with respect to background image
   /// yoff          y-offset to place icon pixmaps at, with respect to background image
AButton * ToolBar::MakeButton(wxImage * up, wxImage * down,
                              wxImage * hilite,
                              const char **foreground,
                              const char **disabledfg,
                              const char **alpha, wxWindowID id,
                              wxPoint placement,
                              bool processdownevents, wxSize size,
                              int xoff, int yoff) 
{

   wxImage * color 			= new wxImage(wxBitmap(foreground).ConvertToImage());
   wxImage * color_disabled = new wxImage(wxBitmap(disabledfg).ConvertToImage());
   wxImage * mask 			= new wxImage(wxBitmap(alpha).ConvertToImage());

   wxImage * up2 			= OverlayImage(up, color, mask, xoff, yoff);
   wxImage * hilite2 		= OverlayImage(hilite, color, mask, xoff, yoff);
   wxImage * down2 			= OverlayImage(down, color, mask, xoff + 1, yoff + 1);
   wxImage * disable2 		= OverlayImage(up, color_disabled, mask, xoff, yoff);

   AButton * button =
      new AButton(this, id, placement, size, up2, hilite2, down2,
            disable2, processdownevents);

   delete color;
   delete color_disabled;
   delete mask;
   delete up2;
   delete down2;
   delete hilite2;
   delete disable2;

   return button;
}

///This changes the state a button (from up to down or vice versa)
void ToolBar::SetButton(bool down, AButton * button) 
{
   if (down)
      button->PushDown();
   else
      button->PopUp();
}


/// This draws the background of a toolbar
void ToolBar::DrawBackground(wxDC &dc, int width, int height)
{

#if defined(__WXMAC__)

   if (mBackgroundWidth < width) {
      if (mBackgroundBitmap)
         delete mBackgroundBitmap;
      
      wxImage *aquaImage = CreateAquaBackground(width, height, 0);
      mBackgroundBitmap = new wxBitmap(aquaImage);
      delete aquaImage;
   }

   wxMemoryDC memDC;
   memDC.SelectObject(*mBackgroundBitmap);

   dc.Blit(0, 0, width, height, &memDC, 0, 0, wxCOPY, FALSE);

#if 0
   height = mIdealSize.GetHeight();

   dc.SetPen(*wxBLACK_PEN);
   dc.DrawLine(27, 0, 27, height - 1);
   dc.DrawLine(55, 0, 55, height - 1);
   dc.DrawLine(83, 0, 83, 27);
   dc.DrawLine(0, 27, 83, 27);
#endif
#else

   dc.SetBrush(mBackgroundBrush);
   dc.SetPen(mBackgroundPen);

   height = mIdealSize.GetHeight();
   dc.DrawRectangle(0, 0, width, height);


#if 0
   // JKC: This code draws a grid of lines around the first few
   // buttons on the toolbar.
   // TODO: Do we want this at all?  
   // If so this should be moved to ControlToolbar.
   // Having it here means that it is also drawn in EditToolBar,
   // which we probably don't want.  (same for the Mac 
   // version in other half of #ifdef).

   dc.SetPen(*wxBLACK_PEN);
   dc.DrawLine(27, 0, 27, height - 1);
   dc.DrawLine(55, 0, 55, height - 1);
   dc.DrawLine(83, 0, 83, height - 1);
   dc.DrawLine(0, 27, 83, 27);
#endif
#endif


}

////////////////////////////////////////////////////////////
/// Methods for ToolBarFrame
////////////////////////////////////////////////////////////
    
BEGIN_EVENT_TABLE(ToolBarFrame, wxMiniFrame) 
   EVT_CLOSE(ToolBarFrame::OnCloseWindow)
END_EVENT_TABLE()  

/// There are three Constructors for ToolBarFrame. The first two take an actual
/// toolbar as an argument, and create a frame around it.  The third takes a
/// ToolBarType, and constructs a new ToolBar of that type inside the new
/// frame.  To instantiate a new toolbar, add a condition to the switch
/// statement inside the third constructor.
ToolBarFrame::ToolBarFrame(wxWindow * parent, ToolBar * TB,
                           const wxString & Title, const wxPoint & position)
   : wxMiniFrame(parent, -1, Title, position,
                 wxSize(TB->GetSize().x,
                        TB->GetSize().y + TOOLBAR_HEIGHT_OFFSET),
                 wxTINY_CAPTION_HORIZ | wxSTAY_ON_TOP | wxMINIMIZE_BOX |
                 wxFRAME_FLOAT_ON_PARENT),
     mToolBar(TB) 
{
   SetSize(TB->GetIdealSize());
} 

///
///This is a alternate constructor for a toolbarframe, taking a 
/// toolbar as an argument
///
ToolBarFrame::ToolBarFrame(wxWindow * parent, ToolBar * TB)
   : wxMiniFrame(parent, -1, TB->GetTitle(), wxPoint(0, 0), TB->GetSize(),
         wxTINY_CAPTION_HORIZ | wxSTAY_ON_TOP | wxMINIMIZE_BOX
         | wxFRAME_FLOAT_ON_PARENT),
     mToolBar(TB) 
{

} 

///
///Alternate constructor for a ToolBarFrame. You give it a type and
///It will create a toolabr of that type inside the frame.
ToolBarFrame::ToolBarFrame(wxWindow * parent, enum ToolBarType tbt)
   : wxMiniFrame(gParentWindow, -1, "", wxPoint(1, 1),
         wxSize(20, 20),
         wxTINY_CAPTION_HORIZ | wxSTAY_ON_TOP | wxMINIMIZE_BOX
         | ((parent == NULL)?0x0:wxFRAME_FLOAT_ON_PARENT))
{
   //Create an embedded toolbar of the proper type
   switch (tbt) {
      case ControlToolBarID:
         mToolBar = new ControlToolBar(this);
         break;
      case MixerToolBarID:
         mToolBar = new MixerToolBar(this);
         break;
      case EditToolBarID:
         mToolBar = new EditToolBar(this);
         break;
      case NoneID:
      default:
         break;
   }

   //Use information about the toolbar to set the frame information properly.
   SetTitle(mToolBar->GetTitle());
   SetSize(wxSize(
           mToolBar->GetSize().x, mToolBar->GetSize().y +
           TOOLBAR_HEIGHT_OFFSET));
}

///
/// Standard ToolBarFrame destructor
ToolBarFrame::~ToolBarFrame() 
{
   delete mToolBar;
}


/// This hides the floating toolbar, effectively 'hiding' the window
void ToolBarFrame::OnCloseWindow(wxCloseEvent & WXUNUSED(event)) 
{
   this->Hide();
} 

