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
#include <wx/brush.h>
#include <wx/dcclient.h>
#include <wx/intl.h>
#include <wx/settings.h>
#endif  /*  */


#include <wx/image.h>

#include "widgets/AButton.h"
#include "widgets/ASlider.h"
#include "ControlToolBar.h"
#include "EditToolBar.h"
#include "Project.h"
#include "Audacity.h"

////////////////////////////////////////////////////////////
/// Methods for ToolBarStub
////////////////////////////////////////////////////////////


// ToolBarStub Constructer. Requires a ToolBarType.
// Whenever a ToolBarStub is around, there will be a floating
// ToolBarFrame.  It may be hidden or unhidden.
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


//ToolBarStub destructer
ToolBarStub::~ToolBarStub() 
{
   if (mToolBarFrame) {
      delete mToolBarFrame;
      mToolBarFrame = NULL;
   }
}


// This will add a new toolbar to all project windows, 
// even if one of that type already exists.
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


// This will unload the toolbar from all windows
// If more than one toolbar of this type exists in a window, this
// will unload them all
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


// This will make the floating ToolBarFrame appear at the specified location
void ToolBarStub::ShowWindowedToolBar(wxPoint * where /* = NULL */ ) 
{
   if (!mWindowedStatus) {

      //Move the frame to the mouse position
      if (where)
         mToolBarFrame->Move(*where);

      //Show the new window
      mToolBarFrame->Show();
   }
   mWindowedStatus = true;
}


// This will make the floating ToolBarFrame disappear (but it will still exist).
void ToolBarStub::HideWindowedToolBar() 
{
   if (mWindowedStatus) {
      mToolBarFrame->Hide();
      mWindowedStatus = false;
   }
}


// This finds out if a ToolBar of this type is loaded in
// a given project window
bool ToolBarStub::IsToolBarLoaded(AudacityProject * p) 
{
   return p->IsToolBarLoaded(mType);
}


// This will return a pointer to the ToolBar inside the member ToolBarFrame
ToolBar * ToolBarStub::GetToolBar() 
{
   return mToolBarFrame ? mToolBarFrame->GetToolBar() : NULL;
}


////////////////////////////////////////////////////////////
/// Methods for ToolBar
////////////////////////////////////////////////////////////

// Constructor for ToolBar. Should be used by children toolbars
// to instantiate the initial parameters of the toolbar.
ToolBar::ToolBar(wxWindow * parent, wxWindowID id, const wxPoint & pos, const
      wxSize & size) : wxWindow(parent, id, pos, size)
{
   //Set some default values that should be overridden
   mTitle = "Audacity Toolbar";

   wxColour backgroundColour =
      wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);

   mBackgroundBrush.SetColour(backgroundColour);
   mBackgroundPen.SetColour(backgroundColour);
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


// This looks at the first pixel in the image, and shifts
// the entire image by the vector difference between that 
// pixel and the dstColour.  For better control, use
// ChangeImageColour(wxImage, wxColour*, wxColour*) below
wxImage * ToolBar::ChangeImageColour(wxImage * srcImage, wxColour & dstColour) 
{
   unsigned char *src = srcImage->GetData();
   wxColour c;
   c.Set(src[0], src[1], src[2]);
   return ChangeImageColour(srcImage, c, dstColour);
}



//This will explicitly shift the image color from
//srcColour to dstColour. 
wxImage * ToolBar::ChangeImageColour(wxImage * srcImage,
                                     wxColour & srcColour,
                                     wxColour & dstColour) 
{
   // This function takes a source image, which it assumes to
   // be grayscale, and smoothly changes the overall color
   // to the specified color, and returns the result as a
   // new image.  This works well for grayscale 3D images.
   // Audacity uses this routines to make the buttons
   // (skip-start, play, stop, record, skip-end) adapt to
   // the color scheme of the user.

   unsigned char *src = srcImage->GetData();
   int width = srcImage->GetWidth();
   int height = srcImage->GetHeight();

   wxImage * dstImage = new wxImage(width, height);
   unsigned char *dst = dstImage->GetData();


   //Get the source color
   int srcVal[3], srcOpp[3];
   srcVal[0] = srcColour.Red();
   srcVal[1] = srcColour.Green();
   srcVal[2] = srcColour.Blue();

   int dstVal[3], dstOpp[3];
   dstVal[0] = dstColour.Red();
   dstVal[1] = dstColour.Green();
   dstVal[2] = dstColour.Blue();

   int i;
   for (i = 0; i < 3; i++) {
      srcOpp[i] = 255 - srcVal[i];
      dstOpp[i] = 255 - dstVal[i];

   }

   int c = 0;
   for (i = 0; i < width * height * 3; i++) {
      int s = (int) *src;

      if (s > srcVal[c])
         *dst++ = dstVal[c] + dstOpp[c] * (s - srcVal[c]) / srcOpp[c];

      else
         *dst++ = dstVal[c] * s / srcVal[c];
      src++;
      c = (c + 1) % 3;
   }

   return dstImage;
}


wxImage * ToolBar::OverlayImage(wxImage * background, wxImage * foreground,
                                wxImage * mask, int xoff, int yoff) 
{
   // Takes a background image, foreground image, and mask
   // (i.e. the alpha channel for the foreground), and
   // returns an new image where the foreground has been
   // overlaid onto the background using alpha-blending,
   // at location (xoff, yoff).
   unsigned char *bg = background->GetData();
   unsigned char *fg = foreground->GetData();
   unsigned char *mk = mask->GetData();

   int bgWidth = background->GetWidth();
   int bgHeight = background->GetHeight();
   int fgWidth = foreground->GetWidth();
   int fgHeight = foreground->GetHeight();
   int mkWidth = mask->GetWidth();
   int mkHeight = mask->GetHeight();


   //Now, determine the dimensions of the images to be masked together
   //on top of the background.  This should be equal to the area of the
   //smaller of the foreground and the mask, as long as it is 
   //within the area of the background, given the offset.

   //Make sure the foreground size is no bigger than the mask
   int wCutoff = (fgWidth < mkWidth) ? fgWidth : mkWidth;
   int hCutoff = (fgHeight < mkHeight) ? fgHeight : mkHeight;


   // If the masked foreground + offset is bigger than the background, masking
   // should only occur within these bounds of the foreground image
   wCutoff = (bgWidth - xoff > wCutoff) ? wCutoff : bgWidth - xoff;
   hCutoff = (bgHeight - yoff > hCutoff) ? hCutoff : bgHeight - yoff;


   //Make a new image the size of the background
   wxImage * dstImage = new wxImage(bgWidth, bgHeight);
   unsigned char *dst = dstImage->GetData();
   memcpy(dst, bg, bgWidth * bgHeight * 3);


   // Go through the foreground image bit by bit and mask it on to the
   // background, at an offset of xoff,yoff.
   // BUT...Don't go beyond the size of the background image,
   // the foreground image, or the mask 
   int x, y;
   for (y = 0; y < hCutoff; y++) {

      unsigned char *bkp = bg + 3 * ((y + yoff) * bgWidth + xoff);
      unsigned char *dstp = dst + 3 * ((y + yoff) * bgWidth + xoff);

      for (x = 0; x < wCutoff; x++) {

         int value = mk[3 * (y * mkWidth + x)];
         int opp = 255 - value;

         for (int c = 0; c < 3; c++)
            dstp[x * 3 + c] = 
               ((bkp[x * 3 + c] * opp) + 
                (fg[3 * (y * fgWidth + x) + c] * value)) / 255;
      }
   } 
   return dstImage;
}


AButton * ToolBar::MakeButton(wxImage * up, wxImage * down,
                                  wxImage * hilite,
                                  const char **foreground,
                                  const char **disabledfg,
                                  const char **alpha, wxWindowID id,
                                  wxPoint placement, wxSize size,
                                  int xoff, int yoff) 
{
   //This is a generic function that will make a button, given
   //generic button images.
   //  Parameters:
   //
   // up:           An image of the blank button in its "up" state
   // down:         An image of the blank button in its "down" state
   // hilite        An image of the blank button with the hilite halo
   // foreground:   A color bitmap of the icon on the button when its
   //                 enabled--Can be a solid field.
   // disabledfg:   A color bitmap of the icon when its disabled--can be a solid field.
   // alpha:        A greyscale mask that determines how much of the
   //                 foreground shows through
   // id            Button ID
   // placement     Location on the toolbar
   // xoff          x-offset to place icon pixmaps at, with respect to background image
   // yoff          y-offset to place icon pixmaps at, with respect to background image

#if wxVERSION_NUMBER < 2303
   wxImage * color = new wxImage(wxBitmap(foreground));
   wxImage * color_disabled = new wxImage(wxBitmap(disabledfg));
   wxImage * mask = new wxImage(wxBitmap(alpha));

   wxImage * up2 = OverlayImage(up, color, mask, xoff, yoff);
   wxImage * hilite2 = OverlayImage(hilite, color, mask, xoff, yoff);
   wxImage * down2 = OverlayImage(down, color, mask, xoff + 1, yoff + 1);
   wxImage * disable2 =
      OverlayImage(up, color_disabled, mask, xoff, yoff);
#else
   wxImage * color 			= new wxImage(wxBitmap(foreground).ConvertToImage());
   wxImage * color_disabled = new wxImage(wxBitmap(disabledfg).ConvertToImage());
   wxImage * mask 			= new wxImage(wxBitmap(alpha).ConvertToImage());

   wxImage * up2 			= OverlayImage(up, color, mask, xoff, yoff);
   wxImage * hilite2 		= OverlayImage(hilite, color, mask, xoff, yoff);
   wxImage * down2 			= OverlayImage(down, color, mask, xoff + 1, yoff + 1);
   wxImage * disable2 		= OverlayImage(up, color_disabled, mask, xoff, yoff);
#endif

   AButton * button =
      new AButton(this, id, placement, size, up2, hilite2, down2,
            disable2);

   delete color;
   delete color_disabled;
   delete mask;
   delete up2;
   delete down2;
   delete hilite2;
   delete disable2;

   return button;
}

//This changes the state a button (from up to down or vice versa)
void ToolBar::SetButton(bool down, AButton * button) 
{
   if (down)
      button->PushDown();
   else
      button->PopUp();
}


////////////////////////////////////////////////////////////
/// Methods for ToolBarFrame
////////////////////////////////////////////////////////////
    
BEGIN_EVENT_TABLE(ToolBarFrame, wxMiniFrame) 
   EVT_CLOSE(ToolBarFrame::OnCloseWindow)
END_EVENT_TABLE()  

// There are three Constructors for ToolBarFrame. The first two take an actual
// toolbar as an argument, and create a frame around it.  The third takes a
// ToolBarType, and constructs a new ToolBar of that type inside the new
// frame.  To instantiate a new toolbar, add a condition to the switch
// statement inside the third constructor.

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


ToolBarFrame::ToolBarFrame(wxWindow * parent, ToolBar * TB)
   : wxMiniFrame(parent, -1, TB->GetTitle(), wxPoint(0, 0), TB->GetSize(),
         wxTINY_CAPTION_HORIZ | wxSTAY_ON_TOP | wxMINIMIZE_BOX
         | wxFRAME_FLOAT_ON_PARENT),
     mToolBar(TB) 
{

} 


ToolBarFrame::ToolBarFrame(wxWindow * parent, enum ToolBarType tbt)
   : wxMiniFrame(gParentWindow, -1, _(""), wxPoint(1, 1),
         wxSize(20, 20),
         wxTINY_CAPTION_HORIZ | wxSTAY_ON_TOP | wxMINIMIZE_BOX
         | wxFRAME_FLOAT_ON_PARENT)
{
   //Create an embedded toolbar of the proper type
   switch (tbt) {
      case ControlToolBarID:
         mToolBar = new ControlToolBar(this);
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


ToolBarFrame::~ToolBarFrame() 
{
   delete mToolBar;
}

void ToolBarFrame::OnCloseWindow(wxCloseEvent & WXUNUSED(event)) 
{
   this->Hide();
} 
