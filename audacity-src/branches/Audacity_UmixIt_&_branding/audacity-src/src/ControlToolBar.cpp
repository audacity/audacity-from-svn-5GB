/**********************************************************************

  Audacity: A Digital Audio Editor

  ControlToolBar.cpp

  Dominic Mazzoni
  Shane T. Mueller
 
  See ControlToolBar.h for details

**********************************************************************/

#include "ControlToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/dcmemory.h>
#include <wx/defs.h>
#include <wx/event.h>
#include <wx/brush.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/settings.h>
#endif

#include <wx/image.h>
#include <wx/tooltip.h>
#include <wx/msgdlg.h>

// For pow() in GetSoundVol()
#include <math.h>

#include "AColor.h"
#include "AudioIO.h"
#include "ImageManipulation.h"
#include "MeterToolBar.h"
#include "Prefs.h"
#include "Project.h"
#include "Track.h"
#include "widgets/AButton.h"
#include "widgets/ASlider.h"

#include "../images/ControlButtons.h"

// Code duplication warning: the tools enum (numTools) in ControlToolBar.h apparently need to be in the
// same order as this enum.

enum {
   ID_SELECT,
   ID_ENVELOPE,
   ID_DRAW,
   ID_ZOOM,
   ID_SLIDE,
   ID_MULTI,
   ID_PLAY_BUTTON,
   #if (AUDACITY_BRANDING == BRAND_THINKLABS)
      ID_LOOP_PLAY_BUTTON,
   //#elif (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
   //   ID_LOCK_BUTTON, 
   #endif
   ID_RECORD_BUTTON,
   ID_PAUSE_BUTTON,
   ID_STOP_BUTTON,
   ID_FF_BUTTON,
   ID_REW_BUTTON,

   ID_FIRST_TOOL = ID_SELECT,
   ID_LAST_TOOL = ID_MULTI
};

// Strings to convert a tool number into a status message
// These MUST be in the same order as the ids above.
const char * MessageOfTool[numTools] = { wxTRANSLATE("Click and drag to select audio"),
   wxTRANSLATE("Click and drag to edit the amplitude envelope"),
   wxTRANSLATE("Click and drag to edit the samples"),
#if defined( __WXMAC__ )
   wxTRANSLATE("Click to Zoom In, Shift-Click to Zoom Out"),
#elif defined( __WXMSW__ )
   wxTRANSLATE("Drag to Zoom Into Region, Right-Click to Zoom Out"),
#elif defined( __WXGTK__ )
   wxTRANSLATE("Left=Zoom In, Right=Zoom Out, Middle=Normal"),
#endif
   wxTRANSLATE("Click and drag to move a track in time"),
   "" // multi-mode tool
};


const int BUTTON_WIDTH = 50;

//static
AudacityProject *ControlToolBar::mBusyProject = NULL;

////////////////////////////////////////////////////////////
/// Methods for ControlToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(ControlToolBar, wxWindow)
   EVT_PAINT(ControlToolBar::OnPaint)
   EVT_CHAR(ControlToolBar::OnKeyEvent)
   EVT_COMMAND_RANGE(ID_FIRST_TOOL, ID_LAST_TOOL,
         wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnTool)
   EVT_COMMAND(ID_PLAY_BUTTON,
         wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnPlay)
   #if (AUDACITY_BRANDING == BRAND_THINKLABS)
      EVT_COMMAND(ID_LOOP_PLAY_BUTTON,
            wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnLoopPlay)
   //#elif (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
   //   EVT_COMMAND(ID_LOCK_BUTTON,
   //         wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnLock)
      //v Never gets called.  EVT_KEY_UP(ControlToolBar::OnKeyUp)
   #endif
   EVT_COMMAND(ID_STOP_BUTTON,
         wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnStop)
   EVT_COMMAND(ID_RECORD_BUTTON,
         wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnRecord)
   EVT_COMMAND(ID_REW_BUTTON,
            wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnRewind)
   EVT_COMMAND(ID_FF_BUTTON,
            wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnFF)
   EVT_COMMAND(ID_PAUSE_BUTTON,
               wxEVT_COMMAND_BUTTON_CLICKED, ControlToolBar::OnPause)
END_EVENT_TABLE()

//Standard contructor
ControlToolBar::ControlToolBar(wxWindow * parent):
ToolBar(parent, -1, wxPoint(1, 1), wxSize(420, 55))
{
   InitializeControlToolBar();
}

//Another constructor
ControlToolBar::ControlToolBar(wxWindow * parent, wxWindowID id,
                               const wxPoint & pos,
                               const wxSize & size):ToolBar(parent, id,
                                                            pos, size)
{
   InitializeControlToolBar();
}


// This sets up the ControlToolBar, initializing all the important values
// and creating the buttons.
void ControlToolBar::InitializeControlToolBar()
{
   mIdealSize = wxSize(420, 55); // ideal for (mShowTools == true)
   mTitle = _("Audacity Control Toolbar");
   mType = ControlToolBarID;
   mShowTools = true;

   wxColour backgroundColour =
      wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
      this->SetBackgroundColour(*wxBLUE); // blue for Ready
   #endif

   //Read the following wxASSERTs as documentating a design decision
   wxASSERT( selectTool   == ID_SELECT   - ID_FIRST_TOOL );
   wxASSERT( envelopeTool == ID_ENVELOPE - ID_FIRST_TOOL );
   wxASSERT( slideTool    == ID_SLIDE    - ID_FIRST_TOOL );
   wxASSERT( zoomTool     == ID_ZOOM     - ID_FIRST_TOOL );
   wxASSERT( drawTool     == ID_DRAW     - ID_FIRST_TOOL );
   wxASSERT( multiTool    == ID_MULTI    - ID_FIRST_TOOL );

   MakeButtons();
   #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
	   AudacityProject* pProj = GetActiveProject();
	   if (pProj) 
			this->SetLock(pProj->m_bWantLockedRecording);
   #endif

   wxImage *sliderOriginal = new wxImage(wxBitmap(Slider).ConvertToImage());
   wxImage *thumbOriginal = new wxImage(wxBitmap(SliderThumb).ConvertToImage());

#ifdef __WXMAC__
   wxImage *sliderNew = sliderOriginal;
   wxImage *thumbNew = thumbOriginal;
#else
   wxImage *sliderNew = ChangeImageColour(sliderOriginal,
                                          backgroundColour);
   wxImage *thumbNew = ChangeImageColour(thumbOriginal,
                                         backgroundColour);
#endif

   delete sliderOriginal;
   delete thumbOriginal;
#ifndef __WXMAC__
   delete sliderNew;
   delete thumbNew;
#endif

   mCurrentTool = selectTool;

   #if (AUDACITY_BRANDING == BRAND_THINKLABS)
      // no tools created for Thinklabs
      mShowTools = false;
   #else 
      mTool[mCurrentTool]->PushDown();
   #endif

   gPrefs->Read("/GUI/AlwaysEnablePlay", &mAlwaysEnablePlay, false);
   gPrefs->Read("/GUI/AlwaysEnablePause", &mAlwaysEnablePause, false);

   mPaused=false;             //Turn the paused state to off
#if 0
   if(!mAlwaysEnablePause)
      mPause->Disable();         //Turn the pause button off.
   gAudioIO->SetAlwaysEnablePause(mAlwaysEnablePause);
#endif

#if 0
#if defined(__WXMAC__)          // && defined(TARGET_CARBON)
   mDivBitmap = new wxBitmap((const char **) Div);
   mMuteBitmap = new wxBitmap((const char **) Mute);
   mLoudBitmap = new wxBitmap((const char **) Loud);
#endif
#endif
}


wxImage *ControlToolBar::MakeToolImage(wxImage * tool,
                                       wxImage * mask, int style)
{
   // This code takes the image of a tool, and its mask,
   // and creates one of four images of this tool inside
   // a little button, for the toolbar.  The tool
   // is alpha-blended onto the background.

   const char **src;

   switch(style) {
   case 1: // hilite
      src = Hilite;
      break;
   case 2: // down
      src = Down;
      break;
   default:
      src = Up;
      break;
   }

   wxImage *bkgndOriginal = new wxImage(wxBitmap(src).ConvertToImage());
   wxImage *upOriginal = new wxImage(wxBitmap(Up).ConvertToImage());

#ifdef __WXMAC__
   wxImage *background = bkgndOriginal;
#else
   wxColour backgroundColour =
       wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   wxColour baseColour;
   unsigned char *data = upOriginal->GetData();
   baseColour.Set(data[28 * 3], data[28 * 3 + 1], data[28 * 3 + 2]);
   wxImage *background = ChangeImageColour(bkgndOriginal,
                                           baseColour,
                                           backgroundColour);
#endif

   // 
   // Overlay the tool on top of it
   //

   wxImage *result;
   if (style == 2)              // down
      result = OverlayImage(background, tool, mask, 1, 1);
   else
      result = OverlayImage(background, tool, mask, 0, 0);
   delete background;

   #ifndef __WXMAC__
   delete bkgndOriginal;
   delete upOriginal;
   #endif

   return result;
}

AButton *ControlToolBar::MakeTool(const char **tool, const char **alpha,
                                  wxWindowID id, int left, int top)
{
   wxImage *ctr = new wxImage(wxBitmap(tool).ConvertToImage());
   wxImage *mask = new wxImage(wxBitmap(alpha).ConvertToImage());
   wxImage *up = MakeToolImage(ctr, mask, 0);
   wxImage *hilite = MakeToolImage(ctr, mask, 1);
   wxImage *down = MakeToolImage(ctr, mask, 2);
   wxImage *dis = MakeToolImage(ctr, mask, 3);

   AButton *button =
       new AButton(this, id, wxPoint(left, top), wxSize(27, 27),
                   up, hilite, down, dis, false);

   delete ctr;
   delete mask;
   delete up;
   delete hilite;
   delete down;
   delete dis;

   return button;
}

void ControlToolBar::EnablePauseCommand(bool bEnable)
{
   // Enable/disable the "P" key command. 
   // GetActiveProject() won't work for all callers, e.g., MakeButton(), because it hasn't finished initializing.
   AudacityProject* pProj = (AudacityProject*)GetParent(); 
   if (pProj)
   {
      CommandManager* pCmdMgr = pProj->GetCommandManager();
      if (pCmdMgr)
         pCmdMgr->Enable(wxT("Pause"), bEnable);
   }
}

// This is a convenience function that allows for button creation in
// MakeButtons() with fewer arguments
AButton *ControlToolBar::MakeButton(char const **foreground,
                                    char const **disabled,
                                    char const **alpha, int id, bool processdownevents)
{

   // Windows (TM) has a little extra room for some reason, so the top of the
   // buttons should be a little lower.
   int buttonTop = 4;
#ifdef __WXMSW__
   buttonTop=4;
#endif


   AButton *r = ToolBar::MakeButton(upPattern, downPattern, hilitePattern,
                                    foreground, disabled, alpha, wxWindowID(id),
                                    wxPoint(mButtonPos,buttonTop), processdownevents,
                                    wxSize(48, 48), 16, 16);
   mButtonPos += BUTTON_WIDTH;
   #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
      // We're overlaying ID_PAUSE_BUTTON on top of ID_PLAY_BUTTON, so don't bump x coordinate after PLAY.
      if (id == ID_PLAY_BUTTON) 
         mButtonPos -= BUTTON_WIDTH;
      else if (id == ID_PAUSE_BUTTON)
      {
         r->Hide(); // Default is to show Play, not Pause.
         this->EnablePauseCommand(false);
      }
   #endif
   return r;
}

void ControlToolBar::MakeLoopImage()
{
   int xoff = 16;
   int yoff = 16;

   wxImage * color 			= new wxImage(wxBitmap(Loop).ConvertToImage());
   wxImage * color_disabled = new wxImage(wxBitmap(LoopDisabled).ConvertToImage());
   wxImage * mask 			= new wxImage(wxBitmap(LoopAlpha).ConvertToImage());
   
   wxImage * up2 			   = OverlayImage(upPattern, color, mask, xoff, yoff);
   wxImage * hilite2 		= OverlayImage(hilitePattern, color, mask, xoff, yoff);
   wxImage * down2 			= OverlayImage(downPattern, color, mask, xoff + 1, yoff + 1);
   wxImage * disable2 		= OverlayImage(upPattern, color_disabled, mask, xoff, yoff);

   mPlay->SetAlternateImages(up2, hilite2, down2, disable2);

   delete color;
   delete color_disabled;
   delete mask;
   delete up2;
   delete hilite2;
   delete down2;
   delete disable2;
}

void ControlToolBar::RegenerateToolsTooltips()
{

// JKC: 
//   Under Win98 Tooltips appear to be buggy, when you have a lot of
//   tooltip messages flying around.  I found that just creating a 
//   twelfth tooltip caused Audacity to crash when it tried to show 
//   any tooltip.
//
//   Win98 does NOT recover from this crash - for any application which is 
//   using tooltips will also crash thereafter...  so you must reboot.
//   Rather weird.  
//
//   Getting windows to process more of its stacked up messages seems
//   to workaround the problem.  The problem is not fully understood though
//   (as of April 2003).
   
   //	Vaughan, October 2003: Now we're crashing on Win2K if 
	// "Quit when closing last window" is unchecked, when we come back 
	// through here, on either of the wxSafeYield calls.
	// James confirms that commenting them out does not cause his original problem 
	// to reappear, so they're commented out now.
	//		wxSafeYield(); //Deal with some queued up messages...

   #if (wxUSE_TOOLTIPS && (AUDACITY_BRANDING != BRAND_THINKLABS)) // no tools for Thinklabs
      mTool[selectTool]->SetToolTip(_("Selection Tool"));
      mTool[envelopeTool]->SetToolTip(_("Envelope Tool"));
      mTool[slideTool]->SetToolTip(_("Time Shift Tool"));
      mTool[zoomTool]->SetToolTip(_("Zoom Tool"));
      mTool[drawTool]->SetToolTip(_("Draw Tool"));
      mTool[multiTool]->SetToolTip(_("Multi-Tool Mode"));
   #endif

   //		wxSafeYield();
   return;

}

static const int kToolsClusterWidth = 3 * 27; // Each tool is 27x27, in a 3x2 grid.
static const int kButtonsXOffset = 14;

#if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH) 
   // Don't bother to conditionalize these in ControlButtons.h, just load them but use these instead. 
   #include "../images/ControlButtons/Pause_yellow.xpm"
   #include "../images/ControlButtons/PauseDisabled_yellow.xpm"
   #include "../images/ControlButtons/Stop_blue.xpm"
   #include "../images/ControlButtons/StopDisabled_blue.xpm"
#endif

void ControlToolBar::MakeButtons()
{
   wxImage *upOriginal = new wxImage(wxBitmap(UpButton).ConvertToImage());
   wxImage *downOriginal = new wxImage(wxBitmap(DownButton).ConvertToImage());
   wxImage *hiliteOriginal = new wxImage(wxBitmap(HiliteButton).ConvertToImage());

   wxColour newColour =
       wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);

#ifdef __WXMAC__
   upPattern = upOriginal;
   downPattern = downOriginal;
   hilitePattern = hiliteOriginal;
#else
   upPattern = ChangeImageColour(upOriginal, newColour);
   downPattern = ChangeImageColour(downOriginal, newColour);
   hilitePattern = ChangeImageColour(hiliteOriginal, newColour);
#endif

   /* Buttons */
   #if (AUDACITY_BRANDING == BRAND_THINKLABS) // || (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
      #define kNumButtons 7
   #else
      #define kNumButtons 6
   #endif
   int buttonOrder[kNumButtons];
   mButtonPos = kButtonsXOffset;
   #if (AUDACITY_BRANDING != BRAND_THINKLABS)
      // no tools for Thinklabs
      if (mShowTools)
         mButtonPos += kToolsClusterWidth;
   #endif
   
   gPrefs->Read("/GUI/ErgonomicTransportButtons", &mErgonomicTransportButtons, false);

   if (mErgonomicTransportButtons)
   {
      #if (AUDACITY_BRANDING == BRAND_THINKLABS)
         buttonOrder[0] = ID_PAUSE_BUTTON;
         buttonOrder[1] = ID_PLAY_BUTTON;
         buttonOrder[2] = ID_LOOP_PLAY_BUTTON;
         buttonOrder[3] = ID_STOP_BUTTON;
         buttonOrder[4] = ID_REW_BUTTON;
         buttonOrder[5] = ID_FF_BUTTON;
         buttonOrder[6] = ID_RECORD_BUTTON;
      #elif (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
         //buttonOrder[0] = ID_LOCK_BUTTON;
         //buttonOrder[1] = ID_RECORD_BUTTON;
         //buttonOrder[2] = ID_REW_BUTTON;
         //buttonOrder[3] = ID_PLAY_BUTTON;
         //buttonOrder[4] = ID_PAUSE_BUTTON; // actually, on top of Play btn, and alternately hidden
         //buttonOrder[5] = ID_STOP_BUTTON;
         //buttonOrder[6] = ID_FF_BUTTON;

         buttonOrder[0] = ID_RECORD_BUTTON;
         buttonOrder[1] = ID_REW_BUTTON;
         buttonOrder[2] = ID_PLAY_BUTTON;
         buttonOrder[3] = ID_PAUSE_BUTTON; // actually, on top of Play btn, and alternately hidden
         buttonOrder[4] = ID_STOP_BUTTON;
         buttonOrder[5] = ID_FF_BUTTON;
      #else
         buttonOrder[0] = ID_PAUSE_BUTTON;
         buttonOrder[1] = ID_PLAY_BUTTON;
         buttonOrder[2] = ID_STOP_BUTTON;
         buttonOrder[3] = ID_REW_BUTTON;
         buttonOrder[4] = ID_FF_BUTTON;
         buttonOrder[5] = ID_RECORD_BUTTON;
      #endif
   } else
   {
      #if (AUDACITY_BRANDING == BRAND_THINKLABS)
         buttonOrder[0] = ID_REW_BUTTON;
         buttonOrder[1] = ID_PLAY_BUTTON;
         buttonOrder[2] = ID_LOOP_PLAY_BUTTON;
         buttonOrder[3] = ID_RECORD_BUTTON;
         buttonOrder[4] = ID_PAUSE_BUTTON;
         buttonOrder[5] = ID_STOP_BUTTON;
         buttonOrder[6] = ID_FF_BUTTON;
      #elif (AUDACITY_BRANDING == BRAND_AUDIOTOUCH) // ignores mErgonomicTransportButtons
         //buttonOrder[0] = ID_LOCK_BUTTON;
         //buttonOrder[1] = ID_RECORD_BUTTON;
         //buttonOrder[2] = ID_REW_BUTTON;
         //buttonOrder[3] = ID_PLAY_BUTTON;
         //buttonOrder[4] = ID_PAUSE_BUTTON; // actually, on top of Play btn, and alternately hidden
         //buttonOrder[5] = ID_STOP_BUTTON;
         //buttonOrder[6] = ID_FF_BUTTON;

         buttonOrder[0] = ID_RECORD_BUTTON;
         buttonOrder[1] = ID_REW_BUTTON;
         buttonOrder[2] = ID_PLAY_BUTTON;
         buttonOrder[3] = ID_PAUSE_BUTTON; // actually, on top of Play btn, and alternately hidden
         buttonOrder[4] = ID_STOP_BUTTON;
         buttonOrder[5] = ID_FF_BUTTON;
      #else
         buttonOrder[0] = ID_REW_BUTTON;
         buttonOrder[1] = ID_PLAY_BUTTON;
         buttonOrder[2] = ID_RECORD_BUTTON;
         buttonOrder[3] = ID_PAUSE_BUTTON;
         buttonOrder[4] = ID_STOP_BUTTON;
         buttonOrder[5] = ID_FF_BUTTON;
      #endif
   }
   
   for (int iButton = 0; iButton < kNumButtons; iButton++)
   {
      switch (buttonOrder[iButton])
      {
      case ID_REW_BUTTON:
         mRewind = MakeButton((char const **) Rewind,
                              (char const **) RewindDisabled,
                              (char const **) RewindAlpha, ID_REW_BUTTON,
                              false);
         break;
      
      case ID_PLAY_BUTTON:
         mPlay = MakeButton((char const **) Play,
                            (char const **) PlayDisabled,
                            (char const **) PlayAlpha, ID_PLAY_BUTTON,
                            false);
         MakeLoopImage();
         break;
      
      #if (AUDACITY_BRANDING == BRAND_THINKLABS)
         case ID_LOOP_PLAY_BUTTON:
            mLoopPlay = MakeButton((char const **) Loop,
                                    (char const **) LoopDisabled,
                                    (char const **) LoopAlpha, ID_LOOP_PLAY_BUTTON,
                                    false);
            break;
      //#elif (AUDACITY_BRANDING == BRAND_AUDIOTOUCH) 
      //   case ID_LOCK_BUTTON:
      //      mLock = MakeButton((char const **) Lock, 
      //                           (char const **) LockDisabled, 
      //                           (char const **) LockAlpha, ID_LOCK_BUTTON, 
      //                           true); // processDownEvents
      //      //v alt images for unlocked? Currently just making it toggle up/down with processDownEvents.
      //      break;
      #endif

      case ID_RECORD_BUTTON:
         if (mErgonomicTransportButtons)
            mButtonPos += 10; // space before record button
         
         mRecord = MakeButton((char const **) Record,
                              (char const **) RecordDisabled,
                              (char const **) RecordAlpha, ID_RECORD_BUTTON,
                              false);
         break;
      
      #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH) 
         // BRAND_AUDIOTOUCH swaps colors for Pause and Stop. 
         case ID_PAUSE_BUTTON:
            mPause = MakeButton((char const **)Pause_yellow,
                              (char const **) PauseDisabled_yellow,
                              (char const **) PauseAlpha, ID_PAUSE_BUTTON,
                              true);
            break;
         
         case ID_STOP_BUTTON:
            mStop = MakeButton((char const **) Stop_blue,
                              (char const **) StopDisabled_blue,
                              (char const **) StopAlpha, ID_STOP_BUTTON,
                              false);
            break;
      #else
         case ID_PAUSE_BUTTON:
            mPause = MakeButton((char const **)Pause,
                              (char const **) PauseDisabled,
                              (char const **) PauseAlpha, ID_PAUSE_BUTTON,
                              true);
            break;
         
         case ID_STOP_BUTTON:
            mStop = MakeButton((char const **) Stop,
                              (char const **) StopDisabled,
                              (char const **) StopAlpha, ID_STOP_BUTTON,
                              false);
            break;
      #endif
      
      case ID_FF_BUTTON:
         mFF = MakeButton((char const **) FFwd,
                          (char const **) FFwdDisabled,
                          (char const **) FFwdAlpha, ID_FF_BUTTON,
                          false);
         break;
      
      default:
         wxASSERT(false); // unknown button id
      }
   }

   #if wxUSE_TOOLTIPS
         mRewind->SetToolTip(_("Skip to Start"));
         mPlay->SetToolTip(_("Play (Shift for loop-play)"));
         #if (AUDACITY_BRANDING == BRAND_THINKLABS)
            mLoopPlay->SetToolTip(_("Loop Play"));
         //#elif (AUDACITY_BRANDING == BRAND_AUDIOTOUCH) 
         //   mLock->SetToolTip(_("Lock/Unlock Recording"));
         #endif
         mRecord->SetToolTip(_("Record"));
         mPause->SetToolTip(_("Pause"));
         mStop->SetToolTip(_("Stop"));
         mFF->SetToolTip(_("Skip to End"));
   #endif

#ifndef __WXMAC__
   delete upPattern;
   delete downPattern;
   delete hilitePattern;
#endif

   delete upOriginal;
   delete downOriginal;
   delete hiliteOriginal;

   /* Tools */

   #if (AUDACITY_BRANDING == BRAND_THINKLABS)
      // no tools for Thinklabs
      for (unsigned int i = 0; i < numTools; i++)
         mTool[i] = NULL;
   #else
      #ifdef __WXMAC__ // different positioning
         mTool[selectTool] = MakeTool(IBeam, IBeamAlpha, ID_SELECT, 0, 0);
         mTool[zoomTool] = MakeTool(Zoom, ZoomAlpha, ID_ZOOM, 0, 26);
         mTool[envelopeTool] = MakeTool(Envelope, EnvelopeAlpha, ID_ENVELOPE, 26, 0);
         mTool[slideTool] = MakeTool(TimeShift, TimeShiftAlpha, ID_SLIDE, 26, 26);
         mTool[drawTool]  = MakeTool(Draw, DrawAlpha, ID_DRAW, 52, 0);
         mTool[multiTool] = MakeTool(Multi, MultiAlpha, ID_MULTI, 52, 26); 
      #else
         mTool[selectTool] = MakeTool(IBeam, IBeamAlpha, ID_SELECT, 0, 0);
         mTool[zoomTool] = MakeTool(Zoom, ZoomAlpha, ID_ZOOM, 0, 28);
         mTool[envelopeTool] = MakeTool(Envelope, EnvelopeAlpha, ID_ENVELOPE, 27, 0);
         mTool[slideTool] = MakeTool(TimeShift, TimeShiftAlpha, ID_SLIDE, 27, 28);
         mTool[drawTool]  = MakeTool(Draw, DrawAlpha, ID_DRAW, 54, 0);
         mTool[multiTool] = MakeTool(Multi, MultiAlpha, ID_MULTI, 54, 28); 
      #endif
   #endif

#if wxUSE_TOOLTIPS
#ifdef __WXMAC__
   wxToolTip::Enable(false);    // DM: tooltips are broken in wxMac
#else
// MB: Should make this a pref
   wxToolTip::Enable(true);     
   wxToolTip::SetDelay(1000);
#endif
#endif

   RegenerateToolsTooltips();
}

ControlToolBar::~ControlToolBar()
{
   for (int i = 0; i < numTools; i++)
      delete mTool[i];

   delete mRewind;
   delete mPlay;
   #if (AUDACITY_BRANDING == BRAND_THINKLABS)
      delete mLoopPlay;
   #endif
   delete mStop;
   delete mRecord;
   delete mFF;
   delete mPause;

   //delete mVolume;

#if 0
#if defined(__WXMAC__)          // && defined(TARGET_CARBON)
   delete mDivBitmap;
   delete mMuteBitmap;
   delete mLoudBitmap;
#endif
#endif
}

void ControlToolBar::OnKeyEvent(wxKeyEvent & event)
{
   if (event.ControlDown() || event.AltDown()) {
      event.Skip();
      return;
   }

   // Play/Stop
#if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
   if (event.GetKeyCode() == WXK_RETURN) {
#else
   if (event.KeyCode() == WXK_SPACE) {
#endif
      if (gAudioIO->IsStreamActive(GetActiveProject()->GetAudioIOToken())) {
         SetPlay(false);
         SetStop(true);
         StopPlaying();
      }
      else if (!gAudioIO->IsBusy()) {
         SetPlay(true);
         SetStop(false);
         PlayCurrentRegion();
      }
      return;
   }
   event.Skip();
}

//void ControlToolBar::OnKeyUp(wxKeyEvent & event)
//{
//   #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
//      if ((event.KeyCode() == WXK_SPACE) && // Record
//            !mIsLocked)
//      {
//         StopPlaying();
//
//         SetPlay(false);
//         SetRecord(false);
//         SetStop(true);
//         mStop->Disable();
//      }
//   #endif
//   event.Skip();
//}
//
void ControlToolBar::UpdatePrefs()
{
	gPrefs->Read("/GUI/AlwaysEnablePlay", &mAlwaysEnablePlay, false);
#if 0
   gPrefs->Read("/GUI/AlwaysEnablePause", &mAlwaysEnablePause, false);

   if(mAlwaysEnablePause)
   {
      mPause->Enable();
      this->EnablePauseCommand(true);
   }
   else if(!mAlwaysEnablePause && !gAudioIO->IsBusy())
   {
      mPause->PopUp();
      mPause->Disable();
      this->EnablePauseCommand(false);

      mPaused = false;
      gAudioIO->SetPaused(false);
   }

   gAudioIO->SetAlwaysEnablePause(mAlwaysEnablePause);
#endif
}

/// Gets the currently active tool
/// In Multi-mode this might not return the multi-tool itself
/// since the active tool may be changed by what you hover over.
int ControlToolBar::GetCurrentTool()
{
   return mCurrentTool;
}

/// Sets the currently active tool
/// @param tool - The index of the tool to be used.
/// @param show - should we update the button display?
void ControlToolBar::SetCurrentTool(int tool, bool show)
{
   #if (AUDACITY_BRANDING == BRAND_THINKLABS) 
      // no tools for Thinklabs
      return;
   #endif

   //In multi-mode the current tool is shown by the 
   //cursor icon.  The buttons are not updated.

   if (tool != mCurrentTool) {
      if (show)
         mTool[mCurrentTool]->PopUp();
      mCurrentTool=tool;
      if (show)
         mTool[mCurrentTool]->PushDown();
   }
   RedrawAllProjects();
}

/// Show/Hide the tools at left. 
void ControlToolBar::ShowTools(bool bShowTools /*= true*/)
{
   if (mShowTools == bShowTools)
      return; // no change
   mShowTools = bShowTools;

   // Show/Hide the tools. 
   for (unsigned int i = 0; i < numTools; i++)
      mTool[i]->Show(bShowTools);

   // Move the transport buttons.
   int x; 
   int y;

   mRewind->GetPosition(&x, &y);
   mRewind->Move(bShowTools ? x + kToolsClusterWidth : x - kToolsClusterWidth, y);

   mPlay->GetPosition(&x, &y);
   mPlay->Move(bShowTools ? x + kToolsClusterWidth : x - kToolsClusterWidth, y);

   #if (AUDACITY_BRANDING == BRAND_THINKLABS)
      mLoopPlay->GetPosition(&x, &y);
      mLoopPlay->Move(bShowTools ? x + kToolsClusterWidth : x - kToolsClusterWidth, y);
   #endif

   mRecord->GetPosition(&x, &y);
   mRecord->Move(bShowTools ? x + kToolsClusterWidth : x - kToolsClusterWidth, y);

   mPause->GetPosition(&x, &y);
   mPause->Move(bShowTools ? x + kToolsClusterWidth : x - kToolsClusterWidth, y);

   mStop->GetPosition(&x, &y);
   mStop->Move(bShowTools ? x + kToolsClusterWidth : x - kToolsClusterWidth, y);

   mFF->GetPosition(&x, &y);
   mFF->Move(bShowTools ? x + kToolsClusterWidth : x - kToolsClusterWidth, y);


   // Change the toolbar size.
   if (bShowTools)
      mIdealSize = wxSize(420, 55); 
   else
      mIdealSize = wxSize(420 - kToolsClusterWidth, 55); 
}

void ControlToolBar::SetPlay(bool down)
{
   if (down)
   {
      mPlay->PushDown();
      #if (AUDACITY_BRANDING == BRAND_THINKLABS)
         mLoopPlay->PopUp();
         mLoopPlay->Disable();
      #elif (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
         mPlay->Hide();
         mPause->Show();
         this->EnablePauseCommand(true);
      #endif
   }
   else {
      mPlay->PopUp();
      #if (AUDACITY_BRANDING == BRAND_THINKLABS)
         mLoopPlay->PopUp();
         mLoopPlay->Enable();
      #elif (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
         mPlay->Show();
         mPause->Hide();
         this->EnablePauseCommand(false);
      #endif
      mPlay->SetAlternate(false);
   }
}

void ControlToolBar::SetStop(bool down)
{
   if (down)
      mStop->PushDown();
   else {
      mStop->PopUp();
      mStop->Disable();
      mRecord->Enable();
      mPlay->Enable();
      #if (AUDACITY_BRANDING == BRAND_THINKLABS)
         mLoopPlay->Enable();
      #elif (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
         mPlay->Show();
         mPause->Hide();
         this->EnablePauseCommand(false);
      #endif
      if(!mAlwaysEnablePause)
      {
         mPause->Disable();
         this->EnablePauseCommand(false);
      }
      mRewind->Enable();
      mFF->Enable();
   }
}

void ControlToolBar::SetRecord(bool down)
{
   if (down)
      mRecord->PushDown();
   else
      mRecord->PopUp();
}

void ControlToolBar::PlayPlayRegion(double t0, double t1,
                                    bool looped /* = false */)
{
   if (gAudioIO->IsBusy()) {
      mPlay->PopUp();
      #if (AUDACITY_BRANDING == BRAND_THINKLABS)
         mLoopPlay->PopUp();
      #elif (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
         mPlay->Show();
         mPause->Hide();
         this->EnablePauseCommand(false);
      #endif
      return;
   }
  
   mStop->Enable();
   mRewind->Disable();
   mRecord->Disable();
   mFF->Disable();
   mPause->Enable();
   this->EnablePauseCommand(true);
   
   AudacityProject *p = GetActiveProject();
   if (p) {
      TrackList *t = p->GetTracks();
      double maxofmins,minofmaxs;
      
      // JS: clarified how the final play region is computed;
      
      if (t1 == t0) {
         // msmeyer: When playing looped, we play the whole file, if
         // no range is selected. Otherwise, we play from t0 to end
         if (looped) {
            // msmeyer: always play from start
            t0 = t->GetStartTime();
         } else {
            // move t0 to valid range
            if (t0 < 0)
               t0 = t->GetStartTime();
            if (t0 > t->GetEndTime())
               t0 = t->GetEndTime();
         }
         
         // always play to end
         t1 = t->GetEndTime();
      }
      else {
         // always t0 < t1 right?

         // the set intersection between the play region and the
         // valid range maximum of lower bounds
         if (t0 < t->GetStartTime())
            maxofmins = t->GetStartTime();
         else
            maxofmins = t0;
         
         // minimum of upper bounds
         if (t1 > t->GetEndTime())
            minofmaxs = t->GetEndTime();
         else
            minofmaxs = t1;

         // we test if the intersection has no volume 
         if (minofmaxs <= maxofmins) {
            // no volume; play nothing
            return;
         }
         else {
            t0 = maxofmins;
            t1 = minofmaxs;
         }
      }

      bool success = false;
      if (t1 > t0) {
         int token =
            gAudioIO->StartStream(t->GetWaveTrackArray(false),
                                  WaveTrackArray(), t->GetTimeTrack(),
                                  p->GetRate(), t0, t1, looped);
         if (token != 0) {
            success = true;
            p->SetAudioIOToken(token);
            mBusyProject = p;
            SetVUMeters(p);
         }
         else {
            // msmeyer: Show error message if stream could not be opened
            wxMessageBox(_("Error while opening sound device. Please check the output "
                           "device settings and the project sample rate."),
                         _("Error"), wxOK | wxICON_EXCLAMATION, this);
         }
      }

      if (!success) {
         SetPlay(false);
         SetStop(false);
         SetRecord(false);
      }
      #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
         if (success)
         {
            mPlay->Hide();
            mPause->Show();
            this->EnablePauseCommand(true);

            this->SetBackgroundColour(*wxGREEN); // green for Playing
            this->Refresh();
         }
      #endif
   }
}

void ControlToolBar::OnShiftDown(wxKeyEvent & event)
{
   // Turn the "Play" button into a "Loop" button
   if (!mPlay->IsDown())
      mPlay->SetAlternate(true);
}

void ControlToolBar::OnShiftUp(wxKeyEvent & event)
{
   // Turn the "Loop" button into a "Play" button
   if (!mPlay->IsDown())
      mPlay->SetAlternate(false);
}

void ControlToolBar::OnPlay(wxCommandEvent &evt)
{
	if(mAlwaysEnablePlay)
		StopPlaying();

   if(mPlay->WasShiftDown())
      PlayCurrentRegion(true);
   else
      PlayCurrentRegion(false);

   if(mAlwaysEnablePlay)
	   mPlay->PopUp();
}

#if (AUDACITY_BRANDING == BRAND_THINKLABS)
   void ControlToolBar::OnLoopPlay(wxCommandEvent &evt)
   {
	   if(mAlwaysEnablePlay)
		   StopPlaying();

      PlayCurrentRegion(true);

      if(mAlwaysEnablePlay)
      {
	      mPlay->PopUp();
         mLoopPlay->PopUp();
      }
   }
#elif (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
   void ControlToolBar::SetLock(bool bLocked)
   {
      mIsLocked = bLocked;
		// 2009-02-22: No lock button.
      //if (mIsLocked)
      //   mLock->PushDown();
      //else
      //   // In unlocked state, user must hold down Record btn or spacebar to continue recording. 
      //   mLock->PopUp();
   }
#endif

void ControlToolBar::SetVUMeters(AudacityProject *p)
{
   MeterToolBar *bar;
   bar = p->GetMeterToolBar();
   if (bar) {
      Meter *play, *record;
      bar->GetMeters(&play, &record);
      gAudioIO->SetMeters(record, play);
   }
}

void ControlToolBar::PlayCurrentRegion(bool looped /* = false */)
{
   mPlay->SetAlternate(looped);

   AudacityProject *p = GetActiveProject();
   if (p)
   {
      if (looped)
         p->mLastPlayMode = loopedPlay;
      else
         p->mLastPlayMode = normalPlay;
      PlayPlayRegion(p->GetSel0(), p->GetSel1(), looped);
   }
}

void ControlToolBar::OnStop(wxCommandEvent &evt)
{
   StopPlaying();
}

void ControlToolBar::StopPlaying()
{
   mStop->PushDown();

   SetStop(false);
   gAudioIO->StopStream();
   SetPlay(false);
   SetRecord(false);
   #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
      this->SetBackgroundColour(*wxBLUE); // blue for Ready
      this->Refresh();
   #endif

   mPause->PopUp();
   mPaused=false;
   //Make sure you tell gAudioIO to unpause
   gAudioIO->SetPaused(mPaused);

   mBusyProject = NULL;
}

void ControlToolBar::OnRecord(wxCommandEvent &evt)
{
   if (gAudioIO->IsBusy()) {
      #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
         if (mIsLocked) // Stop only if in locked mode. 
         {
            this->StopPlaying(); // Stop recording.
            mRecord->PopUp();
         }
         else
         {
            //v evt.GetClassInfo();
         }
      #else
         mRecord->PopUp();
      #endif
      return;
   }

   mPlay->Disable();
   #if (AUDACITY_BRANDING == BRAND_THINKLABS)
      mLoopPlay->Disable();
   #endif

   #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
      mStop->SetEnabled(mIsLocked);
   #else
      mStop->Enable();
   #endif
   mRewind->Disable();
   mFF->Disable();
   #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
      mPause->SetEnabled(mIsLocked);
      this->EnablePauseCommand(mIsLocked);
      this->SetRecord(mIsLocked); // If locked, push Record down, else up.
   #else
      mPause->Enable();
      mRecord->PushDown();
   #endif

   AudacityProject *p = GetActiveProject();
   if (p) {
      TrackList *t = p->GetTracks();

      // Don't do this for Audiotouch. Not necessary. The performance hit is in the track being recorded, below.
      #if (AUDACITY_BRANDING == BRAND_THINKLABS)
         // For versions that default to dual wave/spectrum display, 
         // switch all tracks to WaveformDisplay on Record, for performance.
         TrackListIterator iter(t);
         for (Track* pTrack = iter.First(); pTrack; pTrack = iter.Next())
            if (pTrack->GetKind() == Track::Wave)
               ((WaveTrack*)pTrack)->SetDisplay(WaveTrack::WaveformDisplay);
      #endif

      double t0 = p->GetSel0();
      double t1 = p->GetSel1();
      if (t1 == t0)
         t1 = 1000000000.0;     // record for a long, long time (tens of years)
      #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
         if (p->m_bWantAppendRecording)
         {
            t0 = t->GetEndTime();
            t1 = 1000000000.0;     // record for a long, long time (tens of years)
         }
         gAudioIO->SetWantLatencyCorrection(!p->m_bWantAppendRecording);
      #endif

      /* TODO: set up stereo tracks if that is how the user has set up
       * their preferences, and choose sample format based on prefs */
      WaveTrackArray newRecordingTracks, playbackTracks;

      bool duplex;
      gPrefs->Read("/AudioIO/Duplex", &duplex, true);
      int recordingChannels = gPrefs->Read("/AudioIO/RecordChannels", 1);

      if( duplex )
         playbackTracks = t->GetWaveTrackArray(false);
      else
         playbackTracks = WaveTrackArray();

      for( int c = 0; c < recordingChannels; c++ )
      {
         WaveTrack *newTrack = p->GetTrackFactory()->NewWaveTrack();
         newTrack->SetOffset(t0);
         newTrack->SetRate(p->GetRate());

         //v for performance     newTrack->SetDisplay(WaveTrack::WaveformAndSpectrumDisplay);
         newTrack->SetDisplay(WaveTrack::WaveformDisplay); 
         
         if( recordingChannels == 2 )
         {
            if( c == 0 )
            {
               newTrack->SetChannel(Track::LeftChannel);
               newTrack->SetLinked(true);
            }
            else
               newTrack->SetChannel(Track::RightChannel);
         }
         else
         {
            newTrack->SetChannel( Track::MonoChannel );
         }

         newRecordingTracks.Add(newTrack);
      }

      int token = gAudioIO->StartStream(playbackTracks,
                                        newRecordingTracks, t->GetTimeTrack(),
                                        p->GetRate(), t0, t1);

      bool success = (token != 0);
      for( unsigned int i = 0; i < newRecordingTracks.GetCount(); i++ )
         if (success)
            t->Add(newRecordingTracks[i]);
         else
            delete newRecordingTracks[i];

      if (success) {
         p->SetAudioIOToken(token);
         mBusyProject = p;
         SetVUMeters(p);
         #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
            this->SetBackgroundColour(*wxRED); // red for Recording
            this->Refresh();
         #endif
      }
      else {
         // msmeyer: Show error message if stream could not be opened
         wxMessageBox(_("Error while opening sound device. Please check the input "
                        "device settings and the project sample rate."),
                      _("Error"), wxOK | wxICON_EXCLAMATION, this);

         SetPlay(false);
         #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
            mStop->Enable(); // In case it was disabled above, based on mIsLocked.
         #endif
         SetStop(false);
         SetRecord(false);
      }

      #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
         p->OnZoomFitV();
      #endif
   }
}


void ControlToolBar::OnPause(wxCommandEvent &evt)
{ 
   if(mPaused)
   {
      mPause->PopUp();
      mPaused=false;
   }
   else
   {       
      mPause->PushDown();
      mPaused=true;
   }
   #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
      if (mPaused)
         this->SetBackgroundColour(wxColour(255, 255, 0)); // yellow for Paused Play
      else if (gAudioIO->GetNumCaptureChannels() > 0) 
         this->SetBackgroundColour(*wxRED); // red for Recording
      else 
         this->SetBackgroundColour(*wxGREEN); // green for Playing
      this->Refresh();
   #endif
 
   gAudioIO->SetPaused(mPaused);
}

void ControlToolBar::OnRewind(wxCommandEvent &evt)
{
   mRewind->PushDown();
   mRewind->PopUp();

   AudacityProject *p = GetActiveProject();
   if (p) {
      p->Rewind(mRewind->WasShiftDown());
   }
}

void ControlToolBar::OnFF(wxCommandEvent &evt)
{
   mFF->PushDown();
   mFF->PopUp();

   AudacityProject *p = GetActiveProject();

   if (p) {
      p->SkipEnd(mFF->WasShiftDown());
   }
}

float ControlToolBar::GetSoundVol()
{
   return 1.0; //return mVolume->Get();
}

bool ControlToolBar::GetSelectToolDown()
{
   return mTool[ selectTool]->IsDown();
}

bool ControlToolBar::GetZoomToolDown()
{
   return mTool[ zoomTool]->IsDown();
}

bool ControlToolBar::GetEnvelopeToolDown()
{
   #if (AUDACITY_BRANDING == BRAND_THINKLABS) // no tools for Thinklabs
      return false;
   #else
      return mTool[ envelopeTool]->IsDown();
   #endif
}

bool ControlToolBar::GetSlideToolDown()
{
   return mTool[ slideTool]->IsDown();
}

bool ControlToolBar::GetDrawToolDown()
{
   #if (AUDACITY_BRANDING == BRAND_THINKLABS) // no tools for Thinklabs
      return false;
   #else
      return mTool[ drawTool ]->IsDown();
   #endif
}

bool ControlToolBar::GetMultiToolDown()
{
   #if (AUDACITY_BRANDING == BRAND_THINKLABS) // no tools for Thinklabs
      return false;
   #else
      return mTool[ multiTool ]->IsDown();
   #endif
}

const char * ControlToolBar::GetMessageForTool( int ToolNumber )
{
   wxASSERT( ToolNumber >= 0 );
   wxASSERT( ToolNumber < numTools );
   return _(MessageOfTool[ ToolNumber ]);
}


void ControlToolBar::OnTool(wxCommandEvent & evt)
{
   mCurrentTool = evt.GetId() - ID_FIRST_TOOL;
   for (int i = 0; i < numTools; i++)
      if (i == mCurrentTool) 
         mTool[i]->PushDown();
      else
         mTool[i]->PopUp();

   RedrawAllProjects();
}

void ControlToolBar::OnPaint(wxPaintEvent & evt)
{
   wxPaintDC dc(this);

   int width, height;
   GetSize(&width, &height);


   #if defined __WXMAC__
      // Mac has an Aqua background...
      DrawBackground(dc, width, height); 
   #else
      if (mShowTools)
      {
         //TODO: Get rid of all the magic numbers used in sizing.
         // On other platforms put the big buttons on a beveled platform.
         DrawBackground(dc, 81, height);
         // Width is reduced by an extra two pixels to visually separate
         // the control toolbar from the next grab bar on the right.
         wxRect bevelRect( 81, 0, width-84, height-1 );
         AColor::Bevel( dc, true, bevelRect );
      }
      else
      {
         DrawBackground(dc, width, height); 
         wxRect bevelRect(0, 0, width - 1, height - 1);
         AColor::Bevel(dc, true, bevelRect);
      }
   #endif

   #ifndef __WXMAC__
      if (mShowTools)
      {
         // JKC: Grey horizontal spacer line between buttons.
         // Not quite ideal, but seems the best solution to 
         // make the tool button heights add up to the 
         // main control button height.
         AColor::Dark( &dc, false);
         dc.DrawLine(0, 27, 81, 27);
      }
   #endif
}

void ControlToolBar::EnableDisableButtons()
{
   AudacityProject *p = GetActiveProject();

   bool tracks = (p && !p->GetTracks()->IsEmpty());
   bool busy = gAudioIO->IsBusy();

#if 0
   if (tracks) {
      if (!busy)
         mPlay->Enable();
   } else mPlay->Disable();
#endif

   //mPlay->SetEnabled(tracks && !busy);
   mPlay->SetEnabled(tracks && !mRecord->IsDown());
   #if (AUDACITY_BRANDING == BRAND_THINKLABS)
      mPlay->SetEnabled(tracks && !mRecord->IsDown() && !mLoopPlay->IsDown());
      mLoopPlay->SetEnabled(tracks && !mRecord->IsDown() && !mPlay->IsDown());
   #endif

   mStop->SetEnabled(busy);
   mRewind->SetEnabled(tracks && !busy);
   mFF->SetEnabled(tracks && !busy);
}
