/**********************************************************************

  Audacity: A Digital Audio Editor

  LyricsWindow.cpp

  Vaughan Johnson
  Dominic Mazzoni

**********************************************************************/

#include "LyricsWindow.h"
#include "Lyrics.h"
#include "Project.h"

#include "../images/AudacityLogo.xpm"
#include "../images/AudacityLogo48x48.xpm"

#ifdef __WXMAC__
#include <Carbon/Carbon.h>
#endif

enum {
   kMenuID_BouncingBall = 10101,
   kMenuID_Highlight,
};

BEGIN_EVENT_TABLE(LyricsWindow, wxFrame)
   EVT_CLOSE(LyricsWindow::OnCloseWindow)
   EVT_MENU(kMenuID_BouncingBall, LyricsWindow::OnStyle_BouncingBall)
   EVT_MENU(kMenuID_Highlight, LyricsWindow::OnStyle_Highlight)
END_EVENT_TABLE()

const wxSize gSize = wxSize(LYRICS_DEFAULT_WIDTH, LYRICS_DEFAULT_HEIGHT);

LyricsWindow::LyricsWindow(AudacityProject *parent):
   wxFrame(parent, -1, 
            wxString::Format(_("Audacity Lyrics%s"), 
                              ((parent->GetName() == wxEmptyString) ? 
                                 wxT("") :
                                 wxString::Format(
                                   wxT(" - %s"),
                                   parent->GetName().c_str()).c_str())),
            wxPoint(100, 300), gSize, 
            wxDEFAULT_FRAME_STYLE
#ifndef __WXMAC__
           | ((parent == NULL) ? 0x0 : wxFRAME_FLOAT_ON_PARENT)
#endif
             )
{
#ifdef __WXMAC__
   // WXMAC doesn't support wxFRAME_FLOAT_ON_PARENT, so we do
   SetWindowClass((WindowRef) MacGetWindowRef(), kFloatingWindowClass);
#endif
   mProject = parent;
   mLyricsPanel = new Lyrics(this, -1, wxPoint(0, 0), gSize);

   // loads either the XPM or the windows resource, depending on the platform
#if !defined(__WXMAC__) && !defined(__WXX11__)
   #ifdef __WXMSW__
      wxIcon ic(wxICON(AudacityLogo));
   #else
      wxIcon ic(wxICON(AudacityLogo48x48));
   #endif
   SetIcon(ic);
#endif

   wxMenuBar* pMenuBar = new wxMenuBar();
   this->SetMenuBar(pMenuBar);
   mStyleMenu = new wxMenu();
   pMenuBar->Append(mStyleMenu, _("Style"));
   #ifdef wxHAS_RADIO_MENU_ITEMS 
      mStyleMenu->AppendRadioItem(kMenuID_BouncingBall, _("Bouncing Ball"));
      mStyleMenu->AppendRadioItem(kMenuID_Highlight, _("Highlight"));
   #else
      mStyleMenu->AppendCheckItem(kMenuID_BouncingBall, _("Bouncing Ball"));
      mStyleMenu->AppendCheckItem(kMenuID_Highlight, _("Highlight"));
   #endif
   Lyrics::LyricsStyle currStyle = mLyricsPanel->GetLyricsStyle();
   mStyleMenu->Check(
      (currStyle == Lyrics::kBouncingBallLyrics) ? kMenuID_BouncingBall : kMenuID_Highlight, 
      true);
}

LyricsWindow::~LyricsWindow()
{}

void LyricsWindow::OnCloseWindow(wxCloseEvent & WXUNUSED(event))
{
  this->Hide();
}

void LyricsWindow::OnStyle_BouncingBall(wxCommandEvent &evt)
{
   mLyricsPanel->SetLyricsStyle(Lyrics::kBouncingBallLyrics);
   #ifndef wxHAS_RADIO_MENU_ITEMS 
      mStyleMenu->Check(kMenuID_Highlight, false);
   #endif
}

void LyricsWindow::OnStyle_Highlight(wxCommandEvent &evt)
{
   mLyricsPanel->SetLyricsStyle(Lyrics::kHighlightLyrics);
   #ifndef wxHAS_RADIO_MENU_ITEMS 
      mStyleMenu->Check(kMenuID_BouncingBall, false);
   #endif
}

