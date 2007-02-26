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

//enum {
//   LyricsWindowCtlID = 1000,
//   FontSizeBtnID
//};

BEGIN_EVENT_TABLE(LyricsWindow, wxFrame)
   EVT_CLOSE(LyricsWindow::OnCloseWindow)
   //EVT_BUTTON(FontSizeBtnID, LyricsWindow::OnFontSizeBtn)
END_EVENT_TABLE()

const wxSize gSize = wxSize(LYRICS_DEFAULT_WIDTH, LYRICS_DEFAULT_HEIGHT);

LyricsWindow::LyricsWindow(AudacityProject *parent):
   wxFrame(parent, -1, 
            wxString::Format(_("Audacity Lyrics%s"), 
                              ((parent->GetName() == wxEmptyString) ? 
                                 wxT("") :
                                 wxString::Format(
                                   wxT("- %s"),
                                   parent->GetName().c_str()).c_str())),
            wxDefaultPosition, gSize, 
            wxDEFAULT_FRAME_STYLE | ((parent == NULL) ?
                                     0x0 :
                                     wxFRAME_FLOAT_ON_PARENT))
{
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
}

LyricsWindow::~LyricsWindow()
{}

void LyricsWindow::OnCloseWindow(wxCloseEvent & WXUNUSED(event))
{
  this->Hide();
}

//void LyricsWindow::OnFontSizeBtn(wxCommandEvent & event)
//{
//   // this->UpdateDisplay();
//}

