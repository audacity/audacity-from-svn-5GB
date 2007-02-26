/**********************************************************************

  Audacity: A Digital Audio Editor

  LyricsWindow.h

  Vaughan Johnson
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LYRICS_WINDOW__
#define __AUDACITY_LYRICS_WINDOW__

#include <wx/frame.h>

class AudacityProject;
class Lyrics;

class LyricsWindow : public wxFrame {

 public:
   LyricsWindow(AudacityProject* parent);
   ~LyricsWindow();

   Lyrics *GetLyricsPanel() {
     return mLyricsPanel;
   }

 private:
   void OnCloseWindow(wxCloseEvent & WXUNUSED(event));

   AudacityProject *mProject;
   Lyrics *mLyricsPanel;

 public:
   DECLARE_EVENT_TABLE()
};

#endif
