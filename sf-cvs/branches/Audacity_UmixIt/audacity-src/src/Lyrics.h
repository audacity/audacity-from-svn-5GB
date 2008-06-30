/**********************************************************************

  Audacity: A Digital Audio Editor

  Lyrics.h

  Dominic Mazzoni
  Vaughan Johnson
  
**********************************************************************/

#ifndef __AUDACITY_LYRICS__
#define __AUDACITY_LYRICS__

#include "Audacity.h"

#include <wx/dynarray.h>
#include <wx/panel.h>
#include <wx/scrolwin.h> //vvvvv
#include <wx/textctrl.h> //vvvvv


// Branding removed from Lyrics window.   #include "widgets/LinkingHtmlWindow.h"

#define LYRICS_DEFAULT_WIDTH 608
#define LYRICS_DEFAULT_HEIGHT 280

struct Syllable {
   double t;
   wxString text;
   wxString textWithSpace;
   int char0; // index of first char of syllable in Lyrics::mText
   int char1; // index of last  char of syllable in Lyrics::mText
   int width;
   int leftX;
   int x; // centerX, used only for kBouncingBallLyrics

   int height; // used only for kHighlightLyrics
   int topY;   // used only for kHighlightLyrics
};

WX_DECLARE_OBJARRAY(Syllable, SyllableArray);

// Lyrics stated as a wxPanel, but easier to implement scrolling for kHighlightLyrics
// without subclassing wxScrolledWindow. 
// Note that if Branding is reinstated, it will be a scrolled window inside a scrolled window, 
// but it seems unlikely it will be reinstated.
class Lyrics : public wxPanel //vvvvv wxScrolledWindow 
{
   DECLARE_DYNAMIC_CLASS(Lyrics)

   enum LyricsStyle {
      kBouncingBallLyrics, // Lyrics move from right to left with bouncing ball.
      kHighlightLyrics,    // Lyrics show in scrolling page and syllables highlight successively.
   };

 public:
   Lyrics(wxWindow* parent, wxWindowID id,
          const wxPoint& pos = wxDefaultPosition,
          const wxSize& size = wxDefaultSize);

   ~Lyrics();

   void Clear();
   void Add(double t, wxString syllable);
   void Finish(double finalT);

   LyricsStyle GetLyricsStyle() { return mLyricsStyle; };
   void SetLyricsStyle(const LyricsStyle newLyricsStyle);

   void Update(double t, bool bForce = false);

   //
   // Event handlers
   //
   void OnKeyEvent(wxKeyEvent & event);
   void OnPaint(wxPaintEvent &evt);
   void OnSize(wxSizeEvent &evt);
   void OnMouse(wxMouseEvent &evt);

   void HandlePaint(wxDC &dc);
   void HandlePaint_BouncingBall(wxDC &dc);
   void HandlePaint_Highlight(wxDC &dc);

   void HandleLayout();

private:
   unsigned int GetDefaultFontSize() const; // Depends on mLyricsStyle. Call only after mLyricsStyle is set.
   void SetFont(wxDC *dc);
   void Measure(wxDC *dc);
   int FindSyllable(double t);
   void GetKaraokePosition(double t,
                           int *outX, double *outY);

private:
   int            mWidth;  // client width
   int            mHeight; // client height

   int            mKaraokeHeight; // mHeight - mBrandingHeight (so just mHeight now that Branding is removed).
   unsigned int   mKaraokeFontSize;

   // Branding removed from bottom of Lyrics window.   
   //   int            mBrandingHeight; 
   //   LinkingHtmlWindow* mBrandingPanel;

   LyricsStyle    mLyricsStyle; // default kHighlightLyrics
   wxTextCtrl*    mTextCtrl; // only for kHighlightLyrics

   double         mT;

   int            mCurrentSyllable;
   SyllableArray  mSyllables;
   wxString       mText; 

   int            mTextHeight;


   bool           mMeasurementsDone;

   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_LYRICS__

// Indentation settings for Vim and Emacs.
// Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// 



