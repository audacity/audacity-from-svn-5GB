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
#include <wx/html/htmlwin.h>

#define LYRICS_DEFAULT_WIDTH 440
#define LYRICS_DEFAULT_HEIGHT 330

struct Syllable {
   double t;
   wxString text;
   wxString textWithSpace;
   int char0;
   int char1;
   int width;
   int leftX;
   int x;
};

WX_DECLARE_OBJARRAY(Syllable, SyllableArray);

class LinkingHtmlWindow : public wxHtmlWindow 
{
 public:
   LinkingHtmlWindow(wxWindow *parent) : wxHtmlWindow(parent) {};
   virtual void OnLinkClicked(const wxHtmlLinkInfo& link);
};

class Lyrics : public wxPanel
{
   DECLARE_DYNAMIC_CLASS(Lyrics)

 public:
   Lyrics(wxWindow* parent, wxWindowID id,
          const wxPoint& pos = wxDefaultPosition,
          const wxSize& size = wxDefaultSize);

   ~Lyrics();

   void Clear();
   void Add(double t, wxString syllable);
   void Finish(double finalT);

   void Update(double t, bool bForce = false);

   //
   // Event handlers
   //
   void OnKeyEvent(wxKeyEvent & event);
   void OnPaint(wxPaintEvent &evt);
   void OnSize(wxSizeEvent &evt);
   void OnMouse(wxMouseEvent &evt);

   void HandlePaint(wxDC &dc);
   void HandleLayout();

private:
   void SetFont(wxDC *dc);
   void Measure(wxDC *dc);
   int FindSyllable(double t);
   void GetKaraokePosition(double t,
                           int *outX, double *outY);

   int            mWidth;
   int            mHeight;

   double         mT;

   int            mKaraokeHeight;

   int            mCurrentSyllable;
   SyllableArray  mSyllables;
   wxString       mText;

   int            mTextHeight;

   LinkingHtmlWindow   *mHtml;

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



