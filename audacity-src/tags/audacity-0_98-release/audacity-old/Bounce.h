/**********************************************************************

  Audacity: A Digital Audio Editor

  Bounce.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_BOUNCE__
#define __AUDACITY_BOUNCE__

#include <wx/brush.h>
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/gdicmn.h>
#include <wx/pen.h>
#include <wx/minifram.h>

class Bounce;
class AudacityProject;
class LabelArray;

struct BounceLabel {
   double t;
   wxString title;
   int spacing;
};

WX_DEFINE_ARRAY(BounceLabel *, BounceLabelArray);

class BouncePane:public wxWindow {
 public:
   BouncePane(wxWindow * parent, wxWindowID id,
              const wxPoint & pos, const wxSize & size);

   ~BouncePane();

   void OnMouseEvent(wxMouseEvent & event);
   void OnPaint(wxPaintEvent & event);

   void Resize();

   void SetProject(AudacityProject * project);
   void SetTime(double t);

 private:

   void CreateLabels(LabelArray * l);
   void DestroyLabels();

   AudacityProject *project;
   double t;

   BounceLabelArray *labels;

   wxBrush mBackgroundBrush;
   wxPen mBackgroundPen;

   wxRect mBounds;
   wxBitmap *mBitmap;

    DECLARE_EVENT_TABLE()
};

class Bounce:public wxFrame {
 public:
   Bounce(wxWindow * parent, wxWindowID id,
          const wxString & title, const wxPoint & pos);

   virtual ~ Bounce();

   void OnPaint(wxPaintEvent & event);

   void OnCloseWindow(wxCloseEvent & event);
   void OnSize(wxSizeEvent & event);

   void SetProject(AudacityProject * project);
   void SetTime(double t);

 private:

    BouncePane * mBouncePane;

    DECLARE_EVENT_TABLE()
};

#endif
