/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef _LABELTRACK_
#define _LABELTRACK_

#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/dynarray.h>
#include <wx/string.h>

#include "Track.h"

class wxKeyEvent;
class wxTextFile;
class wxWindow;
class wxIcon;
class TrackList;

class AudacityProject;
class DirManager;

class LabelStruct 
{
public:
   void DrawLines( wxDC & dc, wxRect & r);
   void DrawGlyphs( wxDC & dc, wxRect & r);
   void DrawText( wxDC & dc, wxRect & r);
   
public:
   double t;
   double t1;
   wxString title;
   int width;
// Working storage for on-screen layout.
   int x;
   int x1;
   int xText;
   int y;

};

WX_DEFINE_ARRAY(LabelStruct *, LabelArray);

const int NUM_GLYPH_CONFIGS = 3;
const int NUM_GLYPH_HIGHLIGHTS = 4;
const int MAX_NUM_ROWS =80;

class LabelTrack:public Track {
   friend class LabelStruct;
   friend class BouncePane;
   friend bool ExportPCM(AudacityProject *project,
               wxString format, bool stereo, wxString fName,
               bool selectionOnly, double t0, double t1);

 public:
	 void CreateCustomGlyphs();
   LabelTrack(DirManager * projDirManager);
   LabelTrack(const LabelTrack &orig);

   virtual ~ LabelTrack();

   void Draw(wxDC & dc, wxRect & r, double h, double pps,
             double sel0, double sel1);

   virtual int GetKind() const { return Label; } 

   virtual double GetStartTime();
   virtual double GetEndTime();

   virtual Track *Duplicate() { return new LabelTrack(*this); }

   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual XMLTagHandler *HandleXMLChild(const char *tag);
   virtual void WriteXML(int depth, FILE *fp);

#if LEGACY_PROJECT_FILE_SUPPORT
   virtual bool Load(wxTextFile * in, DirManager * dirManager);
   virtual bool Save(wxTextFile * out, bool overwrite);
#endif

   virtual bool Cut  (double t0, double t1, Track ** dest);
   // JKC Do not add the const modifier to Copy(), because then it 
   // is no longer recognised as a virtual function matching the 
   // one in Track.
   virtual bool Copy (double t0, double t1, Track ** dest);// const;
   virtual bool Clear(double t0, double t1);
   virtual bool Paste(double t, const Track * src);

   virtual bool Silence(double t0, double t1);
   virtual bool InsertSilence(double t, double len);
   int OverGlyph(int x, int y);
   void HandleMouse(const wxMouseEvent & evt, wxRect & r, double h, double pps,
                           double *newSel0, double *newSel1);

   void KeyEvent(double sel0, double sel1, wxKeyEvent & event);

   void Import(wxTextFile & f);
   void Export(wxTextFile & f);

   void Unselect();

   bool IsSelected() const;

   int GetNumLabels() const;
   const LabelStruct *GetLabel(int index) const;

   //This returns the index of the label we just added.
   int AddLabel(double t, double t1, const wxString &title = "");

 private:

   int mSelIndex;           //Keeps track of the currently selected label
   int mMouseOverLabel;     //Keeps track of which label the mouse is currently over.
   LabelArray mLabels;

   wxBrush mUnselectedBrush;
   wxBrush mSelectedBrush;
   wxBrush mTextEditBrush;

   wxPen mLabelSurroundPen;
   wxPen mUnselectedPen;
   wxPen mSelectedPen;

   static int mIconHeight;
   static int mIconWidth;
   static int mTextHeight;
   static bool mbGlyphsReady;
   static wxIcon mBoundaryGlyphs[ NUM_GLYPH_CONFIGS * NUM_GLYPH_HIGHLIGHTS];

   int xUsed[MAX_NUM_ROWS];
   // Used only for a LabelTrack on the clipboard
   double mClipLen;

   void InitColours();
   void ComputeLayout(wxRect & r, double h, double pps);
   void ComputeTextPosition(wxRect & r, int index);

   bool mIsAdjustingLabel;
   int mAdjustingEdge;
      
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
// arch-tag: fa157f82-b858-406d-9d4b-120d89410f11

