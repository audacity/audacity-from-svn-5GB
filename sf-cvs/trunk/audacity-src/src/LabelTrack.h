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
class TrackList;

class AudacityProject;
class DirManager;

struct LabelStruct {
   double t;
   wxString title;
   int width;
};

WX_DEFINE_ARRAY(LabelStruct *, LabelArray);

class LabelTrack:public VTrack {
   friend class BouncePane;
   friend bool ExportPCM(AudacityProject *project,
               wxString format, bool stereo, wxString fName,
               bool selectionOnly, double t0, double t1);

 public:
   LabelTrack(DirManager * projDirManager);
   LabelTrack(const LabelTrack &orig);

   virtual ~ LabelTrack();

   void Draw(wxDC & dc, wxRect & r, double h, double pps,
             double sel0, double sel1);

   virtual int GetKind() const { return Label; } 
   virtual double GetMaxLen() const;

   virtual VTrack *Duplicate() const { return new LabelTrack(*this); }

   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual XMLTagHandler *HandleXMLChild(const char *tag);
   virtual void WriteXML(int depth, FILE *fp);

#if LEGACY_PROJECT_FILE_SUPPORT
   virtual bool Load(wxTextFile * in, DirManager * dirManager);
   virtual bool Save(wxTextFile * out, bool overwrite);
#endif

   virtual void Cut  (double t0, double t1, VTrack ** dest);
   virtual void Copy (double t0, double t1, VTrack ** dest) const;
   virtual void Clear(double t0, double t1);
   virtual void Paste(double t, const VTrack * src);

   virtual void Silence(double t0, double t1);
   virtual void InsertSilence(double t, double len);

   void MouseDown(int x, int y, wxRect & r, double h, double pps);

   void KeyEvent(double sel0, double sel1, wxKeyEvent & event);

   void Import(wxTextFile & f);
   void Export(wxTextFile & f);

   void Unselect();

   bool IsSelected() const;

   int GetNumLabels() const;
   const LabelStruct *GetLabel(int index) const;

 private:

   int mSelIndex;

   LabelArray mLabels;

   wxBrush mFlagBrush;
   wxBrush mUnselectedBrush;
   wxBrush mSelectedBrush;

   wxPen mFlagPen;
   wxPen mUnselectedPen;
   wxPen mSelectedPen;

   // Used only for a LabelTrack on the clipboard
   double mClipLen;

   void InitColours();
};

#endif
