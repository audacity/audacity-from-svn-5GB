/**********************************************************************

  Audacity: A Digital Audio Editor

  NoteTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_NOTETRACK__
#define __AUDACITY_NOTETRACK__

#include <wx/string.h>

#include "Track.h"

class wxDC;
class wxRect;

class DirManager;
class Seq;   // from "allegro.h"


class NoteTrack:public Track {
 public:
   friend class TrackArtist;

   NoteTrack(DirManager * projDirManager);

   virtual Track *Duplicate();
   
   virtual int GetKind() const { return Note; } 

   virtual double GetStartTime() { return 0.0; }
   virtual double GetEndTime() { return mLen; }

   void DrawLabelControls(wxDC & dc, wxRect & r);
   bool LabelClick(wxRect & r, int x, int y, bool right);

   void SetSequence(Seq *seq);

   int GetBottomNote() const { return mBottomNote; }
   void SetBottomNote(int note) 
   { 
      if (note < 0)
         note = 0;
      else if (note > 96)
         note = 96;

      mBottomNote = note; 
   }

   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);
   virtual void WriteXML(XMLWriter &xmlFile);

 private:
   Seq *mSeq;
   double mLen;

   DirManager *mDirManager;

   int mBottomNote;

   int mVisibleChannels;

   void CalcLen();
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
// arch-tag: 214ce825-eb40-416f-9312-84652d6025d1

