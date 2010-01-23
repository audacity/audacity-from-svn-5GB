/**********************************************************************

  Audacity: A Digital Audio Editor

  NoteTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_NOTETRACK__
#define __AUDACITY_NOTETRACK__

#include <wx/string.h>

#include "Track.h"

class Seq;   // from "allegro.h"

class DirManager;

class NoteTrack:public VTrack {
 public:
   friend class TrackArtist;

   NoteTrack(DirManager * projDirManager);

   virtual int GetKind() const { return Note; } 
   virtual double GetMaxLen() const { return mLen; }

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

 private:
   Seq *mSeq;
   double mLen;

   int mBottomNote;

   int mVisibleChannels;

   void CalcLen();
};

#endif
