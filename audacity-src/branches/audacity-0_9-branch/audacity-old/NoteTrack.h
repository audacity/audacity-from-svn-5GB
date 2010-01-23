/**********************************************************************

  Audacity: A Digital Audio Editor

  NoteTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_NOTETRACK__
#define __AUDACITY_NOTETRACK__

#include <wx/string.h>

#include "Track.h"

#include "allegro/allegro.h"

class DirManager;

class NoteTrack:public VTrack {
 public:
   int mBottomNote;

   friend class TrackArtist;

   NoteTrack(DirManager * projDirManager);

   virtual int GetKind() {
      return Note;
   } virtual double GetMaxLen();

   void DrawLabelControls(wxDC & dc, wxRect & r);
   bool LabelClick(wxRect & r, int x, int y, bool right);

   void SetSequence(Seq_ptr seq);

 private:
   Seq_ptr mSeq;
   double mLen;

   int mVisibleChannels;

   void CalcLen();
};

#endif
