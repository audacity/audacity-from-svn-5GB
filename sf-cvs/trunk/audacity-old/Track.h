/**********************************************************************

  Audacity: A Digital Audio Editor

  Track.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK__
#define __AUDACITY_TRACK__

class wxString;
class wxTextFile;
class DirManager;

class VTrack {
 public:
   int collapsedHeight;
   int expandedHeight;

   wxString name;
   bool selected;
   bool linked;
   bool mute;
   bool solo;
   int channel;
   double tOffset;
   int dirty;

   DirManager *dirManager;

   enum {
      LeftChannel,
      RightChannel,
      MonoChannel
   };

   enum {
      None,
      Wave,
      Note,
      Label
   } TrackKindEnum;

    VTrack(DirManager * projDirManager);

   virtual ~ VTrack() { }
   
   virtual VTrack *Duplicate();

   virtual void Cut(double t0, double t1, VTrack ** dest) {
      dest = 0;
   }
   virtual void Copy(double t0, double t1, VTrack ** dest) {
      dest = 0;
   }
   virtual void Paste(double t, VTrack * src) {}
   virtual void Clear(double t0, double t1) {}

   virtual void Silence(double t0, double t1) {}
   virtual void InsertSilence(double t, double len) {}

   virtual void SetHeight(int h);

   virtual void Collapse();
   virtual void Expand();
   virtual void Toggle();
   virtual bool IsCollapsed();

   virtual void Offset(double t) {
      tOffset += t;
   }

   virtual int GetKind() {
      return None;
   }

   virtual int GetChannel() {
      return MonoChannel;
   }

   virtual bool Load(wxTextFile * in, DirManager * dirManager);
   virtual bool Save(wxTextFile * out, bool overwrite);

   virtual int GetHeight() {
      return (collapsed ? collapsedHeight : expandedHeight);
   }

   virtual double GetMaxLen() {
      return 0.0;
   }

 private:
   bool collapsed;

};

/*
  TrackList is a flat linked list of tracks supporting Add,
  Remove, Clear, and Contains, plus serialization of the
  list of tracks.
*/

struct TrackListNode {
   VTrack *t;
   TrackListNode *next;
   TrackListNode *prev;
};

class TrackList;

class TrackListIterator {
 public:
   TrackListIterator(TrackList * l);

   // Iterate functions
   VTrack *First();
   VTrack *Next();
   VTrack *RemoveCurrent();     // returns next

 private:
    TrackList * l;
   TrackListNode *cur;
};

class TrackList {
 public:
   // Create an empty TrackList
   TrackList();

   // Copy constructor
   TrackList(TrackList * t);

   // Destructor
   virtual ~TrackList();

   friend class TrackListIterator;

   // Add a this Track or all children of this TrackGroup
   void Add(VTrack * t);

   // Remove this Track or all children of this TrackGroup
   void Remove(VTrack * t);

   // Make the list empty
   void Clear();

   // Select a track, and if it is linked to another track,
   // select it, too.
   void Select(VTrack * t, bool selected = true);

   // If this track is linked to another track (the track
   // immediately before or after it), return its partner.
   // Otherwise return null.
   VTrack *GetLink(VTrack * t);

   VTrack *GetPrev(VTrack * t);
   VTrack *GetNext(VTrack * t);

   bool CanMoveUp(VTrack * t);
   bool CanMoveDown(VTrack * t);

   bool MoveUp(VTrack * t);
   bool MoveDown(VTrack * t);

   // Test
   bool Contains(VTrack * t);

   bool IsEmpty();

   double GetMaxLen();
   int GetHeight();

   // File I/O
   virtual bool Load(wxTextFile * in, DirManager * dirManager);
   virtual bool Save(wxTextFile * out, bool overwrite);

 private:
   void Swap(TrackListNode * s1, TrackListNode * s2);

   TrackListNode *head;
   TrackListNode *tail;
};

#endif
