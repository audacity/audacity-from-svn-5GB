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
   int collapsedHeight;
   int expandedHeight;

   wxString name;
   bool selected;
   bool linked;
   bool mute;
   bool solo;
   bool collapsed;
   int channel;
   double tOffset;
   int dirty;

   mutable DirManager *dirManager;

 public:

   enum {
      LeftChannel = 0,
      RightChannel = 1,
      MonoChannel = 2
   };

   enum {
      None,
      Wave,
      Note,
      Label
   } TrackKindEnum;

   VTrack(DirManager * projDirManager);

   virtual ~ VTrack() { }
   
   virtual VTrack *Duplicate() const;

   int GetCollapsedHeight() const { return collapsedHeight; }
   int GetExpandedHeight () const { return expandedHeight;  }

   void SetCollapsedHeight( int h ) { collapsedHeight = h; }
   void SetExpandedHeight ( int h ) { expandedHeight  = h; }

   wxString GetName() const { return name; }
   void SetName( wxString n ) { name = n; }

   bool GetSelected() const { return selected; }
   bool GetMute    () const { return mute;     }
   bool GetLinked  () const { return linked;   }
   bool GetSolo    () const { return solo;     }

   void SetSelected(bool s) { selected = s; }
   void SetMute    (bool m) { mute     = m; }
   void SetLinked  (bool l) { linked   = l; }
   void SetSolo    (bool s) { solo     = s; }

   int    GetChannel() const { return channel; }
   double GetOffset () const { return tOffset; }
   int    GetDirty  () const { return dirty;   }

   void SetDirty  (int    d) { dirty   = d; }
   void SetOffset (double o) { tOffset = o; }
   void SetChannel(int    c) { channel = c; }

   // AS: Note that the dirManager is mutable.  This is
   // mostly to support "Duplicate" of const objects,
   // but in general, mucking with the dir manager is
   // seperate from the Track.
   DirManager* GetDirManager() const { return dirManager; }

   virtual void Cut  (double t0, double t1, VTrack ** dest) { dest = 0; }
   virtual void Copy (double t0, double t1, VTrack ** dest) const { dest = 0; }
   virtual void Clear(double t0, double t1) {}
   virtual void Paste(double t, const VTrack * src) {}

   virtual void Silence(double t0, double t1) {}
   virtual void InsertSilence(double t, double len) {}

   virtual void SetHeight(int h);

   virtual void Collapse();
   virtual void Expand();
   virtual void Toggle();
   virtual bool IsCollapsed() const;

   virtual void Offset(double t) {tOffset += t; }
   virtual int GetKind() const { return None; }

   virtual bool Load(wxTextFile * in, DirManager * dirManager);
   virtual bool Save(wxTextFile * out, bool overwrite);

   virtual int GetHeight() const {
      return (collapsed ? collapsedHeight : expandedHeight);
   }

   virtual double GetMaxLen() const { return 0.0; }
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
  TrackListIterator(TrackList * val);
  
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
  friend class ConstTrackListIterator;
  
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
  VTrack *GetLink(VTrack * t) const;
  
  VTrack *GetPrev(VTrack * t) const;
  VTrack *GetNext(VTrack * t) const;
  
  bool CanMoveUp(VTrack * t) const;
  bool CanMoveDown(VTrack * t) const;
  
  bool MoveUp(VTrack * t);
  bool MoveDown(VTrack * t);
  bool Move(VTrack * t, bool up) { return up ? MoveUp(t) : MoveDown(t); }
  
  // Test
  bool Contains(VTrack * t) const;
  
  bool IsEmpty() const;
  
  double GetMaxLen() const;
  int GetHeight() const;
  
  // File I/O
  virtual bool Load(wxTextFile * in, DirManager * dirManager);
  virtual bool Save(wxTextFile * out, bool overwrite);
  
 private:
  void Swap(TrackListNode * s1, TrackListNode * s2);
  
  TrackListNode *head;
  TrackListNode *tail;
};

class ConstTrackListIterator {
 public:
  ConstTrackListIterator(const TrackList * val) : l(val), cur(NULL) {}
  
  // Iterate functions
  VTrack *First() const
    {
      cur = l->head;
      
      if (cur)
	return cur->t;
      else
	return NULL;
    }
  
  VTrack *Next() const
    {
      if (cur)
	cur = cur->next;
      
      if (cur)
	return cur->t;
      else
	return NULL;
    }
  
  
 private:
  const TrackList * l;
  mutable TrackListNode *cur;
};

#endif
