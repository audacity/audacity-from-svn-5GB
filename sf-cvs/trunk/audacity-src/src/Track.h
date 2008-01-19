/**********************************************************************

  Audacity: A Digital Audio Editor

  Track.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK__
#define __AUDACITY_TRACK__

#include "Audacity.h"
#include <wx/string.h>
#include <wx/dynarray.h>
#include <wx/longlong.h>

#include "SampleFormat.h"
#include "xml/XMLTagHandler.h"
#include "Experimental.h"
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
#include <wx/gdicmn.h>
#endif //EXPERIMENTAL_RULER_AUTOSIZE

class wxTextFile;
class DirManager;
class UndoStack;
class TimeTrack;
class WaveTrack;
#ifdef __WXMSW__
#pragma warning(disable:4284)
#endif
WX_DEFINE_ARRAY(WaveTrack*, WaveTrackArray);

class AUDACITY_DLL_API Track: public XMLTagHandler {

 // To be TrackDisplay
 protected:
   int        mHeight;
   wxString   mName;
   wxString   mDefaultName;

   bool       mSelected;

   bool       mLinked;
   bool       mTeamed;
   bool       mMinimized;

 public:
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
   wxSize vrulerSize;
#endif //EXPERIMENTAL_RULER_AUTOSIZE
 
   // This just returns a constant and can be overriden by subclasses
   // to specify a different height for the case that the track is minimized.
   virtual int GetMinimizedHeight() const;
 
   int GetHeight() const;
   void SetHeight( int h ) { mHeight = h; }
   bool GetMinimized() const { return mMinimized; }
   void SetMinimized(bool isMinimized) { mMinimized = isMinimized; }

 // Keep in Track

 protected:
   int                 mChannel;
   double              mOffset;
   bool                mMute;
   bool                mSolo;

   mutable DirManager *mDirManager;

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
      Label,
      Time
   } TrackKindEnum;

   Track(DirManager * projDirManager);
   Track(const Track &orig);

   virtual ~ Track();
   
   void Init(const Track &orig);
   virtual Track *Duplicate() = 0;

   // Called when this track is merged with another, and should
   // take on some paramaters of its partner.
   virtual void Merge(const Track &orig);

   wxString GetName() const { return mName; }
   void SetName( wxString n ) { mName = n; }
   wxString GetDefaultName() const { return mDefaultName; }
   void SetDefaultName( wxString n ) { mDefaultName = n; }

   bool GetSelected() const { return mSelected; }
   bool GetMute    () const { return mMute;     }
   bool GetLinked  () const { return mLinked;   }
   bool GetTeamed  () const { return mTeamed;   }
   bool GetSolo    () const { return mSolo;     }

   void SetSelected(bool s) { mSelected = s; }
   void SetMute    (bool m) { mMute     = m; }
   void SetLinked  (bool l) { mLinked   = l; mTeamed = l; }
   void SetTeamed  (bool l) { mTeamed   = l; mLinked = false; }
   void SetSolo    (bool s) { mSolo     = s; }

   int    GetChannel() const { return mChannel; }
   virtual double GetOffset () { return mOffset; }

   void Offset(double t) { SetOffset(GetOffset() + t); }
   virtual void SetOffset (double o) { mOffset = o; }

   void SetChannel(int    c) { mChannel = c; }

   // AS: Note that the dirManager is mutable.  This is
   // mostly to support "Duplicate" of const objects,
   // but in general, mucking with the dir manager is
   // separate from the Track.
   DirManager* GetDirManager() const { return mDirManager; }

   virtual bool Cut  (double t0, double t1, Track ** dest) {return false;}
   virtual bool Copy (double t0, double t1, Track ** dest) {return false;}
   virtual bool Clear(double t0, double t1) {return false;}
   virtual bool Paste(double t, Track * src) {return false;}

   virtual bool Silence(double t0, double t1) {return false;}
   virtual bool InsertSilence(double t, double len) {return false;}

   virtual int GetKind() const { return None; }

   // XMLTagHandler callback methods

   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) = 0;
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag) = 0;
   virtual void WriteXML(XMLWriter &xmlFile) = 0;

   // Returns true if an error was encountered while trying to
   // open the track from XML
   virtual bool GetErrorOpening() { return false; }

   virtual double GetStartTime() { return 0.0; }
   virtual double GetEndTime() { return 0.0; }
};

struct TrackListNode {
   Track *t;
   TrackListNode *next;
   TrackListNode *prev;
};

class TrackList;

class AUDACITY_DLL_API TrackListIterator {
 public:
  TrackListIterator(TrackList * val = NULL);
  
  // Iterate functions
  Track *First(TrackList * val = NULL);
  Track *Next(bool SkipLinked = false);
  Track *RemoveCurrent();     // returns next

  Track *Last();
  
 private:
  TrackList * l;
  TrackListNode *cur;
};

/** \brief TrackList is a flat linked list of tracks supporting Add,  Remove,
 * Clear, and Contains, plus serialization of the list of tracks.
 */
class AUDACITY_DLL_API TrackList {
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
  void Add(Track * t);
  void AddToHead(Track * t);
  
  // Remove this Track or all children of this TrackGroup
  void Remove(Track * t);
  
  // Make the list empty
  void Clear(bool deleteTracks = false);
  
  // Select a track, and if it is linked to another track,
  // select it, too.
  void Select(Track * t, bool selected = true);
  
  // If this track is linked to another track (the track
  // immediately before or after it), return its partner.
  // Otherwise return null.
  Track *GetLink(Track * t) const;
  
  Track *GetPrev(Track * t, bool linked = false) const;
  Track *GetNext(Track * t, bool linked = false) const;
  int GetGroupHeight( Track*t ) const;
  
  bool CanMoveUp(Track * t) const;
  bool CanMoveDown(Track * t) const;
  
  bool MoveUp(Track * t);
  bool MoveDown(Track * t);
  bool Move(Track * t, bool up) { return up ? MoveUp(t) : MoveDown(t); }

  TimeTrack *GetTimeTrack();

  /** \brief Find out how many channels this track list mixes to
   *
   * This is used in exports of the tracks to work out whether to export in 
   * Mono, Stereo etc. @param selectionOnly Whether to consider the entire track
   * list or only the selected members of it
   */
  int GetNumExportChannels(bool selectionOnly);

  WaveTrackArray GetWaveTrackArray(bool selectionOnly);
  /* Consider this function depricated in favor of the above function */
  void GetWaveTracks(bool selectionOnly, int *num, WaveTrack ***tracks);
  
  // Test
  bool Contains(Track * t) const;
  
  bool IsEmpty() const;
  
  double GetStartTime() const;
  double GetEndTime() const;

  double GetMinOffset() const;
  int GetHeight() const;

#if LEGACY_PROJECT_FILE_SUPPORT  
  // File I/O
  virtual bool Load(wxTextFile * in, DirManager * dirManager);
  virtual bool Save(wxTextFile * out, bool overwrite);
#endif
  
  wxLongLong GetSpaceUsage();
  wxLongLong GetAdditionalSpaceUsage(UndoStack *stack);
 private:
  void Swap(TrackListNode * s1, TrackListNode * s2);
  
  TrackListNode *head;
  TrackListNode *tail;
};

class AUDACITY_DLL_API ConstTrackListIterator {
 public:
    ConstTrackListIterator(const TrackList * val) : l(val), cur(NULL) {}

    // Iterate functions
    Track *First() const
    {
       cur = l->head;

       if (cur) return cur->t;
       return NULL;
    }

    Track *Next() const
    {
       if (cur) cur = cur->next;
       if (cur) return cur->t;
       return NULL;
    }

 private:
    const TrackList * l;
    mutable TrackListNode *cur;
};


class WaveTrack;
class NoteTrack;
class LabelTrack;
class TimeTrack;
class DirManager;

class AUDACITY_DLL_API TrackFactory
{
 private:
   TrackFactory(DirManager *dirManager):
      mDirManager(dirManager)
   {
   }

   DirManager *mDirManager;
   friend class AudacityProject;
   friend class BenchmarkDialog;

 public:
   // These methods are defined in WaveTrack.cpp, NoteTrack.cpp,
   // LabelTrack.cpp, and TimeTrack.cpp respectively
   WaveTrack* DuplicateWaveTrack(WaveTrack &orig);
   WaveTrack *NewWaveTrack(sampleFormat format = (sampleFormat)0,
                           double rate = 0);
   NoteTrack *NewNoteTrack();
   LabelTrack *NewLabelTrack();
   TimeTrack *NewTimeTrack();
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
// arch-tag: 2214d773-8e6c-4117-a03d-36c9722ace52

