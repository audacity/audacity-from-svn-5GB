/**********************************************************************

  Audacity: A Digital Audio Editor

  Track.cpp

  Dominic Mazzoni

*******************************************************************//**

\class Track
\brief Fundamental data object of Audacity, placed in the TrackPanel.
Classes derived form it include the WaveTrack, NoteTrack, LabelTrack 
and TimeTrack.

*//****************************************************************//**

\class ConstTrackListIterator
\brief ConstTrackListIterator provides an iterator to iterate through 
a TrackList.

*//*******************************************************************/

#include <float.h>
#include <wx/file.h>
#include <wx/textfile.h>
#include <wx/log.h>

#include "Track.h"
#include "WaveTrack.h"
#include "NoteTrack.h"
#include "LabelTrack.h"
#include "TimeTrack.h"
#include "DirManager.h"

#ifdef _MSC_VER
//Disable truncation warnings
#pragma warning( disable : 4786 )
#endif

Track::Track(DirManager * projDirManager) 
   : 
   mDirManager(projDirManager)
#ifdef EXPERIMENTAL_RULER_AUTOSIZE
   ,
   vrulerSize(36,0)
#endif //EXPERIMENTAL_RULER_AUTOSIZE
{
   mDirManager->Ref();

   mSelected  = false;
   mLinked    = false;
   mTeamed    = false;
   mMute      = false;
   mSolo      = false;

   //mHeight = 136;
   mHeight = 150;
   
   mMinimized = false;

   mOffset = 0.0;

   mChannel = MonoChannel;
}

Track::Track(const Track &orig)
{
   mDirManager = NULL;

   Init(orig);
   mOffset = orig.mOffset;
}

// Copy all the track properties except the actual contents
void Track::Init(const Track &orig)
{
   mDefaultName = orig.mDefaultName;
   mName = orig.mName;

   if (mDirManager != orig.mDirManager)
   {
      if (mDirManager)
         mDirManager->Deref(); // MM: unreference old DirManager

      // MM: Assign and ref new DirManager
      mDirManager = orig.mDirManager;
      mDirManager->Ref();
   }

   mSelected = orig.mSelected;
   mLinked = orig.mLinked;
   mTeamed = orig.mTeamed;
   mMute = orig.mMute;
   mSolo = orig.mSolo;
   mHeight = orig.mHeight;
   mMinimized = orig.mMinimized;
   mChannel = orig.mChannel;
}

void Track::Merge(const Track &orig)
{
   mSelected = orig.mSelected;
   mMute = orig.mMute;
   mSolo = orig.mSolo;
}

Track::~Track()
{
   mDirManager->Deref();
}

int Track::GetMinimizedHeight() const
{
   if (mTeamed)
      return 20;
   else
      return 40;
}

int Track::GetHeight() const
{
   if (mMinimized)
      return GetMinimizedHeight();
   else
      return mHeight;
}

// TrackListIterator
TrackListIterator::TrackListIterator(TrackList * val)
{
   l = val;
}

Track *TrackListIterator::Last()
{
   if( l == NULL )
      return NULL;

   cur = l->tail;

   if (cur)
      return cur->t;
   else
      return NULL;
}

Track *TrackListIterator::First(TrackList * val)
{
   if (val != NULL)
      l = val;

   if (l == NULL)
      return NULL;

   cur = l->head;

   if (cur)
      return cur->t;
   else
      return NULL;
}

Track *TrackListIterator::Next( bool SkipLinked )
{
   if (SkipLinked && cur && cur->t->GetLinked())
      cur = cur->next;

   if (cur)
      cur = cur->next;

   if (cur)
      return cur->t;
   else
      return NULL;
}

Track *TrackListIterator::RemoveCurrent()
{
   TrackListNode *p = cur;
   TrackListNode *next = p->next;

   // Remove p from the linked list
   if (p->prev)
      p->prev->next = next;
   else
      l->head = next;

   if (next)
      next->prev = p->prev;
   else
      l->tail = p->prev;

   delete p;

   cur = next;

   if (cur)
      return cur->t;
   else
      return NULL;
}

TrackList::TrackList()
{
   head = 0;
   tail = 0;
}

TrackList::TrackList(TrackList * list)
{
   head = 0;
   tail = 0;

   TrackListIterator iter(list);

   Track *t = iter.First();
   while (t) {
      Add(t);
      t = iter.Next();
   }
}

TrackList::~TrackList()
{
   Clear();
}

double TrackList::GetMinOffset() const
{
   if (IsEmpty())
      return 0.0;

   double len = head->t->GetOffset();
   ConstTrackListIterator iter(this);

   for (Track *t = iter.First(); t; t = iter.Next()) {
      double l = t->GetOffset();
      if (l < len)
         len = l;
   }

   return len;
}

int TrackList::GetHeight() const
{
   int height = 0;

   ConstTrackListIterator iter(this);

   for (Track *t = iter.First(); t; t = iter.Next())
      height += t->GetHeight();

   return height;
}

void TrackList::Add(Track * t)
{
   TrackListNode *n = new TrackListNode();
   n->t = (Track *) t;
   n->prev = tail;
   n->next = 0;
   if (tail)
      tail->next = n;
   tail = n;
   if (!head)
      head = n;
}

void TrackList::AddToHead(Track * t)
{
   TrackListNode *n = new TrackListNode();
   n->t = (Track *) t;
   n->prev = 0;
   n->next = head;
   if (head)
      head->prev = n;
   head = n;
   if (!tail)
      tail = n;
}

// TODO: Removing a track does not free the track resources.
void TrackList::Remove(Track * t)
{
   TrackListNode *p = head;
   while (p) {
      if (p->t == t) {
         // Remove p from the linked list

         if (p->prev)
            p->prev->next = p->next;
         else
            head = p->next;

         if (p->next)
            p->next->prev = p->prev;
         else
            tail = p->prev;

         delete p;

         return;
      }
      p = p->next;
   }
}

void TrackList::Clear(bool deleteTracks /* = false */)
{
   while (head) {
      TrackListNode *temp = head;
      if (deleteTracks)
         delete head->t;
      head = head->next;
      delete temp;
   }
   tail = 0;
}

void TrackList::Select(Track * t, bool selected /* = true */ )
{
   TrackListNode *p = head;
   while (p) {
      if (p->t == t) {
         t->SetSelected(selected);
         if (t->GetLinked() && p->next)
            p->next->t->SetSelected(selected);
         else if (p->prev && p->prev->t->GetLinked())
            p->prev->t->SetSelected(selected);

         return;
      }
      p = p->next;
   }
}


Track *TrackList::GetLink(Track * t) const
{
   TrackListNode *p = head;
   while (p) {
      if (p->t == t) {
         if (t->GetLinked() && p->next)
            return p->next->t;
         else if (p->prev && p->prev->t->GetLinked())
            return p->prev->t;

         return NULL;
      }
      p = p->next;
   }
   return NULL;
}

Track *TrackList::GetNext(Track * t, bool linked ) const
{
   TrackListNode *p = head;
   while (p) {
      if (p->t == t) {
         if (linked && t->GetLinked() && p->next)
            p = p->next;
         if (p && p->next)
            return p->next->t;
         else
            return NULL;
      }
      p = p->next;
   }
   return NULL;
}

Track *TrackList::GetPrev(Track * t, bool linked ) const
{
   TrackListNode *p = head;
   while (p) {
      if (p->t == t) {
         if (linked && p->prev && p->prev->prev && p->prev->prev->t->GetLinked())
            p = p->prev;
         if (p && p->prev)
            return p->prev->t;
         else
            return NULL;
      }
      p = p->next;
   }
   return NULL;
}

/// For mono track height of track
/// For stereo track combined height of track and linked track.
/// Assumes we are on the first track of the pair.
int TrackList::GetGroupHeight( Track*t ) const
{
   wxASSERT( t );
   int height=t->GetHeight();
   if( !t->GetLinked() )
      return height;

   TrackListNode *p = head;
   while (p) {
      if (p->t == t) {
         if( p->next )
            return height + p->next->t->GetHeight();
      }
      p = p->next;
   }
   wxASSERT( false );
   return 0;
}

bool TrackList::CanMoveUp(Track * t) const
{
   TrackListNode *p = head;
   while (p) {
      if (p->t == t) {
         if (p->prev && p->prev->t->GetLinked())
            return CanMoveUp(p->prev->t);
         else if (p->prev)
            return true;
         else
            return false;
      }
      p = p->next;
   }
   return false;
}

bool TrackList::CanMoveDown(Track * t) const
{
   TrackListNode *p = head;
   while (p) {
      if (p->t == t) {
         if (t->GetLinked())
            return (p->next != NULL && p->next->next != NULL);
         else
            return (p->next != NULL);
      }
      p = p->next;
   }
   return false;
}

// Precondition: if either of s1 or s2 are "linked", then
// s1 and s2 must each be the FIRST node of the linked pair.
//
// This is used when you want to swap the track or pair of
// tracks in s1 with the track or pair of tracks in s2.
// The complication is that the tracks are stored in a single
// linked list, and pairs of tracks are marked only by a flag
// in one of the tracks.
void TrackList::Swap(TrackListNode * s1, TrackListNode * s2)
{
   Track *source[4];
   TrackListNode *target[4];

   // if a null pointer is passed in, we want to know about it
   wxASSERT(s1);
   wxASSERT(s2);

   target[0] = s1;
   source[0] = target[0]->t;
   if (source[0]->GetLinked()) {
      target[1] = target[0]->next;
      source[1] = target[1]->t;
   } else {
      target[1] = NULL;
      source[1] = NULL;
   }

   target[2] = s2;
   source[2] = target[2]->t;
   if (source[2]->GetLinked()) {
      target[3] = target[2]->next;
      source[3] = target[3]->t;
   } else {
      target[3] = NULL;
      source[3] = NULL;
   }

   int s = 2;
   for (int t = 0; t < 4; t++) {
      if (target[t]) {
         target[t]->t = source[s];
         s = (s + 1) % 4;
         if (!source[s])
            s = (s + 1) % 4;
      }
   }
}

bool TrackList::MoveUp(Track * t)
{
   TrackListNode *p = head;
   while (p) {
      // iterate over all tracks in the list
      if (p->t == t) {
         // this is the track passed in
         TrackListNode *second = p;
         // handle the case where the track passed is second of a linked pair
         // by changing second to be the first of the pair
         if (second->prev && second->prev->t->GetLinked())
            second = second->prev;

         TrackListNode *first = second->prev;
         if (!first)
            return false;  // if no previous track
         if (first->prev && first->prev->t->GetLinked())
            first = first->prev; // if previous is 2nd of linked pair, then
            // move before first of linked pair. Must always work because
            // previous can only be second of linked pair if there is a first
            // track in the linked pair.
         // swap the two tracks over in the list
         Swap(first, second);

         return true;
      }
      p = p->next;
   }
   return false;
}

bool TrackList::MoveDown(Track * t)
{
   TrackListNode *p = head;
   while (p) {
      // this while loop iterates over all tracks looking for the passed
      // argument, t
      if (p->t == t) {
         // this if triggers when we find the track we were passed
         TrackListNode *first = p;
         // if there is a previous track, and that track is linked to this one,
         // move current track pointer back to that track. This avoids trying
         // to move the bottom track of a linked pair.
         if (first->prev && first->prev->t->GetLinked())
            first = first->prev;

         TrackListNode *second;
         if (!p->next)
            return false;  // exit if there is no next track
         if (p->t->GetLinked())
            {  // if next track is part of linked pair
            if (p->next->next)
               {  // if track after next exists
               second = p->next->next; // use it
               }
            else
               {  // there isn't anything after the linked track!
               return false;
               }
            }
         else
            second = p->next; // Just normal next track exists, so move down one

         Swap(first, second);
         return true;
      }
      p = p->next;
   }
   return false;
}

bool TrackList::Contains(Track * t) const
{
   TrackListNode *p = head;
   while (p) {
      if (p->t == t)
         return true;
      p = p->next;
   }
   return false;
}

bool TrackList::IsEmpty() const
{
   return (head == NULL);
}

TimeTrack *TrackList::GetTimeTrack()
{
   TrackListNode *p = head;
   while (p) {
      if (p->t->GetKind() == Track::Time)
         return (TimeTrack *)p->t;
      p = p->next;
   }
   return NULL;
}

int TrackList::GetNumExportChannels(bool selectionOnly)
{
   /* counters for tracks panned different places */
   int numLeft = 0;
   int numRight = 0;
   int numMono = 0;
   /* track iteration kit */
   Track *tr;
   TrackListIterator mIterator;

   for (tr = mIterator.First(this); tr != NULL; tr = mIterator.Next()) {

      // Only want wave tracks
      if (tr->GetKind() != Track::Wave) {
         continue;
      }

      // do we only want selected ones?
      if ( selectionOnly && !(tr->GetSelected()))
      { //want selected but this one is not
          continue;
      }

      // Found a left channel
      if (tr->GetChannel() == Track::LeftChannel) {
         numLeft++;
      }

      // Found a right channel
      else if (tr->GetChannel() == Track::RightChannel) {
         numRight++;
      }

      // Found a mono channel, but it may be panned
      else if (tr->GetChannel() == Track::MonoChannel) {
         float pan = ((WaveTrack*)tr)->GetPan();

         // Figure out what kind of channel it should be
         if (pan == -1.0) {   // panned hard left
            numLeft++;
         }
         else if (pan == 1.0) {  // panned hard right
            numRight++;
         }
         else if (pan == 0) { // panned dead center
            numMono++;
         }
         else {   // panned somewhere else
            numLeft++;
            numRight++;
         }
      }
   }
   // if there is stereo content, report 2, else report 1
   if (numRight > 0 || numLeft > 0) {
      return 2;
   }
   else
   {
      return 1;
   }
}

void TrackList::GetWaveTracks(bool selectionOnly,
                              int *num, WaveTrack ***tracks)
{
   int i;
   *num = 0;

   TrackListNode *p = head;
   while (p) {
      if (p->t->GetKind() == Track::Wave && !(p->t->GetMute()) &&
          (p->t->GetSelected() || !selectionOnly))
         (*num)++;
      p = p->next;
   }

   *tracks = new WaveTrack*[*num];
   p = head;
   i = 0;
   while (p) {
      if (p->t->GetKind() == Track::Wave && !(p->t->GetMute()) &&
          (p->t->GetSelected() || !selectionOnly))
         (*tracks)[i++] = (WaveTrack *)p->t;
      p = p->next;
   }
}

WaveTrackArray TrackList::GetWaveTrackArray(bool selectionOnly)
{
   WaveTrackArray waveTrackArray;

   TrackListNode *p = head;
   while (p) {
      if (p->t->GetKind() == Track::Wave &&
          (p->t->GetSelected() || !selectionOnly))
         waveTrackArray.Add((WaveTrack*)p->t);

      p = p->next;
   }
   return waveTrackArray;
}

#ifdef new
#undef new
#endif

#include <map>
#include "BlockFile.h"
#include "Sequence.h"

// get the sum of the sizes of all blocks this track list
// references.  However, if a block is referred to multiple
// times it is only counted once.  Return value is in bytes
wxLongLong TrackList::GetSpaceUsage()
{
   // the map guarantees that I only count each block once
   std::map<BlockFile*,wxLongLong> blockFiles;
   for (TrackListNode *p = head; p; p = p->next) {
      if (p->t->GetKind() == Track::Wave) {
         WaveTrack* track = ((WaveTrack*)p->t);
         for (WaveClipList::Node* it=track->GetClipIterator(); it; it=it->GetNext())
         {
            WaveClip* clip = it->GetData();
            BlockArray *blocks = clip->GetSequenceBlockArray();
            for (unsigned int i = 0; i < blocks->GetCount(); i++)
               blockFiles[blocks->Item(i)->f] = blocks->Item(i)->f->GetSpaceUsage();
         }
      }
   }

   std::map<BlockFile*,wxLongLong>::const_iterator bfIter;
   wxLongLong bytes = 0;
   for (bfIter = blockFiles.begin(); bfIter != blockFiles.end(); bfIter++)
      bytes += bfIter->second;

   return bytes;
}

#include <set>
#include "UndoManager.h"
// Find out how much additional space was used to execute
// this operation.
//
// Computed by getting a list of all blocks referenced by
// *this* TrackList and removing all blocks referenced by
// any previous TrackList.
wxLongLong TrackList::GetAdditionalSpaceUsage(UndoStack *stack)
{
   TrackListNode *p;
   // get a map of all blocks referenced in this TrackList
   std::map<BlockFile*,wxLongLong> curBlockFiles;
   for (p = head; p; p = p->next) {
      if (p->t->GetKind() == Track::Wave) {
         WaveTrack* track = ((WaveTrack*)p->t);
         for (WaveClipList::Node* it=track->GetClipIterator(); it; it=it->GetNext())
         {
            WaveClip* clip = it->GetData();
            BlockArray *blocks = clip->GetSequenceBlockArray();
            for (unsigned int i = 0; i < blocks->GetCount(); i++)
               curBlockFiles[blocks->Item(i)->f] = blocks->Item(i)->f->GetSpaceUsage();
         }
      }
   }

   // get a set of all blocks referenced in all prev TrackList
   std::set<BlockFile*> prevBlockFiles;
   unsigned int undoStackIdx = 0;
   for (; undoStackIdx < stack->GetCount(); undoStackIdx++) {
      UndoStackElem *stackElem = stack->Item(undoStackIdx);
      if (stackElem->tracks == this)
         break;
      for (p = stackElem->tracks->head; p; p = p->next) {
         if (p->t->GetKind() == Track::Wave) {
            WaveTrack* track = ((WaveTrack*)p->t);
            for (WaveClipList::Node* it=track->GetClipIterator(); it; it=it->GetNext())
            {
               WaveClip* clip = it->GetData();
               BlockArray *blocks = clip->GetSequenceBlockArray();
               for (unsigned int i = 0; i < blocks->GetCount(); i++)
                  prevBlockFiles.insert(blocks->Item(i)->f);
            }         
         }
      }
   }

   // remove all blocks in prevBlockFiles from curBlockFiles
   std::set<BlockFile*>::const_iterator prevIter = prevBlockFiles.begin();
   for (; prevIter != prevBlockFiles.end(); prevIter++)
      curBlockFiles.erase(*prevIter);

   // sum the sizes of the blocks remaining in curBlockFiles;
   std::map<BlockFile*,wxLongLong>::const_iterator curBfIter =
      curBlockFiles.begin();
   wxLongLong bytes = 0;
   for (;curBfIter != curBlockFiles.end(); curBfIter++)
      bytes += curBfIter->second;

   return bytes;
}

double TrackList::GetStartTime() const
{
   if (IsEmpty())
      return 0.0;
   
   double min = head->t->GetStartTime(); // head->t should be APIfied
   ConstTrackListIterator iter(this);
   
   for (Track *t = iter.First(); t; t = iter.Next()) {
      double l = t->GetStartTime();
      if (l < min)
         min = l;
   }
   
   return min;
}

double TrackList::GetEndTime() const
{
   if (IsEmpty())
      return 0.0;
   
   double max = head->t->GetEndTime();
   ConstTrackListIterator iter(this);
   
   for (Track *t = iter.First(); t; t = iter.Next()) {
      double l = t->GetEndTime();
      if (l > max)
         max = l;
   }
   
   return max;
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 575d97aa-2da9-476d-a39e-2ccad16b7cdd

