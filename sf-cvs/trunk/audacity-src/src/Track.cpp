/**********************************************************************

  Audacity: A Digital Audio Editor

  Track.cpp

  Dominic Mazzoni

**********************************************************************/

#include <float.h>
#include <wx/file.h>
#include <wx/textfile.h>

#include "Track.h"
#include "WaveTrack.h"
#include "NoteTrack.h"
#include "LabelTrack.h"
#include "DirManager.h"

VTrack::VTrack(DirManager * projDirManager) 
  : dirManager(projDirManager)
{
   selected  = false;
   collapsed = false;
   linked    = false;
   mute      = false;
   solo      = false;

   collapsedHeight = 20;
   expandedHeight = 121;

   tOffset = 0.0;

   dirty = rand();

   channel = MonoChannel;
}

VTrack::VTrack(const VTrack &orig)
{
   name = orig.name;
   
   dirManager = orig.dirManager;
   selected = orig.selected;
   collapsed = orig.collapsed;
   linked = orig.linked;
   mute = orig.mute;
   solo = orig.solo;

   collapsedHeight = orig.collapsedHeight;
   expandedHeight = orig.expandedHeight;

   tOffset = orig.tOffset;

   channel = orig.channel;

   dirty = rand();
}

bool VTrack::Load(wxTextFile * in, DirManager * dirManager)
{
   this->dirManager = dirManager;

   name = in->GetNextLine();
   wxString line = in->GetNextLine();
   if (line == "left") {
      channel = LeftChannel;
      line = in->GetNextLine();
   }
   if (line == "right") {
      channel = RightChannel;
      line = in->GetNextLine();
   }
   if (line == "linked") {
      linked = true;
      line = in->GetNextLine();
   }
   if (line != "offset")
      return false;
   if (!(in->GetNextLine().ToDouble(&tOffset)))
      return false;

   return true;
}

bool VTrack::Save(wxTextFile * out, bool overwrite)
{
   out->AddLine(name);
   if (channel == LeftChannel)
      out->AddLine("left");
   else if (channel == RightChannel)
      out->AddLine("right");
   if (linked)
      out->AddLine("linked");
   out->AddLine("offset");
   out->AddLine(wxString::Format("%f", tOffset));

   return true;
}

void VTrack::SetHeight(int h)
{
   Expand();
   expandedHeight = h;
}

void VTrack::Collapse()
{
   collapsed = true;
}

void VTrack::Expand()
{
   collapsed = false;
}

void VTrack::Toggle()
{
   collapsed = !collapsed;
}

bool VTrack::IsCollapsed() const
{
   return collapsed;
}

// TrackListIterator
TrackListIterator::TrackListIterator(TrackList * val)
{
   l = val;
}

VTrack *TrackListIterator::First()
{
   cur = l->head;

   if (cur)
      return cur->t;
   else
      return NULL;
}

VTrack *TrackListIterator::Next()
{
   if (cur)
      cur = cur->next;

   if (cur)
      return cur->t;
   else
      return NULL;
}

VTrack *TrackListIterator::RemoveCurrent()
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

// TrackList
bool TrackList::Save(wxTextFile * out, bool overwrite)
{
   TrackListNode *n = head;

   while (n) {
      VTrack *t = n->t;
      switch (((VTrack *) t)->GetKind()) {
      case VTrack::Wave:
         out->AddLine("WaveTrack");
         break;
      case VTrack::Note:
         out->AddLine("NoteTrack");
         break;
      case VTrack::Label:
         out->AddLine("LabelTrack");
         break;
      default:
         out->AddLine("Track");
         break;
      }

      t->Save(out, overwrite);

      n = n->next;
   }

   out->AddLine("EndTracks");

   return true;
}

bool TrackList::Load(wxTextFile * in, DirManager * dirManager)
{
   for (;;) {
      wxString cmd = in->GetNextLine();
      if (cmd == "EndTracks")
         return true;

      VTrack *newt = 0;

      if (cmd == "Track") {
         newt = new VTrack(dirManager);
      }

      if (cmd == "WaveTrack") {
         newt = new WaveTrack(dirManager);
      }

      if (cmd == "NoteTrack") {
         newt = new NoteTrack(dirManager);
      }

      if (cmd == "LabelTrack") {
         newt = new LabelTrack(dirManager);
      }

      if (newt) {
         Add(newt);
         newt->Load(in, dirManager);
      }
   }

   return true;
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

   VTrack *t = iter.First();
   while (t) {
      Add(t);
      t = iter.Next();
   }
}

TrackList::~TrackList()
{
   Clear();
}

double TrackList::GetMaxLen() const
{
   double len = DBL_MIN;

   ConstTrackListIterator iter(this);

   for (VTrack *t = iter.First(); t; t = iter.Next()) {
      double l = t->GetMaxLen();
      if (l > len)
         len = l;
   }

   return len;
}

double TrackList::GetMinOffset() const
{
   double len = DBL_MAX;

   ConstTrackListIterator iter(this);

   for (VTrack *t = iter.First(); t; t = iter.Next()) {
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

   for (VTrack *t = iter.First(); t; t = iter.Next())
      height += t->GetHeight();

   return height;
}

void TrackList::Add(VTrack * t)
{
   TrackListNode *n = new TrackListNode();
   n->t = (VTrack *) t;
   n->prev = tail;
   n->next = 0;
   if (tail)
      tail->next = n;
   tail = n;
   if (!head)
      head = n;
}

void TrackList::Remove(VTrack * t)
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

void TrackList::Select(VTrack * t, bool selected /* = true */ )
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


VTrack *TrackList::GetLink(VTrack * t) const
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

VTrack *TrackList::GetNext(VTrack * t) const
{
   TrackListNode *p = head;
   while (p) {
      if (p->t == t) {
         if (p->next)
            return p->next->t;
         else
            return NULL;
      }
      p = p->next;
   }
   return NULL;
}

VTrack *TrackList::GetPrev(VTrack * t) const
{
   TrackListNode *p = head;
   while (p) {
      if (p->t == t) {
         if (p->prev)
            return p->prev->t;
         else
            return NULL;
      }
      p = p->next;
   }
   return NULL;
}

bool TrackList::CanMoveUp(VTrack * t) const
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

bool TrackList::CanMoveDown(VTrack * t) const
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
   VTrack *source[4];
   TrackListNode *target[4];

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

bool TrackList::MoveUp(VTrack * t)
{
   TrackListNode *p = head;
   while (p) {
      if (p->t == t) {
         TrackListNode *second = p;
         if (second->prev && second->prev->t->GetLinked())
            second = second->prev;

         TrackListNode *first = second->prev;
         if (!first)
            return false;
         if (first->prev && first->prev->t->GetLinked())
            first = first->prev;

         Swap(first, second);

         return true;
      }
      p = p->next;
   }
   return false;
}

bool TrackList::MoveDown(VTrack * t)
{
   TrackListNode *p = head;
   while (p) {
      if (p->t == t) {
         TrackListNode *first = p;
         if (first->prev && first->prev->t->GetLinked())
            first = first->prev;

         TrackListNode *second;
         if (!p->next)
            return false;
         if (p->t->GetLinked())
            second = p->next->next;
         else
            second = p->next;

         Swap(first, second);
         return true;
      }
      p = p->next;
   }
   return false;
}

bool TrackList::Contains(VTrack * t) const
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
