/**********************************************************************

  Audacity: A Digital Audio Editor

  Track.cpp

  Dominic Mazzoni

**********************************************************************/

#include "wx/file.h"

#include "Track.h"
#include "WaveTrack.h"
#include "NoteTrack.h"
#include "DirManager.h"
#include "GenericStream.h"

#if wxUSE_APPLE_IEEE
extern "C" void ConvertToIeeeExtended(double num, unsigned char *bytes);
extern "C" double ConvertFromIeeeExtended(const unsigned char *bytes);
#else
#error Requires Apple IEEE Extended conversion routines
#endif

VTrack::VTrack(DirManager *projDirManager)
{
  selected = false;

  collapsed = false;

  collapsedHeight = 20;
  expandedHeight = 160;

  tOffset = 0.0;

  dirManager = projDirManager;
}

bool VTrack::Load(GenericStream *in, DirManager *dirManager)
{
  this->dirManager = dirManager;

  unsigned char tOffsetExtended[10];
  in->Read(tOffsetExtended, 10);
  tOffset = ConvertFromIeeeExtended((const unsigned char *)tOffsetExtended);

  return true;
}

bool VTrack::Save(GenericStream *out, bool overwrite)
{
  unsigned char tOffsetExtended[10];
  ConvertToIeeeExtended(tOffset, (unsigned char *)tOffsetExtended);
  out->Write(tOffsetExtended, 10);

  return true;
}

VTrack *VTrack::Duplicate()
{
  VTrack *copy = new VTrack(dirManager);

  copy->collapsedHeight = collapsedHeight;
  copy->expandedHeight = expandedHeight;
  copy->tOffset = tOffset;

  return copy;
}

void VTrack::SetHeight(int h)
{
  Expand();
  this->expandedHeight = h;
}

void VTrack::Collapse()
{
  this->collapsed = true;
}

void VTrack::Expand()
{
  this->collapsed = false;
}

void VTrack::Toggle()
{
  this->collapsed = !this->collapsed;
}

bool VTrack::IsCollapsed()
{
  return this->collapsed;
}

bool TrackList::Save(GenericStream *out, bool overwrite)
{
  out->Write((void *)"tlst",4);

  TrackListNode *n = head;

  while(n) {
	VTrack *t = n->t;
	switch(((VTrack *)t)->GetKind()) {
	case VTrack::Wave:
	  out->Write((void *)"wave",4);
	  break;
	case VTrack::Note:
	  out->Write((void *)"note",4);
	  break;
	case VTrack::Beat:
	  out->Write((void *)"beat",4);
	  break;
	default:
	  out->Write((void *)"trak",4);
	  break;
	}
	
	t->Save(out, overwrite);
	
	n = n->next;
  }

  out->Write((void *)"----",4);

  return true;
}

bool TrackList::Load(GenericStream *in, DirManager *dirManager)
{
  char tag[5];
  int rv;

  rv = in->Read(tag,4);
  tag[4] = 0;
  
  if (strcmp(tag,"tlst"))
	return true;

  while(1) {
	rv = in->Read(tag,4);
	tag[4] = 0;
	
	if (!strcmp(tag,"----"))
	  return true;

	VTrack *newt = 0;

	if (!strcmp(tag,"trak")) {
	  newt = new VTrack(dirManager);
	}

	if (!strcmp(tag,"wave")) {
	  newt = new WaveTrack(dirManager);
	}

	if (!strcmp(tag,"note")) {
	  newt = new NoteTrack(dirManager);
	}

	if (newt) {
	  this->Add(newt);
	  newt->Load(in, dirManager);
	}
  }
  
  return true;
}

TrackList::TrackList()
{
  head = 0;
  tail = 0;
  cur = 0;
}

TrackList::TrackList(TrackList *list)
{
  head = 0;
  tail = 0;
  cur = 0;

  VTrack *t = list->First();
  while(t) {
	Add(t);
	t = list->Next();
  }
}

TrackList::~TrackList()
{
  Clear();
}

double TrackList::GetMaxLen()
{
  double len = 0.0;

  VTrack *t = First();
  while(t) {
	double l = t->GetMaxLen();
	if (l > len)
	  len = l;
	t = Next();
  }

  return len;
}

int TrackList::GetHeight()
{
  int height = 0;

  VTrack *t = First();
  while(t) {
	height += t->GetHeight();
	t = Next();
  }

  return height;
}

void TrackList::Add(VTrack *t)
{
  TrackListNode *n = new TrackListNode();
  n->t = (VTrack *)t;
  n->prev = tail;
  n->next = 0;
  if (tail)
	tail->next = n;
  tail = n;
  if (!head)
	head = n;
}

void TrackList::Remove(VTrack *t)
{
  TrackListNode *p = head;
  while(p) {
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
	  
	  cur = 0;
	  return;
	}
	p = p->next;
  }
}

VTrack *TrackList::RemoveCurrent()
{
  TrackListNode *p = cur;
  TrackListNode *next = p->next;

  // Remove p from the linked list
	  
  if (p->prev)
	p->prev->next = next;
  else
	head = next;
  
  if (next)
	next->prev = p->prev;
  else
	tail = p->prev;
  
  delete p;

  cur = next;

  if (cur)	
	return cur->t;
  else
	return NULL;
}

void TrackList::Clear()
{
  while(head) {
	TrackListNode *temp = head;
	head = head->next;
	delete temp;
  }
  tail = 0;
}

bool TrackList::Contains(VTrack *t)
{
  TrackListNode *p = head;
  while(p) {
	if (p->t == t)
	  return true;
	p = p->next;
  }
  return false;
}

VTrack *TrackList::First()
{
  cur = head;

  if (cur)
    return cur->t;
  else
    return 0;
}

VTrack *TrackList::Next()
{
  if (cur)
	  cur = cur->next;

  if (cur)
    return cur->t;
  else
    return 0;
}
