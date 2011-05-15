/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2011 Audacity Team.
   License: GPL v2.  See License.txt.

   TrackGroup.h
   Vaughan Johnson

******************************************************************/

#ifndef __AUDACITY_TRACKGROUP__
#define __AUDACITY_TRACKGROUP__

#include "Track.h"

class TrackGroup 
{
public:
   TrackGroup();
   virtual ~TrackGroup() {};

private:
   TrackList* mTracks;
};

WX_DEFINE_ARRAY(TrackGroup*, TrackGroupArray);


class TrackGroupManager 
{
public:
   TrackGroupManager();
   virtual ~TrackGroupManager() {};

private:
   TrackGroupArray mTrackGroupArray;
};

#endif // __AUDACITY_TRACKGROUP__
