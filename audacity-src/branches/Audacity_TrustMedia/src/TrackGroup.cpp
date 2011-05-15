/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2011 Audacity Team.
   License: GPL v2.  See License.txt.

   TrackGroup.cpp
   Vaughan Johnson

******************************************************************//**

\class TrackGroup
\brief TrackGroup is used by TrackPanel for managing grouped tracks. 

Grouped tracks have these constraints on their constituent tracks:
* Clip boundaries must be the same among all tracks. 

*//*******************************************************************/

#include "TrackGroup.h"

TrackGroup::TrackGroup()
{
}

TrackGroupManager::TrackGroupManager()
{
}
