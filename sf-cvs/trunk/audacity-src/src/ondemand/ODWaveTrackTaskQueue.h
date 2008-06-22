/**********************************************************************

  Audacity: A Digital Audio Editor

  ODWaveTrackTaskQueue.h

  Created by Michael Chinen (mchinen)
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODWaveTrackTaskQueue
\brief watches over all to be done (not yet started and started but not finished)
tasks associated with a WaveTrack.

*//*******************************************************************/




#ifndef __AUDACITY_ODWAVETRACKTASKQUEUE__
#define __AUDACITY_ODWAVETRACKTASKQUEUE__

#include <vector>
#include "ODTaskThread.h"
class WaveTrack;
class ODTask;
/// A class representing a modular task to be used with the On-Demand structures.
class ODWaveTrackTaskQueue
{
 public:

   // Constructor / Destructor

   /// Constructs an ODWaveTrackTaskQueue
   ODWaveTrackTaskQueue(WaveTrack* track);
   
   virtual ~ODWaveTrackTaskQueue();
   
   ///Adds a track to the associated list.
   void AddWaveTrack(WaveTrack* track);
   ///Removes a track from the list.  Also notifies mTasks to stop using references
   ///to the instance in a thread-safe manner (may block)
   void RemoveWaveTrack(WaveTrack* track);      
   
   ///replaces all instances of a WaveTrack within this task with another.  
   void ReplaceWaveTrack(WaveTrack* oldTrack,WaveTrack* newTrack);      
   
   //returns true if the agrument is in the WaveTrack list.
   bool ContainsWaveTrack(WaveTrack* track);
   
   //returns the wavetrack at position x.
   WaveTrack* GetWaveTrack(int x);
   
   ///returns the number of wavetracks in this queue.
   int GetNumWaveTracks();
   
   ///Add a task to the queue.  
   void AddTask(ODTask* task);
   
   //returns true if either tracks or tasks are empty
   bool IsEmpty();
   
   //returns true if the foremost task exists and is empty.
   bool IsFrontTaskComplete();
   
   ///Removes and deletes the front task from the list.
   void RemoveFrontTask();
   
   ///Schedules the front task for immediate execution
   ODTask* GetFrontTask();
   
 protected:
     
  ///the list of tracks associated with this queue.
  std::vector<WaveTrack*> mTracks;
  ODLock mTracksMutex;
  
  ///the list of tasks associated with the tracks.  This class owns these tasks.
  std::vector<ODTask*> mTasks;
  ODLock    mTasksMutex;

};

#endif

