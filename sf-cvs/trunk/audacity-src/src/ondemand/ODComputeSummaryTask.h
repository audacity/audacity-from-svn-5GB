/**********************************************************************

  Audacity: A Digital Audio Editor

  ODComputeSummaryTask.h

  Created by Michael Chinen (mchinen) on 6/8/08.
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODComputeSummaryTask
\brief Computes the summary data for a PCM (WAV) file and writes it to disk,
updating the ODPCMAliasBlockFile and the GUI of the newly available data.

*//*******************************************************************/




#ifndef __AUDACITY_ODComputeSummaryTask__
#define __AUDACITY_ODComputeSummaryTask__

#include <vector>
#include "ODTask.h"
#include "ODTaskThread.h"
class ODPCMAliasBlockFile;
class WaveTrack;

/// A class representing a modular task to be used with the On-Demand structures.
class ODComputeSummaryTask:public ODTask
{
 public:

   // Constructor / Destructor

   /// Constructs an ODTask
   ODComputeSummaryTask();
   virtual ~ODComputeSummaryTask(){};
   
   ///Adds a WaveTrack to compute the summary data for.
   void AddWaveTrack(WaveTrack* track);
   
   ///remove references to a wavetrack safely
   virtual void StopUsingWaveTrack(WaveTrack* track);
   
   ///Replaces all instances to a wavetrack with a new one, effectively transferring the task.
   virtual void ReplaceWaveTrack(WaveTrack* oldTrack,WaveTrack* newTrack);
   
   ///changes the tasks associated with this Waveform to process the task from a different point in the track
   virtual void DemandTrackUpdate(WaveTrack* track, double seconds);

   virtual int GetNumWaveTracks();
   virtual WaveTrack* GetWaveTrack(int i);

   
   ///Return the task name
   virtual const char* GetTaskName(){return "ODComputeSummaryTask";}
   
protected:
     
   ///Computes and writes the data for one BlockFile if it still has a refcount. 
   virtual void DoSomeInternal();
   
   ///Readjusts the blockfile order in the default manner.  If we have had an ODRequest
   ///Then it updates in the OD manner.
   virtual void Update();
   
   ///Readjusts the blockfile order to start at the new cursor.
   virtual void ODUpdate();

   ///Orders the input as either On-Demand or default layered order.
   void OrderBlockFiles(std::vector<ODPCMAliasBlockFile*> &unorderedBlocks);

   
   std::vector<ODPCMAliasBlockFile*> mBlockFiles;
   
   std::vector<WaveTrack*> mWaveTracks;
   ODLock     mWaveTrackMutex;
   
   int mMaxBlockFiles;
   int mComputedBlockFiles;

};

#endif

