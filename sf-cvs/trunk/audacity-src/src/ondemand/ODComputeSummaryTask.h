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
   
   ///Sets the WaveTrack to compute the summary data for.
   void SetWaveTrack(WaveTrack* track);
   
   virtual void StopUsingWaveTrack(WaveTrack* track);
   
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
   
   WaveTrack* mWaveTrack;
   ODLock     mWaveTrackMutex;
   
   int mMaxBlockFiles;
   int mComputedBlockFiles;

};

#endif

