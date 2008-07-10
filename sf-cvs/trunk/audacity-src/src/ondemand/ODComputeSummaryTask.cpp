/**********************************************************************

  Audacity: A Digital Audio Editor

  ODComputeSummaryTask.cpp

  Created by Michael Chinen (mchinen) on 6/8/08.
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODComputeSummaryTask
\brief Computes the summary data for a PCM (WAV) file and writes it to disk,
updating the ODPCMAliasBlockFile and the GUI of the newly available data.

*//*******************************************************************/



#include "ODComputeSummaryTask.h"
#include "../blockfile/ODPCMAliasBlockFile.h"
#include <wx/wx.h>

///Creates a new task that computes summaries for a wavetrack that needs to be specified through SetWaveTrack()
ODComputeSummaryTask::ODComputeSummaryTask()
{
   mMaxBlockFiles = 0;
   mComputedBlockFiles = 0;
   mWaveTrack=NULL;
}
     
///Computes and writes the data for one BlockFile if it still has a refcount. 
void ODComputeSummaryTask::DoSomeInternal()
{
   if(mBlockFiles.size()<=0)
   {
      mPercentCompleteMutex.Lock();
      mPercentComplete = 1.0;
      mPercentCompleteMutex.Unlock();
      return;
   }
   
   ODPCMAliasBlockFile* bf;
   int blockStartSample;
   int blockEndSample;
   bool success =false;
   bf = mBlockFiles[0];
   
   //first check to see if the ref count is at least 2.  It should have one 
   //from when we added it to this instance's mBlockFiles array, and one from
   //the Wavetrack/sequence.  If it doesn't it has been deleted and we should forget it.
   if(bf->RefCount()>=2)
   {
      bf->DoWriteSummary();
      success = true;
      blockStartSample = bf->GetStart();
      blockEndSample = blockStartSample + bf->GetLength();
      mComputedBlockFiles++;
   }
   else
   {
      //the waveform in the wavetrack now is shorter, so we need to update mMaxBlockFiles
      //because now there is less work to do.
      mMaxBlockFiles--;
   }
   
   //Release the refcount we placed on it.
   bf->Deref();
   //take it out of the array - we are done with it.
   mBlockFiles.erase(mBlockFiles.begin());
      
   
   //update percentage complete.
   
   mPercentCompleteMutex.Lock();
   mPercentComplete = (float) 1.0 - ((float)mBlockFiles.size() / (mMaxBlockFiles+1));
   mPercentCompleteMutex.Unlock();

   //TODO: update track gui somehow.. or do it from a timer.  How does the cursor move in playback?
   
   mWaveTrackMutex.Lock();
   if(success && mWaveTrack)
      mWaveTrack->AddInvalidRegion(blockStartSample,blockEndSample);
   mWaveTrackMutex.Unlock();
}


void ODComputeSummaryTask::StopUsingWaveTrack(WaveTrack* track)
{
   mWaveTrackMutex.Lock();
   if(mWaveTrack == track)
      mWaveTrack=NULL;
   mWaveTrackMutex.Unlock();
}

///Replaces all instances to a wavetrack with a new one, effectively transferring the task.
void ODComputeSummaryTask::ReplaceWaveTrack(WaveTrack* oldTrack,WaveTrack* newTrack)
{
   mWaveTrackMutex.Lock();
   if(oldTrack == mWaveTrack)
   {
      mWaveTrack = newTrack;
   }  
   mWaveTrackMutex.Unlock();
}


///by default creates the order of the wavetrack to load.
void ODComputeSummaryTask::Update()
{

   std::vector<ODPCMAliasBlockFile*> tempBlocks;
   mWaveTrackMutex.Lock();
   if(mWaveTrack)
   {
      WaveClip *clip;
      BlockArray *blocks;
      Sequence *seq;
      
      //gather all the blockfiles that we should process in the wavetrack.
      WaveClipList::Node* node = mWaveTrack->GetClipIterator();
      while(node) {
         clip = node->GetData();
         seq = clip->GetSequence();
         //TODO:this lock is way to big since the whole file is one sequence.  find a way to break it down.
         seq->LockDeleteUpdateMutex();
         
         //See Sequence::Delete() for why need this for now..
         blocks = clip->GetSequenceBlockArray();
         int i;
         for(i=0; i<(int)blocks->GetCount(); i++)
         {
            //in the future if we have more than one ODBlockFile, we will need type flags to cast.
            if(!blocks->Item(i)->f->IsSummaryAvailable())
            {
               blocks->Item(i)->f->Ref();
               ((ODPCMAliasBlockFile*)blocks->Item(i)->f)->SetStart(blocks->Item(i)->start);
               tempBlocks.push_back((ODPCMAliasBlockFile*)blocks->Item(i)->f);
               
            }
         }   
         
         seq->UnlockDeleteUpdateMutex();
         node = node->GetNext();
      }
   }
   mWaveTrackMutex.Unlock();
   
   //get the new order.
   OrderBlockFiles(tempBlocks);
}



///Orders the input as either On-Demand or default layered order.
void ODComputeSummaryTask::OrderBlockFiles(std::vector<ODPCMAliasBlockFile*> &unorderedBlocks)
{
   //we are going to take things out of the array.  But first deref them since we ref them when we put them in.
   for(unsigned int i=0;i<mBlockFiles.size();i++)
      mBlockFiles[i]->Deref();
   mBlockFiles.clear();
   //TODO:order the blockfiles into our queue in a fancy convenient way.  (this could be user-prefs)
   //for now just put them in linear.
   for(unsigned int i=0;i<unorderedBlocks.size();i++)
   {
      //check to see if the refcount is at least two before we add it to the list.
      //There should be one Ref() from us, and one from the track.  
      //If there isn't, then the block was deleted for some reason and we should ignore it.
      if(unorderedBlocks[i]->RefCount()>=2)
      {
         mBlockFiles.push_back(unorderedBlocks[i]);
         if(mMaxBlockFiles< (int) mBlockFiles.size())
            mMaxBlockFiles = mBlockFiles.size();
      }
      else
      {
         //Otherwise, let it be deleted and forget about it.
         unorderedBlocks[i]->Deref();
      }
   }
   
}  

void ODComputeSummaryTask::ODUpdate()
{
   //clear old blockFiles and do something smarter.
   
}   

///Sets the wavetrack that will be analyzed for ODPCMAliasBlockFiles that will
///have their summaries computed and written to disk.
void ODComputeSummaryTask::SetWaveTrack(WaveTrack* track)
{
   mWaveTrack=track;
}

