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
   sampleCount blockStartSample;
   sampleCount blockEndSample;
   bool success =false;
   
   for(size_t i=0; i < mWaveTracks.size() && mBlockFiles.size();i++)
   {
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
      
      //upddate the gui for all associated blocks.  It doesn't matter that we're hitting more wavetracks then we should
      //because this loop runs a number of times equal to the number of tracks, they probably are getting processed in
      //the next iteration at the same sample window.
      mWaveTrackMutex.Lock();
      for(size_t i=0;i<mWaveTracks.size();i++)
      {
         if(success && mWaveTracks[i])
            mWaveTracks[i]->AddInvalidRegion(blockStartSample,blockEndSample);
      }
      mWaveTrackMutex.Unlock();
   }   
   
   //update percentage complete.
   
   mPercentCompleteMutex.Lock();
   mPercentComplete = (float) 1.0 - ((float)mBlockFiles.size() / (mMaxBlockFiles+1));
   mPercentCompleteMutex.Unlock();

}


void ODComputeSummaryTask::StopUsingWaveTrack(WaveTrack* track)
{
   mWaveTrackMutex.Lock();
   for(size_t i=0;i<mWaveTracks.size();i++)
   {
      if(mWaveTracks[i] == track)
         mWaveTracks[i]=NULL;
   }
   mWaveTrackMutex.Unlock();
}

///Replaces all instances to a wavetrack with a new one, effectively transferring the task.
void ODComputeSummaryTask::ReplaceWaveTrack(WaveTrack* oldTrack,WaveTrack* newTrack)
{
   mWaveTrackMutex.Lock();
   for(size_t i=0;i<mWaveTracks.size();i++)
   {
      if(oldTrack == mWaveTracks[i])
      {
         mWaveTracks[i] = newTrack;
      }
   }  
   mWaveTrackMutex.Unlock();
}

///changes the tasks associated with this Waveform to process the task from a different point in the track
///@param track the track to update
///@param seconds the point in the track from which the tasks associated with track should begin processing from.
void ODComputeSummaryTask::DemandTrackUpdate(WaveTrack* track, double seconds)
{
   mWaveTrackMutex.Lock();
   for(size_t i=0;i<mWaveTracks.size();i++)
   {
      if(track == mWaveTracks[i])
      {
         SetDemandSample((sampleCount)seconds * track->GetRate());
         break;
      }
   }  
   mWaveTrackMutex.Unlock();
}


///by default creates the order of the wavetrack to load.
void ODComputeSummaryTask::Update()
{

   std::vector<ODPCMAliasBlockFile*> tempBlocks;
   
   mWaveTrackMutex.Lock();
   
   for(size_t j=0;j<mWaveTracks.size();j++)
   {
      if(mWaveTracks[j])
      {
         WaveClip *clip;
         BlockArray *blocks;
         Sequence *seq;
         
         //gather all the blockfiles that we should process in the wavetrack.
         WaveClipList::Node* node = mWaveTracks[j]->GetClipIterator();
         
         int numBlocksDone;         
         while(node) {
            clip = node->GetData();
            seq = clip->GetSequence();
            //TODO:this lock is way to big since the whole file is one sequence.  find a way to break it down.
            seq->LockDeleteUpdateMutex();
            
            //See Sequence::Delete() for why need this for now..
            blocks = clip->GetSequenceBlockArray();
            int i;
            int numBlocksIn;
            numBlocksIn=0;
            for(i=0; i<(int)blocks->GetCount(); i++)
            {
               //in the future if we have more than one ODBlockFile, we will need type flags to cast.
               if(!blocks->Item(i)->f->IsSummaryAvailable())
               {
                  blocks->Item(i)->f->Ref();
                  ((ODPCMAliasBlockFile*)blocks->Item(i)->f)->SetStart(blocks->Item(i)->start);
                  ((ODPCMAliasBlockFile*)blocks->Item(i)->f)->SetClipOffset(clip->GetStartTime()*clip->GetRate());
                  
                  //this next bit ensures that we are spacing the blockfiles over multiple wavetracks evenly.
                  if((j+1)*numBlocksIn>tempBlocks.size())
                     tempBlocks.push_back((ODPCMAliasBlockFile*)blocks->Item(i)->f);
                  else
                     tempBlocks.insert(tempBlocks.begin()+(j+1)*numBlocksIn, (ODPCMAliasBlockFile*)blocks->Item(i)->f);
                     
                  
                  numBlocksIn++;
               }
            }   
            numBlocksDone = numBlocksIn;
            
            seq->UnlockDeleteUpdateMutex();
            node = node->GetNext();
         }
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
   //for now just put them in linear.  We start the order from the first block that includes the ondemand sample
   //(which the user sets by clicking.)   note that this code is pretty hacky - it assumes that the array is sorted in time.
   
   //find the startpoint
   sampleCount processStartSample = GetDemandSample(); 
   for(int i= ((int)unorderedBlocks.size())-1;i>= 0;i--)
   {
      //check to see if the refcount is at least two before we add it to the list.
      //There should be one Ref() from the one added by this ODTask, and one from the track.  
      //If there isn't, then the block was deleted for some reason and we should ignore it.
      if(unorderedBlocks[i]->RefCount()>=2)
      {
         if(mBlockFiles.size() && (unorderedBlocks[i]->GetStart()+unorderedBlocks[i]->GetClipOffset()) + unorderedBlocks[i]->GetLength() >=processStartSample && 
                ( (mBlockFiles[0]->GetStart()+mBlockFiles[0]->GetClipOffset()) +  mBlockFiles[0]->GetLength() < processStartSample || 
                  (unorderedBlocks[i]->GetStart()+unorderedBlocks[i]->GetClipOffset()) <= (mBlockFiles[0]->GetStart() +mBlockFiles[0]->GetClipOffset()))
            )
         {
            //insert at the front of the list if we get blockfiles that are after the demand sample
            mBlockFiles.insert(mBlockFiles.begin()+0,unorderedBlocks[i]);
         }
         else
         {
            //otherwise no priority
            mBlockFiles.push_back(unorderedBlocks[i]);
         }
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

int ODComputeSummaryTask::GetNumWaveTracks()
{
   int num;
   mWaveTrackMutex.Lock();
   num = (int)mWaveTracks.size();
   mWaveTrackMutex.Unlock();
   return num;
}

WaveTrack* ODComputeSummaryTask::GetWaveTrack(int i)
{
   WaveTrack* track;
   mWaveTrackMutex.Lock();
   if(i<(int)mWaveTracks.size())
      track = mWaveTracks[i];
   mWaveTrackMutex.Unlock();
   return track;
}

///Sets the wavetrack that will be analyzed for ODPCMAliasBlockFiles that will
///have their summaries computed and written to disk.
void ODComputeSummaryTask::AddWaveTrack(WaveTrack* track)
{
   mWaveTracks.push_back(track);
}

