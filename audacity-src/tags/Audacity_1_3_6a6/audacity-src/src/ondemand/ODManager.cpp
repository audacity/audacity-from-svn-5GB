/*
 *  ODManager.cpp
 *  Audacity
 *
 *  Created by apple on 6/8/08.
 *  Copyright 2008 __MyCompanyName__. All rights reserved.
 *
 */

#include "ODManager.h"
#include "ODTask.h"
#include "ODTaskThread.h"
#include "ODWaveTrackTaskQueue.h"
#include "../Project.h"
#include <NonGuiThread.h>
#include <wx/utils.h>
#include <wx/wx.h>
#include <wx/thread.h>
#include <wx/event.h>
bool gManagerCreated=false;
/// a flag that is set if we have loaded some OD blockfiles from PCM.  
bool sHasLoadedOD=false;



//libsndfile is not threadsafe - this deals with it
ODLock sLibSndFileMutex;

DEFINE_EVENT_TYPE(EVT_ODTASK_UPDATE)


void ODManager::LockLibSndFileMutex()
{
   sLibSndFileMutex.Lock();
}

void ODManager::UnlockLibSndFileMutex()
{
   sLibSndFileMutex.Unlock();
}


//private constructor - Singleton.
ODManager::ODManager()
{
   mTerminate = false;
   mTerminated = false;
}

//private constructor - delete with static method Quit()
ODManager::~ODManager()
{
   //get rid of all the queues.  The queues get rid of the tasks, so we don't worry abut them.
   //nothing else should be running on OD related threads at this point, so we don't lock.
   for(unsigned int i=0;i<mQueues.size();i++)
      delete mQueues[i];
      
}



///Adds a task to running queue.  Thread-safe.
void ODManager::AddTask(ODTask* task)
{
   mTasksMutex.Lock();
   mTasks.push_back(task);
   mTasksMutex.Unlock();
}

///removes a task from the active task queue
void ODManager::RemoveTaskIfInQueue(ODTask* task)
{
   mTasksMutex.Lock();
   for(unsigned int i=0;i<mTasks.size();i++)
   {
      if(mTasks[i]==task)
      {
         mTasks.erase(mTasks.begin()+i);
         break;
      }
   }
   mTasksMutex.Unlock();

}

///Adds a new task to the queue.  Creates a queue if the tracks associated with the task is not in the list
///
///@param task the task to add
///@param lockMutex locks the mutexes if true (default).  This function is used within other ODManager calls, which many need to set this to false.
void ODManager::AddNewTask(ODTask* task, bool lockMutex)
{
   ODWaveTrackTaskQueue* queue = NULL;

   if(lockMutex)
      mQueuesMutex.Lock();
   for(unsigned int i=0;i<mQueues.size();i++)
   {
      //search for a task containing the lead track.  
      //This may be buggy when adding tracks.  We may have to do an exhaustive search instead.
      //note that GetWaveTrack is not threadsafe, but we are assuming task is not running on a different thread yet.
      if(mQueues[i]->ContainsWaveTrack(task->GetWaveTrack(0)))
         queue=mQueues[i];
   }
  
   if(queue)
   {
      //Add it to the existing queue but keep the lock since this reference can be deleted.
      queue->AddTask(task);
      if(lockMutex)
         mQueuesMutex.Unlock();
   }
   else
   {
      //Make a new one, add it to the local track queue, and to the immediate running task list,
      //since this task is definitely at the head
      queue = new ODWaveTrackTaskQueue();
      queue->AddTask(task);
      mQueues.push_back(queue);
      if(lockMutex)
         mQueuesMutex.Unlock();
     
      AddTask(task);
      
   }
}
   
ODManager* ODManager::Instance()
{
   static ODManager* man=NULL;
   static bool inited = false;
   //this isn't 100% threadsafe but I think Okay for this purpose.
   
 //   wxLogDebug(wxT("Fetching Instance\n"));
   if(!man)
   {
      man = new ODManager();
      gManagerCreated = true;
      man->Init();
      inited = true;
   }
   while(!inited)
      ;
   
   return man;
}

bool ODManager::IsInstanceCreated()
{
  return gManagerCreated;
}

///Launches a thread for the manager and starts accepting Tasks.
void ODManager::Init()
{
   mCurrentThreads = 0;
   mMaxThreads = 5;
   
   //   wxLogDebug(wxT("Initializing ODManager...Creating manager thread\n"));
   ODManagerHelperThread* startThread = new ODManagerHelperThread;
   
//   startThread->SetPriority(0);//default of 50.
   startThread->Create();
//   printf("starting thread from init\n");
   startThread->Run();
   
//   printf("started thread from init\n");
   //destruction is taken care of by wx thread code.  TODO:Check if pthreads code does this.
}

void ODManager::DecrementCurrentThreads()
{
   mCurrentThreadsMutex.Lock();
   mCurrentThreads--;
   mCurrentThreadsMutex.Unlock();
}

///Main loop for managing threads and tasks.
void ODManager::Start()
{   

   ODTask* task;
   ODTaskThread* thread;
   bool tasksInArray;
   
   mNeedsDraw=0;

   //wxLog calls not threadsafe.  are printfs?  thread-messy for sure, but safe?
   //printf("ODManager thread strating \n");
   //TODO: Figure out why this has no effect at all.
   //wxThread::This()->SetPriority(30);
   mTerminateMutex.Lock();
   while(!mTerminate)
   {
      mTerminateMutex.Unlock();
      
      //we should look at our WaveTrack queues to see if we can process a new task to the running queue.
      UpdateQueues();
      
      //start some threads if necessary
      
      mTasksMutex.Lock();
      tasksInArray = mTasks.size()>0;
      mTasksMutex.Unlock();
      mCurrentThreadsMutex.Lock();
      
      while( tasksInArray&& mCurrentThreads < mMaxThreads)
      {
         mCurrentThreads++;
         mCurrentThreadsMutex.Unlock();
         //remove the head
         mTasksMutex.Lock();
         task = mTasks[0];
         
         //the thread will add it back to the array if the job is not yet done at the end of the thread's run.  
         mTasks.erase(mTasks.begin());  
         mTasksMutex.Unlock();
         
         //detach a new thread.
         thread = new ODTaskThread(task);
         
//         thread->SetPriority(10);//default is 50.
         thread->Create();
         thread->Run();
         
         mTasksMutex.Lock();
         tasksInArray = mTasks.size()>0;
         mTasksMutex.Unlock();
         
         mCurrentThreadsMutex.Lock();
      }

      mCurrentThreadsMutex.Unlock();
      wxThread::Sleep(200);
//wxSleep can't be called from non-main thread.
//      ::wxMilliSleep(250);
      
      //if there is some ODTask running, then there will be something in the queue.  If so then redraw to show progress
      
      mQueuesMutex.Lock();
      mNeedsDraw += mQueues.size()>0?1:0;
      mQueuesMutex.Unlock();

      //redraw the current project only (ODTasks will send a redraw on complete even if the projects are in the background)
      if(mNeedsDraw > 11)
      {
         mNeedsDraw=0;
         wxCommandEvent event( EVT_ODTASK_UPDATE );
         AudacityProject::AllProjectsDeleteLock();
         AudacityProject* proj = GetActiveProject();
         if(proj)
            proj->AddPendingEvent( event );
         AudacityProject::AllProjectsDeleteUnlock();
      }   
      mTerminateMutex.Lock();
   }
   mTerminateMutex.Unlock();
   
   mTerminatedMutex.Lock();
   mTerminated=true;
   mTerminatedMutex.Unlock();
   
   //wxLogDebug Not thread safe.
   //printf("ODManager thread terminating\n");

}

void ODManager::Quit()
{
   if(gManagerCreated)
   {
      ODManager::Instance()->mTerminateMutex.Lock();
      ODManager::Instance()->mTerminate = true;
      ODManager::Instance()->mTerminateMutex.Unlock();
      
      ODManager::Instance()->mTerminatedMutex.Lock();
      while(!ODManager::Instance()->mTerminated)
      {
         ODManager::Instance()->mTerminatedMutex.Unlock();
         wxThread::Sleep(200);
         ODManager::Instance()->mTerminatedMutex.Lock();
      }
      ODManager::Instance()->mTerminatedMutex.Unlock();
   

      delete ODManager::Instance();
   }
}
    
///removes a wavetrack and notifies its associated tasks to stop using its reference. 
void ODManager::RemoveWaveTrack(WaveTrack* track)
{
   mQueuesMutex.Lock();
   for(unsigned int i=0;i<mQueues.size();i++)
   {
      if(mQueues[i]->ContainsWaveTrack(track))
         mQueues[i]->RemoveWaveTrack(track);
   }
   mQueuesMutex.Unlock();
}

///replace the wavetrack whose wavecache the gui watches for updates
void ODManager::ReplaceWaveTrack(WaveTrack* oldTrack,WaveTrack* newTrack)
{
   mQueuesMutex.Lock();
   for(unsigned int i=0;i<mQueues.size();i++)
   {
      mQueues[i]->ReplaceWaveTrack(oldTrack,newTrack);
   }
   mQueuesMutex.Unlock();
} 

///if it shares a queue/task, creates a new queue/task for the track, and removes it from any previously existing tasks.
void ODManager::MakeWaveTrackIndependent(WaveTrack* track)
{
   ODWaveTrackTaskQueue* owner=NULL;
   mQueuesMutex.Lock();
   for(unsigned int i=0;i<mQueues.size();i++)
   {
      if(mQueues[i]->ContainsWaveTrack(track))
      {
         owner = mQueues[i];
         break;
      }
   }
   if(owner)
      owner->MakeWaveTrackIndependent(track);

   mQueuesMutex.Unlock();

}

///attach the track in question to another, already existing track's queues and tasks.  Remove the task/tracks.
///only works if both tracks exist.  Sets needODUpdate flag for the task.  This is complicated and will probably need
///better design in the future.
///@return returns success.  Some ODTask conditions require that the tasks finish before merging.  
///e.g. they have different effects being processed at the same time. 
bool ODManager::MakeWaveTrackDependent(WaveTrack* dependentTrack,WaveTrack* masterTrack)
{

   //First, check to see if the task lists are mergeable.  If so, we can simply add this track to the other task and queue, 
   //then delete this one.
   ODWaveTrackTaskQueue* masterQueue=NULL;
   ODWaveTrackTaskQueue* dependentQueue=NULL;
   unsigned int dependentIndex;
   bool canMerge = false;
   
   mQueuesMutex.Lock();
   for(unsigned int i=0;i<mQueues.size();i++)
   {
      if(mQueues[i]->ContainsWaveTrack(masterTrack))
      {
         masterQueue = mQueues[i];
      }
      else if(mQueues[i]->ContainsWaveTrack(dependentTrack))
      {
         dependentQueue = mQueues[i];
         dependentIndex = i;
      }

   }
   if(masterQueue&&dependentQueue)
      canMerge=masterQueue->CanMergeWith(dependentQueue);


   //otherwise we need to let dependentTrack's queue live on.  We'll have to wait till the conflicting tasks are done.
   if(!canMerge)
   {  
      mQueuesMutex.Unlock();
      return false;
   }     
   //then we add dependentTrack to the masterTrack's queue - this will allow future ODScheduling to affect them together.
   //this sets the NeedODUpdateFlag since we don't want the head task to finish without haven't dealt with the depednent
   masterQueue->MergeWaveTrack(dependentTrack);
   
   //finally remove the dependent track
   mQueues.erase(mQueues.begin()+dependentIndex);
   mQueuesMutex.Unlock();
   delete dependentQueue; //note this locks the ODManager's current task queue.
   return true;
}


///changes the tasks associated with this Waveform to process the task from a different point in the track
///@param track the track to update
///@param seconds the point in the track from which the tasks associated with track should begin processing from.
void ODManager::DemandTrackUpdate(WaveTrack* track, double seconds)
{
   mQueuesMutex.Lock();
   for(unsigned int i=0;i<mQueues.size();i++)
   {
      mQueues[i]->DemandTrackUpdate(track,seconds);
   }
   mQueuesMutex.Unlock();

}
     
///remove tasks from ODWaveTrackTaskQueues that have been done.  Schedules new ones if they exist
///Also remove queues that have become empty.
void ODManager::UpdateQueues()
{
   mQueuesMutex.Lock();
   for(unsigned int i=0;i<mQueues.size();i++)
   {

      if(mQueues[i]->IsFrontTaskComplete())
      {
         //this should delete and remove the front task instance.
         mQueues[i]->RemoveFrontTask();
         //schedule next.
         if(!mQueues[i]->IsEmpty())
         {
            //we need to release the lock on the queue vector before using the task vector's lock or we deadlock
            //so get a temp.
            ODWaveTrackTaskQueue* queue;
            queue = mQueues[i];
            
            mQueuesMutex.Unlock();
            AddTask(queue->GetFrontTask());
            mQueuesMutex.Lock();
            
         }
      }
      
      //if the queue is empty delete it.
      if(mQueues[i]->IsEmpty())
      {
         delete mQueues[i];
         mQueues.erase(mQueues.begin()+i);
         i--;
      }
   }
   mQueuesMutex.Unlock();
   
}

//static   
///sets a flag that is set if we have loaded some OD blockfiles from PCM.  
void ODManager::MarkLoadedODFlag()
{
   sHasLoadedOD = true;
}   
   
//static   
///resets a flag that is set if we have loaded some OD blockfiles from PCM.  
void ODManager::UnmarkLoadedODFlag()
{
   sHasLoadedOD = false;
}
   
//static   
///returns a flag that is set if we have loaded some OD blockfiles from PCM.  
bool ODManager::HasLoadedODFlag()
{
   return sHasLoadedOD;
}

///fills in the status bar message for a given track
void ODManager::FillTipForWaveTrack( WaveTrack * t, const wxChar ** ppTip )
{
   mQueuesMutex.Lock();
   for(unsigned int i=0;i<mQueues.size();i++)
   {
      mQueues[i]->FillTipForWaveTrack(t,ppTip);
   }
   mQueuesMutex.Unlock();
}

///Gets the total percent complete for all tasks combined.
float ODManager::GetOverallPercentComplete()
{
   float total=0.0;
   mQueuesMutex.Lock();
   for(unsigned int i=0;i<mQueues.size();i++)
   {
      total+=mQueues[i]->GetFrontTask()->PercentComplete();
   }
   mQueuesMutex.Unlock();
   return (float) total/GetTotalNumTasks();
}
   
///Get Total Number of Tasks.
int ODManager::GetTotalNumTasks()
{
   int ret=0;
   mQueuesMutex.Lock();
   for(unsigned int i=0;i<mQueues.size();i++)
   {
      ret+=mQueues[i]->GetNumTasks();
   }
   mQueuesMutex.Unlock();
   return ret;
}
