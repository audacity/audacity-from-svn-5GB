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
#include "Project.h"
#include <NonGuiThread.h>
#include <wx/utils.h>
#include <wx/wx.h>
#include <wx/thread.h>
bool gManagerCreated=false;

//private constructor - Singleton.
ODManager::ODManager()
{
  mTerminate = false;
}

///Adds a task to running queue.  Thread-safe.
void ODManager::AddTask(ODTask* task)
{
   mTasksMutex.Lock();
   mTasks.push_back(task);
   mTasksMutex.Unlock();
}

void ODManager::RemoveTaskIfInQueue(ODTask* task)
{
   mTasksMutex.Lock();
   for(int i=0;i<mTasks.size();i++)
   {
      if(mTasks[i]==task)
      {
         mTasks.erase(mTasks.begin()+i);
         break;
      }
   }
   mTasksMutex.Unlock();

}

void ODManager::AddTaskToWaveTrack(ODTask* task, WaveTrack* track)
{
   ODWaveTrackTaskQueue* queue = NULL;
   
   mQueuesMutex.Lock();
   for(int i=0;i<mQueues.size();i++)
   {
      if(mQueues[i]->ContainsWaveTrack(track))
         queue=mQueues[i];
   }
  
   if(queue)
   {
      //Add it to the existing queue but keep the lock since this reference can be deleted.
      queue->AddTask(task);
      mQueuesMutex.Unlock();
   }
   else
   {
      //Make a new one, add it to the local track queue, and to the immediate running task list,
      //since this task is definitely at the head
      queue = new ODWaveTrackTaskQueue(track);
      queue->AddTask(task);
      mQueues.push_back(queue);
      
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

///Launches a thread for the manager and starts accepting Tasks.
void ODManager::Init()
{
   mCurrentThreads = 0;
   mMaxThreads = 5;
   
   //   wxLogDebug(wxT("Initializing ODManager...Creating manager thread\n"));
   ODManagerHelperThread* startThread = new ODManagerHelperThread;
   
   startThread->Create();
   startThread->Run();
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
         thread->Create();
         thread->Run();
         
         mTasksMutex.Lock();
         tasksInArray = mTasks.size()>0;
         mTasksMutex.Unlock();
         
         mCurrentThreadsMutex.Lock();
      }

      mCurrentThreadsMutex.Unlock();
      ::wxMilliSleep(250);
      mTerminateMutex.Lock();
      
      //if there is some ODTask running, then there will be something in the queue.  If so then redraw to show progress
      
     // mQueuesMutex.Lock();
//      bool needsDraw = mQueues.size()>0;
//      mQueuesMutex.Unlock();
//      
//      //TODO:this is a little excessive, in the future only redraw some, and if possible only the Tracks on the trackpanel..
//      if(needsDraw)
//      {
//         wxMutexGuiEnter();
//       //  RedrawAllProjects();
//         wxMutexGuiLeave();
//      }   
   }
   mTerminateMutex.Unlock();
}

void ODManager::Quit()
{
   if(gManagerCreated)
   {
      ODManager::Instance()->mTerminateMutex.Lock();
      ODManager::Instance()->mTerminate = true;
      ODManager::Instance()->mTerminateMutex.Unlock();
   }
}
 
///Adds a wavetrack, creates a queue member. 
//void ODManager::AddWaveTrack(WaveTrack* track)
//{
//   //TODO:Check to see if the wavetrack exists in the queue
//   mQueues.push_back(new ODWaveTrackTaskQueue(track));
//}
   
///removes a wavetrack and notifies its associated tasks to stop using its reference. 
void ODManager::RemoveWaveTrack(WaveTrack* track)
{
   
   mQueuesMutex.Lock();
   for(int i=0;i<mQueues.size();i++)
   {
      if(mQueues[i]->ContainsWaveTrack(track))
         mQueues[i]->RemoveWaveTrack(track);
         //TODO:Break?
   }
   mQueuesMutex.Unlock();
}
  
     
///remove tasks from ODWaveTrackTaskQueues that have been done.  Schedules new ones if they exist
///Also remove queues that have become empty.
void ODManager::UpdateQueues()
{
   mQueuesMutex.Lock();
   for(int i=0;i<mQueues.size();i++)
   {
      if(mQueues[i]->IsFrontTaskComplete())
      {
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