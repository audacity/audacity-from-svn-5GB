/**********************************************************************

  Audacity: A Digital Audio Editor

  ODTask.cpp

  Created by Michael Chinen (mchinen)
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODTask
\brief ODTask is an abstract class that outlines the methods that will be used to
support On-Demand background loading of files.  These ODTasks are generally meant to be run
in a background thread.

*//*******************************************************************/


#include "ODTask.h"
#include "ODManager.h"
#include "../Project.h"


DEFINE_EVENT_TYPE(EVT_ODTASK_COMPLETE)

/// Constructs an ODTask
ODTask::ODTask()
{

   static int sTaskNumber=0;
   mPercentComplete=0;
   mDoingTask=false;
   mTerminate = false;
   
   mTaskNumber=sTaskNumber++;
}


void ODTask::TerminateAndBlock()
{  
   //one mutex pair for the value of mTerminate
   mTerminateMutex.Lock();
   mTerminate=true;
   mTerminateMutex.Unlock();
   
   //and one mutex pair for the exit of the function
   mBlockUntilTerminateMutex.Lock();
//TODO lock mTerminate?
   mBlockUntilTerminateMutex.Unlock();
}
   
///Do a modular part of the task.  For example, if the task is to load the entire file, load one BlockFile.
///Relies on DoSomeInternal(), which is the subclasses must implement.
///@param amountWork the percent amount of the total job to do.  1.0 represents the entire job.  the default of 0.0
/// will do the smallest unit of work possible
void ODTask::DoSome(float amountWork)
{
   mBlockUntilTerminateMutex.Lock();

//   printf("%s %i subtask starting on new thread with priority\n", GetTaskName(),GetTaskNumber());

   mDoingTask=mTaskStarted=true;
   
   float workUntil = amountWork+PercentComplete();
   
   if(workUntil<PercentComplete())
      workUntil = PercentComplete();
   
   //check periodically to see if we should exit.
   mTerminateMutex.Lock();
   if(mTerminate)
   {
      mBlockUntilTerminateMutex.Unlock();
      mTerminateMutex.Unlock();
      return;
   }  
   mTerminateMutex.Unlock();

   Update();   
   
   //Do Some of the task.
   
   mTerminateMutex.Lock();
   while(PercentComplete() < workUntil && PercentComplete() < 1.0 && !mTerminate)
   {
      wxThread::This()->Yield();
      //release within the loop so we can cut the number of iterations short
      mTerminateMutex.Unlock();
      //TODO: check to see if ondemand has been called
      if(false)
         ODUpdate();
      DoSomeInternal();
      
      //But add the mutex lock back before we check the value again.
      mTerminateMutex.Lock();
   }
   mTerminateMutex.Unlock();
   mDoingTask=false;
   
   mTerminateMutex.Lock();
   //if it is not done, put it back onto the ODManager queue.
   if(!IsComplete() && !mTerminate)
   {
      ODManager::Instance()->AddTask(this);
 //     printf("%s %i is %f done\n", GetTaskName(),GetTaskNumber(),PercentComplete());
   }
   else
   {
      wxCommandEvent event( EVT_ODTASK_COMPLETE );
      AudacityProject::AllProjectsDeleteLock();
      AudacityProject* proj = GetActiveProject();
      if(proj)
         proj->AddPendingEvent( event );
      AudacityProject::AllProjectsDeleteUnlock();

  //    printf("%s %i complete\n", GetTaskName(),GetTaskNumber());
   }
   mTerminateMutex.Unlock();
   mBlockUntilTerminateMutex.Unlock();
   
}
   
///return the amount of the task that has been completed.  0.0 to 1.0
float ODTask::PercentComplete()
{
   mPercentCompleteMutex.Lock();
   float ret = mPercentComplete;
   mPercentCompleteMutex.Unlock();
   return ret;
}
   
///return 
bool ODTask::IsComplete()
{
   return PercentComplete() >= 1.0;
}