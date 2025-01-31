/**********************************************************************

  Audacity: A Digital Audio Editor

  ODManager.h
  
  Created by Michael Chinen (mchinen) on 6/8/08
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODManager
\brief A singleton that manages currently running Tasks on an arbitrary 
number of threads.

*//*******************************************************************/





#ifndef __AUDACITY_ODMANAGER__
#define __AUDACITY_ODMANAGER__
#include <vector>
#include "ODTask.h"
#include "ODTaskThread.h"
#include <wx/thread.h>

#include <wx/wx.h>

#ifdef __WXMAC__

// On Mac OS X, it's better not to use the wxThread class.
// We use our own implementation based on pthreads instead.

#include <pthread.h>
#include <time.h>
#endif //__WXMAC__


DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_ODTASK_UPDATE, -1)

/// A singleton that manages currently running Tasks on an arbitrary 
/// number of threads.
class WaveTrack;
class ODWaveTrackTaskQueue;
class ODManager
{
 public:

   virtual ~ODManager(){};
   
  
   ///Gets the singleton instance
   static ODManager* Instance();
   
   ///Kills the ODMananger Thread.
   static void Quit();   
   
   ///changes the tasks associated with this Waveform to process the task from a different point in the track
   void DemandTrackUpdate(WaveTrack* track, double seconds);
   
   ///Reduces the count of current threads running.  Meant to be called when ODTaskThreads end in their own threads.  Thread-safe.
   void DecrementCurrentThreads();
   
   ///Adds a wavetrack, creates a queue member. 
   void AddNewTask(ODTask* task, bool lockMutex=true);
   
   ///removes a wavetrack and notifies its associated tasks to stop using its reference. 
   void RemoveWaveTrack(WaveTrack* track);
   
   ///if it shares a queue/task, creates a new queue/task for the track, and removes it from any previously existing tasks.
   void MakeWaveTrackIndependent(WaveTrack* track);
   
   ///attach the track in question to another, already existing track's queues and tasks.  Remove the task/tracks.
   ///Returns success if it was possible..  Some ODTask conditions make it impossible until the Tasks finish.
   bool MakeWaveTrackDependent(WaveTrack* dependentTrack,WaveTrack* masterTrack);
   
   ///replace the functional instance of wavetrack in tasks with another one (keeps oldTrack's gui reference)
   //TODO: this is complicated because concurrent tasks/effects will write over the same blockfile.  Thus if the 
   //compute summary task goes last the effect will be overwritten.
   ///void ReplaceTaskWaveTrack(WaveTrack* oldTrack,WaveTrack* newTrack);
   
   ///replace the wavetrack whose wavecache the gui watches for updates
   void ReplaceWaveTrack(WaveTrack* oldTrack,WaveTrack* newTrack); 
   
   ///Adds a task to the running queue.  Threas-safe.
   void AddTask(ODTask* task);
   
   void RemoveTaskIfInQueue(ODTask* task);
   
   ///sets a flag that is set if we have loaded some OD blockfiles from PCM.  
   static void MarkLoadedODFlag();
   
   ///resets a flag that is set if we have loaded some OD blockfiles from PCM.  
   static void UnmarkLoadedODFlag();
   
   ///returns a flag that is set if we have loaded some OD blockfiles from PCM.  
   static bool HasLoadedODFlag();
   
   ///returns whether or not the singleton instance was created yet
   static bool IsInstanceCreated();
   
   ///fills in the status bar message for a given track
   void FillTipForWaveTrack( WaveTrack * t, const wxChar ** ppTip );
   
   ///Gets the total percent complete for all tasks combined.
   float GetOverallPercentComplete();
   
   ///Get Total Number of Tasks.
   int GetTotalNumTasks();
  
  protected:
   //private constructor - Singleton.
   ODManager();
   ///Launches a thread for the manager and starts accepting Tasks.
   void Init();

   ///Start the main loop for the manager.
   void Start();
   
   ///Remove references in our array to Tasks that have been completed/Schedule new ones
   void UpdateQueues();
 
   
   //List of tracks and their active and inactive tasks.
   std::vector<ODWaveTrackTaskQueue*> mQueues;
   ODLock mQueuesMutex;
   
   //List of current Task to do.
   std::vector<ODTask*> mTasks;
   //mutex for above variable
   ODLock mTasksMutex;

   int mNeedsDraw;
   
   ///Number of threads currently running.   Accessed thru multiple threads
   int mCurrentThreads;
   //mutex for above variable
   ODLock mCurrentThreadsMutex;
   
   
   ///Maximum number of threads allowed out.
   int mMaxThreads;
   
   bool mTerminate;
   ODLock mTerminateMutex;
   
   
#ifdef __WXMAC__

// On Mac OS X, it's better not to use the wxThread class.
// We use our own implementation based on pthreads instead.

class ODManagerHelperThread {
 public:
   typedef int ExitCode;
   ODManagerHelperThread() { mDestroy = false; mThread = NULL; }
  /* ExitCode*/ void Entry(){
         ODManager::Instance()->Start();
   }

   void Create() {}
   void Delete() {
      mDestroy = true;
      pthread_join(mThread, NULL);
   }
   bool TestDestroy() { return mDestroy; }
   void Sleep(int ms) {
      struct timespec spec;
      spec.tv_sec = 0;
      spec.tv_nsec = ms * 1000 * 1000;
      nanosleep(&spec, NULL);
   }
   static void *callback(void *p) {
      ODManagerHelperThread *th = (ODManagerHelperThread *)p;
      /* return (void *) */th->Entry();
   }
   
   ///Specifies the priority the thread will run at.  Currently doesn't work.
   ///@param priority value from 0 (min priority) to 100 (max priority)
   void SetPriority(int priority)
   {
      mPriority=priority;
   }
   
   void Run() {
      pthread_create(&mThread, NULL, callback, this);
   }
 private:
   bool mDestroy;
   pthread_t mThread;
   
   int mPriority;

};

#else

   class ODManagerHelperThread : public wxThread  
   {
      public:
      ///Constructs a ODTaskThread
      ///@param task the task to be launched as an 
      ODManagerHelperThread(): wxThread(){}
   
      protected:
      ///Executes a part of the task
      void *Entry()
      {
         ODManager::Instance()->Start();
         return NULL;
      }
     
   };
   
#endif //__WXMAC__




};

#endif

