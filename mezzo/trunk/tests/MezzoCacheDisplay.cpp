/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  MezzoPlay.cpp

  Copyright (c) 2004 Dominic Mazzoni

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "Sequence.h"
#include "ManagedFileContext.h"
#include "DiskIO.h"
#include "BufferGroup.h"
#include "XMLLoadStore.h"

#include <string>

extern "C" {
#include "srsw_queue.h"
}

using namespace Mezzo;

ManagedFileContext *gContext;
BlockedSequence *gSequence;

void *DiskIOThreadFunc(void *arg)
{
   //gDiskIO->Run();
   while(1) {
      ManagedFileContext::LoadBackgroundRequests();
   }

   return NULL;
}

void StartDiskIOThread()
{
   pthread_t thread;

   if (pthread_create(&thread, NULL,
                      DiskIOThreadFunc, NULL)) {
      fprintf(stderr, "Fatal error: could not create Disk I/O thread!\n");
      exit(0);
   }   
}

int main(int argc, char **argv)
{
   if (argc < 2) {
      printf("Usage: %s <Mezzo Project XML File>\n",
             argv[0]);
      return -1;
   }

   XMLLoader loader(argv[1]);
   loader.GetNextToken();

   gContext = new ManagedFileContext(loader);
   gSequence = new BlockedSequence(loader);

   StartDiskIOThread();

   int numSamples = gSequence->GetLength();
   int totalWidth = 72;
   double samplesPerPixel = numSamples / (double)totalWidth;

   int screenLeft = 0;
   int screenWidth = 30;
   int tick = 0;

   for(;;) {
      struct timespec sleeptime;
      sleeptime.tv_sec = 0;
      sleeptime.tv_nsec = 100000000; /* 100 million nanosecs = 0.1 secs */

      if ((rand()%20)==0)
         screenLeft = (rand() % (totalWidth - screenWidth));

      FloatBuffer min(0), max(0), rms(0);
      Int16Buffer flag(0);

      int left = (int)(screenLeft*samplesPerPixel);
      int len = (int)(screenWidth*samplesPerPixel);

      gSequence->GetWaveDisplay(min, max, rms, flag,
                                left, len, screenWidth,
                                samplesPerPixel);

      printf("%03d ", (tick%1000));
      tick++;

      int i;
      for(i=0; i<totalWidth; i++) {
         if (i < screenLeft || i >= screenLeft+screenWidth)
            printf(".");
         else if (flag[i-screenLeft])
            printf("X");
         else
            printf("-");
      }
      printf("\n");

      nanosleep(&sleeptime, NULL);
   }
   
   return 0;
}

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

