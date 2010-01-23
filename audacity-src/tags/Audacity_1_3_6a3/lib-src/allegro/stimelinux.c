#include "../../src/Experimental.h"

#ifndef EXPERIMENTAL_NOTE_TRACK
/* stimelinux.c -- portable timer implementation for linux */

#include "stime.h"
#include "time.h"

void stime_start(int resolution)
{

}

unsigned long stime_get()
{
  return clock();
}



#endif
