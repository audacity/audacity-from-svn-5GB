/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  RTDiskIONode.cpp

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "RTDiskIONode.h"

#include "Sequence.h"
#include "BufferGroup.h"

extern "C" {
#include "srsw_queue.h"
}

namespace Mezzo {

RTDiskIONode::RTDiskIONode(Sequence *s):
   mSequence(s),
   mOffset(0),
   mQueue(srsw_queue_new())
{
   SetNumPorts(0, 1);
}

void RTDiskIONode::DiskIOThread(int bufSize, BufferGroup *g)
{
   //while(1)
   //{
      if(srsw_queue_get_count(mQueue) > 20)
         return;

      if(mOffset + bufSize > mSequence->GetLength())
         mOffset = 0;

      float *buf = g->Allocate();
      FloatBuffer seqBuf = mSequence->Get(mOffset, bufSize).AsFloat();
      seqBuf.Get(buf, 0, bufSize);

      srsw_queue_enqueue(mQueue, buf);
      mOffset += bufSize;
      //printf("pushed buffer %p\n", buf);
   //}
}

void RTDiskIONode::Run(int bufSize, BufferGroup *g, float **inBuffers, float **outBuffers)
{
   void *out;

   if(srsw_queue_dequeue(mQueue, &out) == 0)
   {
      /* bad! */
      printf("failed to get any buffers!\n");
   }
   else
   {
      //printf("got a buffer!\n");
   }

   outBuffers[0] = (float*)out;
}

} // namespace Mezzo

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

