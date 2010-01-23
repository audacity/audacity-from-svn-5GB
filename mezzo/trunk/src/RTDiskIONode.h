/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  RTDiskIONode.h

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_DISKIONODE__
#define __MEZZO_DISKIONODE__

#include "RTNode.h"
#include "Types.h"

struct SRSW_Queue;

namespace Mezzo {

class Sequence;

class RTDiskIONode : public RTNode {
public:
   RTDiskIONode(Sequence *s);
   void Run(int bufSize, BufferGroup *g, float **inBuffers, float **outBuffers);
   void DiskIOThread(int bufSize, BufferGroup *g);

private:
   Sequence *mSequence;
   long_sample_count mOffset;
   SRSW_Queue *mQueue;
};

} // namespace Mezzo

#endif

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

