/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  Buffer.cpp

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "BufferGroup.h"

#include "Buffer.h"

namespace Mezzo {

BufferGroup::BufferGroup(int bufferSize, int numBuffers):
   mBufferSize(bufferSize)
{
}

float *BufferGroup::Allocate()
{
   return new float[mBufferSize];
}

void BufferGroup::Recycle(float *data)
{
   delete[] data;
}

}

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

