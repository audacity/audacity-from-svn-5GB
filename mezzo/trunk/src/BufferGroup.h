/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  BufferGroup.h

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_BUFFERGROUP__
#define __MEZZO_BUFFERGROUP__

namespace Mezzo {

class BufferData;

class BufferGroup {
public:
   BufferGroup(int bufferSize, int numBuffers);
   float *Allocate();
   void Recycle(float *data);

private:
   int mBufferSize;
};

}

#endif

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

