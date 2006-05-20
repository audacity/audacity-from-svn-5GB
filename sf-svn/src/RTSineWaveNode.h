/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  RTSineWaveNode.h

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_SINEWAVENODE__
#define __MEZZO_SINEWAVENODE__

#include "RTNode.h"

namespace Mezzo {

class RTSineWaveNode : public RTNode {
public:
   RTSineWaveNode(float freq);

   void Run(int bufSize, BufferGroup *g, float **inBuffers, float **outBuffers);

private:
   float mFreq;
   float mPhase;
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

