/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  RTSineWaveNode.h

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "RTSineWaveNode.h"
#include "BufferGroup.h"

#include <cmath>

namespace Mezzo {

RTSineWaveNode::RTSineWaveNode(float freq):
   mFreq(freq),
   mPhase(0)
{
   SetNumPorts(0, 1);
}

void RTSineWaveNode::Run(int bufSize, BufferGroup *g, float **inBuffers, float **outBuffers)
{
   float *buf = g->Allocate();

   for(int i = 0; i < bufSize; i++)
   {
      buf[i] = sin(mPhase);
      mPhase += 2 * M_PI * mFreq / 44100.0;
   }

   outBuffers[0] = buf;

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

