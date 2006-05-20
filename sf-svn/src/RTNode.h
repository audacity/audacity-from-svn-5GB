/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  RTNode.h

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_RTNODE__
#define __MEZZO_RTNODE__

#include <stdlib.h>

namespace Mezzo {

class BufferGroup;
class Buffer;

class RTNode {
public:
   RTNode():next(NULL),connections(NULL),numInPorts(0),numOutPorts(0),tmpOutBuffers(NULL){}
   virtual void Prepare() { }
   virtual void Run(int bufSize, BufferGroup *g, float **inBuffers, float **outBuffers) = 0;
   virtual void Finish() { }

protected:
   void SetNumPorts(int inPorts, int outPorts)
   {
      if(tmpOutBuffers) delete tmpOutBuffers;
      tmpOutBuffers = new float*[outPorts];

      numInPorts = inPorts;
      numOutPorts = outPorts;
   }

private:
   class RTNode *next;
   struct Connection {
      RTNode *from;
      int fromTheirPort;
      int toMyPort;
      Connection *next;
   } *connections;
   int numInPorts, numOutPorts;
   float **tmpOutBuffers;
   friend class RTGraph;
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

