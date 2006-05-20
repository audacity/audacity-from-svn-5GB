/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  RTGraph.h

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_RTGRAPH__
#define __MEZZO_RTGRAPH__

namespace Mezzo {

class RTNode;
class BufferGroup;
class Buffer;

class RTGraph {
public:
   RTGraph(int bufferSize);

   void AddNode(RTNode *node);
   void Connect(RTNode *from, int fromPort, RTNode *to, int toPort);
   int GetNumNodes();

   void Prepare();
   void Run(BufferGroup *g, float **in, float **out);
   void Finish();

private:
   RTNode *mNodes;
   struct ConnectionIntoGraph {
      RTNode *to;
      int intoGraphPort;
      int toNodePort;
      ConnectionIntoGraph *next;
   } *mIntoGraphConnections;
   struct ConnectionOutOfGraph {
      RTNode *from;
      int  fromNodePort;
      int  outOfGraphPort;
      ConnectionOutOfGraph *next;
   } *mOutOfGraphConnections;
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

