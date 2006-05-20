/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  RTGraph.cpp

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "RTGraph.h"

#include <cstring>
#include <stdio.h>

#include "BufferGroup.h"
#include "RTNode.h"

namespace Mezzo {

class DummyRTNode : public RTNode
{
public:
   DummyRTNode() { }

   void Run(int bufSize, BufferGroup *g, float **inBuffers, float **outBuffers)
   {
   }
};

RTGraph::RTGraph(int bufferSize):
   mNodes(new DummyRTNode),
   mIntoGraphConnections(NULL),
   mOutOfGraphConnections(NULL),
   mBufferSize(bufferSize)
{
   mNodes->next = NULL;
}

void RTGraph::AddNode(RTNode *node)
{
   node->next = NULL;
   node->connections = NULL;

   // simple append into a linked list
   RTNode *n = mNodes;
   while(n->next != NULL)
      n = n->next;

   n->next = node;
   node->next = NULL;
}

void RTGraph::Connect(RTNode *from, int fromPort, RTNode *to, int toPort)
{
   //
   // Sanity checks
   //
   if(from == NULL && to == NULL)
   {
      printf("can't connect NULL to NULL\n");
      return;
   }

   // Make sure both nodes are in this graph
   bool fromInList = false, toInList = false;
   for(RTNode *n = mNodes; n != NULL; n = n->next)
   {
      if(n == from)
         fromInList = true;
      if(n == to)
         toInList = true;

      if(fromInList && toInList)
         break;
   }

   if((!fromInList && from != NULL) || (!toInList && to != NULL))
   {
      printf("Tried to connect nodes that aren't in the list!\n");
      return;
   }

   // Make sure that the referred-to ports actually exist
   if(from && (fromPort >= from->numOutPorts))
   {
      printf("Tried to connect from port %d, but node only has %d out ports!\n",
             fromPort, from->numOutPorts);
      return;
   }

   if(to && (toPort >= to->numInPorts))
   {
      printf("Tried to connect to port %d, but node only has %d in ports!\n",
             toPort, to->numInPorts);
   }

   // create the connection
   if(from == NULL)
   {
      // we are connecting into the graph
      RTGraph::ConnectionIntoGraph *newConnection = new RTGraph::ConnectionIntoGraph;
      newConnection->to = to;
      newConnection->intoGraphPort = fromPort;
      newConnection->toNodePort = toPort;
      newConnection->next = NULL;

      if(mIntoGraphConnections == NULL)
         mIntoGraphConnections = newConnection;
      else
      {
         RTGraph::ConnectionIntoGraph *c = mIntoGraphConnections;
         while(c->next)
            c = c->next;
         c->next = newConnection;
      }
   }
   else if(to == NULL)
   {
      // we are connecting out of the graph
      RTGraph::ConnectionOutOfGraph *newConnection = new RTGraph::ConnectionOutOfGraph;
      newConnection->from = from;
      newConnection->fromNodePort = fromPort;
      newConnection->outOfGraphPort = toPort;
      newConnection->next = NULL;

      if(mOutOfGraphConnections == NULL)
         mOutOfGraphConnections = newConnection;
      else
      {
         RTGraph::ConnectionOutOfGraph *c = mOutOfGraphConnections;
         while(c->next)
            c = c->next;
         c->next = newConnection;
      }
   }
   else
   {
      RTNode::Connection *newConnection = new RTNode::Connection;
      newConnection->from = from;
      newConnection->fromTheirPort = fromPort;
      newConnection->toMyPort = toPort;
      newConnection->next = NULL;

      if(to->connections == NULL)
      {
         to->connections = newConnection;
      }
      else
      {
         RTNode::Connection *n = to->connections;
         while(n->next)
            n = n->next;
         n->next = newConnection;
      }
   }
}

void RTGraph::Prepare()
{
   // skip dummy node
   RTNode *n = mNodes->next;

   while(n)
   {
      n->Prepare();
      n = n->next;
   }
}

int RTGraph::GetNumNodes()
{
   // skip dummy node
   RTNode *n = mNodes->next;
   int count = 0;

   while(n)
   {
      count++;
      n = n->next;
   }

   return count;
}

void RTGraph::Run(BufferGroup *g, float **in, float **out)
{
   float *from[20];

   // skip dummy node
   RTNode *n = mNodes->next;
   while(n)
   {
      // if the input to the graph feeds this node, copy that
      for(RTGraph::ConnectionIntoGraph *c = mIntoGraphConnections; c != NULL; c = c->next)
         if(n == c->to && in != NULL)
            from[c->toNodePort] = in[c->intoGraphPort];

      // pull data from nodes that feed this node
      for(RTNode::Connection *c = n->connections; c != NULL; c = c->next)
         from[c->toMyPort] = c->from->tmpOutBuffers[c->fromTheirPort];

      n->Run(mBufferSize, g, from, n->tmpOutBuffers);

      n = n->next;
   }

   if(out != NULL)
      for(RTGraph::ConnectionOutOfGraph *c = mOutOfGraphConnections; c != NULL; c = c->next)
         out[c->outOfGraphPort] = c->from->tmpOutBuffers[c->fromNodePort];
}

void RTGraph::Finish()
{
   // skip dummy node
   RTNode *n = mNodes->next;

   while(n)
   {
      n->Finish();
      n = n->next;
   }
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

