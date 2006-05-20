/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  DiskReaderNode.h

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_DISKREADERNODE__
#define __MEZZO_DISKREADERNODE__

namespace Mezzo {

class BufferGroup;
class Buffer;

class DiskReaderNode : public RTNode {
public:
   void Run(BufferGroup *g, Buffer *inBuffers, Buffer *outBuffers)
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

