// $Id: header_tag.cpp,v 1.1 2001-07-08 09:02:39 dmazzoni Exp $

// id3lib: a C++ library for creating and manipulating id3v1/v2 tags
// Copyright 1999, 2000  Scott Thomas Haug

// This library is free software; you can redistribute it and/or modify it
// under the terms of the GNU Library General Public License as published by
// the Free Software Foundation; either version 2 of the License, or (at your
// option) any later version.
//
// This library is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// License for more details.
//
// You should have received a copy of the GNU Library General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

// The id3lib authors encourage improvements and optimisations to be sent to
// the id3lib coordinator.  Please see the README file for details on where to
// send such submissions.  See the AUTHORS file for a list of people who have
// contributed to id3lib.  See the ChangeLog file for a list of changes to
// id3lib.  These files are distributed with id3lib at
// http://download.sourceforge.net/id3lib/

#if defined HAVE_CONFIG_H
#include <config.h>
#endif



#include <string.h>

#include "header_tag.h"
#include "utils.h"
#include "tag.h"
#include "io_helpers.h"
#include "spec.h"

using namespace dami;

const char* const ID3_TagHeader::ID = "ID3";

bool ID3_TagHeader::SetSpec(ID3_V2Spec spec)
{
  bool changed = this->ID3_Header::SetSpec(spec);
  if (changed)
  {
    if (_info)
    {
      _flags.set(EXPERIMENTAL, _info->is_experimental);
      _flags.set(EXTENDED,     _info->is_extended);
    }
  }
  return changed;
}

size_t ID3_TagHeader::Size() const
{
  size_t bytesUsed = ID3_TagHeader::SIZE;
  
  if (_info->is_extended)
  {
    bytesUsed += _info->extended_bytes + sizeof(uint32);
  }
  
  return bytesUsed;
}


void ID3_TagHeader::Render(ID3_Writer& writer) const
{
  writer.writeChars((uchar *) ID, strlen(ID));

  writer.writeChar(ID3_V2SpecToVer(ID3V2_LATEST));
  writer.writeChar(ID3_V2SpecToRev(ID3V2_LATEST));
  
  // set the flags byte in the header
  writer.writeChar(static_cast<uchar>(_flags.get() & MASK8));
  io::writeUInt28(writer, this->GetDataSize());

  // now we render the extended header
  if (_flags.test(EXTENDED))
  {
    io::writeBENumber(writer, _info->extended_bytes, sizeof(uint32));
  }
}

bool ID3_TagHeader::Parse(ID3_Reader& reader)
{
  io::ExitTrigger et(reader);
  if (!ID3_Tag::IsV2Tag(reader))
  {
    ID3D_NOTICE( "ID3_TagHeader::Parse(): not an id3v2 header" );
    return false;
  }

  uchar id[3];
  reader.readChars(id, 3);
  // The spec version is determined with the MAJOR and MINOR OFFSETs
  uchar major = reader.readChar();
  uchar minor = reader.readChar();
  this->SetSpec(ID3_VerRevToV2Spec(major, minor));

  // Get the flags at the appropriate offset
  _flags.set(static_cast<ID3_Flags::TYPE>(reader.readChar()));

  // set the data size
  this->SetDataSize(io::readUInt28(reader));
  
  if (_flags.test(EXTENDED))
  {
    if (this->GetSpec() == ID3V2_2_1)
    {
      // okay, if we are ID3v2.2.1, then let's skip over the extended header
      // for now because I am lazy
    }

    if (this->GetSpec() == ID3V2_3_0)
    {
      // okay, if we are ID3v2.3.0, then let's actually parse the extended
      // header (for now, we skip it because we are lazy)
    }
  }
  et.setExitPos(reader.getCur());
  return true;
}
