// $Id: tag_parse_v1.cpp,v 1.1 2001-07-08 09:03:52 dmazzoni Exp $

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


#include "tag_impl.h"
#include "helpers.h"
#include "utils.h"
#include "io_decorators.h"
#include "io_helpers.h"
#include "io_strings.h"

using namespace dami;

bool id3::v1::parse(ID3_TagImpl& tag, ID3_Reader& reader)
{
  io::ExitTrigger et(reader);
  
  ID3_Reader::pos_type end = reader.getCur();
  // posn ourselves at 128 bytes from the current position
  if (end < reader.getBeg() + ID3_V1_LEN)
  {
    ID3D_NOTICE( "id3::v1::parse: not enough bytes to parse, pos = " << end );
    return false;
  }
  reader.setCur(end - ID3_V1_LEN);
  ID3_Reader::pos_type beg = reader.getCur();
  //file.seekg(-static_cast<long>(ID3_V1_LEN), ios::cur);
  if (end != beg + ID3_V1_LEN)
  {
    ID3D_WARNING( "id3::v1::parse: failed to reposition " << ID3_V1_LEN << 
                  " bytes" );
    return false;
  }
  
  // read the next 128 bytes in;
  String id = io::readText(reader, ID3_V1_LEN_ID);
  
  // check to see if it was a tag
  if (id != "TAG")
  {
    return false;
  }
  et.setExitPos(beg);
  
  // guess so, let's start checking the v2 tag for frames which are the
  // equivalent of the v1 fields.  When we come across a v1 field that has
  // no current equivalent v2 frame, we create the frame, copy the data
  // from the v1 frame and attach it to the tag
  ID3D_NOTICE("id3::v1::parse: read bytes: " << reader.getCur() - beg);
  String title = io::readTrailingSpaces(reader, ID3_V1_LEN_TITLE);
  if (title.size() > 0)
  {
    id3::v2::setTitle(tag, title);
  }
  ID3D_NOTICE( "id3::v1::parse: title = \"" << title << "\"" );
  
  ID3D_NOTICE("id3::v1::parse: read bytes: " << reader.getCur() - beg);
  String artist = io::readTrailingSpaces(reader, ID3_V1_LEN_ARTIST);
  if (artist.size() > 0)
  {
    id3::v2::setArtist(tag, artist);
  }
  ID3D_NOTICE( "id3::v1::parse: artist = \"" << artist << "\"" );
  
  ID3D_NOTICE("id3::v1::parse: read bytes: " << reader.getCur() - beg);
  String album = io::readTrailingSpaces(reader, ID3_V1_LEN_ALBUM);
  if (album.size() > 0) 
  {
    id3::v2::setAlbum(tag, album);
  }
  ID3D_NOTICE( "id3::v1::parse: album = \"" << title << "\"" );
  
  ID3D_NOTICE("id3::v1::parse: read bytes: " << reader.getCur() - beg);
  String year = io::readTrailingSpaces(reader, ID3_V1_LEN_YEAR);
  if (year.size() > 0)
  {
    id3::v2::setYear(tag, year);
  }
  ID3D_NOTICE( "id3::v1::parse: year = \"" << year << "\"" );
  
  ID3D_NOTICE("id3::v1::parse: read bytes: " << reader.getCur() - beg);
  String comment = io::readTrailingSpaces(reader, ID3_V1_LEN_COMMENT);
  if (comment.length() == ID3_V1_LEN_COMMENT  &&
      '\0' == comment[ID3_V1_LEN_COMMENT - 2] ||
      '\0' != comment[ID3_V1_LEN_COMMENT - 1])
  {
    // This is an id3v1.1 tag.  The last byte of the comment is the track
    // number.  
    size_t track = comment[ID3_V1_LEN_COMMENT - 1];
    id3::v2::setTrack(tag, track, 0);
    ID3D_NOTICE( "id3::v1::parse: track = \"" << track << "\"" );

    ID3D_NOTICE( "id3::v1::parse: comment length = \"" << comment.length() << "\"" );
    io::StringReader sr(comment);
    comment = io::readTrailingSpaces(sr, ID3_V1_LEN_COMMENT - 2);
  }
  ID3D_NOTICE( "id3::v1::parse: comment = \"" << comment << "\"" );
  if (comment.size() > 0)
  {
    id3::v2::setComment(tag, comment, STR_V1_COMMENT_DESC, "XXX");
  }
  
  ID3D_NOTICE("id3::v1::parse: read bytes: " << reader.getCur() - beg);
  // the GENRE field/frame
  uchar genre = reader.readChar();
  if (genre != 0xFF) 
  {
    id3::v2::setGenre(tag, genre);
  }
  ID3D_NOTICE( "id3::v1::parse: genre = \"" << (int) genre << "\"" );

  ID3D_NOTICE("id3::v1::parse: read bytes: " << reader.getCur() - beg);
  return true;
}
