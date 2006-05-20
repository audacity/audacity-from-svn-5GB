/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  Types.h

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_TYPES__
#define __MEZZO_TYPES__

#include <posh/posh.h>

#include <string>

#ifdef POSH_64BIT_INTEGER

// For use in situations where 32 bits could be a limiting factor.  For example, Buffer
// does not need to use it because it makes no sense to have a buffer longer than two
// million samples long.  SeqBlock does not need it.  Sequence does, and WaveTrack does.
typedef posh_i64_t long_sample_count;
#define LONG_SAMPLE_COUNT_MAX POSH_I64_MAX

inline std::string LongSampleCountToStr(long_sample_count num)
{
   char tmp[100];
   snprintf(tmp, sizeof(tmp), "%lld", num);
   return std::string(tmp);
}

inline long_sample_count StrToLongSampleCount(std::string str)
{
   return strtoll(str.c_str(), NULL, 0);
}

#else

typedef posh_i32_t long_sample_count;
#define LONG_SAMPLE_COUNT_MAX POSH_I32_MAX

inline std::string LongSampleCountToStr(long_sample_count num)
{
   char tmp[100];
   snprintf(tmp, sizeof(tmp), "%d", num);
   return std::string(tmp);
}

inline long_sample_count StrToLongSampleCount(std::string str)
{
   return strtol(str.c_str(), NULL, 0);
}

#endif

#endif

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

