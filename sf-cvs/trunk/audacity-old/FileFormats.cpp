/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormats.cpp

  Dominic Mazzoni

**********************************************************************/

#include "sndfile.h"

#include "FileFormats.h"

int gNumPCMFormats = 14;
int gDefaultPCMFormat = 0;  // WAV

PCMFormatInfo  gPCMFormats[14] = {
   {"Windows Wave", "wav", SF_FORMAT_WAV},
   {"Apple/SGI AIFF", "aiff", SF_FORMAT_AIFF},
   {"Sun/NeXT AU", "au", SF_FORMAT_AU},
   {"DEC AU", "au", SF_FORMAT_AULE},
   {"Raw PCM data", "raw", SF_FORMAT_RAW},
   {"Ensoniq PARIS", "paf", SF_FORMAT_PAF},
   {"Amiga IFF/SVX8/SV16", "svx", SF_FORMAT_SVX},
   {"NIST/Sphere", "nist", SF_FORMAT_NIST},
   {"Windows Media Audio", "wma", SF_FORMAT_WMA},
   {"SEK'D Samplitude", "vip", SF_FORMAT_SMPLTD},
   {"VOC", "voc", SF_FORMAT_VOC},
   {"Sound Designer 2", "sd2", SF_FORMAT_SD2},
   {"Propellorheads Rex2", "rex2", SF_FORMAT_REX2},
   {"Berkeley/IRCAM/CARL", "ircam", SF_FORMAT_IRCAM}};
