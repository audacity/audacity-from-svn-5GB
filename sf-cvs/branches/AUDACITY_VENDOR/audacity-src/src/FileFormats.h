/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormats.h

  Dominic Mazzoni

**********************************************************************/

#include <wx/string.h>

#include "Audacity.h"

struct PCMFormatInfo {
   const char *name;
   const char *extension;
   int id;  // LibSndFile format ID
};

extern int            gNumPCMFormats;
extern int            gDefaultPCMFormat;
extern PCMFormatInfo  gPCMFormats[];
