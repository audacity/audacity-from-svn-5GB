/**********************************************************************

  Audacity: A Digital Audio Editor

  DiskFunctions.h

  Win: Mark Tomlinson
  Mac: Dominic Mazzoni

**********************************************************************/

/*********************************************************************
TODO: OBSOLETE
FIXME: OBSOLETE
OBSOLETE: BG: Use wxGetDiskSpace instead
*********************************************************************/

// The free-space routine returns -1 if it can't figure out the
// answer for any reason
#include <wx/longlong.h>

#ifdef __WXMSW__
wxLongLong GetFreeDiskSpace(TCHAR * path);
#else
wxLongLong GetFreeDiskSpace(const char *path);
#endif
