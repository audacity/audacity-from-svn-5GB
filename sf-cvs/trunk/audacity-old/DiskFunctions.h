/**********************************************************************

  Audacity: A Digital Audio Editor

  DiskFunctions.h

  Win: Mark Tomlinson
  Mac: Dominic Mazzoni

**********************************************************************/

// The free-space routine returns -1 if it can't figure out the
// answer for any reason
#ifdef __WXMSW__
long GetFreeDiskSpace( TCHAR *path );
#else
long GetFreeDiskSpace( char *path );
#endif

