/**********************************************************************

  Audacity: A Digital Audio Editor

  Play.h

  Dominic Mazzoni

  Note: This one header file, Play.h, is associated with many
  different sound playing modules, such as PlayLinux.cpp,
  PlayMac.cpp, PlayWin.cpp, and later much more.

**********************************************************************/


#ifdef __WXGTK__
#include "PlayLinux.h"
#endif

#ifdef __WXMAC__
#include "PlayMac.h"
#endif

#ifdef __WXMSW__
#include "PlaySnd.h"
//#include "PlayWin.h"
#endif


