/**********************************************************************

  Audacity: A Digital Audio Editor

  Audacity.h

  Dominic Mazzoni
  Joshua Haberman

********************************************************************//*!

\file Audacity.h

  This is the main include file for Audacity.  All files which need
  any Audacity-specific #defines or need to access any of Audacity's
  global functions should #include this file.

*//********************************************************************/


// Increment this every time you release a new version
#define AUDACITY_VERSION_STRING "1.3.2-beta"

// Increment this every time the prefs need to be reset
// the first part (before the r) indicates the version the reset took place
// the second part (after the r) indicates the number of times the prefs have been reset within the same version
#define AUDACITY_PREFS_VERSION_STRING "1.1.1r1"

// Don't change this unless the file format changes
// in an irrevocable way
#define AUDACITY_FILE_FORMAT_VERSION "1.3.0"

class wxWindow;

extern wxWindow *gParentWindow;

void QuitAudacity(bool bForce);
void QuitAudacity();

#ifdef __WXMAC__
#include "configmac.h"
#endif

#ifdef __WXGTK__
#include "configunix.h"
#endif

#ifdef __WXX11__
#include "configunix.h"
#endif

#ifdef __WXMSW__
#include "configwin.h"
#endif

// For compilers that support precompilation, includes "wx/wx.h".
// Mainly for MSVC developers.
//
// This precompilation is only done for non-unicode debug builds.  
// The rationale is that this is where there is the big time saving
// because that's what you build whilst debugging.
// Whilst disabling precompilation for other builds will ensure
// that missing headers that would affect other platforms do get 
// seen by MSVC developers too.

#ifndef UNICODE
#ifdef __WXDEBUG__
//#include <wx/wxprec.h>
#endif
#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 21aef079-ec47-4ff9-a359-7d159e2ba0e6

