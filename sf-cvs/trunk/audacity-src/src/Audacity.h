/**********************************************************************

  Audacity: A Digital Audio Editor

  Audacity.h

  Dominic Mazzoni
  Joshua Haberman

  This is the main include file for Audacity.  All files which need
  any Audacity-specific #defines or need to access any of Audacity's
  global functions should #include this file.

**********************************************************************/

// Increment this every time you release a new version
#define AUDACITY_VERSION_STRING "1.1.1"

// Don't change this unless the file format changes
// in an irrevocable way
#define AUDACITY_FILE_FORMAT_VERSION "1.1.0"

class wxWindow;
class ToolBarStub;

extern wxWindow *gParentWindow;
extern ToolBarStub *gControlToolBarStub;
extern ToolBarStub *gEditToolBarStub;

void QuitAudacity();

#ifdef __WXMAC__
#include "configmac.h"
#endif

#ifdef __WXGTK__
#include "configunix.h"
#endif

#ifdef __WXMSW__
#include "configwin.h"
#endif
