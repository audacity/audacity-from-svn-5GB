/**********************************************************************

  Audacity: A Digital Audio Editor

  Keyboard.h

  Dominic Mazzoni
  Brian Gunlogson

**********************************************************************/

#include <wx/defs.h>
#include <wx/event.h>
#include <wx/string.h>

#if defined(__WXMAC__)
#include <wx/accel.h>
#include <wx/sysopt.h>

#if !defined(wxMAC_SEPARATE_COMMAND_AND_CONTROL)
#warning Your wxWidgets library does not contains needed support.  You must
#warning apply the patch found in audacity/mac/wxMac-2.8.9.patch and rebuild
#warning wxWidgets.

#define wxMAC_SEPARATE_COMMAND_AND_CONTROL _T("mac.separate-command-and-control")
#define USE_SEPARATE_COMMAND_AND_CONTROL false
#else
#define USE_SEPARATE_COMMAND_AND_CONTROL true
#endif
#endif

wxString KeyStringNormalize(const wxString & key);
wxString KeyStringDisplay(const wxString & key);
wxString KeyEventToKeyString(const wxKeyEvent & keyEvent);

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: c4be8eb8-6ee6-4831-bedf-072750e88f23

