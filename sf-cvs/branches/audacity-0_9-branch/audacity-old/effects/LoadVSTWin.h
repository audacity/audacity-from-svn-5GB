/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadVSTWin.h

  Mark Tomlinson

**********************************************************************/

#include <wx/string.h>

extern "C" {

   void LoadVSTPlugins(wxString searchDir);

};
