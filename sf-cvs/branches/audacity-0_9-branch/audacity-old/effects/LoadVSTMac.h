/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadVSTMac.h

  Dominic Mazzoni

**********************************************************************/

#include <wx/string.h>

extern "C" {

   void LoadVSTPlugins(wxString searchDir);

};
