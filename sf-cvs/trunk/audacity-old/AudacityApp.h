/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApp.h

  Dominic Mazzoni

  This is the main source file for Audacity which handles
  initialization and termination by subclassing wxApp.

**********************************************************************/

#include <wx/app.h>

void QuitAudacity();

class AudacityApp: public wxApp
{
public:
  bool OnInit(void);

private:
  void RunTest();
};
