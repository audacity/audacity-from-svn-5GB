/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApp.cpp

  Dominic Mazzoni

**********************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/wx.h>
#include <wx/brush.h>
#include <wx/image.h>
#endif

#include "AudacityApp.h"
#include "AButton.h"
#include "ASlider.h"
#include "APalette.h"
#include "Play.h"
#include "Project.h"
#include "effects/Amplify.h"
#include "effects/Echo.h"
#include "effects/Fade.h"

#ifdef __WXMAC__
#include "effects/LoadVSTMac.h"
#endif

#ifdef __WXGTK__
void wxOnAssert(const char* fileName, int lineNumber, const char* msg)
{
  if (msg)
	printf("ASSERTION FAILED: %s\n%s: %d\n",msg,fileName,lineNumber);
  else
	printf("ASSERTION FAILED!\n%s: %d\n",fileName,lineNumber);
  exit(0);
}
#endif

IMPLEMENT_APP(AudacityApp)

// The `main program' equivalent, creating the windows and returning the
// main frame
bool AudacityApp::OnInit()
{
  ::wxInitAllImageHandlers();

  Effect::RegisterEffect(new EffectAmplify());
  Effect::RegisterEffect(new EffectEcho());
  Effect::RegisterEffect(new EffectFadeIn());
  Effect::RegisterEffect(new EffectFadeOut());

  InitSoundPlayer();
  
  #ifdef __WXMAC__
  LoadVSTPlugins();
  #endif

  InitAPalette(NULL);
  AudacityProject *project = CreateNewAudacityProject();
  SetTopWindow(project);

  return TRUE;
}
