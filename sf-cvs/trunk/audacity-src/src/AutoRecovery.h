#ifndef __AUDACITY_AUTORECOVERY__
#define __AUDACITY_AUTORECOVERY__

#include "Project.h"

//
// Show auto recovery dialog if there are projects to recover. Should be
// called once at Audacity startup.
//
// This function possibly opens new project windows while it recovers all
// projects. If so, it will re-use *pproj, if != NULL and set it to NULL.
//
// Returns: True, if the start of Audacity should continue as normal
//          False if Audacity should be quit immediately
//
bool ShowAutoRecoveryDialogIfNeeded(AudacityProject** pproj);

#endif
