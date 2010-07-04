/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORT__
#define __AUDACITY_EXPORT__

#include "../AudacityBranding.h"

class AudacityProject;
class DirManager;
class WaveTrack;

bool Export(AudacityProject *project,
            bool selectionOnly, double t0, double t1
            #if (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
               , bool bWantExit = false
            #endif
            );

bool ExportLossy(AudacityProject *project,
                 bool selectionOnly, double t0, double t1);

#endif
