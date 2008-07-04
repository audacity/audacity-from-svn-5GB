/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadLV2.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

*********************************************************************/

#include <slv2/slv2.h>

extern SLV2World gWorld;

// These are needed for comparisons
extern SLV2Value gAudioPortClass;
extern SLV2Value gControlPortClass;
extern SLV2Value gInputPortClass;
extern SLV2Value gOutputPortClass;


void LoadLV2Plugins();
void UnloadLV2Plugins();

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 21a6d3fe-1003-4cec-a44c-4a16335b5cda

