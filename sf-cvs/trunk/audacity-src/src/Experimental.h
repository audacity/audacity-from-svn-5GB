/**********************************************************************

  Audacity: A Digital Audio Editor

  Experimental.h

  Dominic Mazzoni
  James Crook

  Used for includes and #defines for experimental features.

  When the features become mainstream the include files will 
  move out of here and into the files which need them.  The
  #defines will then be retired.



  JKC: This file solves a problem of how to avoid forking the
  code base when working on new features e.g:
    - Additional displays in Audacity
    - Modular architecture.
  Add #defines in here for the new features, and make your code
  conditional on those #defines.

**********************************************************************/

#ifndef __EXPERIMENTAL__
#define __EXPERIMENTAL__

//Uncomment the next #define to enable experimental features.
//#define EXPERIMENTAL_FEATURES

// The Experimental TRackPanel is a refactoring of the existing track panel.
//#define EXPERIMENTAL_TRACK_PANEL

#ifdef EXPERIMENTAL_FEATURES
   // The first experimental feature is a notebook that adds
   // a tabbed divider to the project.
   #define EXPERIMENTAL_NOTEBOOK
   // The notebook in turn can contain:
   // 1. The Nyquist Inspector, which is a browser for the objects in 
   // Audacity.
   #define EXPERIMENTAL_NYQUIST_INSPECTOR
   // 2. The Vocal Studio, a screen for working with vocal sounds
   // particularly vowel sounds.
   #define EXPERIMENTAL_VOCAL_STUDIO
   // 3. The Audacity Tester is an extended version of the benchmarks
   // display.  The crucial idea is to be able to compare waveforms
   // where effects have been applied by audacity but using different 
   // block-sizes.  This should give high confidence that we don't
   // suffer from end-effects on buffers, e.g. losing one sample on
   // each buffer.
   #define EXPERIMENTAL_AUDACITY_TESTER

   // A long term plan is to use dso's and dlls for Audacity extensions
   // These are 'WX' plug ins that manage their own displays using
   // wxWindows.
   #define EXPERIMENTAL_WX_PLUG_INS
#endif

//If you want any of these files, ask JKC.  They are not
//yet checked in to Audacity CVS as of 10-Oct-2004
#ifdef EXPERIMENTAL_NOTEBOOK
#include "widgets/GuiFactory.h"
#include "widgets/APanel.h"
extern void AddPages(   AudacityProject * pProj, GuiFactory & Factory,  wxNotebook  * pNotebook );
#endif

#ifdef EXPERIMENTAL_NYQUIST_INSPECTOR
#include "NyquistAdapter.h"
#endif

#ifdef EXPERIMENTAL_VOCAL_STUDIO
#include "VowelQuad.h"
#include "AboveBarText.h"
#include "BarChart.h"
#include "InfoWindow.h"
#include "ButtonWindow.h"
#endif

#ifdef EXPERIMENTAL_AUDACITY_TESTER
#endif

#ifdef EXPERIMENTAL_WX_PLUG_INS
#endif

#ifdef EXPERIMENTAL_TRACK_PANEL
#include "TrackPanel2.h"
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
// arch-tag:

