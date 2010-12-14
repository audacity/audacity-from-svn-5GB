/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumPrefs.h

  Dominic Mazzoni
  James Crook

**********************************************************************/
/*
  Salvo Ventura
  November 2006

  Added selection box for windowType

  All params are saved in config file.
*/


#ifndef __AUDACITY_SPECTRUM_PREFS__
#define __AUDACITY_SPECTRUM_PREFS__

#include <wx/defs.h>
#include <wx/string.h>

#include "PrefsPanel.h"
#include "../Experimental.h"

class wxWindow;
class ShuttleGui;

class SpectrumPrefs:public PrefsPanel
{
public:
   SpectrumPrefs(wxWindow * parent);
   ~SpectrumPrefs();
   virtual bool Apply();

private:
   void Populate();
   void PopulateOrExchange( ShuttleGui & S );
   wxString minFreqStr;
   wxString maxFreqStr;
   int windowType;

#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
   int fftSkipPoints;
#endif //EXPERIMENTAL_FFT_SKIP_POINTS
#ifdef EXPERIMENTAL_FFT_Y_GRID
   bool fftYGrid;
#endif //EXPERIMENTAL_FFT_Y_GRID
#ifdef EXPERIMENTAL_FIND_NOTES
   bool fftFindNotes;
   bool findNotesQuantize;
   wxString findNotesMinAStr;
   int findNotesMinA;
   wxString findNotesNStr;
   int findNotesN;
#endif
};

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
// arch-tag: d68b1a74-12e3-49a5-a50b-3ba6fe65b40b

