/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseRemoval.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NOISE_REMOVAL__
#define __AUDACITY_EFFECT_NOISE_REMOVAL__

#include <wx/bitmap.h>
#include <wx/button.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/stattext.h>

class wxString;

#include "Effect.h"

class WaveTrack;

class EffectNoiseRemoval: public Effect {
   
public:
   
   EffectNoiseRemoval();
   
   virtual wxString GetEffectName() {
      return wxString("Noise Removal...");
   }
   
   virtual wxString GetEffectAction() {
      if (doProfile)
         return wxString("Creating Noise Profile");
      else
         return wxString("Removing Noise");
   }
   
   virtual bool PromptUser();
   
   virtual bool Process();
   
private:
   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);

   void GetProfile(sampleCount len,
                   sampleType *buffer);
   void RemoveNoise(sampleCount len,
                    sampleType *buffer, bool first);   
   
   int       windowSize;
   float    *noiseGate;
   float    *sum;
   float    *sumsq;
   float    *smoothing;
   int      *profileCount;
   bool      doProfile;
   bool      hasProfile;
   
   int       level;
};

// WDR: class declarations

//----------------------------------------------------------------------------
// NoiseRemovalDialog
//----------------------------------------------------------------------------

// Declare window functions

#define ID_TEXT 10000
#define ID_FILTERPANEL 10001
#define ID_CLEAR 10002

class NoiseRemovalDialog: public wxDialog
{
public:
   // constructors and destructors
   NoiseRemovalDialog( wxWindow *parent, wxWindowID id, const wxString &title,
                       const wxPoint& pos = wxDefaultPosition,
                       const wxSize& size = wxDefaultSize,
                       long style = wxDEFAULT_DIALOG_STYLE );

   wxSizer *MakeNoiseRemovalDialog( wxPanel *parent, bool call_fit = TRUE,
                           bool set_sizer = TRUE );
   
   wxButton *mRemoveNoiseButton;
   wxSlider *mSlider;
   
private:
   // WDR: handler declarations for FilterDialog
   void OnGetProfile( wxCommandEvent &event );
   void OnRemoveNoise( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
   
private:
   DECLARE_EVENT_TABLE()
};

#endif
