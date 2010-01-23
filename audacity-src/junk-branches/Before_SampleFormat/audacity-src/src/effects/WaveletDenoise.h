/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveletDenoise.h

  Matt Brubeck

**********************************************************************/

#ifndef __AUDACITY_EFFECT_WAVELET_DENOISING__
#define __AUDACITY_EFFECT_WAVELET_DENOISING__

#include <wx/bitmap.h>
#include <wx/button.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/intl.h>
#include <libw.h>

class wxString;

#include "Effect.h"

class Envelope;
class WaveTrack;

class EffectWaveletDenoise: public Effect {
    
public:
    
    EffectWaveletDenoise();
    
    virtual wxString GetEffectName() {
        return wxString(_("Wavelet Denoising..."));
    }
    
    virtual wxString GetEffectAction() {
        return wxString(_("Denoising"));
    }
    
//    virtual bool PromptUser();
    
    virtual bool Process();
    
private:
    bool ProcessOne(int count, WaveTrack * t,
                    sampleCount start, sampleCount len);
    void RemoveNoise(sampleCount len,
                    sampleType *buffer, bool first);

    int windowSize;
    real threshold;
};

#endif
