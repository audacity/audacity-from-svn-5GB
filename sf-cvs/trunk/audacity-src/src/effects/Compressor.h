/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_COMPRESSOR__
#define __AUDACITY_EFFECT_COMPRESSOR__

class wxString;

#include "SimpleMono.h"

#include <wx/defs.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/panel.h>

class wxCheckBox;
class wxSlider;
class wxStaticText;

class WaveTrack;

class EffectCompressor: public EffectSimpleMono {
   
public:
   
   EffectCompressor();
   
   virtual wxString GetEffectName() {
      return wxString(_("Compressor..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Applying Dynamic Range Compression..."));
   }
   
   virtual bool PromptUser();
   
 protected:
   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);

 private:

   bool NewTrackSimpleMono();

   float AvgCircle(float x);
   void Follow(float x, double *outEnv, int maxBack);
   float DoCompression(float x, double env);
   
   double    mAttackTime;
   double    mThresholdDB;
   double    mRatio;
   bool      mUseGain;
   
   double    mDecayTime;
   double    mGainDB;
   double    mAttackFactor;
   double    mDecayFactor;
   double    mFloor;
   double    mThreshold;
   double    mGain;
   double    mRMSSum;
   int       mCircleSize;
   int       mCirclePos;
   double   *mCircle;
   double   *mLevelCircle;
   double    mLastLevel;

   friend class CompressorDialog;
};

class CompressorPanel: public wxPanel
{
public:
   CompressorPanel( wxWindow *parent, wxWindowID id, 
                    const wxPoint& pos = wxDefaultPosition,
                    const wxSize& size = wxDefaultSize);

   void OnPaint(wxPaintEvent & event);

   double threshold;
   double ratio;

private:

   wxBitmap *mBitmap;
   wxRect mEnvRect;
   int mWidth;
   int mHeight;

   DECLARE_EVENT_TABLE()
};

// WDR: class declarations

//----------------------------------------------------------------------------
// CompressorDialog
//----------------------------------------------------------------------------

class CompressorDialog: public wxDialog
{
public:
   // constructors and destructors
   CompressorDialog( EffectCompressor *effect,
                     wxWindow *parent, wxWindowID id, const wxString &title,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize,
                     long style = wxDEFAULT_DIALOG_STYLE );

   double threshold;
   double ratio;
   double attack;
   bool useGain;
   
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();
   
private:
   void OnOk( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
   void OnSize( wxSizeEvent &event );
   void OnSlider( wxCommandEvent &event );
   void OnPreview( wxCommandEvent &event );

   EffectCompressor *mEffect;
   CompressorPanel *mPanel;
   wxSlider *mThresholdSlider;
   wxSlider *mRatioSlider;
   wxSlider *mAttackSlider;
   wxCheckBox *mGainCheckBox;
   wxStaticText *mThresholdText;
   wxStaticText *mRatioText;
   wxStaticText *mAttackText;
   
private:
   DECLARE_EVENT_TABLE()
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
// arch-tag: 44dae2d6-58a3-4893-aa9d-c441e0d9890e

