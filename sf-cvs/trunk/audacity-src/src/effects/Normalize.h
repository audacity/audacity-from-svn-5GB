/**********************************************************************

  Audacity: A Digital Audio Editor

  Normalize.h

  Dominic Mazzoni
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NORMALIZE__
#define __AUDACITY_EFFECT_NORMALIZE__

#include "Effect.h"

#include <wx/dialog.h>
#include <wx/textctrl.h>

class wxString;
class wxCheckBox;

class WaveTrack;

class EffectNormalize: public Effect {
   
public:
   
   EffectNormalize();
   
   virtual wxString GetEffectName() {
      return wxString(_("Normalize..."));
   }

   // This is just used internally, users should not see it.  Do not translate.
   virtual wxString GetEffectIdentifier() {
      return wxT("Normalize");
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Normalizing..."));
   }
   
   virtual wxString GetEffectDescription(); // useful only after parameter values have been set

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool Init();
   virtual void End();
   virtual bool CheckWhetherSkipEffect();
   virtual bool Process();
   
 private:
   bool ProcessOne(WaveTrack * t,
                   longSampleCount start, longSampleCount end);

   virtual void StartAnalysis();
   virtual void AnalyzeData(float *buffer, sampleCount len);

   virtual void StartProcessing();
   virtual void ProcessData(float *buffer, sampleCount len);

   bool   mGain;
   bool   mDC;
   double mLevel;

   int    mCurTrackNum;
   double mCurRate;
   double mCurT0;
   double mCurT1;
   int    mCurChannel;
   float mMult;
   float mOffset;
   float mMin;
   float mMax;
   double mSum;
   int mCount;

friend class NormalizeDialog;
};

//----------------------------------------------------------------------------
// NormalizeDialog
//----------------------------------------------------------------------------

class NormalizeDialog: public wxDialog
{
public:
   // constructors and destructors
   NormalizeDialog( EffectNormalize *effect,
                    wxWindow *parent, wxWindowID id, const wxString &title,
                    const wxPoint& pos = wxDefaultPosition,
                    const wxSize& size = wxDefaultSize,
                    long style = wxDEFAULT_DIALOG_STYLE );
   
   bool mGain;
   bool mDC;
   double mLevel;
   
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();
   
   void OnPreview(wxCommandEvent &event);
   void OnOk(wxCommandEvent &event);
   void OnCancel(wxCommandEvent &event);
   void OnUpdateUI(wxCommandEvent& evt);
   EffectNormalize *mEffect;
   wxCheckBox *mGainCheckBox;
   wxCheckBox *mDCCheckBox;
   wxTextCtrl *mLevelTextCtrl;

private:
   void UpdateUI();
   
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
// arch-tag: 2e3f0feb-9ac1-4bac-ba42-3d7e37007aa8

