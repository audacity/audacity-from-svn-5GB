/**********************************************************************

  Audacity: A Digital Audio Editor

  TruncSilence.h

  Lynn Allan (from DM's Normalize)
  //ToDo ... put BlendFrames in Effects, Project, or other class
  //ToDo ... Use ZeroCrossing logic to improve blend
  //ToDo ... BlendFrames on "fade-out"
  //ToDo ... BlendFrameCount is a user-selectable parameter
  //ToDo ... Detect transient signals that are too short to interrupt the TruncatableSilence
  //ToDo ... Extend from mono-only to stereo or n-channels

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TRUNC_SILENCE__
#define __AUDACITY_EFFECT_TRUNC_SILENCE__

#include "Effect.h"

class EffectTruncSilence: public Effect {

public:

   EffectTruncSilence();

   virtual wxString GetEffectName() {
      return wxString(_("Truncate Silence..."));
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("TruncateSilence"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Truncating Silence..."));
   }
   virtual bool Init();
   virtual void End();
   virtual bool CheckWhetherSkipEffect();
   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool Process();

 private:
   bool ProcessOne();

   //ToDo ... put BlendFrames in Effects, Project, or other class
   void BlendFrames(float* buffer, int leftIndex, int rightIndex, int blendFrameCount);

 private:
   int             mTruncDbChoiceIndex;
   WaveTrack       *mTrack;
   longSampleCount mTruncLongestAllowedSilentMs;
   bool            mUserPrompted;
   sampleCount     mBlendFrameCount;

friend class TruncSilenceDialog;
};

//----------------------------------------------------------------------------
// TruncSilenceDialog
//----------------------------------------------------------------------------

class TruncSilenceDialog: public wxDialog
{
public:
   // constructors and destructors
   TruncSilenceDialog(wxWindow *parent, wxWindowID id, const wxString &title);

   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

   int  mTruncLongestAllowedSilentMs;
   int  mTruncDbChoiceIndex;
   int  mBlendFrameCount;

private:
   wxTextCtrl *mTruncLongestAllowedSilentMsText;
   wxChoice   *mTruncDbSilenceThresholdChoice;

   void OnOk( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );

private:
   DECLARE_EVENT_TABLE()
};

#endif
