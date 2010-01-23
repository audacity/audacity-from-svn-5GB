/**********************************************************************

  Audacity: A Digital Audio Editor

  Leveller.h

  Lynn Allan

**********************************************************************/

#ifndef __AUDACITY_EFFECT_LEVELER__
#define __AUDACITY_EFFECT_LEVELER__

#include "SimpleMono.h"

class wxString;
class wxCheckBox;
class wxTextCtrl;
class wxChoice;

class EffectLeveller: public EffectSimpleMono {

public:

   EffectLeveller();

   virtual wxString GetEffectName() {
      return wxString(_("Leveller..."));
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Leveller"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Applying Leveller..."));
   }
   virtual bool Init();
   virtual void End();
   virtual bool CheckWhetherSkipEffect();
   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

protected:
   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);

private:
   friend class LevellerDialog;

   void   CalcLevellerFactors();
   int    mLevellerDbChoiceIndex;
   int    mLevellerNumPasses;
   double mLevellerDbSilenceThreshold;
//   float  mFrameSum;
   float  LevelOneFrame(float frame);
};

//----------------------------------------------------------------------------
// LevellerDialog
//----------------------------------------------------------------------------

class LevellerDialog: public wxDialog
{
public:
   // constructors and destructors
   LevellerDialog(wxWindow *parent, wxWindowID id, const wxString &title);

   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

   void OnOk( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );

   int           mLevellerDbChoiceIndex;
   int           mLevellerNumPasses;

private:
   wxChoice      *mLevellerDbSilenceThresholdChoice;
   wxChoice      *mLevellerNumPassesChoice;

private:
   DECLARE_EVENT_TABLE()
};

#endif

