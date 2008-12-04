/**********************************************************************

  Audacity: A Digital Audio Editor

  Contrast.h

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CONTRAST__
#define __AUDACITY_EFFECT_CONTRAST__

#include "Effect.h"
#include "../widgets/TimeTextCtrl.h"

#include <wx/dialog.h>
#include <wx/slider.h>

class wxButton;
class wxSizer;
class wxString;

class Envelope;
class WaveTrack;

class EffectContrast: public Effect {

public:
   
   EffectContrast();
   virtual ~EffectContrast();

   virtual wxString GetEffectName() {
      return wxString(_("Contrast...\tCtrl+Shift+T"));
   }

   virtual int GetEffectFlags() {
      return BUILTIN_EFFECT | ANALYZE_EFFECT;
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> result;
      result.insert(wxT("http://audacityteam.org/namespace#Contrast"));
      return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Contrast"));
   }

   virtual wxString GetEffectAction() {
      if (mDoBackground)
         return wxString(_("Measuring background"));
      else
         return wxString(_("Measuring foreground"));
   }
   
   virtual bool PromptUser();
   
   virtual bool CheckWhetherSkipEffect();
   virtual bool Process();
   void SaveTimes(bool, double, double);
   double length;
   
private:
   bool      mDoBackground;
   float GetDB();
   double GetStartTime();
   void SetStartTime(double);
   double GetEndTime();
   void SetEndTime(double);
   double mStartTimeF;
   double mEndTimeF;
   bool bFGset;
   double mStartTimeB;
   double mEndTimeB;
   bool bBGset;

friend class ContrastDialog;
};

// WDR: class declarations

//----------------------------------------------------------------------------
// ContrastDialog
//----------------------------------------------------------------------------

// Declare window functions

class ContrastDialog: public EffectDialog
{
public:
   // constructors and destructors
   ContrastDialog(EffectContrast * effect,
                      wxWindow *parent);

   void PopulateOrExchange(ShuttleGui & S);

   void OnGetForegroundDB( wxCommandEvent &event );
   void OnGetBackgroundDB( wxCommandEvent &event );
   void OnTimeCtrlUpdate(wxCommandEvent & event);

private:
   // handlers
   void OnGetURL(wxCommandEvent &event);
   void OnExport(wxCommandEvent &event);
   void OnForegroundStartT(wxCommandEvent & event);
   void OnForegroundEndT(wxCommandEvent & event);
   void OnBackgroundStartT(wxCommandEvent & event);
   void OnBackgroundEndT(wxCommandEvent & event);
   void OnUseSelectionF(wxCommandEvent & event);
   void OnUseSelectionB(wxCommandEvent & event);
   void results();
   void OnOK( wxCommandEvent &event );
   void OnReset(wxCommandEvent & event);

   wxTextCtrl *mForegroundRMSText;
   wxTextCtrl *mBackgroundRMSText;
   wxTextCtrl *mPassFailText;
   wxTextCtrl *mDiffText;

   float foregrounddB;
   float backgrounddB;
   double mT0orig;
   double mT1orig;

 public:

   EffectContrast * m_pEffect;

   wxButton * m_pButton_GetBackground;
   wxButton * m_pButton_GetForeground;
   wxButton * m_pButton_UseCurrentF;
   wxButton * m_pButton_UseCurrentB;
   wxButton * m_pButton_GetURL;
   wxButton * m_pButton_Export;
   wxButton * m_pButton_Reset;

   TimeTextCtrl *mForegroundStartT;
   TimeTextCtrl *mForegroundEndT;
   TimeTextCtrl *mBackgroundStartT;
   TimeTextCtrl *mBackgroundEndT;

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
// arch-tag: c42ae8d9-7625-4bf9-a719-e5d082430ed5

