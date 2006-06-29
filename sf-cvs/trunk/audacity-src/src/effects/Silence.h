/**********************************************************************

  Audacity: A Digital Audio Editor

  Silence.h

  Dominic Mazzoni
  
  An effect for the "Generator" menu to add silence.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_SILENCE__
#define __AUDACITY_EFFECT_SILENCE__

#include <wx/defs.h>
#include <wx/dialog.h>
#include <wx/intl.h>

#include "Effect.h"
#include "../widgets/TimeTextCtrl.h"

class wxSizer;
class wxTextCtrl;

class EffectSilence:public Effect {

 public:
   EffectSilence() {
      length = sDefaultGenerateLen;
   }

   virtual wxString GetEffectName() {
      return wxString(_("&Silence..."));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Generating Silence"));
   }

   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription() { 
      return wxString::Format(_("Applied effect: Generate Silence, %.6lf seconds"), length); 
   } 

   virtual int GetEffectFlags() {
      return BUILTIN_EFFECT | INSERT_EFFECT;
   }

   virtual bool PromptUser();

   virtual bool Process();

 private:
   double length;
};

/// \brief TimeDialog is an EffectDialog that shows a TimeTextCtrl for 
/// setting the duration of a generated effect.
class TimeDialog:public EffectDialog
{
 public:
   TimeDialog(wxWindow * parent, const wxString & title):
      EffectDialog(parent, title, EDS_GENERATE)
   {
   }

   void PopulateOrExchange(ShuttleGui & S)
   {
      S.StartStatic(_("Specify Length"), true);
      {
         mLenCtrl = new
            TimeTextCtrl(this,
                         wxID_ANY,
                         TimeTextCtrl::GetBuiltinFormat(_("seconds")),
                         mLength,
                         44000.0,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
         S.AddWindow(mLenCtrl);
      }
      S.EndStatic();
      TransferDataToWindow();
   }

   bool TransferDataToWindow()
   {
      mLenCtrl->SetTimeValue(mLength);
      mLenCtrl->SetFocus();

      return true;
   }

   bool TransferDataFromWindow()
   {
      mLength = mLenCtrl->GetTimeValue();

      return true;
   }

   double GetLength()
   {
      return mLength;
   }

   void SetLength(double length)
   {
      mLength = length;
   }

 private:

   TimeTextCtrl *mLenCtrl;
   wxString mLenText;
   double mLength;
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
// arch-tag: 077860ae-1dc5-4aa0-a70d-dcbd27e92dd5

