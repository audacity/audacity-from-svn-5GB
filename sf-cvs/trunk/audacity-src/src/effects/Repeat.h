/**********************************************************************

  Audacity: A Digital Audio Editor

  Repeat.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REPEAT__
#define __AUDACITY_EFFECT_REPEAT__

#include "Effect.h"

#include <wx/intl.h>
#include <wx/dialog.h>

class wxString;
class wxStaticText;
class wxTextCtrl;

class WaveTrack;

class EffectRepeat:public Effect {

 public:

   EffectRepeat();

   virtual wxString GetEffectName() {
      return wxString(_("Repeat..."));
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Repeat"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Performing Repeat"));
   }
   
   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );
   
   virtual bool Process();

 private:
   int repeatCount;
};

class RepeatDialog:public wxDialog {
 public:
   // constructors and destructors
   RepeatDialog(wxWindow * parent, wxWindowID id, const wxString & title);

   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

   wxTextCtrl   *mRepeatCount;
   wxStaticText *mTotalTime;

 private:
   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void OnRepeatTextChange(wxCommandEvent & event);

   void DisplayNewTime();

 private:
   DECLARE_EVENT_TABLE()

 public:
   int repeatCount;
   int maxCount;
   double selectionTimeSecs;
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
// arch-tag: 4badebd8-1e5a-40e9-aa25-b19d41f43102

