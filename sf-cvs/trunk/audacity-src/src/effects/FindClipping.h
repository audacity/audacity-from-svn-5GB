/**********************************************************************

  Audacity: A Digital Audio Editor

  FindClipping.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_FINDCLIPPING__
#define __AUDACITY_EFFECT_FINDCLIPPING__

class wxString;

#include <wx/dialog.h>

#include <wx/intl.h>

#include "Effect.h"

class wxStaticText;

class WaveTrack;

class EffectFindClipping:public Effect
{
 friend class FindClippingDialog;

 public:

   EffectFindClipping();

   virtual wxString GetEffectName()
   {
      return wxString(_("Find Clipping..."));
   }

   virtual wxString GetEffectIdentifier()
   {
      return wxString(wxT("FindClipping"));
   }

   virtual wxString GetEffectAction()
   {
      return wxString(_("Detecting clipping"));
   }
   
   virtual int GetEffectFlags() {
      return BUILTIN_EFFECT | ANALYZE_EFFECT;
   }

   virtual wxString GetEffectDescription(); 

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );
   
   virtual bool Process();

 private:
   bool ProcessOne(LabelTrack *l, int count, WaveTrack * t,
                   longSampleCount start, sampleCount len);
 
   sampleCount mStart;
   sampleCount mStop;
};

//----------------------------------------------------------------------------
// FindClippingDialog
//----------------------------------------------------------------------------
class FindClippingDialog:public EffectDialog {
 public:
   FindClippingDialog(EffectFindClipping * effect, wxWindow * parent);

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataFromWindow();

 private:
   EffectFindClipping *mEffect;
};

#endif // __AUDACITY_EFFECT_FINDCLIPPING__

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: cad436f5-7c97-40a2-8ee9-3748e8f3e56f

