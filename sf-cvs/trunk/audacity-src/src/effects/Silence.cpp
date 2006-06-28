/**********************************************************************

  Audacity: A Digital Audio Editor

  Silence.cpp

  Dominic Mazzoni

*******************************************************************//**

\class EffectSilence
\brief An Effect for the "Generator" menu to add silence.

*//****************************************************************//**

\class GenerateDialog
\brief Dialog used with EffectSilence and others..

*//*******************************************************************/


#include "../Audacity.h"

#include <wx/defs.h> 

#include <wx/button.h> 
#include <wx/sizer.h> 
#include <wx/stattext.h> 
#include <wx/textctrl.h> 

#include "Silence.h"
#include "../WaveTrack.h"
#include "../widgets/TimeTextCtrl.h"

class SilenceDialog:public GenerateDialog
{
 public:
   SilenceDialog(wxWindow * parent, const wxString & action):
      GenerateDialog(parent, action)
   {
      Init();
   }

   void PopulateOrExchange(ShuttleGui & S)
   {
      S.StartStatic(_("Specify Length"), true);
         mLenCtrl = new TimeTextCtrl(this,
                                    wxID_ANY,
                                    TimeTextCtrl::GetBuiltinFormat(0),
                                    30.0,
                                    44000.0);
         S.AddWindow(mLenCtrl);
      S.EndStatic();
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

   bool Validate()
   {
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

bool EffectSilence::PromptUser()
{
   if (mT1 > mT0)
      length = mT1 - mT0;

   SilenceDialog dlog(mParent, _("Silence Generator"));
   dlog.SetLength(length);
   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == 0)
      return false;

   length = dlog.GetLength();
   return true;
}

bool EffectSilence::Process()
{
   if (length <= 0.0)
      length = sDefaultGenerateLen;

   //Iterate over each track
   TrackListIterator iter(mTracks);
   Track *track = iter.First();
   while (track) {
      if (track->GetSelected()) {
         track->Clear(mT0, mT1);
         track->InsertSilence(mT0, length);
      }
      
      //Iterate to the next track
      track = iter.Next();
   }

	mT1 = mT0 + length; // Update selection.
   return true;
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 78c8d521-815a-4fdb-830a-f9655cd4f529
