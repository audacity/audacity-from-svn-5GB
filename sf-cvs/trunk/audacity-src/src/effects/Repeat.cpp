/**********************************************************************

  Audacity: A Digital Audio Editor

  Repeat.cpp

  Dominic Mazzoni
  Vaughan Johnson

*******************************************************************//**

\class EffectRepeat
\brief An Effect.

*//****************************************************************//**

\class RepeatDialog
\brief Dialog used with EffectRepeat

*//*******************************************************************/


#include "../Audacity.h"

#include "Repeat.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"

#include <wx/button.h>
#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/validate.h>
#include <wx/valtext.h>

#include <math.h>


EffectRepeat::EffectRepeat()
{
   repeatCount = 10;
}

wxString EffectRepeat::GetEffectDescription() { 
   // Note: This is useful only after values have been set. 
   return wxString::Format(_("Repeated %d times"), repeatCount);
} 

bool EffectRepeat::PromptUser()
{
   //
   // Figure out the maximum number of times the selection
   // could be repeated without overflowing any track
   //
   int maxCount = -1;
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   while (track) {
      sampleCount trackLen = 
         (sampleCount)((track->GetEndTime() - track->GetStartTime()) * 
                       track->GetRate());
      sampleCount selectionLen = (sampleCount)((mT1 - mT0) * track->GetRate());
      int availSamples = 2147483647 - trackLen;
      int count = availSamples / selectionLen;
      if (maxCount == -1 || count < maxCount)
         maxCount = count;

      track = (WaveTrack *) iter.Next();
   }
   
   if (maxCount <= 1) {
      wxMessageBox(_("Tracks are too long to repeat the selection."),
                   _("Repeat"), wxOK | wxCENTRE, mParent);
      return false;
   }

   RepeatDialog dlog(mParent, -1, _("Repeat"));
   dlog.repeatCount = repeatCount;
   dlog.selectionTimeSecs = mT1 - mT0;
   dlog.maxCount = maxCount;
   dlog.TransferDataToWindow();

   dlog.CentreOnParent();

   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   repeatCount = dlog.repeatCount;
   if (repeatCount > maxCount)
      repeatCount = maxCount;
   if (repeatCount < 1)
      repeatCount = 1;

   return true;
}

bool EffectRepeat::TransferParameters( Shuttle & shuttle )
{  
   shuttle.TransferInt(wxT("Count"),repeatCount,1);
   return true;
}

bool EffectRepeat::Process()
{
   this->CopyInputWaveTracks(); // Set up m_pOutputWaveTracks.
   bool bGoodResult = true;

   TrackListIterator iter(m_pOutputWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   int nTrack = 0;
	double maxDestLen = 0.0; // used to change selection to generated bit
   while ((track != NULL) && bGoodResult) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;

      if (t1 <= t0)
         continue;

      longSampleCount start = track->TimeToLongSamples(t0);
      longSampleCount end = track->TimeToLongSamples(t1);
      sampleCount len = (sampleCount)(end - start);
      double tLen = track->LongSamplesToTime(len);
      double tc = t0 + tLen;

      if (len <= 0)
         continue;

      Track *dest;
      track->Copy(t0, t1, &dest);
      for(int j=0; j<repeatCount; j++)
      {
         if (!track->Paste(tc, dest) || 
               TrackProgress(nTrack, j / repeatCount)) // TrackProgress returns true on Cancel.
         {
            bGoodResult = false;
            break;
         }
         tc += tLen;
      }
      if (tc > maxDestLen)
         maxDestLen = tc;
      delete dest;

      track = (WaveTrack *) iter.Next();
      nTrack++;
   }

   if (bGoodResult)
   {
      // Change selection to just the generated bits.
      mT0 = mT1;
	   mT1 = maxDestLen;
   }

   this->ReplaceProcessedWaveTracks(bGoodResult); 
   return bGoodResult;
}

//----------------------------------------------------------------------------
// RepeatDialog
//----------------------------------------------------------------------------

#define ID_REPEAT_TEXT 7000

BEGIN_EVENT_TABLE(RepeatDialog, wxDialog)
   EVT_BUTTON(wxID_OK, RepeatDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL, RepeatDialog::OnCancel)
   EVT_TEXT(ID_REPEAT_TEXT, RepeatDialog::OnRepeatTextChange)
END_EVENT_TABLE()

RepeatDialog::RepeatDialog(wxWindow *parent, wxWindowID id,
                           const wxString &title):
   wxDialog(parent, id, title)
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);

   wxStaticText *statText =
      new wxStaticText(this, -1,
                        /* i18n-hint: && in here is an escape character to get
                         * a single & on screen, so keep it as is */
                       _("Repeat by Dominic Mazzoni && Vaughan Johnson"));
   mainSizer->Add(statText, 0, wxALIGN_CENTRE | wxALL, 5);

   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);

   statText =
      new wxStaticText(this, -1,
                       _("Number of times to repeat: "));
   hSizer->Add(statText, 0, wxALIGN_CENTRE | wxALL, 5);
   
   mRepeatCount =
      new wxTextCtrl(this, ID_REPEAT_TEXT, wxT("10"), wxDefaultPosition,
                     wxSize(60, -1), 0,
                     wxTextValidator(wxFILTER_NUMERIC));
   hSizer->Add(mRepeatCount, 0, wxALL, 5);
   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE | wxALL, 5);

   hSizer = new wxBoxSizer(wxHORIZONTAL);
   mTotalTime =
      new wxStaticText(this, -1, wxString(_("New selection length: ")) +
                       wxT("XX minutes, XX seconds"));
   hSizer->Add(mTotalTime, 1, wxALL | wxEXPAND, 5);
   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE | wxALL, 5);

   // OK & Cancel buttons
   mainSizer->Add(CreateStdButtonSizer(this, eCancelButton|eOkButton), 0, wxEXPAND);

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
}

void RepeatDialog::OnRepeatTextChange(wxCommandEvent & event)
{
   // We may even get called during the constructor.
   // This test saves us from calling unsafe functions.
   if( !IsShown() )
      return;
   TransferDataFromWindow();

   DisplayNewTime();
}

void RepeatDialog::DisplayNewTime()
{
   int newTime = (int)(selectionTimeSecs * (repeatCount + 1));
   wxString str;

   str = _("New selection length: ");
   if (newTime >= 120)
      str += wxString::Format(_("%d minutes, %d seconds"),
                              newTime/60, newTime%60);
   else
      str += wxString::Format(_("%d seconds"),
                              newTime);

   mTotalTime->SetLabel(str);
}

bool RepeatDialog::Validate()
{
   return TRUE;
}

bool RepeatDialog::TransferDataToWindow()
{
   mRepeatCount->SetValue(wxString::Format(wxT("%d"), repeatCount));
   DisplayNewTime();

   return true;
}

bool RepeatDialog::TransferDataFromWindow()
{
   long l;
   mRepeatCount->GetValue().ToLong(&l);

   repeatCount = l;
   if (repeatCount < 1)
      repeatCount = 1;
   if (repeatCount > maxCount)
      repeatCount = maxCount;

   return true;
}

void RepeatDialog::OnOk(wxCommandEvent & event)
{
   TransferDataFromWindow();
   
   EndModal(true);
}

void RepeatDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
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
// arch-tag: 65c3dad7-00c2-48bf-a253-c973e626b9ac

