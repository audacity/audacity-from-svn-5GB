/**********************************************************************

  Audacity: A Digital Audio Editor

  Repeat.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h"

#include <wx/wx.h>
#include <wx/defs.h>
#include <wx/validate.h>
#include <wx/valtext.h>
#include <wx/intl.h>
#include <math.h>

#include "Repeat.h"
#include "../WaveTrack.h"

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
   RepeatDialog dlog(mParent, -1, _("Repeat"));
   dlog.repeatCount = repeatCount;
   dlog.selectionTimeSecs = mT1 - mT0;
   dlog.TransferDataToWindow();

   dlog.CentreOnParent();

   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   repeatCount = dlog.repeatCount;

   // Sensible limit: no more than 24 hours worth
   if (repeatCount * (mT1 - mT0) > (60 * 60 * 24))
      repeatCount = (int)((60 * 60 * 24) / (mT1 - mT0));

   return true;
}

bool EffectRepeat::Process()
{
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   int count = 0;
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;

      if (t1 <= t0)
         continue;

      longSampleCount start = track->TimeToLongSamples(t0);
      longSampleCount end = track->TimeToLongSamples(t1);
      sampleCount len = (sampleCount)(end - start);

      if (len <= 0)
         continue;

      //
      // Create a track that contains 1 or more copies of the
      // selection, cleverly arranged so that every BlockFile
      // is within the minimum and maxmimum lengths of a normal
      // BlockFile.  That allows us to repeat the same sequence
      // of identical BlockFiles, saving lots of disk space.
      //

      sampleFormat format = track->GetSampleFormat();
      WaveTrack *unitTrack = mFactory->NewWaveTrack(format);
      WaveTrack *dest = mFactory->NewWaveTrack(format);
      sampleCount maxBlockSize = unitTrack->GetMaxBlockSize();
      sampleCount minBlockSize = maxBlockSize / 2;
      samplePtr buffer = NewSamples(maxBlockSize, format);

      int numCopies = 1;
      int chunkSize = len;
      int j;

      while (chunkSize * numCopies < minBlockSize)
         numCopies++;
      
      if (chunkSize > maxBlockSize) {
         j=2;

         while(chunkSize/j >= maxBlockSize)
            j++;

         chunkSize = (chunkSize + (j-1)) / j;
      }

      sampleCount totalSamples = 0;
      while(totalSamples < len * numCopies) {
         sampleCount blockLen = chunkSize;
         sampleCount blockStart = (totalSamples % len);

         if (totalSamples + blockLen > len * numCopies)
            blockLen = len * numCopies - totalSamples;

         if (!track->Get(buffer, format, start+blockStart, blockLen)) {
            delete unitTrack;
            return false;
         }

         unitTrack->Append(buffer, format, blockLen);
         if (numCopies == 1)
            unitTrack->Flush();

         totalSamples += blockLen;
      }
      if (numCopies != 1)
         unitTrack->Flush();

      //
      // Repeat the unit track enough times, possible creating a few
      // more than desired
      //

      int desiredCopies = repeatCount+1;
      int desiredUnitTracks = (desiredCopies + (numCopies-1)) / numCopies;
      for(j=0; j<desiredUnitTracks; j++)
         dest->Paste(dest->GetEndTime(), unitTrack);

      //
      // If necessary, delete a few copies from the end
      //

      int actualCopies = desiredUnitTracks * numCopies;
      if (actualCopies > desiredCopies) {
         double oneLen = unitTrack->GetEndTime() / numCopies;
         double clearLen = oneLen * (desiredCopies - actualCopies);
         dest->Clear(dest->GetEndTime() - clearLen, clearLen);
      }

      track->Clear(t0, t1);
      track->Paste(t0, dest);

      delete unitTrack;
      delete dest;

      track = (WaveTrack *) iter.Next();
      count++;
   }

   return true;
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
                       _("Repeat by Dominic Mazzoni"));
   mainSizer->Add(statText, 0, wxALIGN_CENTRE | wxALL, 5);

   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);

   statText =
      new wxStaticText(this, -1,
                       _("Number of times to repeat: "));
   hSizer->Add(statText, 0, wxALL, 5);
   
   mRepeatCount =
      new wxTextCtrl(this, ID_REPEAT_TEXT, "10", wxDefaultPosition,
                     wxSize(60, -1), 0,
                     wxTextValidator(wxFILTER_NUMERIC));
   hSizer->Add(mRepeatCount, 0, wxALL, 5);
   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE | wxALL, 5);

   hSizer = new wxBoxSizer(wxHORIZONTAL);
   mTotalTime =
      new wxStaticText(this, -1, wxString(_("New selection length: ")) +
                       "XX minutes, XX seconds");
   hSizer->Add(mTotalTime, 1, wxALL | wxEXPAND, 5);
   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE | wxALL, 5);

   hSizer = new wxBoxSizer(wxHORIZONTAL);
   wxButton *ok = new wxButton(this, wxID_OK, _("OK"));
   ok->SetDefault();
   hSizer->Add(ok, 0, wxALIGN_CENTRE|wxALL, 5);

   wxButton *cancel = new wxButton(this, wxID_CANCEL, _("Cancel"));
   hSizer->Add(cancel, 0, wxALIGN_CENTRE|wxALL, 5);

   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE|wxALIGN_CENTER_VERTICAL|wxALL, 5);

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
}

void RepeatDialog::OnRepeatTextChange(wxCommandEvent & event)
{
   TransferDataFromWindow();

   DisplayNewTime();
}

void RepeatDialog::DisplayNewTime()
{
   int newTime = (int)(selectionTimeSecs * repeatCount);
   wxString str;
   
   str = _("New selection length: ");
   if (newTime >= 120)
      str += wxString::Format(_("%d minutes, %d seconds"),
                              newTime/60, newTime%60);
   else
      str += wxString::Format(_("%d seconds"),
                              newTime%60);

   mTotalTime->SetLabel(str);
}

bool RepeatDialog::Validate()
{
   return TRUE;
}

bool RepeatDialog::TransferDataToWindow()
{
   mRepeatCount->SetValue(wxString::Format("%d", repeatCount));
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



