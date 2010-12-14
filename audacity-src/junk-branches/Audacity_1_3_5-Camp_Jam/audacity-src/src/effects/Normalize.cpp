/**********************************************************************

  Audacity: A Digital Audio Editor

  Normalize.cpp

  Dominic Mazzoni
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectNormalize
\brief An Effect.

*//****************************************************************//**

\class NormalizeDialog
\brief Dialog used with EffectNormalize

*//*******************************************************************/


#include "../Audacity.h" // for rint from configwin.h

#include <math.h>

#include "Normalize.h"
#include "../ShuttleGui.h"
#include "../Internat.h"
#include "../WaveTrack.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Shuttle.h"

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textdlg.h>

#define NORMALIZE_DB_MIN -240
#define NORMALIZE_DB_MAX 240

EffectNormalize::EffectNormalize()
{
   Init();
}

static double gFrameSum; //lda odd ... having this as member var crashed on exit

bool EffectNormalize::Init()
{
   int boolProxy = gPrefs->Read(wxT("/CsPresets/Norm_AmpDbGain"), 1);
   mGain = (boolProxy == 1);
   boolProxy = gPrefs->Read(wxT("/CsPresets/Norm_RemoveDcOffset"), 1);
   mDC = (boolProxy == 1);
   gPrefs->Read(wxT("/CsPresets/Norm_Level"), &mLevel, 0.0);
   return true;
}

wxString EffectNormalize::GetEffectDescription() // useful only after parameter values have been set
{ 
   // Note: This is useful only after ratio has been set. 
   wxString strResult =
      /* i18n-hint: First %s is the effect name, 2nd and 3rd are either true or
       * false (translated below) if those options were selected */
      wxString::Format(_("Applied effect: %s remove dc offset = %s, normalize amplitude = %s"), 
                        this->GetEffectName().c_str(), 
                        /* i18n-hint: true here means that the option was
                         * selected. Opposite false if not selected */
                        mDC ? _("true") : _("false"), 
                        mGain ? _("true") : _("false"));
   if (mGain)
      strResult += wxString::Format(_(", maximum amplitude = %.1f dB"), -mLevel); 

   return strResult;
} 

bool EffectNormalize::TransferParameters( Shuttle & shuttle )
{
   shuttle.TransferBool( wxT("ApplyGain"), mGain, true );
   shuttle.TransferBool( wxT("RemoveDcOffset"), mDC, true );
   shuttle.TransferDouble( wxT("Level"), mLevel, 0.0);
   return true;
}

bool EffectNormalize::CheckWhetherSkipEffect()
{
   bool rc = ((mGain == false) && (mDC == false));
   return rc;
}

void EffectNormalize::End()
{
	bool bValidate;
	gPrefs->Read(wxT("/Validate/Enabled"), &bValidate, false );
	if( bValidate )
	{
      int checkOffset = abs((int)(mOffset * 1000.0));
      gPrefs->Write(wxT("/Validate/Norm_Offset"), checkOffset);
      int checkMultiplier = abs((int)(mMult * 1000.0));
      gPrefs->Write(wxT("/Validate/Norm_Multiplier"), checkMultiplier);
      int checkFrameSum = (int)gFrameSum;
      gPrefs->Write(wxT("/Validate/Norm_FrameSum"), checkFrameSum);
	}
}

bool EffectNormalize::PromptUser()
{
   NormalizeDialog dlog(this, mParent, -1, _("Normalize"));
   dlog.mGain = mGain;
   dlog.mDC = mDC;
   dlog.mLevel = mLevel;
   dlog.TransferDataToWindow();

   dlog.CentreOnParent();
   dlog.ShowModal();
   
   if (!dlog.GetReturnCode())
      return false;

   mGain = dlog.mGain;
   mDC = dlog.mDC;
   mLevel = dlog.mLevel;
   gPrefs->Write(wxT("/CsPresets/Norm_AmpDbGain"), mGain);
   gPrefs->Write(wxT("/CsPresets/Norm_RemoveDcOffset"), mDC);
   gPrefs->Write(wxT("/CsPresets/Norm_Level"), mLevel);

   return true;
}

bool EffectNormalize::Process()
{
   if (mGain == false &&
       mDC == false)
      return true;

   //Iterate over each track
   this->CopyInputWaveTracks(); // Set up m_pOutputWaveTracks.
   bool bGoodResult = true;

   TrackListIterator iter(m_pOutputWaveTracks);
    WaveTrack *track = (WaveTrack *) iter.First();
   mCurTrackNum = 0;
   while (track) {
      //Get start and end times from track
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();

      //Set the current bounds to whichever left marker is
      //greater and whichever right marker is less:
      mCurT0 = mT0 < trackStart? trackStart: mT0;
      mCurT1 = mT1 > trackEnd? trackEnd: mT1;

      // Process only if the right marker is to the right of the left marker
      if (mCurT1 > mCurT0) {

         //Transform the marker timepoints to samples
         longSampleCount start = track->TimeToLongSamples(mCurT0);
         longSampleCount end = track->TimeToLongSamples(mCurT1);
         
         //Get the track rate and samples
         mCurRate = track->GetRate();
         mCurChannel = track->GetChannel();

         //ProcessOne() (implemented below) processes a single track
         if (!ProcessOne(track, start, end))
         {
            bGoodResult = false;
            break;
         }
      }
      
      //Iterate to the next track
      track = (WaveTrack *) iter.Next();
      mCurTrackNum++;
   }

   this->ReplaceProcessedWaveTracks(bGoodResult); 
   return bGoodResult;
}

//ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
//and executes AnalyzeData, then ProcessData, on it...
bool EffectNormalize::ProcessOne(WaveTrack * track,
                                  longSampleCount start, longSampleCount end)
{
   bool rc = true;
   
   longSampleCount s;
   //Get the length of the buffer (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later 
   double len = (double)(end - start);

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   float *buffer = new float[track->GetMaxBlockSize()];

   int pass;

   for(pass=0; pass<2; pass++) {

      if (pass==0)
         StartAnalysis();
      if (pass==1)
         StartProcessing();

      //Go through the track one buffer at a time. s counts which
      //sample the current buffer starts at.
      s = start;
      while (s < end) {
         //Get a block of samples (smaller than the size of the buffer)
         sampleCount block = track->GetBestBlockSize(s);
         
         //Adjust the block size if it is the final block in the track
         if (s + block > end)
            block = end - s;
         
         //Get the samples from the track and put them in the buffer
         track->Get((samplePtr) buffer, floatSample, s, block);
         
         //Process the buffer.

         if (pass==0)
            AnalyzeData(buffer, block);

         if (pass==1) {
            ProcessData(buffer, block);
         
            //Copy the newly-changed samples back onto the track.
            track->Set((samplePtr) buffer, floatSample, s, block);
         }
            
         //Increment s one blockfull of samples
         s += block;
         
         //Update the Progress meter
			if (TrackProgress(mCurTrackNum, 
									((double)(pass)*0.5) + // Approximate each pass as half.
                           ((double)(s - start) / (len*2)))) {
            rc = false; //lda .. break, not return, so that buffer is deleted
            break;
         }
      }
   }
   //Clean up the buffer
   delete[] buffer;

   //Return true because the effect processing succeeded ... unless cancelled
   return rc;
}

void EffectNormalize::StartAnalysis()
{
   mMin = 1.0;
   mMax = -1.0;
   mSum = 0.0;
   mCount = 0;
}

void EffectNormalize::AnalyzeData(float *buffer, sampleCount len)
{
   int i;

   for(i=0; i<len; i++) {
      if (buffer[i] < mMin)
         mMin = buffer[i];
      if (buffer[i] > mMax)
         mMax = buffer[i];
      mSum += (double)buffer[i];
   }

   mCount += len;
}

void EffectNormalize::StartProcessing()
{
   mMult = 1.0;
   mOffset = 0.0;
   
   float ratio = pow(10.0,TrapDouble(-mLevel*10,
                                     NORMALIZE_DB_MIN,
                                     NORMALIZE_DB_MAX)/200.0);

   if (mDC) {
      mOffset = (float)(-mSum / mCount);
   }

   if (mGain) {
      float extent = fabs(mMax + mOffset);
      if (fabs(mMin + mOffset) > extent)
         extent = fabs(mMin + mOffset);

      if (extent > 0)
         mMult = ratio / extent;
   }
}

void EffectNormalize::ProcessData(float *buffer, sampleCount len)
{
   int i;

   for(i=0; i<len; i++) {
      float adjFrame = (buffer[i] + mOffset) * mMult;
      buffer[i] = adjFrame;
      gFrameSum += fabs(adjFrame);  //lda: validation.
   }
}

//----------------------------------------------------------------------------
// NormalizeDialog
//----------------------------------------------------------------------------

#define ID_NORMALIZE_AMPLITUDE 10002
#define ID_LEVEL_STATIC_MINUS 10003
#define ID_LEVEL_STATIC_DB 10004
#define ID_LEVEL_TEXT 10005

BEGIN_EVENT_TABLE(NormalizeDialog,wxDialog)
   EVT_BUTTON( wxID_OK, NormalizeDialog::OnOk )
   EVT_BUTTON( wxID_CANCEL, NormalizeDialog::OnCancel )
	EVT_BUTTON(ID_EFFECT_PREVIEW, NormalizeDialog::OnPreview)
	EVT_CHECKBOX(ID_NORMALIZE_AMPLITUDE, NormalizeDialog::OnUpdateUI)
END_EVENT_TABLE()

NormalizeDialog::NormalizeDialog(EffectNormalize *effect,
                                 wxWindow *parent, wxWindowID id,
                                 const wxString &title,
                                 const wxPoint &position, const wxSize& size,
                                 long style ) :
   wxDialog( parent, id, title, position, size, style ),
   mEffect(effect)
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);

   mainSizer->Add(new wxStaticText(this, -1,
                                   _("Normalize by Dominic Mazzoni"
												),
                                   wxDefaultPosition, wxDefaultSize,
                                   wxALIGN_CENTRE),
                  0, wxALIGN_CENTRE|wxALL, 5);
   
   mDCCheckBox = new wxCheckBox(this, -1,
                                  _("Remove any DC offset (center on 0 vertically)"));
   mDCCheckBox->SetValue(mDC);
   mainSizer->Add(mDCCheckBox, 0, wxALIGN_LEFT|wxALL, 5);

   mGainCheckBox = new wxCheckBox(this, ID_NORMALIZE_AMPLITUDE,
                                  _("Normalize maximum amplitude to"));
   mGainCheckBox->SetValue(mGain);
   mainSizer->Add(mGainCheckBox, 0, wxALIGN_LEFT|wxALL, 5);
   
   wxBoxSizer *levelSizer = new wxBoxSizer(wxHORIZONTAL);
   levelSizer->AddSpacer(30);
   mLevelTextCtrl = new wxTextCtrl(this, ID_LEVEL_TEXT);
   mLevelTextCtrl->SetValue(wxString::Format(wxT("%.1f"), mLevel));
   levelSizer->Add(new wxStaticText(this, ID_LEVEL_STATIC_MINUS, _("-"),
                   wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT), 0,
                   wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, 5);
   levelSizer->Add(mLevelTextCtrl, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, 5);
   levelSizer->Add(new wxStaticText(this, ID_LEVEL_STATIC_DB, _("dB"),
                   wxDefaultPosition,
                   wxDefaultSize, wxALIGN_LEFT), 0,
                   wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, 5);
   mainSizer->Add(levelSizer, 0, wxALIGN_LEFT|wxALIGN_TOP|wxALL, 0);

   // Preview, OK, & Cancel buttons
   mainSizer->Add(CreateStdButtonSizer(this, ePreviewButton|eCancelButton|eOkButton), 0, wxEXPAND);

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);

   effect->SetDialog(this);
}

void NormalizeDialog::OnUpdateUI(wxCommandEvent& evt)
{
   UpdateUI();
}

void NormalizeDialog::UpdateUI()
{
   bool enable = mGainCheckBox->GetValue();
   FindWindowById(ID_LEVEL_STATIC_MINUS)->Enable(enable);
   FindWindowById(ID_LEVEL_STATIC_DB)->Enable(enable);
   FindWindowById(ID_LEVEL_TEXT)->Enable(enable);
}

bool NormalizeDialog::TransferDataToWindow()
{
   mGainCheckBox->SetValue(mGain);
   mDCCheckBox->SetValue(mDC);
   mLevelTextCtrl->SetValue(Internat::ToString(mLevel, 1));
   
   UpdateUI();

   TransferDataFromWindow();

   return true;
}

bool NormalizeDialog::TransferDataFromWindow()
{
   mGain = mGainCheckBox->GetValue();
   mDC = mDCCheckBox->GetValue();
   mLevel = Internat::CompatibleToDouble(mLevelTextCtrl->GetValue());
   return true;
}

void NormalizeDialog::OnPreview(wxCommandEvent &event)
{
   TransferDataFromWindow();

	// Save & restore parameters around Preview, because we didn't do OK.
   bool oldGain = mEffect->mGain;
   bool oldDC = mEffect->mDC;
   double oldLevel = mEffect->mLevel;

   mEffect->mGain = mGain;
   mEffect->mDC = mDC;
   mEffect->mLevel = mLevel;

   mEffect->Preview();
   
	mEffect->mGain = oldGain;
   mEffect->mDC = oldDC;
   mEffect->mLevel = oldLevel;
}

void NormalizeDialog::OnOk(wxCommandEvent &event)
{
   TransferDataFromWindow();

   EndModal(true);
}

void NormalizeDialog::OnCancel(wxCommandEvent &event)
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
// arch-tag: 0e9ab1c7-3cb3-4864-8f30-876218bea476

