/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseRemoval.cpp

  Dominic Mazzoni

  The noise is removed using noise gates on each frequency band in
  the FFT, and the signal is reconstructed using overlap/add of
  Hanning windows.

**********************************************************************/

#include "NoiseRemoval.h"

#include "../Audacity.h"
#include "../Envelope.h"
#include "../FFT.h"
#include "../WaveTrack.h"
#include "../Prefs.h"
#include "../Project.h"

#include <math.h>

#if defined(__WXMSW__) && !defined(__CYGWIN__)
#include <float.h>
#define finite(x) _finite(x)
#endif

#ifdef __MACOS9__
#include <fp.h>
#define finite(x) isfinite(x)
#endif

#include <wx/file.h>
#include <wx/ffile.h>
#include <wx/bitmap.h>
#include <wx/brush.h>
#include <wx/button.h>
#include <wx/dcmemory.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>

#include "../AudacityApp.h"

EffectNoiseRemoval::EffectNoiseRemoval()
{
   windowSize = 2048;
   int halfWindowSize = windowSize / 2;
   mNoiseGate = new float[windowSize];

   sum = new float[windowSize];
   sumsq = new float[windowSize];
   profileCount = new int[windowSize];
   smoothing = new float[windowSize];
   mHasProfile = false;

   // These two are safe to do, even if not in CleanSpeechMode
   wxGetApp().SetCleanSpeechNoiseGate(mNoiseGate);
   wxGetApp().SetCleanSpeechNoiseGateExpectedCount(halfWindowSize * sizeof(float));
	CleanSpeechMayReadNoisegate();
   Init();
}

EffectNoiseRemoval::~EffectNoiseRemoval()
{
   delete [] mNoiseGate;
   delete [] sum;
   delete [] sumsq;
   delete [] profileCount;
   delete [] smoothing;
}

void EffectNoiseRemoval::CleanSpeechMayReadNoisegate()
{
   int halfWindowSize = windowSize / 2;
	AudacityProject * project = GetActiveProject();
	if( !project || !project->GetCleanSpeechMode() )
      return;
   
   // Try to open the file.
   wxString filename = FILENAME(wxT("noisegate.nrp"));
   // if file doesn't exist, return quietly.
   if( !wxFile::Exists( filename ))
      return;
	wxFFile   noiseGateFile((const wxChar*)filename, (const wxChar*)wxT("rb"));
   bool flag = noiseGateFile.IsOpened();
   if (flag != true)
      return;

   // Now get its data.
   int expectedCount = halfWindowSize * sizeof(float);
   int count = noiseGateFile.Read(mNoiseGate, expectedCount);
   noiseGateFile.Close();
   if (count == expectedCount) {
      for (int i = halfWindowSize; i < windowSize; ++i) {
         mNoiseGate[i] = float(0.0);  // only half filled by Read
      }
      mHasProfile = true;
      mDoProfile = false;
   }
}

void EffectNoiseRemoval::CleanSpeechMayWriteNoiseGate()
{
	AudacityProject * project = GetActiveProject();
	if( !project || !project->GetCleanSpeechMode() )
      return;
   wxFFile   noiseGateFile((const wxChar*)FILENAME(wxT("noisegate.nrp")), (const wxChar*)wxT("wb"));
   bool flag = noiseGateFile.IsOpened();
   if (flag == true) {
      int expectedCount = (windowSize / 2) * sizeof(float);
      int count = noiseGateFile.Write(mNoiseGate, expectedCount);
      noiseGateFile.Close();
   }
}

#define MAX_NOISE_LEVEL  30
bool EffectNoiseRemoval::Init()
{
	mLevel = 1;//set a low level of noise removal as the default.
   return true;
}

bool EffectNoiseRemoval::CheckWhetherSkipEffect()
{
   bool rc = (mLevel == 0);
   return rc;
}

bool EffectNoiseRemoval::PromptUser()
{
   NoiseRemovalDialog dlog(this, mParent, -1, _("Noise Removal"));
   dlog.m_pSlider->SetValue(mLevel);
   dlog.mLevel = mLevel;

   if( !mHasProfile )
   {
   	CleanSpeechMayReadNoisegate();
   }

   // We may want to twiddle the levels if we are setting
   // from an automation dialog, the only case in which we can
   // get here without any wavetracks.
   bool bAllowTwiddleSettings = (mWaveTracks==NULL); 
   if (mHasProfile || bAllowTwiddleSettings ) {
      dlog.m_pButton_Preview->Enable(mWaveTracks != NULL);
		dlog.m_pButton_RemoveNoise->SetDefault();
		dlog.m_pButton_RemoveNoise->SetFocus();
	} else {
      dlog.m_pSlider->Enable(false);
      dlog.m_pButton_Preview->Enable(false);
      dlog.m_pButton_RemoveNoise->Enable(false);
   }
   dlog.CentreOnParent();
   dlog.ShowModal();
   
   if (dlog.GetReturnCode() == 0) {
      return false;
   }
   mLevel = dlog.m_pSlider->GetValue();

   mDoProfile = (dlog.GetReturnCode() == 1);
   return true;
}
   
bool EffectNoiseRemoval::TransferParameters( Shuttle & shuttle )
{  
   shuttle.TransferInt(wxT("Level"),mLevel,1);
   return true;
}

bool EffectNoiseRemoval::Process()
{
	// If we are creating a profile, we don't care whether we have
	// one already.  We just prepare the counters.
   if (mDoProfile) {
      for(int i=0; i<windowSize; i++) {
         sum[i] = float(0.0);
         sumsq[i] = float(0.0);
         profileCount[i] = 0;
      }
   }
	else
	{
		// We need a profile.
      if( !mHasProfile )
      {
         CleanSpeechMayReadNoisegate();
      }
   
	   // If we still don't have a profile we have a problem.
      if( !mHasProfile)
      {
         wxMessageBox( _("Attempt to run Noise Removal without a noise profile\n.") );
         return false;
      }
	}

	// This same code will both remove noise and
	// profile it, depending on 'mDoProfile'
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   int count = 0;
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;

      if (t1 > t0) {
         longSampleCount start = track->TimeToLongSamples(t0);
         longSampleCount end = track->TimeToLongSamples(t1);
         sampleCount len = (sampleCount)(end - start);

         if (!ProcessOne(count, track, start, len)){
            return false;
	      }
      }
      track = (WaveTrack *) iter.Next();
      count++;
   }

   if (mDoProfile) {
      for(int i=0; i<=windowSize/2; i++) {
         //float stddev = sqrt(sumsq[i] - (sum[i]*sum[i])/profileCount[i])
         //                               / profileCount[i];
         mNoiseGate[i] = sum[i] / profileCount[i]; // average
      }
		CleanSpeechMayWriteNoiseGate();
      mHasProfile = true;
      mDoProfile = false;  //lda
   }
   return true;
}

bool EffectNoiseRemoval::ProcessOne(int count, WaveTrack * track,
                                    longSampleCount start, sampleCount len)
{
   bool retCode = true;
   sampleCount s = 0;
//ANSWER-ME: Why the smaller block size (/2) for CleanSpeech mode??
   sampleCount idealBlockLen = track->GetMaxBlockSize() * 4;
//   sampleCount idealBlockLen = track->GetMaxBlockSize() / 2;

   if (idealBlockLen % windowSize != 0)
      idealBlockLen += (windowSize - (idealBlockLen % windowSize));
   
   float *buffer = new float[idealBlockLen];
   
   float *window1 = new float[windowSize];
   float *window2 = new float[windowSize];
   float *thisWindow = window1;
   float *lastWindow = window2;
   
   int i;
   
   for(i=0; i<windowSize; i++) {
      lastWindow[i] = 0;
      smoothing[i] = float(0.0);
   }
   
   while((s < len)&&((len-s)>(windowSize/2))) {
      sampleCount block = idealBlockLen;
      if (s + block > len)
         block = len - s;
      
      track->Get((samplePtr) buffer, floatSample, start + s, block);
      
      for(i=0; i<(block-windowSize/2); i+=windowSize/2) {
         int wcopy = windowSize;
         if (i + wcopy > block)
            wcopy = block - i;
         
         int j;
         for(j=0; j<wcopy; j++)
            thisWindow[j] = buffer[i+j];
         for(j=wcopy; j<windowSize; j++)
            thisWindow[j] = 0;
         
         if (mDoProfile)
            GetProfile(windowSize, thisWindow);
         else {
				//TIDY-ME: could we just test mLevel=<0 in CheckWhetherSkipEffect?
            if (mLevel > 0) { // Skip NoiseRemoval if zero ... may apply for CleanChain
            RemoveNoise(windowSize, thisWindow);
               for(j=0; j<windowSize/2; j++) {
               	buffer[i+j] = thisWindow[j] + lastWindow[windowSize/2 + j];
         		}
            }
         }
         
         float *tempP = thisWindow;
         thisWindow = lastWindow;
         lastWindow = tempP;
      }
      
      // Shift by half-a-window less than the block size we loaded
      // (so that the blocks properly overlap)
      block -= windowSize/2;

      if (!mDoProfile)
         track->Set((samplePtr) buffer, floatSample, start + s, block);
      
      s += block;
      
      if (TrackProgress(count, s / (double) len)) {
         retCode = false;
         break;
      }
   }
   delete[] buffer;
   delete[] window1;
   delete[] window2;
   
   return retCode;
}

void EffectNoiseRemoval::GetProfile(sampleCount len,
                                    float *buffer)
{
   float *in = new float[len];
   float *out = new float[len];
   
   int i;
   
   for(i=0; i<len; i++)
      in[i] = buffer[i];

   // Apply window and FFT
   /* WindowFunc(3, len, in); // Hanning window */
   PowerSpectrum(len, in, out);
   
   for(i=0; i<=len/2; i++) {
      float value = log(out[i]);
      
      if (finite(value)) {
         sum[i] += value;
         sumsq[i] += value*value;
         profileCount[i]++;
      }
   }

   delete[] in;
   delete[] out;
}

void EffectNoiseRemoval::RemoveNoise(sampleCount len, float *buffer)
{
   float *inr = new float[len];
   float *ini = new float[len];
   float *outr = new float[len];
   float *outi = new float[len];
   float *power = new float[len];
   float *plog = new float[len];
   
   int i;
   
   for(i=0; i<len; i++)
      inr[i] = buffer[i];

   // Apply window and FFT
   /* WindowFunc(3, len, inr); // Hanning window */
   FFT(len, false, inr, NULL, outr, outi);

   for(i=0; i<len; i++)
      inr[i] = buffer[i];
   WindowFunc(3, len, inr); // Hanning window
   PowerSpectrum(len, inr, power);

   for(i=0; i<=len/2; i++)
      plog[i] = log(power[i]);
    
   int half = len/2;
   for(i=0; i<=half; i++) {
      float smooth;
      
//Factor of 4 here replaces previous 2, giving a 
//finer gradation of noise removal choice.      
      if (plog[i] < mNoiseGate[i] + (mLevel / 4.0))
         smooth = float(0.0);
      else
         smooth = float(1.0);
      
      smoothing[i] = smooth * 0.5 + smoothing[i] * 0.5;
   }

   /* try to eliminate tinkle bells */
   for(i=2; i<=half-2; i++) {
      if (smoothing[i]>=0.5 &&
          smoothing[i]<=0.55 &&
          smoothing[i-1]<0.1 &&
          smoothing[i-2]<0.1 &&
          smoothing[i+1]<0.1 &&
          smoothing[i+2]<0.1)
          smoothing[i] = float(0.0);
   }

   outr[0] *= smoothing[0];
   outi[0] *= smoothing[0];
   outr[half] *= smoothing[half];
   outi[half] *= smoothing[half];
   for(i=1; i<half; i++) {
      int j = len - i;
      float smooth = smoothing[i];

      outr[i] *= smooth;
      outi[i] *= smooth;
      outr[j] *= smooth;
      outi[j] *= smooth;
   }

   // Inverse FFT and normalization
   FFT(len, true, outr, outi, inr, ini);
   WindowFunc(3, len, inr); // Hanning window */
   
   for(i=0; i<len; i++)
      buffer[i] = inr[i];

   delete[] inr;
   delete[] ini;
   delete[] outr;
   delete[] outi;
   delete[] power;
   delete[] plog;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// NoiseRemovalDialog
//----------------------------------------------------------------------------

// WDR: event table for NoiseRemovalDialog

enum {
   ID_BUTTON_GETPROFILE = 10001,
	ID_BUTTON_PREVIEW
};

BEGIN_EVENT_TABLE(NoiseRemovalDialog,wxDialog)
	EVT_BUTTON(wxID_OK, NoiseRemovalDialog::OnRemoveNoise)
	EVT_BUTTON(wxID_CANCEL, NoiseRemovalDialog::OnCancel)
	EVT_BUTTON(ID_BUTTON_GETPROFILE, NoiseRemovalDialog::OnGetProfile)
	EVT_BUTTON(ID_BUTTON_PREVIEW, NoiseRemovalDialog::OnPreview)
END_EVENT_TABLE()

NoiseRemovalDialog::NoiseRemovalDialog(EffectNoiseRemoval * effect, 
													wxWindow *parent, wxWindowID id,
													const wxString &title,
													const wxPoint &position, 
													const wxSize& size,
													long style ) :
   wxDialog( parent, id, title, position, size, style )
{
	m_pEffect = effect;
   
	// NULL out the control members until the controls are created.
	m_pButton_GetProfile = NULL;
	m_pSlider = NULL;
	m_pButton_Preview = NULL;
	m_pButton_RemoveNoise = NULL;

   this->MakeNoiseRemovalDialog(true); 
}


// WDR: handler implementations for NoiseRemovalDialog

void NoiseRemovalDialog::OnGetProfile( wxCommandEvent &event )
{
   EndModal(1);
}

void NoiseRemovalDialog::OnPreview(wxCommandEvent &event)
{
	// Save & restore parameters around Preview, because we didn't do OK.
   bool oldDoProfile = m_pEffect->mDoProfile;
	int oldLevel = m_pEffect->mLevel;

	m_pEffect->mDoProfile = false;
	m_pEffect->mLevel = m_pSlider->GetValue();
   
	m_pEffect->Preview();
   
	m_pEffect->mDoProfile = oldDoProfile;
	m_pEffect->mLevel = oldLevel; 
}

void NoiseRemovalDialog::OnRemoveNoise( wxCommandEvent &event )
{
   EndModal(2);
}

void NoiseRemovalDialog::OnCancel(wxCommandEvent &event)
{
   EndModal(0);
}

wxSizer *NoiseRemovalDialog::MakeNoiseRemovalDialog(bool call_fit /* = true */, 
																	 bool set_sizer /* = true */)
{
   wxBoxSizer *mainSizer = new wxBoxSizer( wxVERTICAL );
   wxStaticBoxSizer *group;
   wxControl *item;
   
   item = new wxStaticText(this, -1,
                   _("Noise Removal by Dominic Mazzoni"), wxDefaultPosition,
                   wxDefaultSize, wxALIGN_CENTRE );
   mainSizer->Add(item, 0, wxALIGN_CENTRE|wxALL, 5);
	AudacityProject * p;
	p=GetActiveProject();
	bool bCleanSpeechMode = p && p->GetCleanSpeechMode();
	if( bCleanSpeechMode )
	{
      item = new wxStaticText(this, -1,
         _("Entire recording should have been Normalized prior to this step.\n\n"
           "For noisy speech, find the best tradeoff for quieting the gaps between phrases and\n"
           "not causing the voice to sound distorted. For good audio with low noise, a setting\n"
           "more to the left should work well. Leveling and TruncateSilence work better with\n"
           "more of the noise removed, even if the voice ends up sounding somewhat distorted.\n"
           "Your objective is that the softly spoken words can be heard more clearly."),
           wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
      	  mainSizer->Add(item, 0, wxALIGN_LEFT|wxALL, 15);
	}
   // Step 1
   group = new wxStaticBoxSizer(new wxStaticBox(this, -1,
	bCleanSpeechMode ? 
		_("Preparation Steps")
	: 	_("Step 1")
		), wxVERTICAL);

   item = new wxStaticText(this, -1,
	bCleanSpeechMode ?
      _("Listen carefully to section with some speech and some silence to check before/after.\n"
        "Select a few seconds of just noise ('thinner' part of wave pattern usually between\n"
        "spoken phrases or during pauses) so Audacity knows what to filter out, then click")
	:  _("Select a few seconds of just noise\n"
                             "so Audacity knows what to filter out, then\n"
                             "click Get Noise Profile:"),
                           wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );

   group->Add(item, 0, wxALIGN_CENTRE|wxALL, 5 );

   m_pButton_GetProfile = new wxButton(this, ID_BUTTON_GETPROFILE, _("Get Noise Profile"), wxDefaultPosition, wxDefaultSize, 0 );
   group->Add(m_pButton_GetProfile, 0, wxALIGN_CENTRE|wxALL, 5 );

   mainSizer->Add( group, 1, wxALIGN_CENTRE|wxALL|wxGROW, 5 );
   
   // Step 2
   
   group = new wxStaticBoxSizer(new wxStaticBox(this, -1,
			bCleanSpeechMode ? 
         	_("Actually Remove Noise")
			:  _("Step 2")), wxVERTICAL);

   item = new wxStaticText(this, -1,
	bCleanSpeechMode ?
      _("Select what part of the audio you want filtered (Ctrl-A = All), chose how much noise\n"
        "you want filtered out with Slider below, and then click 'OK' to remove noise.\n"
        "Find best setting with Ctrl-Z to Undo, Select All, and change Slider position.")
   :	_("Select all of the audio you want filtered,\n"
                             "choose how much noise you want filtered out,\n"
        "and then click 'OK' to remove noise.\n"),
                           wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
   group->Add(item, 0, wxALIGN_CENTRE|wxALL, 5 );

   m_pSlider = new wxSlider(this, -1, mLevel, 1, MAX_NOISE_LEVEL,  
										wxDefaultPosition, wxDefaultSize, wxSL_HORIZONTAL);
   group->Add(m_pSlider, 1, wxEXPAND|wxALIGN_CENTRE|wxLEFT | wxRIGHT | wxTOP, 5 );

   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);
   item = new wxStaticText(this, -1, _("Less"));
   hSizer->Add(item, 0, wxALIGN_CENTRE|wxLEFT | wxRIGHT | wxBOTTOM, 5 );   
   hSizer->Add(10, 10, 1, wxALIGN_CENTRE | wxLEFT | wxRIGHT | wxBOTTOM, 5);
	if( bCleanSpeechMode )
	{
      item = new wxStaticText(this, -1, _("Medium"));
      hSizer->Add(item, 0, wxALIGN_CENTRE|wxLEFT | wxRIGHT | wxBOTTOM, 5 );   
      hSizer->Add(10, 10, 1, wxALIGN_CENTRE | wxLEFT | wxRIGHT | wxBOTTOM, 5);
	}
   item = new wxStaticText(this, -1, 
	bCleanSpeechMode ?
		_("Extreme (May Distort)")
	:	_("More"));
   hSizer->Add(item, 0, wxALIGN_CENTRE|wxLEFT | wxRIGHT | wxBOTTOM, 5 );
   group->Add(hSizer, 1, wxEXPAND|wxALIGN_CENTRE|wxALL, 5 );
   
   hSizer = new wxBoxSizer(wxHORIZONTAL);

   m_pButton_Preview = new wxButton(this, ID_BUTTON_PREVIEW, m_pEffect->GetPreviewName());
   hSizer->Add(m_pButton_Preview, 0, wxALIGN_LEFT | wxALL, 5);
   
   hSizer->Add(25, 5); // horizontal spacer

   item = new wxButton( this, wxID_CANCEL, _("Cancel"), wxDefaultPosition, wxDefaultSize, 0 );
   hSizer->Add(item, 0, wxALIGN_CENTER | wxALL, 5 );

   hSizer->Add(25, 5); // horizontal spacer
   
	m_pButton_RemoveNoise = new wxButton(this, wxID_OK, _("OK"), wxDefaultPosition, wxDefaultSize, 0 );
   hSizer->Add(m_pButton_RemoveNoise, 0, wxALIGN_RIGHT | wxALL, 5 );

	group->Add(hSizer, 0, wxALIGN_CENTER | wxALL, 5 );
   mainSizer->Add( group, 0, wxALIGN_CENTRE|wxALL|wxGROW, 5 );

   if (set_sizer) {
      this->SetAutoLayout( TRUE );
      this->SetSizer( mainSizer );
      if (call_fit) {
         mainSizer->Fit( this );
         mainSizer->SetSizeHints( this );
      }
   }
    
   return mainSizer;
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
// arch-tag: 685e0d8c-89eb-427b-8933-af606cf33c2b

