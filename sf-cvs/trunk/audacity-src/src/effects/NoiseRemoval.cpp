/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseRemoval.cpp

  Dominic Mazzoni

  The noise is removed using noise gates on each frequency band in
  the FFT, and the signal is reconstructed using overlap/add of
  Hanning windows.

**********************************************************************/

#include "../Audacity.h"

#include <math.h>

#ifdef __WXMSW__
#include <float.h>
#define finite(x) _finite(x)
#endif

#ifdef __MACOSX__
#include <math.h>
#endif

#ifdef __MACOS9__
#include <fp.h>
#define finite(x) isfinite(x)
#endif

#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/brush.h>
#include <wx/image.h>
#include <wx/dcmemory.h>
#include <wx/statbox.h>
#include <wx/intl.h>

#include "NoiseRemoval.h"
#include "../Envelope.h"
#include "../FFT.h"
#include "../WaveTrack.h"

EffectNoiseRemoval::EffectNoiseRemoval()
{
   windowSize = 2048;
   noiseGate = new float[windowSize];
   sum = new float[windowSize];
   sumsq = new float[windowSize];
   profileCount = new int[windowSize];
   smoothing = new float[windowSize];
   hasProfile = false;
   level = 8;
}

bool EffectNoiseRemoval::PromptUser()
{
   NoiseRemovalDialog dlog(mParent, -1, _("Noise Removal"));
   if (hasProfile)
      dlog.mSlider->SetValue(level);
   else {
      dlog.mRemoveNoiseButton->Enable(false);
      dlog.mSlider->Enable(false);
   }
   dlog.CentreOnParent();
   dlog.ShowModal();
   
   if (dlog.GetReturnCode() == 0)
      return false;

   level = dlog.mSlider->GetValue();

   if (dlog.GetReturnCode() == 1)
      doProfile = true;
   else
      doProfile = false;
   
   return true;
}

bool EffectNoiseRemoval::Process()
{
   if (doProfile) {
      for(int i=0; i<windowSize; i++) {
         sum[i] = 0.0;
         sumsq[i] = 0.0;
         profileCount[i] = 0;
      }
   }

   TrackListIterator iter(mWaveTracks);
   Track *t = iter.First();
   int count = 0;
   while(t) {
      sampleCount start, len;
      GetSamples((WaveTrack *)t, &start, &len);
      
      bool success = ProcessOne(count, (WaveTrack *)t, start, len);
      
      if (!success)
         return false;
   
      t = iter.Next();
      count++;
   }
   
   if (doProfile) {
      for(int i=0; i<=windowSize/2; i++) {
         //float stddev = sqrt(sumsq[i] - (sum[i]*sum[i])/profileCount[i])
         //                               / profileCount[i];
         noiseGate[i] = sum[i] / profileCount[i]; // average
      }
      
      hasProfile = true;
   }
   return true;
}

bool EffectNoiseRemoval::ProcessOne(int count, WaveTrack * t,
                                    sampleCount start, sampleCount len)
{
   sampleCount s = start;
   sampleCount idealBlockLen = t->GetMaxBlockSize() * 4;
   
   if (idealBlockLen % windowSize != 0)
      idealBlockLen += (windowSize - (idealBlockLen % windowSize));
   
   float *buffer = new float[idealBlockLen];
   
   float *window1 = new float[windowSize];
   float *window2 = new float[windowSize];
   float *thisWindow = window1;
   float *lastWindow = window2;
   
   sampleCount originalLen = len;
   
   int i;
   
   for(i=0; i<windowSize; i++) {
      lastWindow[i] = 0;
      smoothing[i] = 0.0;
   }
   
   while(len) {
      sampleCount block = idealBlockLen;
      if (block > len)
         block = len;
      
      t->Get(buffer, s, block);
      
      for(i=0; i<block; i+=windowSize/2) {
         int wcopy = windowSize;
         if (i + wcopy > block)
            wcopy = block - i;
         
         int j;
         for(j=0; j<wcopy; j++)
            thisWindow[j] = buffer[i+j];
         for(j=wcopy; j<windowSize; j++)
            thisWindow[j] = 0;
         
         if (doProfile)
            GetProfile(windowSize, thisWindow);
         else {
            RemoveNoise(windowSize, thisWindow);
            for(j=0; j<windowSize/2; j++)
               buffer[i+j] = thisWindow[j] + lastWindow[windowSize/2 + j];
         }
         
         float *tempP = thisWindow;
         thisWindow = lastWindow;
         lastWindow = tempP;
      }
      
      if (len > block && len > windowSize/2)
         block -= windowSize/2;
      
      if (!doProfile)
         t->Set(buffer, s, block);
      
      len -= block;
      s += block;
      
      TrackProgress(count, (s-start)/(double)originalLen);
   }
   
   delete[] buffer;
   delete[] window1;
   delete[] window2;
   
   return true;
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
   WindowFunc(3, len, in); // Hanning window
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

void EffectNoiseRemoval::RemoveNoise(sampleCount len,
                                     float *buffer)
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
   WindowFunc(3, len, inr); // Hanning window
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
      
      if (plog[i] < noiseGate[i] + (level/2.0))
         smooth = 0.0;
      else
         smooth = 1.0;
      
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
         smoothing[i] = 0.0;
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

BEGIN_EVENT_TABLE(NoiseRemovalDialog,wxDialog)
   EVT_BUTTON( 1000, NoiseRemovalDialog::OnGetProfile )
   EVT_BUTTON( 1001, NoiseRemovalDialog::OnRemoveNoise )
   EVT_BUTTON( 1002, NoiseRemovalDialog::OnCancel )
END_EVENT_TABLE()

NoiseRemovalDialog::NoiseRemovalDialog(wxWindow *parent, wxWindowID id,
                           const wxString &title,
                           const wxPoint &position, const wxSize& size,
                           long style ) :
   wxDialog( parent, id, title, position, size, style )
{
   MakeNoiseRemovalDialog( this, TRUE ); 
}


// WDR: handler implementations for NoiseRemovalDialog

void NoiseRemovalDialog::OnGetProfile( wxCommandEvent &event )
{
   EndModal(1);
}

void NoiseRemovalDialog::OnRemoveNoise( wxCommandEvent &event )
{
   EndModal(2);
}

void NoiseRemovalDialog::OnCancel(wxCommandEvent &event)
{
   EndModal(0);
}

wxSizer *NoiseRemovalDialog::MakeNoiseRemovalDialog( wxWindow *parent, bool call_fit, bool set_sizer )
{
   wxBoxSizer *mainSizer = new wxBoxSizer( wxVERTICAL );
   wxStaticBoxSizer *group;
   wxControl *item;
   
   item = new wxStaticText(parent, ID_TEXT,
                   _("Noise Removal by Dominic Mazzoni"), wxDefaultPosition,
                   wxDefaultSize, wxALIGN_CENTRE );
   mainSizer->Add(item, 0, wxALIGN_CENTRE|wxALL, 5);

   // Step 1
   
   group = new wxStaticBoxSizer(new wxStaticBox(parent, -1,
                                                _("Step 1")), wxVERTICAL);

   item = new wxStaticText(parent, ID_TEXT,
                           _("Select a few seconds of just noise\n"
                             "so Audacity knows what to filter out, then\n"
                             "click Get Noise Profile:"),
                           wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
   group->Add(item, 0, wxALIGN_CENTRE|wxALL, 5 );

   item = new wxButton(parent, 1000, _("Get Noise Profile"), wxDefaultPosition, wxDefaultSize, 0 );
   group->Add(item, 0, wxALIGN_CENTRE|wxALL, 5 );

   mainSizer->Add( group, 0, wxALIGN_CENTRE|wxALL, 5 );
   
   // Step 2
   
   group = new wxStaticBoxSizer(new wxStaticBox(parent, -1,
                                                _("Step 2")), wxVERTICAL);

   item = new wxStaticText(parent, ID_TEXT,
                           _("Select all of the audio you want filtered,\n"
                             "choose how much noise you want filtered out,\n"
                             "and then click Remove Noise.\n"),
                           wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
   group->Add(item, 0, wxALIGN_CENTRE|wxALL, 5 );

   mSlider = new wxSlider(parent, -1, 8, 1, 15,
                       wxDefaultPosition, wxDefaultSize, wxSL_HORIZONTAL);
   group->Add(mSlider, 1, wxEXPAND|wxALIGN_CENTRE|wxLEFT | wxRIGHT | wxTOP, 5 );

   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);
   item = new wxStaticText(parent, ID_TEXT, _("Less"));
   hSizer->Add(item, 0, wxALIGN_CENTRE|wxLEFT | wxRIGHT | wxBOTTOM, 5 );   
   hSizer->Add(10, 10, 1, wxALIGN_CENTRE | wxLEFT | wxRIGHT | wxBOTTOM, 5);
   item = new wxStaticText(parent, ID_TEXT, _("More"));
   hSizer->Add(item, 0, wxALIGN_CENTRE|wxLEFT | wxRIGHT | wxBOTTOM, 5 );

   group->Add(hSizer, 1, wxEXPAND|wxALIGN_CENTRE|wxALL, 5 );
   
   mRemoveNoiseButton = new wxButton(parent, 1001, _("Remove Noise"), wxDefaultPosition, wxDefaultSize, 0 );
   group->Add(mRemoveNoiseButton, 0, wxALIGN_CENTRE|wxALL, 5 );

   mainSizer->Add( group, 0, wxALIGN_CENTRE|wxALL, 5 );
   
   // Cancel button

   item = new wxButton( parent, 1002, _("Close"), wxDefaultPosition, wxDefaultSize, 0 );
   mainSizer->Add(item, 0, wxALIGN_CENTRE|wxALL, 5 );

   if (set_sizer) {
      parent->SetAutoLayout( TRUE );
      parent->SetSizer( mainSizer );
      if (call_fit) {
         mainSizer->Fit( parent );
         mainSizer->SetSizeHints( parent );
      }
   }
    
   return mainSizer;
}
