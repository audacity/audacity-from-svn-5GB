/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseRemoval.cpp

  Dominic Mazzoni

  This effect attempts 

  The noise is removed using noise gates on each frequency band in
  the FFT, and the signal is reconstructed using overlap/add of
  Hanning windows.

**********************************************************************/

#include <math.h>

#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/brush.h>
#include <wx/image.h>
#include <wx/dcmemory.h>

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
}

bool EffectNoiseRemoval::PromptUser()
{
   NoiseRemovalDialog dlog(mParent, -1, "Noise Removal");
   dlog.CentreOnParent();
   dlog.ShowModal();
   
   if (dlog.GetReturnCode() == 0)
      return false;

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
   VTrack *t = iter.First();
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
         float avg = sum[i] / profileCount[i];
         float stddev = sqrt(sumsq[i] - (sum[i]*sum[i])/profileCount[i]) / profileCount[i];

         noiseGate[i] = avg + 3.0; //2*stddev;
      }
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
   
   sampleType *buffer = new sampleType[idealBlockLen];
   
   sampleType *window1 = new sampleType[windowSize];
   sampleType *window2 = new sampleType[windowSize];
   sampleType *thisWindow = window1;
   sampleType *lastWindow = window2;
   
   sampleCount originalLen = len;
   
   int i;
   
   for(i=0; i<windowSize; i++)
      lastWindow[i] = 0;
   
   while(len) {
      int block = idealBlockLen;
      if (block > len)
         block = len;
      
      t->Get(buffer, s, block);
      
      for(i=0; i<block; i+=windowSize/2) {
         int wlen = i + windowSize;
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
         
         sampleType *tempP = thisWindow;
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
                                    sampleType *buffer)
{
   float *in = new float[len];
   float *out = new float[len];
   
   int i;
   
   for(i=0; i<len; i++)
      in[i] = buffer[i]/32767.;

   // Apply window and FFT
   WindowFunc(3, len, in); // Hanning window
   PowerSpectrum(len, in, out);
   
   for(i=0; i<=len/2; i++) {
      float value = log(out[i]);
      
      if (isfinite(value)) {
         sum[i] += value;
         sumsq[i] += value*value;
         profileCount[i]++;
      }
   }

   delete[] in;
   delete[] out;
}

void EffectNoiseRemoval::RemoveNoise(sampleCount len,
                                     sampleType *buffer)
{
   float *inr = new float[len];
   float *ini = new float[len];
   float *outr = new float[len];
   float *outi = new float[len];
   float *power = new float[len];
   float *plog = new float[len];
   
   int i;
   
   for(i=0; i<len; i++)
      inr[i] = buffer[i]/32767.;

   // Apply window and FFT
   WindowFunc(3, len, inr); // Hanning window
   FFT(len, false, inr, NULL, outr, outi);

   for(i=0; i<len; i++)
      inr[i] = buffer[i]/32767.;
   WindowFunc(3, len, inr); // Hanning window
   PowerSpectrum(len, inr, power);

   for(i=0; i<=len/2; i++)
      plog[i] = log(power[i]);


/* bad 2   
   int half = len/2;
   for(i=1; i<half; i++) {
      int j = len - i;
      
      if (plog[i] < noiseGate[i] && plog[i-1] < noiseGate[i-1]) {
         outr[i] = 0;
         outi[i] = 0;
      
         outr[j] = 0;
         outi[j] = 0;
      }
   }

   // Inverse FFT and normalization
   FFT(len, true, outr, outi, inr, ini);
   
   for(i=0; i<len; i++)
      buffer[i] = sampleType(inr[i]*32767);
*/   
   
   /* bad

   int half = len/2;
   for(i=1; i<half; i++) {
      int j = len - i;
      
      if (plog[i] < noiseGate[i]) {
         outr[i] = 0;
         outi[i] = 0;
      
         outr[j] = 0;
         outi[j] = 0;
      }
   }
   
   inr[0] = outr[0];
   ini[0] = outi[0];
   inr[1] = (outr[1] + outr[2])/2;
   ini[1] = (outi[1] + outr[2])/2;
   inr[len-1] = (outr[len-1] + outr[len-2])/2;
   ini[len-1] = (outi[len-1] + outr[len-2])/2;
   inr[half] = outr[half];
   ini[half] = outi[half];
   for(i=2; i<half; i++) {
      int j = len - i;
      inr[i] = (outr[i-1]+outr[i]+outr[i+1])/3;
      ini[i] = (outi[i-1]+outi[i]+outi[i+1])/3;
      inr[j] = (outr[j-1]+outr[j]+outr[j+1])/3;
      ini[j] = (outi[j-1]+outi[j]+outi[j+1])/3;
   }
   
   // Inverse FFT and normalization
   FFT(len, true, inr, ini, outr, outi);
   
   for(i=0; i<len; i++)
      buffer[i] = sampleType(outr[i]*32767);
   */
    
   int half = len/2;
   for(i=0; i<=half; i++) {
      int j = len - i;
      
      if (plog[i] < noiseGate[i]) {
         outr[i] = 0;
         outi[i] = 0;
      
         if (i!=0 && i!=len/2) {
            outr[j] = 0;
            outi[j] = 0;
         }
      }
   }

   // Inverse FFT and normalization
   FFT(len, true, outr, outi, inr, ini);
   
   for(i=0; i<len; i++)
      buffer[i] = sampleType(inr[i]*32767);

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

wxSizer *MakeNoiseRemovalDialog( wxPanel *parent, bool call_fit, bool set_sizer )
{
   wxBoxSizer *item0 = new wxBoxSizer( wxVERTICAL );

   wxStaticText *item1 = new wxStaticText( parent, ID_TEXT, "Noise Removal by Dominic Mazzoni", wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE );
   item0->Add( item1, 0, wxALIGN_CENTRE|wxALL, 5 );

   wxButton *item4 = new wxButton( parent, 1000, "Get Profile", wxDefaultPosition, wxDefaultSize, 0 );
   item0->Add( item4, 0, wxALIGN_CENTRE|wxALL, 5 );

   wxButton *item5 = new wxButton( parent, 1001, "Remove Noise", wxDefaultPosition, wxDefaultSize, 0 );
   item0->Add( item5, 0, wxALIGN_CENTRE|wxALL, 5 );

   wxButton *item6 = new wxButton( parent, 1002, "Cancel", wxDefaultPosition, wxDefaultSize, 0 );
   item0->Add( item6, 0, wxALIGN_CENTRE|wxALL, 5 );

   if (set_sizer) {
      parent->SetAutoLayout( TRUE );
      parent->SetSizer( item0 );
      if (call_fit) {
         item0->Fit( parent );
         item0->SetSizeHints( parent );
      }
   }
    
   return item0;
}
