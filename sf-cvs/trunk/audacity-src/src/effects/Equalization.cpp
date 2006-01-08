/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectEqualization.cpp

  Mitch Golden
  Vaughan Johnson (Preview)

  Applies an FFT of certain specific equalization curves, suitable
  for old recordings.

  Clone of the FFT Filter effect, see documentation there.

  Martyn Shaw made it do FIR filters using the overlap-add method, with
  variable filter length.
  Also added the animated 'response' curve.
  Also added the graphic EQ.

**********************************************************************/

#include "Equalization.h"
#include "../Audacity.h"
#include "../Envelope.h"
#include "../FFT.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../WaveTrack.h"
#include "../widgets/Ruler.h"
#include "../xml/XMLFileReader.h"

#include <wx/bitmap.h>
#include <wx/button.h>
#include <wx/msgdlg.h>
#include <wx/brush.h>
#include <wx/dcmemory.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/choice.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textdlg.h>
#include <wx/ffile.h>
#include <wx/filedlg.h>
#include <wx/filefn.h>
#include <wx/stdpaths.h>

#include <math.h>

#include <wx/arrimpl.cpp>
WX_DEFINE_OBJARRAY( EQPointArray );
WX_DEFINE_OBJARRAY( EQCurveArray );

const float EqualizationDialog::thirdOct[] =
  {
    20., 25., 31., 40., 50., 63., 80., 100., 125., 160., 200.,
	250., 315., 400., 500., 630., 800., 1000., 1250., 1600., 2000.,
	2500., 3150., 4000., 5000., 6300., 8000., 10000., 12500., 16000., 20000.,
  };

const float EffectEqualization::curvex[] =
  {
    30., 31., 50., 63., 70., 100., 125., 200., 250., 300.,
    400., 500., 600., 700., 800., 900., 1000., 2000., 3000., 4000.,
    5000., 6000., 7000., 8000., 9000., 10000., 15000., 16000.
  };

// Don't want to be warned about double->float precision loss here
// when compiling with MSVC.
#pragma warning( disable: 4305 )
const float EffectEqualization::curvey[][nCurvePoints] =
   {
      {
	// flat
	0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
	0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
      },
      {
	// amradio
//  30   31   50   63   70   100  125  200  250  300  400  500  600  700  800  900  1000 2000 3000 4000 5000 6000 7000 8000 9000 10000 15000 16000.
	-20.,-20.,-20.,-20.,-20.,-20.,-16.,-12., -8., -4., 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -4., -8.,-12.,-16.,-20.0,-20.,-20.,-20.,-20., -20., -20.0
      },
      {
	// acoustic (see p 52)
	-20.0, -20.0, -20.0,   5.0,   4.4,   3.3,   2.5,   1.7,   0.0,   0.0,
	0.0,     0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  -1.5,  -2.5,
	-3.3,   -4.0,  -4.5,  -5.0, -10.0, -15.0, -20.0, -20.0
      },
      {
	// NAB
	20.0,   20.0,  16.0,  15.6,  15.5,  13.8,  12.0,   8.0,   6.2,   5.0,
	3.0,     1.7,   1.0,   0.0,  -0.5,  -1.0,  -1.3,  -4.2,  -6.5,  -8.5,
	-10.2, -12.0, -13.0, -14.0, -15.0, -16.0, -20.0, -20.0
      },
      {
	// LP
	13.5,   13.2,  13.0,  12.8,  12.5,  11.0,  10.5,   8.0,   7.0,   6.0,
	3.5,     2.5,   1.5,   1.0,   0.5,  -0.5,  -1.0,  -3.5,  -6.0,  -8.0,
	-10.0, -11.5, -12.5, -13.5, -14.5, -16.0, -21.2, -22.0
      },
      {
	// AES
	22.5,   22.5,  18.0,  16.0,  15.0,  12.0,  10.0,   6.5,   5.2,   4.5,
	3.0,     2.0,   1.5,   1.0,   0.5,   0.0,   0.0,  -2.2,  -4.0,  -5.5,
	-6.7,   -8.0,  -9.0, -10.0, -11.0, -12.0, -15.5, -16.0
      },
      {
	// Decca FFRR Micro
	14.0,   14.0,  14.0,  13.8,  13.5,  12.5,  11.5,   8.5,   7.2,   6.0,
	4.0,     2.5,   1.5,   1.0,   0.5,   0.0,   0.0,  -1.5,  -3.0,  -4.5,
	-6.0,   -7.0,  -8.0,  -8.5,  -9.0, -10.0, -12.6, -13.0
      },
      {
	// Decca FFRR 78
	22.0,   21.5,  14.0,  11.2,   9.8,   6.0,   2,0,   1.5,   1.0,   0.5,
	0.0,     0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0   -0.5,
	-1.0,   -2.0,  -2.5,  -3.5,  -4.0,  -4.5,  -7.0,  -7.5
      },
      {
	// RIAA
	18.6,   18.5,  17.0,  16.0,  15.3,  13.1,  11.8,   8.2,   7.9,   5.5,
	3.8,     2.7,   2.0,   1.2,   1.0,   0.5,   0.0,  -2.6,  -4.8,  -6.6,
	-8.2,   -9.6, -10.9, -11.9, -12.9, -13.6, -17.2, -18.0
      },
      {
	// Col 78
	16.0,   16.0,  16.0,  14.0,  12.5,  10.0,   8.5,   5.0,   4.0,   3.0,
	2.0,     1.0,   0.5,   0.2,   0.0,  -0.5,  -1.0,  -3.5,  -6.0,  -8.0,
	-10.0, -11.5, -12.5, -13.5, -14.5, -16.0, -21.2, -22.0
      },
      {
	// Decca FFRR LP
	17.5,   17.2,  14.0,  12.0,  11.5,   9.0,   7.5,   5.0,   4.0,   3.0,
	2.0,     1.5,   1.0,   0.7,   0.2,   0.0,   0.0,  -4.0,  -6.7,  -8.5,
	-10.0, -11.0, -12.0, -13.0, -13.2, -14.0, -16.0, -16.0
      },
      {
	// EMI 78
	14.0,   14.0,  14.0,  12.0,  11.0,   8.0,   7.0,   4.0,   3.0,   2.0,
	1.0,     0.5,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
	0.0,     0.0,   0.0,   0.0,   0.0,   0.0,  -5.0,  -5.0
      },
      {
	// RCA Victor 1938
	24.0,   24.0,  24.0,  21.8,  20.0,  16.0,  13.0,   9.0,   7.5,   6.0,
	4.0,     3.0,   2.0,   1.5,   1.0,   0.5,   0.0,  -2.5,  -5.0,  -6.5,
	-7.5,   -8.0,  -7.7,  -7.5,  -7.5,  -7.5,  -7.5,  -7.5
      },
      {
	// RCA Victor 1947
	24.0,   24.0,  24.0,  21.8,  20.0,  16.0,  13.0,   9.0,   7.5,   6.0,
	4.0,     3.0,   2.0,   1.5,   1.0,   0.5,   0.0,  -2.5,  -5.0,  -6.5,
	-8.0,  -10.0, -11.5, -12.0, -12.5, -12.5, -12.5, -12.5
      },
      {
	// custom
	0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
	0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
      }
  };

#pragma warning( default: 4305 )

const wxChar * EffectEqualization::curveNames[] =
  {
    wxT("flat"),
    wxT("amradio"),
    wxT("acoustic"),
    wxT("NAB"),
    wxT("Columbia LP"),
    wxT("AES"),
    wxT("Decca FFRR Micro"),
    wxT("Decca FFRR 78"),
    wxT("RIAA"),
    wxT("Columbia 78"),
    wxT("Decca FFRR LP"),
    wxT("EMI 78"),
    wxT("RCA Victor 1938"),
    wxT("RCA Victor 1947"),
    wxT("custom")
  };



EffectEqualization::EffectEqualization()
{
   mFilterFuncR = new float[windowSize];
   mFilterFuncI = new float[windowSize];
}


EffectEqualization::~EffectEqualization()
{
   if(mFilterFuncR)
      delete[] mFilterFuncR;
   if(mFilterFuncI)
      delete[] mFilterFuncI;
   mFilterFuncR = NULL;
   mFilterFuncI = NULL;
}


bool EffectEqualization::PromptUser()
{
   TrackListIterator iter(mWaveTracks);
   WaveTrack *t = (WaveTrack *) iter.First();
   float hiFreq;
   if (t)
      hiFreq = ((float)(t->GetRate())/2.);
   else
      hiFreq = ((float)(GetActiveProject()->GetRate())/2.);


   EqualizationDialog dlog(this, ((double)loFreqI), hiFreq, mFilterFuncR, mFilterFuncI, windowSize, &mM,
            mParent, -1, _("Equalization"));

   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   return true;
}

bool EffectEqualization::TransferParameters( Shuttle & shuttle )
{
   //TODO: Lots of parameters...
//   shuttle.TransferInt("",,0);
   return true;
}

bool EffectEqualization::Process()
{
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

         if (!ProcessOne(count, track, start, len))
            return false;
      }

      track = (WaveTrack *) iter.Next();
      count++;
   }

   return true;
}


bool EffectEqualization::ProcessOne(int count, WaveTrack * t,
                                 sampleCount start, sampleCount len)
{
   int M = mM;	//MJS
   int L = windowSize - (M - 1);	//MJS
   sampleCount s = start;
   sampleCount idealBlockLen = t->GetMaxBlockSize() * 4;
   if (idealBlockLen % L != 0)
      idealBlockLen += (L - (idealBlockLen % L));

   float *buffer = new float[idealBlockLen];

   float *window1 = new float[windowSize];
   float *window2 = new float[windowSize];
   float *thisWindow = window1;
   float *lastWindow = window2;

   sampleCount originalLen = len;

   int i;
   for(i=0; i<windowSize; i++)
      lastWindow[i] = 0;

   TrackProgress(count, 0.);

   while(len) {
      sampleCount block = idealBlockLen;
      if (block > len)
         block = len;

      t->Get((samplePtr)buffer, floatSample, s, block);

      int j;

      for(i=0; i<block; i+=L) {	//go through block in lumps of length L
         int wcopy = L;
         if (i + wcopy > block)	//if last lump would exceed block
            wcopy = block - i;	//shorten it
         for(j=0; j<wcopy; j++)
            thisWindow[j] = buffer[i+j];	//copy the L (or remaining) samples
         for(j=wcopy; j<windowSize; j++) {
            thisWindow[j] = 0;	//this includes the padding
         }

         Filter(windowSize, thisWindow);

         for(j=0; j<M-1; j++)
            buffer[i+j] = thisWindow[j] + lastWindow[L + j];
         for(j=M-1; j<wcopy; j++)
            buffer[i+j] = thisWindow[j];

         float *tempP = thisWindow;
         thisWindow = lastWindow;
         lastWindow = tempP;
      }	//next i, windowSize lump of this block

      t->Set((samplePtr)buffer, floatSample, s, block);

      len -= block;
      s += block;

      TrackProgress(count, (s-start)/(double)originalLen);
   }

   delete[] buffer;
   delete[] window1;
   delete[] window2;

   return true;
}

void EffectEqualization::Filter(sampleCount len,
				float *buffer)
{
   float *inr = new float[len];
   float *ini = new float[len];
   float *outr = new float[len];
   float *outi = new float[len];

   int i;
   float temp;

   for(i=0; i<len; i++)
      inr[i] = buffer[i];

   // Apply FFT
   FFT(len, false, inr, NULL, outr, outi);

   // Apply filter
   for(i=0; i<len; i++) {
      temp = outr[i]*mFilterFuncR[i] - outi[i]*mFilterFuncI[i];
      outi[i] = outr[i]*mFilterFuncI[i] + outi[i]*mFilterFuncR[i];
      outr[i] = temp;
   }


   // Inverse FFT and normalization
   FFT(len, true, outr, outi, inr, ini);

   for(i=0; i<len; i++)
      buffer[i] = float(inr[i]);

   delete[] inr;
   delete[] ini;
   delete[] outr;
   delete[] outi;
}


//----------------------------------------------------------------------------
// EqualizationPanel
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(EqualizationPanel, wxPanel)
    EVT_PAINT(EqualizationPanel::OnPaint)
    EVT_MOUSE_EVENTS(EqualizationPanel::OnMouseEvent)
    EVT_SIZE(EqualizationPanel::OnSize)
END_EVENT_TABLE()

EqualizationPanel::EqualizationPanel( double loFreq, double hiFreq,
				      Envelope *env,
				      EqualizationDialog *parent,
				      float *filterFuncR, float *filterFuncI, long windowSize,
				      int *M, float *dBMin, float *dBMax, wxWindowID id,
				      const wxPoint& pos,
				      const wxSize& size):
   wxPanel(parent, id, pos, size)
{
   mBitmap = NULL;
   mWidth = 0;
   mHeight = 0;
   mLoFreq = loFreq;
   mHiFreq = hiFreq;
   mWindowSize = windowSize;
   mFilterFuncR = filterFuncR;
   mFilterFuncI = filterFuncI;
   mM = M;	//MJS
   mdBMin = dBMin;
   mdBMax = dBMax;
   mParent = parent;

   mEnvelope = env;
   mEnvelope->Flatten(0.);
   mEnvelope->Mirror(false);
   mEnvelope->SetTrackLen(1.0);

   SetSizeHints(700, 240);
}


EqualizationPanel::~EqualizationPanel()
{
   if(mBitmap)
      delete mBitmap;
   mBitmap = NULL;
}

void EqualizationPanel::OnSize(wxSizeEvent & evt)
{
   Refresh( false );
}

void EqualizationPanel::OnPaint(wxPaintEvent & evt)
{
   wxPaintDC dc(this);

   int width, height;
   GetSize(&width, &height);

   if (!mBitmap || mWidth!=width || mHeight!=height) {
      if (mBitmap)
         delete mBitmap;

      mWidth = width;
      mHeight = height;
      mBitmap = new wxBitmap(mWidth, mHeight);
   }

   // Ruler
   int w = 0;
   int h = 0;

   Ruler dbRuler;
   dbRuler.SetBounds(0, 0, mWidth, mHeight);
   dbRuler.SetOrientation(wxVERTICAL);
   dbRuler.SetRange(*mdBMax, *mdBMin);
   dbRuler.SetFormat(Ruler::LinearDBFormat);
   dbRuler.SetUnits(_("dB"));
   dbRuler.GetMaxSize(&w, NULL);

   Ruler freqRuler;
   freqRuler.SetBounds(0, 0, mWidth, mHeight);
   freqRuler.SetOrientation(wxHORIZONTAL);
   freqRuler.SetLog(true);
   freqRuler.SetRange(mLoFreq, mHiFreq);
   freqRuler.SetFormat(Ruler::IntFormat);
   freqRuler.SetUnits(_("Hz"));
   freqRuler.SetFlip(true);
   freqRuler.GetMaxSize(NULL, &h);

   dbRuler.SetBounds(0, 0, w, mHeight - h);
   freqRuler.SetBounds(w+2, mHeight - h, mWidth-4, h);

   wxColour bkgnd = GetBackgroundColour();
   wxBrush bkgndBrush(bkgnd, wxSOLID);

   wxMemoryDC memDC;
   memDC.SelectObject(*mBitmap);

   wxRect bkgndRect;
   bkgndRect.x = 0;
   bkgndRect.y = 0;
   bkgndRect.width = w;
   bkgndRect.height = mHeight;
   memDC.SetBrush(bkgndBrush);
   memDC.SetPen(*wxTRANSPARENT_PEN);
   memDC.DrawRectangle(bkgndRect);

   bkgndRect.y = mHeight - h;
   bkgndRect.width = mWidth;
   bkgndRect.height = h;
   memDC.DrawRectangle(bkgndRect);

   wxRect border;
   border.x = w;
   border.y = 0;
   border.width = mWidth - w;
   border.height = mHeight - h + 1;

   memDC.SetBrush(*wxWHITE_BRUSH);
   memDC.SetPen(*wxBLACK_PEN);
   memDC.DrawRectangle(border);

   mEnvRect = border;
   mEnvRect.Deflate( 2, 2 );

   // Pure blue x-axis line
   memDC.SetPen(wxPen(wxColour(0, 0, 255), 1, wxSOLID));
   int center = mEnvRect.height * *mdBMax/(*mdBMax-*mdBMin);
   memDC.DrawLine(mEnvRect.x, mEnvRect.y + center,
                  mEnvRect.x + mEnvRect.width, mEnvRect.y + center);

   // Med-blue envelope line
   memDC.SetPen(wxPen(wxColour(110, 110, 220), 3, wxSOLID));

   // Draw envelope
   double *values = new double[mEnvRect.width];
   mEnvelope->GetValues(values, mEnvRect.width, 0.0, 1.0/mEnvRect.width);
   int x, y, xlast = 0, ylast = 0;
   bool off = false, off1 = false;
   for(int i=0; i<mEnvRect.width; i++) {
      x = mEnvRect.x + i;
      y = (int)(mEnvRect.height*((*mdBMax-values[i])/(*mdBMax-*mdBMin)));
      if( y > mEnvRect.height) {
         y = mEnvRect.height;
         off = true;
      }
      else {
         off = false;
         off1 = false;
      }
      if ( (i != 0) & (!off1) ) {
         memDC.DrawLine(xlast, ylast,
                        x, mEnvRect.y + y);
      }
      off1 = off;
      xlast = x;
      ylast = mEnvRect.y + y;
   }
   delete[] values;

   //Now draw the actual response that you will get -MJS
   //mFilterFunc has a linear scale, window has a log one so we have to fiddle about
   mParent->TransferDataFromWindow();	//to calculate the actual response
   memDC.SetPen(wxPen(wxColour(0, 255, 0), 1, wxSOLID));
   double scale = (double)mEnvRect.height/(*mdBMax-*mdBMin);	//pixels per dB
   double yF;	//gain at this freq
   double delta = mHiFreq/(((double)mWindowSize/2.));	//size of each freq bin
   double loLog = log10(mLoFreq);
   double stepLog = (log10(mHiFreq) - loLog)/((double)mEnvRect.width-1.);
   double freq;	//actual freq corresponding to x position
   int n;	//index to mFreqFunc
   float *outr = new float[mWindowSize];
   float *outi = new float[mWindowSize];
   FFT(mWindowSize,true,mFilterFuncR,mFilterFuncI,outr,outi);	//work out FIR response
   for(int i=0; i<mEnvRect.width; i++) {
      x = mEnvRect.x + i;
      if( (pow(10., loLog + (i+1)*stepLog)-pow(10., loLog + i*stepLog)) < delta) { //not enough resolution in FFT
         freq = M_PI*pow(10., loLog + i*stepLog)/mHiFreq;	//radians, normalized
         yF = 0.;
         for(int j=0;j<(*mM-1)/2;j++) {
            yF += 2. * outr[j] * cos(freq*((*mM-1)/2-j));
         }
         yF += outr[(*mM-1)/2];
         yF = fabs(yF);
         if(yF!=0.)
            yF = 20.0*log10(yF);	//20 here as an amplitude
         else
            yF = *mdBMin;
      }
      else {	//use FFT, it has enough resolution
         freq = pow(10., loLog + i*stepLog);	//Hz
         n = (int)freq/delta;
         if(pow(mFilterFuncR[n],2)+pow(mFilterFuncI[n],2)!=0.)
            yF = 10.0*log10(pow(mFilterFuncR[n],2)+pow(mFilterFuncI[n],2));	//10 here as a power
         else
            yF = *mdBMin;
      }
      if(yF < *mdBMin)
         yF = *mdBMin;
      yF = center-scale*yF;
      if(yF>mEnvRect.height)
         yF = mEnvRect.height;
      if(yF<0.)
         yF=0.;
      y = (int)(yF+0.5);

      if (i != 0) {
         memDC.DrawLine(xlast, ylast,
                        x, mEnvRect.y + y);
      }
      xlast = x;
      ylast = mEnvRect.y + y;
   }
   delete[] outr;
   delete[] outi;

   memDC.SetPen(*wxBLACK_PEN);
//   mEnvRect.y -= 5;
   mEnvelope->Draw(memDC, mEnvRect, 0.0, mEnvRect.width, false, *mdBMin, *mdBMax);
//   mEnvRect.y += 5;

   // Paint border again
//   memDC.SetBrush(*wxTRANSPARENT_BRUSH);
//   memDC.SetPen(*wxBLACK_PEN);
//MJS   memDC.DrawRectangle(border);

   dbRuler.Draw(memDC);
   freqRuler.Draw(memDC);

   dc.Blit(0, 0, mWidth, mHeight,
           &memDC, 0, 0, wxCOPY, FALSE);
}

void EqualizationPanel::OnMouseEvent(wxMouseEvent & event)
{
   if (event.ButtonDown() && !HasCapture()) {
      CaptureMouse();
   }

   if (mEnvelope->MouseEvent(event, mEnvRect, 0.0, mEnvRect.width, false,
                             *mdBMin, *mdBMax, *mdBMin, *mdBMax))
   {
      mParent->EnvelopeUpdated();
      Refresh(false);
   }

   if (event.ButtonUp() && HasCapture()) {
      ReleaseMouse();
   }
}

// WDR: class implementations

//----------------------------------------------------------------------------
// EqualizationDialog
//----------------------------------------------------------------------------

// WDR: event table for EqualizationDialog

BEGIN_EVENT_TABLE(EqualizationDialog,wxDialog)
   EVT_SIZE( EqualizationDialog::OnSize )

   EVT_SLIDER( ID_LENGTH, EqualizationDialog::OnSliderM )
   EVT_SLIDER( ID_DBMAX, EqualizationDialog::OnSliderDBMAX )
   EVT_SLIDER( ID_DBMIN, EqualizationDialog::OnSliderDBMIN )
   EVT_COMMAND_RANGE( ID_SLIDER,
                      ID_SLIDER + NUMBER_OF_BANDS - 1,
                      wxEVT_COMMAND_SLIDER_UPDATED,
                      EqualizationDialog::OnSlider)

   EVT_CHOICE( ID_CURVE, EqualizationDialog::OnCurve )
   EVT_BUTTON( ID_SAVEAS, EqualizationDialog::OnSaveAs )
   EVT_BUTTON( ID_DELETE, EqualizationDialog::OnDelete )
   EVT_BUTTON( ID_CLEAR, EqualizationDialog::OnClear )

   EVT_BUTTON( ID_PREVIEW, EqualizationDialog::OnPreview )
   EVT_BUTTON( wxID_OK, EqualizationDialog::OnOk )
   EVT_BUTTON( wxID_CANCEL, EqualizationDialog::OnCancel )
END_EVENT_TABLE()

EqualizationDialog::EqualizationDialog(EffectEqualization * effect,
					double loFreq, double hiFreq,
					float *filterFuncR,
					float *filterFuncI,
					long windowSize,
					int *M,
					wxWindow *parent, wxWindowID id,
					const wxString &title,
					const wxPoint &position,
					const wxSize& size,
					long style):
   wxDialog( parent, id, title, position, size, style | wxRESIZE_BORDER )
{
   m_pEffect = effect;

   mEnvelope = new Envelope();
   mEnvelope->SetInterpolateDB(false);
   mEnvelope->Mirror(false);

   mLoFreq = loFreq;
   mHiFreq = hiFreq;

   mFilterFuncR = filterFuncR;
   mFilterFuncI = filterFuncI;
   mWindowSize = windowSize;
   mM = M;	//MJS

   mCurve = NULL;
   mDirty = false;

   // Load the EQ curves
   LoadCurves();

   // Create the dialog
   MakeEqualizationDialog();

   // Set initial curve
   setCurve( mEnvelope, 0 );
}

//
// Load external curves with fallback to internal
//
void EqualizationDialog::LoadCurves()
{
   // Construct default curve filename
   //
   // LLL:  Wouldn't you know that as of WX 2.6.2, there is a conflict
   //       between wxStandardPaths and wxConfig under Linux.  The latter
   //       creates a normal file as "$HOME/.audacity", while the former
   //       expects the ".audacity" portion to be a directory.
#if !defined( __WXMSW__ )
   wxFileName fn( wxStandardPaths::Get().GetUserDataDir() + "-data", "EQCurves.xml" );
#else
   wxFileName fn( wxStandardPaths::Get().GetUserDataDir(), wxT("EQCurves.xml") );
#endif

   // If it doesn't exist...
   if( !fn.FileExists() )
   {
      // Fallback to internal
      LoadDefaultCurves();
      return;
   }

   // Start from scratch
   mCurves.Clear();

   // Load the curves
   XMLFileReader reader;
   if( !reader.Parse( this, fn.GetFullPath() ) )
   {
      // Inform user of load failure
      wxMessageBox( reader.GetErrorStr(),
                    _("Error loading EQ curve"),
                    wxOK | wxCENTRE,
                    this );

      // Fallback to internal
      LoadDefaultCurves();
   }

   return;
}

//
// Load the internal curves
//
void EqualizationDialog::LoadDefaultCurves()
{
   int curve;
   int point;

   // Start from scratch
   mCurves.Clear();

   // For all internally defined curves...
   for( curve = 0; curve < EffectEqualization::nCurveTypes; curve++ )
   {
      // Create and name a new curve class
      mCurves.Add( wxString( EffectEqualization::curveNames[ curve ] ) );

      // For all points within this curve...
      for( point = 0; point < EffectEqualization::nCurvePoints; point++ )
      {
         // Extract the frequency and dB values
         double freq = EffectEqualization::curvex[ point ];
         double db   = EffectEqualization::curvey[ curve ][ point ];

         // Add it to the list of points within the curve
         mCurves[ curve ].points.Add( EQPoint( freq, db ) );
      }
   }

   mDirty = true;

   return;
}

//
// Save curves to external file
//
void EqualizationDialog::SaveCurves()
{
   // Construct default curve filename
   //
   // LLL:  Wouldn't you know that as of WX 2.6.2, there is a conflict
   //       between wxStandardPaths and wxConfig under Linux.  The latter
   //       creates a normal file as "$HOME/.audacity", while the former
   //       expects the ".audacity" portion to be a directory.
#if !defined( __WXMSW__ )
   wxFileName fn( wxStandardPaths::Get().GetUserDataDir() + "-data", "EQCurves.xml" );
#else
   wxFileName fn( wxStandardPaths::Get().GetUserDataDir(), wxT("EQCurves.xml") );
#endif

   // If the directory doesn't exist...
   if( !fn.DirExists() )
   {
      // Attempt to create it
      if( !fn.Mkdir( fn.GetPath(), 511, wxPATH_MKDIR_FULL ) )
      {
         // MkDir() will emit message
         return;
      }
   }

   // Create/Open the file
   wxFFile eqFile( fn.GetFullPath().c_str(), wxT("wb") );

   // Complain if open failed
   if( !eqFile.IsOpened() )
   {
      // Constructor will emit message
      return;
   }

   // Write the curves
   WriteXML( 0, eqFile.fp() );

   // Close the file
   eqFile.Close();

   return;
}

//
// Create the Equalization dialog
//
void EqualizationDialog::MakeEqualizationDialog()
{
   *mM = 451;   //MJS - odd int only
   mdBMax = 30.;
   mdBMin = -30.;
   wxBoxSizer *szrV;
   wxBoxSizer *szrH;
   wxFlexGridSizer *szrG;
   wxBoxSizer *szr1;
   wxBoxSizer *szr2;
   wxStaticText *txt;
   wxButton *btn;

   // Create the base sizer
   szrV = new wxBoxSizer( wxVERTICAL );

   // -------------------------------------------------------------------
   // ROW 1: Banner
   // -------------------------------------------------------------------
   txt = new wxStaticText(this, wxID_ANY,
                          _("Equalization, by Martyn Shaw && Mitch Golden"));
   szrV->Add( txt, 0, wxALIGN_CENTRE|wxALL, 4 );

   // -------------------------------------------------------------------
   // ROW 2: EQ panel and sliders for vertical scale
   // -------------------------------------------------------------------
   szr1 = new wxBoxSizer( wxHORIZONTAL );
   szr2 = new wxBoxSizer( wxVERTICAL );
   mdBMaxSlider = new wxSlider(this, ID_DBMAX, 30, 0, 60,
                           wxDefaultPosition, wxSize(-1, -1), wxSL_VERTICAL|wxSL_INVERSE);
   szr2->Add( mdBMaxSlider, 1, wxGROW|wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, 4 );
   mdBMinSlider = new wxSlider(this, ID_DBMIN, -30, -120, -10,
                           wxDefaultPosition, wxSize(-1, -1), wxSL_VERTICAL|wxSL_INVERSE);
   szr2->Add( mdBMinSlider, 1, wxGROW|wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, 4 );
   szr1->Add( szr2, 0, wxGROW|wxALIGN_CENTRE|wxALL, 4 );

   mPanel = new EqualizationPanel( mLoFreq, mHiFreq,
                                   mEnvelope,
                                   this,
                                   mFilterFuncR, mFilterFuncI, mWindowSize, mM, &mdBMin, &mdBMax,
                                   ID_FILTERPANEL,
                                   wxDefaultPosition, wxSize(100,80) );
   szr1->Add( mPanel, 1, wxEXPAND|wxALIGN_CENTRE|wxALL, 4 );
   szrV->Add( szr1, 1, wxEXPAND|wxALIGN_CENTER | wxALL, 0 );

   // -------------------------------------------------------------------
   // ROW 3: Filter length grouping
   // -------------------------------------------------------------------
   szrH = new wxBoxSizer( wxHORIZONTAL );

   // length of filter (M) label
   txt = new wxStaticText(this, wxID_ANY, _("Length of filter:"));
   szrH->Add( txt, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, 4 );

   // length of filter (M) slider
   mMSlider = new wxSlider(this, ID_LENGTH, (*mM -1)/2, 10, 4095,
                           wxDefaultPosition, wxSize(200, -1), wxSL_HORIZONTAL);
   szrH->Add( mMSlider, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, 4 );

   wxString label;
   label.Printf( wxT("%d"), *mM );
   mMText = new wxStaticText(this, wxID_ANY, label);
   szrH->Add( mMText, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, 4 );

   // Add the length grouping
   szrV->Add( szrH, 0, wxALIGN_CENTER | wxALL, 0 );

   // -------------------------------------------------------------------
   // ROW 4: Curve management grouping
   // -------------------------------------------------------------------
   szrH = new wxBoxSizer( wxHORIZONTAL );

   txt = new wxStaticText( this, wxID_ANY, _("Select curve:") );
   szrH->Add( txt, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxLEFT, 4 );

   // Create the choice sizer (helps in recreating choice control)
   mCurveSizer = new wxBoxSizer( wxHORIZONTAL );
   szrH->Add( mCurveSizer, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxLEFT, 4 );

   // Create the choice control
   CreateChoice();

   mSaveAs = new wxButton( this, ID_SAVEAS, _("Save As...") );
   szrH->Add( mSaveAs, 0, wxALIGN_CENTRE|wxLEFT, 4 );

   mDelete = new wxButton( this, ID_DELETE, _("Delete") );
   szrH->Add( mDelete, 0, wxALIGN_CENTRE|wxLEFT, 4 );

   // start over

   //szrH->AddStretchSpacer( true ); // horizontal spacer

   btn = new wxButton( this, ID_CLEAR, _("Clear"));
   szrH->Add( btn, 0, wxALIGN_CENTRE | wxALL, 4 );

   szrV->Add( szrH, 0, wxALIGN_CENTER | wxALL, 0 );

   // -------------------------------------------------------------------
   // ROW 5: Preview, OK, & Cancel buttons
   // -------------------------------------------------------------------
   szrH = new wxBoxSizer(wxHORIZONTAL);

   btn = new wxButton(this, ID_PREVIEW, m_pEffect->GetPreviewName());
   szrH->Add( btn, 0, wxALIGN_LEFT | wxALL, 4 );

   szrH->Add(80, 4); // horizontal spacer

   btn = new wxButton(this, wxID_CANCEL, _("Cancel"));
   szrH->Add( btn, 0, wxALIGN_RIGHT | wxALL, 4 );

   btn = new wxButton(this, wxID_OK, _("OK"));
   btn->SetDefault();
   btn->SetFocus();
   szrH->Add( btn, 0, wxALIGN_RIGHT | wxALL, 4 );

   szrV->Add( szrH, 0, wxALIGN_CENTER | wxALL, 4 );

   // -------------------------------------------------------------------
   // ROW 6: Graphic EQ
   // -------------------------------------------------------------------
   szrG = new wxFlexGridSizer( NUMBER_OF_BANDS, 0, 0 );
   szrG->SetFlexibleDirection( wxVERTICAL );
   for (int i = 0; i < NUMBER_OF_BANDS; ++i) {
      wxString label;
      if( thirdOct[i] < 1000.)
         label.Printf( wxT("%d"), (int)thirdOct[i] );
      else
         if( thirdOct[i]/1000 == (int)(thirdOct[i]/1000.) )
            label.Printf( wxT("%.0fk"), thirdOct[i]/1000. );
         else
            label.Printf( wxT("%dk%.0f"), (int)(thirdOct[i]/1000.),
			              (thirdOct[i]/1000.-(int)(thirdOct[i]/1000.))*10.);
      octText = new wxStaticText(this, wxID_ANY, label);
      szrG->Add( octText, 0, wxALIGN_CENTER );
   }

   for (int i = 0; i < NUMBER_OF_BANDS; ++i) {
      m_sliders[i] = new wxSlider(this, ID_SLIDER + i, 0, -20, +20,
			               wxDefaultPosition, wxSize(20, 124), wxSL_VERTICAL|wxSL_INVERSE);
      szrG->Add( m_sliders[i], 0, wxALIGN_CENTER | wxLEFT | wxRIGHT, 2);
   }

   szrV->Add( szrG, 0, wxALIGN_CENTER | wxALL, 4 );

   SetSizerAndFit( szrV );
   Layout();

   return;
}

//
// (Re)Create the choice control
//
void EqualizationDialog::CreateChoice()
{
   wxChoice *choice;
   int i;
   int numCurves = mCurves.GetCount();
   wxString *curveNames = new wxString[ numCurves ];

   // Create an array of names
   for( i = 0; i < numCurves; i++ )
   {
     curveNames[ i ] = mCurves[ i ].Name;
   }

   // Create the control
   choice = new wxChoice( this, ID_CURVE,
                          wxDefaultPosition, wxDefaultSize,
                          numCurves, curveNames );

   // Have an existing control?
   if( mCurve )
   {
      // Then detach it from its sizer and delete it
      mCurveSizer->Detach( mCurve );
      delete mCurve;
   }

   // Save control ptr and add to its sizer
   mCurve = choice;
   mCurveSizer->Add( mCurve, 0 );

   // Delete the array of names
   delete [] curveNames;
}

//
// Validate data (A do nothing routine)
//
bool EqualizationDialog::Validate()
{
   return TRUE;
}

//
// Populate the window (A do nothing routine)
//
bool EqualizationDialog::TransferDataToWindow()
{
   return TRUE;
}

//
// Retrieve data from the window
//
bool EqualizationDialog::TransferDataFromWindow()
{
   double loLog = log10(mLoFreq);
   double hiLog = log10(mHiFreq);
   double denom = hiLog - loLog;

   double delta = mHiFreq / ((double)(mWindowSize/2.));
   double val0 = mEnvelope->GetValue(0.0);	//no scaling required - saved in mEnvelope as dB
   double val1 = mEnvelope->GetValue(1.0);

   mFilterFuncR[0] = val0;
   double freq = delta;

   int i;
   for(i=1; i<=mWindowSize/2; i++) {
      double when = (log10(freq) - loLog)/denom;
      if(when < 0.) {
         mFilterFuncR[i] = val0;
      }
      else if(when > 1.0) {
         mFilterFuncR[i] = val1;
      }
      else {
        mFilterFuncR[i] = mEnvelope->GetValue(when);
      }
      freq += delta;
   }
   mFilterFuncR[mWindowSize/2] = val1;

   mFilterFuncR[0] = (float)(pow(10., mFilterFuncR[0]/20.));
   for(i=1;i<mWindowSize/2;i++) {
      mFilterFuncR[i] = (float)(pow(10., mFilterFuncR[i]/20.));
      mFilterFuncR[mWindowSize-i]=mFilterFuncR[i];	//Fill entire array
   }
   mFilterFuncR[i] = (float)(pow(10., mFilterFuncR[i]/20.));	//do last one

   //transfer to time domain to do the padding and windowing
   float *outr = new float[mWindowSize];
   float *outi = new float[mWindowSize];
   FFT(mWindowSize,true,mFilterFuncR,NULL,outr,outi);	//To time domain

   int M = *mM;

   for(i=0;i<=(M-1)/2;i++) {	//Windowing - could give a choice, fixed for now - MJS
//	   double mult=0.54-0.46*cos(2*M_PI*(i+(M-1)/2.0)/(M-1));	//Hamming
      double mult=0.42-0.5*cos(2*M_PI*(i+(M-1)/2.0)/(M-1))+.08*cos(4*M_PI*(i+(M-1)/2.0)/(M-1));	//Blackman
      outr[i]*=mult;
      if(i!=0){
         outr[mWindowSize-i]*=mult;
      }
   }
   for(;i<=mWindowSize/2;i++) {	//Padding
      outr[i]=0;
      outr[mWindowSize-i]=0;
   }
   float *tempr = new float[M];
   for(i=0;i<(M-1)/2;i++) {	//shift so that padding on right
      tempr[(M-1)/2+i]=outr[i];
      tempr[i]=outr[mWindowSize-(M-1)/2+i];
   }
   tempr[(M-1)/2+i]=outr[i];

   for(i=0;i<M;i++) {	//and copy useful values back
      outr[i]=tempr[i];
   }
   for(i=M;i<mWindowSize;i++) {	//rest is padding
      outr[i]=0.;
   }

   FFT(mWindowSize,false,outr,NULL,mFilterFuncR,mFilterFuncI);	//Back to the frequency domain so we can use it

   delete[] outr;
   delete[] outi;
   delete[] tempr;

   return TRUE;
}

//
// Make the passed curve index the active one
//
void EqualizationDialog::setCurve(Envelope *env, int currentCurve)
{
   // Set current choice
   Select( currentCurve );

   env->Flatten(0.5);
   env->SetTrackLen(1.0);

   wxASSERT( currentCurve < (int) mCurves.GetCount() );

   if( mCurves[currentCurve].points.GetCount() )
   {
      double when = 0.;
      double value = mCurves[currentCurve].points[0].dB;
      env->Move(when, value);
      double loLog = log10(20.);
      double hiLog = log10(mHiFreq);
      double denom = hiLog - loLog;
      int i;
      int nCurvePoints = mCurves[currentCurve].points.GetCount();
      for(i=0;i<nCurvePoints;i++) {
         when = (log10(mCurves[currentCurve].points[i].Freq) - loLog)/denom;
         value = mCurves[currentCurve].points[i].dB;
         if(when < 1)
            env->Insert(when, value);
         else
            break;
      }
      when = 1.;
      value = mCurves[currentCurve].points[nCurvePoints-1].dB;
      env->Move(when, value);
   }

   mPanel->Refresh( false );
}

//
// Set new curve selection and manage state of delete button
//
void EqualizationDialog::Select( int curve )
{
   // Set current choice
   mCurve->SetSelection( curve );

   // If the "custom" curve became active
   if( curve == mCurve->GetCount() - 1 )
   {
      // Prevent focus from being lost
      if( mDelete->FindFocus() == mDelete )
      {
         mCurve->SetFocus();
      }

      // Not allowed to delete the "custom" curve
      mDelete->Disable();
   }
   else
   {
      // Allow the curve to be deleted
      mDelete->Enable();
   }
}

//
// Capture updated envelope
//
void EqualizationDialog::EnvelopeUpdated()
{
   // Allocate and populate point arrays
   int numPoints = mEnvelope->GetNumberOfPoints();
   double *when = new double[ numPoints ];
   double *value = new double[ numPoints ];
   mEnvelope->GetPoints( when, value, numPoints );

   // Clear the custom curve
   int curve = mCurve->GetCurrentSelection();
   mCurves[ curve ].points.Clear();

   // Yea sure...like I know what these do.  :-)
   double loLog = log10( 20. );
   double hiLog = log10( mHiFreq );
   double denom = hiLog - loLog;

   // Copy and convert points
   int point;
   for( point = 0; point < numPoints; point++ )
   {
      // This is where we need to convert from time/value to freq/dB
      //
      // LLL: Someone better check to see if this is right 'cause
      //      I have no idea why it works, but it seems to give
      //      the desired results....time -> frequency domain.
      double freq = pow( 10., ( ( when[ point ] * denom ) + loLog ));
      double db = value[ point ];

      // Add it to the curve
      mCurves[ curve ].points.Add( EQPoint( freq, db ) );
   }

   // Remember that we've updated the custom curve
   mDirty = true;

   // Clean up
   delete [] when;
   delete [] value;
}

//
// Process XML tags and handle the ones we recognize
//
bool EqualizationDialog::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   // May want to add a version strings...
   if( !wxStrcmp( tag, wxT("equalizationeffect") ) )
   {
      return true;
   }

   // Located a new curve
   if( !wxStrcmp(tag, wxT("curve") ) )
   {
      // Process the attributes
      while( *attrs )
      {
         // Cache attr/value and bump to next
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         // Create a new curve and name it
         if( !wxStrcmp( attr, wxT("name") ) )
         {
            mCurves.Add( EQCurve( value ) );
         }
      }

      // Tell caller it was processed
      return true;
   }

   // Located a new point
   if( !wxStrcmp( tag, wxT("point") ) )
   {
      // Set defaults in case attributes are missing
      double f = 0.0;
      double d = 0.0;

      // Process the attributes
      while( *attrs )
      {
         // Cache attr/value and bump to next
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         // Get the frequency
         if( !wxStrcmp( attr, wxT("f") ) )
         {
            f = Internat::CompatibleToDouble( value );
         }
         // Get the dB
         else if( !wxStrcmp( attr, wxT("d") ) )
         {
            d = Internat::CompatibleToDouble( value );
         }
      }

      // Create a new point
      mCurves[ mCurves.GetCount() - 1 ].points.Add( EQPoint( f, d ) );

      // Tell caller it was processed
      return true;
   }

   // Tell caller we didn't understand the tag
   return false;
}

//
// Return handler for recognized tags
//
XMLTagHandler *EqualizationDialog::HandleXMLChild(const wxChar *tag)
{
   if( !wxStrcmp( tag, wxT("equalizationeffect") ) )
   {
      return this;
   }

   if( !wxStrcmp( tag, wxT("curve") ) )
   {
      return this;
   }

   if( !wxStrcmp( tag, wxT("point") ) )
   {
      return this;
   }

   return NULL;
}

//
// Write all of the curves to the XML file
//
void EqualizationDialog::WriteXML(int depth, FILE *fp)
{
   // Start our heirarchy
   fprintf( fp,
            "<equalizationeffect>\n" );

   // Write all curves
   int numCurves = mCurves.GetCount();
   int curve;
   for( curve = 0; curve < numCurves; curve++ )
   {
      // Start a new curve
      fprintf( fp,
               "\t<curve name='%s'>\n",
               mCurves[ curve ].Name.mb_str() );

      // Write all points
      int numPoints = mCurves[ curve ].points.GetCount();
      int point;
      for( point = 0; point < numPoints; point++ )
      {
         // Convert to external format
         wxString freq = Internat::ToString( mCurves[ curve ].points[ point ].Freq, 12 );
         wxString db = Internat::ToString( mCurves[ curve ].points[ point ].dB, 12 );

         // Write new point
         fprintf( fp,
                  "\t\t<point f='%s' d='%s'/>\n",
                  freq.mb_str(),
                  db.mb_str() );
      }

      // Terminate curve
      fprintf( fp,
               "\t</curve>\n" );
   }

   // Terminate our heirarchy
   fprintf( fp,
            "</equalizationeffect>\n" );
}

// WDR: handler implementations for EqualizationDialog

//
// Length of filter was adjusted
//

void EqualizationDialog::OnSlider(wxCommandEvent &event)
{
   int slider = event.GetId() - ID_SLIDER;
//   wxLogDebug(wxT("EqualizerSlider %d changed to %d"), slider, m_sliders[slider]->GetValue());
   graphicEQ(mEnvelope);
}

void EqualizationDialog::graphicEQ(Envelope *env)
{
   double loLog = log10(mLoFreq);
   double hiLog = log10(mHiFreq);
   double denom = hiLog - loLog;
   int width = 181;	//why this number? - MJS

   double stepLog = (log10(mHiFreq) - loLog)/((double)width-1.);
   double freq;	//actual freq corresponding to x position
   double value;
   int minF,maxF;
   env->Flatten(0.5);
   env->SetTrackLen(1.0);

   for(int i=0; i<width; i++) {
      freq = pow(10., loLog + i*stepLog);	//actual freq for this pixel
      if(freq > thirdOct[30])
         break;
      double when = (log10(freq) - loLog)/denom;	//scaled to env
         for(int j=0;j<31;j++) {	//search for lower slider
            if(thirdOct[j] > freq ) {
               minF = j-1;
               break;
         }
      }
      for(int j=30;j>=0;j--) {	//search for upper slider
         if(thirdOct[j] < freq ) {
            maxF = j+1;
            break;
         }
      }
      double span = (log10(thirdOct[maxF]) - log10(thirdOct[minF]))/denom;
      double dist = (log10(thirdOct[maxF]) - log10(freq))/denom;
      value = m_sliders[minF]->GetValue()*(1. + cos(M_PI*(span-dist)/span))/2. +
              m_sliders[maxF]->GetValue()*(1. + cos(M_PI*dist/span))/2.;
      if(i==0)
         env->Move( 0., value );
      env->Insert( when, value );
   }
   env->Move( 1., value );
   mPanel->Refresh( false );
}

void EqualizationDialog::OnSliderM(wxCommandEvent &event)
{
   int M;

   // Retrieve and constrain new value
   M = mMSlider->GetValue();	// read value from slider
   M = 2*M + 1;	// odd numbers only
   *mM = M;	// remember for actual filtering

   // Update the label
   wxString label;
   label.Printf( wxT("%d"), M );
   mMText->SetLabel( label );

   // Refresh panel to force redrawing based on new length
   mPanel->Refresh( false );
}

void EqualizationDialog::OnSliderDBMAX(wxCommandEvent &event)
{
   // Retrieve new value
   mdBMax = mdBMaxSlider->GetValue();	// read value from slider

   // Refresh panel to force redrawing based on new length
   mPanel->Refresh( false );
}

void EqualizationDialog::OnSliderDBMIN(wxCommandEvent &event)
{
   // Retrieve new value
   mdBMin = mdBMinSlider->GetValue();	// read value from slider

   // Refresh panel to force redrawing based on new length
   mPanel->Refresh( false );
}

//
// New curve was selected
//
void EqualizationDialog::OnCurve(wxCommandEvent &event)
{
   // Select new curve
   setCurve( mEnvelope, mCurve->GetCurrentSelection() );
}

//
// User wants to save a new curve
//
void EqualizationDialog::OnSaveAs(wxCommandEvent &event)
{
   wxString name;
   int numCurves = mCurves.GetCount();
   int curve;

   // build the dialog
   wxTextEntryDialog dlg( this,
                          _("Enter the desired name of the curve"),
                          _("Save As...") );

   // Setup list of characters that aren't allowed
   wxArrayString exclude;
   exclude.Add( wxT("<") );
   exclude.Add( wxT(">") );
   exclude.Add( wxT("'") );
   exclude.Add( wxT("\"") );

   // And tell the validator about them
   dlg.SetTextValidator( wxFILTER_EXCLUDE_CHAR_LIST );
   wxTextValidator *tv = dlg.GetTextValidator();
   tv->SetExcludes( exclude );

   // Prompt the user until a valid name is enter or cancelled
   while( true )
   {
      // Show the dialog and bail if the user cancels
      if( dlg.ShowModal() == wxID_CANCEL )
      {
         return;
      }

      // Extract the name from the dialog
      name = dlg.GetValue();

      // Search list of curves for a duplicate name
      bool bad = false;
      for( curve = 0; curve < numCurves; curve++ )
      {
         if( name == mCurves[ curve ].Name )
         {
            bad = true;
            break;
         }
      }

      // A duplicate wasn't found so break out of loop
      if( !bad )
      {
         break;
      }
   }

   // Create a new entry
   mCurves.Add( EQCurve( wxT("custom") ) );

   // Copy over the points
   mCurves[ numCurves ].points = mCurves[ numCurves - 1 ].points;

   // Give the original custom entry the new name
   mCurves[ numCurves - 1 ].Name = name;

   // Recreate the choice control and layout everything again
   CreateChoice();
   Layout();

   // Select the newly name curve
   Select( numCurves - 1 );

   // Remember that we've updated the custom curve
   mDirty = true;
}

//
// User wants to delete a curve
//
void EqualizationDialog::OnDelete(wxCommandEvent &event)
{
   // Get the one to be deleted
   int sel = mCurve->GetSelection();

   // Create the prompt
   wxString quest;
   quest = _("Delete ") + mCurves[ sel ].Name + _("?");

   // Ask for confirmation
   int ans = wxMessageBox( quest,
                           _("Confirm Deletion"),
                           wxYES_NO | wxCENTRE,
                           this );

   // Denied, so bail
   if( ans == wxNO )
   {
      return;
   }

   // Remove the curve from the array and choice control
   mCurves.RemoveAt( sel );
   mCurve->Delete( sel );

   // Select next (or last) curve
   if( sel >= mCurve->GetCount() )
   {
      // Don't have to worry about going negative since custom
      // will always be in the list
      sel--;
   }

   // Reselect
   setCurve( mEnvelope, sel );

   // Remember that we've updated the custom curve
   mDirty = true;
}

void EqualizationDialog::OnClear(wxCommandEvent &event)
{
   mEnvelope->Flatten(0.);
   mEnvelope->SetTrackLen(1.0);
   EnvelopeUpdated();
   mPanel->Refresh(false);
}

void EqualizationDialog::OnSize(wxSizeEvent &event)
{
   event.Skip();
}

void EqualizationDialog::OnPreview(wxCommandEvent &event)
{
   TransferDataFromWindow();
   m_pEffect->Preview();
   //v Restore previous values?
}

void EqualizationDialog::OnCancel(wxCommandEvent &event)
{
   if(mEnvelope)
     delete mEnvelope;
   mEnvelope = NULL;
   mPanel = NULL;

   EndModal(false);
}

void EqualizationDialog::OnOk(wxCommandEvent &event)
{
   TransferDataFromWindow();

   if( Validate() )
   {
      // Only save curves if they been modified
      if( mDirty )
      {
         SaveCurves();
      }

      if(mEnvelope)
         delete mEnvelope;
      mEnvelope = NULL;
      mPanel = NULL;

      EndModal(true);
   }
   else
   {
      event.Skip();
   }
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
// arch-tag: 65b35bfa-632c-46fe-9170-840a158b3c97

