/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor.cpp

  Dominic Mazzoni
  Martyn Shaw
  Steve Jolly

*******************************************************************//**

\class EffectCompressor
\brief An Effect derived from EffectTwoPassSimpleMono

 - Martyn Shaw made it inherit from EffectTwoPassSimpleMono 10/2005.
 - Steve Jolly made it inherit from EffectSimpleMono.
 - GUI added and implementation improved by Dominic Mazzoni, 5/11/2003.

*//****************************************************************//**

\class CompressorDialog
\brief Dialog used with EffectCompressor.

*//****************************************************************//**

\class CompressorPanel
\brief Panel used within the CompressorDialog for EffectCompressor.

*//*******************************************************************/


#include "../Audacity.h" // for rint from configwin.h

#include <math.h>

#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/brush.h>
#include <wx/image.h>
#include <wx/dcmemory.h>

#include "Compressor.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/Ruler.h"
#include "../AColor.h"
#include "../Shuttle.h"

EffectCompressor::EffectCompressor()
{
   mNormalize = true;
   mFloor = 0.001;
   mAttackTime = 0.2;      // seconds
   mDecayTime = 1.0;       // seconds
   mRatio = 2.0;           // positive number > 1.0
   mThresholdDB = -12.0;
   mCircle = NULL;
	mLevelCircle = NULL;
}

bool EffectCompressor::TransferParameters( Shuttle & shuttle )
{
   shuttle.TransferDouble( wxT("Threshold"), mThresholdDB, -12.0f );
   shuttle.TransferDouble( wxT("Ratio"), mRatio, 2.0f );
   shuttle.TransferDouble( wxT("AttackTime"), mAttackTime, 0.2f );
   shuttle.TransferDouble( wxT("DecayTime"), mDecayTime, 0.2f );
   shuttle.TransferBool( wxT("Normalize"), mNormalize, true );
   return true;
}

bool EffectCompressor::PromptUser()
{
   CompressorDialog dlog(this, mParent);
   dlog.threshold = mThresholdDB;
   dlog.ratio = mRatio;
   dlog.attack = mAttackTime;
   dlog.decay = mDecayTime;
   dlog.useGain = mNormalize;
   dlog.TransferDataToWindow();

   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   mThresholdDB = dlog.threshold;
   mRatio = dlog.ratio;
   mAttackTime = dlog.attack;
   mDecayTime = dlog.decay;
   mNormalize = dlog.useGain;

   return true;
}

bool EffectCompressor::NewTrackPass1()
{
   if (mCircle)
      delete[] mCircle;
   if (mLevelCircle)
      delete[] mLevelCircle;

   mCircleSize = 100;
   mCircle = new double[mCircleSize];
   mLevelCircle = new double[mCircleSize];
   for(int j=0; j<mCircleSize; j++) {
      mCircle[j] = 0.0;
      mLevelCircle[j] = mFloor;
   }
   mCirclePos = 0;
   mRMSSum = 0.0;

   mThreshold = pow(10.0, mThresholdDB/20); // factor of 20 because it's power

   mAttackFactor = exp(-log(mFloor) / (mCurRate * mAttackTime + 0.5));
   mDecayFactor = exp(log(mFloor) / (mCurRate * mDecayTime + 0.5));

   mLastLevel = 0.0;

   return true;
}
bool EffectCompressor::InitPass1()
{
   mMax=0.0;
   if (!mNormalize)
       DisableSecondPass();
   return true;
}
bool EffectCompressor::InitPass2()
{
    // Actually, this should not even be called, because we call
    // DisableSecondPass() before, if mNormalize is false.
    return mNormalize;
}
bool EffectCompressor::ProcessPass1(float *buffer, sampleCount len)
{
   double *follow = new double[len];
   int i;
   
    // This makes sure that the initial value is well-chosen
	if (mLastLevel == 0.0) {
		int preSeed = mCircleSize;
		if (preSeed > len)
			preSeed = len;
		for(i=0; i<preSeed; i++)
			AvgCircle(buffer[i]);
	}

	for (i = 0; i < len; i++) {
		Follow(buffer[i], &follow[i], i);
	}

	for (i = 0; i < len; i++) {
		buffer[i] = DoCompression(buffer[i], follow[i]);
	}

	delete[] follow;

    return true;
}

bool EffectCompressor::ProcessPass2(float *buffer, sampleCount len)
{
    if (mMax != 0)
    {
    	for (int i = 0; i < len; i++)
    		buffer[i] /= mMax;
    }
		
	return true;
}

float EffectCompressor::AvgCircle(float value)
{
   float level;

   // Calculate current level from root-mean-squared of
   // circular buffer ("RMS")
   mRMSSum -= mCircle[mCirclePos];
   mCircle[mCirclePos] = value*value;
   mRMSSum += mCircle[mCirclePos];
   level = sqrt(mRMSSum/mCircleSize);
   mLevelCircle[mCirclePos] = level;
   mCirclePos = (mCirclePos+1)%mCircleSize;

#if 0 // Peak instead of RMS
   int j;
   level = 0.0;
   for(j=0; j<mCircleSize; j++)
      if (mCircle[j] > level)
         level = mCircle[j];
#endif

   return level;
}

void EffectCompressor::Follow(float x, double *outEnv, int maxBack)
{
   /*

   "Follow"ing algorithm by Roger B. Dannenberg, taken from
   Nyquist.  His description follows.  -DMM

   Description: this is a sophisticated envelope follower.
    The input is an envelope, e.g. something produced with
    the AVG function. The purpose of this function is to
    generate a smooth envelope that is generally not less
    than the input signal. In other words, we want to "ride"
    the peaks of the signal with a smooth function. The
    algorithm is as follows: keep a current output value
    (called the "value"). The value is allowed to increase
    by at most rise_factor and decrease by at most fall_factor.
    Therefore, the next value should be between
    value * rise_factor and value * fall_factor. If the input
    is in this range, then the next value is simply the input.
    If the input is less than value * fall_factor, then the
    next value is just value * fall_factor, which will be greater
    than the input signal. If the input is greater than value *
    rise_factor, then we compute a rising envelope that meets
    the input value by working bacwards in time, changing the
    previous values to input / rise_factor, input / rise_factor^2,
    input / rise_factor^3, etc. until this new envelope intersects
    the previously computed values. There is only a limited buffer
    in which we can work backwards, so if the new envelope does not
    intersect the old one, then make yet another pass, this time
    from the oldest buffered value forward, increasing on each
    sample by rise_factor to produce a maximal envelope. This will
    still be less than the input.

    The value has a lower limit of floor to make sure value has a
    reasonable positive value from which to begin an attack.
   */

   float level = AvgCircle(x);
   float high = mLastLevel * mAttackFactor;
   float low = mLastLevel * mDecayFactor;

   if (low < mFloor)
      low = mFloor;

   if (level < low)
      *outEnv = low;
   else if (level < high)
      *outEnv = level;
   else {
      // Backtrack
      double attackInverse = 1.0 / mAttackFactor;
      double temp = level * attackInverse;

      int backtrack = 50;
      if (backtrack > maxBack)
         backtrack = maxBack;

      double *ptr = &outEnv[-1];
      int i;
      bool ok = false;
      for(i=0; i<backtrack-2; i++) {
         if (*ptr < temp) {
            *ptr-- = temp;
            temp *= attackInverse;
         }
         else {
            ok = true;
            break;
         }
      }

      if (!ok && backtrack>1 && (*ptr < temp)) {
         temp = *ptr;
         for (i = 0; i < backtrack-1; i++) {
            ptr++;
            temp *= mAttackFactor;
            *ptr = temp;
         }
      }
      else
         *outEnv = level;
   }

   mLastLevel = *outEnv;
}

float EffectCompressor::DoCompression(float value, double env)
{
   float mult;
   float out;

   if (env > mThreshold)
      mult = pow(mThreshold/env, 1.0-1.0/mRatio);
   else
      mult = 1.0;

   out = value * mult;

   if (out > 1.0)
      out = 1.0;

   if (out < -1.0)
      out = -1.0;

   mMax=mMax<fabs(out)?fabs(out):mMax;

   return out;
}

//----------------------------------------------------------------------------
// CompressorPanel
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(CompressorPanel, wxPanel)
    EVT_PAINT(CompressorPanel::OnPaint)
END_EVENT_TABLE()

CompressorPanel::CompressorPanel( wxWindow *parent, wxWindowID id,
                          const wxPoint& pos,
                          const wxSize& size):
   wxPanel(parent, id, pos, size)
{
   mBitmap = NULL;
   mWidth = 0;
   mHeight = 0;
}

void CompressorPanel::OnPaint(wxPaintEvent & evt)
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

   double rangeDB = 60;

   // Ruler
   int w = 0;
   int h = 0;

   Ruler vRuler;
   vRuler.SetBounds(0, 0, mWidth, mHeight);
   vRuler.SetOrientation(wxVERTICAL);
   vRuler.SetRange(0, -rangeDB);
   vRuler.SetFormat(Ruler::LinearDBFormat);
   vRuler.SetUnits(_("dB"));
   vRuler.GetMaxSize(&w, NULL);
   
   Ruler hRuler;
   hRuler.SetBounds(0, 0, mWidth, mHeight);
   hRuler.SetOrientation(wxHORIZONTAL);
   hRuler.SetRange(-rangeDB, 0);
   hRuler.SetFormat(Ruler::LinearDBFormat);
   hRuler.SetUnits(_("dB"));
   hRuler.SetFlip(true);
   hRuler.GetMaxSize(NULL, &h);

   vRuler.SetBounds(0, 0, w, mHeight - h);
   hRuler.SetBounds(w, mHeight - h, mWidth, mHeight);

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

   int kneeX = (int)rint((rangeDB+threshold)*mEnvRect.width/rangeDB);
   int kneeY = (int)rint((rangeDB+threshold)*mEnvRect.height/rangeDB);

   int finalY = kneeY + (int)rint((-threshold/ratio)*mEnvRect.height/rangeDB);

   // Yellow line for threshold
   memDC.SetPen(wxPen(wxColour(220, 220, 0), 1, wxSOLID));
   AColor::Line(memDC,
                mEnvRect.x,
                mEnvRect.y + mEnvRect.height - kneeY,
                mEnvRect.x + mEnvRect.width - 1,
                mEnvRect.y + mEnvRect.height - kneeY);

   // Was: Nice dark red line for the compression diagram
//   memDC.SetPen(wxPen(wxColour(180, 40, 40), 3, wxSOLID));

   // Nice blue line for compressor, same color as used in the waveform envelope.
   memDC.SetPen( AColor::WideEnvelopePen) ;

   AColor::Line(memDC,
                mEnvRect.x,
                mEnvRect.y + mEnvRect.height,
                mEnvRect.x + kneeX - 1,
                mEnvRect.y + mEnvRect.height - kneeY);

   AColor::Line(memDC,
                mEnvRect.x + kneeX,
                mEnvRect.y + mEnvRect.height - kneeY,
                mEnvRect.x + mEnvRect.width - 1,
                mEnvRect.y + mEnvRect.height - finalY);

   // Paint border again
   memDC.SetBrush(*wxTRANSPARENT_BRUSH);
   memDC.SetPen(*wxBLACK_PEN);
   memDC.DrawRectangle(border);

   vRuler.Draw(memDC);
   hRuler.Draw(memDC);

   dc.Blit(0, 0, mWidth, mHeight,
           &memDC, 0, 0, wxCOPY, FALSE);
}

//----------------------------------------------------------------------------
// CompressorDialog
//----------------------------------------------------------------------------

enum {
   ThresholdID = 7100,
   RatioID,
   AttackID,
   DecayID
};

BEGIN_EVENT_TABLE(CompressorDialog, EffectDialog)
   EVT_SIZE( CompressorDialog::OnSize )
   EVT_BUTTON( ID_EFFECT_PREVIEW, CompressorDialog::OnPreview )
   EVT_SLIDER( ThresholdID, CompressorDialog::OnSlider )
   EVT_SLIDER( RatioID, CompressorDialog::OnSlider )
   EVT_SLIDER( AttackID, CompressorDialog::OnSlider )
   EVT_SLIDER( DecayID, CompressorDialog::OnSlider )
END_EVENT_TABLE()

CompressorDialog::CompressorDialog(EffectCompressor *effect, wxWindow *parent)
:  EffectDialog(parent, _("Dynamic Range Compressor"), PROCESS_EFFECT,
                wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER ),
   mEffect(effect)
{
   Init();

   SetSizeHints(500, 300);
   SetSize(500, 400);
}

void CompressorDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(10);
   S.StartHorizontalLay(wxCENTER, false);
   {
      S.AddTitle(_("by Dominic Mazzoni"));
   }
   S.EndHorizontalLay();
   S.SetBorder(5);

   S.StartHorizontalLay(wxEXPAND, true);
   {
      S.SetBorder(10);
      mPanel = new CompressorPanel(S.GetParent(), wxID_ANY);
      mPanel->threshold = threshold;
      mPanel->ratio = ratio;
      S.Prop(true).AddWindow(mPanel, wxEXPAND | wxALL);
      S.SetBorder(5);
   }
   S.EndHorizontalLay();

   S.StartMultiColumn(3, wxCENTER | wxALIGN_CENTER_VERTICAL);
   {
      mThresholdLabel = S.AddVariableText(_("Threshold:"), true,
                                          wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
      S.SetStyle(wxSL_HORIZONTAL);
      mThresholdSlider = S.Id(ThresholdID).AddSlider(wxT(""), -8, -1, -60);
      mThresholdSlider->SetName(_("Threshold"));
      mThresholdText = S.AddVariableText(wxT("XXX dB"), true,
                                         wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

      mRatioLabel = S.AddVariableText(_("Ratio:"), true,
                                      wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
      S.SetStyle(wxSL_HORIZONTAL);
      mRatioSlider = S.Id(RatioID).AddSlider(wxT(""), 4, 20, 3);
      mRatioSlider->SetName(_("Ratio"));
      mRatioText = S.AddVariableText(wxT("XXXX:1"), true,
                                          wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

      mAttackLabel = S.AddVariableText(_("Attack Time:"), true,
                                      wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
      S.SetStyle(wxSL_HORIZONTAL);
      mAttackSlider = S.Id(AttackID).AddSlider(wxT(""), 2, 10, 1);
      mAttackSlider->SetName(_("Attack Time"));
      mAttackText = S.AddVariableText(wxT("XXXX secs"), true,
                                      wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

      mDecayLabel = S.AddVariableText(_("Decay Time:"), true,
                                      wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
      S.SetStyle(wxSL_HORIZONTAL);
      mDecaySlider = S.Id(DecayID).AddSlider(wxT(""), 2, 10, 1);
      mDecaySlider->SetName(_("Decay Time"));
      mDecayText = S.AddVariableText(wxT("XXXX secs"), true,
                                     wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);
   }
   S.EndMultiColumn();

   S.StartHorizontalLay(wxCENTER, false);
   {
      mGainCheckBox = S.AddCheckBox(_("Make-up gain for 0dB after compressing"),
                                    wxT("true"));
   }
   S.EndHorizontalLay();
}

bool CompressorDialog::TransferDataToWindow()
{
   mPanel->threshold = threshold;
   mPanel->ratio = ratio;

   mThresholdSlider->SetValue((int)rint(threshold));
   mRatioSlider->SetValue((int)rint(ratio*2));
   mAttackSlider->SetValue((int)rint(attack*10));
   mDecaySlider->SetValue((int)rint(attack));
   mGainCheckBox->SetValue(useGain);

   TransferDataFromWindow();

   return true;
}

bool CompressorDialog::TransferDataFromWindow()
{
   threshold = (double)mThresholdSlider->GetValue();
   ratio = (double)(mRatioSlider->GetValue() / 2.0);
   attack = (double)(mAttackSlider->GetValue() / 10.0);
   decay = (double)(mDecaySlider->GetValue());
   useGain = mGainCheckBox->GetValue();

   mPanel->threshold = threshold;
   mPanel->ratio = ratio;

   mThresholdLabel->SetName(wxString::Format(_("Threshold %d dB"), (int)threshold));
   mThresholdText->SetLabel(wxString::Format(_("%3d dB"), (int)threshold));

   if (mRatioSlider->GetValue()%2 == 0) {
      mRatioLabel->SetName(wxString::Format(_("Ratio %.0f to 1"), ratio));
      mRatioText->SetLabel(wxString::Format(_("%.0f:1"), ratio));
   }
   else {
      mRatioLabel->SetName(wxString::Format(_("Ratio %.1f to 1"), ratio));
      mRatioText->SetLabel(wxString::Format(_("%.1f:1"), ratio));
   }

   mAttackLabel->SetName(wxString::Format(_("Attack Time %.1f secs"), attack));
   mAttackText->SetLabel(wxString::Format(_("%.1f secs"), attack));

   mDecayLabel->SetName(wxString::Format(_("Decay Time %.1f secs"), decay));
   mDecayText->SetLabel(wxString::Format(_("%.1f secs"), decay));

   mPanel->Refresh(false);

   return true;
}

void CompressorDialog::OnSize(wxSizeEvent &event)
{
   mPanel->Refresh( false );
   event.Skip();
}

void CompressorDialog::OnPreview(wxCommandEvent &event)
{
   TransferDataFromWindow();

	// Save & restore parameters around Preview, because we didn't do OK.
   double    oldAttackTime = mEffect->mAttackTime;
   double    oldDecayTime = mEffect->mDecayTime;
   double    oldThresholdDB = mEffect->mThresholdDB;
   double    oldRatio = mEffect->mRatio;
   bool      oldUseGain = mEffect->mNormalize;

   mEffect->mAttackTime = attack;
   mEffect->mDecayTime = decay;
   mEffect->mThresholdDB = threshold;
   mEffect->mRatio = ratio;
   mEffect->mNormalize = useGain;

   mEffect->Preview();

   mEffect->mAttackTime = oldAttackTime;
   mEffect->mDecayTime = oldDecayTime;
   mEffect->mThresholdDB = oldThresholdDB;
   mEffect->mRatio = oldRatio;
   mEffect->mNormalize = oldUseGain;
}
void CompressorDialog::OnSlider(wxCommandEvent &event)
{
   TransferDataFromWindow();
}
