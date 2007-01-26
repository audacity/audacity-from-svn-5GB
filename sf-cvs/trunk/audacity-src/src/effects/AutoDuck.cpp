/**********************************************************************

  Audacity: A Digital Audio Editor

  AutoDuck.cpp

  Markus Meyer

*******************************************************************//**

\class EffectAutoDuck
\brief Implements the Auto Ducking effect

*******************************************************************/

#include <math.h>
#include <wx/sizer.h>
#include <wx/dynarray.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>

#include "../Audacity.h"
#include "../Prefs.h"
#include "../Internat.h"
#include "../Theme.h"
#include "../AllThemeResources.h"
#include "../AColor.h"

#include "AutoDuck.h"

#include <wx/arrimpl.cpp>

/*
 * Default values for effect params
 */
#define PARAM_DEFAULT_DUCK_AMOUNT_DB -12.0
#define PARAM_DEFAULT_FADE_DOWN_LEN 0.5
#define PARAM_DEFAULT_FADE_UP_LEN 0.5
#define PARAM_DEFAULT_THRESHOLD_DB -30.0
#define PARAM_DEFAULT_MAXIMUM_PAUSE 0

/*
 * Common constants
 */

#define BUF_SIZE 4096 // number of samples to process at once
#define RMS_WINDOW_SIZE 100 // samples in circular RMS window buffer

/*
 * A auto duck region and an array of auto duck regions
 */

struct AutoDuckRegion
{
   AutoDuckRegion(double t0, double t1)
   {
      this->t0 = t0;
      this->t1 = t1;
   }
   
   double t0;
   double t1;
};

WX_DECLARE_OBJARRAY(AutoDuckRegion, AutoDuckRegionArray);
WX_DEFINE_OBJARRAY(AutoDuckRegionArray);

/*
 * Effect implementation
 */

EffectAutoDuck::EffectAutoDuck()
{
   SetEffectFlags(BUILTIN_EFFECT | PROCESS_EFFECT | ADVANCED_EFFECT);
   
   mControlTrack = NULL;
}

bool EffectAutoDuck::Init()
{
   gPrefs->Read(wxT("/Effects/AutoDuck/DuckAmountDb"),
      &mDuckAmountDb, PARAM_DEFAULT_DUCK_AMOUNT_DB);
   gPrefs->Read(wxT("/Effects/AutoDuck/FadeDownLen"),
      &mFadeDownLen, PARAM_DEFAULT_FADE_DOWN_LEN);
   gPrefs->Read(wxT("/Effects/AutoDuck/FadeUpLen"),
      &mFadeUpLen, PARAM_DEFAULT_FADE_UP_LEN);
   gPrefs->Read(wxT("/Effects/AutoDuck/ThresholdDb"),
      &mThresholdDb, PARAM_DEFAULT_THRESHOLD_DB);
   gPrefs->Read(wxT("/Effects/AutoDuck/MaximumPause"),
      &mMaximumPause, PARAM_DEFAULT_MAXIMUM_PAUSE);

   mControlTrack = NULL;
   
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   
   bool lastWasSelectedWaveTrack = false;
   WaveTrack *controlTrackCandidate = NULL;
   
   while(t)
   {
      if (lastWasSelectedWaveTrack && !t->GetSelected() &&
          t->GetKind() == Track::Wave)
      {
         // This could be the control track, so remember it
         controlTrackCandidate = (WaveTrack*)t;
      }

      lastWasSelectedWaveTrack = false;
      
      if (t->GetSelected())
      {
         if (t->GetKind() == Track::Wave)
         {
            lastWasSelectedWaveTrack = true;
         } else
         {
            wxMessageBox(
               _("You selected a track which does not contain audio. AutoDuck can only process audio tracks."),
               _("AutoDuck"), wxICON_ERROR, mParent);
            return false;
         }
      }

      t = iter.Next();
   }
   
   if (!controlTrackCandidate)
   {
      wxMessageBox(
         _("AutoDuck needs a control track which must be placed below the selected track(s)."),
         _("AutoDuck"), wxICON_ERROR, mParent);
      return false;
   }
   
   mControlTrack = controlTrackCandidate;
   wxASSERT(mWaveTracks);

   return true;
}

bool EffectAutoDuck::TransferParameters( Shuttle & shuttle )
{
   shuttle.TransferDouble(wxT("DuckAmountDb"), mDuckAmountDb,
      PARAM_DEFAULT_DUCK_AMOUNT_DB);
   shuttle.TransferDouble(wxT("FadeDownLen"), mFadeDownLen,
      PARAM_DEFAULT_FADE_DOWN_LEN);
   shuttle.TransferDouble(wxT("FadeUpLen"), mFadeUpLen,
      PARAM_DEFAULT_FADE_UP_LEN);
   shuttle.TransferDouble(wxT("ThresholdDb"), mThresholdDb,
      PARAM_DEFAULT_THRESHOLD_DB);
   shuttle.TransferDouble(wxT("MaximumPause"), mMaximumPause,
      PARAM_DEFAULT_MAXIMUM_PAUSE);

   return true;
}

bool EffectAutoDuck::CheckWhetherSkipEffect()
{
   return false;
}

void EffectAutoDuck::End()
{
   mControlTrack = NULL;
}

bool EffectAutoDuck::PromptUser()
{
   EffectAutoDuckDialog dlog(this, mParent);
   
   if (dlog.ShowModal() != wxID_OK)
      return false; // user cancelled dialog
      
   gPrefs->Write(wxT("/Effects/AutoDuck/DuckAmountDb"), mDuckAmountDb);
   gPrefs->Write(wxT("/Effects/AutoDuck/FadeDownLen"), mFadeDownLen);
   gPrefs->Write(wxT("/Effects/AutoDuck/FadeUpLen"), mFadeUpLen);
   gPrefs->Write(wxT("/Effects/AutoDuck/ThresholdDb"), mThresholdDb);
   gPrefs->Write(wxT("/Effects/AutoDuck/MaximumPause"), mMaximumPause);

   return true;
}

bool EffectAutoDuck::Process()
{
   int i;
   
   if (!mWaveTracks || !mControlTrack)
      return false;

   bool cancel = false;

   longSampleCount start =
      mControlTrack->TimeToLongSamples(mT0 + mFadeDownLen);
   longSampleCount end =
      mControlTrack->TimeToLongSamples(mT1 - mFadeUpLen);
   
   if (end <= start)
      return false;

   // the minimum number of samples we have to wait until the maximum
   // pause has been exceeded
   double maxPause = mMaximumPause;
   if (maxPause < mFadeDownLen + mFadeUpLen)
      maxPause = mFadeDownLen + mFadeUpLen;
   longSampleCount minSamplesPause =
      mControlTrack->TimeToLongSamples(maxPause);

   double threshold = pow(10.0, mThresholdDb/20);
      
   // adjust the threshold so we can compare it to the rmsSum value
   threshold = threshold * threshold * RMS_WINDOW_SIZE;

   int rmsPos = 0;
   float rmsSum = 0;
   float *rmsWindow = new float[RMS_WINDOW_SIZE];
   for (i = 0; i < RMS_WINDOW_SIZE; i++)
      rmsWindow[i] = 0;
   
   float *buf = new float[BUF_SIZE];
   
   bool inDuckRegion = false;

   // initialize the following two variables to prevent compiler warning
   double duckRegionStart = 0;
   sampleCount curSamplesPause = 0;
   
   // to make the progress bar appear more natural, we first look for all
   // duck regions and apply them all at once afterwards
   AutoDuckRegionArray regions;
   longSampleCount pos = start;
   
   while (pos < end)
   {
      longSampleCount len = end - pos;
      if (len > BUF_SIZE)
         len = BUF_SIZE;
      
      mControlTrack->Get((samplePtr)buf, floatSample, pos, (sampleCount)len);
      
      for (i = pos; i < pos + len; i++)
      {
         rmsSum -= rmsWindow[rmsPos];
         rmsWindow[rmsPos] = buf[i - pos] * buf[i - pos];
         rmsSum += rmsWindow[rmsPos];
         rmsPos = (rmsPos + 1) % RMS_WINDOW_SIZE;

         bool thresholdExceeded = rmsSum > threshold;
         
         if (thresholdExceeded)
         {
            // everytime the threshold is exceeded, reset our count for
            // the number of pause samples
            curSamplesPause = 0;
            
            if (!inDuckRegion)
            {
               // the threshold has been exceeded for the first time, so
               // let the duck region begin here
               inDuckRegion = true;
               duckRegionStart = mControlTrack->LongSamplesToTime(i);
            }
         }
         
         if (!thresholdExceeded && inDuckRegion)
         {
            // the threshold has not been exceeded and we are in a duck
            // region, but only fade in if the maximum pause has been
            // exceeded
            curSamplesPause += 1;
            
            if (curSamplesPause >= minSamplesPause)
            {
               // do the actual duck fade and reset all values
               double duckRegionEnd =
                  mControlTrack->LongSamplesToTime(i - curSamplesPause);
                  
               regions.Add(AutoDuckRegion(
                              duckRegionStart - mFadeDownLen,
                              duckRegionEnd + mFadeUpLen));

               inDuckRegion = false;
            }
         }
      }
      
      pos += len;

      if (TotalProgress( ((double)(pos-start)) / (end-start) /
                         (GetNumWaveTracks() + 1) ))
      {
         cancel = true;
         break;
      }
   }
   
   // apply last duck fade, if any
   if (inDuckRegion)
   {
      double duckRegionEnd =
         mControlTrack->LongSamplesToTime(end - curSamplesPause);
      regions.Add(AutoDuckRegion(
                     duckRegionStart - mFadeDownLen,
                     duckRegionEnd + mFadeUpLen));
   }
   
   delete[] buf;
   delete[] rmsWindow;

   if (!cancel)
   {
      TrackListIterator iter(mWaveTracks);
      Track *iterTrack = iter.First();
      
      int trackNumber = 0;
      
      while (iterTrack)
      {
         wxASSERT(iterTrack->GetKind() == Track::Wave);
         
         WaveTrack* t = (WaveTrack*)iterTrack;
      
         for (i = 0; i < (int)regions.GetCount(); i++)
         {
            const AutoDuckRegion& region = regions[i];
            if (ApplyDuckFade(trackNumber, t, region.t0, region.t1))
            {
               cancel = true;
               break;
            }
         }
         
         if (cancel)
            break;

         iterTrack = iter.Next();
         trackNumber++;
      }
   }
   
   return !cancel;
}

// this currently does an exponential fade
bool EffectAutoDuck::ApplyDuckFade(int trackNumber, WaveTrack* t,
                                   double t0, double t1)
{
   bool cancel = false;
   
   int start = t->TimeToLongSamples(t0);
   int end = t->TimeToLongSamples(t1);
   
   float *buf = new float[BUF_SIZE];
   int pos = start;
   
   int fadeDownSamples = t->TimeToLongSamples(mFadeDownLen);
   if (fadeDownSamples < 1)
      fadeDownSamples = 1;

   int fadeUpSamples = t->TimeToLongSamples(mFadeUpLen);
   if (fadeUpSamples < 1)
      fadeUpSamples = 1;
      
   float fadeDownStep = mDuckAmountDb / fadeDownSamples;
   float fadeUpStep = mDuckAmountDb / fadeUpSamples;

   while (pos < end)
   {
      longSampleCount len = end - pos;
      if (len > BUF_SIZE)
         len = BUF_SIZE;

      t->Get((samplePtr)buf, floatSample, pos, (sampleCount)len);
      
      for (int i = pos; i < pos + len; i++)
      {
         float gainDown = fadeDownStep * (i - start);
         float gainUp = fadeUpStep * (end - i);;
         
         float gain;
         if (gainDown > gainUp)
            gain = gainDown;
         else
            gain = gainUp;
         if (gain < mDuckAmountDb)
            gain = mDuckAmountDb;

         buf[i - pos] *= pow(10.0, gain / 20.0);
      }
      
      t->Set((samplePtr)buf, floatSample, pos, (sampleCount)len);

      pos += len;
      
      float curTime = t->LongSamplesToTime(pos);
      float fractionFinished = (curTime - mT0) / (mT1 - mT0);
      if (TotalProgress( (trackNumber + 1 + fractionFinished) /
                         (GetNumWaveTracks() + 1) ))
      {
         cancel = true;
         break;
      }
   }

   return cancel;
}

/*
 * Effect dialog implementation
 */
 
#define ID_DUCK_AMOUNT_DB  10001
#define ID_THRESHOLD_DB    10002
#define ID_FADE_DOWN_LEN   10003
#define ID_FADE_UP_LEN     10004
#define ID_MAXIMUM_PAUSE   10005
#define ID_PANEL           10006

BEGIN_EVENT_TABLE(EffectAutoDuckDialog, wxDialog)
   EVT_BUTTON(wxID_OK, EffectAutoDuckDialog::OnOk)
   EVT_BUTTON(wxID_OK, EffectAutoDuckDialog::OnCancel)
   EVT_TEXT(ID_DUCK_AMOUNT_DB, EffectAutoDuckDialog::OnValueChanged)
   EVT_TEXT(ID_THRESHOLD_DB, EffectAutoDuckDialog::OnValueChanged)
   EVT_TEXT(ID_FADE_DOWN_LEN, EffectAutoDuckDialog::OnValueChanged)
   EVT_TEXT(ID_FADE_UP_LEN, EffectAutoDuckDialog::OnValueChanged)
   EVT_TEXT(ID_MAXIMUM_PAUSE, EffectAutoDuckDialog::OnValueChanged)
END_EVENT_TABLE()

EffectAutoDuckDialog::EffectAutoDuckDialog(EffectAutoDuck* effect,
   wxWindow *parent) : wxDialog(parent, -1, _("Auto Duck"))
{
   mEffect = effect;

   ShuttleGui S(this, eIsCreating);
   
   S.SetBorder(5);
   S.StartVerticalLay(true);
   {
      S.StartHorizontalLay(wxCENTER, false);
      {
         S.AddTitle(_("Auto Duck by Markus Meyer"));
      }
      S.EndHorizontalLay();

      S.StartHorizontalLay(wxCENTER, false);
      {
         // Add a little space
      }
      S.EndHorizontalLay();
      
      mPanel = (EffectAutoDuckPanel*)
         S.AddWindow(new EffectAutoDuckPanel(this, ID_PANEL));

      S.StartHorizontalLay(wxCENTER, false);
      {
         // Add a little space
      }
      S.EndHorizontalLay();

      S.StartMultiColumn(6, wxCENTER);
      {
         mDuckAmountDbBox = S.Id(ID_DUCK_AMOUNT_DB).AddTextBox(
            _("Duck Amount:"),
            Internat::ToDisplayString(mEffect->mDuckAmountDb), 10);
         S.AddUnits(_("db"));
         
         mMaximumPauseBox = S.Id(ID_MAXIMUM_PAUSE).AddTextBox(
            _("Maximum Pause:"),
            Internat::ToDisplayString(mEffect->mMaximumPause), 10);
         S.AddUnits(_("seconds"));

         mFadeDownLenBox = S.Id(ID_FADE_DOWN_LEN).AddTextBox(
            _("Fade Down Length:"),
            Internat::ToDisplayString(mEffect->mFadeDownLen), 10);
         S.AddUnits(_("seconds"));
         
         mThresholdDbBox = S.Id(ID_THRESHOLD_DB).AddTextBox(
            _("Threshold:"),
            Internat::ToDisplayString(mEffect->mThresholdDb), 10);
         S.AddUnits(_("db"));

         mFadeUpLenBox = S.Id(ID_FADE_UP_LEN).AddTextBox(
            _("Fade Up Length:"),
            Internat::ToDisplayString(mEffect->mFadeUpLen), 10);
         S.AddUnits(_("seconds"));

      }
      S.EndMultiColumn();

      S.StartHorizontalLay(wxALIGN_CENTER, false);
      {
#if defined(__WXGTK20__) || defined(__WXMAC__)
         S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
         S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
#else
         S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
         S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
#endif
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   GetSizer()->AddSpacer(5);
   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();
}

void EffectAutoDuckDialog::OnOk(wxCommandEvent& evt)
{
   double duckAmountDb = 0, thresholdDb = 0, fadeDownLen = 0;
   double fadeUpLen = 0, maximumPause = 0;
   
   bool success = 
      mDuckAmountDbBox->GetValue().ToDouble(&duckAmountDb) &&
      mEffect->mDuckAmountDb > -100 &&
      mEffect->mDuckAmountDb < 0 &&
      mThresholdDbBox->GetValue().ToDouble(&thresholdDb) &&
      mEffect->mThresholdDb > -100 &&
      mEffect->mThresholdDb < 0 &&
      mFadeDownLenBox->GetValue().ToDouble(&fadeDownLen) &&
      mEffect->mFadeDownLen >= 0 &&
      mEffect->mFadeDownLen < 1000 &&
      mFadeUpLenBox->GetValue().ToDouble(&fadeUpLen) &&
      mEffect->mFadeUpLen >= 0 &&
      mEffect->mFadeUpLen < 1000 &&
      mMaximumPauseBox->GetValue().ToDouble(&maximumPause) &&
      mEffect->mMaximumPause >= 0 &&
      mEffect->mMaximumPause < 1000;
      
   if (!success)
   {
      wxMessageBox(_("Please enter valid values."), _("Auto Duck"),
         wxICON_ERROR, this);
      return;
   }
   
   mEffect->mDuckAmountDb = duckAmountDb;
   mEffect->mThresholdDb = thresholdDb;
   mEffect->mFadeDownLen = fadeDownLen;
   mEffect->mFadeUpLen = fadeUpLen;
   mEffect->mMaximumPause = maximumPause;

   EndModal(wxID_OK);
}

void EffectAutoDuckDialog::OnCancel(wxCommandEvent& evt)
{
   EndModal(wxID_CANCEL);
}

void EffectAutoDuckDialog::OnValueChanged(wxCommandEvent& evt)
{
   mPanel->Refresh(false);
}

/*
 * Effect dialog panel implementation
 */

#define CONTROL_POINT_REGION 10 // pixel distance to click on a control point
#define CONTROL_POINT_MIN_MOVE 5 // min mouse move until value is changed

#define TEXT_DISTANCE 15 // pixel distance text <-> center of control point

#define FADE_DOWN_START 50 // x coordinate
#define FADE_UP_START 550 // x coordinate
#define DUCK_AMOUNT_START 50 // y coordinate

#define MAX_DUCK_AMOUNT 0 // db
#define MIN_DUCK_AMOUNT -24 // db

#define MIN_FADE 0 // seconds
#define MAX_FADE 4.5 // seconds

#define FADE_SCALE 50 // scale factor for second -> pixel conversion
#define DUCK_AMOUNT_SCALE 8 // scale factor for db -> pixel conversion

static int GetDistance(const wxPoint& first, const wxPoint& second)
{
   int distanceX = abs(first.x - second.x);
   int distanceY = abs(first.y - second.y);
   if (distanceX > distanceY)
      return distanceX;
   else
      return distanceY;
}

BEGIN_EVENT_TABLE(EffectAutoDuckPanel, wxPanel)
   EVT_PAINT(EffectAutoDuckPanel::OnPaint)
   EVT_MOUSE_CAPTURE_CHANGED(EffectAutoDuckPanel::OnMouseCaptureChanged)
   EVT_LEFT_DOWN(EffectAutoDuckPanel::OnLeftDown)
   EVT_LEFT_UP(EffectAutoDuckPanel::OnLeftUp)
   EVT_MOTION(EffectAutoDuckPanel::OnMotion)
END_EVENT_TABLE()

EffectAutoDuckPanel::EffectAutoDuckPanel(EffectAutoDuckDialog* parent,
   wxWindowID id) : wxPanel(parent, id, wxDefaultPosition, wxSize(600, 300))
{
   mParent = parent;
   mCurrentControlPoint = none;
   mBackgroundBitmap = NULL;
   
   ResetControlPoints();
}

EffectAutoDuckPanel::~EffectAutoDuckPanel()
{
   if (mBackgroundBitmap)
      delete mBackgroundBitmap;
}

void EffectAutoDuckPanel::ResetControlPoints()
{
   mControlPoints[fadeDown] = wxPoint(-100,-100);
   mControlPoints[fadeUp] = wxPoint(-100,-100);
   mControlPoints[duckAmount] = wxPoint(-100,-100);
}

void EffectAutoDuckPanel::OnPaint(wxPaintEvent& evt)
{
   int clientWidth, clientHeight;
   GetSize(&clientWidth, &clientHeight);
   
   if (!mBackgroundBitmap || mBackgroundBitmap->GetWidth() != clientWidth ||
       mBackgroundBitmap->GetHeight() != clientHeight)
   {
      if (mBackgroundBitmap)
         delete mBackgroundBitmap;
      mBackgroundBitmap = new wxBitmap(clientWidth, clientHeight);
   }
   
   wxMemoryDC dc;
   dc.SelectObject(*mBackgroundBitmap);

   dc.SetBrush(*wxWHITE_BRUSH);
   dc.SetPen(*wxBLACK_PEN);
   dc.DrawRectangle(0, 0, clientWidth, clientHeight);
   
   dc.SetFont(wxFont(10, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL,
                     wxFONTWEIGHT_NORMAL));
   dc.SetTextForeground(*wxBLACK);
   dc.SetTextBackground(*wxWHITE);

   double duckAmountDb = 0;
   double fadeDownLen = 0;
   double fadeUpLen = 0;
   mParent->mDuckAmountDbBox->GetValue().ToDouble(&duckAmountDb);
   mParent->mFadeDownLenBox->GetValue().ToDouble(&fadeDownLen);
   mParent->mFadeUpLenBox->GetValue().ToDouble(&fadeUpLen);
   
   if (fadeDownLen < MIN_FADE || fadeDownLen > MAX_FADE ||
       fadeUpLen < MIN_FADE || fadeUpLen > MAX_FADE ||
       duckAmountDb < MIN_DUCK_AMOUNT || duckAmountDb > MAX_DUCK_AMOUNT)
   {
      // values are out of range, no preview available
      wxString message = wxString::Format(_("Preview not available"));
      int textWidth = 0, textHeight = 0;
      dc.GetTextExtent(message, &textWidth, &textHeight);
      dc.DrawText(message, (clientWidth - textWidth) / 2,
                           (clientHeight - textHeight) / 2);

      ResetControlPoints();
   } else
   {
      // draw preview
      dc.SetBrush(*wxTRANSPARENT_BRUSH);
      dc.SetPen(wxPen(theTheme.Colour(clrGraphLines), 3, wxSOLID));
      
      wxPoint points[6];
      
      points[0].x = FADE_DOWN_START - 30;
      points[0].y = DUCK_AMOUNT_START;
      
      points[1].x = FADE_DOWN_START;
      points[1].y = DUCK_AMOUNT_START;
      
      points[2].x = FADE_DOWN_START + (int)(fadeDownLen * FADE_SCALE);
      points[2].y = DUCK_AMOUNT_START -
         (int)(duckAmountDb * DUCK_AMOUNT_SCALE);
      
      points[3].x = FADE_UP_START - (int)(fadeUpLen * FADE_SCALE);
      points[3].y = DUCK_AMOUNT_START -
         (int)(duckAmountDb * DUCK_AMOUNT_SCALE);
      
      points[4].x = FADE_UP_START;
      points[4].y = DUCK_AMOUNT_START;
      
      points[5].x = FADE_UP_START + 30;
      points[5].y = DUCK_AMOUNT_START;
      
      dc.DrawLines(6, points);
      
      dc.SetPen(AColor::envelopePen);
      dc.SetBrush(*wxWHITE_BRUSH);
      
      mControlPoints[fadeDown] = wxPoint(points[2].x - 3, points[2].y - 2);
      mControlPoints[fadeUp] = wxPoint(points[3].x - 3, points[3].y - 2);
      mControlPoints[duckAmount] = wxPoint(
         (points[2].x + points[3].x) / 2 - 3, points[2].y - 3);
         
      for (int i = 0; i < AUTO_DUCK_PANEL_NUM_CONTROL_POINTS; i++)
      {
         int digits;
         float value;
         
         if (i == (int)fadeDown)
         {
            value = fadeDownLen;
            digits = 2;
         }
         else if (i == (int)fadeUp)
         {
            value = fadeUpLen;
            digits = 2;
         }
         else
         {
            value = duckAmountDb;
            digits = 3;
         }

         wxString valueStr = Internat::ToDisplayString(value, digits);
         valueStr += wxT(" ");
         
         if (i == (int)duckAmount)
            valueStr += wxT("db"); // i18n-hint: short form of 'decibels'
         else
            valueStr += _("s"); // i18n-hint: short form of 'seconds'
   
         int textWidth = 0, textHeight = 0;
         GetTextExtent(valueStr, &textWidth, &textHeight);
         
         int textPosX = mControlPoints[i].x - textWidth / 2;
         int textPosY = mControlPoints[i].y;
         
         if (i == (int)duckAmount)
            textPosY -= TEXT_DISTANCE + textHeight;
         else
            textPosY += TEXT_DISTANCE;
            
         dc.DrawText(valueStr, textPosX, textPosY);

         dc.DrawEllipse(mControlPoints[i].x,
                        mControlPoints[i].y, 6, 6);
      }
   }

   // copy background buffer to paint dc
   wxPaintDC paintDC(this);
   paintDC.Blit(0, 0, clientWidth, clientHeight, &dc, 0, 0);

   // clean up: necessary to free resources on Windows
   dc.SetPen(wxNullPen);
   dc.SetBrush(wxNullBrush);
   dc.SetFont(wxNullFont);
   dc.SelectObject(wxNullBitmap);
}

void EffectAutoDuckPanel::OnMouseCaptureChanged(
   wxMouseCaptureChangedEvent &evt)
{
   SetCursor(wxNullCursor);
   mCurrentControlPoint = none;
}

EffectAutoDuckPanel::EControlPoint
   EffectAutoDuckPanel::GetNearestControlPoint(const wxPoint& pt)
{
   int dist[AUTO_DUCK_PANEL_NUM_CONTROL_POINTS];
   int i;
   
   for (i = 0; i < AUTO_DUCK_PANEL_NUM_CONTROL_POINTS; i++)
      dist[i] = GetDistance(pt, mControlPoints[i]);
      
   int curMinimum = 0;
   for (i = 0; i < AUTO_DUCK_PANEL_NUM_CONTROL_POINTS; i++)
      if (dist[i] < dist[curMinimum])
         curMinimum = i;
   
   if (dist[curMinimum] <= CONTROL_POINT_REGION)
      return (EControlPoint)curMinimum;
   else
      return none;
}

void EffectAutoDuckPanel::OnLeftDown(wxMouseEvent &evt)
{
   EControlPoint nearest = GetNearestControlPoint(evt.GetPosition());
   
   if (nearest != none)
   {
      // this control point has been clicked
      mMouseDownPoint = evt.GetPosition();
      
      mCurrentControlPoint = nearest;
      mControlPointMoveActivated = false;
      
      for (int i = 0; i < AUTO_DUCK_PANEL_NUM_CONTROL_POINTS; i++)
         mMoveStartControlPoints[i] = mControlPoints[i];
         
      CaptureMouse();
   }
}

void EffectAutoDuckPanel::OnLeftUp(wxMouseEvent &evt)
{
   if (mCurrentControlPoint != none)
   {
      mCurrentControlPoint = none;
      ReleaseMouse();
   }
}

void EffectAutoDuckPanel::OnMotion(wxMouseEvent &evt)
{
   switch (GetNearestControlPoint(evt.GetPosition()))
   {
   case none:
      SetCursor(wxNullCursor);
      break;
   case fadeDown:
   case fadeUp:
      SetCursor(wxCursor(wxCURSOR_SIZEWE));
      break;
   case duckAmount:
      SetCursor(wxCursor(wxCURSOR_SIZENS));
      break;
   }
   
   if (mCurrentControlPoint != none)
   {
      if (!mControlPointMoveActivated)
      {
         int dist;
         
         if (mCurrentControlPoint == duckAmount)
            dist = abs(evt.GetY() - mMouseDownPoint.y);
         else
            dist = abs(evt.GetX() - mMouseDownPoint.x);

         if (dist >= CONTROL_POINT_MIN_MOVE)
            mControlPointMoveActivated = true;
      }

      if (mControlPointMoveActivated)
      {
         int dist;
         
         if (mCurrentControlPoint == duckAmount)
            dist = abs(evt.GetY() -
               mMoveStartControlPoints[mCurrentControlPoint].y);
         else
            dist = abs(evt.GetX() -
               mMoveStartControlPoints[mCurrentControlPoint].x);

         float newValue;
         
         switch (mCurrentControlPoint)
         {
         case fadeDown:
            newValue = ((double)(evt.GetX() - FADE_DOWN_START)) / FADE_SCALE;
            if (newValue < MIN_FADE)
               newValue = MIN_FADE;
            if (newValue > MAX_FADE)
               newValue = MAX_FADE;
            mParent->mFadeDownLenBox->SetValue(
               Internat::ToDisplayString(newValue));
            break;
         case fadeUp:
            newValue = ((double)(FADE_UP_START - evt.GetX())) / FADE_SCALE;
            if (newValue < MIN_FADE)
               newValue = MIN_FADE;
            if (newValue > MAX_FADE)
               newValue = MAX_FADE;
            mParent->mFadeUpLenBox->SetValue(
               Internat::ToDisplayString(newValue));
            break;
         case duckAmount:
            newValue = ((double)(DUCK_AMOUNT_START - evt.GetY())) /
                           DUCK_AMOUNT_SCALE;
            if (newValue < MIN_DUCK_AMOUNT)
               newValue = MIN_DUCK_AMOUNT;
            if (newValue > MAX_DUCK_AMOUNT)
               newValue = MAX_DUCK_AMOUNT;
            mParent->mDuckAmountDbBox->SetValue(
               Internat::ToDisplayString(newValue));
            break;
         case none:
            wxASSERT(false); // should not happen
         }
         
         Refresh(false);
      }
   }
}
