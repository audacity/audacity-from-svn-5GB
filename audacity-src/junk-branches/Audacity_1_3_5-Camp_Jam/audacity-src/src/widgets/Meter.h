/**********************************************************************

  Audacity: A Digital Audio Editor

  Meter.h

  Dominic Mazzoni

  VU Meter, for displaying recording/playback level

  This is a bunch of common code that can display many different
  forms of VU meters and other displays.

**********************************************************************/

#ifndef __AUDACITY_METER__
#define __AUDACITY_METER__

#include <wx/defs.h>
#include <wx/panel.h>
#include <wx/timer.h>

#include "../AudacityBranding.h"
#include "../SampleFormat.h"
#include "Ruler.h"

#if (AUDACITY_BRANDING == BRAND_JAMLING__EASY)
   // Hide menus on meter toolbar. Don't show numbers and ticks. Label as "Mix Volume" and "Input Volume"
   #define WANT_METER_MENU 0
#else
   #define WANT_METER_MENU 1
#endif

// Increase this when we add support for multichannel meters
// (most of the code is already there)
const int kMaxMeterBars = 2;

struct MeterBar {
   bool   vert;
   wxRect r;
   float  peak;
   float  rms;
   float  peakHold;
   double peakHoldTime;
   wxRect rClip;
   bool   clipping;
   int    tailPeakCount;
   float  peakPeakHold;
};

struct MeterUpdateMsg
{
   int numFrames;
   float peak[kMaxMeterBars];
   float rms[kMaxMeterBars];
   bool clipping[kMaxMeterBars];
   int headPeakCount[kMaxMeterBars];
   int tailPeakCount[kMaxMeterBars];
};

// Thread-safe queue of update messages
class MeterUpdateQueue
{
 public:
   MeterUpdateQueue(int maxLen);
   ~MeterUpdateQueue();

   bool Put(MeterUpdateMsg &msg);
   bool Get(MeterUpdateMsg &msg);

   void Clear();

 private:
   int              mStart;
   int              mEnd;
   int              mBufferSize;
   MeterUpdateMsg  *mBuffer;
};

class Meter : public wxPanel
{
   DECLARE_DYNAMIC_CLASS(Meter)

 public:
   Meter(wxWindow* parent, wxWindowID id,
         bool isInput,
         const wxPoint& pos = wxDefaultPosition,
         const wxSize& size = wxDefaultSize);

   ~Meter();

   void UpdatePrefs();

   // These should be kept in the same order as they appear
   // in the menu
   enum Style {
      HorizontalStereo,
      VerticalStereo,
      VerticalMulti,
      Equalizer,
      Waveform
   };

   Style GetStyle() { return mStyle; }
   void SetStyle(Style newStyle);

   //
   // These methods are thread-safe!  Feel free to call from a
   // different thread (like from an audio I/O callback)
   void Reset(double sampleRate, bool resetClipping);
   void UpdateDisplay(int numChannels,
                      int numFrames, float *sampleData);
   bool IsMeterDisabled();
   // End thread-safe methods
   //
   
   float GetMaxPeak();

   //
   // Event handlers
   //

   void OnErase(wxEraseEvent &evt);
   void OnPaint(wxPaintEvent &evt);
   void OnSize(wxSizeEvent &evt);
   void OnMouse(wxMouseEvent &evt);

   void OnMeterUpdate(wxTimerEvent &evt);

   void HandlePaint(wxDC &dc);
   void HandleLayout();

   #if WANT_METER_MENU
      //
      // Pop-up menu handlers
      //
      void OnDisableMeter(wxCommandEvent &evt);
      void OnHorizontal(wxCommandEvent &evt);
      void OnVertical(wxCommandEvent &evt);
      void OnMulti(wxCommandEvent &evt);
      void OnEqualizer(wxCommandEvent &evt);
      void OnWaveform(wxCommandEvent &evt);
      void OnLinear(wxCommandEvent &evt);
      void OnDB(wxCommandEvent &evt);
      void OnClip(wxCommandEvent &evt);
      void OnMonitor(wxCommandEvent &evt);
      void OnFloat(wxCommandEvent &evt);
      void OnPreferences(wxCommandEvent &evt);
   #endif // WANT_METER_MENU
   
   void StartMonitoring();
 private:
   void DrawMeterBar(wxDC &dc, MeterBar *meterBar);
   void ResetBar(MeterBar *bar, bool resetClipping);
   void RepaintBarsNow();
   void CreateIcon(int aquaOffset);
   wxFont GetFont();

   MeterUpdateQueue mQueue;
   wxTimer          mTimer;

   int       mWidth;
   int       mHeight;

   bool      mIsInput;

   Style     mStyle, mSavedStyle;
   bool      mDB;
   int       mDBRange;
   bool      mDecay;
   float     mDecayRate; // dB/sec
   bool      mClip;
   int       mNumPeakSamplesToClip;
   double    mPeakHoldDuration;
   double    mT;
   double    mRate;
   long      mMeterRefreshRate;
   long      mMeterDisabled; //is used as a bool, needs long for easy gPrefs...

   int       mNumBars;
   MeterBar  mBar[kMaxMeterBars];

   bool      mLayoutValid;

   wxBitmap *mBitmap;
#if WANT_METER_MENU
   wxRect    mMenuRect;
#endif // WANT_METER_MENU
   wxPoint   mIconPos;
   wxPoint   mLeftTextPos;
   wxPoint   mRightTextPos;
   wxString  mLeftText;
   wxString  mRightText;
   wxSize    mLeftSize;
   wxSize    mRightSize;
   wxBitmap *mIcon;
   wxPen     mPen;
   wxPen     mLightPen;
   wxPen     mSavedLightPen;
   wxPen     mDarkPen;
   wxPen     mSavedDarkPen;
   wxPen     mDisabledPen;
   wxPen     mPeakPeakPen;
   wxBrush   mBrush;
   wxBrush   mRMSBrush;
   wxBrush   mClipBrush;
   wxBrush   mBkgndBrush;
   wxBrush   mSavedBkgndBrush;
   wxBrush   mSavedBrush;
   wxBrush   mSavedRMSBrush;
   wxBrush   mDisabledBkgndBrush;
   wxRect    mAllBarsRect;
   Ruler     mRuler;

   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_METER__

// Indentation settings for Vim and Emacs.
// Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// 
