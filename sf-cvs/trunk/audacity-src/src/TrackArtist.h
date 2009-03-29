/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackArtist.h

  Dominic Mazzoni

  This singleton class handles the actual rendering of WaveTracks
  (both waveforms and spectra), NoteTracks, and LabelTracks.

  It's actually a little harder than it looks, because for
  waveforms at least it needs to cache the samples that are
  currently on-screen.

**********************************************************************/

#ifndef __AUDACITY_TRACKARTIST__
#define __AUDACITY_TRACKARTIST__

#include <wx/brush.h>
#include <wx/pen.h>
#include "Experimental.h"
#include "Sequence.h"

class wxDC;
class wxRect;
class wxHashTable;

class Track;
class WaveTrack;
class WaveClip;
class NoteTrack;
class LabelTrack;
class TimeTrack;
class TrackList;
class Ruler;
struct ViewInfo;

#ifndef uchar
typedef unsigned char uchar;
#endif

class AUDACITY_DLL_API TrackArtist {

 public:
   TrackArtist();
   ~TrackArtist();

   void SetColours();
   void DrawTracks(TrackList * tracks,
                   wxDC & dc, wxRegion & reg,
                   wxRect & r, wxRect & clip, ViewInfo * viewInfo, 
                   bool drawEnvelope,bool drawSamples,bool drawSliders);

   void DrawVRuler(Track * t, wxDC * dc, wxRect & r);

   void SetInset(int left, int top, int right, int bottom);

   void UpdatePrefs();

   //
   // Lower-level drawing functions
   // 

   void DrawWaveform(WaveTrack *track,
                     wxDC & dc, wxRect & r,
                     ViewInfo * viewInfo, 
                     bool drawEnvelope, bool drawSamples,
                     bool drawSliders, bool dB, bool muted);

   void DrawSpectrum(WaveTrack *track,
                     wxDC & dc, wxRect & r,
                     ViewInfo * viewInfo, bool autocorrelation, bool logF);
#ifdef USE_MIDI
   void DrawNoteTrack(NoteTrack *track,
                      wxDC & dc, wxRect & r, ViewInfo * viewInfo);
#endif // USE_MIDI

   void DrawLabelTrack(LabelTrack *track,
                       wxDC & dc, wxRect & r, ViewInfo * viewInfo);

   void DrawTimeTrack(TimeTrack *track,
                       wxDC & dc, wxRect & r, ViewInfo * viewInfo);

   void DrawTimeSlider(WaveTrack *track,
                       wxDC & dc, wxRect & r, ViewInfo * viewInfo,
                       bool rightwards);

   void DrawClipWaveform(WaveTrack* track, WaveClip* clip,
                         wxDC & dc, wxRect & r,
                         ViewInfo * viewInfo,
                         bool drawEnvelope,
                         bool drawSamples,
                         bool drawSliders,
                         bool dB, bool muted);

   void DrawClipSpectrum(WaveTrack *track, WaveClip *clip,
                         wxDC & dc, wxRect & r,
                         ViewInfo * viewInfo, bool autocorrelation, bool logF);

   void SetBackgroundBrushes(wxBrush unselectedBrush, wxBrush selectedBrush,
			     wxPen unselectedPen, wxPen selectedPen) {
     this->unselectedBrush = unselectedBrush;
     this->selectedBrush = selectedBrush;
     this->unselectedPen = unselectedPen;
     this->selectedPen = selectedPen;
   }

 private:

   int mInsetLeft;
   int mInsetTop;
   int mInsetRight;
   int mInsetBottom;

   float mdBrange;
   long mShowClipping;

   wxBrush blankBrush;
   wxBrush unselectedBrush;
   wxBrush selectedBrush;
   wxBrush sampleBrush;
   wxBrush selsampleBrush;
   wxBrush dragsampleBrush;// for samples which are draggable.
   wxBrush muteSampleBrush;
   wxPen blankPen;
   wxPen unselectedPen;
   wxPen selectedPen;
   wxPen samplePen;
   wxPen rmsPen;
   wxPen muteRmsPen;
   wxPen selsamplePen;
   wxPen muteSamplePen;
   wxPen odProgressNotYetPen;
   wxPen odProgressDonePen;
   wxPen shadowPen;
   wxPen clippedPen;
   wxPen muteClippedPen;

   Ruler *vruler;

#ifdef EXPERIMENTAL_FFT_Y_GRID
   bool fftYGridOld;
#endif //EXPERIMENTAL_FFT_Y_GRID

#ifdef EXPERIMENTAL_FIND_NOTES
   bool fftFindNotesOld;
   int findNotesMinAOld;
   int findNotesNOld;
   bool findNotesQuantizeOld;
#endif

   // Waveform utility functions

   void DrawWaveformBackground(wxDC &dc, wxRect r, uchar *imageBuffer,
                               sampleCount *where, sampleCount ssel0, sampleCount ssel1,
                               double *env, 
                               float zoomMin, float zoomMax,
                               bool dB, bool drawEnvelope);

   void DrawIndividualSamples(wxDC &dc, wxRect r,
                              WaveTrack *track,                
                              double t0, double pps, double h,
                              float zoomMin, float zoomMax,
                              bool dB,
                              bool drawSamples,
                              bool showPoints, bool muted);

   void DrawIndividualClipSamples(wxDC &dc, wxRect r,
                                        WaveClip *clip,
                                        double t0, double pps, double h,
                                        float zoomMin, float zoomMax,
                                        bool dB,
                                        bool drawSamples,
                                        bool showPoints, bool muted);

   void DrawMinMaxRMS(wxDC &dc, wxRect r, uchar *imageBuffer,
                      float zoomMin, float zoomMax,
                      double *envValues,
                      float *min, float *max, float *rms,int* bl,
                      bool dB, bool muted, bool showProgress);

   void DrawNegativeOffsetTrackArrows(wxDC &dc, wxRect &r);

   void DrawEnvLine(wxDC &dc, wxRect r, int x, int y, bool top);

};

extern int GetWaveYPos(float value, float min, float max,
			  int height, bool dB, bool outer, float dBr, 
			  bool clip);



#endif                          // define __AUDACITY_TRACKARTIST__

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: cb734132-2712-4d57-9644-778dae6d3c50

