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

class wxDC;
class wxRect;
class wxHashTable;

class Track;
class WaveTrack;
class NoteTrack;
class LabelTrack;
class TimeTrack;
class TrackList;
class Ruler;
struct ViewInfo;

class TrackArtist {

 public:
   TrackArtist();
   ~TrackArtist();

   void DrawTracks(TrackList * tracks,
                   wxDC & dc, wxRect & r,
                   wxRect & clip, ViewInfo * viewInfo, 
                   bool drawEnvelope,bool drawSamples,bool drawSliders);

   void DrawVRuler(Track * t, wxDC * dc, wxRect & r);

   void SetInset(int left, int top, int right, int bottom);
   int GetWaveYPos(float value, int height, bool dB);

 private:
   int mInsetLeft;
   int mInsetTop;
   int mInsetRight;
   int mInsetBottom;

   wxBrush blankBrush;
   wxBrush unselectedBrush;
   wxBrush selectedBrush;
   wxBrush sampleBrush;
   wxBrush selsampleBrush;
   wxBrush dragsampleBrush;// for samples which are draggable.
   wxPen blankPen;
   wxPen unselectedPen;
   wxPen selectedPen;
   wxPen samplePen;
   wxPen rmsPen;
   wxPen selsamplePen;
   wxPen shadowPen;

   Ruler *vruler;

   void DrawWaveform(WaveTrack *track,
                     wxDC & dc, wxRect & r,
                     ViewInfo * viewInfo, 
                     bool drawEnvelope, bool drawSamples,bool drawSliders, bool dB);

   void DrawSpectrum(WaveTrack *track,
                     wxDC & dc, wxRect & r,
                     ViewInfo * viewInfo, bool autocorrelation);

   void DrawNoteTrack(NoteTrack *track,
                      wxDC & dc, wxRect & r, ViewInfo * viewInfo);

   void DrawLabelTrack(LabelTrack *track,
                       wxDC & dc, wxRect & r, ViewInfo * viewInfo);

   void DrawTimeTrack(TimeTrack *track,
                       wxDC & dc, wxRect & r, ViewInfo * viewInfo);

   void DrawTimeSlider(WaveTrack *track,
                       wxDC & dc, wxRect & r, ViewInfo * viewInfo, bool rightwards);

};

#endif                          // define __AUDACITY_TRACKARTIST__
