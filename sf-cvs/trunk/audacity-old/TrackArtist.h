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

class VTrack;
class WaveTrack;
class NoteTrack;
class LabelTrack;
struct TrackInfoCache;

#include "TrackPanel.h"

class TrackArtist {

public:
  TrackArtist();
  ~TrackArtist();

  void DrawTracks(TrackList *tracks,
				  wxDC &dc, wxRect &r,
				  wxRect &clip,
				  ViewInfo *viewInfo,
				  bool drawEnvelope);

  void DrawVRuler(VTrack *t,
				  wxDC *dc, wxRect &r);
  
  void SetInset(int left, int top, int right, int bottom);

private:
  wxHashTable     *mTrackHash;

  int mInsetLeft;
  int mInsetTop;
  int mInsetRight;
  int mInsetBottom;

  wxBrush blankBrush;
  wxBrush unselectedBrush;
  wxBrush selectedBrush;
  wxBrush sampleBrush;
  wxBrush selsampleBrush;
  wxPen blankPen;
  wxPen unselectedPen;
  wxPen selectedPen;
  wxPen samplePen;
  wxPen selsamplePen;
  wxPen shadowPen;

  void PrepareCacheWaveform(TrackInfoCache *cache,
							double start, double pps,
							int screenWidth);

  int GetWaveYPos(float value, int height, bool dB);

  void DrawWaveform(TrackInfoCache *cache,
					wxDC &dc, wxRect &r,
					ViewInfo *viewInfo,
					bool drawEnvelope,
					bool dB);

  void PrepareCacheSpectrum(TrackInfoCache *cache,
							double start, double pps,
							int screenWidth, int screenHeight,
							bool autocorrelation);

  void DrawSpectrum(TrackInfoCache *cache,
					wxDC &dc, wxRect &r,
					ViewInfo *viewInfo,
					bool autocorrelation);

  void DrawNoteTrack(TrackInfoCache *cache,
					 wxDC &dc, wxRect &r,
					 ViewInfo *viewInfo);

  void DrawLabelTrack(TrackInfoCache *cache,
					  wxDC &dc, wxRect &r,
					  ViewInfo *viewInfo);
};

#endif // define __AUDACITY_TRACKARTIST__
