/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioView.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_AUDIOVIEW__
#define __AUDACITY_AUDIOVIEW__

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/docview.h"
#include "wx/scrolwin.h"

#include "UndoManager.h"

class TrackList;

class AudioView;

const int kAutoScrollDelay = 50;

class RulerPanel: public wxPanel
{
private:
  int bitWidth;
  int bitHeight;
  wxBitmap *bitmap;
  wxCursor *mainCursor;

public:
  AudioView *view;
  
  RulerPanel(wxView *v, wxFrame *frame,
			 const wxPoint& pos, const wxSize& size, 
			 const long style);
  virtual void OnPaint(wxPaintEvent &evt);
  void OnMouseEvent(wxMouseEvent& event);  
  void OnEraseBackground(wxEraseEvent &ignore);

  DECLARE_EVENT_TABLE()
};

class TrackPanel: public wxPanel
{
private:
  int bitWidth;
  int bitHeight;
  wxBitmap *bitmap;

public:
  AudioView *view;
  
  TrackPanel(wxView *v, wxFrame *frame,
			 const wxPoint& pos, const wxSize& size, 
			 const long style);

  void OnEraseBackground(wxEraseEvent &ignore);

  void OnPaint(wxPaintEvent &evt);
  void OnMouseEvent(wxMouseEvent& event);
  
  void FastScroll(int hoffset, int voffset);

  wxCursor *mainCursor;
  wxCursor *noneCursor;
  wxCursor *dragCursor;
  wxCursor *resizeCursor;
  
  DECLARE_EVENT_TABLE()
};

class AudioViewTimer: public wxTimer
{
public:
  virtual void Notify();
  AudioView *view;
};

struct ScrollInfo {
  double h;       // h pos in secs
  double screen;  // screen width in secs
  double total;   // total width in secs
  double zoom;    // pixels per second
  double lastZoom;

  // actual scroll bar positions, in pixels
  int sbarH;
  int sbarScreen;
  int sbarTotal;

  int scrollStep;
};

class AudioView: public wxView
{
  DECLARE_DYNAMIC_CLASS(AudioView)

private:
  static int trackInset;

  wxBrush backgroundBrush;
  wxPen backgroundPen;
  wxColour backgroundColor;

  wxBrush lightBrush[2];
  wxBrush mediumBrush[2];
  wxBrush darkBrush[2];
  wxPen lightPen[2];
  wxPen mediumPen[2];
  wxPen darkPen[2];

  AudioViewTimer myTimer;

public:
  static int labelWidth;

  wxMenuBar *menuBar;
  wxMenu *fileMenu;
  wxMenu *editMenu;
  wxMenu *projectMenu;
  wxMenu *trackMenu;
  wxMenu *effectMenu;
  wxMenu *helpMenu;

  wxFrame *frame;
  TrackPanel *trackPanel;
  RulerPanel *rulerPanel;
  wxScrollBar *hsbar;
  wxScrollBar *vsbar;
  wxStaticText *status;
  wxBoxSizer *mainSizer;
  wxBoxSizer *topSizer;
  wxBoxSizer *bottomSizer;
  wxBoxSizer *trackSizer;

  ScrollInfo sinfo; // horizontal scroll
  int vpos;         // vertical scroll
  
  double selstart;
  double sel0;
  double sel1;
  
  bool dragging;
  bool resizing;

  int mouseClickX;
  int mouseClickY;
  int mouseMostRecentX;
  int mouseMostRecentY;
  int initialTrackHeight;

  VTrack *capturedTrack;
  wxRect capturedRect;
  int capturedNum;
  
  int numEffects;

  TrackList *clipboard;
  double clipLen;

  UndoManager undoManager;

  AudioView();
  ~AudioView();
  
  TrackList *GetTracks();
  
  virtual bool ProcessEvent(wxEvent& event);
  
  void ClearClipboard();
  
  void Cut();
  void Copy();
  void Paste();
  void Clear();  
  void SelectAll();
  void SelectNone();

  void InitialState();
  void PushState();
  void PopState(TrackList *l);
  void Undo();
  void Redo();

  void UpdateMenus();
  
  void OnKeyDown(wxKeyEvent& keyEvent);

  void FixScrollbars();

  void OnZoomInButton();
  void OnZoomOutButton();
  void OnPlayButton();
  void OnStopButton();

  void AutoCorrelate();
  void Pitch();
  void QuickMix();

  void WaveDisplay();
  void SpectrumDisplay();

  void NewWaveTrack();
  void NewLabelTrack();

  void RemoveTracks();

  void Import();
  void ImportRaw();
  void ImportMIDI();
  void ImportMP3();
  void Export();

  void Scroll(wxScrollEvent &evt);

  void FillBackgroundColor(wxDC *dc, wxRect *r);
  void DrawRuler(wxDC *dc, wxRect *r);
  void DrawLabel(wxDC *dc, VTrack *t, int d, int y,
				 int num, int total);
  void DrawTracks(wxDC *dc, wxRect *visible);
  
  VTrack *FindTrack(int mouseX, int mouseY, bool label,
					wxRect *trackRect = NULL, int *trackNum = NULL);

  bool DClickLabel(wxMouseEvent& event);
  bool ClickLabel(wxMouseEvent& event);

  void TrackSetCursor(wxMouseEvent& event);
  
  void AutoScroll();
  bool ClickTrack(wxMouseEvent& event);
  bool DragTrack(wxMouseEvent& event);
  void ButtonUpTrack();

  void OnDraw(wxDC *dc);  
  void OnSize(wxSizeEvent &evt);
  bool OnCreate(wxDocument *doc, long flags);
  void OnUpdate(wxView *sender, wxObject *hint = (wxObject *) NULL);
  bool OnClose(bool deleteWindow = TRUE);

DECLARE_EVENT_TABLE()
};

#endif
