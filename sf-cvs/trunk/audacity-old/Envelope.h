/**********************************************************************

  Audacity: A Digital Audio Editor

  Envelope.h

  Dominic Mazzoni

  This class manages an envelope - i.e. a piecewise linear funtion
  that the user can edit by dragging control points around.  The
  envelope is most commonly used to control the amplitude of a
  waveform, but it is also used to shape a general FFT filter.

**********************************************************************/

#ifndef __AUDACITY_ENVELOPE__
#define __AUDACITY_ENVELOPE__

#include <wx/dynarray.h>
#include <wx/event.h>
#include <wx/textfile.h>

class DirManager;

struct EnvPoint {
  double t;
  double val;
};

WX_DEFINE_ARRAY(EnvPoint *, EnvArray);

class Envelope
{
public:
  Envelope();

  void CopyFrom(Envelope *e);

  // File I/O

  virtual bool Load(wxTextFile *in, DirManager *dirManager);
  virtual bool Save(wxTextFile *out, bool overwrite);

  // Event Handlers

  void Draw(wxDC &dc, wxRect &r, double h, double pps);

  // Returns true if parents needs to be redrawn
  bool MouseEvent(wxMouseEvent &event, wxRect &r, double h, double pps);

  // Control

  void SetOffset(double newOffset);

  void SetTrackLen(double trackLen);

  // Accessors

  bool IsLinearInRegion(double t0, double t1);

  double GetValue(double t);

  bool IsDirty();

private:

  int Insert(double when, double value);

  EnvArray   mEnv;
  double     mOffset;
  double     mTrackLen;

  int        mDragPoint;
  int        mInitialX;
  int        mInitialY;
  double     mInitialWhen;
  double     mInitialVal;
  bool       mUpper;
  bool       mIsDeleting;

  bool       mDirty;
};

#endif



