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

class wxRect;
class wxDC;
class wxMouseEvent;
class wxTextFile;

class DirManager;

struct EnvPoint {
   double t;
   double val;
};

WX_DEFINE_ARRAY(EnvPoint *, EnvArray);

class Envelope {
 public:
   Envelope();

   virtual ~ Envelope();

   void Flatten(double value);

   void Mirror(bool mirror);

   void CopyFrom(const Envelope * e, double t0, double t1);

   // File I/O

   virtual bool Load(wxTextFile * in, DirManager * dirManager);
   virtual bool Save(wxTextFile * out, bool overwrite);

   // Event Handlers

   void Draw(wxDC & dc, wxRect & r, double h, double pps, bool dB);

   // Returns true if parents needs to be redrawn
   bool MouseEvent(wxMouseEvent & event, wxRect & r,
                   double h, double pps, bool dB);

   // Handling Cut/Copy/Paste events
   void CollapseRegion(double t0, double t1);
   void Paste(double t0, Envelope *e);

   // Control

   void SetOffset(double newOffset);
   void SetTrackLen(double trackLen);

   // Accessors

   double GetValue(double t) const;

   // This is much faster than calling GetValue() multiple times
   // if you need more than one value in a row.
   void GetValues(double *buffer, int len, double t0, double tstep) const;

   bool IsDirty() const;

 private:
   
   bool mMirror;

   int Insert(double when, double value);

   double fromDB(double x) const;
   double toDB(double x);

   EnvArray mEnv;
   double mOffset;
   double mTrackLen;

   int mDragPoint;
   int mInitialX;
   int mInitialY;
   double mInitialWhen;
   double mInitialVal;
   bool mUpper;
   bool mIsDeleting;

   bool mDirty;
};

#endif
