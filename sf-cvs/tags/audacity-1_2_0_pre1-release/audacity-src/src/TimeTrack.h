/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeTrack.h

  Dr William Bland

**********************************************************************/

#ifndef __AUDACITY_TIMETRACK__
#define __AUDACITY_TIMETRACK__

#include "Track.h"
#include <wx/brush.h>
#include <wx/pen.h>

class wxRect;
class wxDC;
class Envelope;
class Ruler;
class ConverterList; /* defined at the end of this file */


class TimeTrack: public Track {

 public:

   TimeTrack(DirManager * projDirManager);
   TimeTrack(TimeTrack &orig);

   virtual ~TimeTrack();

   // Identifying the type of track
   virtual int GetKind() const { return Time; } 

   // TimeTrack parameters

   virtual double GetOffset() { return 0.0; };
   virtual void SetOffset(double /* t */) {};

   virtual double GetStartTime() { return 0.0; };
   virtual double GetEndTime() { return 0.0; };

   void Draw(wxDC & dc, wxRect & r, double h, double pps);

   // XMLTagHandler callback methods for loading and saving

   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual XMLTagHandler *HandleXMLChild(const char *tag);
   virtual void WriteXML(int depth, FILE *fp);

   // Lock and unlock the track: you must lock the track before
   // doing a copy and paste between projects.

   bool Lock();
   bool Unlock();

   // Access the track's speed envelope

   Envelope *GetEnvelope() { return mEnvelope; }

   // Get/Set the speed-warping range, as percentage of original speed (e.g. 90%-110%)

   long GetRangeLower() { return mRangeLower; }
   long GetRangeUpper() { return mRangeUpper; }

   void SetRangeLower(long lower) { mRangeLower = lower; }
   void SetRangeUpper(long upper) { mRangeUpper = upper; }

   double warp( double t );

   void addConverter( int i, const char *name );
   ConverterList* GetConverterList() { return mConverterList; }
   int getConverter() { return mConverter; }
   void setConverter( int i ) { mConverter = i; }

   void testMe();

 private:
   Envelope        *mEnvelope;
   Ruler           *mRuler;
   long             mRangeLower;
   long             mRangeUpper;
   ConverterList   *mConverterList;
   int              mConverter;

   void Init(const TimeTrack &orig);
   virtual Track *Duplicate();

   friend class TrackFactory;

   wxBrush blankBrush;
   wxPen blankPen;
};



/*
  ConverterList is a linked list of available sample rate
  conversion methods.
*/
class ConverterList
{
public:
   int id;
   const char *name;
   ConverterList *next;

   ConverterList()
   {
     id = 0;
     name = NULL;
     next = NULL;
   }

   ConverterList( int the_id, const char *the_name )
   {
     id = the_id;
     name = the_name;
   }
};


#endif // __AUDACITY_TIMETRACK__
