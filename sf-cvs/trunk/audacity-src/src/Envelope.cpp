/**********************************************************************

  Audacity: A Digital Audio Editor

  Envelope.cpp

  Dominic Mazzoni

**********************************************************************/

#include "Envelope.h"

#include <math.h>

#include <wx/dc.h>
#include <wx/brush.h>
#include <wx/event.h>
#include <wx/pen.h>
#include <wx/textfile.h>

#include "AColor.h"
#include "DirManager.h"

Envelope::Envelope()
{
   mOffset = 0.0;
   mTrackLen = 1000000000.0;

   mDB = true;

   Insert(0.0, 1.0);
   Insert(1000000000.0, 1.0);

   mDragPoint = -1;
   mDirty = false;

   mIsDeleting = false;

   mMirror = true;
}

Envelope::~Envelope()
{
   WX_CLEAR_ARRAY(mEnv)
}

void Envelope::SetInterpolateDB(bool db)
{
   mDB = db;
}

void Envelope::Mirror(bool mirror)
{
   mMirror = mirror;
}

void Envelope::Flatten(double value)
{
   WX_CLEAR_ARRAY(mEnv)
   Insert(0.0, value);
   Insert(1000000000.0, value);
}

void Envelope::CopyFrom(const Envelope * e, double t0, double t1)
{
   mOffset = wxMax(t0, e->mOffset);
   mTrackLen = wxMin(t1, e->mOffset + e->mTrackLen) - mOffset;

   WX_CLEAR_ARRAY(mEnv)
   
   int len = e->mEnv.Count();

   // Create the point at 0 
   EnvPoint *pt = new EnvPoint();
   pt->t = 0;
   pt->val = e->GetValue(t0);
   mEnv.Add(pt);

   // Skip the points that come before the copied region
   int i = 0;
   while (i < len && e->mOffset + e->mEnv[i]->t <= t0)
      i++;

   // Copy points from inside the copied region
   while (i < len && e->mOffset + e->mEnv[i]->t < mOffset + mTrackLen) {
      EnvPoint *pt = new EnvPoint();
      pt->t = e->mEnv[i]->t + e->mOffset - mOffset;
      pt->val = e->mEnv[i]->val;
      mEnv.Add(pt);
      i++;
   }

   // Create the final point
   if (mTrackLen > 0) {
      EnvPoint *pt = new EnvPoint();
      pt->t = mTrackLen;
      pt->val = e->GetValue(mOffset + mTrackLen);
      mEnv.Add(pt);
   }
}

double Envelope::toDB(double value)
{
   if (value == 0)
      return 0;

   double sign = (value >= 0 ? 1 : -1);

   double db = 20 * log10(fabs(value));
   // The smallest value we will see is -45.15 (10*log10(1/32768))
   // XXX FIXME This is wrong - swh
   double val = (db + 45.0) / 45.0;
   if (val < 0.0)
      val = 0.0;
   if (val > 1.0)
      val = 1.0;

   return sign * val;
}

double Envelope::fromDB(double value) const
{
   if (value == 0)
      return 0;

   return pow(10.0, ((value * 45.0) - 45.0) / 20.0);
}

void Envelope::Draw(wxDC & dc, wxRect & r, double h, double pps, bool dB)
{
   h -= mOffset;

   double tright = h + (r.width / pps);

   dc.SetPen(AColor::envelopePen);
   dc.SetBrush(*wxWHITE_BRUSH);

   int ctr, height;
   if (mMirror) {
      height = r.height / 2;
      ctr = r.y + height;
   }
   else {
      height = r.height;
      ctr = r.y + height;
   }

   int len = mEnv.Count();
   for (int i = 0; i < len; i++) {
      if (mEnv[i]->t >= h && mEnv[i]->t <= tright) {
         if (i == mDragPoint) {
            dc.SetPen(AColor::envelopePen);
            dc.SetBrush(AColor::envelopeBrush);
         }

         double v = mEnv[i]->val;
         int x = int ((mEnv[i]->t - h) * pps);
         int y;

         if (dB)
            y = int (toDB(v) * height);
         else
         y = int (v * height);

         wxRect circle(r.x + x - 1, ctr - y, 4, 4);
         dc.DrawEllipse(circle);
         if (mMirror) {
            circle.y = ctr + y - 2;
            dc.DrawEllipse(circle);
         }

         if (i == mDragPoint) {
            dc.SetPen(AColor::envelopePen);
            dc.SetBrush(*wxWHITE_BRUSH);
         }
      }
   }
}

bool Envelope::HandleXMLTag(const char *tag, const char **attrs)
{
   if (!strcmp(tag, "envelope")) {
      int numPoints = 0;

      while (*attrs) {
         const char *attr = *attrs++;
         const char *value = *attrs++;
         if (!strcmp(attr, "numpoints"))
            numPoints = atoi(value);
      }
      WX_CLEAR_ARRAY(mEnv)
      mEnv.Alloc(numPoints);
      return true;
   }
   else {
      return false;
   }
}

XMLTagHandler *Envelope::HandleXMLChild(const char *tag)
{
   if (!strcmp(tag, "controlpoint")) {
      EnvPoint *e = new EnvPoint();
      mEnv.Add(e);
      return e;
   }
   else
      return NULL;
}

void Envelope::WriteXML(int depth, FILE *fp)
{
   unsigned int ctrlPt;
   int i;

   for (i = 0; i < depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "<envelope numpoints='%ld'>\n", mEnv.GetCount());

   for (ctrlPt = 0; ctrlPt < mEnv.GetCount(); ctrlPt++) {
      for(i = 0; i < depth+1; i++)
         fprintf(fp, "\t");
      fprintf(fp, "<controlpoint t='%f' val='%f'/>\n", mEnv[ctrlPt]->t,
                                                       mEnv[ctrlPt]->val);
   }

   for (i = 0; i < depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "</envelope>\n");
}

#if LEGACY_PROJECT_FILE_SUPPORT

bool Envelope::Load(wxTextFile * in, DirManager * dirManager)
{
   if (in->GetNextLine() != "EnvNumPoints")
      return false;

   unsigned long len;
   if (!(in->GetNextLine().ToULong(&len)))
      return false;

   WX_CLEAR_ARRAY(mEnv)
   mEnv.Alloc(len);

   for (unsigned long i = 0; i < len; i++) {
      EnvPoint *e = new EnvPoint();
      if (!(in->GetNextLine().ToDouble(&e->t)))
         return false;
      if (!(in->GetNextLine().ToDouble(&e->val)))
         return false;
      if (i > 0 && mEnv[i - 1]->t > e->t)
         return false;
      mEnv.Add(e);
   }

   mTrackLen = mEnv[len-1]->t;

   if (in->GetNextLine() != "EnvEnd")
      return false;

   return true;
}

bool Envelope::Save(wxTextFile * out, bool overwrite)
{
   out->AddLine("EnvNumPoints");
   int len = mEnv.Count();
   out->AddLine(wxString::Format("%d", len));
   for (int i = 0; i < len; i++) {
      out->AddLine(wxString::Format("%lf", mEnv[i]->t));
      out->AddLine(wxString::Format("%lf", mEnv[i]->val));
   }
   out->AddLine("EnvEnd");

   return true;
}

#endif /* LEGACY_PROJECT_FILE_SUPPORT */

// Returns true if parent needs to be redrawn
bool Envelope::MouseEvent(wxMouseEvent & event, wxRect & r,
                          double h, double pps, bool dB)
{
   //h -= mOffset;

   int ctr, height;
   bool upper;
   if (mMirror) {
      height = r.height / 2;
      ctr = r.y + height;
      upper = (event.m_y < ctr);
   }
   else {
      height = r.height;
      ctr = r.y + height;
      upper = true;
   }

   if (event.ButtonDown()) {
      mIsDeleting = false;
      double tleft = h - mOffset;
      double tright = tleft + (r.width / pps);
      int bestNum = -1;
      int bestDist = 10;

      int len = mEnv.Count();
      for (int i = 0; i < len; i++) {
         if (mEnv[i]->t >= tleft && mEnv[i]->t <= tright) {
            double v = mEnv[i]->val;
            int x = int ((mEnv[i]->t + mOffset - h) * pps) + r.x;
            int dy;

            if (dB)
               dy = int (toDB(v) * height);
            else
            dy = int (v * height);

            int y;
            if (upper)
               y = int (ctr - dy);
            else
            y = int (ctr + dy);

#ifndef SQR
#define SQR(X) ((X)*(X))
#endif

            int d =
                int (sqrt(SQR(x - event.m_x) + SQR(y - event.m_y)) + 0.5);
            if (d < bestDist) {
               bestNum = i;
               bestDist = d;
            }
         }
      }

      if (bestNum >= 0) {
         mDragPoint = bestNum;
      } else {
         // Create new point
         double when = h + (event.m_x - r.x) / pps - mOffset;

         if (when <= 0 || when >= mTrackLen)
            return false;

         int dy;
         if (upper)
            dy = ctr - event.m_y;
         else
            dy = event.m_y - ctr;

         double newVal;

         if (dB)
            newVal = fromDB(dy / double (height));
         else
            newVal = dy / double (height);

         if (newVal < 0.0)
            newVal = 0.0;
         if (newVal > 1.0)
            newVal = 1.0;

         mDragPoint = Insert(when, newVal);
         mDirty = true;
      }

      mUpper = upper;

      mInitialWhen = mEnv[mDragPoint]->t;
      mInitialVal = mEnv[mDragPoint]->val;

      mInitialX = event.m_x;
      mInitialY = event.m_y;

      return true;
   }

   if (event.Dragging() && mDragPoint >= 0) {
      mDirty = true;

      wxRect larger = r;
      larger.Inflate(5, 5);

      if (!mIsDeleting &&
          mDragPoint > 0 && mDragPoint < (int)mEnv.Count() - 1 &&
          !larger.Inside(event.m_x, event.m_y)) {

         mEnv[mDragPoint]->t = mEnv[mDragPoint - 1]->t;
         mEnv[mDragPoint]->val = mEnv[mDragPoint - 1]->val;

         mIsDeleting = true;

         return true;
      }

      if (larger.Inside(event.m_x, event.m_y))
         mIsDeleting = false;

      if (mIsDeleting)
         return false;

      int y;
      if (mUpper)
         y = ctr - event.m_y;
      else
         y = event.m_y - ctr;

      double newVal;

      if (dB)
         newVal = fromDB(y / double (height));
      else
         newVal = y / double (height);

      if (newVal < 0.0)
         newVal = 0.0;
      if (newVal > 1.0)
         newVal = 1.0;

      double newWhen = mInitialWhen + (event.m_x - mInitialX) / pps;

      if (mDragPoint > 0 && newWhen < mEnv[mDragPoint - 1]->t)
         newWhen = mEnv[mDragPoint - 1]->t;

      if (mDragPoint < (int)mEnv.Count() - 1
          && newWhen > (double)mEnv[mDragPoint + 1]->t)
         newWhen = mEnv[mDragPoint + 1]->t;

      if (mDragPoint == 0)
         newWhen = 0;

      if (mDragPoint == (int)mEnv.Count() - 1)
         newWhen = mTrackLen;

      mEnv[mDragPoint]->t = newWhen;
      mEnv[mDragPoint]->val = newVal;

      return true;
   }

   if (event.ButtonUp()) {
      if (mIsDeleting) {
         delete mEnv[mDragPoint];
         mEnv.RemoveAt(mDragPoint);
      }
      mDragPoint = -1;
      return true;
   }

   return false;
}

void Envelope::CollapseRegion(double t0, double t1)
{
   // This gets called when somebody clears samples.  All of the
   // control points within the region disappear and the points
   // to the right get shifted over.

   t0 -= mOffset;
   t1 -= mOffset;

   int len = mEnv.Count();
   int i;

   for (i = 1; i < len - 1; i++)
      if (mEnv[i]->t >= t0 && mEnv[i]->t < t1) {
         delete mEnv[i];
         mEnv.RemoveAt(i);
         len--;
         i--;
      }

   for (i = 0; i < len; i++)
      if (mEnv[i]->t >= t1)
         mEnv[i]->t -= (t1 - t0);
}

void Envelope::Paste(double t0, Envelope *e)
{
   t0 = wxMin(t0 - mOffset, mTrackLen);
   double deltat = e->mTrackLen;

   unsigned int i;
   unsigned int len = mEnv.Count();
   if (!len) return;

   // Shift existing points to the right
   for (i = 0; i < len; i++)
      if (mEnv[i]->t > t0)
         mEnv[i]->t += deltat;
   mTrackLen += deltat;

   // Start of the selection
   double endval = GetValue(t0 + mOffset);
   Insert(t0, endval);

   // Copy points from inside the selection
   len = e->mEnv.Count();
   for (i = 0; i < len; i++)
      Insert(t0 + e->mEnv[i]->t, e->mEnv[i]->val);

   // End of the selection
   Insert(t0 + deltat, endval);

   // Clean up duplicate points
   //
   // Worst-case, this is O(nm) when we are removing m of the n total points.
   // I don't think this will be an issue, but if so then we could test just
   // the points we generate at the selection boundaries.

   for (i = 2; i < mEnv.Count(); i++) {
      double v0 = mEnv[i-2]->val,
             v1 = mEnv[i-1]->val,
             v2 = mEnv[i]->val;

      if (v0 == v1 && v1 == v2) {
         delete mEnv[i-1];
         mEnv.RemoveAt(i-1);
         --i;
      }
   }
}

void Envelope::InsertSpace(double t0, double tlen)
{
   unsigned int len = mEnv.Count();
   unsigned int i;

   for (i = 0; i < len; i++)
      if (mEnv[i]->t > t0)
         mEnv[i]->t += tlen;
   mTrackLen += tlen;
}

// Private methods

int Envelope::Insert(double when, double value)
{
   EnvPoint *e = new EnvPoint();
   e->t = when;
   e->val = value;

   int len = mEnv.Count();
   if (len == 0) {
      mEnv.Add(e);
      return 0;
   } else {
      int i = 0;
      while (i < len && when >= mEnv[i]->t)
         i++;

      if (i < len) {
         mEnv.Insert(e, i);
         return i;
      } else {
         mEnv.Add(e);
         return len;
      }
   }
}

// Control

void Envelope::SetOffset(double newOffset)
{
   mOffset = newOffset;
}

void Envelope::SetTrackLen(double trackLen)
{
   mTrackLen = trackLen;

   int len = mEnv.Count();
   for (int i = 0; i < len - 1; i++)
      if (mEnv[i]->t > mTrackLen) {
         delete mEnv[i];
         mEnv.RemoveAt(i);
         len--;
      }
   mEnv[len - 1]->t = mTrackLen;
}

// Accessors
double Envelope::GetValue(double t) const
{
   double temp;

   GetValues(&temp, 1, t, 1.0);
   return temp;
}

void Envelope::GetValues(double *buffer, int bufferLen,
                         double t0, double tstep) const
{
   t0 -= mOffset;

   int len = mEnv.Count();

   double t = t0;

   double tprev, vprev, tnext = 0, vnext, vstep = 0;

   for (int b = 0; b < bufferLen; b++) {

      if (len <= 0 || t < mEnv[0]->t || t > mEnv[len - 1]->t) {
         buffer[b] = 1.0;
         t += tstep;
         continue;
      }

      if (b == 0 || t > tnext) {

         // binary search
         int lo = 0;
         int hi = len - 1;
         while (hi > (lo + 1)) {
            int mid = (lo + hi) / 2;
            if (t < mEnv[mid]->t)
               hi = mid;
            else
               lo = mid;
         }

         tprev = mEnv[lo]->t;
         tnext = mEnv[hi]->t;

         if (mDB) {
            vprev = log10(mEnv[lo]->val);
            vnext = log10(mEnv[hi]->val);

            // Special case for the log of zero
            if (mEnv[lo]->val <= 0.0)
               vprev = -4.6;       // This corresponds to the log10 of 1/32768
            if (mEnv[hi]->val <= 0.0)
               vnext = -4.6;
         }
         else {
            vprev = mEnv[lo]->val;
            vnext = mEnv[hi]->val;
         }

         // Interpolate

         double dt = (tnext - tprev);

         double to = t - tprev;
         double v;
         if (dt > 0.0)
            v = (vprev * (dt - to) + vnext * to) / dt;
         else
            v = tnext;

         if (mDB) {
            buffer[b] = pow(10.0, v);
            if (dt > 0.0)
               vstep = pow(10.0, (vnext - vprev) * tstep / dt);
            else
               vstep = 1.0;
         }
         else {
            buffer[b] = v;
            if (dt > 0.0)
               vstep = (vnext - vprev) * tstep / dt;
            else
               vstep = 0.0;
         }
      } else {
         if (mDB)
            buffer[b] = buffer[b - 1] * vstep;
         else
            buffer[b] = buffer[b - 1] + vstep;
      }

      t += tstep;
   }

}
