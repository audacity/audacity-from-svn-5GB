/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelExporter.h

  (c) The Audacity Team

**********************************************************************/

#ifndef __AUDACITY_LABELEXPORTER__
#define __AUDACITY_LABELEXPORTER__


/// This class includes routines to export the label track to
/// a text file.  Options could include different formats.


#include "Exporter.h"
#include "../SampleFormat.h"
class wxString;
class AudacityProject;
class DirManager;
class WaveTrack;
      


class LabelExporter: public Exporter
{
 public:
   LabelExporter(AudacityProject * project, double t0, double t1,
	      bool exportSelection, int mRate, int channels);
  
  ~LabelExporter();
  
  bool Export(const wxString &filename);
  bool Verify();
 private:
  
};

#endif
