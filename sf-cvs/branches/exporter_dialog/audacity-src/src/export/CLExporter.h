/**********************************************************************

  Audacity: A Digital Audio Editor

  CLExporter.h

  (c) The Audacity Team

**********************************************************************/

#ifndef __AUDACITY_CLEXPORTER__
#define __AUDACITY_CLEXPORTER__


/// This class includes routines to export the selected audio to
/// an file, supported by a command-line encoder.  Most useful on linux and
/// OSX.

#include "Exporter.h"
#include "../SampleFormat.h"
class wxString;
class AudacityProject;
class DirManager;
class WaveTrack;
      


class CLExporter: public Exporter
{
 public:
   CLExporter(AudacityProject * project, double t0, double t1,
	      bool exportSelection, int mRate, 
	      int channels);
  
  ~CLExporter();
  
  bool Export(const wxString &filename);
  bool Verify();
 private:
  
};

#endif
