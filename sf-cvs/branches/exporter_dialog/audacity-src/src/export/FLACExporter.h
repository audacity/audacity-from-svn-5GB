/**********************************************************************

  Audacity: A Digital Audio Editor

  FLACExporter.h

  (c) The Audacity Team

**********************************************************************/

#ifndef __AUDACITY_FLACEXPORTER__
#define __AUDACITY_FLACEXPORTER__


/// This class includes routines to export the selected audio
/// track into a 'FLAC' format file.


#include "Exporter.h"
#include "../SampleFormat.h"
class wxString;
class AudacityProject;
class DirManager;
class WaveTrack;
      


class FLACExporter: public Exporter
{
 public:
   FLACExporter(AudacityProject * project, double t0, double t1,
	      bool exportSelection, int mRate, 
	      int channels);
  
  ~FLACExporter();
  
  bool Export(const wxString &filename);
  bool Verify();
 private:
  
};

#endif
