/**********************************************************************

  Audacity: A Digital Audio Editor

  OggExporter.h

  (c) The Audacity Team

**********************************************************************/

#ifndef __AUDACITY_OGGEXPORTER__
#define __AUDACITY_OGGEXPORTER__


/// This class includes routines to export the selected audio to
/// an 'Ogg Vorbis'  file, using libvorbis encoder.

#include "Exporter.h"
#include "../SampleFormat.h"
class wxString;
class AudacityProject;
class DirManager;
class WaveTrack;
      


class OggExporter: public Exporter
{
 public:
  OggExporter(AudacityProject * project, double t0, double t1,
	      bool exportSelection, int mRate, 
	      int channels);
  
  ~OggExporter();
  
  bool Export(const wxString &filename);
  bool Verify();
 private:
  
};

#endif
