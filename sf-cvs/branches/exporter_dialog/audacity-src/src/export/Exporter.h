/******************************************************************
 *            Audacity Generic Exporter Class
 *
 * 
 *  This class is the base class, which should be inherited and 
 *  its methods overridden by format-specific exporters.
 *****************************************************************/
#ifndef __AUDACITY_EXPORTER__
#define __AUDACITY_EXPORTER__

#include <wx/string.h>   

class AudacityProject;
class DirManager;
class WaveTrack;


///Another enum may exist for this.
enum ExportSampleType
  {
    EST_UNKNOWN,
    EST_INTEGER,
    EST_FLOAT,
    EST_DOUBLE
  };

class Exporter
{
 public:
  Exporter(AudacityProject * project, double t0, double t1,
	   bool exportSelection, int rate, int channels);
  virtual ~Exporter();
  virtual bool Verify();
  virtual bool Export(const wxString & filename){mFileName = filename; return false;};       //This method needs to be overridden.

 protected:

  
  AudacityProject * mProject;
  int mChannels;
  wxString mFileName;

  double m_t0, m_t1;               //Left and right of selection
  bool   mExportSelection;         //Whether the selection or the whole file should be exported.
  double mInRate;                  //Sample rate of the project.
  int    mOutRate;                 //Sample rate of exported file.
  double mQuality;                 //Format-specific number determining compression quality.
};





#endif
