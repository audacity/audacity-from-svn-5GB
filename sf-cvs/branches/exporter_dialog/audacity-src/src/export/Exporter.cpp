/*****************************************************************
 *            Audacity Generic Exporter Class
 *
 * 
 *  This class is the base class, which should be inherited and 
 *  its methods overridden by format-specific exporters.
 *****************************************************************/


#include "Exporter.h"
#include "../Audacity.h"
#include "../Project.h"
#include "../Mix.h"
#include "../Prefs.h"


#include <wx/progdlg.h>
#include <wx/ffile.h>
#include <wx/log.h>
#include <wx/msgdlg.h>
#include "wx/string.h"


Exporter::Exporter(AudacityProject * project,  double t0, double t1,
		   bool exportSelection, int outrate, int channels):
  mProject(project),
  mChannels(channels),
  m_t0(t0),
  m_t1(t1),
  mExportSelection(exportSelection),
  mOutRate(outrate),
  mInRate(project->GetRate())
{
}

Exporter::~Exporter()
{
}


bool Exporter::Verify()
{
  return true;
}
