/**********************************************************************

  Audacity: A Digital Audio Editor

  CLExporter.cpp

  (c) The Audacity Team

**********************************************************************/

#include "../Audacity.h"
#include "../FileFormats.h"
#include "../LabelTrack.h"
#include "../Mix.h"
#include "../Project.h"
#include "../SampleFormat.h"
#include "../Track.h"
#include "../WaveTrack.h"

#include "CLExporter.h"
#include "Exporter.h"

#include <iostream>

CLExporter::CLExporter(AudacityProject * project, double t0, double t1,
			 bool exportSelection, int rate,
			 int channels):
  Exporter(project, t0, t1, exportSelection, rate, channels)
{
  
}

CLExporter::~CLExporter()
{

}

bool CLExporter::Verify()
{
  return true;
}


bool CLExporter::Export(const wxString & filename)
{

  std::cout << "Exporting Command-line Format Not Implemented\n";
  mFileName = filename;

   return true;
}
