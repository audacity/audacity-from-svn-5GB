/**********************************************************************

  Audacity: A Digital Audio Editor

  OggExporter.cpp

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

#include "OggExporter.h"
#include "Exporter.h"

#include <iostream>

OggExporter::OggExporter(AudacityProject * project, double t0, double t1,
			 bool exportSelection, int rate,
			 int channels):
  Exporter(project, t0, t1, exportSelection, rate, channels)
{
  
}

OggExporter::~OggExporter()
{

}

bool OggExporter::Verify()
{
  return true;
}


bool OggExporter::Export(const wxString & filename)
{

  std::cout << "Exporting Ogg Format Not Implemented\n";
  mFileName = filename;

   return true;
}
