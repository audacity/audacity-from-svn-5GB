/**********************************************************************

  Audacity: A Digital Audio Editor

  FLACExporter.cpp

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

#include "FLACExporter.h"
#include "Exporter.h"

#include <iostream>

FLACExporter::FLACExporter(AudacityProject * project, double t0, double t1,
			 bool exportSelection, int rate,
			 int channels):
  Exporter(project, t0, t1, exportSelection, rate, channels)
{
  
}

FLACExporter::~FLACExporter()
{

}

bool FLACExporter::Verify()
{
  return true;
}


bool FLACExporter::Export(const wxString & filename)
{

  std::cout << "Exporting FLAC Format Not Implemented\n";
  mFileName = filename;

   return true;
}
