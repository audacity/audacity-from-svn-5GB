/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelExporter.cpp

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

#include "LabelExporter.h"
#include "Exporter.h"

#include <iostream>

LabelExporter::LabelExporter(AudacityProject * project, double t0, double t1,
			     bool exportSelection, int rate,
			     int channels):
  Exporter(project, t0, t1, exportSelection, rate, channels)
{
  
}

LabelExporter::~LabelExporter()
{

}

bool LabelExporter::Verify()
{
  return true;
}


bool LabelExporter::Export(const wxString & filename)
{

  std::cout << "Exporting Label Format Not Implemented\n";
  mFileName = filename;

   return true;
}
