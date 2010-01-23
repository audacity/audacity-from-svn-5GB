/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPCM.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/textctrl.h>
#include <wx/string.h>
#include <wx/window.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/timer.h>
#include <wx/intl.h>

#include "sndfile.h"

#include "../Audacity.h"
#include "../FileFormats.h"
#include "../LabelTrack.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../SampleFormat.h"
#include "../Track.h"
#include "../WaveTrack.h"

#include "PCMExporter.h"
#include "Exporter.h"

#ifdef __WXMAC__
#define __MOVIES__   /* Apple's Movies.h not compatible with Audacity */
/* #define __MACHELP__ */

#include <wx/mac/private.h>
# ifdef __UNIX__
#  include <CoreServices/CoreServices.h>
# else
# endif
#endif


#include <iostream>

PCMExporter::PCMExporter(AudacityProject * project, double t0, double t1,
			 bool exportSelection, int outrate,
			 int channels, int sf_format):
  Exporter(project, t0, t1, exportSelection, outrate, channels),
  mSoundFileFormat(sf_format)
{
  
}

PCMExporter::~PCMExporter()
{

}

bool PCMExporter::Verify()
{
  return true;
}


bool PCMExporter::Export(const wxString & filename)
{

  mFileName = filename;
  //Unless it has been changed, use the project rate
  if(!mOutRate) 
    mOutRate = mProject->GetRate();
  
  wxWindow    *parent = mProject;
  TrackList   *tracks = mProject->GetTracks();

  wxString     formatStr;
  SF_INFO      info;
  SNDFILE     *sf = NULL;
  int          err;

   formatStr = sf_header_name(mSoundFileFormat & SF_FORMAT_TYPEMASK);

   // Use libsndfile to export file

   info.samplerate = mOutRate;
   info.frames = (unsigned int)((m_t1 - m_t0)*(double)mOutRate + 0.5);
   info.channels = mChannels;
   info.format = mSoundFileFormat;
   info.sections = 1;
   info.seekable = 0;

   // If we can't export exactly the format they requested,
   // try the default format for that header type...
   if (!sf_format_check(&info))
      info.format = (info.format & SF_FORMAT_TYPEMASK);
   if (!sf_format_check(&info)) {
      wxMessageBox(_("Cannot export audio in this format."));
      return false;
   }

   sf = sf_open((const char *)mFileName, SFM_WRITE, &info);
   if (!sf) {
      wxMessageBox(wxString::Format(_("Cannot export audio to %s"),
                                    (const char *)mFileName));
      return false;
   }

   //Determine what format the track is currently in, to avoid
   //multiple conversions.
   sampleFormat format;
   if (sf_subtype_more_than_16_bits(info.format))
      format = floatSample;
   else
      format = int16Sample;

   int maxBlockLen = 44100 * 5;

   wxProgressDialog *progress = NULL;
   wxYield();
   wxStartTimer();
   wxBusyCursor busy;
   bool cancelling = false;

   int numWaveTracks;
   WaveTrack **waveTracks;
   tracks->GetWaveTracks(mExportSelection, &numWaveTracks, &waveTracks);



  
   Mixer *mixer = new Mixer(numWaveTracks, waveTracks,
                            tracks->GetTimeTrack(),
                            m_t0, m_t1,
                            info.channels, maxBlockLen, true,
                            mInRate, format);

   while(!cancelling) {
      sampleCount numSamples = mixer->Process(maxBlockLen);

      if (numSamples == 0)
         break;
      
      samplePtr mixed = mixer->GetBuffer();

      if (format == int16Sample)
         sf_writef_short(sf, (short *)mixed, numSamples);
      else
         sf_writef_float(sf, (float *)mixed, numSamples);

      if (!progress && wxGetElapsedTime(false) > 500) {

         wxString message;

         if (mExportSelection)
            message =
                wxString::
                Format(_("Exporting the selected audio as a %s file"),
                       (const char *) formatStr);
         else
            message =
                wxString::
                Format(_("Exporting the entire project as a %s file"),
                       (const char *) formatStr);

         progress =
             new wxProgressDialog(_("Export"),
                                  message,
                                  1000,
                                  parent,
                                  wxPD_CAN_ABORT |
                                  wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
      }
      if (progress) {
         int progressvalue = int (1000 * ((mixer->MixGetCurrentTime()-m_t0) /
                                          (m_t1-m_t0)));
         cancelling = !progress->Update(progressvalue);
      }
   }

   delete mixer;

   delete[] waveTracks;                            

   err = sf_close(sf);

   if (err) {
      char buffer[1000];
      sf_error_str(sf, buffer, 1000);
      wxMessageBox(wxString::Format
                   (_("Error (file may not have been written): %s"),
                    buffer));
   }

#ifdef __WXMAC__

   FSSpec spec;

   wxMacFilename2FSSpec(fName, &spec);

   FInfo finfo;
   if (FSpGetFInfo(&spec, &finfo) == noErr) {
      finfo.fdType = sf_header_mactype(mSoundFileFormat & SF_FORMAT_TYPEMASK);
      finfo.fdCreator = AUDACITY_CREATOR;

      FSpSetFInfo(&spec, &finfo);
   }
#endif

   if (progress)
      delete progress;

   return true;
}
