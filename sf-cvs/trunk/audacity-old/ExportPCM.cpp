/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPCM.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/string.h>
#include <wx/window.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/timer.h>

#include "snd/snd.h"

#include "Audacity.h"
#include "LabelTrack.h"
#include "Mix.h"
#include "Project.h"
#include "Track.h"
#include "WaveTrack.h"

bool ExportPCM(AudacityProject *project,
               wxString format, bool stereo, wxString fName,
               bool selectionOnly, double t0, double t1)
{
   double rate = project->GetRate();
   wxWindow *parent = project;
   TrackList *tracks = project->GetTracks();

   int header = SND_HEAD_NONE;
#ifdef __WXMAC__
   bool trackMarkers = false;
#endif

   if (format == "WAV")
      header = SND_HEAD_WAVE;
   else if (format == "AIFF")
      header = SND_HEAD_AIFF;
   else if (format == "IRCAM")
      header = SND_HEAD_IRCAM;
   else if (format == "AU")
      header = SND_HEAD_NEXT;
#ifdef __WXMAC__
   else if (format == "AIFF with track markers") {
      header = SND_HEAD_AIFF;
      trackMarkers = true;
   }
#endif


   // Use snd library to export file

   snd_node sndfile;
   snd_node sndbuffer;

   sndfile.device = SND_DEVICE_FILE;
   sndfile.write_flag = SND_WRITE;
   strcpy(sndfile.u.file.filename, (const char *) fName);
   sndfile.u.file.file = 0;
   sndfile.u.file.header = header;
   sndfile.u.file.byte_offset = 0;
   sndfile.u.file.end_offset = 0;
   sndfile.u.file.swap = 0;
   sndfile.format.channels = stereo ? 2 : 1;
   sndfile.format.mode = SND_MODE_PCM;  // SND_MODE_FLOAT
   sndfile.format.bits = 16;
   sndfile.format.srate = int (rate + 0.5);

   int err;
   long flags = 0;

   err = snd_open(&sndfile, &flags);
   if (err) {
      wxMessageBox("Could not write to file.");
      return false;
   }

   sndbuffer.device = SND_DEVICE_MEM;
   sndbuffer.write_flag = SND_READ;
   sndbuffer.u.mem.buffer_max = 0;
   sndbuffer.u.mem.buffer = 0;
   sndbuffer.u.mem.buffer_len = 0;
   sndbuffer.u.mem.buffer_pos = 0;
   sndbuffer.format.channels = stereo ? 2 : 1;
   sndbuffer.format.mode = SND_MODE_PCM;        // SND_MODE_FLOAT
   sndbuffer.format.bits = 16;
   sndbuffer.format.srate = int (rate + 0.5);

   double timeStep = 10.0;      // write in blocks of 10 secs

   wxProgressDialog *progress = NULL;
   wxYield();
   wxStartTimer();
   wxBusyCursor busy;
   bool cancelling = false;

   double t = t0;

   while (t < t1 && !cancelling) {

      double deltat = timeStep;
      if (t + deltat > t1)
         deltat = t1 - t;

      sampleCount numSamples = int (deltat * rate + 0.5);

      Mixer *mixer = new Mixer(stereo ? 2 : 1, numSamples, true, rate);
      wxASSERT(mixer);
      mixer->Clear();

      char *buffer = new char[numSamples * 2 * sndbuffer.format.channels];
      wxASSERT(buffer);

      TrackListIterator iter(tracks);
      VTrack *tr = iter.First();
      while (tr) {
         if (tr->GetKind() == VTrack::Wave) {
            if (tr->selected || !selectionOnly) {
               if (tr->channel == VTrack::MonoChannel)
                  mixer->MixMono((WaveTrack *) tr, t, t + deltat);
               if (tr->channel == VTrack::LeftChannel)
                  mixer->MixLeft((WaveTrack *) tr, t, t + deltat);
               if (tr->channel == VTrack::RightChannel)
                  mixer->MixRight((WaveTrack *) tr, t, t + deltat);
            }
         }
         tr = iter.Next();
      }

      sampleType *mixed = mixer->GetBuffer();

      long b2 = snd_convert(&sndfile, buffer,   // to
                            &sndbuffer, mixed, numSamples);     // from

      snd_write(&sndfile, buffer, b2);

      t += deltat;

      if (!progress && wxGetElapsedTime(false) > 500) {

         wxString message;

         if (selectionOnly)
            message =
                wxString::
                Format("Exporting the selected audio as a %s file",
                       (const char *) format);
         else
            message =
                wxString::
                Format("Exporting the entire project as a %s file",
                       (const char *) format);

         progress =
             new wxProgressDialog("Export",
                                  message,
                                  1000,
                                  parent,
                                  wxPD_CAN_ABORT |
                                  wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
      }
      if (progress) {
         cancelling =
             !progress->Update(int (((t - t0) * 1000) / (t1 - t0) + 0.5));
      }

      delete mixer;
      delete[]buffer;
   }

   snd_close(&sndfile);

#ifdef __WXMAC__

   FSSpec spec;

   wxMacFilename2FSSpec(fName, &spec);

   if (trackMarkers) {
      // Export the label track as "CD Spin Doctor" files

      LabelTrack *labels = NULL;
      TrackListIterator iter(tracks);
      VTrack *t = iter.First();
      while (t && !labels) {
         if (t->GetKind() == VTrack::Label)
            labels = (LabelTrack *) t;
         t = iter.Next();
      }
      if (labels) {
         FSpCreateResFile(&spec, 'AIFF', AUDACITY_CREATOR, 0);
         int resFile = FSpOpenResFile(&spec, fsWrPerm);
         if (resFile == -1) {
            int x = ResError();
         }
         if (resFile != -1) {
            UseResFile(resFile);

            int numLabels = labels->mLabels.Count();
            for (int i = 0; i < numLabels; i++) {
               int startBlock = (int) (labels->mLabels[i]->t * 75);
               int lenBlock;
               if (i < numLabels - 1)
                  lenBlock =
                      (int) ((labels->mLabels[i + 1]->t -
                              labels->mLabels[i]->t) * 75);
               else
                  lenBlock =
                      (int) ((tracks->GetMaxLen() -
                              labels->mLabels[i]->t) * 75);
               int startSample = startBlock * 1176 + 54;
               int lenSample = lenBlock * 1176 + 54;

               Handle theHandle = NewHandle(50);
               HLock(theHandle);
               char *data = (char *) (*theHandle);
               *(int *) &data[0] = startSample;
               *(int *) &data[4] = lenSample;
               *(int *) &data[8] = startBlock;
               *(int *) &data[12] = lenBlock;
               *(short *) &data[16] = i + 1;

               wxString title = labels->mLabels[i]->title;
               if (title.Length() > 31)
                  title = title.Left(31);
               data[18] = title.Length();
               strcpy(&data[19], (const char *) title);

               HUnlock(theHandle);
               AddResource(theHandle, 'SdCv', 128 + i, "\p");
            }
            CloseResFile(resFile);

            wxMessageBox("Saved track information with file.");
         }
      }
   }

   FInfo finfo;
   if (FSpGetFInfo(&spec, &finfo) == noErr) {
      switch (header) {
      case SND_HEAD_AIFF:
         finfo.fdType = 'AIFF';
         break;
      case SND_HEAD_IRCAM:
         finfo.fdType = 'IRCA';
         break;
      case SND_HEAD_NEXT:
         finfo.fdType = 'AU  ';
         break;
      case SND_HEAD_WAVE:
         finfo.fdType = 'WAVE';
         break;
      }

      finfo.fdCreator = AUDACITY_CREATOR;

      FSpSetFInfo(&spec, &finfo);
   }
#endif

   if (progress)
      delete progress;

   return true;

}
