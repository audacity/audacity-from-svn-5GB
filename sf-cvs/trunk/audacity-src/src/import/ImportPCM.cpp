/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportPCM.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/file.h>
#include <wx/string.h>
#include <wx/thread.h>
#include <wx/timer.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/intl.h>

#include "Import.h"
#include "ImportPCM.h"

#include "../FileFormats.h"
#include "../WaveTrack.h"
#include "../DirManager.h"
#include "../Prefs.h"

#include "sndfile.h"

bool IsPCM(wxString fName)
{
   wxFile testFile;
   testFile.Open(fName);
   if (!testFile.IsOpened())
      return false;
   testFile.Close();

   SF_INFO    info;
   SNDFILE   *fp;

   fp = sf_open_read(fName, &info);

   if (fp) {
      sf_close(fp);
      return true;
   }

   return false;
}


bool ImportPCM(wxWindow * parent,
               wxString fName, 
               WaveTrack ** channels[],
               int *numChannels,
               DirManager * dirManager)
{
   SF_INFO       info;
   SNDFILE      *fp;
   sampleFormat  format;

   fp = sf_open_read(fName, &info);

   if (!fp) {
      char str[1000];
      sf_error_str((SNDFILE *)NULL, str, 1000);
      wxMessageBox(str);

      return false;
   }

   wxString progressStr;
   wxString formatName = sf_header_name(info.format & SF_FORMAT_TYPEMASK);
   progressStr.Printf(_("Importing %s file..."),
                      (const char *)formatName);

   *numChannels = info.channels;
   *channels = new WaveTrack*[*numChannels];

   if (info.pcmbitwidth > 16)
      format = floatSample;
   else
      format = int16Sample;

   int c;
   for(c=0; c<*numChannels; c++) {
      (*channels)[c] = new WaveTrack(dirManager);
      (*channels)[c]->SetSampleFormat(format);
      (*channels)[c]->SetRate(info.samplerate);
      (*channels)[c]->SetName(TrackNameFromFileName(fName));
      (*channels)[c]->SetChannel(VTrack::MonoChannel);
   }

   if (*numChannels == 2) {
      (*channels)[0]->SetChannel(VTrack::LeftChannel);
      (*channels)[1]->SetChannel(VTrack::RightChannel);
      (*channels)[0]->SetLinked(true);
   }

   sampleCount fileTotalFrames = (sampleCount)info.samples;
   sampleCount maxBlockSize = (*channels)[0]->GetMaxBlockSize();

   wxString copyEdit =
       gPrefs->Read("/FileFormats/CopyOrEditUncompressedData", "edit");

   // Fall back to "edit" if it doesn't match anything else
   bool doEdit = true;          
   if (copyEdit.IsSameAs("copy", false))
      doEdit = false;

   if (doEdit) {

      // If this mode has been selected, we form the tracks as
      // aliases to the files we're editing, i.e. ("foo.wav", 12000-18000)
      // instead of actually making fresh copies of the samples.

      wxProgressDialog *progress = NULL;
      wxYield();
      wxStartTimer();
      wxBusyCursor busy;

      bool cancelling = false;

      for (sampleCount i = 0; i < fileTotalFrames; i += maxBlockSize) {
         sampleCount blockLen = maxBlockSize;
         if (i + blockLen > fileTotalFrames)
            blockLen = fileTotalFrames - i;

         for(c=0; c<*numChannels; c++)
            (*channels)[c]->AppendAlias(fName, i, blockLen, c);

         if (!progress && wxGetElapsedTime(false) > 500) {
            progress =
                new wxProgressDialog(_("Import"), progressStr,
                                     1000,
                                     parent,
                                     wxPD_CAN_ABORT |
                                     wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
         }
         if (progress) {
            cancelling = !progress->Update((int)((i*1000.0)/fileTotalFrames));

            if (cancelling)
               i = fileTotalFrames;
         }
      }

      //printf(_("Time elapsed: %d\n"), wxGetElapsedTime());

      if (progress)
         delete progress;

      if (cancelling) {
         for(c=0; c<*numChannels; c++)
            delete (*channels)[c];
         delete[] (*channels);
         *channels = NULL;

         return false;
      }

      return true;
   }

   // Otherwise, we're in the "copy" mode, where we read in the actual
   // samples from the file and store our own local copy of the
   // samples in the tracks.

   samplePtr srcbuffer = NewSamples(maxBlockSize * (*numChannels),
                                    format);
   samplePtr buffer = NewSamples(maxBlockSize, format);

   unsigned long framescompleted = 0;

   wxProgressDialog *progress = NULL;
   wxYield();
   wxStartTimer();
   wxBusyCursor busy;

   bool cancelling = false;

   long block;
   do {
      block = maxBlockSize;

      if (format == int16Sample)
         block = sf_readf_short(fp, (short *)srcbuffer, block);
      else
         block = sf_readf_float(fp, (float *)srcbuffer, block);

      if (block) {
         for(c=0; c<(*numChannels); c++) {

            if (format==int16Sample) {
               if (info.pcmbitwidth == 8) {
                  for(int j=0; j<block; j++)
                     ((short *)buffer)[j] =
                        ((short *)srcbuffer)[(*numChannels)*j+c] << 8;
               }
               else {
                  for(int j=0; j<block; j++)
                     ((short *)buffer)[j] =
                        ((short *)srcbuffer)[(*numChannels)*j+c];
               }
            }
            else
               for(int j=0; j<block; j++)
                  ((float *)buffer)[j] =
                     ((float *)srcbuffer)[(*numChannels)*j+c];

            (*channels)[c]->Append(buffer, format, block);
         }

         framescompleted += block;
      }

      if (!progress && wxGetElapsedTime(false) > 500) {
         progress =
            new wxProgressDialog(_("Import"), progressStr,
                                  1000,
                                  parent,
                                  wxPD_CAN_ABORT |
                                  wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
      }
      if (progress) {
         int progressvalue = (framescompleted > fileTotalFrames) ?
             fileTotalFrames : framescompleted;

         cancelling =
            !progress->Update((int)((progressvalue*1000.0)/fileTotalFrames));

         if (cancelling)
            block = 0;
      }
   } while (block > 0);

   sf_close(fp);

   //printf("Time elapsed: %d\n", wxGetElapsedTime());

   if (progress)
      delete progress;

   DeleteSamples(srcbuffer);
   DeleteSamples(buffer);

   if (cancelling) {
      for(c=0; c<*numChannels; c++)
         delete (*channels)[c];
      delete[] (*channels);
      *channels = NULL;

      return false;
   }

   return true;
}
