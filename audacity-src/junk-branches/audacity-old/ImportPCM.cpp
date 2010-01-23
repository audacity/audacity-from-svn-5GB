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

#include "FileFormats.h"
#include "Import.h"
#include "ImportPCM.h"
#include "WaveTrack.h"
#include "DirManager.h"
#include "Prefs.h"

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
   SF_INFO    info;
   SNDFILE   *fp;

   fp = sf_open_read(fName, &info);

   if (!fp) {
      char str[1000];
      sf_error_str((SNDFILE *)NULL, str, 1000);
      wxMessageBox(str);

      return false;
   }

   wxString progressStr;
   wxString formatName;
   for(int z=0; z<gNumPCMFormats; z++)
      if ((info.format & SF_FORMAT_TYPEMASK) == gPCMFormats[z].id)
         formatName = gPCMFormats[z].name;
   progressStr.Printf("Importing %s file...",
                      formatName);

   *numChannels = info.channels;
   *channels = new WaveTrack*[*numChannels];

   int c;
   for(c=0; c<*numChannels; c++) {
      (*channels)[c] = new WaveTrack(dirManager);
      (*channels)[c]->rate = info.samplerate;
      (*channels)[c]->name = TrackNameFromFileName(fName);
      (*channels)[c]->channel = VTrack::MonoChannel;
   }

   if (*numChannels == 2) {
      (*channels)[0]->channel = VTrack::LeftChannel;
      (*channels)[1]->channel = VTrack::RightChannel;
      (*channels)[0]->linked = true;
   }

   sampleCount fileTotalFrames = (sampleCount)info.samples;
   sampleCount maxBlockSize = (sampleCount)WaveTrack::GetIdealBlockSize();

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
                new wxProgressDialog("Import", progressStr,
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

      //printf("Time elapsed: %d\n", wxGetElapsedTime());

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

   sampleType *srcbuffer = new short[maxBlockSize * (*numChannels)];
   sampleType *buffer = new short[maxBlockSize];

   unsigned long framescompleted = 0;

   wxProgressDialog *progress = NULL;
   wxYield();
   wxStartTimer();
   wxBusyCursor busy;

   bool cancelling = false;

   long block;
   do {
      block = maxBlockSize;
      block = sf_readf_short(fp, srcbuffer, block);

      if (block) {
         for(c=0; c<(*numChannels); c++) {
            for(int j=0; j<block; j++)
               buffer[j] = srcbuffer[(*numChannels)*j+c];
            (*channels)[c]->Append(buffer, block);
         }

         framescompleted += block;
      }

      if (!progress && wxGetElapsedTime(false) > 500) {
         progress =
            new wxProgressDialog("Import", progressStr,
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

   delete[] srcbuffer;
   delete[] buffer;

   if (cancelling) {
      for(c=0; c<*numChannels; c++)
         delete (*channels)[c];
      delete[] (*channels);
      *channels = NULL;

      return false;
   }

   return true;
}
