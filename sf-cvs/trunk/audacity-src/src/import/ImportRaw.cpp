/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportRaw.cpp

  Dominic Mazzoni

**********************************************************************/

#include <math.h>

#include <wx/button.h>
#include <wx/dc.h>
#include <wx/dcmemory.h>
#include <wx/file.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/thread.h>
#include <wx/timer.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/intl.h>

#include "Import.h"
#include "ImportRaw.h"

#include "../WaveTrack.h"
#include "../DirManager.h"

#include "../FFT.h"

// bits16, signed, stereo, bigendian

#define MODE_8_SIGNED 0,1,0,0
#define MODE_8_UNSIGNED 0,0,0,0
#define MODE_8_SIGNED_STEREO 0,1,1,0
#define MODE_8_UNSIGNED_STEREO 0,0,1,0
#define MODE_16_SIGNED_BE 1,1,0,1
#define MODE_16_UNSIGNED_BE 1,0,0,1
#define MODE_16_SIGNED_STEREO_BE 1,1,1,1
#define MODE_16_UNSIGNED_STEREO_BE 1,0,1,1
#define MODE_16_SIGNED_LE 1,1,0,0
#define MODE_16_UNSIGNED_LE 1,0,0,0
#define MODE_16_SIGNED_STEREO_LE 1,1,1,0
#define MODE_16_UNSIGNED_STEREO_LE 1,0,1,0

float AmpStat(float *data, int len)
{
   int i;

   if (len == 0)
      return 1.0;

   // Calculate standard deviation of the amplitudes

   float sum = 0.0;
   float sumofsquares = 0.0;

   for (i = 0; i < len; i++) {
      float x = fabs(data[i]);
      sum += x;
      sumofsquares += x * x;
   }

   float avg = sum / len;
   float variance = sumofsquares / len - (avg * avg);

   float dev = sqrt(variance);

   return dev;
}

float JumpStat(float *data, int len)
{
   int i;

   // Calculate 1.0 - avg jump
   // A score near 1.0 means avg jump is pretty small

   float avg = 0.0;
   for (i = 0; i < len - 1; i++)
      avg += fabs(data[i + 1] - data[i]);
   avg = 1.0 - (avg / (len - 1) / 2.0);

   return avg;
}

float RedundantStereo(float *data, int len)
{
   int i;
   int c = 0;

   for (i = 1; i < len - 1; i += 2)
      if (fabs(data[i + 1] - data[i]) > fabs(data[i] - data[i - 1]))
         c++;

   return ((c * 2.0) / (len - 2));
}

float PredictStat(float *data, int len)
{
   int i;

   // Calculate 1.0 - avg distance between a sample and the
   // midpoint of its two neighboring samples
   // A score near 1.0 means the neighbors are good predictors

   float avg = 0.0;
   for (i = 1; i < len - 1; i++)
      avg += fabs(data[i] - (data[i - 1] + data[i - 2]) / 2);
   avg = 1.0 - (avg / (len - 1) / 2.0);

   return avg;
}

float FreqStat(float *data, int len)
{
   int i;

   // Calculate fft bins

   float *out = new float[len];

   PowerSpectrum(len, data, out);
   float max = 0.0;
   for (i = 0; i < len / 2; i++) {
      if (out[i] > max)
         max = out[i];
   }

   float freq = 0.0;
   for (i = 0; i < len / 2; i++) {
      freq += (out[i] / max) * (i / (len / 2));
   }

   delete[]out;

   return freq;
}

void Extract(bool bits16,
             bool sign,
             bool stereo,
             bool bigendian,
             bool offset,
             char *rawData, int dataSize,
             float *data1, float *data2, int *len1, int *len2)
{
   *len1 = 0;
   *len2 = 0;

   if (offset && bits16) {
      rawData++;
      dataSize--;
   }

   int rawCount = 0;
   int dataCount1 = 0;
   int dataCount2 = 0;

   if (!bits16 && sign && !stereo)
      while (rawCount < dataSize) {
         // 8-bit signed
         data1[dataCount1++] =
             (*(signed char *) (&rawData[rawCount++])) / 128.0;
      }
   if (!bits16 && !sign && !stereo)
      while (rawCount < dataSize) {
         // 8-bit unsigned
         data1[dataCount1++] =
             (*(unsigned char *) &rawData[rawCount++]) / 128.0 - 1.0;
      }
   if (!bits16 && sign && stereo)
      while (rawCount + 1 < dataSize) {
         // 8-bit signed stereo
         data1[dataCount1++] =
             (*(signed char *) &rawData[rawCount++]) / 128.0;
         data2[dataCount2++] =
             (*(signed char *) &rawData[rawCount++]) / 128.0;
      }
   if (!bits16 && !sign && stereo)
      while (rawCount + 1 < dataSize) {
         // 8-bit unsigned stereo
         data1[dataCount1++] =
             (*(unsigned char *) &rawData[rawCount++]) / 128.0 - 1.0;
         data2[dataCount2++] =
             (*(unsigned char *) &rawData[rawCount++]) / 128.0 - 1.0;
      }
   if (bits16 && sign && !stereo && bigendian)
      while (rawCount + 1 < dataSize) {
         // 16-bit signed BE
         data1[dataCount1++] =
             wxINT16_SWAP_ON_LE(*
                                ((signed short *) &rawData[rawCount += 2]))
             / 32768.0;
      }
   if (bits16 && !sign && !stereo && bigendian)
      while (rawCount + 1 < dataSize) {
         // 16-bit unsigned BE
         data1[dataCount1++] = wxUINT16_SWAP_ON_LE(*((unsigned short *)
                                                     &rawData[rawCount +=
                                                              2]))
             / 32768.0 - 1.0;
      }
   if (bits16 && sign && stereo && bigendian)
      while (rawCount + 3 < dataSize) {
         // 16-bit signed stereo BE
         data1[dataCount1++] =
             wxINT16_SWAP_ON_LE(*
                                ((signed short *) &rawData[rawCount += 2]))
             / 32768.0;
         data2[dataCount2++] =
             wxINT16_SWAP_ON_LE(*
                                ((signed short *) &rawData[rawCount += 2]))
             / 32768.0;
      }
   if (bits16 && sign && stereo && bigendian)
      while (rawCount + 3 < dataSize) {
         // 16-bit unsigned stereo BE
         data1[dataCount1++] = wxUINT16_SWAP_ON_LE(*((unsigned short *)
                                                     &rawData[rawCount +=
                                                              2]))
             / 32768.0 - 1.0;
         data2[dataCount2++] = wxUINT16_SWAP_ON_LE(*((unsigned short *)
                                                     &rawData[rawCount +=
                                                              2]))
             / 32768.0 - 1.0;
      }
   if (bits16 && sign && !stereo && !bigendian)
      while (rawCount + 1 < dataSize) {
         // 16-bit signed LE
         data1[dataCount1++] =
             wxINT16_SWAP_ON_BE(*
                                ((signed short *) &rawData[rawCount += 2]))
             / 32768.0;
      }
   if (bits16 && !sign && !stereo && !bigendian)
      while (rawCount + 1 < dataSize) {
         // 16-bit unsigned LE
         data1[dataCount1++] = wxUINT16_SWAP_ON_BE(*((unsigned short *)
                                                     &rawData[rawCount +=
                                                              2]))
             / 32768.0 - 1.0;
      }
   if (bits16 && sign && stereo && !bigendian)
      while (rawCount + 3 < dataSize) {
         // 16-bit signed stereo LE
         data1[dataCount1++] =
             wxINT16_SWAP_ON_BE(*
                                ((signed short *) &rawData[rawCount += 2]))
             / 32768.0;
         data2[dataCount2++] =
             wxINT16_SWAP_ON_BE(*
                                ((signed short *) &rawData[rawCount += 2]))
             / 32768.0;
      }
   if (bits16 && !sign && stereo && !bigendian)
      while (rawCount + 3 < dataSize) {
         // 16-bit unsigned stereo LE
         data1[dataCount1++] = wxUINT16_SWAP_ON_BE(*((unsigned short *)
                                                     &rawData[rawCount +=
                                                              2]))
             / 32768.0 - 1.0;
         data2[dataCount2++] = wxUINT16_SWAP_ON_BE(*((unsigned short *)
                                                     &rawData[rawCount +=
                                                              2]))
             / 32768.0 - 1.0;
      }

   *len1 = dataCount1;
   *len2 = dataCount2;
}

bool GuessPCMFormat(wxString fName,
                    bool & guess16bit,
                    bool & guessSigned,
                    bool & guessStereo,
                    bool & guessBigEndian,
                    bool & guessOffset,
                    char **sampleData, int *sampleDataLen)
{
   guess16bit = false;
   guessSigned = false;
   guessStereo = false;
   guessBigEndian = false;
   guessOffset = false;

#ifdef IMPORT_DEBUG
   FILE *af = fopen("raw.txt", "a");
   fprintf(af, "File: %s\n", (const char *) fName);
#endif

   wxFile inf;

   inf.Open(fName, wxFile::read);

   if (!inf.IsOpened()) {
      wxMessageBox( _("Could not open file: ") + fName);
      return false;
   }

   const int headerSkipSize = 8192;
   const int dataSize = 8192;
   const int numTests = 11;

   inf.Seek(0, wxFromEnd);
   int fileLen = inf.Tell();

   int test;

   fileLen -= headerSkipSize;   // skip header

   if (fileLen < dataSize) {
      wxMessageBox(_("File not large enough to analyze."));
      return false;
   }

   char *rawData[numTests];
   for (test = 0; test < numTests; test++) {
      rawData[test] = new char[dataSize + 4];
      wxASSERT(rawData[test]);

      int startPoint = (fileLen - dataSize) * (test + 1) / (numTests + 2);

      inf.Seek(headerSkipSize + startPoint, wxFromStart);
      int actual = inf.Read((void *) rawData[test], dataSize);
      if (actual != dataSize) {
         wxString message;
         message.Printf(_("Expected %d bytes, got %d bytes."), dataSize,
                        actual);
         wxMessageBox(message);
         return false;
      }
   }

   inf.Close();

   bool evenMSB[numTests];

   char *rawData2 = new char[dataSize + 4];
   float *data1 = new float[dataSize + 4];
   float *data2 = new float[dataSize + 4];
   int len1;
   int len2;

   //
   // First test: we attempt to determine if the data is 8-bit or 16-bit.
   // We extract the odd and even bytes interpreted as signed-valued samples,
   // and compare their amplitude distributions.  Noting that in 16-bit values,
   // the less significant 8 bits should have roughly flat distribution, while
   // the more significant 8 bits should have a tighter distribution, with a
   // smaller standard deviation.
   //
   // Note that this correctly makes the distinction whether we are dealing with
   // mono or stereo data.
   //
   // It is important that we run this test on multiple sections of the file,
   // since some parts of the file might contain non-audio data, and also that
   // we do not assume that the byte order is consistent throughout the file
   // (because a 16-bit file might have odd-length blocks in the middle of the
   // file).
   // 

   int vote8 = 0;
   int vote16 = 0;

   for (test = 0; test < numTests; test++) {
      Extract(MODE_8_SIGNED_STEREO, false, rawData[test], dataSize,
              data1, data2, &len1, &len2);
      float even = AmpStat(data1, len1);
      float odd = AmpStat(data2, len2);
      if ((even > 0.15) && (odd > 0.15)) {
#ifdef IMPORT_DEBUG
         fprintf(af, "Both appear random.\n");
#endif
      }
      if ((even > 0.15) || (odd > 0.15)) {
         vote16++;

         // Record which of the two was the MSB for future reference
         evenMSB[test] = (even < odd);
      } else
         vote8++;

   }

   if (vote8 > vote16)
      guess16bit = false;
   else
      guess16bit = true;

   if (!guess16bit) {
      // 8-bit
#ifdef IMPORT_DEBUG
      fprintf(af, "8-bit\n");
#endif

      //
      // Next we compare signed to unsigned, interpreted as if the file were
      // stereo just to be safe.  If the file is actually mono, the test
      // still works, and we lose a tiny bit of accuracy.  (It would not make
      // sense to assume the file is mono, because if the two tracks are not
      // very similar we would get inaccurate results.)
      //
      // The JumpTest measures the average jump between two successive samples
      // and returns a value 0-1.  0 is maximally discontinuous, 1 is smooth.
      // 

      int signvotes = 0;
      int unsignvotes = 0;

      for (test = 0; test < numTests; test++) {
         Extract(MODE_8_SIGNED_STEREO, false, rawData[test], dataSize,
                 data1, data2, &len1, &len2);
         float signL = JumpStat(data1, len1);
         float signR = JumpStat(data2, len2);
         Extract(MODE_8_UNSIGNED_STEREO, false, rawData[test], dataSize,
                 data1, data2, &len1, &len2);
         float unsignL = JumpStat(data1, len1);
         float unsignR = JumpStat(data2, len2);

         if (signL > unsignL)
            signvotes++;
         else
            unsignvotes++;

         if (signR > unsignR)
            signvotes++;
         else
            unsignvotes++;
      }

      if (signvotes > unsignvotes)
         guessSigned = true;
      else
         guessSigned = false;

#ifdef IMPORT_DEBUG
      if (guessSigned)
         fprintf(af, "signed\n");
      else
         fprintf(af, "unsigned\n");
#endif

      // Finally we test stereo/mono.  We use the same JumpStat, and say
      // that the file is stereo if and only if for the majority of the
      // tests, the left channel and the right channel are more smooth than
      // the entire stream interpreted as one channel.

      int stereoVotes = 0;
      int monoVotes = 0;

      for (test = 0; test < numTests; test++) {
         Extract(0, guessSigned, 1, 0, 0, rawData[test], dataSize, data1,
                 data2, &len1, &len2);
         float leftChannel = JumpStat(data1, len1);
         float rightChannel = JumpStat(data2, len2);
         Extract(0, guessSigned, 0, 0, 0, rawData[test], dataSize, data1,
                 data2, &len1, &len2);
         float combinedChannel = JumpStat(data1, len1);

         if (leftChannel > combinedChannel
             && rightChannel > combinedChannel)
            stereoVotes++;
         else
            monoVotes++;
      }

      if (stereoVotes > monoVotes)
         guessStereo = true;
      else
         guessStereo = false;

      if (guessStereo == false) {

         // test for repeated-byte, redundant stereo

         int rstereoVotes = 0;
         int rmonoVotes = 0;

         for (test = 0; test < numTests; test++) {
            Extract(0, guessSigned, 0, 0, 0, rawData[test], dataSize,
                    data1, data2, &len1, &len2);
            float redundant = RedundantStereo(data1, len1);

            if (redundant > 0.9 || redundant < 0.1)
               rstereoVotes++;
            else
               rmonoVotes++;
         }

         if (rstereoVotes > rmonoVotes)
            guessStereo = true;

      }
#ifdef IMPORT_DEBUG
      if (guessStereo)
         fprintf(af, "stereo\n");
      else
         fprintf(af, "mono\n");
#endif

   } else {
      // 16-bit
#ifdef IMPORT_DEBUG
      fprintf(af, "16-bit\n");
#endif

      // 
      // Do the signed/unsigned test by using only the MSB.
      //

      int signvotes = 0;
      int unsignvotes = 0;

      for (test = 0; test < numTests; test++) {

         // Extract a new array of the MSBs only:

         for (int i = 0; i < dataSize / 2; i++)
            rawData2[i] = rawData[test][2 * i + (evenMSB ? 0 : 1)];

         // Test signed/unsigned of the MSB

         Extract(MODE_8_SIGNED_STEREO, 0,
                 rawData2, dataSize / 2, data1, data2, &len1, &len2);
         float signL = JumpStat(data1, len1);
         float signR = JumpStat(data2, len2);
         Extract(MODE_8_UNSIGNED_STEREO, 0,
                 rawData2, dataSize / 2, data1, data2, &len1, &len2);
         float unsignL = JumpStat(data1, len1);
         float unsignR = JumpStat(data2, len2);

         if (signL > unsignL)
            signvotes++;
         else
            unsignvotes++;

         if (signR > unsignR)
            signvotes++;
         else
            unsignvotes++;
      }

      if (signvotes > unsignvotes)
         guessSigned = true;
      else
         guessSigned = false;

#ifdef IMPORT_DEBUG
      if (guessSigned)
         fprintf(af, "signed\n");
      else
         fprintf(af, "unsigned\n");
#endif

      //
      // Test mono/stereo using only the MSB
      //

      int stereoVotes = 0;
      int monoVotes = 0;

      for (test = 0; test < numTests; test++) {

         // Extract a new array of the MSBs only:

         for (int i = 0; i < dataSize / 2; i++)
            rawData2[i] = rawData[test][2 * i + (evenMSB ? 0 : 1)];

         Extract(0, guessSigned, 1, 0, 0,
                 rawData2, dataSize / 2, data1, data2, &len1, &len2);
         float leftChannel = JumpStat(data1, len1);
         float rightChannel = JumpStat(data2, len2);
         Extract(0, guessSigned, 0, 0, 0,
                 rawData2, dataSize / 2, data1, data2, &len1, &len2);
         float combinedChannel = JumpStat(data1, len1);

         if (leftChannel > combinedChannel
             && rightChannel > combinedChannel)
            stereoVotes++;
         else
            monoVotes++;
      }

      if (stereoVotes > monoVotes)
         guessStereo = true;
      else
         guessStereo = false;

      if (guessStereo == false) {

         // Test for repeated-byte, redundant stereo

         int rstereoVotes = 0;
         int rmonoVotes = 0;

         for (test = 0; test < numTests; test++) {

            // Extract a new array of the MSBs only:

            for (int i = 0; i < dataSize / 2; i++)
               rawData2[i] = rawData[test][2 * i + (evenMSB ? 0 : 1)];

            Extract(0, guessSigned, 0, 0, 0, rawData[test], dataSize / 2,
                    data1, data2, &len1, &len2);
            float redundant = RedundantStereo(data1, len1);

            if (redundant > 0.9 || redundant < 0.1)
               rstereoVotes++;
            else
               rmonoVotes++;
         }

         if (rstereoVotes > rmonoVotes)
            guessStereo = true;

      }
#ifdef IMPORT_DEBUG
      if (guessStereo)
         fprintf(af, "stereo\n");
      else
         fprintf(af, "mono\n");
#endif

      //
      // Finally, determine the endianness and offset.
      // 
      // Odd MSB -> BigEndian or LittleEndian with Offset
      // Even MSB -> LittleEndian or BigEndian with Offset
      //

      guessBigEndian = (!evenMSB);
      guessOffset = 0;

      int formerVotes = 0;
      int latterVotes = 0;

      for (test = 0; test < numTests; test++) {

         Extract(1, guessSigned, guessStereo, guessBigEndian, guessOffset,
                 rawData[test], dataSize, data1, data2, &len1, &len2);
         float former = JumpStat(data1, len1);
         Extract(1, guessSigned, guessStereo, !guessBigEndian,
                 !guessOffset, rawData[test], dataSize, data1, data2,
                 &len1, &len2);
         float latter = JumpStat(data1, len1);

         if (former > latter)
            formerVotes++;
         else
            latterVotes++;
      }

      if (latterVotes > formerVotes) {
         guessBigEndian = !guessBigEndian;
         guessOffset = !guessOffset;
      }
#ifdef IMPORT_DEBUG
      if (guessBigEndian)
         fprintf(af, "big endian\n");
      else
         fprintf(af, "little endian\n");
#endif

#ifdef IMPORT_DEBUG
      if (guessOffset)
         fprintf(af, "offset 1 byte\n");
      else
         fprintf(af, "no byte offset\n");
#endif

   }

   if (sampleData) {
      *sampleData = new char[dataSize];
      *sampleDataLen = dataSize;
      for (int i = 0; i < dataSize; i++)
         (*sampleData)[i] = rawData[numTests / 2][i];
   }

   for (test = 0; test < numTests; test++) {
      delete[]rawData[test];
   }

   delete[]data1;
   delete[]data2;
   delete[]rawData2;

#ifdef IMPORT_DEBUG
   fprintf(af, "\n");
   fclose(af);
#endif

   return true;
}

bool ImportRaw(wxWindow * parent,
               wxString fName,
               WaveTrack ** dest1, WaveTrack ** dest2,
               DirManager * dirManager)
{
   *dest1 = 0;
   *dest2 = 0;

   bool b16 = true;
   bool sign = true;
   bool stereo = false;
   bool big = false;
   bool offset = false;

   char *data;
   int dataLen;

   bool success =
       GuessPCMFormat(fName, b16, sign, stereo, big, offset, &data,
                      &dataLen);

   if (!success) {

      return false;
   }

   ImportDialog dlg(data, dataLen, parent);

   dlg.bits[b16]->SetValue(true);
   dlg.sign[!sign]->SetValue(true);
   dlg.stereo[stereo]->SetValue(true);
   dlg.endian[big]->SetValue(true);
   dlg.offset[offset]->SetValue(true);
   dlg.RefreshPreview();

   dlg.ShowModal();
   if (!dlg.GetReturnCode())
      return false;

   b16    = dlg.bits[1]->GetValue();
   sign   = dlg.sign[0]->GetValue();
   stereo = dlg.stereo[1]->GetValue();
   big    = dlg.endian[1]->GetValue();
   offset = dlg.offset[1]->GetValue();

   *dest1 = new WaveTrack(dirManager);
   (*dest1)->SetName(TrackNameFromFileName(fName));
   (*dest1)->SetChannel(VTrack::MonoChannel);
   wxASSERT(*dest1);
   if (stereo) {
      *dest2 = new WaveTrack(dirManager);
      wxASSERT(*dest2);
      (*dest2)->SetName(TrackNameFromFileName(fName));
      (*dest1)->SetChannel(VTrack::LeftChannel);
      (*dest2)->SetChannel(VTrack::RightChannel);
      (*dest1)->SetLinked(true);
   }

   wxProgressDialog *progress = NULL;
   wxYield();
   wxStartTimer();
   wxBusyCursor busy;

   bool cancelling = false;

   wxFile inf;
   inf.Open(fName, wxFile::read);
   if (!inf.IsOpened()) {
      wxMessageBox( _("Could not open file: ") + fName);
      return false;
   }
   int len = inf.Length();

   if (offset) {
      char junk;
      inf.Read(&junk, 1);
      offset = false;
      len--;
   }

   int blockSize = 1048576;
   int bytescompleted = 0;

   char *buffer = new char[blockSize];
   float *data1 = new float[blockSize];
   float *data2 = new float[blockSize];
   short *samples1 = new short[blockSize];
   short *samples2 = new short[blockSize];
   wxASSERT(buffer);
   wxASSERT(samples1);
   wxASSERT(samples2);
   int numBytes = len;
   while (numBytes && !cancelling) {
      int block = (numBytes < blockSize ? numBytes : blockSize);
      int actual = inf.Read((void *) buffer, block);
      int len1, len2, i;

      Extract(b16, sign, stereo, big, offset,
              buffer, block, data1, data2, &len1, &len2);

      for (i = 0; i < len1; i++) {
         samples1[i] = (short) (data1[i] * 32767);
      }
      (*dest1)->Append((samplePtr)samples1, int16Sample, len1);

      if (stereo) {
         for (i = 0; i < len2; i++) {
            samples2[i] = (short) (data2[i] * 32767);
         }
         (*dest2)->Append((samplePtr)samples2, int16Sample, len2);
      }

      numBytes -= actual;
      bytescompleted += actual;

      if (!progress && wxGetElapsedTime(false) > 500) {
         progress =
             new wxProgressDialog("Import",
                                  "Importing raw audio data",
                                  1000,
                                  parent,
                                  wxPD_CAN_ABORT |
                                  wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
      }

      if (progress) {
         int progressvalue = (bytescompleted > len) ? len : bytescompleted;
         cancelling = !progress->Update(progressvalue * 1000.0 / len);
      }

   }
   delete[]buffer;
   delete[]data1;
   delete[]data2;
   delete[]samples1;
   delete[]samples2;

   if (progress)
      delete progress;

   return true;
}

const int PREV_RADIO_ID = 9000;

BEGIN_EVENT_TABLE(ImportDialog, wxDialog)
    EVT_BUTTON(wxID_OK, ImportDialog::OnOK)
    EVT_BUTTON(wxID_CANCEL, ImportDialog::OnCancel)
    EVT_RADIOBUTTON(PREV_RADIO_ID, ImportDialog::RadioButtonPushed)
END_EVENT_TABLE()

IMPLEMENT_CLASS(ImportDialog, wxDialog)

ImportDialog::ImportDialog(char *data,
                           int dataLen,
                           wxWindow * parent, const wxPoint & pos)
:wxDialog(parent, -1, _("Import Raw Data"), pos,
          wxDefaultSize, wxDEFAULT_DIALOG_STYLE)
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *topSizer = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer *bottomSizer = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer *leftSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *rightSizer = new wxBoxSizer(wxVERTICAL);

   bits[0] =
       new wxRadioButton(this, PREV_RADIO_ID, _("8-bit"), wxDefaultPosition,
                         wxDefaultSize, wxRB_GROUP);
   bits[1] = new wxRadioButton(this, PREV_RADIO_ID, _("16-bit"));

   sign[0] =
       new wxRadioButton(this, PREV_RADIO_ID, _("signed"), wxDefaultPosition,
                         wxDefaultSize, wxRB_GROUP);
   sign[1] = new wxRadioButton(this, PREV_RADIO_ID, _("unsigned"));

   stereo[0] =
       new wxRadioButton(this, PREV_RADIO_ID, _("mono"), wxDefaultPosition,
                         wxDefaultSize, wxRB_GROUP);
   stereo[1] = new wxRadioButton(this, PREV_RADIO_ID, _("stereo"));

   endian[0] =
       new wxRadioButton(this, PREV_RADIO_ID, _("little-endian"),
                         wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
   endian[1] = new wxRadioButton(this, PREV_RADIO_ID, _("big-endian"));

   offset[0] =
       new wxRadioButton(this, PREV_RADIO_ID, _("0-byte offset"),
                         wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
   offset[1] = new wxRadioButton(this, PREV_RADIO_ID, _("1-byte offset"));

   wxButton *ok = new wxButton(this, wxID_OK, _("Import"));
   wxButton *cancel = new wxButton(this, wxID_CANCEL, _("Cancel"));

   preview = new PreviewPanel(data, dataLen,
                              this, wxDefaultPosition,
                              wxSize(400, 180), 0);

   leftSizer->Add(bits[0], 0, wxLEFT);
   rightSizer->Add(bits[1], 0, wxLEFT);

   leftSizer->Add(sign[0], 0, wxLEFT);
   rightSizer->Add(sign[1], 0, wxLEFT);

   leftSizer->Add(stereo[0], 0, wxLEFT);
   rightSizer->Add(stereo[1], 0, wxLEFT);

   leftSizer->Add(endian[0], 0, wxLEFT);
   rightSizer->Add(endian[1], 0, wxLEFT);

   leftSizer->Add(offset[0], 0, wxLEFT);
   rightSizer->Add(offset[1], 0, wxLEFT);

   topSizer->Add(leftSizer, 0, wxCENTER);
   topSizer->Add(rightSizer, 0, wxCENTER);
   topSizer->Add(preview, 0, wxEXPAND | wxALL, 8);

   bottomSizer->Add(ok, 0, wxCENTER);
   bottomSizer->Add(cancel, 0, wxCENTER);

   mainSizer->Add(topSizer, 0, wxCENTER);
   mainSizer->Add(bottomSizer, 0, wxCENTER | wxALL, 8);

   SetAutoLayout(true);
   SetSizer(mainSizer);

   mainSizer->SetSizeHints(this);
   mainSizer->Fit(this);

   Centre(wxBOTH | wxCENTER_FRAME);
}

ImportDialog::~ImportDialog()
{
}

void ImportDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   EndModal(true);
}

void ImportDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal(false);
}

BEGIN_EVENT_TABLE(PreviewPanel, wxPanel)
    EVT_PAINT(PreviewPanel::OnPaint)
    EVT_ERASE_BACKGROUND(PreviewPanel::OnEraseBackground)
END_EVENT_TABLE()

void PreviewPanel::OnEraseBackground(wxEraseEvent & ignore)
{
}

void PreviewPanel::OnPaint(wxPaintEvent & event)
{
   wxPaintDC dc(this);

   wxRect r;
   r.x = 0;
   r.y = 0;
   GetSize(&r.width, &r.height);
   if (r.width != bitWidth || r.height != bitHeight || !bitmap) {
      bitWidth = r.width;
      bitHeight = r.height;

      if (bitmap)
         delete bitmap;

      bitmap = new wxBitmap(r.width, r.height);
   }

   wxMemoryDC memDC;

   memDC.SelectObject(*bitmap);

   memDC.SetBrush(wxBrush(wxColour(153, 153, 255), wxSOLID));
   memDC.DrawRectangle(r);

   memDC.SetPen(wxPen(wxColour(0, 0, 0), 1, wxSOLID));

   Extract(param[0], param[1], param[2], param[3], param[4],
           rawData, dataLen, data1, data2, &len1, &len2);

   if (param[2]) {
      // stereo

      int ctr = r.height / 2;
      memDC.DrawLine(1, ctr, r.width - 1, ctr);
      ctr /= 2;

      int i;
      for (i = 0; i < (r.width - 2) / 2 && i < len1 - 1; i++)
         memDC.DrawLine(i * 2 + 1, ctr - (ctr * data1[i]), i * 2 + 3,
                        ctr - (ctr * data1[i + 1]));

      for (i = 0; i < (r.width - 2) / 2 && i < len1 - 1; i++)
         memDC.DrawLine(i * 2 + 1, 3 * ctr - (ctr * data2[i]), i * 2 + 3,
                        3 * ctr - (ctr * data2[i + 1]));

   } else {
      int ctr = r.height / 2;

      for (int i = 0; i < (r.width - 2) / 2 && i < len1 - 1; i++)
         memDC.DrawLine(i * 2 + 1, ctr - (ctr * data1[i]), i * 2 + 3,
                        ctr - (ctr * data1[i + 1]));
   }

   //  view->DrawTracks(&memDC, &r);

   dc.Blit(0, 0, r.width, r.height, &memDC, 0, 0, wxCOPY, FALSE);
}

void ImportDialog::RadioButtonPushed(wxCommandEvent & event)
{
   RefreshPreview();
}

void ImportDialog::RefreshPreview()
{
   preview->param[0] = bits[1]->GetValue();
   preview->param[1] = sign[0]->GetValue();
   preview->param[2] = stereo[1]->GetValue();
   preview->param[3] = endian[1]->GetValue();
   preview->param[4] = offset[1]->GetValue();

   preview->Refresh(false);
}

PreviewPanel::PreviewPanel(char *rawData, int dataLen, wxWindow * parent,
                           const wxPoint & pos, const wxSize & size,
                           const long style):wxPanel(parent, -1, pos, size,
                                                     style)
{
   this->rawData = rawData;
   this->dataLen = dataLen;

   data1 = new float[dataLen];
   data2 = new float[dataLen];

   bitWidth = bitHeight = 0;
   bitmap = 0;
}

PreviewPanel::~PreviewPanel()
{
   delete data1;
   delete data2;
}
