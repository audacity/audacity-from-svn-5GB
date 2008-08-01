/*
 * Test PortBurn
 *
 * Dominic Mazzoni
 * License: LGPL
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
//#include <unistd.h>
#include <Windows.h>

#include "portburn.h"

const char *filename = "clip.wav";
const int file_frames = 166;
const int frame_size = 588;

#define swap_uint16(x) \
       ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8))

void FailErr(int result) {
   if (result != 0) {
      printf("Failed: %d\n", result);
      exit(-1);
   }
}

int main(int argc, char **argv) {
   FILE *fp;
   short *buffer = (short *)malloc(file_frames * frame_size * 4);
   void *handle = PortBurn_Open();
   int count = PortBurn_GetNumDevices(handle);
   int i, j;
   unsigned int swaptest;
   int needswap;

   swaptest = 0x01020304;
   if (((unsigned char *)&swaptest)[0] == 1) {
      printf("Big-endian machine, will do byteswapping\n");
      needswap = 1;
   }
   else {
      printf("Little-endian machine, will not do byteswapping\n");
      needswap = 0;
   }

   if (count == 0) {
      printf("No devices found!\n");
      return -1;
   }

   for(i = 0; i < count; i++) {
      char *name = PortBurn_GetDeviceName(handle, i);
      printf("Device %d: '%s'\n", i, name);
      free(name);
   }

   printf("Using the first device\n");

   FailErr(PortBurn_OpenDevice(handle, 0));
//   FailErr(PortBurn_EjectDevice(handle));
//   FailErr(PortBurn_EraseDevice(handle));

   printf("Press enter when the device is ready\n");
   getchar();

   printf("Staging audio\n");
   FailErr(PortBurn_StartStaging(handle, "c:\temp"));

   fp = fopen(filename, "r");
   if (!fp) {
      printf("Couldn't open file: %s\n", filename);
      return -1;
   }
   fseek(fp, 44, SEEK_SET);
   fread(buffer, 2, file_frames * frame_size * 2, fp);
   fclose(fp);

   if (needswap) {
      for(i = 0; i < file_frames * frame_size * 2; i++) {
         ((unsigned short *)buffer)[i] =
            swap_uint16(((unsigned short *)buffer)[i]);
      }
   }

   FailErr(PortBurn_StartTrack(handle, "80 loops", 80 * file_frames));
   for(j = 0; j < 80; j++) {
      for(i = 0; i < file_frames; i++) {
         FailErr(PortBurn_AddFrame(handle, &buffer[i * 2 * frame_size]));
      }
   }
   FailErr(PortBurn_EndTrack(handle));

   FailErr(PortBurn_StartTrack(handle, "100 loops", 100 * file_frames));
   for(j = 0; j < 100; j++) {
      for(i = 0; i < file_frames; i++) {
         FailErr(PortBurn_AddFrame(handle, &buffer[i * 2 * frame_size]));
      }
   }
   FailErr(PortBurn_EndTrack(handle));

   printf("Burning!!!\n");
   FailErr(PortBurn_StartBurning(handle));

   for(;;) {
      float frac;
      int result;

      result = PortBurn_GetStatus(handle, &frac);
      if (result != 0)
         break;

      printf("Status: %.3f\n", frac);

      if (frac == 1.0) {
         printf("Completed!!!\n");
         break;
      }
   }

   printf("Cleaning up\n");
   PortBurn_CloseDevice(handle);
   PortBurn_Close(handle);

   printf("Press enter to end\n");
   getchar();

   return 0;
}
