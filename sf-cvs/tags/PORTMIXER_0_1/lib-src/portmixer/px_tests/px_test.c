#include <stdio.h>

#include "portmixer.h"

int main(int argc, char **argv)
{
   int num_mixers;
   int i;

   num_mixers = Px_GetNumMixers(0);
   printf("Number of mixers: %d\n", num_mixers);
   for(i=0; i<num_mixers; i++) {
      PxMixer *mixer;
      int num;
      int j;

      printf("Mixer %d: %s\n", i, Px_GetMixerName(0, i));
      mixer = Px_OpenMixer(0, i);
      if (!mixer) {
         printf("  Could not open mixer!\n");
         continue;
      }
      
      printf("  Master volume: %.2f\n", Px_GetMasterVolume(mixer));
      printf("  PCM output volume: %.2f\n", Px_GetPCMOutputVolume(mixer));

      num = Px_GetNumOutputVolumes(mixer);
      printf("  Num outputs: %d\n", num);
      for(j=0; j<num; j++) {
         printf("    Output %d (%s): %.2f\n",
                j,
                Px_GetOutputVolumeName(mixer, j),
                Px_GetOutputVolume(mixer, j));
      }

      num = Px_GetNumInputSources(mixer);
      printf("  Num input sources: %d\n", num);
      for(j=0; j<num; j++) {
         printf("    Input %d (%s) %s\n",
                j,
                Px_GetInputSourceName(mixer, j),
                (Px_GetCurrentInputSource(mixer)==j?
                 "SELECTED": ""));
      }
      printf("  Input volume: %.2f\n", Px_GetInputVolume(mixer));

      Px_CloseMixer(mixer);
   }

   return 0;
}
