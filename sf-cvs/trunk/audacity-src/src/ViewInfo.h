/**********************************************************************

  Audacity: A Digital Audio Editor

  ViewInfo.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_VIEWINFO__
#define __AUDACITY_VIEWINFO__

struct ViewInfo {

   // Current selection (in seconds)

   double sel0;
   double sel1;

   // Scroll info

   int vpos;                    // vertical scroll pos

   double h;                    // h pos in secs
   double screen;               // screen width in secs
   double total;                // total width in secs
   double zoom;                 // pixels per second
   double lastZoom;

   // Actual scroll bar positions, in pixels
   int sbarH;
   int sbarScreen;
   int sbarTotal;

   int scrollStep;

   // Other stuff, mainly states (true or false) related to autoscroll and drawing the waveform
   // Mabye this should be put somewhere else?

   bool bUpdateSpectrogram;
   bool bRedrawWaveform;
   bool bUpdateTrackIndicator;

   bool bIsPlaying;
};

#endif
