/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveletDenoise.cpp

  Matt Brubeck

**********************************************************************/

#include <math.h>

#ifdef __WXMSW__
#include <float.h>
#define isfinite(X) _finite(X)
#else
#define isfinite(X) finite(X)
#endif

#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/brush.h>
#include <wx/image.h>
#include <wx/dcmemory.h>
#include <wx/statbox.h>

#include "WaveletDenoise.h"
#include "../Envelope.h"
#include "../FFT.h"
#include "../WaveTrack.h"

EffectWaveletDenoise::EffectWaveletDenoise()
{
    // windowSize = 2048;
}

// Process
//
// Called whenever the effect is applied.  This function is taken directly
// from Dominic's NoiseRemoval effect.  For each WaveTrack it simply calls 
// ProcessOne(), where the real work is done.

bool EffectWaveletDenoise::Process()
{
    TrackListIterator iter(mWaveTracks);
    VTrack *t = iter.First();
    int count = 0;
    while(t) {
        sampleCount start, len;
        GetSamples((WaveTrack *)t, &start, &len);
        
        bool success = ProcessOne(count, (WaveTrack *)t, start, len);
        
        if (!success)
            return false;
    
        t = iter.Next();
        count++;
    }
    return true;
}

bool EffectWaveletDenoise::ProcessOne(int count, WaveTrack * t,
                                        sampleCount start, sampleCount len)
{
    sampleCount s = start;
    sampleCount idealBlockLen = t->GetMaxBlockSize() * 4;
    
    if (idealBlockLen % windowSize != 0)
        idealBlockLen += (windowSize - (idealBlockLen % windowSize));
    
    sampleType *buffer = new sampleType[idealBlockLen];
    
    sampleType *window1 = new sampleType[windowSize];
    sampleType *window2 = new sampleType[windowSize];
    sampleType *thisWindow = window1;
    sampleType *lastWindow = window2;
    
    sampleCount originalLen = len;
    
    int i;
    bool first = true;
    
    for(i=0; i<windowSize; i++) {
        lastWindow[i] = 0;
    }
    
    while(len) {
        int block = idealBlockLen;
        if (block > len)
            block = len;
        
        t->Get(buffer, s, block);
        
        for(i=0; i<block; i+=windowSize/2) {
            int wlen = i + windowSize;
            int wcopy = windowSize;
            if (i + wcopy > block)
                wcopy = block - i;
            
            int j;
            for(j=0; j<wcopy; j++)
                thisWindow[j] = buffer[i+j];
            for(j=wcopy; j<windowSize; j++)
                thisWindow[j] = 0;
            
            //RemoveNoise(windowSize, thisWindow, first);
            first = false;
            for(j=0; j<windowSize/2; j++)
                buffer[i+j] = thisWindow[j] + lastWindow[windowSize/2 + j];
            
            sampleType *tempP = thisWindow;
            thisWindow = lastWindow;
            lastWindow = tempP;
        }
        
        if (len > block && len > windowSize/2)
            block -= windowSize/2;
        
        t->Set(buffer, s, block);
        
        len -= block;
        s += block;
        
        TrackProgress(count, (s-start)/(double)originalLen);
    }
    
    delete[] buffer;
    delete[] window1;
    delete[] window2;
    
    return true;
}
