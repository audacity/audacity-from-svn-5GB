/**********************************************************************

  Audacity: A Digital Audio Editor

  SimplePairedTwoTrack.h

  Vincent A. Busam
  Dominic Mazzoni

  This abstract class simplifies the implementation of a basic
  two track effect where you want the values of both tracks
  together.  Inherit from it if your effect doesn't just
  modifies a track in place and doesn't care how many samples
  it gets at a time.  Your derived class only needs to implement
  GetEffectName, GetEffectAction, and ProcessSimplePairedTwoTrack.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_SIMPLE_PAIRED_TWO_TRACK__
#define __AUDACITY_EFFECT_SIMPLE_PAIRED_TWO_TRACK__

#include "Effect.h"

class WaveTrack;

// NOTE:  This class should probably be a template.  Then we could do:

// typedef EffectSimplePairedTwoTrack<short int,int16Sample> EffectSimplePairedTwoTrackInt16

//  and similarly for other types.

class EffectSimplePairedTwoTrackInt16:public Effect {

public:
   // It is not usually necessary to override this method
   virtual bool Process();

protected:
   // It is not usually necessary to override this method
   virtual bool Init();

   // It is not usually necessary to override this method
   virtual bool ProcessTwo( int count, WaveTrack *left, WaveTrack *right,
							longSampleCount lstart,
							longSampleCount rstart, sampleCount len);

   // It is not usually necessary to override this method
   void GetSamples(WaveTrack *track, 
	               longSampleCount *start,
				   sampleCount *len);

   // It is not usually necessary to override this method
   void End();

 protected:  

   // Override this method to actually process audio
   virtual bool ProcessSimplePairedTwoTrack(short int *bufferLeft, 
											short int *bufferRight, // may be 0
											sampleCount len) = 0;

   // Other useful information

   sampleCount mnBlockSize;	// 0 if no processing done, thus no buffers allocated

   short int *mLeftBuffer;
   short int *mRightBuffer;

   int mnTracks;			// either 1 or 2, set in Init
};

#endif
