/**********************************************************************

  Audacity: A Digital Audio Editor

  BassBoost.cpp

  Nasca Octavian Paul   <paulnasca@email.ro> or <paulnasca@yahoo.com>

**********************************************************************/

#include <math.h>

//#include <wx/generic/textdlgg.h>

#include "BassBoost.h"
#include "../WaveTrack.h"

//
// EffectBassBoost
//

EffectBassBoost::EffectBassBoost()
{
  frequency=200;
  dB_boost=12;
}

bool EffectBassBoost::Begin(wxWindow *parent)
{
  return true;
}

bool EffectBassBoost::DoIt(WaveTrack *t,
						   sampleCount start,
						   sampleCount len)
{
  float samplerate = (float)(t->rate);

  /* Compute coefficents of the biquand IIR filter */
  float omega=2*3.141592653589*frequency/samplerate;
  float sn=sin(omega);
  float cs=cos(omega);
  float a=exp(log(10)*dB_boost/40);
  float shape=1.0;/*Low Shelf filter's shape, if this is too large
				or too small it will result an unstable filter */
  float beta=sqrt((a*a+1)/shape-(pow((a-1),2)));
  /*  Coefficients  */
  float b0=a*((a+1)-(a-1)*cs+beta*sn);
  float b1=2*a*((a-1)-(a+1)*cs);
  float b2=a*((a+1)-(a-1)*cs-beta*sn);
  float a0=((a+1)+(a-1)*cs+beta*sn);
  float a1=-2*((a-1)+(a+1)*cs);
  float a2=(a+1)+(a-1)*cs-beta*sn;
  /* initialise the filter */
  float xn1=0,xn2=0,yn1=0,yn2=0;

  float out,in=0;

  sampleCount s = start;
  sampleCount blockSize = t->GetIdealBlockSize();
  
  sampleType *buffer = new sampleType[blockSize];
    
  while(len) {
    int block = blockSize;
    if (block > len)
      block = len;
    
    t->Get(buffer, s, block);

    for(int i=0; i<block; i++) {
	  in=buffer[i];
	  out=(b0*in+b1*xn1+b2*xn2-a1*yn1-a2*yn2)/a0;
	  xn2=xn1;
	  xn1=in;
	  yn2=yn1;
	  yn1=out;
	  
	  if (out<-32768) out=-32768;
	  else if (out>32767) out=32767; //Prevents clipping

      buffer[i] = (sampleType)out;
	}

    t->Set(buffer, s, block);
    
    len -= block;
    s += block;
  }

  delete[] buffer;

  return true;
}

