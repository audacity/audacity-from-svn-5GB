/**********************************************************************

  Audacity: A Digital Audio Editor

  Wahwah.cpp

  Nasca Octavian Paul   <paulnasca@email.ro> or <paulnasca@yahoo.com>

**********************************************************************/

#include <math.h>

//#include <wx/generic/textdlgg.h>

#include "Wahwah.h"
#include "../WaveTrack.h"

//
// EffectWahwah
//

#define lfoskipsamples 30

EffectWahwah::EffectWahwah()
{
  freq=1.5;
  startphase=0;
  depth=0.7;
  freqofs=0.3;
  res=2.5;
}

bool EffectWahwah::Begin(wxWindow *parent)
{
  return true;
}

bool EffectWahwah::DoIt(WaveTrack *t,
						   sampleCount start,
						   sampleCount len)
{
  float samplerate = (float)(t->rate);
  
  /*Wahwah initialisation */
  float lfoskip=freq*2*3.141592653589/samplerate;
  unsigned long skipcount=0;
  float xn1=0,xn2=0,yn1=0,yn2=0;
  float frequency,omega,sn,cs,alpha,b0,b1,b2,a0,a1,a2;
  float in,out;

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
	  
	  if ((skipcount++)%lfoskipsamples==0){
		frequency=(1+cos(skipcount*lfoskip+startphase))/2;
		frequency=frequency*depth*(1-freqofs)+freqofs;
		frequency=exp((frequency-1)*6);
		omega=3.141592653589*frequency;
		sn=sin(omega);
		cs=cos(omega);
		alpha=sn/(2*res);
		b0=(1-cs)/2;
		b1=1-cs;
		b2=(1-cs)/2;
		a0=1+alpha;
		a1=-2*cs;
		a2=1-alpha;
	  };
	  out=(b0*in+b1*xn1+b2*xn2-a1*yn1-a2*yn2)/a0;
	  xn2=xn1;
	  xn1=in;
	  yn2=yn1;
	  yn1=out;
	  
	  // Prevents clipping
	  if (out<-32768)
		out=-32768;
	  else if (out>32767)
		out=32767;
	  
      buffer[i] = (sampleType)out;
	}
	
    t->Set(buffer, s, block);
    
    len -= block;
    s += block;
  }
  
  delete[] buffer;

  return true;
}

