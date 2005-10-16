/**********************************************************************

  Audacity: A Digital Audio Editor

  TwoPassSimpleMono.h

  Dominic Mazzoni

  This bit by Martyn Shaw.
  This class implements a two pass process by using EffectSimpleMono.
  Inherit from it if your effect needs to pass twice over the data.
  It does the first pass on all selected tracks before going back and
  doing the second pass over all selected tracks.

**********************************************************************/
#include "SimpleMono.h"
class EffectTwoPassSimpleMono:public EffectSimpleMono
{
public:
    virtual bool Process();
    virtual bool InitFirstPass();
    virtual bool InitSecondPass();

};
