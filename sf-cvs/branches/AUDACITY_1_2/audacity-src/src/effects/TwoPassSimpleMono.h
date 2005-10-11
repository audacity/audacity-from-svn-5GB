#include "SimpleMono.h"
class EffectTwoPassSimpleMono:public EffectSimpleMono
{
public:
    virtual bool Process();
    virtual bool InitFirstPass();
    virtual bool InitSecondPass();

};
