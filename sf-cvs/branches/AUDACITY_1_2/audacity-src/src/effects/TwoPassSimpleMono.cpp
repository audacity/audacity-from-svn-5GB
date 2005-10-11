#include "TwoPassSimpleMono.h"

bool EffectTwoPassSimpleMono::Process()
{
    InitFirstPass();
    if (!EffectSimpleMono::Process())
        return false;
    InitSecondPass();
    return EffectSimpleMono::Process();
}

bool EffectTwoPassSimpleMono::InitFirstPass()
{
	return true;
}

bool EffectTwoPassSimpleMono::InitSecondPass()
{
	return true;
}

