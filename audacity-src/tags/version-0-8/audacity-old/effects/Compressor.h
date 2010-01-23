/**********************************************************************

  Audacity: A Digital Audio Editor

  FilterFade.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __FILTER_COMPRESSOR__
#define __FILTER_COMPRESSOR__

#include <wx/string.h>

#include "WaveTrack.h"
#include "Filter.h"

class FilterCompressor: public Filter {

private:
	int increment;
	int how_far;
	int max_region;

public:
	virtual bool Prepare(WaveTrack *t, double t0, double t1, sampleCount total);
	virtual wxString GetFilterName() { return wxString("Compressor..."); }
	virtual int MaxBlockLen() {return 65536; }
	virtual void DoIt(sampleType *src, sampleType *dst,
										sampleCount total, sampleCount x, sampleCount len);
};

#endif
#endif
