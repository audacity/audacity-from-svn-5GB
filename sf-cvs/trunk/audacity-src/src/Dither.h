/**********************************************************************

  Audacity: A Digital Audio Editor

  Steve Harris
  Markus Meyer

  This class implements various functions for dithering and is derived
  from the dither code in the Ardour project, written by Steve Harris.

  Dithering is only done if it really is necessary. Otherwise (e.g.
  when the source and destination format of the samples is the same),
  the samples are only copied or converted. However, copied samples
  are always checked for out-of-bounds values and possibly clipped
  accordingly.

  These dither algorithms are currently implemented:
  - No dithering at all
  - Rectangle dithering
  - Triangle dithering
  - Noise-shaped dithering

**********************************************************************/

#ifndef __AUDACITY_DITHER_H__
#define __AUDACITY_DITHER_H__

#include "SampleFormat.h"

/// Dither class. You must construct an instance because it keeps
/// state. Call Dither::Apply() to apply the dither. You can call
/// Reset() between subsequent dithers to reset the dither state
/// and get deterministic behaviour.
class Dither
{
public:
    /// Default constructor
    Dither();
    
    /// These ditherers are currently available:
    enum DitherType { none = 0, rectangle = 1, triangle = 2, shaped = 3};

    /// Reset state of the dither.
    void Reset();

    /// Apply the actual dithering. Expects the source sample in the
    /// 'source' variable, the destination sample in the 'dest' variable,
    /// and hints to the formats of the samples. Even if the sample formats
    /// are the same, samples are clipped, if necessary.
    void Apply(DitherType ditherType,
               const samplePtr source, sampleFormat sourceFormat,
               samplePtr dest, sampleFormat destFormat,
               unsigned int len, unsigned int stride = 1);

private:
    // Dither methods
    float NoDither(float sample);
    float RectangleDither(float sample);
    float TriangleDither(float sample);
    float ShapedDither(float sample);

    // Dither constants
    static const int BUF_SIZE; /* = 8 */
    static const int BUF_MASK; /* = 7 */
    static const float SHAPED_BS[];
    
    // Dither state
    int mPhase;
    float mTriangleState;
    float mBuffer[8 /* = BUF_SIZE */];
};

#endif /* __AUDACITY_DITHER_H__ */

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 0dabf170-119a-4897-afee-d6fe1ac1a8d6

