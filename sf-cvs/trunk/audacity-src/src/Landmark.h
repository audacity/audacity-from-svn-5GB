/**********************************************************************

  Audacity: A Digital Audio Editor

  Landmark.h

  Dominic Mazzoni

  Pitch detector based on "landmark" algorithm by Cooper and Ng,
  University of Leeds SCS, Division of AI.

  \cite{cooper94}

**********************************************************************/

int GetLandmarkPeriod(int numSamples, double *sample);

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 44f8760b-1db0-4f5d-9a21-aa04d664836c

