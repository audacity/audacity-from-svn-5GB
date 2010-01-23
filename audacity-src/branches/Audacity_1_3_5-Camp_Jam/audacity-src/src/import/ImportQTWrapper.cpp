/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportQTWrapper.cpp

  Joshua Haberman

  This file contains an implementation of some functions that can't be
  included as part of ImportQT.cpp because of incompatible header
  files.

**********************************************************************/

#undef Track
#define Track XTrack

#ifdef __MACH__
   #include <Carbon/Carbon.h>
   #include <QuickTime/QuickTime.h>
#else
   #include <ConditionalMacros.h>
   #include <Movies.h>
   #include <QuickTimeComponents.h>
   #include <Sound.h>
   #include <Folders.h>
   #include <ToolUtils.h>
   #include <Gestalt.h>
   #include <Navigation.h>
#endif

void InitQuicktime()
{
   EnterMovies();
}

void ExitQuicktime()
{
   ExitMovies();
}

Media GetMediaFromMovie(Movie mov)
{
   Track theTrack;

   // get the first sound track
   theTrack = GetMovieIndTrackType(mov, 1,
				   SoundMediaType, movieTrackMediaType);
   if(theTrack == NULL)
      return NULL;

   return GetTrackMedia(theTrack);
}

// Indentation settings for Vim and Emacs.
// Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

