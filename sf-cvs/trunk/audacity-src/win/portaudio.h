
// Set to 0 for version 1.8 or 1 for version 1.9
#define USE_PORTAUDIO_V19 1

// Don't change anything below here
#if USE_PORTAUDIO_V19

#  include "..\lib-src\portaudio-v19\include\portaudio.h"

#  if defined(__WXMSW__)
#     if DEBUG
#        if wxUSE_UNICODE
#           pragma comment(lib, "pastaticwmmev19ud")
#        else
#           pragma comment(lib, "pastaticwmmev19d")
#        endif
#     else
#        if wxUSE_UNICODE
#           pragma comment(lib, "pastaticwmmev19u")
#        else
#           pragma comment(lib, "pastaticwmmev19")
#        endif
#     endif
#  endif

#else

#   include "..\lib-src\portaudio\pa_common\portaudio.h"

#  if defined(__WXMSW__)
#     if DEBUG
#        if wxUSE_UNICODE
#           pragma comment(lib, "pastaticwmmeud")
#        else
#           pragma comment(lib, "pastaticwmmed")
#        endif
#     else
#        if wxUSE_UNICODE
#           pragma comment(lib, "pastaticwmmeu")
#        else
#           pragma comment(lib, "pastaticwmme")
#        endif
#     endif
#  endif

#endif