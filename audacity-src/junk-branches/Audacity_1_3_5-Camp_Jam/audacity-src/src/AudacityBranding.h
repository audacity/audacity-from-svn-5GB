/**********************************************************************

   Audacity: A Digital Audio Editor

   AudacityBranding.h

   Vaughan Johnson, March 2007

   Build flags for branding.
   Change values depending on the desired build characteristics.

**********************************************************************/

#pragma once

#if !defined(__AUDACITY_BRANDING_H__)
#define __AUDACITY_BRANDING_H__

// custom version ID's
#define BRAND_AUDACITY        0
#define BRAND_UMIXIT          1  //v Not yet ported past 1.2.6
#define BRAND_THINKLABS       2  //v Not yet ported past 1.2.6
#define BRAND_AUDIOTOUCH      3  //v Not yet ported past 1.2.6
#define BRAND_JAMLING__EASY   4
#define BRAND_JAMLING__FULL   5


// Allow specification from the command line
#if !defined(AUDACITY_BRANDING)

// Uncomment the one you want to build. Recomment the one you don't. 
//#define AUDACITY_BRANDING BRAND_AUDACITY // standard version
//#define AUDACITY_BRANDING BRAND_UMIXIT
//#define AUDACITY_BRANDING BRAND_THINKLABS
//#define AUDACITY_BRANDING BRAND_AUDIOTOUCH
//#define AUDACITY_BRANDING BRAND_JAMLING__EASY
#define AUDACITY_BRANDING BRAND_JAMLING__FULL

#endif

#if ((AUDACITY_BRANDING == BRAND_AUDACITY) || (AUDACITY_BRANDING == BRAND_AUDIOTOUCH))
   #define WANT_BRAND_TOOLBAR 0
   #define WANT_BRANDING_PANEL 0
   #define WANT_MULTICOLOR_TRACKS 0
#elif (AUDACITY_BRANDING == BRAND_UMIXIT)
   #define WANT_BRAND_TOOLBAR 0
   #define WANT_BRANDING_PANEL 1
   #define WANT_MULTICOLOR_TRACKS 1
#elif ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   #define WANT_BRAND_TOOLBAR 1
   #define WANT_BRANDING_PANEL 0
   #define WANT_MULTICOLOR_TRACKS 1
#endif

#define AUDACITY_URL wxT("http://audacity.sourceforge.net/")

#if (AUDACITY_BRANDING == BRAND_UMIXIT)
   #define AUDACITY_BRANDING_BRANDNAME wxT("UmixIt")
   #define AUDACITY_BRANDING_BRANDURL wxT("http://www.umixit.com/")
#elif (AUDACITY_BRANDING == BRAND_THINKLABS)
   #define AUDACITY_BRANDING_BRANDNAME "Thinklabs"
   #define AUDACITY_BRANDING_BRANDURL wxT("http://Thinklabsmedical.com/")
#elif (AUDACITY_BRANDING == BRAND_AUDIOTOUCH)
   #define AUDACITY_BRANDING_BRANDNAME wxT("Audacity_Audiotouch")
   #define AUDACITY_BRANDING_BRANDURL wxT("http://www.audiotouch.com.au/")
#elif ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   #define AUDACITY_BRANDING_BRANDNAME wxT("Jamling Audacity")
   #define AUDACITY_BRANDING_BRANDURL wxT("http://jamling.com/")
#endif

#endif
