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

#define BRAND_AUDACITY     0
#define BRAND_UMIXIT       1 
#define BRAND_THINKLABS    2
#define BRAND_AUDIOTOUCH   3

//#define AUDACITY_BRANDING BRAND_AUDACITY // standard version
//#define AUDACITY_BRANDING BRAND_UMIXIT
//#define AUDACITY_BRANDING BRAND_THINKLABS
#define AUDACITY_BRANDING BRAND_AUDIOTOUCH

#define AUDACITY_URL wxT("http://audacity.sourceforge.net/")

#if AUDACITY_BRANDING == BRAND_UMIXIT

   #define AUDACITY_BRANDING_BRANDNAME wxT("UmixIt")
   #define AUDACITY_BRANDING_BRANDURL wxT("http://www.umixit.com/")

#elif AUDACITY_BRANDING == BRAND_THINKLABS

   #define AUDACITY_BRANDING_BRANDNAME "Thinklabs"
   #define AUDACITY_BRANDING_BRANDURL wxT("http://Thinklabsmedical.com/")

#elif AUDACITY_BRANDING == BRAND_AUDIOTOUCH

   #define AUDACITY_BRANDING_BRANDNAME wxT("Audacity_Audiotouch")
   #define AUDACITY_BRANDING_BRANDURL wxT("http://www.audiotouch.com.au/")

#endif

#endif
