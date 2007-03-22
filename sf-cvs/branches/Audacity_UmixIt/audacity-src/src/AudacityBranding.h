/**********************************************************************

   Audacity: A Digital Audio Editor

   Branding.h

   Vaughan Johnson, March 2007

   Build flags for branding.
   Change values depending on the desired build characteristics.

**********************************************************************/

#pragma once

//enum {
//   Brand_Audacity,
//   Brand_UmixIt,
//   Brand_Thinklabs
//} BrandIndex;
#define BRAND_AUDACITY  0
#define BRAND_UMIXIT    1 
#define BRAND_THINKLABS 2

#define AUDACITY_BRANDING BRAND_UMIXIT

//vvvvv OR do it this way:
//#undef AUDACITY_BRANDING_BRANDNAME // standard version

#define AUDACITY_BRANDING_BRANDNAME wxT("UmixIt")
#define AUDACITY_BRANDING_BRANDURL wxT("http://www.umixit.com/")

//#define AUDACITY_BRANDING_BRANDNAME "Thinklabs"
//#define AUDACITY_BRANDING_BRANDURL wxT("http://Thinklabsmedical.com/")

