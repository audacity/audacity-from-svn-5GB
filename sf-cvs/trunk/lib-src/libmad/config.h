/*
 * this file is specific to Audacity. It acts as a dispatch,
 * #including the correct config.h depending on the platform.
 */

#ifdef WIN32
#  include "configwin32.h"
#else
#  include "configunix.h"   /* This one must be automatically generated from config.h.in */
#endif