/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormats.h

  Dominic Mazzoni

**********************************************************************/

#include <wx/string.h>

#include "Audacity.h"

int sf_num_headers();
wxString sf_header_name(int format);
wxString sf_header_extension(int format);

#ifdef __WXMAC__
# ifdef __UNIX__
# include <CoreServices/CoreServices.h>
# endif

OSType sf_header_mactype(int format);
#endif

int sf_num_encodings();
wxString sf_encoding_name(int subtype);

typedef struct
{   unsigned int    format;
    unsigned int    pcmbitwidth	;
    const char	    *name;
    const char	    *extension;
} SF_FORMAT_SIMPLE_INFO ;

int sf_num_simple_formats();
SF_FORMAT_SIMPLE_INFO *sf_simple_format(int i);
