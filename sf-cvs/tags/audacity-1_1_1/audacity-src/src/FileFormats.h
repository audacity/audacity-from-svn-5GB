/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormats.h

  Dominic Mazzoni

**********************************************************************/

#include <wx/list.h>
#include <wx/string.h>

#include "Audacity.h"

int sf_num_headers();
wxString sf_header_name(int format);
wxString sf_header_extension(int format);

void sf_get_all_extensions(wxStringList exts);

#ifdef __WXMAC__
# ifdef __UNIX__
#  include <CoreServices/CoreServices.h>
# else
#  include <Types.h>
# endif

OSType sf_header_mactype(int format);
#endif

int sf_num_encodings();
wxString sf_encoding_name(int subtype);

typedef struct
{   unsigned int    format;
    const char	    *name;
    const char	    *extension;
} SF_FORMAT_SIMPLE_INFO ;

int sf_num_simple_formats();
SF_FORMAT_SIMPLE_INFO *sf_simple_format(int i);

bool sf_subtype_more_than_16_bits(unsigned int format);
