/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormats.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/intl.h>
#include "sndfile.h"

#include "FileFormats.h"

int sf_num_headers()
{
   return 14;
}

// i18n: These are marked with wxTRANSLATE(), which is a no-op macro.
// The actual translation call must take place when the array is accessed.

wxString HeaderNames[14] = {
   _("Windows Wave"),
   _("Apple/SGI AIFF"),
   _("Sun/NeXT AU"),
   _("DEC AU"),
   _("Raw PCM data"),
   _("Ensoniq PARIS"),
   _("Amiga IFF/SVX8/SV16"),
   _("NIST/Sphere"),
   _("Windows Media Audio"),
   _("SEK'D Samplitude"),
   _("VOC"),
   _("Sound Designer 2"),
   _("Propellorheads Rex2"),
   _("Berkeley/IRCAM/CARL")
};

wxString ExtensionNames[14] = {
   "wav",
   "aiff",
   "au",
   "au",
   "raw",
   "par",
   "iff",
   "nist",
   "wma",
   "samp",
   "voc",
   "sd2",
   "rex2",
   "ircam"
};

#ifdef __WXMAC__

// TODO: find out the appropriate OSType
// for the ones with an '????'.  The others
// are at least the same type used by
// SoundApp.

OSType MacNames[14] = {
   'WAVE', // WAVE
   'AIFF', // AIFF
   'NeXT', // Sun/NeXT AU
   'NeXT', // Sun/NeXT AU
   'BINA', // RAW i.e. binary
   'PAR ', // ??? Ensoniq PARIS
   '8SVX', // Amiga IFF / SVX8
   'NIST', // ??? NIST/Sphere
   'WMA ', // ??? Windows Media Audio
   'SAMP', // ??? SEK'D Samplitude
   'VOC ', // VOC
   'SFIL', // Sound Designer II
   'REX2', // ?? Propellorheads Rex2
   'IRCM'  // IRCAM
};

OSType sf_header_mactype(int format)
{
   if (format >= 0x10000)
      return MacNames[(format/0x10000)-1];
   else if (format>=0 && format<14)
      return MacNames[format];
   else
      return '????';
}

#endif // __WXMAC__

wxString sf_header_name(int format)
{
   if (format >= 0x10000)
      return HeaderNames[(format/0x10000)-1];
   else if (format>=0 && format<14)
      return HeaderNames[format];
   else
      return _("Unknown header");
}

wxString sf_header_extension(int format)
{
   if (format >= 0x10000)
      return ExtensionNames[(format/0x10000)-1];
   else if (format>=0 && format<14)
      return ExtensionNames[format];
   else
      return _("Unknown header");
}

int sf_num_encodings()
{
   return 17;
}

wxString EncodingNames[17] = {
   _("PCM"),
   _("floating-point"),
   _("8-bit u-law encoding"),
   _("8-bit a-law encoding"),
   _("4-bit IMA ADPCM encoding"),
   _("4-bit MS ADPCM encoding"),
   _("PCM big-endian"),
   _("PCM little-endian"),
   _("signed PCM"),
   _("unsigned PCM"),
   _("SVX Fibonacci Delta encoding"),
   _("SVX Exponential Delta encoding"),
   _("GSM 6.10 encoding"),
   _("32kbs G721 ADPCM encoding"),
   _("24kbs G723 ADPCM encoding"),
   _("big-endian floating-point"),
   _("little-endian floating-point")
};

wxString sf_encoding_name(int subtype)
{
   if (subtype >= 1 && subtype <= 17)
      return EncodingNames[subtype-1];
   else
      return _("Unknown Subtype");
}

#define SF_NUM_SIMPLE_FORMATS 10
/* These should be kept alphabetical order. */
static SF_FORMAT_SIMPLE_INFO
psf_simple_format_array [SF_NUM_SIMPLE_FORMATS] = 
{	{	SF_FORMAT_AIFF | SF_FORMAT_PCM, 8,
		"AIFF (Apple/SGI 8 bit)", "aiff"
		},

	{	SF_FORMAT_AIFF | SF_FORMAT_PCM, 16,
		"AIFF (Apple/SGI 16 bit)", "aiff" 
		},
	
	{	SF_FORMAT_AIFF | SF_FORMAT_FLOAT, 32,
		"AIFF (Apple/SGI 32 bit float)", "aifc" 
		},

	{	SF_FORMAT_AU | SF_FORMAT_ULAW, 16,
		"AU (Sun/Next 8-bit u-law)", "au"
		},
		
	{	SF_FORMAT_AU | SF_FORMAT_PCM, 16,
		"AU (Sun/Next 16 bit)", "au"
		},
		
	{	SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, 16,
		"WAV (Microsoft 4 bit MS ADPCM)", "wav"
		},
	
	{	SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, 16,
		"WAV (Microsoft 4 bit IMA ADPCM)", "wav"
		},
		
	{	SF_FORMAT_WAV | SF_FORMAT_PCM, 8,
		"WAV (Microsoft 8 bit)", "wav"
		},

	{	SF_FORMAT_WAV | SF_FORMAT_PCM, 16,
		"WAV (Microsoft 16 bit)", "wav"
		},

	{	SF_FORMAT_WAV | SF_FORMAT_FLOAT, 32,
		"WAV (Microsoft 32 bit float)", "wav"
		}
} ; /* sf_simple_format_array */

int sf_num_simple_formats()
{
   return SF_NUM_SIMPLE_FORMATS;
}

SF_FORMAT_SIMPLE_INFO *sf_simple_format(int i)
{
   return &psf_simple_format_array[i];
}



