/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormats.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/intl.h>
#include "sndfile.h"

#ifndef SNDFILE_1
#error Requires libsndfile 1.0 or higher
#endif

#include "FileFormats.h"

int sf_num_headers()
{
   return 19;
}

// i18n: These are marked with wxTRANSLATE(), which is a no-op macro.
// The actual translation call must take place when the array is accessed.

wxString HeaderNames[19] = {
   _("Windows Wave"),
   _("Apple/SGI AIFF"),
   _("Sun/NeXT AU"),
   _("Raw PCM data"),
   _("Ensoniq PARIS"),
   _("Amiga IFF/SVX8/SV16"),
   _("NIST/Sphere"),
   _("VOC (SoundBlaster)"),
   _("Propellorheads Rex"),
   _("Berkeley/IRCAM/CARL"),
   _("64-bit Wave (Sonic Foundry)"),
   _("Sound Designer 2"),
   _("Propellorheads Rex 2"),   
   _("Yamaha TX16 sampler file"),
   _("Kurzweil sampler file"),
   _("GNU Octave data file"),
   _("SEK'D Samplitude"),
   _("Windows Media Audio"),
   _("Shorten (lossless compression)")
};

wxString ExtensionNames[38] = {
   "wav", "",
   "aif", "aiff",
   "au", "snd",
   "raw", "",
   "svx", "iff",
   "sph", "nist",
   "voc", "",
   "rex", "",
   "sf", "ircam",
   "w64", "",
   "sd2", "",
   "rex2", "",
   "txw", "",
   "krz", "",
   "oct", "",
   "sekd", "",
   "wma", "",
   "shn", ""
};

void sf_get_all_extensions(wxStringList exts)
{
   for(int i=0; i<sf_num_headers()*2; i++)
      if (strlen(ExtensionNames[i])>0)
         exts.Add(ExtensionNames[i]);
}

#ifdef __WXMAC__

// TODO: find out the appropriate OSType
// for the ones with an '????'.  The others
// are at least the same type used by
// SoundApp.

OSType MacNames[19] = {
   'WAVE', // WAVE
   'AIFF', // AIFF
   'NeXT', // Sun/NeXT AU
   'BINA', // RAW i.e. binary
   'PAR ', // ??? Ensoniq PARIS
   '8SVX', // Amiga IFF / SVX8
   'NIST', // ??? NIST/Sphere
   'VOC ', // VOC
   'REX ', // ?? Propellorheads Rex
   'SF  ', // ?? IRCAM
   'W64 ', // ?? Wave64
   'SFIL', // Sound Designer II
   'REX2', // ?? Propellorheads Rex2
   'TX16', // ?? Yamaha TX16 sampler file
   'KURZ', // ?? Kurzweil sampler file
   'Octv', // ?? GNU Octave data file
   'SAMP', // ?? SEK'D Samplitude
   'WMA ', // ?? Windows Media Audio
   'SHTN', // ?? Shorten (lossless compression)
};

OSType sf_header_mactype(int format)
{
   if (format >= 0x10000)
      return MacNames[(format/0x10000)-1];
   else if (format>=0 && format<19)
      return MacNames[format];
   else
      return '????';
}

#endif // __WXMAC__

wxString sf_header_name(int format)
{
   if (format >= 0x10000)
      return HeaderNames[(format/0x10000)-1];
   else if (format>=0 && format<19)
      return HeaderNames[format];
   else
      return _("Unknown header");
}

wxString sf_header_extension(int format)
{
   if (format >= 0x10000) {
      int index = ((format & SF_FORMAT_TYPEMASK)/0x10000)-1;
      return ExtensionNames[index*2];
   }
   else if (format>=0 && format<19)
      return ExtensionNames[format*2];
   else
      return _("Unknown header");
}

int sf_num_encodings()
{
   return 20;
}

wxString EncodingNames[20] = {
   _("8-bit PCM"),
   _("16-bit PCM"),
   _("24-bit PCM"),
   _("32-bit PCM"),
   _("8-bit unsigned PCM"),
   _("32-bit floating point"),
   _("64-bit floating point"),
   _("8-bit u-law"),
   _("8-bit a-law"),
   _("4-bit IMA ADPCM"),
   _("4-bit MS ADPCM"),
   "GSM610",
   _("32kbps G721 ADPCM"),
   _("24kbps G721 ADPCM"),
   _("12-bit DWVW"),
   _("16-bit DWVW"),
   _("24-bit DWVW"),
   _("n-bit DWVW"),
   _("SVX Fibonacci"),
   _("SVX Exponential")
};

wxString sf_encoding_name(int subtype)
{
   if (subtype >= 1 && subtype <= 20)
      return EncodingNames[subtype-1];
   else
      return _("Unknown Subtype");
}

#define SF_NUM_SIMPLE_FORMATS 8
/* These should be kept alphabetical order. */
static SF_FORMAT_SIMPLE_INFO
psf_simple_format_array [SF_NUM_SIMPLE_FORMATS] = 
{	{	SF_FORMAT_AIFF | SF_FORMAT_PCM_S8,
		"AIFF (Apple/SGI 8 bit)", "aiff"
		},

	{	SF_FORMAT_AIFF | SF_FORMAT_PCM_16,
		"AIFF (Apple/SGI 16 bit)", "aiff" 
		},
	
	{	SF_FORMAT_AIFF | SF_FORMAT_FLOAT,
		"AIFF (Apple/SGI 32 bit float)", "aifc" 
		},

	{	SF_FORMAT_AU | SF_FORMAT_ULAW,
		"AU (Sun/Next 8-bit u-law)", "au"
		},
		
	{	SF_FORMAT_AU | SF_FORMAT_PCM_16,
		"AU (Sun/Next 16 bit)", "au"
		},
		
	{	SF_FORMAT_WAV | SF_FORMAT_PCM_U8,
		"WAV (Microsoft 8 bit)", "wav"
		},

	{	SF_FORMAT_WAV | SF_FORMAT_PCM_16,
		"WAV (Microsoft 16 bit)", "wav"
		},

	{	SF_FORMAT_WAV | SF_FORMAT_FLOAT,
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

bool sf_subtype_more_than_16_bits(unsigned int format)
{
   unsigned int subtype = format & SF_FORMAT_SUBMASK;
   return (subtype == SF_FORMAT_FLOAT ||
           subtype == SF_FORMAT_DOUBLE ||
           subtype == SF_FORMAT_PCM_32 ||
           subtype==SF_FORMAT_PCM_24 ||
           subtype==SF_FORMAT_DWVW_24);
}
      

