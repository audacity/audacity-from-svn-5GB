/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormats.cpp

  Dominic Mazzoni

**********************************************************************/

#include "sndfile.h"

#include "FileFormats.h"

int sf_num_headers()
{
   return 14;
}

wxString HeaderNames[14] = {
   "Windows Wave",
   "Apple/SGI AIFF",
   "Sun/NeXT AU",
   "DEC AU",
   "Raw PCM data",
   "Ensoniq PARIS",
   "Amiga IFF/SVX8/SV16",
   "NIST/Sphere",
   "Windows Media Audio",
   "SEK'D Samplitude",
   "VOC",
   "Sound Designer 2",
   "Propellorheads Rex2",
   "Berkeley/IRCAM/CARL"};

wxString sf_header_name(int format)
{
   if (format >= 0x10000)
      return HeaderNames[(format/0x10000)-1];
   else if (format>=0 && format<14)
      return HeaderNames[format];
   else
      return "Unknown header";
}

int sf_num_encodings()
{
   return 17;
}

wxString EncodingNames[17] = {
   "PCM",
   "floating-point",
   "8-bit u-law encoding",
   "8-bit a-law encoding",
   "4-bit IMA ADPCM encoding",
   "4-bit MS ADPCM encoding",
   "PCM big-endian",
   "PCM little-endian",
   "signed PCM",
   "unsigned PCM",
   "SVX Fibonacci Delta encoding",
   "SVX Exponential Delta encoding",
   "GSM 6.10 encoding",
   "32kbs G721 ADPCM encoding",
   "24kbs G723 ADPCM encoding",
   "big-endian floating-point",
   "little-endian floating-point"};

wxString sf_encoding_name(int subtype)
{
   if (subtype >= 1 && subtype <= 17)
      return EncodingNames[subtype-1];
   else
      return "Unknown Subtype";
}

#define SF_NUM_SIMPLE_FORMATS 10
/* These should be kept alphabetical order. */
static SF_FORMAT_SIMPLE_INFO
psf_simple_format_array [SF_NUM_SIMPLE_FORMATS] = 
{	{	SF_FORMAT_AIFF | SF_FORMAT_PCM_BE, 8,
		"AIFF (Apple/SGI 8 bit)", "aiff"
		},

	{	SF_FORMAT_AIFF | SF_FORMAT_PCM_BE, 16,
		"AIFF (Apple/SGI 16 bit)", "aiff" 
		},
	
	{	SF_FORMAT_AIFF | SF_FORMAT_FLOAT_BE, 32,
		"AIFF (Apple/SGI 32 bit float)", "aifc" 
		},

	{	SF_FORMAT_AU | SF_FORMAT_ULAW, 16,
		"AU (Sun/Next 8-bit u-law)", "au"
		},
		
	{	SF_FORMAT_AU | SF_FORMAT_PCM_BE, 16,
		"AU (Sun/Next 16 bit)", "au"
		},
		
	{	SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, 16,
		"WAV (Microsoft 4 bit MS ADPCM)", "wav"
		},
	
	{	SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, 16,
		"WAV (Microsoft 4 bit IMA ADPCM)", "wav"
		},
		
	{	SF_FORMAT_WAV | SF_FORMAT_PCM_LE, 8,
		"WAV (Microsoft 8 bit)", "wav"
		},

	{	SF_FORMAT_WAV | SF_FORMAT_PCM_LE, 16,
		"WAV (Microsoft 16 bit)", "wav"
		},

	{	SF_FORMAT_WAV | SF_FORMAT_FLOAT_LE, 32,
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



