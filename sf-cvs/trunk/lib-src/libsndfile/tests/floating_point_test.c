/*
** Copyright (C) 1999-2002 Erik de Castro Lopo <erikd@zip.com.au>
**  
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
** 
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software 
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include	<stdio.h>
#include	<string.h>
#include	<unistd.h>
#include	<math.h>

#include	<sndfile.h>

#include	"dft_cmp.h"
#include	"utils.h"

/* Configureation header from ../src or ../Win32. */
#include	<config.h>
#include	<float_cast.h>

#define	SAMPLE_RATE			11025

static void	float_scaled_test	(char *filename, int allow_exit, int filetype, unsigned int target_hash, double target_snr) ;
static void	double_scaled_test	(char *filename, int allow_exit, int filetype, double target_snr) ;

static	double	orig_data [DFT_DATA_LENGTH] ;
static	double	test_data [DFT_DATA_LENGTH] ;


int		
main (int argc, char *argv [])
{	int allow_exit = 1 ;

	if (argc == 2 && ! strstr (argv [1], "no-exit"))
		allow_exit = 0 ;

#if (! (HAVE_LRINTF || HAVE_LRINT_REPLACEMENT))
	puts ("** This platform does not have lrintf(), so file hash \n"
		  "** checking cannot be performed.") ;
#endif

	/* Float tests. */
	float_scaled_test	("float.raw", allow_exit, SF_ENDIAN_LITTLE | SF_FORMAT_RAW  | SF_FORMAT_FLOAT, 0x80413625, -163.0) ;

	/* Test both signed and unsigned 8 bit files. */
	float_scaled_test	("pcm_s8.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_PCM_S8, 0x929316c4, -39.0) ;
	float_scaled_test	("pcm_u8.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_PCM_U8, 0x529542c4, -39.0) ;

	float_scaled_test	("pcm_16.raw", allow_exit, SF_ENDIAN_BIG    | SF_FORMAT_RAW | SF_FORMAT_PCM_16, 0x87bd2472, -87.0) ;
	float_scaled_test	("pcm_24.raw", allow_exit, SF_ENDIAN_LITTLE | SF_FORMAT_RAW | SF_FORMAT_PCM_24, 0xdc903e47, -138.0) ;
	float_scaled_test	("pcm_32.raw", allow_exit, SF_ENDIAN_BIG    | SF_FORMAT_RAW | SF_FORMAT_PCM_32, 0xb214e199, -163.0) ;

	float_scaled_test	("ulaw.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_ULAW, 0x37b101b5, -50.0) ;
	float_scaled_test	("alaw.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_ALAW, 0x0c7141ec, -49.0) ;

	float_scaled_test	("imaadpcm.wav", allow_exit, SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, 0x590d3df8, -47.0) ;
	float_scaled_test	("msadpcm.wav" , allow_exit, SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, 0xe49453f9, -40.0) ;
	float_scaled_test	("gsm610.raw"  , allow_exit, SF_FORMAT_RAW | SF_FORMAT_GSM610, 0x6b20daf0, -33.0) ;

	float_scaled_test	("g721_32.au", allow_exit, SF_FORMAT_AU | SF_FORMAT_G721_32, 0x0d15119d, -34.0) ;
	float_scaled_test	("g723_24.au", allow_exit, SF_FORMAT_AU | SF_FORMAT_G723_24, 0x8394e3b8, -34.0) ;
	float_scaled_test	("g723_40.au", allow_exit, SF_FORMAT_AU | SF_FORMAT_G723_40, 0xbad070d2, -40.0) ;

	/*	PAF files do not use the same encoding method for 24 bit PCM data as other file
	**	formats so we need to explicitly test it here.
	*/
	float_scaled_test	("le_paf_24.paf", allow_exit, SF_ENDIAN_LITTLE | SF_FORMAT_PAF | SF_FORMAT_PCM_24, 0xf36eb03a, -149.0) ;
	float_scaled_test	("be_paf_24.paf", allow_exit, SF_ENDIAN_BIG    | SF_FORMAT_PAF | SF_FORMAT_PCM_24, 0x730d92d3, -149.0) ;

	float_scaled_test	("dwvw_12.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_DWVW_12, 0x137efb5e, -64.0) ;
	float_scaled_test	("dwvw_16.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_DWVW_16, 0xb3070047, -92.0) ;
	float_scaled_test	("dwvw_24.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_DWVW_24, 0x6f408fa0, -151.0) ;

	/*==============================================================================
	** Double tests. 
	*/

	double_scaled_test	("double.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_DOUBLE, -300.0) ;

	/* Test both signed (AIFF) and unsigned (WAV) 8 bit files. */
	double_scaled_test	("pcm_s8.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_PCM_S8, -39.0) ;
	double_scaled_test	("pcm_u8.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_PCM_U8, -39.0) ;

	double_scaled_test	("pcm_16.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_PCM_16, -87.0) ;
	double_scaled_test	("pcm_24.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_PCM_24, -135.0) ;
	double_scaled_test	("pcm_32.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_PCM_32, -184.0) ;

	double_scaled_test	("ulaw.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_ULAW, -50.0) ;
	double_scaled_test	("alaw.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_ALAW, -49.0) ;

	double_scaled_test	("imaadpcm.wav", allow_exit, SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, -47.0) ;
	double_scaled_test	("msadpcm.wav" , allow_exit, SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, -40.0) ;
	double_scaled_test	("gsm610.raw"  , allow_exit, SF_FORMAT_RAW | SF_FORMAT_GSM610, -33.0) ;

	double_scaled_test	("g721_32.au", allow_exit, SF_FORMAT_AU | SF_FORMAT_G721_32, -34.0) ;
	double_scaled_test	("g723_24.au", allow_exit, SF_FORMAT_AU | SF_FORMAT_G723_24, -34.0) ;
	double_scaled_test	("g723_40.au", allow_exit, SF_FORMAT_AU | SF_FORMAT_G723_40, -40.0) ;

	/*	24 bit PCM PAF files tested here. */
	double_scaled_test	("be_paf_24.paf", allow_exit, SF_ENDIAN_BIG    | SF_FORMAT_PAF  | SF_FORMAT_PCM_24, -151.0) ;
	double_scaled_test	("le_paf_24.paf", allow_exit, SF_ENDIAN_LITTLE | SF_FORMAT_PAF  | SF_FORMAT_PCM_24, -151.0) ;

	double_scaled_test	("dwvw_12.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_DWVW_12, -64.0) ;
	double_scaled_test	("dwvw_16.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_DWVW_16, -92.0) ;
	double_scaled_test	("dwvw_24.raw", allow_exit, SF_FORMAT_RAW | SF_FORMAT_DWVW_24, -151.0) ;

	return 0;
} /* main */

/*============================================================================================
 *	Here are the test functions.
 */ 

#define PUT_DOTS(k)					\
			{	while (k--)			\
					putchar ('.') ;	\
				putchar (' ') ;		\
				}
static void	
float_scaled_test (char *filename, int allow_exit, int filetype, unsigned int target_hash, double target_snr)
{	static	float	float_orig [DFT_DATA_LENGTH] ;
	static	float	float_test [DFT_DATA_LENGTH] ;

	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	int			k ;
	double		snr ;
	
	printf ("    float_scaled_test  : %s ", filename) ;	
	k = abs (18 - strlen (filename)) ;
	PUT_DOTS (k) ;
	fflush (stdout) ;
	
	gen_windowed_sine (orig_data, DFT_DATA_LENGTH, 0.95) ;
	
	for (k = 0 ; k < DFT_DATA_LENGTH ; k++)
		float_orig [k] = orig_data [k] ;
		
	sfinfo.samplerate  = SAMPLE_RATE ;
	sfinfo.frames     = DFT_DATA_LENGTH ;
	sfinfo.channels    = 1 ;
	sfinfo.format 	   = filetype ;

	if (! (file = sf_open (filename, SFM_WRITE, &sfinfo)))
	{	printf ("\n\nLine %d: sf_open_write failed with error : ", __LINE__) ;
		fflush (stdout) ;
		sf_perror (NULL) ;
		exit (1) ;
		} ;

	if ((k = sf_write_float (file, float_orig, DFT_DATA_LENGTH)) != DFT_DATA_LENGTH)
	{	printf ("\n\nLine %d: sf_write_float failed with error (%d -> %d) : ", __LINE__, DFT_DATA_LENGTH, k) ;
		fflush (stdout) ;
		sf_perror (file) ;
		exit (1) ;
		} ;
		
	sf_close (file) ;

#if (! (HAVE_LRINTF || HAVE_LRINT_REPLACEMENT))
	target_hash = target_hash ; /* Avoid compiler warning. */
#else
	check_file_hash_or_die (filename, target_hash, __LINE__) ;
#endif
	
	memset (float_test, 0, sizeof (float_test)) ;

	if (! (file = sf_open (filename, SFM_READ, &sfinfo)))
	{	printf ("\n\nLine %d: sf_open_read failed with error : ", __LINE__) ;
		fflush (stdout) ;
		sf_perror (NULL) ;
		exit (1) ;
		} ;
	
	if (sfinfo.format != filetype)
	{	printf ("\n\nLine %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
		exit (1) ;
		} ;
	
	if (sfinfo.frames < DFT_DATA_LENGTH)
	{	printf ("\n\nLine %d: Incorrect number of.frames in file (too short). (%ld should be %d)\n", __LINE__, SF_COUNT_TO_LONG (sfinfo.frames), DFT_DATA_LENGTH) ;
		exit (1) ;
		} ;
	
	if (sfinfo.channels != 1)
	{	printf ("\n\nLine %d: Incorrect number of channels in file.\n", __LINE__) ;
		exit (1) ;
		} ;

	check_log_buffer_or_die (file) ;
		
	if ((k = sf_read_float (file, float_test, DFT_DATA_LENGTH)) < DFT_DATA_LENGTH)
	{	printf ("\n\nLine %d: short read (%d).\n", __LINE__, k) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	for (k = 0 ; k < DFT_DATA_LENGTH ; k++)
		test_data [k] = float_test [k] ;

	snr = dft_cmp (__LINE__, orig_data, test_data, DFT_DATA_LENGTH, target_snr, allow_exit) ;

	if (snr < target_snr)
		printf ("% 6.1fdB SNR ... ok\n", snr) ;

	unlink (filename) ;

	return ;			
} /* float_scaled_test */

static void	
double_scaled_test (char *filename, int allow_exit, int filetype, double target_snr)
{	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	int			k ;
	double		snr ;

	printf ("    double_scaled_test : %s ", filename) ;
	k = abs (18 - strlen (filename)) ;
	PUT_DOTS (k) ;
	fflush (stdout) ;
	
	gen_windowed_sine (orig_data, DFT_DATA_LENGTH, 0.95) ;
		
	sfinfo.samplerate  = SAMPLE_RATE ;
	sfinfo.frames     = DFT_DATA_LENGTH ;
	sfinfo.channels    = 1 ;
	sfinfo.format 	   = filetype ;

	if (! (file = sf_open (filename, SFM_WRITE, &sfinfo)))
	{	printf ("\n\nLine %d: sf_open_write failed with error : ", __LINE__) ;
		fflush (stdout) ;
		sf_perror (NULL) ;
		exit (1) ;
		} ;

	if (sf_write_double (file, orig_data, DFT_DATA_LENGTH) != DFT_DATA_LENGTH)
	{	printf ("\n\nLine %d: sf_write_double failed with error : ", __LINE__) ;
		fflush (stdout) ;
		sf_perror (file) ;
		exit (1) ;
		} ;
		
	sf_close (file) ;
	
	memset (test_data, 0, sizeof (test_data)) ;

	if (! (file = sf_open (filename, SFM_READ, &sfinfo)))
	{	printf ("\n\nLine %d: sf_open_read failed with error : ", __LINE__) ;
		fflush (stdout) ;
		sf_perror (NULL) ;
		exit (1) ;
		} ;
	
	if (sfinfo.format != filetype)
	{	printf ("\n\nLine %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
		exit (1) ;
		} ;
	
	if (sfinfo.frames < DFT_DATA_LENGTH)
	{	printf ("\n\nLine %d: Incorrect number of.frames in file (too short). (%ld should be %d)\n", __LINE__, SF_COUNT_TO_LONG (sfinfo.frames), DFT_DATA_LENGTH) ;
		exit (1) ;
		} ;
	
	if (sfinfo.channels != 1)
	{	printf ("\n\nLine %d: Incorrect number of channels in file.\n", __LINE__) ;
		exit (1) ;
		} ;

	check_log_buffer_or_die (file) ;
		
	if ((k = sf_read_double (file, test_data, DFT_DATA_LENGTH)) < DFT_DATA_LENGTH)
	{	printf ("\n\nLine %d: short read (%d).\n", __LINE__, k) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	snr = dft_cmp (__LINE__, orig_data, test_data, DFT_DATA_LENGTH, target_snr, allow_exit) ;

	if (snr < target_snr)
		printf ("% 6.1fdB SNR ... ok\n", snr) ;

	unlink (filename) ;

	return ;			
} /* double_scaled_test */
