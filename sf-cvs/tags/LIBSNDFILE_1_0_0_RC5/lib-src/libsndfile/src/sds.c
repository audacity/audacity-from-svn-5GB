/*
** Copyright (C) 2002 Erik de Castro Lopo <erikd@zip.com.au>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include	<stdio.h>
#include	<unistd.h>
#include	<fcntl.h>
#include	<string.h>
#include	<ctype.h>

#include	"sndfile.h"
#include	"config.h"
#include	"sfendian.h"
#include	"common.h"

/*------------------------------------------------------------------------------
** Macros to handle big/little endian issues.
*/

#define  SFE_SDS_NOT_SDS 		666
#define  SFE_SDS_BAD_BIT_WIDTH	667

#define	SDS_DATA_OFFSET			0x15
#define SDS_BLOCK_SIZE			127

#define SDS_BYTES_PER_BLOCK		120

#if (ENABLE_EXPERIMENTAL_CODE == 0)

int
sds_open	(SF_PRIVATE *psf)
{	if (psf)
		return SFE_UNIMPLEMENTED ;
	return (psf && 0) ;
} /* sds_open */

#else

/*------------------------------------------------------------------------------
** Typedefs.
*/

typedef struct tag_SDS_PRIVATE
{	int bitwidth, blockcount, frames ;

	int (*reader) (struct tag_SDS_PRIVATE *psds) ;
	int (*writer) (struct tag_SDS_PRIVATE *psds) ;

	int	blockindex, block, samplesperblock ;
	unsigned char data [SDS_BLOCK_SIZE] ;
	int	samples [SDS_BLOCK_SIZE / 2] ; /* Maximum samples per block */
} SDS_PRIVATE ;

/*------------------------------------------------------------------------------
** Private static functions.
*/

static int	sds_close		(SF_PRIVATE *psf) ;

static int	sds_write_header (SF_PRIVATE *psf, int calc_length) ;
static int	sds_read_header (SF_PRIVATE *psf, SDS_PRIVATE *psds) ;

static int sds_init (SF_PRIVATE *psf, SDS_PRIVATE *psds) ;

static sf_count_t sds_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t sds_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t sds_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t sds_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t sds_seek (SF_PRIVATE *psf, int mode, sf_count_t offset) ;

static int sds_2byte_read (SDS_PRIVATE *psds) ;
static int sds_3byte_read (SDS_PRIVATE *psds) ;
static int sds_4byte_read (SDS_PRIVATE *psds) ;

static int sds_read (SF_PRIVATE *psf, SDS_PRIVATE *psds, int *iptr, int readcount) ;

/*-static int sds_2byte_write (SF_PRIVATE *psf, int len) ;
static int sds_3byte_write (SF_PRIVATE *psf, int len) ;
static int sds_4byte_write (SF_PRIVATE *psf, int len) ;
-*/

/*------------------------------------------------------------------------------
** Public function.
*/

int
sds_open	(SF_PRIVATE *psf)
{	SDS_PRIVATE	*psds ;
	int			encoding, subformat, error = 0 ;

	if (! (psds = malloc (sizeof (SDS_PRIVATE))))
		return SFE_MALLOC_FAILED ;

	memset (psds, 0, sizeof (SDS_PRIVATE)) ;
	psf->fdata = psds ;

	if (psf->mode == SFM_READ || (psf->mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = sds_read_header (psf, psds)))
			return error ;
		} ;

	if ((psf->sf.format & SF_FORMAT_TYPEMASK) != SF_FORMAT_SDS)
		return	SFE_BAD_OPEN_FORMAT ;

	subformat = psf->sf.format & SF_FORMAT_SUBMASK ;

	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	psf->endian = psf->sf.format & SF_FORMAT_ENDMASK ;
		if (CPU_IS_LITTLE_ENDIAN && psf->endian == SF_ENDIAN_CPU)
			psf->endian = SF_ENDIAN_LITTLE ;
		else if (psf->endian != SF_ENDIAN_LITTLE)
			psf->endian = SF_ENDIAN_BIG ;

		if (! (encoding = sds_write_header (psf, SF_FALSE)))
			return psf->error ;

		psf->write_header = sds_write_header ;
		} ;

	if ((error = sds_init (psf, psds)))
		return error ;

	psf->new_seek = sds_seek ;
	psf->close    = sds_close ;

	/*-psf->blockwidth = psf->bytewidth * psf->sf.channels ;-*/

	return error ;
} /* sds_open */

/*------------------------------------------------------------------------------
*/

static int
sds_close	(SF_PRIVATE  *psf)
{
	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	/*  Now we know for certain the length of the file we can
		 *  re-write correct values for the datasize header element.
		 */

		psf_fseek (psf->filedes, 0, SEEK_END) ;
		psf->filelength = psf_ftell (psf->filedes) ;

		/*- psf->datalength = psf->filelength - AU_DATA_OFFSET ;-*/
		psf_fseek (psf->filedes, 0, SEEK_SET) ;

		psf->sf.frames = psf->datalength / psf->blockwidth ;
		sds_write_header (psf, SF_FALSE) ;
		} ;

	if (psf->fdata)
		free (psf->fdata) ;
	psf->fdata = NULL ;

	return 0 ;
} /* sds_close */

static int
sds_read_header (SF_PRIVATE *psf, SDS_PRIVATE *psds)
{	unsigned char channel, bitwidth, loop_type, byte ;
	unsigned short sample_no, marker ;
	unsigned int  samp_period, samp_length, sustain_loop_start, sustain_loop_end ;
	int	bytesread ;

	/* Set position to start of file to begin reading header. */
	bytesread = psf_binheader_readf (psf, "pE211", 0, &marker, &channel, &byte) ;

	if (marker != 0xF07E || byte != 0x01)
		return SFE_SDS_NOT_SDS ;

	psf_log_printf (psf, "Read only : Midi Sample Dump Standard\nF07E\n Channels : %d\n", channel) ;

	bytesread += psf_binheader_readf (psf, "e2133331", &sample_no, &bitwidth, &samp_period,
						&samp_length, &sustain_loop_start, &sustain_loop_end, &loop_type) ;

	psds->bitwidth = bitwidth ;

	psf->sf.samplerate = (int) (1.0 / (1e-9 * samp_period)) ;
	psf->sf.frames = samp_length ;

	psf_log_printf (psf, 	" Sample Number : %d\n"
							" Bit Width     : %d\n"
							" Sample Rate   : %d\n"
							" Sample Length : %d\n"
							" Sustain Loop\n"
							"   Start     : %d\n"
							"   End       : %d\n"
							"   Loop Type : %d\n",
			sample_no, psds->bitwidth, psf->sf.samplerate, samp_length,
			sustain_loop_start, sustain_loop_end, loop_type) ;

	/*
	** Lie to the user about PCM bit width. Always round up to the next
	** multiple of 8.
	*/
	switch ((psds->bitwidth + 7) / 8)
	{	case 1 :
			psf->sf.format = SF_FORMAT_SDS | SF_FORMAT_PCM_S8 ;
			break ;

		case 2 :
			psf->sf.format = SF_FORMAT_SDS | SF_FORMAT_PCM_16 ;
			break ;

		case 3 :
			psf->sf.format = SF_FORMAT_SDS | SF_FORMAT_PCM_24 ;
			break ;

		case 4 :
			psf->sf.format = SF_FORMAT_SDS | SF_FORMAT_PCM_32 ;
			break ;

		default :
			psf_log_printf (psf, "*** Weird byte width (%d)\n", (psds->bitwidth + 7) / 8) ;
			return SFE_SDS_BAD_BIT_WIDTH ;
		} ;

	psf->dataoffset = SDS_DATA_OFFSET ;
	psf->datalength = psf->filelength - psf->dataoffset ;

	psds->blockcount = psf->datalength % SDS_BLOCK_SIZE ;

	bytesread += psf_binheader_readf (psf, "1", &byte) ;
	if (byte != 0xF7)
		psf_log_printf (psf, "bad end : %X\n", byte & 0xFF) ;

	for (psds->blockcount = 0 ; bytesread < psf->filelength ; psds->blockcount++)
	{
		bytesread += psf_binheader_readf (psf, "E2", &marker) ;

		if (marker == 0)
			break ;

		psf_binheader_readf (psf, "j", SDS_BLOCK_SIZE - 2) ;
		bytesread += SDS_BLOCK_SIZE - 2 ;
		} ;

	if (psf->datalength % SDS_BLOCK_SIZE)
		psf_log_printf (psf, " Datalength : %D (truncated data??? %d)\n", psf->datalength, (int) (psf->datalength % SDS_BLOCK_SIZE)) ;
	else
		psf_log_printf (psf, " Datalength : %D\n", psf->datalength) ;

	psf_log_printf (psf, " Blocks     : %d\n", psds->blockcount) ;

	/* Always Mono */
	psf->sf.channels = 1 ;
	psf->sf.sections = 1 ;

	psf_fseek (psf->filedes, SDS_DATA_OFFSET, SEEK_SET) ;

	return 0 ;
} /* sds_read_header */

static int
sds_write_header (SF_PRIVATE *psf, int calc_length)
{
	if (psf && calc_length)
		return 0 ;

	return 0 ;
} /* sds_write_header */


/*------------------------------------------------------------------------------
*/

static int
sds_init (SF_PRIVATE *psf, SDS_PRIVATE *psds)
{
	if (psds->bitwidth < 8 || psds->bitwidth > 28)
		return (psf->error = SFE_SDS_BAD_BIT_WIDTH) ;

	if (psds->bitwidth < 14)
	{	psds->reader = sds_2byte_read ;
		psds->samplesperblock = SDS_BYTES_PER_BLOCK / 2 ;
		}
	else if (psds->bitwidth < 21)
	{	psds->reader = sds_3byte_read ;
		psds->samplesperblock = SDS_BYTES_PER_BLOCK / 3 ;
		}
	else
	{	psds->reader = sds_4byte_read ;
		psds->samplesperblock = SDS_BYTES_PER_BLOCK / 4 ;
		} ;

	psf->read_short  = sds_read_s ;
	psf->read_int    = sds_read_i ;
	psf->read_float  = sds_read_f ;
	psf->read_double = sds_read_d ;



	return 0 ;
} /* dsds_init */

static int
sds_2byte_read (SDS_PRIVATE *psds)
{	if (psds)
	{	puts ("sds_2byte_read") ;
		exit (1) ;
		} ;
	return 0 ;
} /* sds_2byte_read */

static int
sds_3byte_read (SDS_PRIVATE *psds)
{	unsigned char /*-*ucptr,-*/ checksum ;
	int k ;
	
	checksum = psds->data [1] ;
	if (checksum != 0x7E)
	{	printf ("Error 1 : %02X\n", checksum & 0xFF) ;
		exit (1) ;
		}
	
	for (k = 2 ; k < SDS_BLOCK_SIZE - 3 ; k ++)
	{	printf ("%02X %02X\n", checksum & 0xFF, psds->data [k] & 0xFF) ;
		checksum ^= psds->data [k] ;
		} ;
	checksum &= 0x7F ;
		
	if (checksum != psds->data [SDS_BLOCK_SIZE - 2])
	{	printf ("checksum is %02X should be %02X\n", checksum, psds->data [SDS_BLOCK_SIZE - 2]) ;
		exit (1) ;
		} ;
	
puts ("checksum OK") ;
	
	puts ("sds_2byte_read") ;
	exit (1) ;

	return 0 ;
} /* sds_3byte_read */

static int
sds_4byte_read (SDS_PRIVATE *psds)
{	if (psds)
	{	puts ("sds_4byte_read") ;
		exit (1) ;
		} ;
	return 0 ;
} /* sds_4byte_read */


static sf_count_t
sds_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	if (! (psf && ptr))
		return len ;

	return 0 ;
} /* sds_read_s */

static sf_count_t
sds_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	if (! (psf && ptr))
		return len ;

	return 0 ;
} /* sds_read_i */

static sf_count_t
sds_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	if (! (psf && ptr))
		return len ;

	return 0 ;
} /* sds_read_f */

static sf_count_t
sds_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	SDS_PRIVATE	*psds ;
	int			*iptr ;
	int			k, bufferlen, readcount, count ;
	sf_count_t	total = 0 ;
	double		normfact ;

	if (! psf->fdata)
		return 0 ;
	psds = (SDS_PRIVATE*) psf->fdata ;

	normfact = (psf->norm_double == SF_TRUE) ? (1.0 / 0x80000000) : 1.0 ;

	iptr = (int*) psf->buffer ;
	bufferlen = sizeof (psf->buffer) / sizeof (int) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = sds_read (psf, psds, iptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * iptr [k] ;
		total += count ;
		len -= readcount ;
		} ;
	return total ;
} /* sds_read_d */

static sf_count_t
sds_seek (SF_PRIVATE *psf, int mode, sf_count_t offset)
{	if (psf && mode)
		return offset ;

	return 0 ;
} /* sds_seek */

static int
sds_read (SF_PRIVATE *psf, SDS_PRIVATE *psds, int *iptr, int readcount)
{
	if (psf_fread (psds->data, SDS_BLOCK_SIZE, 1, psf->filedes) != 1)
		return 0 ;
		
	if (psds->data [0] != 0xF0)
	{	printf ("Error A : %02X\n", psds->data [0] & 0xFF) ;
		exit (1) ;
		} ;
	
	psds->reader (psds) ;
	return 0 ;
} /* sds_read */

#endif
