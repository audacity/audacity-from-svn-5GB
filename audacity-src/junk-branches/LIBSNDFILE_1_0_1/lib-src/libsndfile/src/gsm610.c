/*
** Copyright (C) 1999-2002 Erik de Castro Lopo <erikd@zip.com.au>
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

#include	"sndfile.h"
#include	"config.h"
#include	"sfendian.h"
#include	"float_cast.h"
#include	"common.h"
#include	"wav_w64.h"
#include	"GSM610/gsm.h"

#if (CPU_IS_LITTLE_ENDIAN == 1)
#	define	MAKE_MARKER(a,b,c,d)		((a)|((b)<<8)|((c)<<16)|((d)<<24))
#elif (CPU_IS_BIG_ENDIAN == 1)
#	define	MAKE_MARKER(a,b,c,d)		(((a)<<24)|((b)<<16)|((c)<<8)|(d))
#else
#	error "Cannot determine endian-ness of processor."
#endif

#define	GSM610_BLOCKSIZE		33
#define	GSM610_SAMPLES			160

typedef struct gsm610_tag
{	int				blocks ;
	int				blockcount, samplecount ;
	int				samplesperblock, blocksize ;
	
	int				(*decode_block) (SF_PRIVATE *psf, struct gsm610_tag *pgsm610) ;
	int				(*encode_block) (SF_PRIVATE *psf, struct gsm610_tag *pgsm610) ;

	short			samples [WAV_W64_GSM610_SAMPLES] ;
	unsigned char	block [WAV_W64_GSM610_BLOCKSIZE] ;

	gsm				gsm_data ;
} GSM610_PRIVATE ;

static sf_count_t	gsm610_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	gsm610_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	gsm610_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	gsm610_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t	gsm610_write_s (SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	gsm610_write_i (SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	gsm610_write_f (SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	gsm610_write_d (SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static	int gsm610_read_block (SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610, short *ptr, int len) ;
static	int gsm610_write_block (SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610, short *ptr, int len) ;

static	int	gsm610_decode_block (SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610) ;
static	int	gsm610_encode_block (SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610) ;

static	int	gsm610_wav_decode_block (SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610) ;
static	int	gsm610_wav_encode_block (SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610) ;

static sf_count_t  gsm610_seek   (SF_PRIVATE *psf, int mode, sf_count_t offset) ;

static int	gsm610_close	(SF_PRIVATE  *psf) ;

/*============================================================================================
** WAV GSM610 initialisation function.
*/

int
gsm610_init (SF_PRIVATE *psf)
{	GSM610_PRIVATE	*pgsm610 ;
	int  true = 1 ;

	if (psf->mode == SFM_RDWR)
		return SFE_BAD_MODE_RW ;

	psf->sf.seekable = SF_FALSE ;

	if (! (pgsm610 = malloc (sizeof (GSM610_PRIVATE))))
		return SFE_MALLOC_FAILED ;

	psf->fdata = (void*) pgsm610 ;

	memset (pgsm610, 0, sizeof (GSM610_PRIVATE)) ;

/*============================================================

Need separate gsm_data structs for encode and decode.

============================================================*/

	if (! (pgsm610->gsm_data = gsm_create ()))
		return SFE_MALLOC_FAILED ;

	if ((psf->sf.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_WAV ||
				(psf->sf.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_W64)
	{	gsm_option (pgsm610->gsm_data,  GSM_OPT_WAV49, &true) ;

		pgsm610->encode_block = gsm610_wav_encode_block ;
		pgsm610->decode_block = gsm610_wav_decode_block ;

		pgsm610->samplesperblock = WAV_W64_GSM610_SAMPLES ;
		pgsm610->blocksize = WAV_W64_GSM610_BLOCKSIZE ;
		}
	else
	{	pgsm610->encode_block = gsm610_encode_block ;
		pgsm610->decode_block = gsm610_decode_block ;
		
		pgsm610->samplesperblock = GSM610_SAMPLES ;
		pgsm610->blocksize = GSM610_BLOCKSIZE ;
		} ;
		
	if (psf->mode == SFM_READ)
	{	if (psf->datalength % pgsm610->blocksize)
		{	psf_log_printf (psf, "*** Warning : data chunk seems to be truncated.\n") ;
			pgsm610->blocks = psf->datalength / pgsm610->blocksize + 1 ;
			}
		else
			pgsm610->blocks = psf->datalength / pgsm610->blocksize ;

		psf->sf.frames = pgsm610->samplesperblock * pgsm610->blocks ;

		pgsm610->decode_block (psf, pgsm610) ;	/* Read first block. */

		psf->read_short  = gsm610_read_s ;
		psf->read_int    = gsm610_read_i ;
		psf->read_float  = gsm610_read_f ;
		psf->read_double = gsm610_read_d ;
		} ;

	if (psf->mode == SFM_WRITE)
	{	pgsm610->blockcount  = 0 ;
		pgsm610->samplecount = 0 ;

		psf->write_short  = gsm610_write_s ;
		psf->write_int    = gsm610_write_i ;
		psf->write_float  = gsm610_write_f ;
		psf->write_double = gsm610_write_d ;
		} ;

	psf->close    = gsm610_close ;
	psf->new_seek = gsm610_seek ;

	psf->filelength = psf_get_filelen (psf->filedes) ;
	psf->datalength = psf->filelength - psf->dataoffset ;

	return 0 ;
} /* gsm610_init */

/*============================================================================================
** GSM 6.10 Read Functions.
*/

static int
gsm610_wav_decode_block (SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610)
{	int	k ;

	pgsm610->blockcount ++ ;
	pgsm610->samplecount = 0 ;

	if (pgsm610->blockcount > pgsm610->blocks)
	{	memset (pgsm610->samples, 0, WAV_W64_GSM610_SAMPLES * sizeof (short)) ;
		return 1 ;
		} ;

	if ((k = psf_fread (pgsm610->block, 1, WAV_W64_GSM610_BLOCKSIZE, psf->filedes)) != WAV_W64_GSM610_BLOCKSIZE)
		psf_log_printf (psf, "*** Warning : short read (%d != %d).\n", k, WAV_W64_GSM610_BLOCKSIZE) ;

	if (gsm_decode (pgsm610->gsm_data, pgsm610->block, pgsm610->samples) < 0)
	{	psf_log_printf (psf, "Error from gsm_decode() on frame : %d\n", pgsm610->blockcount) ;
		return 0 ;
		} ;

	if (gsm_decode (pgsm610->gsm_data, pgsm610->block+(WAV_W64_GSM610_BLOCKSIZE+1)/2, pgsm610->samples+WAV_W64_GSM610_SAMPLES/2) < 0)
	{	psf_log_printf (psf, "Error from gsm_decode() on frame : %d.5\n", pgsm610->blockcount) ;
		return 0 ;
		} ;

	return 1 ;
} /* gsm610_wav_decode_block */

static int
gsm610_decode_block (SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610)
{	int	k ;

	pgsm610->blockcount ++ ;
	pgsm610->samplecount = 0 ;

	if (pgsm610->blockcount > pgsm610->blocks)
	{	memset (pgsm610->samples, 0, GSM610_SAMPLES * sizeof (short)) ;
		return 1 ;
		} ;

	if ((k = psf_fread (pgsm610->block, 1, GSM610_BLOCKSIZE, psf->filedes)) != GSM610_BLOCKSIZE)
		psf_log_printf (psf, "*** Warning : short read (%d != %d).\n", k, GSM610_BLOCKSIZE) ;

	if (gsm_decode (pgsm610->gsm_data, pgsm610->block, pgsm610->samples) < 0)
	{	psf_log_printf (psf, "Error from gsm_decode() on frame : %d\n", pgsm610->blockcount) ;
		return 0 ;
		} ;

	return 1 ;
} /* gsm610_decode_block */

static int
gsm610_read_block (SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610, short *ptr, int len)
{	int	count, total = 0, index = 0 ;

	while (index < len)
	{	if (pgsm610->blockcount >= pgsm610->blocks && pgsm610->samplecount >= pgsm610->samplesperblock)
		{	memset (&(ptr[index]), 0, (size_t) ((len - index) * sizeof (short))) ;
			return total ;
			} ;

		if (pgsm610->samplecount >= pgsm610->samplesperblock)
			pgsm610->decode_block (psf, pgsm610) ;

		count = pgsm610->samplesperblock - pgsm610->samplecount ;
		count = (len - index > count) ? count : len - index ;

		memcpy (&(ptr[index]), &(pgsm610->samples [pgsm610->samplecount]), count * sizeof (short)) ;
		index += count ;
		pgsm610->samplecount += count ;
		total = index ;
		} ;

	return total ;
} /* gsm610_read_block */

static sf_count_t
gsm610_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	GSM610_PRIVATE 	*pgsm610 ;
	int			readcount, count ;
	sf_count_t	total = 0 ;

	if (! psf->fdata)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->fdata ;

	while (len > 0)
	{	readcount = (len > 0x10000000) ? 0x1000000 : (int) len ;

		count = gsm610_read_block (psf, pgsm610, ptr, readcount) ;

		total += count ;
		len -= count ;

		if (count != readcount)
			break ;
		} ;

	return total ;
} /* gsm610_read_s */

static sf_count_t
gsm610_read_i  (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	GSM610_PRIVATE *pgsm610 ;
	short		*sptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;

	if (! psf->fdata)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->fdata ;

	sptr = (short*) psf->buffer ;
	bufferlen = SF_BUFFER_LEN / sizeof (short) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = gsm610_read_block (psf, pgsm610, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = sptr [k] << 16 ;

		total += count ;
		len -= readcount ;
		} ;
	return total ;
} /* gsm610_read_i */

static sf_count_t
gsm610_read_f  (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	GSM610_PRIVATE *pgsm610 ;
	short		*sptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if (! psf->fdata)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->fdata ;

	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x8000) : 1.0 ;

	sptr = (short*) psf->buffer ;
	bufferlen = SF_BUFFER_LEN / sizeof (short) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = gsm610_read_block (psf, pgsm610, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * sptr [k] ;

		total += count ;
		len -= readcount ;
		} ;
	return total ;
} /* gsm610_read_f */

static sf_count_t
gsm610_read_d  (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	GSM610_PRIVATE *pgsm610 ;
	short		*sptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;
	double		normfact ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x8000) : 1.0 ;

	if (! psf->fdata)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->fdata ;

	sptr = (short*) psf->buffer ;
	bufferlen = SF_BUFFER_LEN / sizeof (short) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = gsm610_read_block (psf, pgsm610, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * sptr [k] ;

		total += count ;
		len -= readcount ;
		} ;
	return total ;
} /* gsm610_read_d */

static sf_count_t
gsm610_seek   (SF_PRIVATE *psf, int mode, sf_count_t offset)
{	GSM610_PRIVATE *pgsm610 ;
	int			newblock, newsample ;

	mode = mode ;

	if (! psf->fdata)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->fdata ;

	if (psf->dataoffset < 0)
	{	psf->error = SFE_BAD_SEEK ;
		return	((sf_count_t) -1) ;
		} ;

	if (offset == 0)
	{	int true = 1 ;

		psf_fseek (psf->filedes, psf->dataoffset, SEEK_SET) ;
		pgsm610->blockcount  = 0 ;

		gsm_init (pgsm610->gsm_data) ;
		if ((psf->sf.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_WAV ||
				(psf->sf.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_W64)
			gsm_option (pgsm610->gsm_data,  GSM_OPT_WAV49, &true) ;

		pgsm610->decode_block (psf, pgsm610) ;
		pgsm610->samplecount = 0 ;
		return 0 ;
		} ;

	if (offset < 0 || offset > pgsm610->blocks * pgsm610->samplesperblock)
	{	psf->error = SFE_BAD_SEEK ;
		return	((sf_count_t) -1) ;
		} ;

	newblock  = offset / pgsm610->samplesperblock ;
	newsample = offset % pgsm610->samplesperblock ;

	if (psf->mode == SFM_READ)
	{	if (psf->read_current != newblock * pgsm610->samplesperblock + newsample)
		{	psf_fseek (psf->filedes, psf->dataoffset + newblock * pgsm610->samplesperblock, SEEK_SET) ;
			pgsm610->blockcount  = newblock ;
			pgsm610->decode_block (psf, pgsm610) ;
			pgsm610->samplecount = newsample ;
			} ;

		return newblock * pgsm610->samplesperblock + newsample ;
		} ;

	/* What to do about write??? */
	psf->error = SFE_BAD_SEEK ;
	return	((sf_count_t) -1) ;
} /* gsm610_seek */

/*==========================================================================================
** GSM 6.10 Write Functions.
*/

static int
gsm610_encode_block (SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610)
{	int k ;

	/* Encode the samples. */
	gsm_encode (pgsm610->gsm_data, pgsm610->samples, pgsm610->block) ;

	/* Write the block to disk. */
	if ((k = psf_fwrite (pgsm610->block, 1, GSM610_BLOCKSIZE, psf->filedes)) != GSM610_BLOCKSIZE)
		psf_log_printf (psf, "*** Warning : short write (%d != %d).\n", k, GSM610_BLOCKSIZE) ;

	pgsm610->samplecount = 0 ;
	pgsm610->blockcount ++ ;

	/* Set samples to zero for next block. */
	memset (pgsm610->samples, 0, WAV_W64_GSM610_SAMPLES * sizeof (short)) ;

	return 1 ;
} /* gsm610_encode_block */

static int
gsm610_wav_encode_block (SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610)
{	int k ;

	/* Encode the samples. */
	gsm_encode (pgsm610->gsm_data, pgsm610->samples, pgsm610->block) ;
	gsm_encode (pgsm610->gsm_data, pgsm610->samples+WAV_W64_GSM610_SAMPLES/2, pgsm610->block+WAV_W64_GSM610_BLOCKSIZE/2) ;

	/* Write the block to disk. */
	if ((k = psf_fwrite (pgsm610->block, 1, WAV_W64_GSM610_BLOCKSIZE, psf->filedes)) != WAV_W64_GSM610_BLOCKSIZE)
		psf_log_printf (psf, "*** Warning : short write (%d != %d).\n", k, WAV_W64_GSM610_BLOCKSIZE) ;

	pgsm610->samplecount = 0 ;
	pgsm610->blockcount ++ ;

	/* Set samples to zero for next block. */
	memset (pgsm610->samples, 0, WAV_W64_GSM610_SAMPLES * sizeof (short)) ;

	return 1 ;
} /* gsm610_wav_encode_block */

static int
gsm610_write_block (SF_PRIVATE *psf, GSM610_PRIVATE *pgsm610, short *ptr, int len)
{	int		count, total = 0, index = 0 ;

	while (index < len)
	{	count = pgsm610->samplesperblock - pgsm610->samplecount ;

		if (count > len - index)
			count = len - index ;

		memcpy (&(pgsm610->samples [pgsm610->samplecount]), &(ptr [index]), count * sizeof (short)) ;
		index += count ;
		pgsm610->samplecount += count ;
		total = index ;

		if (pgsm610->samplecount >= pgsm610->samplesperblock)
			pgsm610->encode_block (psf, pgsm610) ;
		} ;

	return total ;
} /* gsm610_write_block */

static sf_count_t
gsm610_write_s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	GSM610_PRIVATE 	*pgsm610 ;
	int			writecount, count ;
	sf_count_t	total = 0 ;

	if (! psf->fdata)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->fdata ;

	while (len > 0)
	{	writecount = (len > 0x10000000) ? 0x10000000 : (int) len ;

		count = gsm610_write_block (psf, pgsm610, ptr, writecount) ;

		total += count ;
		len -= count ;

		if (count != writecount)
			break ;
		} ;

	return total ;
} /* gsm610_write_s */

static sf_count_t
gsm610_write_i  (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	GSM610_PRIVATE *pgsm610 ;
	short		*sptr ;
	int			k, bufferlen, writecount = 0, count ;
	sf_count_t	total = 0 ;

	if (! psf->fdata)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->fdata ;

	sptr = (short*) psf->buffer ;
	bufferlen = SF_BUFFER_LEN / sizeof (short) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = ptr [total + k] >> 16 ;
		count = gsm610_write_block (psf, pgsm610, sptr, writecount) ;

		total += count ;
		len -= writecount ;
		} ;
	return total ;
} /* gsm610_write_i */

static sf_count_t
gsm610_write_f  (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	GSM610_PRIVATE *pgsm610 ;
	short		*sptr ;
	int			k, bufferlen, writecount = 0, count ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if (! psf->fdata)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->fdata ;

	normfact = (psf->norm_float == SF_TRUE) ? (1.0 * 0x7FFF) : 1.0 ;

	sptr = (short*) psf->buffer ;
	bufferlen = SF_BUFFER_LEN / sizeof (short) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = lrintf (normfact * ptr [total + k])  ;
		count = gsm610_write_block (psf, pgsm610, sptr, writecount) ;

		total += count ;
		len -= writecount ;
		} ;
	return total ;
} /* gsm610_write_f */

static sf_count_t
gsm610_write_d  (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	GSM610_PRIVATE *pgsm610 ;
	short		*sptr ;
	int			k, bufferlen, writecount = 0, count ;
	sf_count_t	total = 0 ;
	double		normfact ;

	if (! psf->fdata)
		return 0 ;
	pgsm610 = (GSM610_PRIVATE*) psf->fdata ;

	normfact = (psf->norm_double == SF_TRUE) ? (1.0 * 0x7FFF) : 1.0 ;

	sptr = (short*) psf->buffer ;
	bufferlen = SF_BUFFER_LEN / sizeof (short) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = lrint (normfact * ptr [total + k]) ;
		count = gsm610_write_block (psf, pgsm610, sptr, writecount) ;

		total += count ;
		len -= writecount ;
		} ;
	return total ;
} /* gsm610_write_d */

static int
gsm610_close	(SF_PRIVATE  *psf)
{	GSM610_PRIVATE *pgsm610 ;

	if (! psf->fdata)
		return 0 ;

	pgsm610 = (GSM610_PRIVATE*) psf->fdata ;

	if (psf->mode == SFM_WRITE)
	{	/*	If a block has been partially assembled, write it out
		**	as the final block.
		*/

		if (pgsm610->samplecount && pgsm610->samplecount < pgsm610->samplesperblock)
			pgsm610->encode_block (psf, pgsm610) ;

		/*  Now we know for certain the length of the file we can
		**  re-write correct values for the RIFF and data chunks.
		*/

		psf_fseek (psf->filedes, 0, SEEK_END) ;
		psf->filelength = psf_ftell (psf->filedes) ;

		psf->sf.frames = pgsm610->samplesperblock * pgsm610->blockcount ;
		psf->datalength = psf->filelength - psf->dataoffset ;

		if (psf->write_header)
			psf->write_header (psf, SF_FALSE) ;
		} ;

	if (pgsm610->gsm_data)
		gsm_destroy (pgsm610->gsm_data) ;

	if (psf->fdata)
		free (psf->fdata) ;
	psf->fdata = NULL ;

	return 0 ;
} /* gsm610_close */

