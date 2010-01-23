/*
** Copyright (C) 2002,2003 Erik de Castro Lopo <erikd@mega-nerd.com>
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
** Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
*/

#include "calc_snr.h"

#include "config.h"

#if (HAVE_LIBFFTW && HAVE_LIBRFFTW)

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <rfftw.h>

#define	MAX_SPEC_LEN	(1<<18)
#define	MAX_PEAKS		10

static void log_mag_spectrum (double *input, int len, double *magnitude) ;
static double find_snr (const double *magnitude, int len) ;

typedef struct
{	double	peak ;
	int		index ;
} PEAK_DATA ;

double
calculate_snr (float *data, int len)
{	static double magnitude [MAX_SPEC_LEN] ;
	static double datacopy [MAX_SPEC_LEN] ;

	double snr = 200.0 ;
	int k ;

	if (len > MAX_SPEC_LEN)
	{	printf (__FILE__ " : line %d : data length too large.\n", __LINE__) ;
		exit (1) ;
		} ;

	/* Padd the data just a little to speed up the FFT. */
	while ((len & 0x1F) && len < MAX_SPEC_LEN)
	{	data [len] = 0.0 ;
		len ++ ;
		} ;

	for (k = 0 ; k < len ; k++)
		datacopy [k] = data [k] ;

	log_mag_spectrum (datacopy, len, magnitude) ;

	snr = find_snr (magnitude, len) ;

	return snr ;
} /* calculate_snr */

static void
log_mag_spectrum (double *input, int len, double *magnitude)
{	rfftw_plan plan = NULL ;

	double	maxval ;
	int		k ;

	if (input == NULL || magnitude == NULL)
		return ;

	plan = rfftw_create_plan (len, FFTW_REAL_TO_COMPLEX, FFTW_ESTIMATE | FFTW_OUT_OF_PLACE) ;
	if (plan == NULL)
	{	printf (__FILE__ " : line %d : create plan failed.\n", __LINE__) ;
		exit (1) ;
		} ;

	rfftw_one (plan, input, magnitude) ;

	/* (k < N/2 rounded up) */
	maxval = 0.0 ;
	for (k = 1 ; k < len / 2 ; k++)
	{	magnitude [k] = sqrt (magnitude [k] * magnitude [k] + magnitude [len - k] * magnitude [len - k]) ;
		maxval = (maxval < magnitude [k]) ? magnitude [k] : maxval ;
		} ;
	for ( ; k < len ; k++)
		magnitude [k] = 0.0 ;

	/* Don't care about DC component. Make it zero. */
	magnitude [0] = 0.0 ;

	/* log magnitude. */
	for (k = 0 ; k < len ; k++)
	{	magnitude [k] = magnitude [k] / maxval ;
		magnitude [k] = (magnitude [k] < 1e-15) ? -200.0 : 20.0 * log10 (magnitude [k]) ;
		} ;

	return ;
} /* log_mag_spectrum */

static int
peak_compare (const void *vp1, const void *vp2)
{	const PEAK_DATA *peak1, *peak2 ;

	peak1 = (const PEAK_DATA*) vp1 ;
	peak2 = (const PEAK_DATA*) vp2 ;

	return (peak1->peak < peak2->peak) ? 1 : -1 ;
} /* peak_compare */

static double
find_snr (const double *magnitude, int len)
{	PEAK_DATA peaks [MAX_PEAKS] ;

	int		k, peak_count = 0 ;
	double	snr ;

	memset (peaks, 0, sizeof (peaks)) ;

	/* Find the MAX_PEAKS largest peaks. */
	for (k = 1 ; k < len - 1 ; k++)
	{	if (magnitude [k-1] < magnitude [k] && magnitude [k] >= magnitude [k+1])
		{	if (peak_count < MAX_PEAKS)
			{	peaks [peak_count].peak = magnitude [k] ;
				peaks [peak_count].index = k ;
				peak_count ++ ;
				}
			else if (magnitude [k] > peaks [MAX_PEAKS - 1].peak)
			{	qsort (peaks, MAX_PEAKS, sizeof (PEAK_DATA), peak_compare) ;
				peaks [MAX_PEAKS - 1].peak = magnitude [k] ;
				peaks [MAX_PEAKS - 1].index = k ;
				} ;
			} ;
		} ;

	if (peak_count < MAX_PEAKS / 2)
	{	printf ("\n" __FILE__ " : line %d : bad peak_count (%d).\n\n", __LINE__, peak_count) ;
		return -1.0 ;
		} ;

	/* Sort the peaks. */
	qsort (peaks, peak_count, sizeof (PEAK_DATA), peak_compare) ;

	snr = peaks [0].peak ;
	for (k = 1 ; k < peak_count ; k++)
		if (fabs (snr - peaks [k].peak) > 10.0)
			return fabs (peaks [k].peak) ;

	return snr ;
} /* find_snr */

#else /* ! (HAVE_LIBFFTW && HAVE_LIBRFFTW) */

double
calculate_snr (float *data, int len)
{	double snr = 200.0 ;

	data = data ;
	len = len ;

	return snr ;
} /* calculate_snr */

#endif
