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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include	"util.h"

#ifndef	M_PI
#define	M_PI			3.14159265358979323846264338
#endif

void
gen_windowed_sines (float *data, int data_len, double *freqs, int freq_count)
{	int 	k, freq ;
	double	amplitude, phase ;

	amplitude = 1.0 / freq_count ;

	for (k = 0 ; k < data_len ; k++)
		data [k] = 0.0 ;

	for (freq = 0 ; freq < freq_count ; freq++)
	{	phase = 0.9 * M_PI / freq_count ;

		if (freqs [freq] <= 0.0 || freqs [freq] >= 0.5)
		{	printf ("\n" __FILE__ " : Error : freq [%d] == %g is out of range.\n", freq, freqs [freq]) ;
			exit (1) ;
			} ;

		for (k = 0 ; k < data_len ; k++)
			data [k] += amplitude * sin (freqs [freq] * (2 * k) * M_PI + phase) ;
		} ;

	/* Apply Hanning Window. */
	for (k = 0 ; k < data_len ; k++)
		data [k] *= 0.5 - 0.5 * cos ((2 * k) * M_PI / (data_len - 1)) ;

	return ;
} /* gen_windowed_sines */

void
save_oct_data (char *filename, float *input, int in_len, float *output, int out_len)
{	FILE 	*file ;
	int		k ;

	printf ("Dumping input and output data to file : %s.\n\n", filename) ;

	if (! (file = fopen (filename, "w")))
		return ;

	fprintf (file, "# Not created by Octave\n") ;

	fprintf (file, "# name: input\n") ;
	fprintf (file, "# type: matrix\n") ;
	fprintf (file, "# rows: %d\n", in_len) ;
	fprintf (file, "# columns: 1\n") ;

	for (k = 0 ; k < in_len ; k++)
		fprintf (file, "% g\n", input [k]) ;

	fprintf (file, "# name: output\n") ;
	fprintf (file, "# type: matrix\n") ;
	fprintf (file, "# rows: %d\n", out_len) ;
	fprintf (file, "# columns: 1\n") ;

	for (k = 0 ; k < out_len ; k++)
		fprintf (file, "% g\n", output [k]) ;

	fclose (file) ;
	return ;
} /* save_oct_data */

void
force_efence_banner (void)
{	void *dummy ;

	dummy = malloc (1) ;
	free (dummy) ;
} /* force_efence_banner */
