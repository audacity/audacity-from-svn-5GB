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

#include <samplerate.h>

#include "util.h"
#include "config.h"

#define	BUFFER_LEN		(1<<16)

#define	ARRAY_LEN(x)	((int) (sizeof (x) / sizeof ((x) [0])))

static void multi_channel_test (int converter, int channel_count) ;

int
main (void)
{
	/* Force output of the Electric Fence banner message. */
	force_efence_banner () ;

	puts ("\n    Zero Order Hold interpolator :") ;
	multi_channel_test (SRC_ZERO_ORDER_HOLD, 1) ;
	multi_channel_test (SRC_ZERO_ORDER_HOLD, 2) ;
	multi_channel_test (SRC_ZERO_ORDER_HOLD, 3) ;
	multi_channel_test (SRC_ZERO_ORDER_HOLD, 7) ;

	puts ("\n    Linear interpolator :") ;
	multi_channel_test (SRC_LINEAR, 1) ;
	multi_channel_test (SRC_LINEAR, 2) ;
	multi_channel_test (SRC_LINEAR, 3) ;
	multi_channel_test (SRC_LINEAR, 7) ;

	puts ("\n    Sinc interpolator :") ;
	multi_channel_test (SRC_SINC_FASTEST, 1) ;
	multi_channel_test (SRC_SINC_FASTEST, 2) ;
	multi_channel_test (SRC_SINC_FASTEST, 3) ;
	multi_channel_test (SRC_SINC_FASTEST, 7) ;

	puts ("") ;

	return 0 ;
} /* main */

/*==============================================================================
*/

#define	CHANNEL_FUNC(x)	(2.0 / ((x) + 1) - 1.0)

static void
multi_channel_test (int converter, int channel_count)
{	static float input [BUFFER_LEN], output [BUFFER_LEN] ;

	SRC_DATA	src_data ;

	int ch, k, error, ignore ;

	printf ("\tmulti_channel_test (%d channels) ................. ", channel_count) ;
	fflush (stdout) ;

	/* Choose a converstion ratio < 1.0. */
	src_data.src_ratio = 0.95 ;

	src_data.data_in = input ;
	src_data.input_frames = BUFFER_LEN / channel_count ;

	for (k = 0 ; k < src_data.input_frames ; k++)
		for (ch = 0 ; ch < channel_count ; ch++)
			src_data.data_in [channel_count * k + ch] = CHANNEL_FUNC (ch) ;

	src_data.data_out = output ;
	src_data.output_frames = BUFFER_LEN / channel_count ;

	if ((error = src_simple (&src_data, converter, channel_count)))
	{	printf ("\n\nLine %d : %s\n\n", __LINE__, src_strerror (error)) ;
		exit (1) ;
		} ;

	if (fabs (src_data.output_frames_gen - src_data.src_ratio * src_data.input_frames) > 2)
	{	printf ("\n\nLine %d : bad output data length %ld should be %d.\n", __LINE__,
					src_data.output_frames_gen, (int) floor (src_data.src_ratio * src_data.input_frames)) ;
		printf ("\tsrc_ratio  : %.4f\n", src_data.src_ratio) ;
		printf ("\tinput_len  : %ld\n", src_data.input_frames) ;
		printf ("\toutput_len : %ld\n\n", src_data.output_frames_gen) ;
		exit (1) ;
		} ;

	ignore = src_data.output_frames_gen / 4 ;

	for (k = ignore ; k < src_data.output_frames_gen - ignore ; k++)
		for (ch = 0 ; ch < channel_count ; ch++)
			if (fabs (src_data.data_out [channel_count * k + ch] - CHANNEL_FUNC (ch)) > 1.5e-5)
			{	printf ("\n\nLine %d    %5d : %f -> %f   (%f difference)\n", __LINE__, k - ignore,
						CHANNEL_FUNC (ch), src_data.data_out [channel_count * k + ch],
						fabs (src_data.data_out [channel_count * k + ch] - CHANNEL_FUNC (ch))) ;
				save_oct_data ("multi_channel_test.dat", src_data.data_in, src_data.input_frames,
							src_data.data_out, src_data.output_frames_gen) ;
				exit (1) ;
				} ;

	puts ("ok") ;

	return ;
} /* multi_channel_test */


