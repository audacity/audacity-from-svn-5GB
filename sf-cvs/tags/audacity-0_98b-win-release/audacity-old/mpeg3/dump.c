#include "libmpeg3.h"
#include <stdlib.h>

#define BUFSIZE 65536

void test_32bit_overflow(char *outfile, int (*out_counter), FILE *(*out))
{
	if(ftell(*out) > 0x7f000000)
	{
		char string[1024];
		fclose(*out);
		sprintf(string, "%s%03d", outfile, ++(*out_counter));
		(*out) = fopen(string, "wb");
	}
}

int main(int argc, char *argv[])
{
	mpeg3_t *file;
	int i, j, result = 0;
	unsigned char *output, **output_rows;
	int out_counter = 0;
	float *audio_output_f;
	unsigned char *audio_output_i;
	long total_samples = 0;
	FILE *out;
	char outfile[1024];
	int decompress_audio = 0, decompress_video = 0;
	int audio_track = 0;

	outfile[0] = 0;
	if(argc < 2)
	{
		printf("Need an MPEG stream.\n");
		exit(1);
	}

	for(i = 1; i < argc; i++)
	{
		if(!strncmp(argv[i], "-a", 2))
		{
// Check for track number
			if(strlen(argv[i]) > 2) audio_track = atol(argv[i] + 2);

// Check for filename
			if(i + 1 < argc)
			{
				strcpy(outfile, argv[++i]);
				decompress_audio = 1;
			}
			else
			{
				fprintf(stderr, "-a must be paired with a filename.\n");
				exit(1);
			}

// Check if file exists
			if(out = fopen(outfile, "r"))
			{
				fprintf(stderr, "%s exists.\n", outfile);
				exit(1);
			}
		}
	}

	file = mpeg3_open(argv[argc - 1]);
	if(outfile[0])
	{
		out = fopen(outfile, "wb");
	}

	if(file)
	{
		fprintf(stderr, "MMX supported %d\n", file->have_mmx);
		fprintf(stderr, "Audio streams: %d\n", mpeg3_total_astreams(file));
		for(i = 0; i < mpeg3_total_astreams(file); i++)
		{
			fprintf(stderr, "  Stream %d: channels %d sample rate %d total samples %ld\n", 
				i, 
				mpeg3_audio_channels(file, i), 
				mpeg3_sample_rate(file, i),
				mpeg3_audio_samples(file, i));
		}
		fprintf(stderr, "Video streams: %d\n", mpeg3_total_vstreams(file));
		for(i = 0; i < mpeg3_total_vstreams(file); i++)
		{
			fprintf(stderr, "  Stream %d: width %d height %d frame rate %0.3f total frames %ld\n", 
				i, 
				mpeg3_video_width(file, i), 
				mpeg3_video_height(file, i), 
				mpeg3_frame_rate(file, i),
				mpeg3_video_frames(file, i));
		}

// Write audio
		if(decompress_audio)
		{
			mpeg3_set_cpus(file, 2);
 			audio_output_f = malloc(BUFSIZE * sizeof(float));
			audio_output_i = malloc(BUFSIZE * 3 * mpeg3_audio_channels(file, audio_track));

//printf("%d\n", mpeg3_end_of_audio(file, audio_track));
			while(!mpeg3_end_of_audio(file, audio_track) && !result)
			{
				test_32bit_overflow(outfile, &out_counter, &out);
				
				for(i = 0; i < mpeg3_audio_channels(file, audio_track); i++)
				{
					if(i == 0)
  						result = mpeg3_read_audio(file, 
							audio_output_f, 
							0, 
							i, 
							BUFSIZE, 
							audio_track);
					else
						result = mpeg3_reread_audio(file, 
							audio_output_f,      /* Pointer to pre-allocated buffer of floats */
							0,      /* Pointer to pre-allocated buffer of int16's */
							i,          /* Channel to decode */
							BUFSIZE,         /* Number of samples to decode */
							audio_track);

					for(j = 0; j < BUFSIZE; j++)
					{
						int sample = audio_output_f[j] * 0x7fffff;
						unsigned char *output_i = audio_output_i + j * 3 * mpeg3_audio_channels(file, audio_track) + i * 3;
						*output_i = (sample & 0xff000000);
						*output_i++ |= (sample & 0xff0000) >> 16;
						*output_i++ = (sample & 0xff00) >> 8;
						*output_i = sample & 0xff;
					}
						
				}
				
				result = !fwrite(audio_output_i, BUFSIZE * 3 * mpeg3_audio_channels(file, audio_track), 1, out);
			}
		}

/*
 * 		audio_output_i = malloc(BUFSIZE * 2 * mpeg3_audio_channels(file, 0));
 * 		mpeg3_seek_percentage(file, 0.1);
 * 		result = mpeg3_read_audio(file, 0, audio_output_i, 1, BUFSIZE, 0);
 */

/*
 *   		output = malloc(mpeg3_video_width(file, 0) * mpeg3_video_height(file, 0) * 3 + 4);
 *   		output_rows = malloc(sizeof(unsigned char*) * mpeg3_video_height(file, 0));
 *   		for(i = 0; i < mpeg3_video_height(file, 0); i++)
 *   			output_rows[i] = &output[i * mpeg3_video_width(file, 0) * 3];
 * printf("dump 1\n");
 *  		mpeg3_seek_percentage(file, 0.375);
 *  		result = mpeg3_read_frame(file, 
 *  					output_rows, 
 *  					0, 
 *  					0, 
 *  					mpeg3_video_width(file, 0), 
 * 					mpeg3_video_height(file, 0), 
 *  					mpeg3_video_width(file, 0), 
 *  					mpeg3_video_height(file, 0), 
 * 					MPEG3_RGB888, 
 *  					0);
 * printf("dump 2\n");
 */

		mpeg3_close(file);
	}
	return 0;
}
