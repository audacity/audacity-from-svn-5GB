#include "libmpeg3.h"
#include "mpeg3io.h"
#include "mpeg3protos.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

#define ABS(x) ((x) >= 0 ? (x) : -(x))

/* Don't advance pointer */
static inline unsigned char mpeg3packet_next_char(mpeg3_demuxer_t *demuxer)
{
	return demuxer->raw_data[demuxer->raw_offset];
}

unsigned char mpeg3packet_read_char(mpeg3_demuxer_t *demuxer)
{
	unsigned char result = demuxer->raw_data[demuxer->raw_offset++];
	return result;
}

static inline unsigned int mpeg3packet_read_int16(mpeg3_demuxer_t *demuxer)
{
	unsigned int a, b, result;
	a = demuxer->raw_data[demuxer->raw_offset++];
	b = demuxer->raw_data[demuxer->raw_offset++];
	result = (a << 8) | b;

	return result;
}

static inline unsigned int mpeg3packet_next_int24(mpeg3_demuxer_t *demuxer)
{
	unsigned int a, b, c, result;
	a = demuxer->raw_data[demuxer->raw_offset];
	b = demuxer->raw_data[demuxer->raw_offset + 1];
	c = demuxer->raw_data[demuxer->raw_offset + 2];
	result = (a << 16) | (b << 8) | c;

	return result;
}

static inline unsigned int mpeg3packet_read_int24(mpeg3_demuxer_t *demuxer)
{
	unsigned int a, b, c, result;
	a = demuxer->raw_data[demuxer->raw_offset++];
	b = demuxer->raw_data[demuxer->raw_offset++];
	c = demuxer->raw_data[demuxer->raw_offset++];
	result = (a << 16) | (b << 8) | c;

	return result;
}

static inline unsigned int mpeg3packet_read_int32(mpeg3_demuxer_t *demuxer)
{
	unsigned int a, b, c, d, result;
	a = demuxer->raw_data[demuxer->raw_offset++];
	b = demuxer->raw_data[demuxer->raw_offset++];
	c = demuxer->raw_data[demuxer->raw_offset++];
	d = demuxer->raw_data[demuxer->raw_offset++];
	result = (a << 24) | (b << 16) | (c << 8) | d;

	return result;
}

static inline unsigned int mpeg3packet_skip(mpeg3_demuxer_t *demuxer, long length)
{
	demuxer->raw_offset += length;
	return 0;
}

int mpeg3_get_adaptation_field(mpeg3_demuxer_t *demuxer)
{
	long length;
	int pcr_flag;

	demuxer->adaptation_fields++;
/* get adaptation field length */
	length = mpeg3packet_read_char(demuxer);
/* get first byte */
  	pcr_flag = (mpeg3packet_read_char(demuxer) >> 4) & 1;           

	if(pcr_flag)
	{
    	unsigned long clk_ref_base = mpeg3packet_read_int32(demuxer);
    	unsigned int clk_ref_ext = mpeg3packet_read_int16(demuxer);

		if (clk_ref_base > 0x7fffffff)
		{   /* correct for invalid numbers */
			clk_ref_base = 0;               /* ie. longer than 32 bits when multiplied by 2 */
			clk_ref_ext = 0;                /* multiplied by 2 corresponds to shift left 1 (<<=1) */
		}
		else 
		{
			clk_ref_base <<= 1; /* Create space for bit */
			clk_ref_base |= (clk_ref_ext >> 15);          /* Take bit */
			clk_ref_ext &= 0x01ff;                        /* Only lower 9 bits */
		}
		demuxer->time = clk_ref_base + clk_ref_ext / 300;
	    if(length) mpeg3packet_skip(demuxer, length - 7);
	}
	else
	mpeg3packet_skip(demuxer, length - 1);

	return 0;
}

int mpeg3_get_program_association_table(mpeg3_demuxer_t *demuxer)
{
	demuxer->program_association_tables++;
	demuxer->table_id = mpeg3packet_read_char(demuxer);
	demuxer->section_length = mpeg3packet_read_int16(demuxer) & 0xfff;
	demuxer->transport_stream_id = mpeg3packet_read_int16(demuxer);
	mpeg3packet_skip(demuxer, demuxer->raw_size - demuxer->raw_offset);
	return 0;
}

int mpeg3packet_get_data_buffer(mpeg3_demuxer_t *demuxer)
{
	while(demuxer->raw_offset < demuxer->raw_size && demuxer->data_size < demuxer->data_allocated)
	{
		demuxer->data_buffer[demuxer->data_size++] = demuxer->raw_data[demuxer->raw_offset++];
	}
	return 0;
}

int mpeg3_get_pes_packet_header(mpeg3_demuxer_t *demuxer, unsigned long *pts, unsigned long *dts)
{
	unsigned int pes_header_bytes = 0;
	unsigned int pts_dts_flags;
	int pes_header_data_length;

/* drop first 8 bits */
	mpeg3packet_read_char(demuxer);  
	pts_dts_flags = (mpeg3packet_read_char(demuxer) >> 6) & 0x3;
	pes_header_data_length = mpeg3packet_read_char(demuxer);

/* Get Presentation Time stamps and Decoding Time Stamps */
	if(pts_dts_flags == 2)
	{
		*pts = (mpeg3packet_read_char(demuxer) >> 1) & 7;  /* Only low 4 bits (7==1111) */
		*pts <<= 15;
		*pts |= (mpeg3packet_read_int16(demuxer) >> 1);
		*pts <<= 15;
		*pts |= (mpeg3packet_read_int16(demuxer) >> 1);
		pes_header_bytes += 5;
	}
	else if(pts_dts_flags == 3)
	{      
		*pts = (mpeg3packet_read_char(demuxer) >> 1) & 7;  /* Only low 4 bits (7==1111) */
		*pts <<= 15;
		*pts |= (mpeg3packet_read_int16(demuxer) >> 1);
		*pts <<= 15;
		*pts |= (mpeg3packet_read_int16(demuxer) >> 1);
		*dts = (mpeg3packet_read_char(demuxer) >> 1) & 7;  /* Only low 4 bits (7==1111) */
		*dts <<= 15;
		*dts |= (mpeg3packet_read_int16(demuxer) >> 1);
		*dts <<= 15;
		*dts |= (mpeg3packet_read_int16(demuxer) >> 1);
		pes_header_bytes += 10;
  }
/* extract other stuff here! */
  
	mpeg3packet_skip(demuxer, pes_header_data_length - pes_header_bytes);
	return 0;
}

int get_unknown_data(mpeg3_demuxer_t *demuxer)
{
	mpeg3packet_skip(demuxer, demuxer->raw_size - demuxer->raw_offset);
	return 0;
}

int mpeg3_get_pes_packet_data(mpeg3_demuxer_t *demuxer, unsigned int stream_id)
{
	unsigned long pts = 0, dts = 0;

	if((stream_id >> 4) == 12 || (stream_id >> 4) == 13)
	{
/* Just pick the first available stream if no ID is set */
		if(demuxer->astream == -1)
		    demuxer->astream = (stream_id & 0x0f);

    	if((stream_id & 0x0f) == demuxer->astream && demuxer->do_audio)
		{
			mpeg3_get_pes_packet_header(demuxer, &pts, &dts);
			demuxer->pes_audio_time = pts;
			demuxer->audio_pid = demuxer->pid;
			return mpeg3packet_get_data_buffer(demuxer);
    	}
	}
	else 
	if((stream_id >> 4)==14)
	{
/* Just pick the first available stream if no ID is set */
		if(demuxer->vstream == -1)
			demuxer->vstream = (stream_id & 0x0f);

		if((stream_id & 0x0f) == demuxer->vstream && demuxer->do_video)
		{
			mpeg3_get_pes_packet_header(demuxer, &pts, &dts);
			demuxer->pes_video_time = pts;
			demuxer->video_pid = demuxer->pid;
			return mpeg3packet_get_data_buffer(demuxer);
		}
	}
	else 
	{
		return get_unknown_data(demuxer);
	}

	mpeg3packet_skip(demuxer, demuxer->raw_size - demuxer->raw_offset);

	return 0;
}

int mpeg3_get_pes_packet(mpeg3_demuxer_t *demuxer)
{
	unsigned int stream_id;

	demuxer->pes_packets++;
	stream_id = mpeg3packet_read_char(demuxer);
/* Skip startcode */
	mpeg3packet_read_int24(demuxer);
/* Skip pes packet length */
	mpeg3packet_read_int16(demuxer);

	if(stream_id != MPEG3_PRIVATE_STREAM_2 && stream_id != MPEG3_PADDING_STREAM)
	{
		return mpeg3_get_pes_packet_data(demuxer, stream_id);
	}
	else
	if(stream_id == MPEG3_PRIVATE_STREAM_2)
	{
/* Dump private data! */
		fprintf(stderr, "stream_id == MPEG3_PRIVATE_STREAM_2\n");
		mpeg3packet_skip(demuxer, demuxer->raw_size - demuxer->raw_offset);
		return 0;
	}
	else
	if(stream_id == MPEG3_PADDING_STREAM)
	{
		mpeg3packet_skip(demuxer, demuxer->raw_size - demuxer->raw_offset);
		return 0;
	}
	else
	{
    	fprintf(stderr, "unknown stream_id in pes packet");
		return 1;
	}
	return 0;
}

int mpeg3_get_payload(mpeg3_demuxer_t *demuxer)
{
	if(demuxer->payload_unit_start_indicator)
	{
    	if(demuxer->pid==0) mpeg3_get_program_association_table(demuxer);
    	else 
		if(mpeg3packet_next_int24(demuxer) == MPEG3_PACKET_START_CODE_PREFIX) mpeg3_get_pes_packet(demuxer);
    	else 
		mpeg3packet_skip(demuxer, demuxer->raw_size - demuxer->raw_offset);
	}
	else
	{
    	if(demuxer->pid == demuxer->audio_pid && demuxer->do_audio)
		{
			mpeg3packet_get_data_buffer(demuxer);
		}
    	else 
		if(demuxer->pid == demuxer->video_pid && demuxer->do_video)
		{
			mpeg3packet_get_data_buffer(demuxer);
		}
    	else 
		mpeg3packet_skip(demuxer, demuxer->raw_size - demuxer->raw_offset);
	}
	return 0;
}

/* Read a transport packet */
int mpeg3_read_transport(mpeg3_demuxer_t *demuxer)
{
	mpeg3_title_t *title = demuxer->titles[demuxer->current_title];
	int result = mpeg3io_read_data(demuxer->raw_data, demuxer->packet_size, title->fs);
	unsigned int bits;
	int table_entry;

	demuxer->raw_size = demuxer->packet_size;
	demuxer->raw_offset = 0;
	if(result)
	{
		perror("mpeg3_read_transport");
		return 1;
	}

/* Sync byte */
	if(mpeg3packet_read_char(demuxer) != MPEG3_SYNC_BYTE)
	{
		fprintf(stderr, "mpeg3packet_read_char(demuxer) != MPEG3_SYNC_BYTE\n");
		return 1;
	}

/* 	bits = 	mpeg3packet_read_int24(demuxer) & 0x0000ffff; */
/* 	demuxer->transport_error_indicator = bits >> 15; */
/* 	demuxer->payload_unit_start_indicator = (bits >> 14) & 1; */
/* 	demuxer->pid = bits & 0x00001fff; */
/* 	demuxer->transport_scrambling_control = (mpeg3packet_next_char(demuxer) >> 6) & 0x3; */
/* 	demuxer->adaptation_field_control = (mpeg3packet_next_char(demuxer) >> 4) & 0x3; */
/* 	demuxer->continuity_counter = (mpeg3packet_read_char(demuxer) & 0xf); */

    bits =  mpeg3packet_read_int24(demuxer) & 0x00ffffff;
    demuxer->transport_error_indicator = (bits >> 23) & 0x1;
    demuxer->payload_unit_start_indicator = (bits >> 22) & 0x1;
    demuxer->pid = (bits >> 8) & 0x00001fff;
    demuxer->transport_scrambling_control = (bits >> 6) & 0x3;
    demuxer->adaptation_field_control = (bits >> 4) & 0x3;
    demuxer->continuity_counter = bits & 0xf;

	if(demuxer->transport_error_indicator)
	{
		fprintf(stderr, "demuxer->transport_error_indicator\n");
		return 1;
	}
 
    if (demuxer->pid == 0x1fff)
	{
		demuxer->is_padding = 1;  /* padding; just go to next */
		return 0;
    }
	else
	{
		demuxer->is_padding = 0;
	}

/* Get pid */
	for(table_entry = 0, result = 0; table_entry < demuxer->total_pids; table_entry++)
	{
		if(demuxer->pid == demuxer->pid_table[table_entry])
		{
			result = 1;
			break;
		}
	}

/* Not in pid table */
	if(!result)
	{
		demuxer->pid_table[table_entry] = demuxer->pid;
		demuxer->continuity_counters[table_entry] = demuxer->continuity_counter;  /* init */
		demuxer->total_pids++;
	}
	result = 0;

/* Check counters */
    if(demuxer->pid != MPEG3_PROGRAM_ASSOCIATION_TABLE && 
		demuxer->pid != MPEG3_CONDITIONAL_ACCESS_TABLE &&
        (demuxer->adaptation_field_control == 1 || demuxer->adaptation_field_control == 3))
	{
		if(demuxer->continuity_counters[table_entry] != demuxer->continuity_counter)
		{
			fprintf(stderr, "demuxer->continuity_counters[table_entry] != demuxer->continuity_counter\n");
/* Reset it */
			demuxer->continuity_counters[table_entry] = demuxer->continuity_counter;
		}
		if(++(demuxer->continuity_counters[table_entry]) > 15) demuxer->continuity_counters[table_entry] = 0;
	}

    if(demuxer->adaptation_field_control == 2 || demuxer->adaptation_field_control == 3)
    	result = mpeg3_get_adaptation_field(demuxer);

    if(demuxer->adaptation_field_control == 1 || demuxer->adaptation_field_control == 3)
    	result = mpeg3_get_payload(demuxer);

	return result;
}

int mpeg3_get_system_header(mpeg3_demuxer_t *demuxer)
{
	int length = mpeg3packet_read_int16(demuxer);
	mpeg3packet_skip(demuxer, length);
	return 0;
}

unsigned long mpeg3_get_timestamp(mpeg3_demuxer_t *demuxer)
{
	unsigned long timestamp;
/* Only low 4 bits (7==1111) */
	timestamp = (mpeg3packet_read_char(demuxer) >> 1) & 7;  
	timestamp <<= 15;
	timestamp |= (mpeg3packet_read_int16(demuxer) >> 1);
	timestamp <<= 15;
	timestamp |= (mpeg3packet_read_int16(demuxer) >> 1);
	return timestamp;
}

int mpeg3_get_pack_header(mpeg3_demuxer_t *demuxer, unsigned int *header)
{
	unsigned long i, j;
	unsigned long clock_ref, clock_ref_ext;

/* Get the time code */
	if((mpeg3packet_next_char(demuxer) >> 4) == 2)
	{
/* MPEG-1 */
			demuxer->time = (double)mpeg3_get_timestamp(demuxer) / 90000;
/* Skip 3 bytes */
			mpeg3packet_read_int24(demuxer);
	}
	else
	if(mpeg3packet_next_char(demuxer) & 0x40)
	{
		i = mpeg3packet_read_int32(demuxer);
		j = mpeg3packet_read_int16(demuxer);
		if(i & 0x40000000 || (i >> 28) == 2)
		{
    		clock_ref = ((i & 0x31000000) << 3);
    		clock_ref |= ((i & 0x03fff800) << 4);
    		clock_ref |= ((i & 0x000003ff) << 5);
    		clock_ref |= ((j & 0xf800) >> 11);
    		clock_ref_ext = (j >> 1) & 0x1ff;

   			demuxer->time = (double)(clock_ref + clock_ref_ext / 300) / 90000;
/* Skip 3 bytes */
			mpeg3packet_read_int24(demuxer);
			i = mpeg3packet_read_char(demuxer) & 0x7;

/* stuffing */
			mpeg3packet_skip(demuxer, i);  
		}
	}
	else
	{
		mpeg3packet_skip(demuxer, 2);
	}
	return 0;
}

/* Program packet reading core */
int mpeg3_get_ps_pes_packet(mpeg3_demuxer_t *demuxer, unsigned int *header)
{
	unsigned long pts = 0, dts = 0;
	int stream_id;
	int pes_packet_length;
	int pes_packet_start;
	int i;
	mpeg3_t *file = demuxer->file;

	stream_id = *header & 0xff;
	pes_packet_length = mpeg3packet_read_int16(demuxer);
	pes_packet_start = demuxer->raw_offset;

	if(stream_id != MPEG3_PRIVATE_STREAM_2 &&
		stream_id != MPEG3_PADDING_STREAM)
	{
		if((mpeg3packet_next_char(demuxer) >> 6) == 0x02)
		{
/* Get MPEG-2 packet */
			int pes_header_bytes = 0;
			int scrambling;
    		int pts_dts_flags;
			int pes_header_data_length;

			scrambling = mpeg3packet_read_char(demuxer) & 0x30;
/* Reset scrambling bit for the mpeg3cat utility */
			if(scrambling) demuxer->raw_data[demuxer->raw_offset - 1] &= 0xcf;
    		pts_dts_flags = (mpeg3packet_read_char(demuxer) >> 6) & 0x3;
			pes_header_data_length = mpeg3packet_read_char(demuxer);

			if(scrambling && (demuxer->do_audio || demuxer->do_video))
			{
/* Decrypt it */
				if(mpeg3_decrypt_packet(demuxer->titles[demuxer->current_title]->fs->css, 
					demuxer->raw_data))
				{
					fprintf(stderr, "mpeg3_get_ps_pes_packet: Decryption not available\n");
					return 1;
				}
				
			}

/* Get Presentation and Decoding Time Stamps */
			if(pts_dts_flags == 2)
			{
				pts = (mpeg3packet_read_char(demuxer) >> 1) & 7;  /* Only low 4 bits (7==1111) */
				pts <<= 15;
				pts |= (mpeg3packet_read_int16(demuxer) >> 1);
				pts <<= 15;
				pts |= (mpeg3packet_read_int16(demuxer) >> 1);
				pes_header_bytes += 5;
			}
    		else 
			if(pts_dts_flags == 3)
			{
        		pts = (mpeg3packet_read_char(demuxer) >> 1) & 7;  /* Only low 4 bits (7==1111) */
        		pts <<= 15;
        		pts |= (mpeg3packet_read_int16(demuxer) >> 1);
        		pts <<= 15;
        		pts |= (mpeg3packet_read_int16(demuxer) >> 1);
        		dts = (mpeg3packet_read_char(demuxer) >> 1) & 7;  /* Only low 4 bits (7==1111) */
        		dts <<= 15;
        		dts |= (mpeg3packet_read_int16(demuxer) >> 1);
        		dts <<= 15;
        		dts |= (mpeg3packet_read_int16(demuxer) >> 1);
        		pes_header_bytes += 10;
    		}

/* Skip unknown */
        	mpeg3packet_skip(demuxer, pes_header_data_length - pes_header_bytes);
		}
		else
		{
			int pts_dts_flags;
/* Get MPEG-1 packet */
			while(mpeg3packet_next_char(demuxer) == 0xff)
			{
				mpeg3packet_read_char(demuxer);
			}

/* Skip STD buffer scale */
			if((mpeg3packet_next_char(demuxer) & 0x40) == 0x40)
			{
				mpeg3packet_skip(demuxer, 2);
			}

/* Decide which timestamps are available */
			pts_dts_flags = mpeg3packet_next_char(demuxer);

			if(pts_dts_flags >= 0x30)
			{
/* Get the presentation and decoding time stamp */
				pts = mpeg3_get_timestamp(demuxer);
				dts = mpeg3_get_timestamp(demuxer);
			}
			else
			if(pts_dts_flags >= 0x20)
			{
/* Get just the presentation time stamp */
				pts = mpeg3_get_timestamp(demuxer);
			}
			else
			if(pts_dts_flags == 0x0f)
			{
/* End of timestamps */
				mpeg3packet_read_char(demuxer);
			}
			else
			{
				return 1;     /* Error */
			}
		}

/* Now extract the payload. */
		if((stream_id >> 4) == 0xc || (stream_id >> 4) == 0xd)
		{
/* Audio data */
/* Take first stream ID if -1 */
			pes_packet_length -= demuxer->raw_offset - pes_packet_start;
			if(demuxer->read_all)
				demuxer->astream_table[stream_id & 0x0f] = AUDIO_MPEG;
			else
			if(demuxer->astream == -1) 
				demuxer->astream = stream_id & 0x0f;

			if((stream_id & 0x0f) == demuxer->astream && demuxer->do_audio)
			{
				if(pts) demuxer->pes_audio_time = pts;

				memcpy(&demuxer->data_buffer[demuxer->data_size],
					&demuxer->raw_data[demuxer->raw_offset],
					pes_packet_length);
				demuxer->data_size += pes_packet_length;
				demuxer->raw_offset += pes_packet_length;
		  	}
			else 
			{
    			mpeg3packet_skip(demuxer, pes_packet_length);
			}
		}
    	else 
		if((stream_id >> 4) == 0xe)
		{
/* Video data */
/* Take first stream ID if -1 */
			if(demuxer->read_all) 
				demuxer->vstream_table[stream_id & 0x0f] = 1;
			else
			if(demuxer->vstream == -1) 
				demuxer->vstream = stream_id & 0x0f;

			pes_packet_length -= demuxer->raw_offset - pes_packet_start;
    	    if((stream_id & 0x0f) == demuxer->vstream && demuxer->do_video)
			{
        		if(pts) demuxer->pes_video_time = pts;

				memcpy(&demuxer->data_buffer[demuxer->data_size], 
					&demuxer->raw_data[demuxer->raw_offset],
					pes_packet_length);
				demuxer->data_size += pes_packet_length;
				demuxer->raw_offset += pes_packet_length;
    	  	}
    		else 
			{
        	    mpeg3packet_skip(demuxer, pes_packet_length);
    		}
    	}
    	else 
		if((stream_id == 0xbd || stream_id == 0xbf) && 
			mpeg3packet_next_char(demuxer) != 0xff &&
			((mpeg3packet_next_char(demuxer) & 0xf0) != 0x20))
		{
/* DVD audio data */
/* Get the audio format */
			int format;
//printf("mpeg3_get_ps_pes_packet 5 %x\n", mpeg3packet_next_char(demuxer) & 0xf0);
			if((mpeg3packet_next_char(demuxer) & 0xf0) == 0xa0)
				format = AUDIO_PCM;
			else
				format = AUDIO_AC3;

// Picks up bogus data if (& 0xf) or (& 0x7f)
			stream_id = mpeg3packet_next_char(demuxer);

//printf("mpeg3_get_ps_pes_packet %x\n", stream_id);
/* Take first stream ID if not building TOC. */
			if(demuxer->read_all)
				demuxer->astream_table[stream_id] = format;
			else
			if(demuxer->astream == -1)
				demuxer->astream = stream_id;

      		if(stream_id == demuxer->astream && demuxer->do_audio)
			{
				demuxer->aformat = format;
        		if(pts) demuxer->pes_audio_time = pts;
				mpeg3packet_read_int32(demuxer);
				pes_packet_length -= demuxer->raw_offset - pes_packet_start;

				memcpy(&demuxer->data_buffer[demuxer->data_size],
					&demuxer->raw_data[demuxer->raw_offset],
					pes_packet_length);
				demuxer->data_size += pes_packet_length;
				demuxer->raw_offset += pes_packet_length;
      		}
      		else
			{
				pes_packet_length -= demuxer->raw_offset - pes_packet_start;
        	    mpeg3packet_skip(demuxer, pes_packet_length);
      		}
//printf("mpeg3_get_ps_pes_packet 6 %d\n", demuxer->astream_table[0x20]);
    	}
    	else 
		if(stream_id == 0xbc || 1)
		{
			pes_packet_length -= demuxer->raw_offset - pes_packet_start;
        	mpeg3packet_skip(demuxer, pes_packet_length);
    	}
	}
  	else 
	if(stream_id == MPEG3_PRIVATE_STREAM_2 || stream_id == MPEG3_PADDING_STREAM)
	{
		pes_packet_length -= demuxer->raw_offset - pes_packet_start;
        mpeg3packet_skip(demuxer, pes_packet_length);
  	}
	return 0;
}

int mpeg3_read_program(mpeg3_demuxer_t *demuxer)
{
	int result = 0, count = 0;
	mpeg3_t *file = demuxer->file;
	mpeg3_title_t *title = demuxer->titles[demuxer->current_title];
	unsigned int header;
	demuxer->raw_size = demuxer->packet_size;
	demuxer->raw_offset = 0;
	demuxer->data_size = 0;

/* Search backward for it. */
	header = mpeg3io_read_int32(title->fs);
	result = mpeg3io_eof(title->fs);

	if(!result) result = mpeg3io_seek_relative(title->fs, -4);
//printf("mpeg3_read_program 1 %lx\n", mpeg3io_tell(title->fs));

// Search backwards for header
	while(header != MPEG3_PACK_START_CODE && !result && count < demuxer->packet_size)
	{
		result = mpeg3io_seek_relative(title->fs, -1);
		if(!result)
		{
			header >>= 8;
			header |= mpeg3io_read_char(title->fs) << 24;
			result = mpeg3io_seek_relative(title->fs, -1);
		}
		count++;
	}

	if(result)
	{
// couldn't find MPEG3_PACK_START_CODE
		return 1;
	}

	result = mpeg3io_read_data(demuxer->raw_data, demuxer->packet_size, title->fs);
//printf("mpeg3_read_program 2 %d %lx\n", demuxer->packet_size, mpeg3io_tell(title->fs));

	if(result)
	{
		perror("mpeg3_read_program");
		return 1;
	}

	while(demuxer->raw_offset + 4 < demuxer->raw_size && !result)
	{
		header = mpeg3packet_read_int32(demuxer);

		if(header == MPEG3_PACK_START_CODE)
		{
			if(demuxer->raw_offset < 5)
			{
				result = mpeg3_get_pack_header(demuxer, &header);
			}
			else
			{
				mpeg3io_seek_relative(title->fs, demuxer->raw_offset - demuxer->raw_size - 4);
				demuxer->raw_offset = demuxer->raw_size;
				break;
			}
		}
		else
		if(header == MPEG3_SYSTEM_START_CODE)
		{
 			mpeg3_get_system_header(demuxer);
		}
		else
		if((header >> 8) == MPEG3_PACKET_START_CODE_PREFIX)
		{
			result = mpeg3_get_ps_pes_packet(demuxer, &header);
		}
		else
			demuxer->raw_offset -= 3;
	}
	return result;
}

double mpeg3_lookup_time_offset(mpeg3_demuxer_t *demuxer, long byte)
{
	int i;
	mpeg3_title_t *title = demuxer->titles[demuxer->current_title];

	if(!title->timecode_table_size) return 0;

	for(i = title->timecode_table_size - 1; 
		i >= 0 && title->timecode_table[i].start_byte > byte;
		i--)
		;
	if(i < 0) i = 0;
	return title->timecode_table[i].absolute_start_time - title->timecode_table[i].start_time;
}

int mpeg3_advance_timecode(mpeg3_demuxer_t *demuxer, int reverse)
{
	mpeg3_title_t *title = demuxer->titles[demuxer->current_title];
	int result = 0;
	int do_seek = 0;
	int last_timecode = demuxer->current_timecode;
//printf("mpeg3_advance_timecode 0\n");fflush(stdout);

/* Skip timecode advancing when constructing timecode table */
	if(!title->timecode_table || 
		!title->timecode_table_size || 
		demuxer->read_all) return 0;

//printf("mpeg3_advance_timecode 1\n");fflush(stdout);
	if(!reverse)
	{
/* Get inside the current timecode */
		if(mpeg3io_tell(title->fs) < title->timecode_table[demuxer->current_timecode].start_byte)
		{
			mpeg3io_seek(title->fs, title->timecode_table[demuxer->current_timecode].start_byte);
		}

/* Get the next timecode */
		while(!result && 
			(mpeg3io_tell(title->fs) >= title->timecode_table[demuxer->current_timecode].end_byte ||
				demuxer->current_program != title->timecode_table[demuxer->current_timecode].program))
		{
			demuxer->current_timecode++;
			if(demuxer->current_timecode >= title->timecode_table_size)
			{
				demuxer->current_timecode = 0;
				if(demuxer->current_title + 1 < demuxer->total_titles)
				{
					mpeg3demux_open_title(demuxer, ++demuxer->current_title);
					do_seek = 1;
				}
				else
				{
					mpeg3io_seek(title->fs, mpeg3io_total_bytes(title->fs));
			 		result = 1;
				}
			}
			title = demuxer->titles[demuxer->current_title];
		}

//if(last_timecode != demuxer->current_timecode && demuxer->do_video)
//	printf("using title %d timecode %x %x\n", demuxer->current_title, title->timecode_table[demuxer->current_timecode].start_byte, title->timecode_table[demuxer->current_timecode].end_byte);

//printf("2 %d\n", title->timecode_table[demuxer->current_timecode].program);
		if(!result && do_seek)
		{
			mpeg3io_seek(title->fs, title->timecode_table[demuxer->current_timecode].start_byte);
		}
	}
	else
	{
/* Get the previous timecode */
		while(!result && 
			(mpeg3io_tell(title->fs) < title->timecode_table[demuxer->current_timecode].start_byte ||
				demuxer->current_program != title->timecode_table[demuxer->current_timecode].program))
		{
/*
 * printf("mpeg3_reverse_timecode %d %d %d %d\n", 
 * 	mpeg3io_tell(title->fs), 
 * 	demuxer->current_timecode,
 * 	title->timecode_table[demuxer->current_timecode].start_byte,
 *  title->timecode_table[demuxer->current_timecode].end_byte);
 */

			demuxer->current_timecode--;
			if(demuxer->current_timecode < 0)
			{
				if(demuxer->current_title > 0)
				{
//printf("advance_timecode 2 %d\n", demuxer->current_title);
					mpeg3demux_open_title(demuxer, --demuxer->current_title);
					title = demuxer->titles[demuxer->current_title];
// Seek to end since we opened at the beginning of the next title
					mpeg3io_seek(title->fs, title->total_bytes - demuxer->packet_size);
//printf("advance_timecode 3 %d %d\n", demuxer->current_title, mpeg3io_tell(title->fs));
					demuxer->current_timecode = title->timecode_table_size - 1;
					do_seek = 1;
				}
				else
				{
					mpeg3io_seek(title->fs, 0);
					demuxer->current_timecode = 0;
					result = 1;
				}
			}
		}

		if(!result && do_seek) 
			mpeg3io_seek(title->fs, title->timecode_table[demuxer->current_timecode].start_byte);
	}

//printf("mpeg3_advance_timecode 2 %d\n", demuxer->current_title);fflush(stdout);
	return result;
}

/* Read packet in the forward direction */
int mpeg3_read_next_packet(mpeg3_demuxer_t *demuxer)
{
	int result = 0;
	long current_position;
	mpeg3_t *file = demuxer->file;
	mpeg3_title_t *title = demuxer->titles[demuxer->current_title];
	demuxer->data_size = 0;
	demuxer->data_position = 0;

//printf("mpeg3_read_next_packet 1 %d %lx\n", demuxer->current_title, mpeg3io_tell(title->fs));
/* Flip the file descriptor back to the end of the packet for forward */
/* reading. */
	if(demuxer->reverse)
	{
		result = mpeg3io_seek_relative(title->fs, demuxer->packet_size);
		demuxer->reverse = 0;
	}

/* Read packets until the output buffer is full */
	if(!result)
	{
		do
		{
			result = mpeg3_advance_timecode(demuxer, 0);

			if(!result)
			{
				demuxer->time_offset = mpeg3_lookup_time_offset(demuxer, mpeg3io_tell(title->fs));

				if(file->is_transport_stream)
				{
					result = mpeg3_read_transport(demuxer);
				}
				else
				if(file->is_program_stream)
				{
					result = mpeg3_read_program(demuxer);
				}
				else
				{
/* Read elementary stream. */
					result = mpeg3io_read_data(demuxer->data_buffer, demuxer->packet_size, title->fs);
					if(!result) demuxer->data_size = demuxer->packet_size;
				}
			}
//printf("mpeg3_read_next_packet 2 %d %lx\n", demuxer->current_title, mpeg3io_tell(title->fs));
		}while(!result && 
			demuxer->data_size == 0 && 
			(demuxer->do_audio || demuxer->do_video));
	}

//printf("mpeg3_read_next_packet 4\n");
	return result;
}

/* Read the packet right before the packet we're currently on. */
int mpeg3_read_prev_packet(mpeg3_demuxer_t *demuxer)
{
	int result = 0;
	mpeg3_t *file = demuxer->file;
	long current_position;
	mpeg3_title_t *title = demuxer->titles[demuxer->current_title];

	demuxer->data_size = 0;
	demuxer->data_position = 0;

//printf("mpeg3_read_prev_packet 1 %d %d\n", demuxer->current_title, demuxer->current_timecode);
	do
	{
/* Rewind to the start of the packet to be read. */
		result = mpeg3io_seek_relative(title->fs, -demuxer->packet_size);

//printf("mpeg3_read_prev_packet 1 %p\n", title->fs->fd);
		if(!result) result = mpeg3_advance_timecode(demuxer, 1);
//printf("mpeg3_read_prev_packet 2 %p->%p->%p\n", title, title->fs, title->fs->fd);
		if(!result) demuxer->time_offset = mpeg3_lookup_time_offset(demuxer, mpeg3io_tell(title->fs));

		if(file->is_transport_stream && !result)
		{
			result = mpeg3_read_transport(demuxer);
			if(!mpeg3io_bof(title->fs))
			/* if(!result)  */result = mpeg3io_seek_relative(title->fs, -demuxer->packet_size);
		}
		else
		if(file->is_program_stream && !result)
		{

			result = mpeg3_read_program(demuxer);
			if(!mpeg3io_bof(title->fs))
			result = mpeg3io_seek_relative(title->fs, -demuxer->packet_size);
		}
		else
		if(!result)
		{
/* Elementary stream */
/* Read the packet forwards and seek back to the start */
			result = mpeg3io_read_data(demuxer->data_buffer, demuxer->packet_size, title->fs);
			if(!result) 
			{
				demuxer->data_size = demuxer->packet_size;
				result = mpeg3io_seek_relative(title->fs, -demuxer->packet_size);
			}
		}
	}while(!result && demuxer->data_size == 0 && (demuxer->do_audio || demuxer->do_video));
//printf("mpeg3_read_prev_packet 2 %d %d\n", demuxer->current_title, demuxer->current_timecode);

/* Remember that the file descriptor is at the beginning of the packet just read. */
	demuxer->reverse = 1;
	demuxer->error_flag = result;
//printf("mpeg3_read_prev_packet 2\n");
	return result;
}


/* Used for audio */
int mpeg3demux_read_data(mpeg3_demuxer_t *demuxer, 
		unsigned char *output, 
		long size)
{
	long i;
	int result = 0;
	mpeg3_t *file = demuxer->file;
	demuxer->error_flag = 0;
	
	if(demuxer->data_position >= 0)
	{
/* Read forwards */
		for(i = 0; i < size && !result; )
		{
			int fragment_size = size - i;
			if(fragment_size > demuxer->data_size - demuxer->data_position)
				fragment_size = demuxer->data_size - demuxer->data_position;
			memcpy(output + i, demuxer->data_buffer + demuxer->data_position, fragment_size);
			demuxer->data_position += fragment_size;
			i += fragment_size;

			if(i < size)
			{
				result = mpeg3_read_next_packet(demuxer);
			}
		}
	}
	else
	{
/* Read backwards a full packet. */
/* Only good for reading less than the size of a full packet, but */
/* this routine should only be used for searching for previous markers. */
		long current_position = demuxer->data_position;
		result = mpeg3_read_prev_packet(demuxer);
		if(!result) demuxer->data_position = demuxer->data_size + current_position;
		memcpy(output, demuxer->data_buffer + demuxer->data_position, size);
		demuxer->data_position += size;
	}

	demuxer->error_flag = result;
	return result;
}

unsigned int mpeg3demux_read_char_packet(mpeg3_demuxer_t *demuxer)
{
	demuxer->error_flag = 0;
	if(demuxer->data_position >= demuxer->data_size)
		demuxer->error_flag = mpeg3_read_next_packet(demuxer);
	demuxer->next_char = demuxer->data_buffer[demuxer->data_position++];
	return demuxer->next_char;
}

unsigned int mpeg3demux_read_prev_char_packet(mpeg3_demuxer_t *demuxer)
{
	demuxer->error_flag = 0;
	demuxer->data_position--;
	if(demuxer->data_position < 0)
	{
		demuxer->error_flag = mpeg3_read_prev_packet(demuxer);
		if(!demuxer->error_flag) demuxer->data_position = demuxer->data_size - 1;
	}
	demuxer->next_char = demuxer->data_buffer[demuxer->data_position];
	return demuxer->next_char;
}

mpeg3demux_timecode_t* mpeg3demux_next_timecode(mpeg3_demuxer_t *demuxer, 
		int *current_title, 
		int *current_timecode,
		int current_program)
{
	int done = 0;
	while(!done)
	{
/* Increase timecode number */
		if(*current_timecode < demuxer->titles[*current_title]->timecode_table_size - 1) 
		{
			(*current_timecode)++;
			if(demuxer->titles[*current_title]->timecode_table[*current_timecode].program == current_program)
				return &(demuxer->titles[*current_title]->timecode_table[*current_timecode]);
		}
		else
/* Increase title number */
		if(*current_title < demuxer->total_titles - 1)
		{
			(*current_title)++;
			(*current_timecode) = 0;
			if(demuxer->titles[*current_title]->timecode_table[*current_timecode].program == current_program)
				return &(demuxer->titles[*current_title]->timecode_table[*current_timecode]);
		}
		else
/* End of disk */
			done = 1;
	}
	return 0;
}

mpeg3demux_timecode_t* mpeg3demux_prev_timecode(mpeg3_demuxer_t *demuxer, 
		int *current_title, 
		int *current_timecode,
		int current_program)
{
	int done = 0;
	while(!done)
	{
/* Increase timecode number */
		if(*current_timecode > 0)
		{
			(*current_timecode)--;
			if(demuxer->titles[*current_title]->timecode_table[*current_timecode].program == current_program)
				return &(demuxer->titles[*current_title]->timecode_table[*current_timecode]);
		}
		else
/* Increase title number */
		if(*current_title > 0)
		{
			(*current_title)--;
			(*current_timecode) = demuxer->titles[*current_title]->timecode_table_size - 1;
			if(demuxer->titles[*current_title]->timecode_table[*current_timecode].program == current_program)
				return &(demuxer->titles[*current_title]->timecode_table[*current_timecode]);
		}
		else
/* End of disk */
			done = 1;
		
	}
	return 0;
}

int mpeg3demux_open_title(mpeg3_demuxer_t *demuxer, int title_number)
{
	mpeg3_title_t *title;

	if(title_number < demuxer->total_titles)
	{
		if(demuxer->current_title >= 0)
		{
			mpeg3io_close_file(demuxer->titles[demuxer->current_title]->fs);
			demuxer->current_title = -1;
		}

		title = demuxer->titles[title_number];
		if(mpeg3io_open_file(title->fs))
		{
			demuxer->error_flag = 1;
			perror("mpeg3demux_open_title");
		}
		else
		{
			demuxer->current_title = title_number;
		}
	}

//	demuxer->current_timecode = 0;

	return demuxer->error_flag;
}

/* Assign program numbers to interleaved programs */
int mpeg3demux_assign_programs(mpeg3_demuxer_t *demuxer)
{
	int current_program = 0;
	int current_title = 0;
	int current_timecode = 0;
	double current_time;
	mpeg3demux_timecode_t *timecode;
	int total_programs = 1;
	int i, j;
	int program_exists, last_program_assigned = 0;
	int total_timecodes;
	mpeg3_title_t **titles = demuxer->titles;

	for(i = 0, total_timecodes = 0; i < demuxer->total_titles; i++)
	{
		total_timecodes += demuxer->titles[i]->timecode_table_size;
		for(j = 0; j < demuxer->titles[i]->timecode_table_size; j++)
		{
			timecode = &demuxer->titles[i]->timecode_table[j];
			if(timecode->program > total_programs - 1)
				total_programs = timecode->program + 1;
		}
	}

/* Assign absolute timecodes in each program. */
	for(current_program = 0; 
		current_program < total_programs; 
		current_program++)
	{
		current_time = 0;
		current_title = 0;
		current_timecode = -1;
		while(timecode = mpeg3demux_next_timecode(demuxer, 
		    &current_title, 
			&current_timecode, 
			current_program))
		{
			timecode->absolute_start_time = current_time;
			current_time += timecode->end_time - timecode->start_time;
			timecode->absolute_end_time = current_time;
		}
	}

//for(i = 0; i < demuxer->total_titles; i++) mpeg3_dump_title(demuxer->titles[i]);

	demuxer->current_program = 0;
	return 0;
}

int mpeg3demux_copy_titles(mpeg3_demuxer_t *dst, mpeg3_demuxer_t *src)
{
	long i;
	mpeg3_t *file = dst->file;
	mpeg3_title_t *dst_title, *src_title;

	dst->packet_size = src->packet_size;
	dst->total_titles = src->total_titles;
	dst->total_programs = src->total_programs;
	for(i = 0; i < MPEG3_MAX_STREAMS; i++)
	{
		dst->astream_table[i] = src->astream_table[i];
		dst->vstream_table[i] = src->vstream_table[i];
	}
	for(i = 0; i < src->total_titles; i++)
	{
		src_title = src->titles[i];
		dst_title = dst->titles[i] = mpeg3_new_title(file, src->titles[i]->fs->path);
		mpeg3_copy_title(dst_title, src_title);
	}

	mpeg3demux_open_title(dst, src->current_title);
	dst->current_timecode = 0;
	return 0;
}

/* ==================================================================== */
/*                            Entry points */
/* ==================================================================== */

mpeg3_demuxer_t* mpeg3_new_demuxer(mpeg3_t *file, int do_audio, int do_video, int stream_id)
{
	mpeg3_demuxer_t *demuxer = calloc(1, sizeof(mpeg3_demuxer_t));
	int i;

/* The demuxer will change the default packet size for its own use. */
	demuxer->file = file;
	demuxer->packet_size = file->packet_size;
	demuxer->do_audio = do_audio;
	demuxer->do_video = do_video;

/* Allocate buffer + padding */
	demuxer->raw_data = calloc(1, MPEG3_MAX_PACKSIZE);
	demuxer->data_buffer = calloc(1, MPEG3_MAX_PACKSIZE);
	demuxer->data_allocated = MPEG3_MAX_PACKSIZE;
/* System specific variables */
	demuxer->audio_pid = stream_id;
	demuxer->video_pid = stream_id;
	demuxer->astream = stream_id;
	demuxer->vstream = stream_id;
	demuxer->current_title = -1;
	return demuxer;
}

int mpeg3_delete_demuxer(mpeg3_demuxer_t *demuxer)
{
	int i;

	if(demuxer->current_title >= 0)
	{
		mpeg3io_close_file(demuxer->titles[demuxer->current_title]->fs);
	}

	for(i = 0; i < demuxer->total_titles; i++)
	{
		mpeg3_delete_title(demuxer->titles[i]);
	}

	if(demuxer->data_buffer) free(demuxer->data_buffer);
	free(demuxer->raw_data);
	free(demuxer);
	return 0;
}

/* Need a timecode table to do this */
double mpeg3demux_length(mpeg3_demuxer_t *demuxer)
{
	mpeg3_title_t *title;
	int i, j;
	double length;
	
	for(i = demuxer->total_titles - 1; i >= 0; i--)
	{
		title = demuxer->titles[i];
		for(j = title->timecode_table_size - 1; j >= 0; j--)
		{
			if(title->timecode_table[j].program == demuxer->current_program)
			{
				return title->timecode_table[j].end_time - 
					title->timecode_table[j].start_time + 
					title->timecode_table[j].absolute_start_time;
			}
		}
	}

	return 1;
}

int mpeg3demux_eof(mpeg3_demuxer_t *demuxer)
{
	if(demuxer->current_title >= 0)
	{
		if(mpeg3io_eof(demuxer->titles[demuxer->current_title]->fs) &&
			demuxer->current_title >= demuxer->total_titles - 1)
			return 1;
	}

	return 0;
}

int mpeg3demux_bof(mpeg3_demuxer_t *demuxer)
{
	if(demuxer->current_title >= 0)
	{
		if(mpeg3io_bof(demuxer->titles[demuxer->current_title]->fs) &&
			demuxer->current_title <= 0)
			return 1;
	}
	return 0;
}


/* Seek to a byte in the current title */
int mpeg3demux_seek_byte(mpeg3_demuxer_t *demuxer, long byte)
{
	long current_position;
	mpeg3_t *file = demuxer->file;
	mpeg3_title_t *title = demuxer->titles[demuxer->current_title];
	
	demuxer->data_position = 0;
	demuxer->data_size = 0;

	demuxer->error_flag = mpeg3io_seek(title->fs, byte);

	if(!demuxer->error_flag && (file->is_transport_stream || file->is_program_stream))
	{
/* Get on a packet boundary only for system streams. */
		current_position = mpeg3io_tell(title->fs);
		if(byte % demuxer->packet_size)
		{
			demuxer->error_flag |= mpeg3io_seek(title->fs, current_position - (current_position % demuxer->packet_size));
		}
	}
	return demuxer->error_flag;
}

int mpeg3demux_seek_end(mpeg3_demuxer_t *demuxer)
{
	mpeg3demux_open_title(demuxer, demuxer->total_titles - 1);
	demuxer->current_timecode = 0;
	return mpeg3demux_seek_byte(demuxer, demuxer->titles[demuxer->current_title]->total_bytes);
}

int mpeg3demux_seek_start(mpeg3_demuxer_t *demuxer)
{
	mpeg3demux_open_title(demuxer, 0);
	demuxer->current_timecode = 0;
	return mpeg3demux_seek_byte(demuxer, 0);
}

/* For programs streams and toc seek to a time */
int mpeg3demux_seek_time(mpeg3_demuxer_t *demuxer, double new_time)
{
	int i, j, done = 0, result = 0;
	double byte_offset, new_byte_offset;
	double guess = 0, minimum = 65535;
	mpeg3_title_t *title;
	mpeg3demux_timecode_t *timecode;

	demuxer->error_flag = 0;

	i = 0;
	j = 0;
	title = demuxer->titles[i];
	timecode = &title->timecode_table[j];


/* Get the title and timecode of the new position */
	while(!demuxer->error_flag &&
		!(timecode->absolute_start_time <= new_time &&
		timecode->absolute_end_time > new_time &&
		timecode->program == demuxer->current_program))
	{
/* Next timecode */
		j++;
		if(j >= title->timecode_table_size)
		{
			i++;
			j = 0;
			if(i >= demuxer->total_titles)
			{
				demuxer->error_flag = 1;
				return 1;
			}
/*
 * 			else
 * 			{
 * 				mpeg3demux_open_title(demuxer, i);
 * 			}
 */
		}

		title = demuxer->titles[i];
		timecode = &title->timecode_table[j];
	}

//printf("mpeg3demux_seek_time %d %f %f %f\n", i, timecode->absolute_start_time, timecode->absolute_end_time, new_time);
	if(demuxer->current_title != i)
    	mpeg3demux_open_title(demuxer, i);

/* Guess the new byte position */
	demuxer->current_timecode = j;

	byte_offset = ((new_time - timecode->absolute_start_time) /
		(timecode->absolute_end_time - timecode->absolute_start_time) *
		(timecode->end_byte - timecode->start_byte) +
		timecode->start_byte);
//printf("mpeg3demux_seek_time %f %f\n", new_time, byte_offset);

	while(!done && !result && byte_offset >= 0)
	{
		result = mpeg3demux_seek_byte(demuxer, (long)byte_offset);
//printf("seek_time 0 byte %.0f want %f result %d\n", byte_offset, new_time, result); 

		if(!result)
		{
			result = mpeg3_read_next_packet(demuxer);
// printf("seek_time 1 guess %f want %f\n", guess, new_time); 
			guess = demuxer->time + demuxer->time_offset;

			if(fabs(new_time - guess) >= fabs(minimum)) done = 1;
			else
			{
				minimum = guess - new_time;
				new_byte_offset = byte_offset + ((new_time - guess) / 
					(timecode->end_time - timecode->start_time) *
					(timecode->end_byte - timecode->start_byte));
				if(labs((long)new_byte_offset - (long)byte_offset) < demuxer->packet_size) done = 1;
				byte_offset = new_byte_offset;
			}
		}
	}

/* Get one packet before the packet just read */
	if(!result && byte_offset > demuxer->packet_size && minimum > 0)
	{
		mpeg3_read_prev_packet(demuxer);
		mpeg3_read_prev_packet(demuxer);
	}
//printf("seek_time %d %d %d\n", demuxer->current_title, demuxer->current_timecode, mpeg3demux_tell(demuxer));
	demuxer->error_flag = result;
	return result;
}

int mpeg3demux_seek_percentage(mpeg3_demuxer_t *demuxer, double percentage)
{
	double total_bytes = 0;
	double absolute_position;
	long relative_position;
	int i, new_title;
	mpeg3_title_t *title;

	demuxer->error_flag = 0;

/* Get the absolute byte position; */
	for(i = 0; i < demuxer->total_titles; i++)
		total_bytes += demuxer->titles[i]->total_bytes;

	absolute_position = percentage * total_bytes;

/* Get the title the byte is inside */
	for(new_title = 0, total_bytes = 0; new_title < demuxer->total_titles; new_title++)
	{
		total_bytes += demuxer->titles[new_title]->total_bytes;
		if(absolute_position < total_bytes) break;
	}

	if(new_title >= demuxer->total_titles)
	{
		new_title = demuxer->total_titles - 1;
	}

/* Got a title */
	title = demuxer->titles[new_title];
	total_bytes -= title->total_bytes;
	relative_position = (long)(absolute_position - total_bytes);

/* Get the timecode the byte is inside */
	for(demuxer->current_timecode = 0; 
		demuxer->current_timecode < title->timecode_table_size; 
		demuxer->current_timecode++)
	{
		if(title->timecode_table[demuxer->current_timecode].start_byte <= relative_position &&
			title->timecode_table[demuxer->current_timecode].end_byte > relative_position)
		{
			break;
		}
	}

	if(demuxer->current_timecode >= title->timecode_table_size)
		demuxer->current_timecode = title->timecode_table_size - 1;

/* Get the nearest timecode in the same program */
	while(demuxer->current_timecode < title->timecode_table_size - 1 &&
			title->timecode_table[demuxer->current_timecode].program != demuxer->current_program)
	{
		demuxer->current_timecode++;
	}

/*
 * printf("seek percentage 1 %d %d %d %d\n", demuxer->current_title, 
 * 	demuxer->current_timecode,
 * 	title->timecode_table[demuxer->current_timecode].start_byte,
 * 	title->timecode_table[demuxer->current_timecode].end_byte);
 */

/* Open the new title and seek to the correct byte */
	if(new_title != demuxer->current_title)
	{
		demuxer->error_flag = mpeg3demux_open_title(demuxer, new_title);
	}

/*
 * printf("seek percentage 2 %d %d %d %d\n", demuxer->current_title, 
 * 	demuxer->current_timecode,
 * 	title->timecode_table[demuxer->current_timecode].start_byte,
 * 	title->timecode_table[demuxer->current_timecode].end_byte);
 */

	if(!demuxer->error_flag)
		demuxer->error_flag = mpeg3io_seek(title->fs, relative_position);

	return demuxer->error_flag;
}

double mpeg3demux_tell_percentage(mpeg3_demuxer_t *demuxer)
{
	double total_bytes = 0;
	double position = 0;
	int i;

	demuxer->error_flag = 0;
	position = mpeg3io_tell(demuxer->titles[demuxer->current_title]->fs);
	for(i = 0; i < demuxer->total_titles; i++)
	{
		if(i == demuxer->current_title)
		{
			position += total_bytes;
		}
		total_bytes += demuxer->titles[i]->total_bytes;
	}
	return position / total_bytes;
}

double mpeg3demux_get_time(mpeg3_demuxer_t *demuxer)
{
	return demuxer->time;
}

long mpeg3demux_tell(mpeg3_demuxer_t *demuxer)
{
	return mpeg3io_tell(demuxer->titles[demuxer->current_title]->fs);
}

long mpeg3demuxer_total_bytes(mpeg3_demuxer_t *demuxer)
{
	mpeg3_title_t *title = demuxer->titles[demuxer->current_title];
	return title->total_bytes;
}

mpeg3_demuxer_t* mpeg3_get_demuxer(mpeg3_t *file)
{
	if(file->is_program_stream || file->is_transport_stream)
	{
		if(file->has_audio) return file->atrack[0]->demuxer;
		else
		if(file->has_video) return file->vtrack[0]->demuxer;
	}
	return 0;
}
