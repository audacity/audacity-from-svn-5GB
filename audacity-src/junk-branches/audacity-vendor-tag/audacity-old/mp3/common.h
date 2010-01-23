/**********************************************************************
Copyright (c) 1991 MPEG/audio software simulation group, All Rights Reserved
common.h
**********************************************************************/
/**********************************************************************
 * MPEG/audio coding/decoding software, work in progress              *
 *   NOT for public distribution until verified and approved by the   *
 *   MPEG/audio committee.  For further information, please contact   *
 *   Chad Fogg email: <cfogg@xenon.com>                               *
 *                                                                    *
 * VERSION 4.1                                                        *
 *   changes made since last update:                                  *
 *   date   programmers         comment                               *
 * 2/25/91  Doulas Wong,        start of version 1.0 records          *
 *          Davis Pan                                                 *
 * 5/10/91  W. Joseph Carter    Reorganized & renamed all ".h" files  *
 *                              into "common.h" and "encoder.h".      *
 *                              Ported to Macintosh and Unix.         *
 *                              Added additional type definitions for *
 *                              AIFF, double/SANE and "bitstream.c".  *
 *                              Added function prototypes for more    *
 *                              rigorous type checking.               *
 * 27jun91  dpwe (Aware)        Added "alloc_*" defs & prototypes     *
 *                              Defined new struct 'frame_params'.    *
 *                              Changed info.stereo to info.mode_ext  *
 *                              #define constants for mode types      *
 *                              Prototype arguments if PROTO_ARGS     *
 * 5/28/91  Earle Jennings      added MS_DOS definition               *
 *                              MsDos function prototype declarations *
 * 7/10/91  Earle Jennings      added FLOAT definition as double      *
 *10/ 3/91  Don H. Lee          implemented CRC-16 error protection   *
 * 2/11/92  W. Joseph Carter    Ported new code to Macintosh.  Most   *
 *                              important fixes involved changing     *
 *                              16-bit ints to long or unsigned in    *
 *                              bit alloc routines for quant of 65535 *
 *                              and passing proper function args.     *
 *                              Removed "Other Joint Stereo" option   *
 *                              and made bitrate be total channel     *
 *                              bitrate, irrespective of the mode.    *
 *                              Fixed many small bugs & reorganized.  *
 *                              Modified some function prototypes.    *
 *                              Changed BUFFER_SIZE back to 4096.     *
 * 7/27/92  Michael Li          (re-)Ported to MS-DOS                 *
 * 7/27/92  Masahiro Iwadare    Ported to Convex                      *
 * 8/07/92  mc@tv.tek.com                                             *
 * 8/10/92  Amit Gulati         Ported to the AIX Platform (RS6000)   *
 *                              AIFF string constants redefined       *
 * 8/27/93 Seymour Shlien,      Fixes in Unix and MSDOS ports,        *
 *         Daniel Lauzon, and                                         *
 *         Bill Truerniet                                             *
 *--------------------------------------------------------------------*
 * 4/23/92  J. Pineda           Added code for Layer III.             *
 * 11/9/92  Amit Gulati         Added defines for layerIII stereo     *
 *                              modes.                                *
 *  8/24/93 Masahiro Iwadare    Included IS modification in Layer III.*
 *                              Changed for 1 pass decoding.          *
 *  9/07/93 Toshiyuki Ishino    Integrated Layer III with Ver 3.9.    *
 *--------------------------------------------------------------------*
 * 11/20/93 Masahiro Iwadare    Integrated Layer III with Ver 4.0.    *
 *--------------------------------------------------------------------*
 *  7/14/94 Juergen Koller      Fix for HPUX an IRIX in AIFF-Strings  *
 **********************************************************************/

/***********************************************************************
*
*  Global Conditional Compile Switches
*
***********************************************************************/

/* #define      UNIX            /* Unix conditional compile switch */
/* #define      MACINTOSH       /* Macintosh conditional compile switch */
/* #define      MS_DOS          /* IBM PC conditional compile switch */
/* #define      MSC60           /* Compiled for MS_DOS with MSC v6.0 */
/* #define      AIX             /* AIX conditional compile switch    */
/* #define      CONVEX          /* CONVEX conditional compile switch */

#if defined(MSC60) 
#ifndef MS_DOS
#define MS_DOS
#endif
#ifndef PROTO_ARGS
#define PROTO_ARGS
#endif
#endif

#ifdef  UNIX
#define         TABLES_PATH     "tables"  /* to find data files */
/* name of environment variable holding path of table files */
#define         MPEGTABENV      "MPEGTABLES"
#define         PATH_SEPARATOR  "/"        /* how to build paths */
#endif  /* UNIX */

#ifdef  MACINTOSH
/* #define      TABLES_PATH ":tables:"  /* where to find data files */
#endif  /* MACINTOSH */

/* 
 * Don't define FAR to far unless you're willing to clean up the 
 * prototypes
 */
#define FAR /*far*/

#ifdef __STDC__
#ifndef PROTO_ARGS
#define PROTO_ARGS
#endif
#endif

#ifdef CONVEX
#define SEEK_SET        0
#define SEEK_CUR        1
#define SEEK_END        2
#endif

/* MS_DOS and VMS do not define TABLES_PATH, so OpenTableFile will default
   to finding the data files in the default directory */

/***********************************************************************
*
*  Global Include Files
*
***********************************************************************/

#include        <stdio.h>
#include        <string.h>
#include        <math.h>

#ifdef  UNIX
#include        <unistd.h>
#endif  /* UNIX */

#ifdef  MACINTOSH
#include        <stdlib.h>
#include        <console.h>
#endif  /* MACINTOSH */

#ifdef  MS_DOS
#include        <stdlib.h>
#ifdef MSC60
#include        <memory.h>
#else
#include        <alloc.h>
#include        <mem.h>
#endif  /* MSC60 */
#endif  /* MS_DOS */

/***********************************************************************
*
*  Global Definitions
*
***********************************************************************/

/* General Definitions */

#ifdef  MS_DOS
#define         FLOAT                   double
#else
#define         FLOAT                   float
#endif

#ifndef FALSE
#define         FALSE                   0
#endif
#ifndef TRUE
#define         TRUE                    1
#endif
#define         NULL_CHAR               '\0'

#define         MAX_U_32_NUM            0xFFFFFFFF
#define         PI                      3.14159265358979
#define         PI4                     PI/4
#define         PI64                    PI/64
#define         LN_TO_LOG10             0.2302585093

#define         VOL_REF_NUM             0
#define         MPEG_AUDIO_ID           1
#define         MAC_WINDOW_SIZE         24

#define         MONO                    1
#define         STEREO                  2
#define         BITS_IN_A_BYTE          8
#define         WORD                    16
#define         MAX_NAME_SIZE           81
#define         SBLIMIT                 32
#define         SSLIMIT                 18
#define         FFT_SIZE                1024
#define         HAN_SIZE                512
#define         SCALE_BLOCK             12
#define         SCALE_RANGE             64
#define         SCALE                   32768
#define         CRC16_POLYNOMIAL        0x8005

/* MPEG Header Definitions - Mode Values */

#define         MPG_MD_STEREO           0
#define         MPG_MD_JOINT_STEREO     1
#define         MPG_MD_DUAL_CHANNEL     2
#define         MPG_MD_MONO             3

/* Mode Extention */

#define         MPG_MD_LR_LR             0
#define         MPG_MD_LR_I              1
#define         MPG_MD_MS_LR             2
#define         MPG_MD_MS_I              3

/* AIFF Definitions */

/* 
 * Note:  The value of a multi-character constant
 *        is implementation-defined.
 */
#if !defined(MS_DOS) && !defined(AIX) && !defined(__hpux) && !defined(sgi) && !defined(linux)
#define         IFF_LONG
#define         IFF_ID_FORM             'FORM'
#define         IFF_ID_AIFF             'AIFF'
#define         IFF_ID_COMM             'COMM'
#define         IFF_ID_SSND             'SSND'
#define         IFF_ID_MPEG             'MPEG'
#else
#define         IFF_ID_FORM             "FORM"
#define         IFF_ID_AIFF             "AIFF"
#define         IFF_ID_COMM             "COMM"
#define         IFF_ID_SSND             "SSND"
#define         IFF_ID_MPEG             "MPEG"
#endif

/* "bit_stream.h" Definitions */

#define         MINIMUM         4    /* Minimum size of the buffer in bytes */
#define         MAX_LENGTH      32   /* Maximum length of word written or
                                        read from bit stream */
#define         READ_MODE       0
#define         WRITE_MODE      1
#define         ALIGNING        8
#define         BINARY          0
#define         ASCII           1
#define         BS_FORMAT       ASCII /* BINARY or ASCII = 2x bytes */
#define         BUFFER_SIZE     4096

#define         MIN(A, B)       ((A) < (B) ? (A) : (B))
#define         MAX(A, B)       ((A) > (B) ? (A) : (B))

/***********************************************************************
*
*  Global Type Definitions
*
***********************************************************************/

/* Structure for Reading Layer II Allocation Tables from File */

typedef struct {
    unsigned int    steps;
    unsigned int    bits;
    unsigned int    group;
    unsigned int    quant;
} sb_alloc, *alloc_ptr;

typedef sb_alloc        al_table[SBLIMIT][16];

/* Header Information Structure */

typedef struct {
    int version;
    int lay;
    int error_protection;
    int bitrate_index;
    int sampling_frequency;
    int padding;
    int extension;
    int mode;
    int mode_ext;
    int copyright;
    int original;
    int emphasis;
} layer, *the_layer;

/* Parent Structure Interpreting some Frame Parameters in Header */

typedef struct {
    layer       *header;        /* raw header information */
    int         actual_mode;    /* when writing IS, may forget if 0 chs */
    al_table    *alloc;         /* bit allocation table read in */
    int         tab_num;        /* number of table as loaded */
    int         stereo;         /* 1 for mono, 2 for stereo */
    int         jsbound;        /* first band of joint stereo coding */
    int         sblimit;        /* total number of sub bands */
} frame_params;

/* Double and SANE Floating Point Type Definitions */

typedef struct  IEEE_DBL_struct {
    unsigned long   hi;
    unsigned long   lo;
} IEEE_DBL;

typedef struct  SANE_EXT_struct {
    unsigned long   l1;
    unsigned long   l2;
    unsigned short  s1;
} SANE_EXT;

/* AIFF Type Definitions */

typedef char    ID[4];

typedef struct  ChunkHeader_struct {
    ID      ckID;
    long    ckSize;
} ChunkHeader;

typedef struct  Chunk_struct {
    ID      ckID;
    long    ckSize;
    ID      formType;
} Chunk;

typedef struct  CommonChunk_struct {
    ID              ckID;
    long            ckSize;
    short           numChannels;
    unsigned long   numSampleFrames;
    short           sampleSize;
    char            sampleRate[10];
} CommonChunk;

typedef struct  SoundDataChunk_struct {
    ID              ckID;
    long            ckSize;
    unsigned long   offset;
    unsigned long   blockSize;
} SoundDataChunk;

typedef struct  blockAlign_struct {
    unsigned long   offset;
    unsigned long   blockSize;
} blockAlign;

typedef struct  IFF_AIFF_struct {
    short           numChannels;
    unsigned long   numSampleFrames;
    short           sampleSize;
    double          sampleRate;
    unsigned long   sampleType;
    blockAlign      blkAlgn;
} IFF_AIFF;

/* "bit_stream.h" Type Definitions */

typedef struct  bit_stream_struc {
    FILE        *pt;            /* pointer to bit stream device */
    unsigned char *buf;         /* bit stream buffer */
    int         buf_size;       /* size of buffer (in number of bytes) */
    long        totbit;         /* bit counter of bit stream */
    int         buf_byte_idx;   /* pointer to top byte in buffer */
    int         buf_bit_idx;    /* pointer to top bit of top byte in buffer */
    int         mode;           /* bit stream open in read or write mode */
    int         eob;            /* end of buffer index */
    int         eobs;           /* end of bit stream flag */
    char        format;
    
    /* format of file in rd mode (BINARY/ASCII) */
} Bit_stream_struc;

/* Layer III side information. */

typedef struct {
	unsigned main_data_begin;
	unsigned private_bits;
	struct {
	    unsigned scfsi[4];
		struct gr_info_s {
			unsigned part2_3_length;
			unsigned big_values;
			unsigned global_gain;
			unsigned scalefac_compress;
			unsigned window_switching_flag;
			unsigned block_type;
			unsigned mixed_block_flag;
			unsigned table_select[3];
			unsigned subblock_gain[3];
			unsigned region0_count;
			unsigned region1_count;
			unsigned preflag;
			unsigned scalefac_scale;
			unsigned count1table_select;
			} gr[2];
		} ch[2];
	} III_side_info_t;

/* Layer III scale factors. */

typedef struct {
	int l[23];            /* [cb] */
	int s[3][13];         /* [window][cb] */
	} III_scalefac_t[2];  /* [ch] */

/***********************************************************************
*
*  Global Variable External Declarations
*
***********************************************************************/

extern char     *mode_names[4];
extern char     *layer_names[3];
extern double   s_freq[4];
extern int      bitrate[3][15];
extern double FAR multiple[64];

/***********************************************************************
*
*  Global Function Prototype Declarations
*
***********************************************************************/

/* The following functions are in the file "common.c" */

#ifdef  PROTO_ARGS
extern FILE           *OpenTableFile(char*);
extern int            read_bit_alloc(int, al_table*);
extern int            pick_table(frame_params*);
extern int            js_bound(int, int);
extern void           hdr_to_frps(frame_params*);
extern void           WriteHdr(frame_params*, FILE*);
extern void           WriteBitAlloc(unsigned int[2][SBLIMIT], frame_params*,
                        FILE*);
extern void           WriteScale(unsigned int[2][SBLIMIT],
                        unsigned int[2][SBLIMIT], unsigned int[2][3][SBLIMIT],
                        frame_params*, FILE*);
extern void           WriteSamples(int, unsigned int FAR [SBLIMIT],
                        unsigned int[SBLIMIT], frame_params*, FILE*);
extern int            NumericQ(char*);
extern int            BitrateIndex(int, int);
extern int            SmpFrqIndex(long);
extern int            memcheck(char*, int, int);
extern void           FAR *mem_alloc(unsigned long, char*);
extern void           mem_free(void**);
extern void           double_to_extended(double*, char[10]);
extern void           extended_to_double(char[10], double*);
extern int            aiff_read_headers(FILE*, IFF_AIFF*);
extern int            aiff_seek_to_sound_data(FILE*);
extern int            aiff_write_headers(FILE*, IFF_AIFF*);
extern int            refill_buffer(Bit_stream_struc*);
extern void           empty_buffer(Bit_stream_struc*, int);
extern void           open_bit_stream_w(Bit_stream_struc*, char*, int);
extern void           open_bit_stream_r(Bit_stream_struc*, char*, int);
extern void           close_bit_stream_r(Bit_stream_struc*);
extern void           close_bit_stream_w(Bit_stream_struc*);
extern void           alloc_buffer(Bit_stream_struc*, int);
extern void           desalloc_buffer(Bit_stream_struc*);
extern void           back_track_buffer(Bit_stream_struc*, int);
extern unsigned int   get1bit(Bit_stream_struc*);
extern void           put1bit(Bit_stream_struc*, int);
extern unsigned long  look_ahead(Bit_stream_struc*, int);
extern unsigned long  getbits(Bit_stream_struc*, int);
extern void           putbits(Bit_stream_struc*, unsigned int, int);
extern void           byte_ali_putbits(Bit_stream_struc*, unsigned int, int);
extern unsigned long  byte_ali_getbits(Bit_stream_struc*, int);
extern unsigned long  sstell(Bit_stream_struc*);
extern int            end_bs(Bit_stream_struc*);
extern int            seek_sync(Bit_stream_struc*, long, int);
extern void           I_CRC_calc(frame_params*, unsigned int[2][SBLIMIT],
                        unsigned int*);
extern void           II_CRC_calc(frame_params*, unsigned int[2][SBLIMIT],
                        unsigned int[2][SBLIMIT], unsigned int*);
extern void           update_CRC(unsigned int, unsigned int, unsigned int*);
extern void           read_absthr(FLOAT*, int);
extern unsigned int   hget1bit(); /* MI */
extern unsigned long  hgetbits(int);
extern unsigned long  hsstell();
extern void           hputbuf(unsigned int,int);



#ifdef  MACINTOSH
extern void           set_mac_file_attr(char[MAX_NAME_SIZE], short, OsType,
                        OsType);
#endif
#ifdef MS_DOS
extern char           *new_ext(char *filename, char *extname); 
#endif

#else
extern FILE           *OpenTableFile();
extern int            read_bit_alloc();
extern int            pick_table();
extern int            js_bound();
extern void           hdr_to_frps();
extern void           WriteHdr();
extern void           WriteBitAlloc();
extern void           WriteScale();
extern void           WriteSamples();
extern int            NumericQ();
extern int            BitrateIndex();
extern int            SmpFrqIndex();
extern int            memcheck();
extern void           FAR *mem_alloc();
extern void           mem_free();
extern void           double_to_extended();
extern void           extended_to_double();
extern int            aiff_read_headers();
extern int            aiff_seek_to_sound_data();
extern int            aiff_write_headers();
extern int            refill_buffer();
extern void           empty_buffer();
extern void           open_bit_stream_w();
extern void           open_bit_stream_r();
extern void           close_bit_stream_r();
extern void           close_bit_stream_w();
extern void           alloc_buffer();
extern void           desalloc_buffer();
extern void           back_track_buffer();
extern unsigned int   get1bit();
extern void           put1bit();
extern unsigned long  look_ahead();
extern unsigned long  getbits();
extern void           putbits();
extern void           byte_ali_putbits();
extern unsigned long  byte_ali_getbits();
extern unsigned long  sstell();
extern int            end_bs();
extern int            seek_sync();
extern void           I_CRC_calc();
extern void           II_CRC_calc();
extern void           update_CRC();
extern void           read_absthr();

extern unsigned int   hget1bit();
extern unsigned long  hgetbits();
extern unsigned long  hsstell();
extern void           hputbuf();

#ifdef MS_DOS
extern char           *new_ext(); 
#endif
#endif
