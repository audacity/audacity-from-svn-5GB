/******************************************************************
*               modified JCD 27 Apr-88 for AMIGA
*      cext.h -- extensions to c to make it more portable
* Copyright 1989 Carnegie Mellon University
*
*******************************************************************

cext must provide the following definitions:

true     -- a  constant
false     -- a boolean constant
private -- defined as static, used to declare local functions
public     -- defined as empty string, used to declare exported functions
boolean -- a new type
byte     -- unsigned 8-bit quantity
ushort     -- unsigned 16-bit quantity
ulong     -- unsigned 32-bit quantity
Pointer -- pointer to char, a generic pointer
abs()     -- absolute value of any type of number
max()     -- maximum of two numbers
min()     -- minimum of two numbers
round()	  -- round a double to long		     

NULL     -- pointer to nothing, a constant
EOS      -- end of string, a constant '\0'
MALLOC(x)     -- allocates x bytes
FREE(x)     -- frees something from MALLOC
AVAILMEM     -- tells how much memory is available.
                   (N.B.: no parens, no args.)
EXIT(n)  -- calls exit(n) after shutting down/deallocating resources

*****************************************************************************/

#ifndef CEXT_H
#ifndef SWITCHES
#include "switches.h"
#endif

#ifdef UNIX
#include <sys/types.h>
#ifndef _IBMR2
#ifndef sgi
#ifndef linux
typedef unsigned long ulong;
/* under Mach, ushort is defined in types.h, but ulong isn't (!) */
#ifndef __sparc__
#ifndef NeXT
/* typedef unsigned short ushort; */
#endif
#endif
#endif
#endif
#endif
#endif

/* There's a name conflict between true/false as an enum type in
 * Apple #includes:Types.h on the Mac, and true/false as #defined below
 */
#ifdef MACINTOSH
#include "Types.h"
#else
#define true 1
#define false 0
#endif
#define private static
#define public

#ifdef MACINTOSH
/* declare malloc: */
#include <stdlib.h>
#include <stdio.h>

typedef unsigned char byte;
typedef unsigned short ushort;
typedef unsigned long ulong;

/*FILE *fopen(char *filename, char *mode);
int fclose(FILE *file);*/

#else
#ifdef DOS
#include "stdlib.h"
#include "malloc.h"
typedef unsigned char byte;
typedef unsigned short ushort;
typedef unsigned long ulong;
#else
/* declare malloc for everyone else */
public void *malloc();
#endif
#endif

#ifdef UNIX
typedef unsigned char byte;
#ifdef ITC
/* Note: stdio will pull this in later if we don't do it now: */
#include "sys/types.h"
/* on RS6000, types.h defines ulong and ushort if _ALL_SOURCE is defined */
#ifndef _ALL_SOURCE
typedef unsigned long ulong;
#ifdef UNIX_ITC
typedef unsigned short ushort;
#endif
#endif
#endif
#endif

#ifdef AMIGA
typedef unsigned char byte;
typedef unsigned short ushort;
typedef unsigned long ulong;
#endif

typedef char *Pointer;

#ifdef UNIX_MACH
typedef int boolean;
#else
/* hopefully, unsigned short will save sign extension instructions */
typedef unsigned char boolean;
#endif

#ifndef abs
#define abs(a) (((a) > 0) ? (a) : -(a))
#endif
#ifndef max
#define max(a, b) (((a) > (b)) ? (a) : (b))
#endif
#ifndef min
#define min(a, b) (((a) < (b)) ? (a) : (b))
#endif

#define MAXULONG 0xffffffff

#ifndef NULL
#define NULL 0L
#endif

#ifndef EOS
#define EOS '\0'
#endif

#define SAFETYBUF    10    /* Safety buffer when allocating memory */
#define BIGGEST_BLOCK    32765    /* Should find a happy medium for this  */

#ifdef MACINTOSH /*DMH: gets AVAILMEM in record.c*/
#include "stddef.h"
#define MALLOC(x)       malloc((size_t)(x))  /*DMH: size_t is ulong, for MAC*/
#define FREE(x)    free((char *)(x))
#define AVAILMEM    MyMaxMem(NULL)/*???*/
#endif

#ifdef LATTICE322
#define MALLOC  malloc
#define FREE    free
#define AVAILMEM    MyMaxMem(NULL)

#else

#ifdef DOS /* was MICROSOFT */
#define MALLOC  malloc
#define FREE    free
#define AVAILMEM MyMaxMem(NULL)
#endif
#endif

#ifdef UNIX
#define MALLOC  malloc
#define FREE     free
#define AVAILMEM     10000000 /* since we have virtual memory, assume 10Mb */
#endif

#ifdef AMIGA
#define MALLOC  malloc
#define FREE    free
#define AVAILMEM     128000
#endif

public ulong MyMaxMem(ushort *);

#ifndef MEM
#include "mem.h"
#endif

#ifndef CLEANUP
#include "cleanup.h"
#endif

#ifdef CMTSTUFF
#define EXIT cmt_exit
public void EXIT(int);
/* don't allow anyone to call exit directly */
#define exit(n) PLEASE_CALL_EXIT_NOT_exit
#else
#define EXIT(n) exit(n)
#endif

#define _cext

#ifndef MALLOC
MALLOC is not defined!
#endif

#define round(x) ((long) ((x) + 0.5))

#define CEXT_H
#endif
