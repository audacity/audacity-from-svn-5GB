/* switches.h is copied from uswitches.h
 * makefile will copy changes back to switches.h but you
 * lose changes made to uswitches.h */

/* Copyright 1989 Carnegie Mellon University */

#define MACINTOSH

#define XLISP_SOUND

/* this will enable code to read midi files, etc. */
#define CMTSTUFF

/* NYQUIST tells some CMT code that we're really in
 * XLISP and NYQUIST
 */
#define NYQUIST

#define BUFFERED_SYNCHRONOUS_INPUT

/*------------------------------------------*/

#define MAX_CHANNELS 16
#define SPACE_FOR_PLAY 10000
#define SWITCHES

#include "swlogic.h"

#include "Types.h"

