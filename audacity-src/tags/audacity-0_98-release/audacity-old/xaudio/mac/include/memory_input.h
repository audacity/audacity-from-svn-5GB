/*****************************************************************
|
|      Memory Buffer Input Module
|
|      (c) 1998 MpegTV, LLC
|      Author: Gilles Boccon-Gibod (gilles@mpegtv.com)
|
 ****************************************************************/

#ifndef __MEMORY_INPUT_H__
#define __MEMORY_INPUT_H__

/*----------------------------------------------------------------------
|       includes
+---------------------------------------------------------------------*/
#include "decoder.h"

/*----------------------------------------------------------------------
|       prototypes
+---------------------------------------------------------------------*/
#ifdef __cplusplus
extern "C" {
#endif

int XA_EXPORT memory_input_feed(void *device, 
                                const unsigned char *data, 
                                unsigned int nb_bytes);
int XA_EXPORT memory_input_flush(void *device);
int XA_EXPORT memory_input_module_register(XA_InputModule *module);

#ifdef __cplusplus
}
#endif
 
#endif /* __MEMORY_INPUT_H__ */
