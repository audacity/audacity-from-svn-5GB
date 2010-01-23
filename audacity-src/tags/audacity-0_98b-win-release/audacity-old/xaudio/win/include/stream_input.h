/*****************************************************************
|
|      Network Library. Stream Input Module
|
|      (c) 1996-1998 MpegTV, LLC
|
 ****************************************************************/

#ifndef __STREAM_INPUT_H__
#define __STREAM_INPUT_H__

/*----------------------------------------------------------------------
|       includes
+---------------------------------------------------------------------*/
#include "decoder.h"


/*----------------------------------------------------------------------
|       constants
+---------------------------------------------------------------------*/
enum {
    XA_PROGRESS_CODE_STREAM_CONNECTING = 0x10,
    XA_PROGRESS_CODE_STREAM_CONNECTED,
    XA_PROGRESS_CODE_STREAM_SENDING_REQUEST,
    XA_PROGRESS_CODE_STREAM_BUFFER_UNDERFLOW,
    XA_PROGRESS_CODE_STREAM_BUFFER_OK,
    XA_PROGRESS_CODE_STREAM_BUFFER_FULLNESS,
    XA_PROGRESS_CODE_STREAM_REACHED_EOS
};

/*----------------------------------------------------------------------
|       prototypes
+---------------------------------------------------------------------*/
#ifdef __cplusplus
extern "C" {
#endif

int XA_EXPORT stream_input_module_register(XA_InputModule *module);

#ifdef __cplusplus
}
#endif
 
#endif /* __STREAM_INPUT_H__ */
