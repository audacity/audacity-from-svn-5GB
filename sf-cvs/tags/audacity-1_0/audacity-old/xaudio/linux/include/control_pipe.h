/*****************************************************************
|
|      XAudio Player GUI. Control Driver, fork() implementation
|
|      (c) 1997 Gilles Boccon-Gibod. bok@bok.net
|
 ****************************************************************/

#ifndef __CONTROL_PIPE_H__
#define __CONTROL_PIPE_H__

/*----------------------------------------------------------------------
|       includes
+---------------------------------------------------------------------*/
#include <unistd.h>

/*----------------------------------------------------------------------
|       types
+---------------------------------------------------------------------*/
struct XA_MessageQueue;

typedef struct {
    pid_t                   decoder_pid;
    pid_t                   forwarder_pid;
    int                     command_read_pipe;
    int                     command_write_pipe;
    int                     event_read_pipe;
    int                     event_write_pipe;
    struct XA_MessageQueue *command_queue;
    struct XA_MessageQueue *event_queue;
} XA_PipeControl;

#endif /* __CONTROL_PIPE_H__    */
