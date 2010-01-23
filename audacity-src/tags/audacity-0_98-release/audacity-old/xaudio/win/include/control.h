/*****************************************************************
|
|      XAudio Player. ASYNC API
|
|
|      (c) 1996-1998 MpegTV, LLC
|      Author: Gilles Boccon-Gibod (gilles@mpegtv.com)
|
 ****************************************************************/

#ifndef __CONTROL_H__
#define __CONTROL_H__

/*----------------------------------------------------------------------
|       includes
+---------------------------------------------------------------------*/
#include "xaudio.h"
#include "decoder.h"

/*----------------------------------------------------------------------
|       dthread messages
+---------------------------------------------------------------------*/
typedef enum {
    XA_MSG_UNKNOWN,

    /* player -> dthread */
    XA_MSG_COMMAND_EXIT,
    XA_MSG_COMMAND_PING,
    XA_MSG_COMMAND_PLAY,
    XA_MSG_COMMAND_PAUSE,
    XA_MSG_COMMAND_STOP,
    XA_MSG_COMMAND_SEEK,
    XA_MSG_COMMAND_INPUT_OPEN,
    XA_MSG_COMMAND_INPUT_CLOSE,
    XA_MSG_COMMAND_INPUT_SEND_MESSAGE,
    XA_MSG_COMMAND_INPUT_ADD_FILTER,
    XA_MSG_COMMAND_INPUT_REMOVE_FILTER,
    XA_MSG_COMMAND_INPUT_FILTERS_LIST,
    XA_MSG_COMMAND_INPUT_MODULE_REGISTER,
    XA_MSG_COMMAND_INPUT_MODULE_QUERY,
    XA_MSG_COMMAND_INPUT_MODULES_LIST,
    XA_MSG_COMMAND_OUTPUT_OPEN,
    XA_MSG_COMMAND_OUTPUT_CLOSE,
    XA_MSG_COMMAND_OUTPUT_SEND_MESSAGE,
    XA_MSG_COMMAND_OUTPUT_MUTE,
    XA_MSG_COMMAND_OUTPUT_UNMUTE,
    XA_MSG_COMMAND_OUTPUT_RESET,
    XA_MSG_COMMAND_OUTPUT_DRAIN,
    XA_MSG_COMMAND_OUTPUT_ADD_FILTER,
    XA_MSG_COMMAND_OUTPUT_REMOVE_FILTER,
    XA_MSG_COMMAND_OUTPUT_FILTERS_LIST,
    XA_MSG_COMMAND_OUTPUT_MODULE_REGISTER,
    XA_MSG_COMMAND_OUTPUT_MODULE_QUERY,
    XA_MSG_COMMAND_OUTPUT_MODULES_LIST,
    XA_MSG_SET_PLAYER_MODE,
    XA_MSG_GET_PLAYER_MODE,
    XA_MSG_SET_PLAYER_ENVIRONMENT_INTEGER,
    XA_MSG_GET_PLAYER_ENVIRONMENT_INTEGER,
    XA_MSG_SET_PLAYER_ENVIRONMENT_STRING,
    XA_MSG_GET_PLAYER_ENVIRONMENT_STRING,
    XA_MSG_UNSET_PLAYER_ENVIRONMENT,
    XA_MSG_SET_INPUT_NAME,
    XA_MSG_GET_INPUT_NAME,
    XA_MSG_SET_INPUT_MODULE,
    XA_MSG_GET_INPUT_MODULE,
    XA_MSG_SET_INPUT_POSITION_RANGE,
    XA_MSG_GET_INPUT_POSITION_RANGE,
    XA_MSG_SET_INPUT_TIMECODE_GRANULARITY,
    XA_MSG_GET_INPUT_TIMECODE_GRANULARITY,
    XA_MSG_SET_OUTPUT_NAME,
    XA_MSG_GET_OUTPUT_NAME,
    XA_MSG_SET_OUTPUT_MODULE,
    XA_MSG_GET_OUTPUT_MODULE,
    XA_MSG_SET_OUTPUT_VOLUME,
    XA_MSG_GET_OUTPUT_VOLUME,
    XA_MSG_SET_OUTPUT_CHANNELS,
    XA_MSG_GET_OUTPUT_CHANNELS,
    XA_MSG_SET_OUTPUT_PORTS,
    XA_MSG_GET_OUTPUT_PORTS,
    XA_MSG_SET_OUTPUT_FEEDBACK_RATE,
    XA_MSG_GET_OUTPUT_FEEDBACK_RATE,
    XA_MSG_SET_CODEC_QUALITY,
    XA_MSG_GET_CODEC_QUALITY,
    XA_MSG_SET_CODEC_EQUALIZER,
    XA_MSG_GET_CODEC_EQUALIZER,
    XA_MSG_SET_NOTIFICATION_MASK,
    XA_MSG_GET_NOTIFICATION_MASK,
    XA_MSG_SET_DEBUG_LEVEL,
    XA_MSG_GET_DEBUG_LEVEL,

    /* dthread -> player */
    XA_MSG_NOTIFY_READY,
    XA_MSG_NOTIFY_ACK,
    XA_MSG_NOTIFY_NACK,
    XA_MSG_NOTIFY_PONG,
    XA_MSG_NOTIFY_EXITED,
    XA_MSG_NOTIFY_PLAYER_STATE,
    XA_MSG_NOTIFY_PLAYER_MODE,
    XA_MSG_NOTIFY_PLAYER_ENVIRONMENT_INTEGER,
    XA_MSG_NOTIFY_PLAYER_ENVIRONMENT_STRING,
    XA_MSG_NOTIFY_INPUT_STATE,
    XA_MSG_NOTIFY_INPUT_NAME,
    XA_MSG_NOTIFY_INPUT_CAPS,
    XA_MSG_NOTIFY_INPUT_POSITION,
    XA_MSG_NOTIFY_INPUT_POSITION_RANGE,
    XA_MSG_NOTIFY_INPUT_TIMECODE,
    XA_MSG_NOTIFY_INPUT_TIMECODE_GRANULARITY,
    XA_MSG_NOTIFY_INPUT_DURATION,
    XA_MSG_NOTIFY_INPUT_STREAM_INFO,
    XA_MSG_NOTIFY_INPUT_MODULE,
    XA_MSG_NOTIFY_INPUT_MODULE_INFO,
    XA_MSG_NOTIFY_INPUT_DEVICE_INFO,
    XA_MSG_NOTIFY_INPUT_FILTER_INFO,
    XA_MSG_NOTIFY_OUTPUT_STATE,
    XA_MSG_NOTIFY_OUTPUT_NAME,
    XA_MSG_NOTIFY_OUTPUT_CAPS,
    XA_MSG_NOTIFY_OUTPUT_VOLUME,
    XA_MSG_NOTIFY_OUTPUT_BALANCE,
    XA_MSG_NOTIFY_OUTPUT_PCM_LEVEL,
    XA_MSG_NOTIFY_OUTPUT_MASTER_LEVEL,
    XA_MSG_NOTIFY_OUTPUT_CHANNELS,
    XA_MSG_NOTIFY_OUTPUT_PORTS,
    XA_MSG_NOTIFY_OUTPUT_FEEDBACK_RATE,
    XA_MSG_NOTIFY_OUTPUT_MODULE,
    XA_MSG_NOTIFY_OUTPUT_MODULE_INFO,
    XA_MSG_NOTIFY_OUTPUT_DEVICE_INFO,
    XA_MSG_NOTIFY_OUTPUT_FILTER_INFO,
    XA_MSG_NOTIFY_CODEC_QUALITY,
    XA_MSG_NOTIFY_CODEC_EQUALIZER,
    XA_MSG_NOTIFY_NOTIFICATION_MASK,
    XA_MSG_NOTIFY_DEBUG_LEVEL,
    XA_MSG_NOTIFY_PROGRESS,
    XA_MSG_NOTIFY_DEBUG,
    XA_MSG_NOTIFY_ERROR,

    /* dthread -> timesync */
    XA_MSG_COMMAND_QUEUE_FEEDBACK_EVENT,
    XA_MSG_COMMAND_QUEUE_TAG_EVENT,
    XA_MSG_COMMAND_FEEDBACK_PAUSE,
    XA_MSG_COMMAND_FEEDBACK_RESTART,
    XA_MSG_COMMAND_FEEDBACK_FLUSH,

    /* timesync -> player */
    XA_MSG_NOTIFY_FEEDBACK_EVENT,
    XA_MSG_NOTIFY_TAG_EVENT,

    /* sentinel */
    XA_MSG_LAST
} XA_MessageCode;

typedef enum { 
    XA_PLAYER_STATE_STOPPED, 
    XA_PLAYER_STATE_PLAYING,
    XA_PLAYER_STATE_PAUSED,
    XA_PLAYER_STATE_EOF
} XA_PlayerState;

typedef enum {
    XA_INPUT_STATE_OPEN,
    XA_INPUT_STATE_CLOSED
} XA_InputState;

typedef enum {
    XA_OUTPUT_STATE_OPEN,
    XA_OUTPUT_STATE_CLOSED
} XA_OutputState;

typedef enum { 
    XA_OUTPUT_CHANNELS_STEREO, 
    XA_OUTPUT_CHANNELS_MONO_LEFT,         
    XA_OUTPUT_CHANNELS_MONO_RIGHT,
    XA_OUTPUT_CHANNELS_MONO_MIX 
} XA_OutputChannels;

typedef struct {
    const char *name;
    union {
        long        integer;
        const char *string;
    } value;
} XA_EnvironmentInfo;

typedef struct {
    unsigned char h;
    unsigned char m;
    unsigned char s;
    unsigned char f;
} XA_TimecodeInfo;

typedef struct {
    unsigned char command;
    short         code;
} XA_NackInfo;
        
typedef struct {
    unsigned char master_level;
    unsigned char pcm_level;
    unsigned char balance;
} XA_VolumeInfo;

typedef struct {
    unsigned long offset;
    unsigned long range;
} XA_PositionInfo;

typedef struct {
    unsigned char id;
    unsigned char nb_devices;
    const char   *name;
    const char   *description;
} XA_ModuleInfo;

typedef struct {
    short          id;
    const char    *name;
} XA_FilterInfo;

typedef struct {
    unsigned char module_id;
    unsigned char index;
    unsigned char flags;
    const char   *name;
    const char   *description;
} XA_DeviceInfo;

typedef struct {
    unsigned char  level;
    unsigned char  layer;
    unsigned short bitrate;
    unsigned short frequency;
    unsigned char  mode;
} XA_StreamInfo;

typedef struct {
    unsigned short type;
    unsigned short size;
    void          *data;
} XA_ModuleMessage;

typedef struct {
    XA_AbsoluteTime when;
    unsigned long tag;
} XA_TagEvent;

typedef struct {
    XA_AbsoluteTime      when;
    unsigned short       channels;
    unsigned short       nb_samples;
    const unsigned char *samples;
} XA_FeedbackEvent;

typedef struct {
    unsigned char  source;
    unsigned char  code;
    short          value;
    const char    *message;
} XA_ProgressInfo;

typedef struct {
    unsigned char source;
    unsigned char level;
    const char   *message;
} XA_DebugInfo;

typedef struct {
    unsigned char source;
    short         code;
    const char   *message;
} XA_ErrorInfo;

typedef struct {
    XA_MessageCode code;
    union {
        char              *buffer;
        char              *name;
        char              *string;
        short              module_id;
        unsigned char      state;
        unsigned long      mode;
        unsigned char      channels;
        unsigned char      quality;
        unsigned long      duration;
        unsigned long      range;
        unsigned long      granularity;
        unsigned long      caps;
        unsigned char      ports;
        unsigned char      ack;
        unsigned long      tag;
        unsigned char      debug_level;
        unsigned long      notification_mask;
        unsigned char      rate;
        XA_NackInfo        nack;
        XA_VolumeInfo      volume;
        XA_PositionInfo    position;
        XA_EqualizerInfo  *equalizer;
        XA_ModuleInfo      module_info;
        XA_FilterInfo      filter_info;
        XA_DeviceInfo      device_info;
        XA_StreamInfo      stream_info;
        XA_EnvironmentInfo environment_info;
        XA_TimecodeInfo    timecode;
        XA_ModuleMessage   module_message;
        XA_TagEvent        tag_event;
        XA_FeedbackEvent   feedback_event;
        XA_ProgressInfo    progress;
        XA_DebugInfo       debug;
        XA_ErrorInfo       error;
    } data;
} XA_Message;

typedef void (*XA_ControlProcedure)(void *control, void *args);
typedef void (*XA_EventForwarderProcedure)(void *control, void *args);

/*----------------------------------------------------------------------
|       constants
+---------------------------------------------------------------------*/
#define XA_TIMEOUT_INFINITE                -1

#define XA_CONTROL_PRIORITY_LOWEST          0
#define XA_CONTROL_PRIORITY_LOW             1
#define XA_CONTROL_PRIORITY_NORMAL          2
#define XA_CONTROL_PRIORITY_HIGH            3
#define XA_CONTROL_PRIORITY_HIGHEST         4

/*----------------------------------------------------------------------
|       prototypes
+---------------------------------------------------------------------*/
#ifdef __cplusplus
extern "C" {
#endif

int  XA_EXPORT control_procedure_new(void **control, 
                                     void *control_driver_args,
                                     XA_ControlProcedure procedure,
                                     void *procedure_args,
                                     XA_EventForwarderProcedure forwarder,
                                     void *forwarder_args);
int  XA_EXPORT control_procedure_delete(void *control);
int  XA_EXPORT control_procedure_set_priority(void *control, int priority);
int  XA_EXPORT control_procedure_get_priority(void *control);
int  XA_EXPORT control_message_to_bytes(const XA_Message *message, 
                                        unsigned char *buffer,
                                        int max_size);
int  XA_EXPORT control_message_from_bytes(XA_Message *message, 
                                          const unsigned char *buffer);
int            control_message_send(void *control, int code, ...);
int            control_event_send(void *control, int code, ...);
int  XA_EXPORT control_message_send_N(void *control, int code);
int  XA_EXPORT control_message_send_P(void *control, int code, 
                                      const void *ptr1);
int  XA_EXPORT control_message_send_S(void *control, int code, 
                                      const char *str1);
int  XA_EXPORT control_message_send_SS(void *control, int code, 
                                       const char *str1, const char *str2);
int  XA_EXPORT control_message_send_SI(void *control, int code, 
                                       const char *str1, int int1);
int  XA_EXPORT control_message_send_I(void *control, int code, int int1);
int  XA_EXPORT control_message_send_II(void *control, int code,
                                       int int1, int int2);
int  XA_EXPORT control_message_send_III(void *control, int code, 
                                        int int1, int int2, int int3);
int  XA_EXPORT control_message_send_IIII(void *control, int code, 
                                         int int1, int int2, 
                                         int int3, int int4);
int  XA_EXPORT control_message_send_IPI(void *control, int code,
                                        int int1, const void *ptr1, int int2);
int  XA_EXPORT control_message_post_to_command_queue(void *control, 
                                                   const XA_Message *message);
int  XA_EXPORT control_message_post_to_event_queue(void *control, 
                                                   const XA_Message *message);
int  XA_EXPORT control_message_get(void *control, XA_Message *message);
int  XA_EXPORT control_message_wait(void *control, XA_Message *message,
                                    int timeout);
void XA_EXPORT control_message_print(const XA_Message *message);
void XA_EXPORT control_message_sprint(char *string, const XA_Message *message);

#ifdef __cplusplus
}
#endif

#endif /* __CONTROL_H__ */
