/*****************************************************************
|
|      Xaudio General Definitions
|
|      (c) 1996-1998 MpegTV, LLC
|      Author: Gilles Boccon-Gibod (gilles@mpegtv.com)
|
 ****************************************************************/

#ifndef __XAUDIO_H__
#define __XAUDIO_H__

/*----------------------------------------------------------------------
|       types
+---------------------------------------------------------------------*/
typedef void (*XA_ProgressNotificationFunction)(void *client,
                                                int source,
                                                int code,
                                                long value,
                                                const char *message);

typedef void (*XA_DebugNotificationFunction)(void *client,
                                             int source, 
                                             int level,
                                             const char *message, 
                                             const char *reason);

typedef void (*XA_ErrorNotificationFunction)(void *client,
                                             int source,
                                             int code,
                                             const char *message,
                                             const char *reason);

typedef struct {
    void                           *client;
    XA_ProgressNotificationFunction notify_progress;
    XA_DebugNotificationFunction    notify_debug;
    XA_ErrorNotificationFunction    notify_error;
} XA_NotificationClient;

#define XA_NOTIFY_PROGRESS(_client, _source, _code, _value, _message)    \
if ((_client) && (_client)->notify_progress)                             \
    (*(_client)->notify_progress)((_client)->client,                     \
     _source, _code, _value, _message)

#define XA_NOTIFY_DEBUG(_client, _source, _level, _message, _reason)     \
if ((_client) && (_client)->notify_debug)                                \
    (*(_client)->notify_debug)((_client)->client,                        \
     _source, _level, _message, _reason)

#define XA_NOTIFY_ERROR(_client, _source, _code, _message, _reason)      \
if ((_client) && (_client)->notify_error)                                \
    (*(_client)->notify_error)((_client)->client,                        \
     _source, _code, _message, _reason)

/*----------------------------------------------------------------------
|       macros
+---------------------------------------------------------------------*/
#if defined(WIN32) || defined(_WIN32)
#define XA_EXPORT __stdcall
#define XA_IMPORT __stdcall
#else
#define XA_EXPORT
#define XA_IMPORT
#endif

/*----------------------------------------------------------------------
|       apis
+---------------------------------------------------------------------*/
#define XA_API_ID_SYNC  1
#define XA_API_ID_ASYNC 2 

/*----------------------------------------------------------------------
|       error codes
+---------------------------------------------------------------------*/
#define XA_SUCCESS                           ( 0)
#define XA_FAILURE                           (-1)

/* general error codes */
#define XA_ERROR_BASE_GENERAL              (-100)
#define XA_ERROR_OUT_OF_MEMORY             (XA_ERROR_BASE_GENERAL - 0)
#define XA_ERROR_INVALID_PARAMETERS        (XA_ERROR_BASE_GENERAL - 1)
#define XA_ERROR_INTERNAL                  (XA_ERROR_BASE_GENERAL - 2)
#define XA_ERROR_TIMEOUT                   (XA_ERROR_BASE_GENERAL - 3)
#define XA_ERROR_VERSION_EXPIRED           (XA_ERROR_BASE_GENERAL - 4)

/* network error codes */
#define XA_ERROR_BASE_NETWORK              (-200)
#define XA_ERROR_CONNECT_TIMEOUT           (XA_ERROR_BASE_NETWORK -  0)
#define XA_ERROR_CONNECT_FAILED            (XA_ERROR_BASE_NETWORK -  1)
#define XA_ERROR_CONNECTION_REFUSED        (XA_ERROR_BASE_NETWORK -  2)
#define XA_ERROR_ACCEPT_FAILED             (XA_ERROR_BASE_NETWORK -  3)
#define XA_ERROR_LISTEN_FAILED             (XA_ERROR_BASE_NETWORK -  4)
#define XA_ERROR_SOCKET_FAILED             (XA_ERROR_BASE_NETWORK -  5)
#define XA_ERROR_SOCKET_CLOSED             (XA_ERROR_BASE_NETWORK -  6)
#define XA_ERROR_BIND_FAILED               (XA_ERROR_BASE_NETWORK -  7)
#define XA_ERROR_HOST_UNKNOWN              (XA_ERROR_BASE_NETWORK -  8)
#define XA_ERROR_HTTP_INVALID_REPLY        (XA_ERROR_BASE_NETWORK -  9)
#define XA_ERROR_HTTP_ERROR_REPLY          (XA_ERROR_BASE_NETWORK - 10)
#define XA_ERROR_HTTP_FAILURE              (XA_ERROR_BASE_NETWORK - 11)
#define XA_ERROR_FTP_INVALID_REPLY         (XA_ERROR_BASE_NETWORK - 12)
#define XA_ERROR_FTP_ERROR_REPLY           (XA_ERROR_BASE_NETWORK - 13)
#define XA_ERROR_FTP_FAILURE               (XA_ERROR_BASE_NETWORK - 14)

/* control error codes */
#define XA_ERROR_BASE_CONTROL              (-300)
#define XA_ERROR_PIPE_FAILED               (XA_ERROR_BASE_CONTROL - 0)
#define XA_ERROR_FORK_FAILED               (XA_ERROR_BASE_CONTROL - 1)
#define XA_ERROR_SELECT_FAILED             (XA_ERROR_BASE_CONTROL - 2)
#define XA_ERROR_PIPE_CLOSED               (XA_ERROR_BASE_CONTROL - 3)
#define XA_ERROR_PIPE_READ_FAILED          (XA_ERROR_BASE_CONTROL - 4)
#define XA_ERROR_PIPE_WRITE_FAILED         (XA_ERROR_BASE_CONTROL - 5)
#define XA_ERROR_INVALID_MESSAGE           (XA_ERROR_BASE_CONTROL - 6)
#define XA_ERROR_CIRQ_FULL                 (XA_ERROR_BASE_CONTROL - 7)
#define XA_ERROR_POST_FAILED               (XA_ERROR_BASE_CONTROL - 8)

/* url error codes */
#define XA_ERROR_BASE_URL                  (-400)
#define XA_ERROR_URL_UNSUPPORTED_SCHEME    (XA_ERROR_BASE_URL - 0)
#define XA_ERROR_URL_INVALID_SYNTAX        (XA_ERROR_BASE_URL - 1)

/* i/o error codes */
#define XA_ERROR_BASE_IO                   (-500)
#define XA_ERROR_OPEN_FAILED               (XA_ERROR_BASE_IO -  0)
#define XA_ERROR_CLOSE_FAILED              (XA_ERROR_BASE_IO -  1)
#define XA_ERROR_READ_FAILED               (XA_ERROR_BASE_IO -  2)
#define XA_ERROR_WRITE_FAILED              (XA_ERROR_BASE_IO -  3)
#define XA_ERROR_PERMISSION_DENIED         (XA_ERROR_BASE_IO -  4)
#define XA_ERROR_NO_DEVICE                 (XA_ERROR_BASE_IO -  5)
#define XA_ERROR_IOCTL_FAILED              (XA_ERROR_BASE_IO -  6)
#define XA_ERROR_MODULE_NOT_FOUND          (XA_ERROR_BASE_IO -  7)
#define XA_ERROR_UNSUPPORTED_INPUT         (XA_ERROR_BASE_IO -  8)
#define XA_ERROR_UNSUPPORTED_OUTPUT        (XA_ERROR_BASE_IO -  9)
#define XA_ERROR_UNSUPPORTED_FORMAT        (XA_ERROR_BASE_IO - 10)
#define XA_ERROR_DEVICE_BUSY               (XA_ERROR_BASE_IO - 11)
#define XA_ERROR_NO_SUCH_DEVICE            (XA_ERROR_BASE_IO - 12)
#define XA_ERROR_NO_SUCH_FILE              (XA_ERROR_BASE_IO - 13)
#define XA_ERROR_INPUT_EOF                 (XA_ERROR_BASE_IO - 14)

/* bitstream error codes */
#define XA_ERROR_BASE_BITSTREAM            (-600)
#define XA_ERROR_INVALID_FRAME             (XA_ERROR_BASE_BITSTREAM - 0)

/* dynamic linking error codes */
#define XA_ERROR_BASE_DYNLINK              (-700)
#define XA_ERROR_DLL_NOT_FOUND             (XA_ERROR_BASE_DYNLINK - 0)
#define XA_ERROR_SYMBOL_NOT_FOUND          (XA_ERROR_BASE_DYNLINK - 1)

/* environment variables error codes */
#define XA_ERROR_BASE_ENVIRONMENT          (-800)
#define XA_ERROR_NO_SUCH_ENVIRONMENT       (XA_ERROR_BASE_ENVIRONMENT - 0)
#define XA_ERROR_ENVIRONMENT_TYPE_MISMATCH (XA_ERROR_BASE_ENVIRONMENT - 1)

/*----------------------------------------------------------------------
|       error strings
+---------------------------------------------------------------------*/
#ifdef __cplusplus
extern "C" {
#endif

const char * XA_EXPORT xaudio_error_string(int code);
unsigned int XA_EXPORT xaudio_get_api_version(unsigned int api_id);

#ifdef __cplusplus
}
#endif

#endif /* __XAUDIO_H__ */
