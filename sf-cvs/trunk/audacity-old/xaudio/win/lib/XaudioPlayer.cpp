/*****************************************************************
|
|      XAudio Player Wrapper Class    
|
|
|      (c) 1996-1998 MpegTV, LLC
|      Author: Gilles Boccon-Gibod (gilles@mpegtv.com)
|
 ****************************************************************/

/*----------------------------------------------------------------------
|       includes
+---------------------------------------------------------------------*/
#ifndef STRICT
#define STRICT
#endif

#include <tchar.h>
#include "XaudioPlayer.h"
#include "control_win32.h"

/*----------------------------------------------------------------------
|       XaudioMessageHandler
+---------------------------------------------------------------------*/
LRESULT CALLBACK
XaudioMessageHandler(HWND window, 
                     UINT win32_message, 
                     WPARAM win32_wparam, 
                     LPARAM win32_lparam)
{
    XA_Message xaudio_message;

    XaudioPlayer *player = 
        (XaudioPlayer *)GetWindowLong(window, GWL_USERDATA);
    if (player == NULL || win32_message < XA_MSG_BASE) {
        return DefWindowProc(window, win32_message, win32_wparam, win32_lparam);
    }

    if (control_win32_params_to_message(player->control,
                                        &xaudio_message,
                                        win32_message,
                                        win32_wparam,
                                        win32_lparam) == XA_SUCCESS) {
        player->HandleNotificationMessage(&xaudio_message);
        return 0;
    } else {
        return DefWindowProc(window, win32_message, win32_wparam, win32_lparam);
    }
}

/*----------------------------------------------------------------------
|       XaudioPlayer contructor
+---------------------------------------------------------------------*/
XaudioPlayer::XaudioPlayer(HINSTANCE instance)
{
    /* check the API version */
    unsigned int version;
    version = xaudio_get_api_version(XA_API_ID_ASYNC);
    if ((((version >> 16)&0xFF) != XA_REQUIRED_VERSION_MAJOR) ||
        (((version >>  8)&0xFF) != XA_REQUIRED_VERSION_MINOR)) {
        MessageBox(NULL, 
                   TEXT("Xaudio DLL API version mismatch"), 
                   TEXT("Xaudio Error"), 
                   MB_OK            | 
                   MB_ICONERROR     | 
                   MB_SETFOREGROUND |
                   MB_TOPMOST);
        return;
    }

    /* register a window class */
    LPCTSTR   class_name = TEXT("Xaudio Hidden Window Class");
    WNDCLASS               wclass;
    wclass.style         = 0;
    wclass.lpfnWndProc   = XaudioMessageHandler;
    wclass.cbClsExtra    = 0;
    wclass.cbWndExtra    = 0;
    wclass.hInstance     = instance;
    wclass.hIcon         = NULL;
    wclass.hCursor       = NULL;
    wclass.hbrBackground = NULL;
    wclass.lpszMenuName  = NULL;
    wclass.lpszClassName = class_name;

    /* register the class and ignore any error because we might */
    /* be registering the class more than once                  */
    RegisterClass(&wclass);

    /* create a hidden window to receive notification messages */
    m_HiddenWindow = CreateWindow(
        class_name,                 // pointer to registered class name 
        TEXT("Xaudio Hidden Window"),// pointer to window name 
        0,                          // window style 
        0,                          // horizontal position of window 
        0,                          // vertical position of window 
        0,                          // window width 
        0,                          // window height 
        NULL,                       // handle to parent or owner window 
        NULL,                       // handle to menu or child-window identifier 
        instance,                   // handle to application instance 
        NULL);

    if (m_HiddenWindow) {
        /* set the window's user data to this object */
        SetWindowLong(m_HiddenWindow, GWL_USERDATA, (LONG)this);
    }

    /* instanciate the XA_Player object */
    this->Run((void *)&m_HiddenWindow);
}

/*----------------------------------------------------------------------
|       XaudioPlayer destructor
+---------------------------------------------------------------------*/
XaudioPlayer::~XaudioPlayer()
{
    /* delete the hidden window */
    DestroyWindow(m_HiddenWindow);
}

/*----------------------------------------------------------------------
|       XaudioPlayer:: HandleNotificationMessage
+---------------------------------------------------------------------*/
void 
XaudioPlayer::HandleNotificationMessage(XA_Message *message)
{
    switch (message->code) {
      case XA_MSG_NOTIFY_READY:
        this->OnNotifyReady();
        break;

      case XA_MSG_NOTIFY_ACK:
        this->OnNotifyAck(message->data.ack);
        break;

      case XA_MSG_NOTIFY_NACK:
        this->OnNotifyNack(&message->data.nack);
        break;

      case XA_MSG_NOTIFY_PONG:
        this->OnNotifyPong(message->data.tag);
        break;

      case XA_MSG_NOTIFY_EXITED:
        this->OnNotifyExited();
        break;

      case XA_MSG_NOTIFY_PLAYER_STATE:
        this->OnNotifyPlayerState((XA_PlayerState)message->data.state);
        break;

      case XA_MSG_NOTIFY_PLAYER_MODE:
        this->OnNotifyPlayerMode(message->data.mode);
        break;

      case XA_MSG_NOTIFY_PLAYER_ENVIRONMENT_INTEGER:
        this->OnNotifyPlayerEnvironmentInteger(
            message->data.environment_info.name,
            message->data.environment_info.value.integer);
        break;

      case XA_MSG_NOTIFY_PLAYER_ENVIRONMENT_STRING:
        this->OnNotifyPlayerEnvironmentString(
            message->data.environment_info.name,
            message->data.environment_info.value.string);
        break;

      case XA_MSG_NOTIFY_INPUT_STATE:
        this->OnNotifyInputState((XA_InputState)message->data.state);
        break;

      case XA_MSG_NOTIFY_INPUT_NAME:
        this->OnNotifyInputName(message->data.name);
        break;

      case XA_MSG_NOTIFY_INPUT_CAPS:
        this->OnNotifyInputCaps(message->data.caps);
        break;

      case XA_MSG_NOTIFY_INPUT_POSITION:
        this->OnNotifyInputPosition(
            message->data.position.offset,
            message->data.position.range);
        break;

      case XA_MSG_NOTIFY_INPUT_POSITION_RANGE:
        this->OnNotifyInputPositionRange(message->data.range);
        break;

      case XA_MSG_NOTIFY_INPUT_TIMECODE:
        this->OnNotifyInputTimecode(&message->data.timecode);
        break;

      case XA_MSG_NOTIFY_INPUT_TIMECODE_GRANULARITY:
        this->OnNotifyInputTimecodeGranularity(message->data.granularity);
        break;

      case XA_MSG_NOTIFY_INPUT_DURATION:
        this->OnNotifyInputDuration(message->data.duration);
        break;

      case XA_MSG_NOTIFY_INPUT_STREAM_INFO:
        this->OnNotifyInputStreamInfo(&message->data.stream_info);
        break;

      case XA_MSG_NOTIFY_INPUT_MODULE:
        this->OnNotifyInputModule(message->data.module_id);
        break;

      case XA_MSG_NOTIFY_INPUT_MODULE_INFO:
        this->OnNotifyInputModuleInfo(&message->data.module_info);
        break;

      case XA_MSG_NOTIFY_INPUT_DEVICE_INFO:
        this->OnNotifyInputDeviceInfo(&message->data.device_info);
        break;

      case XA_MSG_NOTIFY_INPUT_FILTER_INFO:
        this->OnNotifyInputFilterInfo(&message->data.filter_info);
        break;

      case XA_MSG_NOTIFY_OUTPUT_STATE:
        this->OnNotifyOutputState((XA_OutputState)message->data.state);
        break;

      case XA_MSG_NOTIFY_OUTPUT_NAME:
        this->OnNotifyOutputName(message->data.name);
        break;

      case XA_MSG_NOTIFY_OUTPUT_CAPS:
        this->OnNotifyOutputCaps(message->data.caps);
        break;

      case XA_MSG_NOTIFY_OUTPUT_VOLUME:
        this->OnNotifyOutputVolume(&message->data.volume);
        break;

      case XA_MSG_NOTIFY_OUTPUT_BALANCE:
        this->OnNotifyOutputBalance(message->data.volume.balance);
        break;

      case XA_MSG_NOTIFY_OUTPUT_PCM_LEVEL:
        this->OnNotifyOutputPcmLevel(message->data.volume.pcm_level);
        break;

      case XA_MSG_NOTIFY_OUTPUT_MASTER_LEVEL:
        this->OnNotifyOutputMasterLevel(message->data.volume.master_level);
        break;

      case XA_MSG_NOTIFY_OUTPUT_CHANNELS:
        this->OnNotifyOutputChannels((XA_OutputChannels)message->data.channels);
        break;

      case XA_MSG_NOTIFY_OUTPUT_PORTS:
        this->OnNotifyOutputPorts(message->data.ports);
        break;

      case XA_MSG_NOTIFY_OUTPUT_MODULE:
        this->OnNotifyOutputModule(message->data.module_id);
        break;

      case XA_MSG_NOTIFY_OUTPUT_MODULE_INFO:
        this->OnNotifyOutputModuleInfo(&message->data.module_info);
        break;

      case XA_MSG_NOTIFY_OUTPUT_DEVICE_INFO:
        this->OnNotifyOutputDeviceInfo(&message->data.device_info);
        break;

      case XA_MSG_NOTIFY_OUTPUT_FILTER_INFO:
        this->OnNotifyOutputFilterInfo(&message->data.filter_info);
        break;

      case XA_MSG_NOTIFY_CODEC_QUALITY:
        this->OnNotifyCodecQuality(message->data.quality);
        break;

      case XA_MSG_NOTIFY_CODEC_EQUALIZER:
        this->OnNotifyCodecEqualizer(message->data.equalizer);
        break;

      case XA_MSG_NOTIFY_NOTIFICATION_MASK:
        this->OnNotifyNotificationMask(message->data.notification_mask);
        break;

      case XA_MSG_NOTIFY_DEBUG_LEVEL:
        this->OnNotifyDebugLevel(message->data.debug_level);
        break;

      case XA_MSG_NOTIFY_PROGRESS:
        this->OnNotifyProgress(&message->data.progress);
        break;

      case XA_MSG_NOTIFY_DEBUG:
        this->OnNotifyDebug(&message->data.debug);
        break;

      case XA_MSG_NOTIFY_ERROR:
        this->OnNotifyError(&message->data.error);
        break;
    }
}