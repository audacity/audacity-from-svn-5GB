/*****************************************************************
|
|      XAudio Player Wrapper Class    
|
|      (c) 1996-1998 MpegTV, LLC
|      Author: Gilles Boccon-Gibod (gilles@mpegtv.com)
|
|
|      This class implements a thin C++ wrapper around
|      the Xaudio ASYNC API.
|      A typical player impementation would subclass this base 
|      class, and overload some of the stubbed notification 
|      funtions.
|      To send commands to the player, just use the functions
|      inherited from XA_Player.
|      To receive notification, overload the notification
|      functions in the subclass to implement your own 
|      notication handlers.
|
 ****************************************************************/

#ifndef __XAUDIO_PLAYER_H__
#define __XAUDIO_PLAYER_H__

/*----------------------------------------------------------------------
|       includes
+---------------------------------------------------------------------*/
#ifndef STRICT
#define STRICT
#endif

#include <windows.h>
#include "player.h"

/*----------------------------------------------------------------------
|       constants
+---------------------------------------------------------------------*/
#define XA_REQUIRED_VERSION_MAJOR 3
#define XA_REQUIRED_VERSION_MINOR 2

/*----------------------------------------------------------------------
|       XaudioPlayer Class
+---------------------------------------------------------------------*/
class XaudioPlayer : public XA_Player
{
public:
    XaudioPlayer(HINSTANCE instance);
   ~XaudioPlayer();
    virtual void HandleNotificationMessage(XA_Message *message);

protected:
    virtual void OnNotifyReady() {};
    virtual void OnNotifyAck(int command) {};
    virtual void OnNotifyNack(XA_NackInfo *info) {};
    virtual void OnNotifyPong(unsigned long tag) {};
    virtual void OnNotifyExited() {};
    virtual void OnNotifyPlayerState(XA_PlayerState state) {};
    virtual void OnNotifyPlayerMode(unsigned long mode) {};
    virtual void OnNotifyPlayerEnvironmentInteger(const char *name, long value) {};
    virtual void OnNotifyPlayerEnvironmentString(const char *name, const char *value) {};
    virtual void OnNotifyInputState(XA_InputState state) {};
    virtual void OnNotifyInputName(const char *name) {};
    virtual void OnNotifyInputCaps(unsigned long caps) {};
    virtual void OnNotifyInputPosition(unsigned long offset, unsigned long range) {};
    virtual void OnNotifyInputPositionRange(unsigned long range) {};
    virtual void OnNotifyInputTimecode(XA_TimecodeInfo *timecode) {};
    virtual void OnNotifyInputTimecodeGranularity(unsigned long granularity) {};
    virtual void OnNotifyInputDuration(unsigned long duration) {};
    virtual void OnNotifyInputStreamInfo(XA_StreamInfo *info) {};
    virtual void OnNotifyInputModule(int module) {};
    virtual void OnNotifyInputModuleInfo(XA_ModuleInfo *info) {};
    virtual void OnNotifyInputDeviceInfo(XA_DeviceInfo *info) {};
    virtual void OnNotifyInputFilterInfo(XA_FilterInfo *info) {};
    virtual void OnNotifyOutputState(XA_OutputState state) {};
    virtual void OnNotifyOutputName(const char *name) {};
    virtual void OnNotifyOutputCaps(unsigned long caps) {};
    virtual void OnNotifyOutputVolume(XA_VolumeInfo *info) {};
    virtual void OnNotifyOutputBalance(unsigned char balance) {};
    virtual void OnNotifyOutputPcmLevel(unsigned char level) {};
    virtual void OnNotifyOutputMasterLevel(unsigned char level) {};
    virtual void OnNotifyOutputChannels(XA_OutputChannels channels) {};
    virtual void OnNotifyOutputPorts(unsigned long ports) {};
    virtual void OnNotifyOutputModule(int module) {};
    virtual void OnNotifyOutputModuleInfo(XA_ModuleInfo *info) {};
    virtual void OnNotifyOutputDeviceInfo(XA_DeviceInfo *info) {};
    virtual void OnNotifyOutputFilterInfo(XA_FilterInfo *info) {};
    virtual void OnNotifyCodecQuality(unsigned int quality) {};
    virtual void OnNotifyCodecEqualizer(XA_EqualizerInfo *info) {};
    virtual void OnNotifyNotificationMask(unsigned long mask) {};
    virtual void OnNotifyDebugLevel(int level) {};
    virtual void OnNotifyProgress(XA_ProgressInfo *info) {};
    virtual void OnNotifyDebug(XA_DebugInfo *info) {};
    virtual void OnNotifyError(XA_ErrorInfo *info) {};

private:
    HWND m_HiddenWindow;
};

#endif /* __XAUDIO_PLAYER_H__ */