/*****************************************************************
|
|      XAudio Player GUI. Asynchronous Player Logic    
|
|
|      (c) 1996-1998 MpegTV, LLC
|      Author: Gilles Boccon-Gibod (gilles@mpegtv.com)
|
 ****************************************************************/

#ifndef __PLAYER_H__
#define __PLAYER_H__

/*----------------------------------------------------------------------
|       includes
+---------------------------------------------------------------------*/
#include "xaudio.h"
#include "control.h"

/*----------------------------------------------------------------------
|       constants
+---------------------------------------------------------------------*/
#define XA_PLAYER_MODE_OUTPUT_AUTO_CLOSE_ON_STOP        0x0001
#define XA_PLAYER_MODE_OUTPUT_AUTO_CLOSE_ON_PAUSE       0x0002
#define XA_PLAYER_MODE_OUTPUT_AUTO_CLOSE_ON_EOF         0x0004

#define XA_NOTIFY_MASK_ERROR                            0x0000001
#define XA_NOTIFY_MASK_DEBUG                            0x0000002
#define XA_NOTIFY_MASK_PROGRESS                         0x0000004
#define XA_NOTIFY_MASK_ACK                              0x0000008
#define XA_NOTIFY_MASK_NACK                             0x0000010
#define XA_NOTIFY_MASK_PLAYER_STATE                     0x0000020
#define XA_NOTIFY_MASK_INPUT_STATE                      0x0000040
#define XA_NOTIFY_MASK_INPUT_NAME                       0x0000080
#define XA_NOTIFY_MASK_INPUT_CAPS                       0x0000100
#define XA_NOTIFY_MASK_INPUT_DURATION                   0x0000200
#define XA_NOTIFY_MASK_INPUT_POSITION                   0x0000400
#define XA_NOTIFY_MASK_INPUT_POSITION_RANGE             0x0000800
#define XA_NOTIFY_MASK_INPUT_TIMECODE                   0x0001000
#define XA_NOTIFY_MASK_INPUT_TIMECODE_GRANULARITY       0x0002000
#define XA_NOTIFY_MASK_INPUT_STREAM_INFO                0x0004000
#define XA_NOTIFY_MASK_OUTPUT_STATE                     0x0008000
#define XA_NOTIFY_MASK_OUTPUT_NAME                      0x0010000
#define XA_NOTIFY_MASK_OUTPUT_CAPS                      0x0020000
#define XA_NOTIFY_MASK_OUTPUT_VOLUME                    0x0040000
#define XA_NOTIFY_MASK_OUTPUT_BALANCE                   0x0080000
#define XA_NOTIFY_MASK_OUTPUT_PCM_LEVEL                 0x0100000
#define XA_NOTIFY_MASK_OUTPUT_MASTER_LEVEL              0x0200000
#define XA_NOTIFY_MASK_OUTPUT_PORTS                     0x0400000
#define XA_NOTIFY_MASK_CODEC_EQUALIZER                  0x0800000
#define XA_NOTIFY_MASK_FEEDBACK_EVENT                   0x1000000

#define XA_OUTPUT_VOLUME_IGNORE_FIELD                   255

/*----------------------------------------------------------------------
|       prototypes
+---------------------------------------------------------------------*/
#ifdef __cplusplus
extern "C" {
#endif

extern void player_procedure(void *control, void *args);
extern int XA_EXPORT player_new(void **control, void *args);
extern int XA_EXPORT player_delete(void *control);
extern int XA_EXPORT player_set_priority(void *control, int priority);
extern int XA_EXPORT player_get_priority(void *control);

#ifdef __cplusplus
}

#define XA_CMSEND control_message_send

class XA_Player {
public:
    void *control;
    XA_Player() {
        control = (void *)0;
    }
    ~XA_Player() {
        if (control) player_delete(control);
    }
    int Run(void *args) {
        int status;
        status = player_new(&control, args);
        if (status) control = (void *)0;
        return status;
    }
    int SetPriority(int priority) {
        return player_set_priority(control, priority);
    }
    int GetPriority() {
        return player_get_priority(control);
    }
    int GetMessage(XA_Message *message) {
        return control_message_get(control, message);
    }
    int WaitMessage(XA_Message *message, int timeout) {
        return control_message_wait(control, message, timeout);
    }
    int Exit() { 
        return XA_CMSEND(control, XA_MSG_COMMAND_EXIT);
    }
    int Ping(unsigned long tag) {
        return XA_CMSEND(control, XA_MSG_COMMAND_PING, tag);
    }
    int Play() {
        return XA_CMSEND(control, XA_MSG_COMMAND_PLAY);
    }
    int Pause() {
        return XA_CMSEND(control, XA_MSG_COMMAND_PAUSE);
    }
    int Stop() {
        return XA_CMSEND(control, XA_MSG_COMMAND_STOP);
    }
    int Seek(unsigned long offset, unsigned long range) {
        return XA_CMSEND(control, XA_MSG_COMMAND_SEEK, offset, range);
    }
    int InputOpen(const char *name) {
        return XA_CMSEND(control, XA_MSG_COMMAND_INPUT_OPEN, name);
    }
    int InputClose() {
        return XA_CMSEND(control, XA_MSG_COMMAND_INPUT_CLOSE);
    }
    int InputSendMessage(unsigned short type, void *data, unsigned int size) {
        return XA_CMSEND(control, XA_MSG_COMMAND_INPUT_SEND_MESSAGE,
                         type, data, size);
    }
    int InputAddFilter(const char *name, int where) {
        return XA_CMSEND(control, XA_MSG_COMMAND_INPUT_ADD_FILTER, 
                         name, where);
    }
    int InputRemoveFilter(const char *name, int id) {
        return XA_CMSEND(control, XA_MSG_COMMAND_INPUT_REMOVE_FILTER, 
                         name, id);
    }
    int InputFiltersList() {
        return XA_CMSEND(control, XA_MSG_COMMAND_INPUT_FILTERS_LIST);
    }
    int InputModuleRegister(const char *name) {
        return XA_CMSEND(control, XA_MSG_COMMAND_INPUT_MODULE_REGISTER, name);
    }
    int InputModuleQuery(int module) {
        return XA_CMSEND(control, XA_MSG_COMMAND_INPUT_MODULE_QUERY, module);
    }
    int InputModulesList() {
        return XA_CMSEND(control, XA_MSG_COMMAND_INPUT_MODULES_LIST);
    }
    int OutputOpen(const char *name) {
        return XA_CMSEND(control, XA_MSG_COMMAND_OUTPUT_OPEN, name);
    }
    int OutputClose() {
        return XA_CMSEND(control, XA_MSG_COMMAND_OUTPUT_CLOSE);
    }
    int OutputMute() {
        return XA_CMSEND(control, XA_MSG_COMMAND_OUTPUT_MUTE);
    }
    int OutputUnmute() {
        return XA_CMSEND(control, XA_MSG_COMMAND_OUTPUT_UNMUTE);
    }
    int OutputReset() {
        return XA_CMSEND(control, XA_MSG_COMMAND_OUTPUT_RESET);
    }
    int OutputDrain() {
        return XA_CMSEND(control, XA_MSG_COMMAND_OUTPUT_DRAIN);
    }
    int OutputSendMessage(unsigned short type, void *data, unsigned int size) {
        return XA_CMSEND(control, XA_MSG_COMMAND_OUTPUT_SEND_MESSAGE,
                         type, data, size);
    }
    int OutputAddFilter(const char *name, int where) {
        return XA_CMSEND(control, XA_MSG_COMMAND_OUTPUT_ADD_FILTER, 
                         name, where);
    }
    int OutputRemoveFilter(const char *name, int id) {
        return XA_CMSEND(control, XA_MSG_COMMAND_OUTPUT_REMOVE_FILTER, 
                         name, id);
    }
    int OutputFiltersList() {
        return XA_CMSEND(control, XA_MSG_COMMAND_OUTPUT_FILTERS_LIST);
    }
    int OutputModuleRegister(const char *name) {
        return XA_CMSEND(control, XA_MSG_COMMAND_OUTPUT_MODULE_REGISTER, name);
    }
    int OutputModuleQuery(int module) {
        return XA_CMSEND(control, XA_MSG_COMMAND_OUTPUT_MODULE_QUERY, module);
    }
    int OutputModulesList() {
        return XA_CMSEND(control, XA_MSG_COMMAND_OUTPUT_MODULES_LIST);
    }
    int SetPlayerMode(unsigned long mode) {
         return XA_CMSEND(control, XA_MSG_SET_PLAYER_MODE, mode);
    }
    int GetPlayerMode() {
        return XA_CMSEND(control, XA_MSG_GET_PLAYER_MODE);
    }
    int SetPlayerEnvironmentInteger(const char *name, int value) {
        return XA_CMSEND(control, XA_MSG_SET_PLAYER_ENVIRONMENT_INTEGER,
                         name, value);
    }
    int GetPlayerEnvironmentInteger(const char *name) {
        return XA_CMSEND(control, XA_MSG_GET_PLAYER_ENVIRONMENT_INTEGER, name);
    }
    int SetPlayerEnvironmentString(const char *name, const char *value) {
        return XA_CMSEND(control, XA_MSG_SET_PLAYER_ENVIRONMENT_STRING, 
                         name, value);
    }
    int GetPlayerEnvironmentString(const char *name) {
        return XA_CMSEND(control, XA_MSG_GET_PLAYER_ENVIRONMENT_STRING, name);
    }
    int UnsetPlayerEnvironment(const char *name) {
        return XA_CMSEND(control, XA_MSG_UNSET_PLAYER_ENVIRONMENT, name);
    }
    int SetInputModule(int module) {
        return XA_CMSEND(control, XA_MSG_SET_INPUT_MODULE, module);
    }
    int GetInputModule() {
        return XA_CMSEND(control, XA_MSG_GET_INPUT_MODULE);
    }
    int SetInputName(const char *name) {
        return XA_CMSEND(control, XA_MSG_SET_INPUT_NAME, name);
    }
    int GetInputName() {
        return XA_CMSEND(control, XA_MSG_GET_INPUT_NAME);
    }
    int SetInputPositionRange(unsigned long range) {
        return XA_CMSEND(control, XA_MSG_SET_INPUT_POSITION_RANGE, range);
    }
    int GetInputPositionRange() {
        return XA_CMSEND(control, XA_MSG_GET_INPUT_POSITION_RANGE);
    }
    int SetInputTimecodeGranularity(unsigned long granularity) {
        return XA_CMSEND(control, XA_MSG_SET_INPUT_TIMECODE_GRANULARITY,
                         granularity);
    }
    int GetInputTimecodeGranularity() {
        return XA_CMSEND(control, XA_MSG_GET_INPUT_TIMECODE_GRANULARITY);
    }
    int SetOutputModule(int module) {
        return XA_CMSEND(control, XA_MSG_SET_OUTPUT_MODULE, module);
    }
    int GetOutputModule() {
        return XA_CMSEND(control, XA_MSG_GET_OUTPUT_MODULE);
    }
    int SetOutputName(const char *name) {
        return XA_CMSEND(control, XA_MSG_SET_OUTPUT_NAME, name);
    }
    int GetOutputName() {
        return XA_CMSEND(control, XA_MSG_GET_OUTPUT_NAME);
    }
    int SetOutputVolume(int balance, int pcm_level, int master_level) {
        return XA_CMSEND(control, XA_MSG_SET_OUTPUT_VOLUME,
                         balance, pcm_level, master_level);
    }
    int GetOutputVolume() {
        return XA_CMSEND(control, XA_MSG_GET_OUTPUT_VOLUME);
    }
    int SetOutputChannels(int channels) {
        return XA_CMSEND(control, XA_MSG_SET_OUTPUT_CHANNELS, channels);
    }
    int GetOutputChannels() {
        return XA_CMSEND(control, XA_MSG_GET_OUTPUT_CHANNELS);
    }
    int SetOutputPorts(unsigned char ports) {
        return XA_CMSEND(control, XA_MSG_SET_OUTPUT_PORTS, ports);
    }
    int GetOutputPorts() {
        return XA_CMSEND(control, XA_MSG_GET_OUTPUT_PORTS);
    }
    int SetOutputFeedbackRate(int rate) {
        return XA_CMSEND(control, XA_MSG_SET_OUTPUT_FEEDBACK_RATE, rate);
    }
    int GetOutputFeedbackRate() {
        return XA_CMSEND(control, XA_MSG_GET_OUTPUT_FEEDBACK_RATE);
    }
    int SetCodecQuality(unsigned int quality) {
        return XA_CMSEND(control, XA_MSG_SET_CODEC_QUALITY, quality);
    }
    int GetCodecQuality() {
        return XA_CMSEND(control, XA_MSG_GET_CODEC_QUALITY);
    }
    int SetCodecEqualizer(XA_EqualizerInfo *equalizer) {
        return XA_CMSEND(control, XA_MSG_SET_CODEC_EQUALIZER, equalizer);
    }
    int GetCodecEqualizer() {
        return XA_CMSEND(control, XA_MSG_GET_CODEC_EQUALIZER);
    }
    int SetNotificationMask(unsigned long mask) {
        return XA_CMSEND(control, XA_MSG_SET_NOTIFICATION_MASK, mask);
    }
    int GetNotificationMask() {
        return XA_CMSEND(control, XA_MSG_GET_NOTIFICATION_MASK);
    }
    int SetDebugLevel(int level) {
        return XA_CMSEND(control, XA_MSG_SET_DEBUG_LEVEL, level);
    }
    int GetDebugLevel() {
        return XA_CMSEND(control, XA_MSG_GET_DEBUG_LEVEL);
    }
};
#endif

#endif /* __PLAYER_H__ */
