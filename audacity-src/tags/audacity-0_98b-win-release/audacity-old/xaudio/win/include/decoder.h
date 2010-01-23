/*****************************************************************
|
|      MPEG audio decoder. SYNC API
|
|      (c) 1996-1998 MpegTV, LLC
|      Author: Gilles Boccon-Gibod (gilles@mpegtv.com)
|
 ****************************************************************/

#ifndef __DECODER_H__
#define __DECODER_H__

/*----------------------------------------------------------------------
|       includes
+---------------------------------------------------------------------*/
#include "xaudio.h"

/*----------------------------------------------------------------------
|       constants
+---------------------------------------------------------------------*/
#define XA_DECODER_INPUT_SEEKABLE                  0x0001

#define XA_DECODER_DEVICE_HAS_MASTER_LEVEL_CONTROL 0x00000001
#define XA_DECODER_DEVICE_HAS_PCM_LEVEL_CONTROL    0x00000002
#define XA_DECODER_DEVICE_HAS_BALANCE_CONTROL      0x00000004
#define XA_DECODER_DEVICE_HAS_LINE_OUT             0x00000008
#define XA_DECODER_DEVICE_HAS_SPEAKER_OUT          0x00000010
#define XA_DECODER_DEVICE_HAS_HEADPHONE_OUT        0x00000020
#define XA_DECODER_DEVICE_HAS_08_000_KHZ           0x00000040
#define XA_DECODER_DEVICE_HAS_11_025_KHZ           0x00000080
#define XA_DECODER_DEVICE_HAS_12_000_KHZ           0x00000100
#define XA_DECODER_DEVICE_HAS_16_000_KHZ           0x00000200
#define XA_DECODER_DEVICE_HAS_22_050_KHZ           0x00000400
#define XA_DECODER_DEVICE_HAS_24_000_KHZ           0x00000800
#define XA_DECODER_DEVICE_HAS_32_000_KHZ           0x00001000
#define XA_DECODER_DEVICE_HAS_44_100_KHZ           0x00002000
#define XA_DECODER_DEVICE_HAS_48_000_KHZ           0x00004000
#define XA_DECODER_DEVICE_HAS_8_BITS               0x00008000
#define XA_DECODER_DEVICE_HAS_MU_LAW               0x00010000
#define XA_DECODER_DEVICE_HAS_16_BITS              0x00020000
#define XA_DECODER_DEVICE_HAS_MUTE                 0x00040000
#define XA_DECODER_DEVICE_HAS_RESET                0x00080000
#define XA_DECODER_DEVICE_HAS_PAUSE                0x00100000
#define XA_DECODER_DEVICE_HAS_DRAIN                0x00200000
#define XA_DECODER_DEVICE_HAS_BUFFER_STATUS        0x00400000

#define XA_DECODER_DEVICE_FREQUENCIES_OFFSET       6
#define XA_DECODER_DEVICE_FREQUENCIES_MASK         0x1FF

#define XA_DECODER_CONTROL_BPS                     0x0001
#define XA_DECODER_CONTROL_STEREO                  0x0002
#define XA_DECODER_CONTROL_SAMPLE_RATE             0x0004
#define XA_DECODER_CONTROL_MASTER_LEVEL            0x0008
#define XA_DECODER_CONTROL_PCM_LEVEL               0x0010
#define XA_DECODER_CONTROL_BALANCE                 0x0020
#define XA_DECODER_CONTROL_PORTS                   0x0040
#define XA_DECODER_CONTROL_ALL_INFO                0x007F
#define XA_DECODER_CONTROL_MUTE                    0x0080
#define XA_DECODER_CONTROL_UNMUTE                  0x0100
#define XA_DECODER_CONTROL_RESET                   0x0200
#define XA_DECODER_CONTROL_PAUSE                   0x0400
#define XA_DECODER_CONTROL_RESTART                 0x0800
#define XA_DECODER_CONTROL_DRAIN                   0x1000

#define XA_DECODER_CONTROL_OUTPUT_LINE             0x01
#define XA_DECODER_CONTROL_OUTPUT_SPEAKER          0x02
#define XA_DECODER_CONTROL_OUTPUT_HEADPHONE        0x04

#define XA_DECODER_ENVIRONMENT_MAX_NAME_LENGTH     256
#define XA_DECODER_ENVIRONMENT_MAX_STRING_LENGTH   1024

#define XA_DECODER_INPUT_QUERY_MODULE_NAME         0x01
#define XA_DECODER_INPUT_QUERY_NB_DEVICES          0x02
#define XA_DECODER_INPUT_QUERY_DEVICE_NAME         0x04

#define XA_DECODER_INPUT_QUERY_NAME_IS_GENERIC     0x01
#define XA_DECODER_INPUT_QUERY_DEVICE_IS_DEFAULT   0x02

#define XA_DECODER_OUTPUT_QUERY_MODULE_NAME        0x01
#define XA_DECODER_OUTPUT_QUERY_NB_DEVICES         0x02
#define XA_DECODER_OUTPUT_QUERY_DEVICE_NAME        0x04

#define XA_DECODER_OUTPUT_QUERY_NAME_IS_GENERIC    0x01
#define XA_DECODER_OUTPUT_QUERY_DEVICE_IS_DEFAULT  0x02
#define XA_DECODER_OUTPUT_QUERY_DEVICE_IS_SHAREABLE 0x04

#define XA_DECODER_MAX_NAME_LENGTH                 255
#define XA_DECODER_MAX_DESCRIPTION_LENGTH          255

#define XA_DECODER_INPUT_AUTOSELECT                -1
#define XA_DECODER_OUTPUT_AUTOSELECT               -1

#define XA_DECODER_INPUT_FILTER_FIRST              -1
#define XA_DECODER_INPUT_FILTER_LAST                0
#define XA_DECODER_INPUT_FILTER_BY_NAME            -2

#define XA_DECODER_OUTPUT_FILTER_FIRST             -1
#define XA_DECODER_OUTPUT_FILTER_LAST               0
#define XA_DECODER_OUTPUT_FILTER_BY_NAME           -2

#define XA_TIMECODE_FRACTIONS_PER_SECOND            100
#define XA_TIMECODE_FRACTIONS_PER_MINUTE        (100*60)
#define XA_TIMECODE_FRACTIONS_PER_HOUR       (100*60*60)

#define XA_DECODER_CODEC_QUALITY_HIGH               0
#define XA_DECODER_CODEC_QUALITY_MEDIUM             1
#define XA_DECODER_CODEC_QUALITY_LOW                2

#define XA_DECODER_EQUALIZER_NB_BANDS               32
#define XA_DECODER_FEEDBACK_NB_BANDS                32

/*----------------------------------------------------------------------
|       types
+---------------------------------------------------------------------*/
typedef void (*XA_EnvironmentCallback)(void *listener,
                                       const char *name, 
                                       void  *value);

typedef struct {
    int h;
    int m;
    int s;
    int f;
} XA_TimeCode;

typedef struct {
    long seconds;
    long microseconds;
} XA_AbsoluteTime;

enum XA_IOState { XA_IO_STATE_CLOSED = 0, XA_IO_STATE_OPEN };

typedef struct {
    int changed;
    int level;
    int layer;
    int bitrate;
    int frequency;
    int mode;
} XA_InputStreamInfo;

typedef struct {
    int index;
    unsigned long flags;
    char name[XA_DECODER_MAX_NAME_LENGTH];
    char description[XA_DECODER_MAX_DESCRIPTION_LENGTH];
} XA_InputModuleQuery;

typedef struct {
    int   id;
    void *global;
} XA_InputModuleClassInfo;

struct XA_DecoderInfo;

typedef struct {
    int  (*input_module_probe)(const char *name);
    int  (*input_module_query)(XA_InputModuleQuery *query, 
                               unsigned long query_mask);
    int  (*input_new)(void **input, const char *name,
                      XA_InputModuleClassInfo *class_info,
                      struct XA_DecoderInfo *decoder);
    int  (*input_delete)(void *input);
    int  (*input_open)(void *input);
    int  (*input_close)(void *input);
    int  (*input_read)(void *input, void *buffer, unsigned long n);
    int  (*input_seek)(void *input, unsigned long offset);
    long (*input_get_caps)(void *input);
    long (*input_get_size)(void *input);
    int  (*input_send_message)(void *input, int type, 
                               const void *data, unsigned int size);
} XA_InputModule;

struct XA_InputFilterNode;

typedef struct {
    const char *(*filter_get_name)(void);
    int  (*filter_new)(void **filter, int id,
                       struct XA_DecoderInfo *decoder);
    int  (*filter_delete)(void *filter);
    int  (*input_open)(struct XA_InputFilterNode *node);
    int  (*input_close)(struct XA_InputFilterNode *node);
    int  (*input_read)(struct XA_InputFilterNode *node, 
                       void *buffer, unsigned long n);
    int  (*input_seek)(struct XA_InputFilterNode *node, 
                       unsigned long offset);
    long (*input_get_caps)(struct XA_InputFilterNode *node);
    long (*input_get_size)(struct XA_InputFilterNode *node);
    int  (*input_send_message)(struct XA_InputFilterNode *node, 
                               int type, const void *data, unsigned int size);
} XA_InputFilter;

typedef struct XA_InputFilterNode {
    int                        id;
    void                      *instance;
    XA_InputFilter             vtable;
    struct XA_InputFilterNode *next;
    struct XA_InputFilterNode *prev;
} XA_InputFilterNode;

typedef struct {
    enum XA_IOState       state;
    unsigned long         caps;
    unsigned long         position;
    unsigned long         size;
    unsigned long         duration;
    void                 *device;
    const XA_InputModule *module;
    const char           *name;
} XA_InputInfo;

typedef enum { 
    XA_OUTPUT_STEREO, 
    XA_OUTPUT_MONO_LEFT,         
    XA_OUTPUT_MONO_RIGHT,
    XA_OUTPUT_MONO_MIX 
} XA_OutputChannelsMode;

typedef struct {
    short left[XA_DECODER_FEEDBACK_NB_BANDS];
    short right[XA_DECODER_FEEDBACK_NB_BANDS];
} XA_OutputFeedbackBands;

typedef struct {
    int                     changed;
    short                  *pcm_samples;
    int                     size;
    int                     bytes_per_sample;
    int                     stereo;
    int                     sample_rate;
    unsigned long           delay;
    /*XA_OutputFeedbackBands *feedback;*/
} XA_OutputBufferInfo;

typedef struct {
    unsigned long delay;
} XA_OutputStatus;

typedef struct {
    int bytes_per_sample;
    int sample_rate;
    int stereo;
    int master_level;
    int pcm_level;
    int balance;
    int ports;
} XA_OutputControl;

typedef struct {
    int index;
    unsigned long flags;
    char name[XA_DECODER_MAX_NAME_LENGTH];
    char description[XA_DECODER_MAX_DESCRIPTION_LENGTH];
} XA_OutputModuleQuery;

typedef struct {
    int   id;
    void *global;
} XA_OutputModuleClassInfo;

typedef struct {
    int   (*output_module_probe)(const char *name);
    int   (*output_module_query)(XA_OutputModuleQuery *query, 
                                 unsigned long query_mask);
    int   (*output_new)(void **output, const char *name,
                        XA_OutputModuleClassInfo *class_info,
                        struct XA_DecoderInfo *decoder);
    int   (*output_delete)(void *output);
    int   (*output_open)(void *output);
    int   (*output_close)(void *output);
    int   (*output_write)(void *output, void *buffer, 
                          unsigned long size, int bytes_per_sample,
                          int stereo, int sample_rate);
    void* (*output_get_buffer)(void *output, unsigned long size); 
    int   (*output_set_control)(void *output, 
                                XA_OutputControl *control, 
                                unsigned long flags);
    int   (*output_get_control)(void *output, 
                                XA_OutputControl *control, 
                                unsigned long control_mask);
    int   (*output_get_status)(void *output,
                               XA_OutputStatus *status);
    long  (*output_get_caps)(void *output);
    int   (*output_send_message)(void *output, int type, 
                                 const void *data, unsigned int size);
} XA_OutputModule;

struct XA_OutputFilterNode;

typedef struct {
    const char *(*filter_get_name)(void);
    int  (*filter_new)(void **filter, int id,
                       struct XA_DecoderInfo *decoder);
    int  (*filter_delete)(void *filter);
    int  (*output_open)(struct XA_OutputFilterNode *node);
    int  (*output_close)(struct XA_OutputFilterNode *node);
    int  (*output_write)(struct XA_OutputFilterNode *node, 
                         void *buffer, 
                         unsigned long size, int bytes_per_sample,
                         int stereo, int sample_rate);
    void* (*output_get_buffer)(struct XA_OutputFilterNode *node, 
                               unsigned long size); 
    int  (*output_set_control)(struct XA_OutputFilterNode *node, 
                               XA_OutputControl *control, 
                               unsigned long flags);
    int  (*output_get_control)(struct XA_OutputFilterNode *node, 
                               XA_OutputControl *control, 
                               unsigned long control_mask);
    int  (*output_get_status)(struct XA_OutputFilterNode *node,
                              XA_OutputStatus *status);
    long (*output_get_caps)(struct XA_OutputFilterNode *node);
    int  (*output_send_message)(struct XA_OutputFilterNode *node, 
                                int type, const void *data, unsigned int size);
} XA_OutputFilter;

typedef struct XA_OutputFilterNode {
    int                         id;
    void                       *instance;
    XA_OutputFilter             vtable;
    struct XA_OutputFilterNode *next;
    struct XA_OutputFilterNode *prev;
} XA_OutputFilterNode;

typedef struct {
    enum XA_IOState            state;
    unsigned long              caps;
    void                      *device;
    const XA_OutputModule     *module;
    const char                *name;
} XA_OutputInfo;

typedef struct {
    signed char left[XA_DECODER_EQUALIZER_NB_BANDS];
    signed char right[XA_DECODER_EQUALIZER_NB_BANDS];
} XA_EqualizerInfo;

typedef struct {
    int nb_input_modules;
    int nb_output_modules;
} XA_ModulesInfo;

typedef struct {
    unsigned long      frame;
    float              position;
    XA_InputStreamInfo info;
    XA_TimeCode        timecode;
} XA_StatusInfo;

struct XA_DecoderPrivateInfo;
struct XA_DecoderInfo;

typedef struct {
    int (XA_EXPORT *get_environment_integer)(struct XA_DecoderInfo *decoder,
                                             const char *name,
                                             long *value);
    int (XA_EXPORT *get_environment_string)(struct XA_DecoderInfo *decoder,
                                            const char *name,
                                            char *value);    
    int (XA_EXPORT *add_environment_listener)(struct XA_DecoderInfo *decoder,
                                              const char *name,
                                              void *client,
                                              XA_EnvironmentCallback callback,
                                              void **handle);
    int (XA_EXPORT *remove_environment_listener)(struct XA_DecoderInfo *decoder,
                                                 void *handle);
} XA_DecoderCallbackTable;

typedef struct XA_DecoderInfo {
    const char                    *name;
    XA_InputInfo                  *input;
    XA_OutputInfo                 *output;
    XA_ModulesInfo                *modules;
    XA_StatusInfo                 *status;
    XA_OutputBufferInfo           *output_buffer;
    XA_NotificationClient          notification_client;
    XA_DecoderCallbackTable        callbacks;
    struct XA_DecoderPrivateInfo *hidden;
} XA_DecoderInfo;

/*----------------------------------------------------------------------
|       prototypes
+---------------------------------------------------------------------*/
#ifdef __cplusplus
extern "C" {
#endif

char* XA_EXPORT decoder_version(char **major, char **minor, char **state);
int XA_EXPORT decoder_new(XA_DecoderInfo **decoder);
int XA_EXPORT decoder_delete(XA_DecoderInfo *decoder);
int XA_EXPORT decoder_input_module_register(XA_DecoderInfo *decoder, 
                                            const XA_InputModule *module);
int XA_EXPORT decoder_input_module_query(XA_DecoderInfo *decoder,
                                         int module,
                                         XA_InputModuleQuery *query,
                                         unsigned long query_flags);
int XA_EXPORT decoder_input_add_filter(XA_DecoderInfo *decoder, 
                                       const XA_InputFilter *filter,
                                       int where);
int XA_EXPORT decoder_input_remove_filter(XA_DecoderInfo *decoder, 
                                          const char *name, int id); 
int XA_EXPORT decoder_input_filters_list(XA_DecoderInfo *decoder,
                                         void *client,
                                         void (*callback)
                                         (void *client, 
                                          XA_InputFilterNode *node));
int XA_EXPORT decoder_input_new(XA_DecoderInfo *decoder, 
                                const char *name, int module);
int XA_EXPORT decoder_input_delete(XA_DecoderInfo *decoder);
int XA_EXPORT decoder_input_open(XA_DecoderInfo *decoder);
int XA_EXPORT decoder_input_close(XA_DecoderInfo *decoder);
int XA_EXPORT decoder_input_read(XA_DecoderInfo *decoder, 
                                 void *buffer, int size);
int XA_EXPORT decoder_input_seek_to_offset(XA_DecoderInfo *decoder, 
                                           unsigned long offset);
int XA_EXPORT decoder_input_seek_to_position(XA_DecoderInfo *decoder, 
                                             float position);
int XA_EXPORT decoder_input_seek_to_time(XA_DecoderInfo *decoder, 
                                         float seconds);
int XA_EXPORT decoder_input_seek_to_timecode(XA_DecoderInfo *decoder, 
                                             const XA_TimeCode *timecode);
int XA_EXPORT decoder_input_send_message(XA_DecoderInfo *decoder,
                                         int type,
                                         const void *data,
                                         unsigned int size);
int XA_EXPORT decoder_output_module_register(XA_DecoderInfo *decoder,
                                             const XA_OutputModule *module);
int XA_EXPORT decoder_output_module_query(XA_DecoderInfo *decoder,
                                          int module,
                                          XA_OutputModuleQuery *query,
                                          unsigned long query_flags);
int XA_EXPORT decoder_output_add_filter(XA_DecoderInfo *decoder, 
                                        const XA_OutputFilter *filter,
                                        int where);
int XA_EXPORT decoder_output_remove_filter(XA_DecoderInfo *decoder, 
                                           const char *name, int id); 
int XA_EXPORT decoder_output_filters_list(XA_DecoderInfo *decoder,
                                          void *client,
                                          void (*callback)
                                          (void *client, 
                                           XA_OutputFilterNode *node));
int XA_EXPORT decoder_output_new(XA_DecoderInfo *decoder,
                                 const char *name, int module);
int XA_EXPORT decoder_output_delete(XA_DecoderInfo *decoder);
int XA_EXPORT decoder_output_open(XA_DecoderInfo *decoder); 
int XA_EXPORT decoder_output_close(XA_DecoderInfo *decoder);
int XA_EXPORT decoder_output_write(XA_DecoderInfo *decoder, void *buffer, 
                                   unsigned long size, 
                                   int bytes_per_sample, 
                                   int stereo, int sample_rate);
int XA_EXPORT decoder_output_set_control(XA_DecoderInfo *decoder, 
                                         XA_OutputControl *control,
                                         unsigned long control_flags);
int XA_EXPORT decoder_output_get_control(XA_DecoderInfo *decoder, 
                                         XA_OutputControl *control,
                                         unsigned long control_flags);
int XA_EXPORT decoder_output_send_message(XA_DecoderInfo *decoder,
                                          int type,
                                          const void *data,
                                          unsigned int size);
int XA_EXPORT decoder_codec_set_channels(XA_DecoderInfo *decoder,
                                         XA_OutputChannelsMode channels);
int XA_EXPORT decoder_codec_get_channels(XA_DecoderInfo *decoder);
int XA_EXPORT decoder_codec_set_quality(XA_DecoderInfo *decoder,
                                        unsigned int quality);
int XA_EXPORT decoder_codec_get_quality(XA_DecoderInfo *decoder);
int XA_EXPORT decoder_codec_set_equalizer(XA_DecoderInfo *decoder,
                                          XA_EqualizerInfo *equalizer);
int XA_EXPORT decoder_codec_get_equalizer(XA_DecoderInfo *decoder,
                                          XA_EqualizerInfo *equalizer);
int XA_EXPORT decoder_codec_enable_feedback(XA_DecoderInfo *decoder);
int XA_EXPORT decoder_codec_disable_feedback(XA_DecoderInfo *decoder);
int XA_EXPORT decoder_set_environment_integer(XA_DecoderInfo *decoder,
                                              const char *name,
                                              long value);
int XA_EXPORT decoder_get_environment_integer(XA_DecoderInfo *decoder,
                                              const char *name,
                                              long *value);
int XA_EXPORT decoder_set_environment_string(XA_DecoderInfo *decoder,
                                             const char *name,
                                             const char *value);
int XA_EXPORT decoder_get_environment_string(XA_DecoderInfo *decoder,
                                             const char *name,
                                             char *value);
int XA_EXPORT decoder_unset_environment(XA_DecoderInfo *decoder,
                                        const char *name);
int XA_EXPORT decoder_add_environment_listener(XA_DecoderInfo *decoder,
                                               const char *name,
                                               void *client,
                                               XA_EnvironmentCallback callback,
                                               void **handle);
int XA_EXPORT decoder_remove_environment_listener(XA_DecoderInfo *decoder,
                                                  void *handle);

int XA_EXPORT decoder_decode(XA_DecoderInfo *decoder, void *output_buffer);
int XA_EXPORT decoder_play(XA_DecoderInfo *decoder);

#ifdef __cplusplus
}

class XA_Decoder {
public:
    XA_DecoderInfo *info;
    XA_Decoder() { 
        info = (XA_DecoderInfo *)0; 
        decoder_new(&info); 
    }
    ~XA_Decoder() { 
        decoder_delete(info); 
    }
    char* Version(char **major, char **minor, char **state) {
        return decoder_version(major, minor, state);
    }
    int InputModuleRegister(const XA_InputModule *module) {
        return decoder_input_module_register(info, module);
    }
    int InputModuleQuery(int module, XA_InputModuleQuery *query,
                         unsigned long query_flags) {
        return decoder_input_module_query(info, module, query, query_flags);
    }
    int InputAddFilter(const XA_InputFilter *filter, int where) {
        return decoder_input_add_filter(info, filter, where);
    }
    int InputRemoveFilter(const char *name, int id) {
        return decoder_input_remove_filter(info, name, id);
    }
    int InputNew(const char *name, int module) {
        return decoder_input_new(info, name, module);
    }
    int InputDelete() {
        return decoder_input_delete(info);
    }
    int InputOpen() {
        return decoder_input_open(info);
    }
    int InputClose() {
        return decoder_input_close(info);
    }
    int InputSeekToOffset(unsigned long offset) {
        return decoder_input_seek_to_offset(info, offset);
    }
    int InputSeekToPosition(float position) {
        return decoder_input_seek_to_position(info, position);
    }
    int InputSeekToTime(float seconds) {
        return decoder_input_seek_to_time(info, seconds);
    }
    int InputSeekToTimecode(const XA_TimeCode *timecode) {
        return decoder_input_seek_to_timecode(info, timecode);
    }
    int InputSendMessage(int type, const void *data, unsigned int size) {
        return decoder_input_send_message(info, type, data, size);
    }
    int OutputModuleRegister(const XA_OutputModule *module) {
        return decoder_output_module_register(info, module);
    }
    int OutputModuleQuery(int module, XA_OutputModuleQuery *query,
                          unsigned long query_flags) {
        return decoder_output_module_query(info, module, query, query_flags);
    }
    int OutputAddFilter(const XA_OutputFilter *filter, int where) {
        return decoder_output_add_filter(info, filter, where);
    }
    int OutputRemoveFilter(const char *name, int id) {
        return decoder_output_remove_filter(info, name, id);
    }
    int OutputNew(const char *name, int module) {
        return decoder_output_new(info, name, module);
    }
    int OutputDelete() {
        return decoder_output_delete(info);
    }
    int OutputOpen() {
        return decoder_output_open(info);
    }
    int OutputClose() {
        return decoder_output_close(info);
    }
    int OutputSetControl(XA_OutputControl *control, 
                         unsigned long control_flags) {
        return decoder_output_set_control(info, control, control_flags);
    }
    int OutputGetControl(XA_OutputControl *control,
                         unsigned long control_flags) {
        return decoder_output_get_control(info, control, control_flags);
    }
    int OutputSendMessage(int type, const void *data, unsigned int size) {
        return decoder_output_send_message(info, type, data, size);
    }
    int CodecSetEqualizer(XA_EqualizerInfo *equalizer) {
        return decoder_codec_set_equalizer(info, equalizer);
    }
    int CodecGetEqualizer(XA_EqualizerInfo *equalizer) {
        return decoder_codec_get_equalizer(info, equalizer);
    }
    int SetEnvironmentInteger(const char *name, long value) {
        return decoder_set_environment_integer(info, name, value);
    }
    int GetEnvironmentInteger(const char *name, long *value) {
        return decoder_get_environment_integer(info, name, value);
    }
    int SetEnvironmentString(const char *name, const char *value) {
        return decoder_set_environment_string(info, name, value);
    }
    int GetEnvironmentString(const char *name, char *value) {
        return decoder_get_environment_string(info, name, value);
    }
    int UnsetEnvironment(const char *name) {
        return decoder_unset_environment(info, name);
    }
    int AddEnvironmentListener(const char *name, void *client,
                               XA_EnvironmentCallback callback,
                               void **handle) {
        return decoder_add_environment_listener(info, name, client, 
                                                callback, handle);
    }
    int RemoveEnvironmentListener(void *handle) {
        return decoder_remove_environment_listener(info, handle);
    }
    int Decoder(void *output_buffer) {
        return decoder_decode(info, output_buffer);
    }
    int Play() {
        return decoder_play(info);
    }
};
#endif

#endif /* __DECODER_H__ */



















