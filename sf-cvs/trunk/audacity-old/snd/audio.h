    
#ifdef __cplusplus
extern "C" {
#endif
    
int audio_buffer_size(snd_type snd);
/* in bytes.  max value that could be returned by audio_poll().     -eub */

long audio_poll(snd_type snd);
long audio_read(snd_type snd, void *buffer, long length);
long audio_write(snd_type snd, void *buffer, long length);
long audio_process(snd_type snd, void *buffer, long length);
int audio_open(snd_type snd, long *flags);
int audio_close(snd_type snd);
int audio_reset(snd_type snd);
int audio_flush(snd_type snd);

#ifdef __cplusplus
    }
#endif

#ifdef WIN32
#include "audiont.h"
#endif

#ifdef SGI
#include "audiosgi.h"
#endif

#ifdef LINUX
//#include "audiolinux.h"
#endif

