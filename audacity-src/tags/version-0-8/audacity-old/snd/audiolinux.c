#include "memory.h"
#include "stdio.h" 
#include "snd.h"

#ifdef __cplusplus
extern "C" {
#endif	

int audio_open(snd_node *n, long *f)
{
    printf("audio_open not implemented\n");
    return SND_SUCCESS;
}


int audio_close(snd_node n)
{
    printf("audio_close not implemented\n");
    return SND_SUCCESS;
}


int audio_flush(snd_type snd)
{
    printf("audio_flush not implemented\n");
    return SND_SUCCESS;
}


long audio_read(snd_node *n, void *p, long f)
{
    printf("audio_read not implemented\n");
    return 0;
}


long audio_write(snd_node *n, void *p, long f)
{
    printf("audio_write not implemented\n");
    return 0;
}

int audio_reset(snd_node *n)
{
    printf("audio reset not implemented\n");
    return SND_SUCCESS;
}


long audio_poll(snd_type snd)
{
    return 1000000;
}

snd_fns_node oldlinux_dictionary = { audio_poll, audio_read, audio_write, 
                       audio_open, audio_close, audio_reset, audio_flush };


void snd_init()
{
    snd_add_device("LinuxStd", "default", &oldlinux_dictionary);
}


#ifdef __cplusplus
} // extern "C"
#endif

