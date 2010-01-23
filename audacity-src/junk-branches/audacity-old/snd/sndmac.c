#include <stdio.h>

#include "snd.h"
#include "sndfileio.h"

#ifdef __WXMAC__
#include "wx/filefn.h"
#endif

void snd_fail(char *msg)
{
    printf("ERROR: %s\n", msg);
}

int snd_file_open(char *fname, int mode)
{
  return (int)fopen(fname, mode==SND_RDONLY? "rb" : "r+b");
}


int snd_file_creat(char *fname)
{
  return (int)fopen(fname, "wb");
}


long snd_file_len(int file)
{
  FILE *fp = (FILE *)file;
  
  long save = ftell(fp);
  fseek(fp, 0, SEEK_END);
  long len = ftell(fp);
  fseek(fp, save, SEEK_SET);
  
  return len;
}


long snd_file_read(int fp, char *data, long len)
{
    return fread(data, 1, len, (FILE *)fp);
}


long snd_file_write(int fp, char *data, long len)
{
    return fwrite(data, 1, len, (FILE *)fp);
}


int snd_file_close(int fp)
{
    return fclose((FILE *)fp);
}

int snd_file_lseek(int file, long offset, int param)
{
    if (param == SND_SEEK_CUR) param = SEEK_CUR;
    else if (param == SND_SEEK_SET) param = SEEK_SET;
    else param = SEEK_END;
    
    fseek((FILE *)file, offset, param);
    
    return ftell((FILE *)file);
}

void *snd_alloc(size_t s) { return malloc(s); }

void snd_free(void *a) { free(a); }
