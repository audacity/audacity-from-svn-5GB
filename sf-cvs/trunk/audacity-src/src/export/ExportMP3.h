/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP3.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORTMP3__
#define __AUDACITY_EXPORTMP3__

#include <wx/string.h>
#include <wx/dynlib.h>

#include "lame.h"

/* --------------------------------------------------------------------------*/

typedef lame_global_flags *lame_init_t(void);
typedef int lame_init_params_t(lame_global_flags*);
typedef const char* get_lame_version_t(void);

typedef int lame_encode_buffer_t (
      lame_global_flags* gf,
      const short int    buffer_l [],
      const short int    buffer_r [],
      const int          nsamples,
      unsigned char *    mp3buf,
      const int          mp3buf_size );

typedef int lame_encode_buffer_interleaved_t(
      lame_global_flags* gf,
      short int          pcm[],
      int                num_samples,   /* per channel */
      unsigned char*     mp3buf,
      int                mp3buf_size );

typedef int lame_encode_flush_t(
      lame_global_flags *gf,
      unsigned char*     mp3buf,
      int                size );

typedef int lame_close_t(lame_global_flags*);

typedef int lame_set_in_samplerate_t(lame_global_flags*, int);
typedef int lame_set_out_samplerate_t(lame_global_flags*, int);
typedef int lame_set_num_channels_t(lame_global_flags*, int );
typedef int lame_set_quality_t(lame_global_flags*, int);
typedef int lame_get_quality_t(lame_global_flags*);
typedef int lame_set_brate_t(lame_global_flags*, int);
typedef int lame_get_brate_t(lame_global_flags*);
typedef int lame_set_VBR_t(lame_global_flags *, vbr_mode);
typedef vbr_mode lame_get_VBR_t(const lame_global_flags *);
typedef int lame_set_VBR_q_t(lame_global_flags *, int);
typedef int lame_get_VBR_q_t(const lame_global_flags *);
typedef int lame_set_mode_t(lame_global_flags *, MPEG_mode);
typedef MPEG_mode lame_get_mode_t(const lame_global_flags *);


/* --------------------------------------------------------------------------*/

class AudacityProject;
class MixerSpec;

class MP3Exporter {

   public:
   
      MP3Exporter();
      virtual ~MP3Exporter() { };

      bool FindLibrary(wxWindow *parent);
      bool LoadLibrary();
      bool ValidLibraryLoaded();
      wxString GetLibraryVersion();

      /* returns the number of samples PER CHANNEL to send for each call to EncodeBuffer */
      int InitializeStream(int channels, int sampleRate);
      /* In bytes. must be called AFTER InitializeStream */
      int GetOutBufferSize();
      /* returns the number of bytes written. input is interleaved if stereo*/
      int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]);
      int EncodeRemainder(short int inbuffer[], int nSamples,
                          unsigned char outbuffer[]);

      int EncodeBufferMono(short int inbuffer[], unsigned char outbuffer[]);
      int EncodeRemainderMono(short int inbuffer[], int nSamples,
                              unsigned char outbuffer[]);

      int FinishStream(unsigned char outbuffer[]);
      void CancelEncoding();

      /* The number of different quality settings */
      int GetQualityVariance();
      
      /* These global settings keep state over the life of the object */
      int GetConfigurationCaps();
      void SetBitrate(int rate);
      int GetBitrate();
      void SetQuality(int quality);
      int GetQuality();
      void SetVBRQuality(int quality);
      int GetVBRQuality();
      void SetMode(MPEG_mode mode);
      MPEG_mode GetMode();

   protected:

      wxString mLibPath;
      wxDynamicLibrary lame_enc_lib;

   private:

      /* function pointers to the symbols we get from the library */
      lame_init_t* lame_init;
      lame_init_params_t* lame_init_params;
      lame_encode_buffer_t* lame_encode_buffer;
      lame_encode_buffer_interleaved_t* lame_encode_buffer_interleaved;
      lame_encode_flush_t* lame_encode_flush;
      lame_close_t* lame_close;
      get_lame_version_t* get_lame_version;
      
      lame_set_in_samplerate_t* lame_set_in_samplerate;
      lame_set_out_samplerate_t* lame_set_out_samplerate;
      lame_set_num_channels_t* lame_set_num_channels;
      lame_set_quality_t* lame_set_quality;
      lame_get_quality_t* lame_get_quality;
      lame_set_brate_t* lame_set_brate;
      lame_get_brate_t* lame_get_brate;
      lame_set_VBR_t* lame_set_VBR;
      lame_get_VBR_t* lame_get_VBR;
      lame_set_VBR_q_t* lame_set_VBR_q;
      lame_get_VBR_q_t* lame_get_VBR_q;
      lame_set_mode_t* lame_set_mode;
      lame_get_mode_t* lame_get_mode;

      lame_global_flags *mGF;
      
      bool mLibraryLoaded, mEncoding;
      char mVersion[20];

      static const int mSamplesPerChunk = 220500;
      static const int mOutBufferSize = int(1.25 * mSamplesPerChunk + 7200);

};

#define MP3CONFIG_BITRATE 0x00000001
#define MP3CONFIG_QUALITY 0x00000002

MP3Exporter *GetMP3Exporter();
void         ReleaseMP3Exporter();
        
bool ExportMP3(AudacityProject *project,
               bool stereo, wxString fName,
               bool selectionOnly, double t0, double t1, 
               MixerSpec *mixerSpec = NULL);

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 697b9941-3e7e-44c1-929e-19d34ed70151

