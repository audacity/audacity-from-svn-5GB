/**********************************************************************

  Audacity: A Digital Audio Editor

  MP3Exporter.h

   (c) Joshua Haberman and The Audacity Team
 

  This just acts as an interface to LAME. A Lame dynamic library must
  be present

  The difficulty in our approach is that we are attempting to use LAME
  in a way it was not designed to be used. LAME's API is reasonably
  consistant, so if we were linking directly against it we could expect
  this code to work with a variety of different LAME versions. However,
  the data structures change from version to version, and so linking
  with one version of the header and dynamically linking against a
  different version of the dynamic library will not work correctly.

  The solution is to find the lowest common denominator between versions.
  The bare minimum of functionality we must use is this:
      1. Initialize the library.
      2. Set, at minimum, the following global options:
          i.  input sample rate
          ii. input channels
      3. Encode the stream
      4. Call the finishing routine

  Just so that it's clear that we're NOT free to use whatever features
  of LAME we like, I'm not including lame.h, but instead enumerating
  here the extent of functions and structures that we can rely on being
  able to import and use from a dynamic library.

  For the record, we aim to support LAME 3.70 on. Since LAME 3.70 was
  released in April of 2000, that should be plenty.


                          -------------------


 MP3Exporter is a child class of Exporter, which is a generic class
 handling exporting of all file types.  Each specific platform creates 
 an inherited PlatformMP3Exporter, which is created directly by the export
 dialog. Only public methods ::Export(), ::Verify() and the constructor
 should be used from outside.



**********************************************************************/

#ifndef __AUDACITY_MP3EXPORTER__
#define __AUDACITY_MP3EXPORTER__

///This class contains routines to export the selected audio into
/// a .mp3 file using the lame encoder.


#include "Exporter.h"
#include <wx/string.h>
#include <wx/dynlib.h>
#include "../SampleFormat.h"

class wxString;
class AudacityProject;
class DirManager;
class WaveTrack;
      


class MP3Exporter: public Exporter
{
 public:
  MP3Exporter(AudacityProject * project, double t0, double t1,
	      bool exportSelection, int outrate, int channels);
  
  virtual ~MP3Exporter();
  
  virtual bool Export(const wxString &filename);
  virtual bool Verify();
 
  virtual int GetConfigurationCaps() = 0;
  virtual bool ValidLibraryLoaded() = 0;
  virtual wxString GetLibraryVersion() = 0;
  virtual bool LoadLibrary() = 0;
  virtual bool FindLibrary(wxWindow *parent);

  
 protected:
 
 
  virtual void SetBitRate(int rate) = 0;
  virtual int GetBitRate() = 0;
  virtual void SetQuality(int quality) = 0;
  virtual int GetQuality() = 0;

 
  virtual wxString GetLibraryPath() = 0;
  virtual wxString GetLibraryName() = 0;
  virtual wxString GetLibraryMessage() = 0;
  virtual wxString GetLibraryTypeString() = 0;
  



  
  /* returns the number of samples PER CHANNEL to send for each call to EncodeBuffer */
  virtual int InitializeStream() = 0;
  /* In bytes. must be called AFTER InitializeStream */
  virtual int GetOutBufferSize() = 0;
  /* returns the number of bytes written. input is interleaved if stereo*/
  virtual int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]) = 0;
  virtual int EncodeRemainder(short int inbuffer[], int nSamples,
			      unsigned char outbuffer[]) = 0;
  virtual int FinishStream(unsigned char outbuffer[]) = 0;
  virtual void CancelEncoding() = 0;
  
  /* The number of different quality settings */
  virtual int GetQualityVariance() = 0;
  
 
  
  wxString mLibPath;
  wxDynamicLibrary lame_enc_lib;

};

#define MP3CONFIG_BITRATE 0x00000001
#define MP3CONFIG_QUALITY 0x00000002




/* --------------------------------------------------------------------------*/
//The following code is compiled only on WXGTK (linux etc.)

#ifdef __WXGTK__
   struct lame_global_flags;
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
   typedef int lame_set_num_channels_t(lame_global_flags*, int );
   typedef int lame_set_quality_t(lame_global_flags*, int);
   typedef int lame_get_quality_t(lame_global_flags*);
   typedef int lame_set_brate_t(lame_global_flags*, int);
   typedef int lame_get_brate_t(lame_global_flags*);
   

   /* --------------------------------------------------------------------------*/

class PlatformMP3Exporter : public MP3Exporter {
   public:
  
  PlatformMP3Exporter(AudacityProject * project, double t0, double t1,
		      bool exportSelection, int rate, int channels,
		      int bitrate, int quality);
  virtual ~PlatformMP3Exporter();

  virtual int GetConfigurationCaps();
  virtual bool ValidLibraryLoaded();
  virtual wxString GetLibraryVersion();
  virtual bool LoadLibrary();
  
   protected:


  void SetBitRate(int rate);
  int GetBitRate();
  void SetQuality(int quality);
  int GetQuality();
  

  wxString GetLibraryPath(); 
  wxString GetLibraryName();
  wxString GetLibraryTypeString();
  wxString GetLibraryMessage();
  int InitializeStream();
  int GetOutBufferSize();
  int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]);
  int EncodeRemainder(short int inbuffer[], int nSamples, unsigned char outbuffer[]);
  int FinishStream(unsigned char outbuffer[]);
  void CancelEncoding();
  int GetQualityVariance();

   private:
  /* function pointers to the symbols we get from the library */
  lame_init_t* lame_init;
  lame_init_params_t* lame_init_params;
  lame_encode_buffer_interleaved_t* lame_encode_buffer_interleaved;
  lame_encode_flush_t* lame_encode_flush;
  lame_close_t* lame_close;
  get_lame_version_t* get_lame_version;
  
  lame_set_in_samplerate_t* lame_set_in_samplerate;
  lame_set_num_channels_t* lame_set_num_channels;
  lame_set_quality_t* lame_set_quality;
  lame_get_quality_t* lame_get_quality;
  lame_set_brate_t* lame_set_brate;
  lame_get_brate_t* lame_get_brate;
  
  lame_global_flags *mGF;
  
  bool mLibraryLoaded, mEncoding;
  char mVersion[20];
  
  static const int mSamplesPerChunk = 220500;
  static const int mOutBufferSize = int(1.25 * mSamplesPerChunk + 7200);
  
  
};


/* --------------------------------------------------------------------------*/
/* --------------------------------------------------------------------------*/
//The following code is compiled only on Mac OSX 


#elif defined(__MACOSX__)

   struct lame_global_flags;
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
   typedef int lame_set_num_channels_t(lame_global_flags*, int );
   typedef int lame_set_quality_t(lame_global_flags*, int);
   typedef int lame_get_quality_t(lame_global_flags*);
   typedef int lame_set_brate_t(lame_global_flags*, int);
   typedef int lame_get_brate_t(lame_global_flags*);
   

   /* --------------------------------------------------------------------------*/

class PlatformMP3Exporter : public MP3Exporter {
   public:
  
  PlatformMP3Exporter(AudacityProject * project, double t0, double t1,
		      bool exportSelection, int outrate,
		      int channels, int bitrate, int quality);
  virtual ~PlatformMP3Exporter();

  virtual int GetConfigurationCaps();
  virtual bool ValidLibraryLoaded();
  virtual wxString GetLibraryVersion();
  virtual bool LoadLibrary();

 
  protected:


  void SetBitRate(int rate);
  int GetBitRate();
  void SetQuality(int quality);
  int GetQuality(); 


  wxString GetLibraryPath();
  int InitializeStream();
  int GetOutBufferSize();
  int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]);
  int EncodeRemainder(short int inbuffer[], int nSamples, unsigned char outbuffer[]);
  int FinishStream(unsigned char outbuffer[]);
  void CancelEncoding();
  int GetQualityVariance();
  wxString GetLibraryName();
  wxString GetLibraryTypeString();
  wxString GetLibraryMessage();




   private:
  /* function pointers to the symbols we get from the library */
  lame_init_t* lame_init;
  lame_init_params_t* lame_init_params;
  lame_encode_buffer_interleaved_t* lame_encode_buffer_interleaved;
  lame_encode_flush_t* lame_encode_flush;
  lame_close_t* lame_close;
  get_lame_version_t* get_lame_version;
  
  lame_set_in_samplerate_t* lame_set_in_samplerate;
  lame_set_num_channels_t* lame_set_num_channels;
  lame_set_quality_t* lame_set_quality;
  lame_get_quality_t* lame_get_quality;
  lame_set_brate_t* lame_set_brate;
  lame_get_brate_t* lame_get_brate;
  
  lame_global_flags *mGF;
  
  bool mLibraryLoaded, mEncoding;
  char mVersion[20];
  
  static const int mSamplesPerChunk = 220500;
  static const int mOutBufferSize = int(1.25 * mSamplesPerChunk + 7200);
  
  
  void MakePString(unsigned char *p, const char *c);
  
  // MachOFunctionPointerForCFMFunctionPointer(void *cfmfp)
  //
  // Borrowed from the Apple Sample Code file "CFM_MachO_CFM.c"
  // This function allocates a block of CFM glue code which contains
  // the instructions to call CFM routines
  
  void *NewMachOFromCFM(void *cfmfp);

  


};


/* --------------------------------------------------------------------------*/
/* --------------------------------------------------------------------------*/
/* --------------------------------------------------------------------------*/
//The following code is compiled only on classic MacOS 


#elif defined(__WXMAC__)

   /* --------------------------------------------------------------------------*/

   /* The following code is intended to work with LAMELib, roughly LAME
      verson 3.87, as distributed by N2MP3. */

   typedef struct {
      /* input file description */
      unsigned long num_samples;  /* number of samples. default=2^32-1    */
      int num_channels;           /* input number of channels. default=2  */
      int in_samplerate;          /* input_samp_rate. default=44.1kHz     */
      int out_samplerate;         /* output_samp_rate. (usually determined automatically)   */ 
      float scale;                /* scale input by this amount */

      /* general control params */
      int gtkflag;                /* run frame analyzer?       */
      int bWriteVbrTag;           /* add Xing VBR tag?         */
      int disable_waveheader;     /* disable writing of .wav header, when *decoding* */
      int decode_only;            /* use lame/mpglib to convert mp3 to wav */
      int ogg;                    /* encode to Vorbis .ogg file */

      int quality;                /* quality setting 0=best,  9=worst  */
      int silent;                 /* disable some status output */
      float update_interval;      /* to use Frank's time status display */
      int brhist_disp;            /* enable VBR bitrate histogram display */
      int mode;                       /* 0,1,2,3 stereo,jstereo,dual channel,mono */
      int mode_fixed;                 /* use specified the mode, do not use lame's opinion of the best mode */
      int force_ms;                   /* force M/S mode.  requires mode=1 */
      int brate;                      /* bitrate */
      float compression_ratio;          /* user specified compression ratio, instead of brate */
      int free_format;                /* use free format? */

      int space[10000];  /* to liberally accomadate for the real size of the struct */
   } lame_global_flags;

   /* All functions types are suffexed with _t because gcc won't let you have a
    * type and a variable of the same name */

   typedef void lame_init_t(lame_global_flags *);

   /* NOTE: Same deal with this one: >= 3.88 changes it to: const char
    * *get_lame_version(), but this time they don't even leave us a
    * compatibility version! aggh! */
   typedef void lame_version_t(lame_global_flags *, char *);
   typedef const char *get_lame_version_t();

   typedef void lame_init_params_t(lame_global_flags*);

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

   typedef int lame_encode_finish_t(
         lame_global_flags *gf,
         unsigned char*     mp3buf,
         int                size );

   /* --------------------------------------------------------------------------*/

class PlatformMP3Exporter : public MP3Exporter {

   public:
  PlatformMP3Exporter(AudacityProject * project, double t0, double t1,
		      bool exportSelection, int outrate,
		      int channels, int bitrate, int quality);

  virtual ~PlatformMP3Exporter();

  virtual int GetConfigurationCaps();
  virtual bool ValidLibraryLoaded();
  virtual wxString GetLibraryVersion();
  virtual bool LoadLibrary();


   protected:
  wxString GetLibraryPath();
  wxString GetLibraryName();         
  wxString GetLibraryTypeString();
  wxString GetLibraryMessage();

  int InitializeStream();
  int GetOutBufferSize();      
 
  int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]);
  int EncodeRemainder(short int inbuffer[], int nSamples, unsigned char outbuffer[]);
  int FinishStream(unsigned char outbuffer[]);
  void CancelEncoding();
  int GetQualityVariance();
  void SetBitRate(int rate);
  int GetBitRate();
  void SetQuality(int quality);
  int GetQuality();

   private:

  lame_init_t* lame_init;
  lame_version_t* lame_version;
  get_lame_version_t* get_lame_version;
  lame_init_params_t* lame_init_params;
  lame_encode_buffer_t* lame_encode_buffer;
  lame_encode_finish_t* lame_encode_finish;
  
  lame_global_flags *mGF;
  
  bool mLibraryLoaded, mEncoding;
  char mVersion[20];
  
  static const int mSamplesPerChunk = 220500;
  static const int mOutBufferSize = int(1.25 * mSamplesPerChunk + 7200);
  
  short int *mLeftBuffer;
  short int *mRightBuffer;
  
  

};



/* --------------------------------------------------------------------------*/
/* --------------------------------------------------------------------------*/
/* --------------------------------------------------------------------------*/
//The following code is compiled only on  Win32

//--------------------------------------------------------
#elif defined(__WXMSW__)

#include "BladeMP3EncDLL.h"

class PlatformMP3Exporter : public MP3Exporter {



public:
  PlatformMP3Exporter(AudacityProject * project, double t0, double t1,
		      bool exportSelection, int outrate, int channels
		      int bitrate, int quality);
  virtual ~PlatformMP3Exporter();

   protected:


  void SetBitRate(int rate);
  int GetBitRate();
  void SetQuality(int quality);
  int GetQuality();


  wxString GetLibraryPath();
  wxString GetLibraryName();   
  wxString GetLibraryTypeString();
  wxString GetLibraryMessage();
  bool  LoadLibrary();
  bool ValidLibraryLoaded();
  wxString GetLibraryVersion();
  int InitializeStream();
  int GetOutBufferSize();
  int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]);
  int EncodeRemainder(short int inbuffer[], int nSamples, unsigned char outbuffer[]);
  int FinishStream(unsigned char outbuffer[]);
  void CancelEncoding();
  int GetQualityVariance();
  int GetConfigurationCaps();
  
private:
   BE_CONFIG mConf;
   BE_VERSION mVersion;
   HBE_STREAM mStreamHandle;
   bool mLibraryLoaded, mEncoding,mStereo;
   unsigned long mOutBufferSize, mInSampleNum;
   int mDefaultRate;


   BEINITSTREAM beInitStream;
   BEENCODECHUNK beEncodeChunk;
   BEDEINITSTREAM beDeinitStream;
   BECLOSESTREAM beCloseStream;
   BEVERSION beVersion;


};


#endif  




#endif
