/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP3.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORTMP3__
#define __AUDACITY_EXPORTMP3__

#include <wx/string.h>
#include <wx/dynlib.h>

class AudacityProject;
class MixerSpec;

class MP3Exporter {
   public:
   
      MP3Exporter();
   
      virtual wxString GetLibraryPath() = 0;
      virtual wxString GetLibraryName() = 0;
      virtual wxString GetLibraryMessage() = 0;
      virtual wxString GetLibraryTypeString() = 0;

      virtual bool FindLibrary(wxWindow *parent);
      virtual bool LoadLibrary() = 0;
      virtual bool ValidLibraryLoaded() = 0;
      virtual wxString GetLibraryVersion() = 0;

      /* returns the number of samples PER CHANNEL to send for each call to EncodeBuffer */
      virtual int InitializeStream(int channels, int sampleRate) = 0;
      /* In bytes. must be called AFTER InitializeStream */
      virtual int GetOutBufferSize() = 0;
      /* returns the number of bytes written. input is interleaved if stereo*/
      virtual int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]) = 0;
      virtual int EncodeRemainder(short int inbuffer[], int nSamples,
                               unsigned char outbuffer[]) = 0;

      virtual int EncodeBufferMono(short int inbuffer[], unsigned char outbuffer[]) = 0;
      virtual int EncodeRemainderMono(short int inbuffer[], int nSamples,
                               unsigned char outbuffer[]) = 0;

      virtual int FinishStream(unsigned char outbuffer[]) = 0;
      virtual void CancelEncoding() = 0;

      /* The number of different quality settings */
      virtual int GetQualityVariance() = 0;
      
      /* These global settings keep state over the life of the object */
      virtual int GetConfigurationCaps() = 0;
      virtual void SetBitrate(int rate) = 0;
      virtual int GetBitrate() = 0;
      virtual void SetQuality(int quality) = 0;
      virtual int GetQuality() = 0;
      virtual ~MP3Exporter() { };

  protected:
   wxString mLibPath;
   wxDynamicLibrary lame_enc_lib;
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

