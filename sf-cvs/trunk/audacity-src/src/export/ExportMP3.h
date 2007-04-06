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

class AudacityProject;
class MixerSpec;

class MP3Exporter
{
public:

   enum
   {
      MP3_MODE_STEREO = 0,
      MP3_MODE_JOINT,
      MP3_MODE_DUAL,
      MP3_MODE_MONO
   } MP3Modes;

   MP3Exporter();
   virtual ~MP3Exporter();

   bool FindLibrary(wxWindow *parent, bool showdialog);
   bool LoadLibrary(wxWindow *parent, bool askuser);
   bool ValidLibraryLoaded();

   /* These global settings keep state over the life of the object */
   void SetBitrate(int rate);
   int GetBitrate();
   void SetVBRQuality(int quality);
   int GetVBRQuality();
   void SetMode(int mode);
   int GetMode();

   /* Virtual methods that must be supplied by library interfaces */

   /* initialize the library interface */
   virtual bool InitLibrary(wxString libpath) = 0;
   virtual void FreeLibrary() = 0;

   /* get library info */
   virtual wxString GetLibraryVersion() = 0;
   virtual wxString GetLibraryName() = 0;
   virtual wxString GetLibraryPath() = 0;
   virtual wxString GetLibraryTypeString() = 0;

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

protected:

   wxString mLibPath;

   bool mLibraryLoaded;
   bool mEncoding;

   int mBitrate;
   bool mVBR;
   int mVBRQuality;
   int mMode;
};

#define MP3CONFIG_BITRATE 0x00000001
#define MP3CONFIG_QUALITY 0x00000002

MP3Exporter *GetMP3Exporter();
void         ReleaseMP3Exporter();

bool ExportMP3(AudacityProject *project,
               int channels, wxString fName,
               bool selectionOnly, double t0, double t1, 
               MixerSpec *mixerSpec = NULL);

bool ExportMP3Options(AudacityProject *project);

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

