/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP3.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORTMP3__
#define __AUDACITY_EXPORTMP3__

#include <wx/string.h>

class MP3Exporter {
   public:
      
      virtual bool LoadLibrary(wxString fileName = "") = 0;
      virtual bool ValidLibraryLoaded() = 0;
      virtual const char *GetLibraryVersion() = 0;

      /* returns the number of samples to send for each call to EncodeBuffer */
      virtual int InitializeStream(int channels, int sampleRate) = 0;
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
      
      /* These global settings keep state over the life of the object */
      virtual void SetBitrate(int rate) = 0;
      virtual int GetBitrate() = 0;
      virtual void SetQuality(int quality) = 0;
      virtual int GetQuality() = 0;
      virtual ~MP3Exporter() { };
};

extern MP3Exporter *gMP3Exporter;
        
bool ExportMP3(bool stereo, double rate, wxString fName, wxWindow * parent,
               TrackList * tracks, bool selectionOnly, double t0,
               double t1);

#endif
