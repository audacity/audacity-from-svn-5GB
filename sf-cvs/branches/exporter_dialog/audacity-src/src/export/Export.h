/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORT__
#define __AUDACITY_EXPORT__

class AudacityProject;
class DirManager;
class WaveTrack;
class wxChoice;
class wxCheckBox;
class wxFrame;
class wxRadioButton;
class wxSlider;

bool Export(AudacityProject *project,
            bool selectionOnly, double t0, double t1);

bool ExportLossy(AudacityProject *project,
                 bool selectionOnly, double t0, double t1);


class  FormatSelectionDialog: public wxDialog
{
  enum {
    FSD_FORMAT_CHOOSER,
    FSD_SECONDARY_CHOOSER,
    FSD_CHANNEL_CHOOSER,
    FSD_LOSSY_BITRATE,
    FSD_SAMPLE_RATE_CHOOSER,
    FSD_OK,
    FSD_CANCEL
  };
  

  enum FormatType {
    FORMAT_TYPE_SF_WAV          = 0x010000,             /* LibSndFile: Microsoft WAV format (little endian). */
    FORMAT_TYPE_SF_AIFF         = 0x020000,             /* LibSndFile: Apple/SGI AIFF format (big endian). */
    FORMAT_TYPE_SF_AU           = 0x030000,             /* LibSndFile: Sun/NeXT AU format (big endian). */
    FORMAT_TYPE_SF_RAW          = 0x040000,             /* LibSndFile: RAW PCM data. */
    FORMAT_TYPE_SF_PAF          = 0x050000,             /* LibSndFile: Ensoniq PARIS file format. */
    FORMAT_TYPE_SF_SVX          = 0x060000,             /* LibSndFile: Amiga IFF / SVX8 / SV16 format. */
    FORMAT_TYPE_SF_NIST         = 0x070000,             /* LibSndFile: Sphere NIST format. */
    FORMAT_TYPE_SF_VOC          = 0x080000,             /* LibSndFile: VOC files. */
    FORMAT_TYPE_SF_IRCAM        = 0x0A0000,             /* LibSndFile: Berkeley/IRCAM/CARL */
    FORMAT_TYPE_SF_W64          = 0x0B0000,             /* LibSndFile: Sonic Foundry's 64 bit RIFF/WAV */
    FORMAT_TYPE_SF_MAT4         = 0x0C0000,             /* LibSndFile: Matlab (tm) V4.2 / GNU Octave 2.0 */
    FORMAT_TYPE_SF_MAT5         = 0x0D0000,             /* LibSndFile: Matlab (tm) V5.0 / GNU Octave 2.1 */
    FORMAT_TYPE_OGG,                                    /* Ogg Vorbis encoder*/
    FORMAT_TYPE_MP3,                                    /* Lame MP3 encoder*/
    FORMAT_TYPE_FLAC,                                   /* FLAC Lossless encoder (integer-only?)*/
    FORMAT_TYPE_LABELS,                                 /* Export text labels*/
    FORMAT_TYPE_COMMANDLINE,                            /* Command-line format*/
    FORMAT_TYPE_UNKNOWN                                 /* Don't know what is selected*/
  };
  
 public:
  FormatSelectionDialog(AudacityProject * project, bool selection,double t0, double t1);
  virtual ~FormatSelectionDialog();
  DECLARE_EVENT_TABLE()
 private:
 


  void OnSelectFormat(wxCommandEvent & event);  //The
  void OnLossySlider(wxCommandEvent & event);   //A slider used in lossy formats to adjust compression
  void OnRate(wxCommandEvent & event);          //Allows the exported sample rate to be changed.
  void OnOptions(wxCommandEvent & event);      //LibSampleFile (or other) secondary encoding options selector.
  void OnCancel(wxCommandEvent & event);   //The close button
  void OnOK(wxCommandEvent & event);       //The OK button
  void OnResize(wxCommandEvent & event);
  void OnChannelsChange(wxCommandEvent & event);

  
  wxChoice       * mFormatChooser;
  wxChoice       * mOptionChooser;                   //Chooses secondary options for libsoundfile formats
  wxChoice       * mChannelChooser;
  wxChoice       * mSampleRates;  
  wxTextCtrl     * mOtherSampleRate;
  wxSlider       * mQualitySlider;
  wxButton       * mOK;
  wxButton       * mCancel;
  wxBoxSizer     * mainSizer;
  wxBoxSizer     * mSecondaryOptionSizer;
 

  AudacityProject * mProject;               //The associated project to export.
  FormatType  mCurrentFormatChoice;         //Determine what basic export format.
  int mNumAvailableFormats;                  //The number of formats that are currently available, ignoring the LSF format panoply
  wxString  mFileName;
  int m_sf_Format;                          //Keeps track of selected libsndfile format
  int m_sf_Encoding;                        //Keeps track of selected libsndfile Encoding
  int m_sf_Endianness;                      //Keeps track of selected libsndfile endianness
  
  FormatType* mFormats;                           //An array that keeps track of the 
  int * mEncodings;                         ///An array that keeps track of the encoding formats for the 
                                            ///currently-selected format.
  bool mSelectionOnly;                      //Whether only the selected tracks should be exported.
  double m_t0, m_t1;                        //Left and right edge of selection in seconds.
  int mNumExportedChannels;                 //The number of channels that the file will be exported to.
  int mNumTotalChannels;                    //The total number of channels 
  int mRate;                          //Selected Sample Rate.
  int mBitRate;
};

#endif
