/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORT__
#define __AUDACITY_EXPORT__

#include <wx/dynarray.h>
#include <wx/filename.h>
#include <wx/panel.h>

class wxFileName;
class wxMemoryDC;
class wxStaticText;
class AudacityProject;
class DirManager;
class WaveTrack;
class TrackList;
class MixerSpec;
class FileDialog;

//----------------------------------------------------------------------------
// ExportType
//----------------------------------------------------------------------------
class ExportType;

WX_DECLARE_OBJARRAY(ExportType, ExportTypeArray);

// Prototype for the options dialog
typedef bool (*ExportOptions)(AudacityProject *project);

// Prototype for the actual export routine
typedef bool (*ExportRoutine)(AudacityProject *project,
                              int channels,
                              wxString fName,
                              bool selectedOnly,
                              double t0,
                              double t1,
                              MixerSpec *mixerSpec);

class ExportType
{
public:

   ExportType();

   void Set(ExportRoutine routine,
            ExportOptions options,
            wxString format,
            wxString extension,
            int maxchannels,
            bool canmetadata = false,
            wxString description = wxEmptyString);
   
   ExportRoutine GetRoutine();
   ExportOptions GetOptions();
   wxString GetFormat();
   wxString GetExtension();
   wxString GetDescription();
   wxString GetMask();
   int GetMaxChannels();
   bool GetCanMetaData();
   
   bool DisplayOptions(AudacityProject *project = NULL);
   bool Export(AudacityProject *project,
               int channels,
               wxString fName,
               bool selectedOnly,
               double t0,
               double t1,
               MixerSpec *mixerSpec = NULL);

private:

   ExportRoutine mRoutine;
   ExportOptions mOptions;
   wxString mFormat;
   wxString mExtension;
   wxString mDescription;
   int mMaxChannels;
   bool mCanMetaData;
};

//----------------------------------------------------------------------------
// Export
//----------------------------------------------------------------------------
class Export
{
public:

   Export();
   virtual ~Export();

   bool Process(AudacityProject *project, bool selectedOnly, double t0, double t1);

   void DisplayOptions(int index);

   static ExportTypeArray GetTypes();

private:

   bool ExamineTracks();
   bool GetFilename();
   bool CheckMix();
   bool ExportTracks();

private:

   AudacityProject *mProject;
   MixerSpec *mMixerSpec;

   ExportTypeArray mTypes;

   wxFileName mFilename;
   wxFileName mActualName;

   double mT0;
   double mT1;
   int mFormat;
   int mNumSelected;
   int mNumLeft;
   int mNumRight;
   int mNumMono;
   int mChannels;
   bool mSelectedOnly;
};

//----------------------------------------------------------------------------
// ExportMixerPanel
//----------------------------------------------------------------------------
class ExportMixerPanel: public wxPanel
{
public:
   ExportMixerPanel( MixerSpec *mixerSpec, wxArrayString trackNames, 
         wxWindow *parent, wxWindowID id, const wxPoint& pos = wxDefaultPosition,
         const wxSize& size = wxDefaultSize);
   virtual ~ExportMixerPanel();

   void OnMouseEvent(wxMouseEvent & event);
   void OnPaint(wxPaintEvent & event);

private:
   wxBitmap *mBitmap;
   wxRect mEnvRect;
   int mWidth;
   int mHeight;
   MixerSpec *mMixerSpec;
   wxRect *mChannelRects;
   wxRect *mTrackRects;
   int mSelectedTrack, mSelectedChannel;
   wxArrayString mTrackNames;
   int mBoxWidth, mChannelHeight, mTrackHeight;

   void SetFont( wxMemoryDC &memDC, wxString text, int width, int height ); 
   double Distance( wxPoint &a, wxPoint &b );
   bool IsOnLine( wxPoint p, wxPoint la, wxPoint lb );

   DECLARE_EVENT_TABLE()
};

//----------------------------------------------------------------------------
// ExportMixerDialog
//----------------------------------------------------------------------------
class ExportMixerDialog: public wxDialog
{
public:
   // constructors and destructors
   ExportMixerDialog( TrackList * tracks, bool selectedOnly, int maxNumChannels,
         wxWindow *parent, wxWindowID id, const wxString &title, 
         const wxPoint& pos = wxDefaultPosition,
         const wxSize& size = wxDefaultSize, 
         long style = wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER );
   virtual ~ExportMixerDialog();

   MixerSpec* GetMixerSpec() { return mMixerSpec; }

private:
   wxStaticText *mChannelsText;
   MixerSpec *mMixerSpec;
   wxArrayString mTrackNames;
   
private:
   void OnOk( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
   void OnSlider( wxCommandEvent &event );
   void OnSize( wxSizeEvent &event );

private:
   DECLARE_EVENT_TABLE()
};
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
// arch-tag: b8dfb802-198d-4917-86dd-f53f29421419

