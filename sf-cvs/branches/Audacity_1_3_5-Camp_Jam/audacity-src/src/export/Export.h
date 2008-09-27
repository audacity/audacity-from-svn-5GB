/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORT__
#define __AUDACITY_EXPORT__

#include <wx/dialog.h>
#include <wx/dynarray.h>
#include <wx/filename.h>
#include <wx/panel.h>
#include "../Tags.h"

class wxMemoryDC;
class wxStaticText;
class AudacityProject;
class DirManager;
class WaveTrack;
class TrackList;
class MixerSpec;
class FileDialog;

//----------------------------------------------------------------------------
// ExportPlugin
//----------------------------------------------------------------------------
class ExportPlugin
{
public:

   ExportPlugin();
   virtual ~ExportPlugin();
   virtual void Destroy();

   void SetFormat(const wxString & format);
   void SetDescription(const wxString & description);
   void SetExtension(const wxString & extension);
   void SetExtensions(const wxArrayString & extensions);
   void SetMask(const wxString & mask);
   void SetMaxChannels(int maxchannels);
   void SetCanMetaData(bool canmetadata);
   
   virtual wxString GetFormat();
   virtual wxString GetDescription();
   virtual wxString GetExtension();
   virtual wxArrayString GetExtensions();
   virtual wxString GetMask();
   virtual int GetMaxChannels();
   virtual bool GetCanMetaData();
   
   virtual bool IsExtension(wxString & ext);

   virtual bool DisplayOptions(AudacityProject *project = NULL);
   virtual bool DoDisplayOptions(AudacityProject *project);

   /** \brief called to export audio into a file.
    *
    * @param metadata A Tags object that will over-ride the one in *project and
    * be used to tag the file that is exported.
    */
   virtual bool Export(AudacityProject *project,
                       int channels,
                       wxString fName,
                       bool selectedOnly,
                       double t0,
                       double t1,
                       MixerSpec *mixerSpec = NULL,
                       Tags *metadata = NULL);

   virtual bool DoExport(AudacityProject *project,
                         int channels,
                         wxString fName,
                         bool selectedOnly,
                         double t0,
                         double t1,
                         MixerSpec *mixerSpec);

private:

   wxString mFormat;
   wxString mDescription;
   wxString mExtension;
   wxArrayString mExtensions;
   wxString mMask;
   int mMaxChannels;
   bool mCanMetaData;
};

WX_DECLARE_OBJARRAY(ExportPlugin *, ExportPluginArray);

//----------------------------------------------------------------------------
// Exporter
//----------------------------------------------------------------------------
class Exporter
{
public:

   Exporter();
   virtual ~Exporter();

   void RegisterPlugin(ExportPlugin *plugin);

   bool Process(AudacityProject *project, bool selectedOnly,
                double t0, double t1);
   bool Process(AudacityProject *project, int numChannels,
                const wxChar *type, const wxString filename,
                bool selectedOnly, double t0, double t1);

   void DisplayOptions(int index);

   const ExportPluginArray GetPlugins();

private:

   bool ExamineTracks();
   bool GetFilename();
   bool CheckMix();
   bool ExportTracks();

private:

   AudacityProject *mProject;
   MixerSpec *mMixerSpec;

   ExportPluginArray mPlugins;

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
class ExportMixerDialog : public wxDialog
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
