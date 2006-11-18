/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORT__
#define __AUDACITY_EXPORT__

#include <wx/panel.h>
#include <wx/stattext.h>

class AudacityProject;
class DirManager;
class WaveTrack;
class TrackList;
class MixerSpec;
class wxMemoryDC;

bool ExportPCM(AudacityProject *project,
               bool selectionOnly, double t0, double t1);

bool ExportCompressed(AudacityProject *project, const wxString& format,
                      bool selectionOnly, double t0, double t1);

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

class ExportMixerDialog: public wxDialog
{
public:
   // constructors and destructors
   ExportMixerDialog( TrackList * tracks, bool selectionOnly, int maxNumChannels,
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

