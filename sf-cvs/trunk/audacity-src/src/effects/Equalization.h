/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectEqualization.h

  Mitch Golden
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_EQUALIZATION__
#define __AUDACITY_EFFECT_EQUALIZATION__

#include <wx/button.h>
#include <wx/panel.h>
#include <wx/dialog.h>
#include <wx/dynarray.h>
#include <wx/intl.h>
#include <wx/stattext.h>
#include <wx/slider.h>

// Declare window functions

class wxString;
class wxBoxSizer;
class wxChoice;

#include "Effect.h"
#include "../xml/XMLTagHandler.h"

class Envelope;
class WaveTrack;
class EqualizationDialog;

//
// One point in a curve
//
class EQPoint
{
public:
   EQPoint( const double f, const double d ) { Freq = f; dB = d; }
   double Freq;
   double dB;
};
WX_DECLARE_OBJARRAY( EQPoint, EQPointArray);

//
// One curve in a list
//
// LLL:  This "really" isn't needed as the EQPointArray could be
//       attached as wxClientData to the wxChoice entries.  I
//       didn't realize this until after the fact and am too
//       lazy to change it.  (But, hollar if you want me to.)
//
class EQCurve
{
public:
   EQCurve( const wxString & name ) { Name = name; }
   EQCurve( const wxChar * name ) { Name = name; }
   wxString Name;
   EQPointArray points;
};
WX_DECLARE_OBJARRAY( EQCurve, EQCurveArray );

class EffectEqualization: public Effect {
   
public:
   
   EffectEqualization();
   virtual ~EffectEqualization();
   
   virtual wxString GetEffectName() {
      return wxString(_("Equalization..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Performing Equalization"));
   }
   
   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );
   
   virtual bool Process();

   // Number of samples in an FFT window
   enum {windowSize=16384};	//MJS - work out the optimum for this at run time?  Have a dialog box for it?

   // Low frequency of the FFT.  20Hz is the
   // low range of human hearing
   enum {loFreqI=20};
   
private:
   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);

   void Filter(sampleCount len,
               float *buffer);
   
   float *mFilterFuncR;
   float *mFilterFuncI;
   int mM;	//MJS

public:
   enum curveType {
     flat, amradio, acoustic,
     nab, lp, aes, deccaffrrmicro, deccaffrr78, riaa,
     col78, deccaffrrlp, emi78, rcavictor1938, rcavictor1947,
     custom,
     nCurveTypes
   };

   enum {nCurvePoints=28};
   static const float curvex[];
   static const float curvey[][nCurvePoints];
   static const wxChar * curveNames[];

friend class EqualizationDialog;
friend class EqualizationPanel;
};


class EqualizationPanel: public wxPanel
{
public:
   EqualizationPanel( double loFreq, double hiFreq,
		      Envelope *env,
		      EqualizationDialog *parent,
            float *filterFuncR, float *filterFuncI, long windowSize, int *M, wxWindowID id, 
		      const wxPoint& pos = wxDefaultPosition,
		      const wxSize& size = wxDefaultSize);
   ~EqualizationPanel();

   void OnMouseEvent(wxMouseEvent & event);
   void OnPaint(wxPaintEvent & event);
   void OnSize (wxSizeEvent & event);

private:

   wxBitmap *mBitmap;
   wxRect mEnvRect;
   EqualizationDialog *mParent;
   int mWidth;
   int mHeight;
   long mWindowSize;
   float *mFilterFuncR;
   float *mFilterFuncI;
   int *mM;	//MJS

   double mLoFreq;
   double mHiFreq;

   Envelope *mEnvelope;

   DECLARE_EVENT_TABLE()
};


// WDR: class declarations

//----------------------------------------------------------------------------
// EqualizationDialog
//----------------------------------------------------------------------------

class EqualizationDialog: public wxDialog, public XMLTagHandler
{
public:
   // constructors and destructors
   EqualizationDialog(EffectEqualization * effect,
								double loFreq, double hiFreq,
								float *filterFuncR, float *filterFuncI, long windowSize, int *mM,
								wxWindow *parent, wxWindowID id,
								const wxString &title,
								const wxPoint& pos = wxDefaultPosition,
								const wxSize& size = wxDefaultSize,
								long style = wxDEFAULT_DIALOG_STYLE );
   
   // WDR: method declarations for EqualizationDialog
   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

   void EnvelopeUpdated();

private:
   void MakeEqualizationDialog();
   void CreateChoice();
   void LoadDefaultCurves();
   void LoadCurves();
   void SaveCurves();
   void Select(int sel);
   void setCurve(Envelope *env, int currentCurve);

   // XMLTagHandler callback methods for loading and saving
   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   XMLTagHandler *HandleXMLChild(const wxChar *tag);
   void WriteXML(int depth, FILE *fp);

private:
   // WDR: member variable declarations for EqualizationDialog

   enum
   {
      ID_FILTERPANEL = 10000,
      ID_LENGTH,
      ID_CURVE,
      ID_SAVEAS,
      ID_DELETE,
      ID_CLEAR,
      ID_PREVIEW
   };
   
private:
   // WDR: handler declarations for EqualizationDialog
   void OnSize( wxSizeEvent &event );
   void OnSlider( wxCommandEvent &event );
   void OnCurve( wxCommandEvent &event );
   void OnSaveAs( wxCommandEvent &event );
   void OnDelete( wxCommandEvent &event );
   void OnClear( wxCommandEvent &event );
   void OnPreview(wxCommandEvent &event);
   void OnOk( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
   
private:
	EffectEqualization * m_pEffect;

   double mLoFreq;
   double mHiFreq;
   float *mFilterFuncR;
   float *mFilterFuncI;
   long mWindowSize;
   int *mM;	//MJS
   bool mDirty;

   EqualizationPanel *mPanel;
   Envelope *mEnvelope;
   wxBoxSizer *mCurveSizer;
   wxChoice *mCurve;
   wxButton *mDelete;
   wxButton *mSaveAs;
   wxStaticText *mMText;
   wxSlider *mMSlider;

   EQCurveArray mCurves;

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
// arch-tag: 309f263d-748c-4dc0-9e68-9e86732890bb

