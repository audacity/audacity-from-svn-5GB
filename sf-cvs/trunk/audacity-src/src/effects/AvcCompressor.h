/**********************************************************************

  Audacity: A Digital Audio Editor

  AvcCompressor.h

  Vincent A. Busam

**********************************************************************/

#ifndef __AUDACITY_EFFECT_AVCCOMPRESSOR__
#define __AUDACITY_EFFECT_AVCCOMPRESSOR__

#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/statbox.h>
#include <wx/valtext.h>
#include <wx/checkbox.h>
#include <wx/intl.h>


class wxString;

class iAVC;

#include <wx/intl.h>
#include "SimplePairedTwoTrack.h"

#include "../../lib-src/iAVC/iAVC.h"	// for MULTIPLY_PCT_ARRAY_SIZE

class WaveTrack;

class EffectAvcCompressor: public EffectSimplePairedTwoTrackInt16 {
   
public:
   
   EffectAvcCompressor();
   
   virtual wxString GetEffectName() {
      return wxString(_("Automatic Volume Control..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Changing volume"));
   }
   
   
protected:

   virtual bool PromptUser();		// invoked by Effect
   virtual bool Init();				// invoked by Effect

   // invoked by SimplePairedTwoTrack
   virtual bool ProcessSimplePairedTwoTrack(short int *bufferLeft, 
											short int *bufferRight, // may be 0
											sampleCount len);
	AutoVolCtrl  mAutoVolCtrl;	// iAVC class (LGPL license)
	long  mnChangeWindow;
};

// WDR: class declarations

//----------------------------------------------------------------------------
// NoiseRemovalDialog
//----------------------------------------------------------------------------

// Declare window functions

// If you change the following number you may need to change the EVENT_TABLE
#define NUM_CURVE_POINTS    7    // includes (0,0), (32768,32768) points

#define ID_TEXT 10000
#define ID_ADJWINTEXT 10001
#define ID_DELAYTEXT 10002
#define ID_CHANGEWINTEXT 10003
#define ID_MINPCTTEXT 10004

#define ID_RESTORE_DEFAULTS 10005

//following IDs need to be spaced out at least as much as NUM_CURVE_POINTS
#define ID_FIRST_CURVE_CHECK  10100
#define ID_FIRST_CURVE_X      10200
#define ID_FIRST_CURVE_Y      10300

class AvcCompressorDialog: public wxDialog
{
public:
   // constructors and destructors
   AvcCompressorDialog( wxWindow *parent, wxWindowID id, const wxString &title,
                       const wxPoint& pos = wxDefaultPosition,
                       const wxSize& size = wxDefaultSize,
                       long style = wxDEFAULT_DIALOG_STYLE );
   ~AvcCompressorDialog();

	long GetAdjusterWindow() { return mnAdjWin; };
	long GetDelay()          { return mnDelay; };
	long GetChangeWindow()   { return mnChangeWin; };
	long GetMinimumPercent() { return mnMinPct; };
	void GetTransformArray( unsigned short int nTransform[MULTIPLY_PCT_ARRAY_SIZE] );
   
   //wxButton *mRemoveNoiseButton;
   //wxSlider *mSlider;
      
private:
   DECLARE_EVENT_TABLE()

protected:
	wxSizer *MakeAvcCompressorDialog( wxWindow *parent, bool call_fit = TRUE,
									bool set_sizer = TRUE );
    void OnCancel( wxCommandEvent &event );
    void OnOK(wxCommandEvent &event);
	void OnRestoreDefaults(wxCommandEvent &event);
	void OnCheckBox(wxCommandEvent & event);

	bool LongRangeCheck (  wxWindow *window,
						   const long nValue,
						   const long nMin,
						   const long nMax );

	// Values for Adjustment Settings
	wxTextCtrl *mctlAdjWin;
	wxTextCtrl *mctlDelay;
	wxTextCtrl *mctlChangeWin;
	wxTextCtrl *mctlMinPct;

	wxString mstrAdjWin;
	wxString mstrDelay;
	wxString mstrChangeWin;
	wxString mstrMinPct;

	long mnAdjWin;
	long mnDelay;
	long mnChangeWin;
	long mnMinPct;

	// Values for Amplification Settings
	wxCheckBox *mctlCheckBoxes[NUM_CURVE_POINTS];
	wxTextCtrl *mctlXAxis[NUM_CURVE_POINTS];
	wxTextCtrl *mctlYAxis[NUM_CURVE_POINTS];

	wxString mstrXAxis[NUM_CURVE_POINTS];
	wxString mstrYAxis[NUM_CURVE_POINTS];

	long mnXAxis[NUM_CURVE_POINTS];	
	long mnYAxis[NUM_CURVE_POINTS];
};

#endif

