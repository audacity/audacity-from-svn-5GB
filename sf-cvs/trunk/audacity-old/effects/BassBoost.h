/**********************************************************************

  Audacity: A Digital Audio Editor

  Bass Boost

  Nasca Octavian Paul   <paulnasca@email.ro> or <paulnasca@yahoo.com>

**********************************************************************/

#ifndef __AUDACITY_EFFECT_BASS_BOOST__
#define __AUDACITY_EFFECT_BASS_BOOST__

#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>

// Declare window functions

#define ID_TEXT 10000
#define ID_FREQ_TEXT 10001
#define ID_FREQ_SLIDER 10002
#define ID_BOOST_TEXT 10003
#define ID_BOOST_SLIDER 10004

#include "Effect.h"

class WaveTrack;

class EffectBassBoost: public Effect {

public:
  EffectBassBoost();

  virtual wxString GetEffectName() { return wxString("BassBoost..."); }

  virtual bool Begin(wxWindow *parent);
  virtual bool DoIt(WaveTrack *t,
            sampleCount start,
            sampleCount len);

private:
  float frequency, dB_boost;
};

// WDR: class declarations

//----------------------------------------------------------------------------
// BassBoostDialog
//----------------------------------------------------------------------------

wxSizer *MakeBassBoostDialog( wxPanel *parent, bool call_fit, bool set_sizer );

class BassBoostDialog: public wxDialog
{
public:
    // constructors and destructors
    BassBoostDialog( wxWindow *parent, wxWindowID id, const wxString &title,
        const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxDefaultSize,
        long style = wxDEFAULT_DIALOG_STYLE );
    
    // WDR: method declarations for BassBoostDialog
    wxSlider* GetBoostSlider()  { return (wxSlider*) FindWindow( ID_BOOST_SLIDER ); }
    wxSlider* GetFreqSlider()  { return (wxSlider*) FindWindow( ID_FREQ_SLIDER ); }
    wxTextCtrl* GetBoostText()  { return (wxTextCtrl*) FindWindow( ID_BOOST_TEXT ); }
    wxTextCtrl* GetFreqText()  { return (wxTextCtrl*) FindWindow( ID_FREQ_TEXT ); }
    virtual bool Validate();
    virtual bool TransferDataToWindow();
    virtual bool TransferDataFromWindow();
    
private:
    // WDR: member variable declarations for BassBoostDialog
    
private:
    // WDR: handler declarations for BassBoostDialog
    void OnBoostText( wxCommandEvent &event );
    void OnFreqText( wxCommandEvent &event );
    void OnBoostSlider( wxCommandEvent &event );
    void OnFreqSlider( wxCommandEvent &event );
    void OnOk( wxCommandEvent &event );
    void OnCancel( wxCommandEvent &event );

private:
    DECLARE_EVENT_TABLE()

public:
	float freq;
	float boost;
	
};



#endif
