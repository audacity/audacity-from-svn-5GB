/**********************************************************************

  Audacity: A Digital Audio Editor

  AvcCompressor.cpp

  Vincent A. Busam

**********************************************************************/

/*  TODO List:

  1.  Add graph shows curve specified by grid, keep it up to date,
        allow setting of grid points by moving points in grid.
  2.  Better help
  3.  Radio button selection so Adjustment Settings can be times instead
		of samples.
  4.  Radio button selection so Amplification Settings can be db instead
        of raw values.
  5.  Save settings by name 
  6.  Remove "help" text in window when Audacity help available.

*/

#include <math.h>

#include <wx/wxprec.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/dcmemory.h>

#include "AvcCompressor.h"
#include "../WaveTrack.h"
#include "../Envelope.h"
#include "../widgets/Ruler.h"

// Including the following cpp file is quite unorthodox, but it gives the opportunity to
//		use iAVC's capability of generating inline code for the important methods.  We
//		can't trust compilers to generated inline code, even when the inline keyword is
//		used.
#ifdef _WINDOWS				// kludge for Audacity since we don't really have MS Windows
	#define max(a,b)  ( (a<b)?b:a )
#endif

#include "../../lib-src/iAVC/iAVC.h"
#include "../../lib-src/iAVC/iAVCsamples.h"
#include "../../lib-src/iAVC/iAVC.cpp"

#define ADJWIN_DEFAULT  DEFAULT_ADJUSTER_WINDOW_SIZE
#define ADJWIN_MIN		1000
#define ADJWIN_MAX		10000

#define DELAY_DEFAULT	0  //TEMP?  DEFAULT_MINIMUM_SAMPLES_BEFORE_SWITCH
#define DELAY_MIN		0
#define DELAY_MAX		5000

#define CHANGE_DEFAULT  DEFAULT_MINIMUM_SAMPLES_BEFORE_SWITCH
#define CHANGE_MIN		1000
#define CHANGE_MAX		5000

#define MINPCT_DEFAULT	DEFAULT_MAX_PCT_CHANGE_AT_ONCE
#define MINPCT_MIN		5
#define MINPCT_MAX		50

EffectAvcCompressor::EffectAvcCompressor()
{
}

bool EffectAvcCompressor::PromptUser()
{
   AvcCompressorDialog dlog(mParent, -1, _("Automatic Volume Control"));

   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   // Set parameters for iAVC class
   unsigned short int nTransform [ MULTIPLY_PCT_ARRAY_SIZE ];
   mnChangeWindow=dlog.GetChangeWindow();
   dlog.GetTransformArray(nTransform);

   if ( mAutoVolCtrl.SetSampleWindowSize(dlog.GetAdjusterWindow()+mnChangeWindow,
											dlog.GetAdjusterWindow(),
											0) == false ||
											mAutoVolCtrl.SetMinSamplesBeforeSwitch(mnChangeWindow) == false ) {
            wxMessageBox("Error setting parameters for automatic volume control.");
            return false;
   }
   
   mAutoVolCtrl.SetMaxPctChangeAtOnce(dlog.GetMinimumPercent());
   mAutoVolCtrl.SetMultipliers(nTransform);
   mAutoVolCtrl.SetNumberTracks(mnTracks);

   return true;
}

bool EffectAvcCompressor::Init()	// invoked before PromptUser
{
	if ( EffectSimplePairedTwoTrackInt16::Init() == false )
		return false;

	return true;
}

bool EffectAvcCompressor::ProcessSimplePairedTwoTrack(short int *bufferLeft, 
													  short int *bufferRight, // may be 0
													  sampleCount len)
{
	sampleCount i;
	short int left;
	short int right = 0;

	for ( i = 0 ; i < len ; ++i ) {
		left = bufferLeft[i];
		if ( bufferRight )
			right = bufferRight[i];
		mAutoVolCtrl.SetNextSample(left, right);
		mAutoVolCtrl.GetNextSample(left, right);
		bufferLeft[i] = left;
		if ( bufferRight )
			bufferRight[i] = right;
	}
	return true;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// AvcCompressorDialog
//----------------------------------------------------------------------------

// WDR: event table for AvcCompressorDialog

BEGIN_EVENT_TABLE(AvcCompressorDialog,wxDialog)
   EVT_BUTTON( wxID_OK, AvcCompressorDialog::OnOK )
   EVT_BUTTON( wxID_CANCEL, AvcCompressorDialog::OnCancel )
   EVT_BUTTON( ID_RESTORE_DEFAULTS, AvcCompressorDialog::OnRestoreDefaults )
   EVT_CHECKBOX(ID_FIRST_CURVE_CHECK+1, AvcCompressorDialog::OnCheckBox)
   EVT_CHECKBOX(ID_FIRST_CURVE_CHECK+2, AvcCompressorDialog::OnCheckBox)
   EVT_CHECKBOX(ID_FIRST_CURVE_CHECK+3, AvcCompressorDialog::OnCheckBox)
   EVT_CHECKBOX(ID_FIRST_CURVE_CHECK+4, AvcCompressorDialog::OnCheckBox)
   EVT_CHECKBOX(ID_FIRST_CURVE_CHECK+5, AvcCompressorDialog::OnCheckBox)
END_EVENT_TABLE()

AvcCompressorDialog::AvcCompressorDialog(wxWindow *parent, wxWindowID id,
                           const wxString &title,
                           const wxPoint &position, const wxSize& size,
                           long style ) :
		wxDialog( parent, id, title, position, size, style ),
		mctlAdjWin ( 0 ),
		mctlDelay ( 0 ),
		mctlChangeWin ( 0 ),
		mctlMinPct ( 0 )
{
	for ( int i = 0 ; i < NUM_CURVE_POINTS ; ++i ) {
		mctlCheckBoxes[i] = 0;
		mctlXAxis[i] = 0;
		mctlYAxis[i] = 0;
	}

	MakeAvcCompressorDialog( this, TRUE ); 

	wxCommandEvent eventDummy;
	OnRestoreDefaults(eventDummy);
}

AvcCompressorDialog::~AvcCompressorDialog()
{	// zero out pointers in case reference counts being used
	mctlAdjWin = 0;
	mctlDelay = 0;
	mctlChangeWin = 0;
	mctlMinPct = 0;

	for ( int i = 0 ; i < NUM_CURVE_POINTS ; ++i ) {
		mctlCheckBoxes[i] = 0;
		mctlXAxis[i] = 0;
		mctlYAxis[i] = 0;
	}
}

// figure out Y value for each possible X value
void AvcCompressorDialog::GetTransformArray( unsigned short int nTransform[MULTIPLY_PCT_ARRAY_SIZE] )
{
	long iCurPoint = 0;		// index to mnXAxis and mnYAxis
	long iPrevPoint= 0;		// index to mnXAxis and mnYAxis
	long iMultiply = 1;		// iMultiply and iDivide used to calculate fractional slopes
	long iDivide   = 1;
	long iBias     = 0;	    // keeps values from decreasing

	nTransform [ 0 ] = 0;

	for ( long i = 0 ; i < MULTIPLY_PCT_ARRAY_SIZE - 1 ; ++i ) {
		if ( i == mnXAxis [ iCurPoint ] && iCurPoint < NUM_CURVE_POINTS - 1 ) {
			// time to move to next point
			iPrevPoint = iCurPoint;
			// find next checked point
			while ( mctlCheckBoxes[++iCurPoint]->GetValue() == false)
				;	// last box guaranteed to be checked
			// Recalculate bias based on what would be calculated with old values and
			//		what would be calculated with new values.
			long iOld = i * iMultiply / iDivide + iBias;

			iMultiply = mnYAxis [ iCurPoint ] - mnYAxis [ iPrevPoint ];
			iDivide   = mnXAxis [ iCurPoint ] - mnXAxis [ iPrevPoint ];

			iBias = iOld - ( i * iMultiply / iDivide );
		}
		nTransform [ i ] = (unsigned short int)
         ( i * iMultiply / iDivide + iBias );
	}
	// set boundary case for loudest sound
	nTransform [ MULTIPLY_PCT_ARRAY_SIZE - 1 ] = MULTIPLY_PCT_ARRAY_SIZE - 1;
}

bool AvcCompressorDialog::LongRangeCheck ( wxWindow *window,
										   const long nValue,
										   const long nMin,
										   const long nMax )
{
	if ( nValue < nMin || nValue > nMax ) {
		// value out of range
        if ( !wxValidator::IsSilent() )
            wxBell();
		wxString strTemp;
		strTemp.Printf ( _("Value must be from %d to %d."), nMin, nMax );
		wxMessageBox(strTemp, _("Validation error"),
					 wxOK | wxICON_EXCLAMATION, GetParent() );
 		if ( window )
			window->SetFocus();
		return false;
	}
	return true;
}

// WDR: handler implementations for NoiseRemovalDialog

void AvcCompressorDialog::OnOK(wxCommandEvent &event)
{
	if ( Validate() && TransferDataFromWindow() ) {
		// do our dialog specific validation

		// Check Adjustment Settings
        mstrAdjWin.ToLong(&mnAdjWin);
		if ( LongRangeCheck( mctlAdjWin, mnAdjWin, ADJWIN_MIN, ADJWIN_MAX ) == false ) {
			// value out of range
			return;
		}

        mstrDelay.ToLong(&mnDelay);
		if ( LongRangeCheck( mctlDelay, mnDelay, DELAY_MIN, DELAY_MAX ) == false ) {
			// value out of range
			return;
		}

        mstrChangeWin.ToLong(&mnChangeWin);
		if ( LongRangeCheck( mctlChangeWin, mnChangeWin, CHANGE_MIN, CHANGE_MAX ) == false ) {
			// value out of range
			return;
		}

		if ( mnChangeWin > mnAdjWin ) {
			wxMessageBox(_("Change window size must be less than or equal to Adjustment window size."),
					 _("Validation error"), wxOK | wxICON_EXCLAMATION, GetParent() );
 			if ( mctlChangeWin )
				mctlChangeWin->SetFocus();
			return;
		}

        mstrMinPct.ToLong(&mnMinPct);
		if ( LongRangeCheck( mctlMinPct, mnMinPct, MINPCT_MIN, MINPCT_MAX ) == false ) {
			// value out of range
			return;
		}

		// Check Amplification Settings
		long iPrevPoint= 0;		// index to mnXAxis and mnYAxis
		for ( int i = 0 ; i < NUM_CURVE_POINTS ; ) {
			mstrXAxis[i].ToLong(&mnXAxis[i]);
			mstrYAxis[i].ToLong(&mnYAxis[i]);

			if ( i > 0 ) {
				if ( mnXAxis[i] <= mnXAxis[iPrevPoint] ) {
					wxMessageBox(_("Values in columns must be in ascending order."), 
								 _("Validation error"),
								 wxOK | wxICON_EXCLAMATION, GetParent() );
					mctlXAxis[(i==NUM_CURVE_POINTS-1) ? iPrevPoint : i]->SetFocus();
					return;
				}
				if ( mnYAxis[i] <= mnYAxis[iPrevPoint] ) {
					wxMessageBox(_("Values in columns must be in ascending order."), 
								 _("Validation error"),
								 wxOK | wxICON_EXCLAMATION, GetParent() );
					mctlYAxis[(i==NUM_CURVE_POINTS-1) ? iPrevPoint : i]->SetFocus();
					return;
				}
			}
			iPrevPoint = i;
			// find next checked point
			while ( ++i < NUM_CURVE_POINTS && mctlCheckBoxes[i]->GetValue() == false)
				;	// last box guaranteed to be checked
		}

		// AOK, time to return
        if ( IsModal() )
            EndModal(wxID_OK);
        else {
		    SetReturnCode(wxID_OK);
		    this->Show(FALSE);
        }
	}
}

void AvcCompressorDialog::OnCancel(wxCommandEvent &event)
{
   EndModal(false);
}

void AvcCompressorDialog::OnRestoreDefaults(wxCommandEvent &event)
{
	mstrAdjWin.Printf(_("%d"), ADJWIN_DEFAULT);
	mstrDelay.Printf(_("%d"), DELAY_DEFAULT);
	mstrChangeWin.Printf(_("%d"), CHANGE_DEFAULT);
	mstrMinPct.Printf(_("%d"), MINPCT_DEFAULT);

	for ( int i = 0 ; i < NUM_CURVE_POINTS ; ++ i ) {
		mctlCheckBoxes[i]->SetValue(true);
		mctlXAxis[i]->Show ( true );
		mctlYAxis[i]->Show ( true );
		mstrXAxis[i].Printf( _("%d"), iHoriz_AE75_3HK[i] );
		mstrYAxis[i].Printf( _("%d"), iVert_AE75_3HK[i] );
	}
	TransferDataToWindow();
}

void AvcCompressorDialog::OnCheckBox(wxCommandEvent & event)
{
	bool bCheck = mctlCheckBoxes[event.m_id-ID_FIRST_CURVE_CHECK]->GetValue();
    mctlXAxis[event.m_id-ID_FIRST_CURVE_CHECK]->Show ( bCheck );
    mctlYAxis[event.m_id-ID_FIRST_CURVE_CHECK]->Show ( bCheck );
}

wxSizer *AvcCompressorDialog::MakeAvcCompressorDialog(wxWindow * parent, bool call_fit,
														bool set_sizer)
{
	wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
	wxStaticBoxSizer *group;
	wxBoxSizer *boxSizer;
	wxStaticText *staticText;
	//wxTextCtrl *textCtrl;
	wxButton *button;
	wxFlexGridSizer *flexGridSizer;

	staticText =
       new wxStaticText(parent, ID_TEXT,
                        _("Automatic Volume Control by Vincent A. Busam"),
                        wxDefaultPosition, wxDefaultSize, 0);
	mainSizer->Add(staticText, 0, wxALIGN_CENTRE | wxALL, 5);

	// 0.  Box Sizer for horizontal components
	wxBoxSizer *horizontalSizer = new wxBoxSizer(wxHORIZONTAL);

	// 1.  Box Sizer for leftmost group of controls
	wxBoxSizer *leftSizer = new wxBoxSizer(wxVERTICAL);

	// 1.1  Group Box for adjustment window settings
   
	group = new wxStaticBoxSizer(new wxStaticBox(parent, -1,
                                                _("Adjustment Settings")), wxVERTICAL);
	flexGridSizer = new wxFlexGridSizer(2, 0, 0);

	// 1.1.1  Adjustment Window
	staticText =
       new wxStaticText(parent, ID_TEXT, _("Adjustment Window:"),
                        wxDefaultPosition, wxDefaultSize, 0 );
	flexGridSizer->Add(staticText, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

	mctlAdjWin =
       new wxTextCtrl(parent, ID_ADJWINTEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0,
					  wxTextValidator(wxFILTER_NUMERIC, &mstrAdjWin ) );
	flexGridSizer->Add(mctlAdjWin, 0, wxALIGN_CENTRE | wxALL, 5);

	// 1.1.2  Adjustment Delay
	//staticText =
    //   new wxStaticText(parent, ID_TEXT, _("Adjustment Delay:"),
    //                    wxDefaultPosition, wxDefaultSize, 0);
	//flexGridSizer->Add(staticText, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

	//mctlDelay =
    //   new wxTextCtrl(parent, ID_DELAYTEXT, "", wxDefaultPosition,
    //                  wxSize(40, -1), 0,
	//				  wxTextValidator(wxFILTER_NUMERIC, &mstrDelay ));
	//flexGridSizer->Add(mctlDelay, 0, wxALIGN_CENTRE | wxALL, 5);

	// 1.1.3  Min Change Window
	staticText =
       new wxStaticText(parent, ID_TEXT, _("Minimum Change Window:"),
                        wxDefaultPosition, wxDefaultSize, 0);
	flexGridSizer->Add(staticText, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

	mctlChangeWin =
       new wxTextCtrl(parent, ID_CHANGEWINTEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0,
					  wxTextValidator(wxFILTER_NUMERIC, &mstrChangeWin ));
	flexGridSizer->Add(mctlChangeWin, 0, wxALIGN_CENTRE | wxALL, 5);

	// 1.1.4  Min Change %
	staticText =
       new wxStaticText(parent, ID_TEXT, _("Minimum Change %:"),
                        wxDefaultPosition, wxDefaultSize, 0);
	flexGridSizer->Add(staticText, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

	mctlMinPct =
       new wxTextCtrl(parent, ID_MINPCTTEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0,
					  wxTextValidator(wxFILTER_NUMERIC, &mstrMinPct ));
	 flexGridSizer->Add(mctlMinPct, 0, wxALIGN_CENTRE | wxALL, 5);

	// 1.1.end  Add group
	group->Add( flexGridSizer, 0,  wxALIGN_CENTRE|wxALL, 5 );

	leftSizer->Add( group, 0, wxALIGN_TOP |wxALL, 5 );

	// 1.2 area under group box
	staticText =
       new wxStaticText(parent, ID_TEXT,
                        _("Above values are in samples.\n"
						  "Adjustment Window defines number of \nsamples in moving average.\n"
						  "Change window defines minimum time \nbetween volume changes.\n"
						  "Minimum % change of volume adjustment \nbefore making a volume change.\n"
						  "Grid at right determines how much to amplify \neach volume level.\n"
						  "For more information see: \n"
						  "http://www.busam.com/skyland/iavc\n"
						  "7/21/02: WAV and MP3 files both work."
						 ), 
                        wxDefaultPosition, wxDefaultSize, 0);
	leftSizer->Add(staticText, 0, wxALIGN_CENTRE | wxALL, 5);

	// 1.end 
	horizontalSizer->Add( leftSizer, 0, wxALIGN_TOP |wxALL, 5 );

	// 2.  Group Box for volume settings
   
	group = new wxStaticBoxSizer(new wxStaticBox(parent, -1,
                                                _("Amplification Settings")), wxVERTICAL);

	// 2.1  Add one row each time through loop

	flexGridSizer = new wxFlexGridSizer(3, 0, 0);

	staticText =
       new wxStaticText(parent, ID_TEXT, _("Enabled"),
                        wxDefaultPosition, wxDefaultSize, 0);
	flexGridSizer->Add(staticText, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

	staticText =
       new wxStaticText(parent, ID_TEXT, _("Original Value"),
                        wxDefaultPosition, wxDefaultSize, 0);
	flexGridSizer->Add(staticText, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
	
	staticText =
       new wxStaticText(parent, ID_TEXT, _("New Value"),
                        wxDefaultPosition, wxDefaultSize, 0);
	flexGridSizer->Add(staticText, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

	for ( int i = 0 ; i < NUM_CURVE_POINTS ; ++i )
	{
		mctlCheckBoxes[i] =
		   new wxCheckBox(parent, ID_FIRST_CURVE_CHECK + i, "", wxDefaultPosition,
						  wxSize(40, -1), 0);
		flexGridSizer->Add(mctlCheckBoxes[i], 0, wxALIGN_CENTRE | wxALL, 5);
		mctlCheckBoxes[i]->SetValue(true);

		mctlXAxis[i] =
		   new wxTextCtrl(parent, ID_FIRST_CURVE_X + i, "", wxDefaultPosition,
						  wxSize(40, -1), 0,
						  wxTextValidator(wxFILTER_NUMERIC, &mstrXAxis[i] ));
		flexGridSizer->Add(mctlXAxis[i], 0, wxALIGN_CENTRE | wxALL, 5);

		mctlYAxis[i] =
		   new wxTextCtrl(parent, ID_FIRST_CURVE_Y + i, "", wxDefaultPosition,
						  wxSize(40, -1), 0,
						  wxTextValidator(wxFILTER_NUMERIC, &mstrYAxis[i] ));
		flexGridSizer->Add(mctlYAxis[i], 0, wxALIGN_CENTRE | wxALL, 5);
	}
	mctlCheckBoxes[0]->Enable(false);
	mctlXAxis[0]->Enable(false);
	mctlYAxis[0]->Enable(false);
	mctlCheckBoxes[NUM_CURVE_POINTS-1]->Enable(false);
	mctlXAxis[NUM_CURVE_POINTS-1]->Enable(false);
	mctlYAxis[NUM_CURVE_POINTS-1]->Enable(false);

	// 2.end  Add group
	group->Add( flexGridSizer, 0,  wxALIGN_CENTRE|wxALL, 5 );
	horizontalSizer->Add( group, 0, wxALIGN_CENTRE|wxALL, 5 );

	mainSizer->Add( horizontalSizer, 0,  wxALIGN_CENTRE|wxALL, 5 );

	// Last:  Add buttons
	boxSizer = new wxBoxSizer(wxHORIZONTAL);

	// Add restore defaults button
	button =
       new wxButton(parent, ID_RESTORE_DEFAULTS, _("Restore Defaults"), wxDefaultPosition,
                    wxDefaultSize, 0);
	boxSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

	// Add OK button
	button =
       new wxButton(parent, wxID_OK, _("OK"), wxDefaultPosition,
                    wxDefaultSize, 0);
	button->SetDefault();
	button->SetFocus();
	boxSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

	// Add Cancel button
	button =
       new wxButton(parent, wxID_CANCEL, _("Cancel"), wxDefaultPosition,
                    wxDefaultSize, 0);
	boxSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

	mainSizer->Add(boxSizer, 0, wxALIGN_CENTRE | wxALL, 5);

	if (set_sizer) {
		parent->SetAutoLayout(TRUE);
		parent->SetSizer(mainSizer);
		if (call_fit) {
			mainSizer->Fit(parent);
			mainSizer->SetSizeHints(parent);
		}
	}

	return mainSizer;
}

