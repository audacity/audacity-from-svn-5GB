/**********************************************************************

  Audacity: A Digital Audio Editor

  Phaser.cpp

  Nasca Octavian Paul   <paulnasca@email.ro> or <paulnasca@yahoo.com>

**********************************************************************/

#include <math.h>

//#include <wx/generic/textdlgg.h>

#include "Phaser.h"
#include "../WaveTrack.h"

//
// EffectPhaser
//


#define phaserlfoshape 4

// How many samples are processed before compute the lfo value again
#define lfoskipsamples 20 

EffectPhaser::EffectPhaser()
{
  freq=0.4;
  depth=100;
  startphase=0;
  stages=2;
  drywet=128;
  fb=0;
}

bool EffectPhaser::Begin(wxWindow *parent)
{
  PhaserDialog dlog(parent, -1, "Phaser");

  dlog.freq = freq;
  dlog.startphase = startphase * 180 / M_PI;
  dlog.fb = fb;
  dlog.depth = depth;
  dlog.stages = stages;
  dlog.drywet = drywet;

  dlog.TransferDataToWindow();
  dlog.CentreOnParent();
  dlog.ShowModal();

  if (!dlog.GetReturnCode())
	return false;

  freq = dlog.freq;
  startphase = dlog.startphase * M_PI / 180;
  fb = dlog.fb;
  depth = dlog.depth;
  stages = dlog.stages;
  drywet = dlog.drywet;

  return true;
}

bool EffectPhaser::DoIt(WaveTrack *t,
                           sampleCount start,
                           sampleCount len)
{
  float samplerate = (float)(t->rate);
  
  /*Phaser initialisation */
  float *old = new float[stages];
  int j;
  unsigned long skipcount=0;
  for(j=0;j<stages;j++)
    old[j]=0;
  float m,gain=0,tmp,in,out,fbout=0;
  float lfoskip=freq*2*3.141592653589/samplerate;

  sampleCount s = start;
  sampleCount blockSize = t->GetIdealBlockSize();
  
  sampleType *buffer = new sampleType[blockSize];
    
  while(len) {
    int block = blockSize;
    if (block > len)
      block = len;
    
    t->Get(buffer, s, block);

    for(int i=0; i<block; i++) {
      in=buffer[i];

      m=in+fbout*fb/100;
      if (((skipcount++)%lfoskipsamples)==0){
        //compute sine between 0 and 1
        gain=(1+cos(skipcount*lfoskip+startphase))/2;
        
        // change lfo shape
        gain=(exp(gain*phaserlfoshape)-1)/(exp(phaserlfoshape)-1); 
        
        gain=1-gain/255*depth; // attenuate the lfo
      }

      // phasing routine
      for (j=0;j<stages;j++) {
        tmp=old[j];
        old[j]=gain*tmp+m;
        m=tmp-gain*old[j];
      }
      fbout=m;
      out=(m*drywet+in*(255-drywet))/255;

      // Prevents clipping
      if (out<-32768)
        out=-32768;
      else if (out>32767)
        out=32767;
      
      buffer[i] = (sampleType)out;
    }
    
    t->Set(buffer, s, block);
    
    len -= block;
    s += block;
  }
  
  delete[] buffer;

  return true;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// PhaserDialog
//----------------------------------------------------------------------------

#define FREQ_MIN 1
#define FREQ_MAX 40
#define PHASE_MIN 0
#define PHASE_MAX 359
#define DEPTH_MIN 0
#define DEPTH_MAX 255
#define STAGES_MIN 2
#define STAGES_MAX 24
#define DRYWET_MIN 0
#define DRYWET_MAX 255
#define FB_MIN -100
#define FB_MAX 100

// WDR: event table for PhaserDialog

BEGIN_EVENT_TABLE(PhaserDialog,wxDialog)
    EVT_BUTTON( wxID_OK, PhaserDialog::OnOk )
    EVT_BUTTON( wxID_CANCEL, PhaserDialog::OnCancel )
    EVT_TEXT( ID_FREQTEXT, PhaserDialog::OnFreqText )
    EVT_TEXT( ID_PHASETEXT, PhaserDialog::OnPhaseText )
    EVT_TEXT( ID_DEPTHTEXT, PhaserDialog::OnDepthText )
    EVT_TEXT( ID_FEEDBACKTEXT, PhaserDialog::OnFeedbackText )
    EVT_SLIDER( ID_FREQSLIDER, PhaserDialog::OnFreqSlider )
    EVT_SLIDER( ID_PHASESLIDER, PhaserDialog::OnPhaseSlider )
    EVT_SLIDER( ID_DEPTHSLIDER, PhaserDialog::OnDepthSlider )
    EVT_SLIDER( ID_FEEDBACKSLIDER, PhaserDialog::OnFeedbackSlider )
END_EVENT_TABLE()

PhaserDialog::PhaserDialog( wxWindow *parent, wxWindowID id, const wxString &title,
    const wxPoint &position, const wxSize& size, long style ) :
    wxDialog( parent, id, title, position, size, style )
{
    CreatePhaserDialog( this, TRUE ); 
}

bool PhaserDialog::Validate()
{
    return TRUE;
}

bool PhaserDialog::TransferDataToWindow()
{
  wxSlider *slider;

  slider = GetFreqSlider();
  if (slider)
	slider->SetValue(freq*10);

  slider = GetPhaseSlider();
  if (slider)
	slider->SetValue(startphase);

  slider = GetDepthSlider();
  if (slider)
	slider->SetValue(depth);

  slider = GetFeedbackSlider();
  if (slider)
	slider->SetValue(fb);

  slider = GetDryWet();
  if (slider)
	slider->SetValue(drywet);

  wxSpinCtrl* spin = GetStages();
  if (spin)
	spin->SetValue(stages);

  wxTextCtrl *text = GetFreqText();
  if (text) {
	wxString str;
	str.Printf("%.1f", freq);
	text->SetValue(str);
  }

  text = GetPhaseText();
  if (text) {
	wxString str;
	str.Printf("%d", (int)startphase);
	text->SetValue(str);
  }

  text = GetDepthText();
  if (text) {
	wxString str;
	str.Printf("%d", (int)depth);
	text->SetValue(str);
  }

  text = GetFeedbackText();
  if (text) {
	wxString str;
	str.Printf("%d", (int)fb);
	text->SetValue(str);
  }

  return TRUE;
}

bool PhaserDialog::TransferDataFromWindow()
{
  wxTextCtrl *c;
  long x;

  c = GetFreqText();
  if (c) {
	double d;
	c->GetValue().ToDouble(&d);
	freq = TrapDouble(d*10, FREQ_MIN, FREQ_MAX)/10;
  }

  c = GetPhaseText();
  if (c) {
	c->GetValue().ToLong(&x);
	startphase = TrapLong(x, PHASE_MIN, PHASE_MAX);
  }

  c = GetDepthText();
  if (c) {
	c->GetValue().ToLong(&x);
	depth = TrapLong(x, DEPTH_MIN, DEPTH_MAX);
  }

  c = GetFeedbackText();
  if (c) {
	c->GetValue().ToLong(&x);
	fb = TrapLong(x, FB_MIN, FB_MAX);
  }

  wxSpinCtrl *p = GetStages();
  if (p) {
	stages = TrapLong(p->GetValue(), STAGES_MIN, STAGES_MAX);
	if ((stages%2) == 1) // must be even
	  stages = TrapLong(stages-1, STAGES_MIN, STAGES_MAX);
	
  }

  wxSlider *s = GetDryWet();
  if (s) {
	drywet = TrapLong(s->GetValue(), DRYWET_MIN, DRYWET_MAX);
  }

  return TRUE;
}

// WDR: handler implementations for PhaserDialog

void PhaserDialog::OnFeedbackSlider( wxCommandEvent &event )
{
  wxString str;
  long fb = GetFeedbackSlider()->GetValue();
  if (fb > 0)  // round to nearest multiple of 10
	fb = ((fb+5)/10)*10;
  else
	fb = ((fb-5)/10)*10;
  str.Printf("%d", fb);
  GetFeedbackText()->SetValue(str);
}

void PhaserDialog::OnDepthSlider( wxCommandEvent &event )
{
  wxString str;
  long depth = GetDepthSlider()->GetValue();
  str.Printf("%d", depth);
  GetDepthText()->SetValue(str);
}

void PhaserDialog::OnPhaseSlider( wxCommandEvent &event )
{
  wxString str;
  long phase = GetPhaseSlider()->GetValue();
  phase = ((phase+5)/10)*10;  // round to nearest multiple of 10
  str.Printf("%d", phase);
  GetPhaseText()->SetValue(str);
}

void PhaserDialog::OnFreqSlider( wxCommandEvent &event )
{
  wxString str;
  long freq = GetFreqSlider()->GetValue();
  str.Printf("%.1f", freq/10.0);
  GetFreqText()->SetValue(str);
}

void PhaserDialog::OnFeedbackText( wxCommandEvent &event )
{
  wxTextCtrl *c = GetFeedbackText();
  if (c) {
	long fb;

	c->GetValue().ToLong(&fb);
	fb = TrapLong(fb, FB_MIN, FB_MAX);
	
	wxSlider *slider = GetFeedbackSlider();
	if (slider)
	  slider->SetValue(fb);
  }
}

void PhaserDialog::OnDepthText( wxCommandEvent &event )
{
  wxTextCtrl *c = GetDepthText();
  if (c) {
	long depth;

	c->GetValue().ToLong(&depth);
	depth = TrapLong(depth, DEPTH_MIN, DEPTH_MAX);
	
	wxSlider *slider = GetDepthSlider();
	if (slider)
	  slider->SetValue(depth);
  }
}

void PhaserDialog::OnPhaseText( wxCommandEvent &event )
{
  wxTextCtrl *c = GetPhaseText();
  if (c) {
	long phase;

	c->GetValue().ToLong(&phase);
	phase = TrapLong(phase, PHASE_MIN, PHASE_MAX);
	
	wxSlider *slider = GetPhaseSlider();
	if (slider)
	  slider->SetValue(phase);
  }
}

void PhaserDialog::OnFreqText( wxCommandEvent &event )
{
  wxTextCtrl *c = GetFreqText();
  if (c) {
	double freq;

	c->GetValue().ToDouble(&freq);
	freq = TrapDouble(freq*10, FREQ_MIN, FREQ_MAX);

	wxSlider *slider = GetFreqSlider();
	if (slider)
	  slider->SetValue(freq);
  }
}

void PhaserDialog::OnOk(wxCommandEvent &event)
{
  TransferDataFromWindow();

  if (Validate())
    EndModal(true);
  else {
    event.Skip();
  }
}

void PhaserDialog::OnCancel(wxCommandEvent &event)
{
  EndModal(false);
}

// Implement window functions

wxSizer *CreatePhaserDialog( wxPanel *parent, bool call_fit, bool set_sizer )
{
    wxBoxSizer *item0 = new wxBoxSizer( wxVERTICAL );

    wxStaticText *item1 = new wxStaticText( parent, ID_TEXT, "Phaser by Nasca Octavian Paul", wxDefaultPosition, wxDefaultSize, 0 );
    item0->Add( item1, 0, wxALIGN_CENTRE|wxALL, 5 );

    wxBoxSizer *item2 = new wxBoxSizer( wxHORIZONTAL );

    wxStaticText *item3 = new wxStaticText( parent, ID_TEXT, "Stages:", wxDefaultPosition, wxDefaultSize, 0 );
    item2->Add( item3, 0, wxALIGN_CENTRE|wxALL, 5 );

    wxSpinCtrl *item4 = new wxSpinCtrl( parent, ID_STAGES, "2", wxDefaultPosition, wxSize(40,-1), 0, 2, 24, 2 );
    item2->Add( item4, 0, wxALIGN_CENTRE|wxALL, 5 );

    wxBoxSizer *item5 = new wxBoxSizer( wxVERTICAL );

    wxSlider *item6 = new wxSlider( parent, ID_DRYWET, 0, DRYWET_MIN, DRYWET_MAX, wxDefaultPosition, wxSize(100,-1), wxSL_HORIZONTAL );
    item5->Add( item6, 1, wxGROW|wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxTOP, 5 );

    wxBoxSizer *item7 = new wxBoxSizer( wxHORIZONTAL );

    wxStaticText *item8 = new wxStaticText( parent, ID_TEXT, "DRY", wxDefaultPosition, wxDefaultSize, 0 );
    item7->Add( item8, 0, wxALIGN_CENTRE|wxALL, 5 );

    item7->Add( 10, 10, 1, wxALIGN_CENTRE|wxLEFT|wxRIGHT|wxTOP, 5 );

    wxStaticText *item9 = new wxStaticText( parent, ID_TEXT, "WET", wxDefaultPosition, wxDefaultSize, 0 );
    item7->Add( item9, 0, wxALIGN_CENTRE|wxALL, 5 );

    item5->Add( item7, 1, wxGROW|wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT, 5 );

    item2->Add( item5, 1, wxALIGN_CENTRE|wxALL, 5 );

    item0->Add( item2, 0, wxGROW|wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    wxFlexGridSizer *item10 = new wxFlexGridSizer( 3, 0, 0 );

    wxStaticText *item11 = new wxStaticText( parent, ID_TEXT, "LFO Frequency (Hz):", wxDefaultPosition, wxDefaultSize, 0 );
    item10->Add( item11, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    wxTextCtrl *item12 = new wxTextCtrl( parent, ID_FREQTEXT, "", wxDefaultPosition, wxSize(40,-1), 0 );
    item10->Add( item12, 0, wxALIGN_CENTRE|wxALL, 5 );

    wxSlider *item13 = new wxSlider( parent, ID_FREQSLIDER, 100, FREQ_MIN, FREQ_MAX, wxDefaultPosition, wxSize(100,-1), wxSL_HORIZONTAL );
    item10->Add( item13, 0, wxALIGN_CENTRE|wxALL, 5 );

    wxStaticText *item14 = new wxStaticText( parent, ID_TEXT, "LFO Start Phase (deg.):", wxDefaultPosition, wxDefaultSize, 0 );
    item10->Add( item14, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    wxTextCtrl *item15 = new wxTextCtrl( parent, ID_PHASETEXT, "", wxDefaultPosition, wxSize(40,-1), 0 );
    item10->Add( item15, 0, wxALIGN_CENTRE|wxALL, 5 );

    wxSlider *item16 = new wxSlider( parent, ID_PHASESLIDER, 0, PHASE_MIN, PHASE_MAX, wxDefaultPosition, wxSize(100,-1), wxSL_HORIZONTAL );
    item10->Add( item16, 0, wxALIGN_CENTRE|wxALL, 5 );

    wxStaticText *item17 = new wxStaticText( parent, ID_TEXT, "Depth:", wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
    item10->Add( item17, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    wxTextCtrl *item18 = new wxTextCtrl( parent, ID_DEPTHTEXT, "", wxDefaultPosition, wxSize(40,-1), 0 );
    item10->Add( item18, 0, wxALIGN_CENTRE|wxALL, 5 );

    wxSlider *item19 = new wxSlider( parent, ID_DEPTHSLIDER, 0, DEPTH_MIN, DEPTH_MAX, wxDefaultPosition, wxSize(100,-1), wxSL_HORIZONTAL );
    item10->Add( item19, 0, wxALIGN_CENTRE|wxALL, 5 );

    wxStaticText *item20 = new wxStaticText( parent, ID_TEXT, "Feedback (%):", wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
    item10->Add( item20, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    wxTextCtrl *item21 = new wxTextCtrl( parent, ID_FEEDBACKTEXT, "", wxDefaultPosition, wxSize(40,-1), 0 );
    item10->Add( item21, 0, wxALIGN_CENTRE|wxALL, 5 );

    wxSlider *item22 = new wxSlider( parent, ID_FEEDBACKSLIDER, 0, FB_MIN, FB_MAX, wxDefaultPosition, wxSize(100,-1), wxSL_HORIZONTAL );
    item10->Add( item22, 0, wxALIGN_CENTRE|wxALL, 5 );

    item0->Add( item10, 0, wxALIGN_CENTRE|wxALL, 5 );

    wxBoxSizer *item23 = new wxBoxSizer( wxHORIZONTAL );

    wxButton *item24 = new wxButton( parent, wxID_OK, "OK", wxDefaultPosition, wxDefaultSize, 0 );
    item24->SetDefault();
    item23->Add( item24, 0, wxALIGN_CENTRE|wxALL, 5 );

    wxButton *item25 = new wxButton( parent, wxID_CANCEL, "Cancel", wxDefaultPosition, wxDefaultSize, 0 );
    item23->Add( item25, 0, wxALIGN_CENTRE|wxALL, 5 );

    item0->Add( item23, 0, wxALIGN_CENTRE|wxALL, 5 );

    if (set_sizer)
    {
        parent->SetAutoLayout( TRUE );
        parent->SetSizer( item0 );
        if (call_fit)
        {
            item0->Fit( parent );
            item0->SetSizeHints( parent );
        }
    }
    
    return item0;
}
