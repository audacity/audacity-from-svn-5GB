/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.cpp

  Dominic Mazzoni
  
  This class implements a VST Plug-in effect.  The plug-in must be
  loaded in a platform-specific way and passed into the constructor,
  but from here this class handles the interfacing.  VST plug-ins
  are used in Cubase and other Steinberg products, and all of those
  files and the information within is copyrighted by Steinberg.

**********************************************************************/

#include "AEffect.h"   // VST API

#include <wx/button.h>
#include <wx/slider.h>

#include "Effect.h"    // Audacity Effect base class
#include "VSTEffect.h" // This class's header file

VSTEffect::VSTEffect(wxString pluginName, AEffect *aEffect)
{
    this->aEffect = aEffect;
    this->pluginName = pluginName;
}
  
wxString VSTEffect::GetEffectName()
{
    return pluginName;
}

bool VSTEffect::DoIt(
        WaveTrack *t,
	    sampleCount start,
	    sampleCount len)
{
    aEffect->dispatcher(aEffect, effOpen, 0, 0, NULL, 0.0);

    sampleCount blockSize = t->GetIdealBlockSize();

    aEffect->dispatcher(aEffect, effSetSampleRate, 0, 0, NULL, (float)t->rate);
    aEffect->dispatcher(aEffect, effSetBlockSize, 0, blockSize, NULL, 0.0);
    
    // Try to figure out how many parameters it takes by seeing how
    // many parameters have names
    char temp[8][256];
    int numParameters=0;
    do {
        aEffect->dispatcher(aEffect, effGetParamName, numParameters, 0,
                            (void *)temp[numParameters], 0.0);
        
        if (strstr(temp[numParameters], "ABOUT"))
            break;
        if (numParameters>0 && !strcmp(temp[numParameters], temp[numParameters-1]))
            break;

        numParameters++;
    } while (temp[0] != 0 && numParameters<8);
    
    //numParameters = aEffect->numParams;
    
    if (numParameters > 0) {
        VSTEffectDialog d((wxWindow *)0, pluginName, numParameters, aEffect);
        d.ShowModal();

        if (!d.GetReturnCode())
            return false;
    }
    
    // Do the processing
    
    sampleCount s = start;
    
    int inputs = aEffect->numInputs;
    int outputs = aEffect->numOutputs;
    int i, j;

    sampleType *buffer = new sampleType[blockSize];
    float **fInBuffer = new float *[inputs];
    for(i=0; i<inputs; i++)
        fInBuffer[i] = new float[blockSize];
    float **fOutBuffer = new float *[outputs];
    for(i=0; i<outputs; i++)
        fOutBuffer[i] = new float[blockSize];

    while(len) {
        int block = blockSize;
        if (block > len)
            block = len;

        t->Get(buffer, s, block);
        for(j=0; j<inputs; j++)
          for(i=0; i<block; i++)
            fInBuffer[j][i] = float(buffer[i] / 32767.);

        aEffect->processReplacing(aEffect, fInBuffer, fOutBuffer, block);
                
        for(i=0; i<block; i++)
          buffer[i] = (sampleType)(fOutBuffer[0][i] * 32767.);
          
        t->Set(buffer, s, block);

        len -= block;
        s += block;
    }

    delete[] buffer;
    for(i=0; i<inputs; i++)
        delete fInBuffer[i];
    for(i=0; i<outputs; i++)
        delete fOutBuffer[i];
    delete[] fInBuffer;
    delete[] fOutBuffer; 

    #ifndef __WXMSW__
    aEffect->dispatcher(aEffect, effClose, 0, 0, NULL, 0.0);
    #endif

    return true;
}

const int VSTEFFECT_SLIDER_ID = 13100;

BEGIN_EVENT_TABLE(VSTEffectDialog, wxDialog)
  EVT_BUTTON(wxID_OK, VSTEffectDialog::OnOK)
  EVT_BUTTON(wxID_CANCEL, VSTEffectDialog::OnCancel)
  EVT_COMMAND_SCROLL(VSTEFFECT_SLIDER_ID, VSTEffectDialog::OnSlider)
  EVT_SLIDER(VSTEFFECT_SLIDER_ID, VSTEffectDialog::OnSlider)
END_EVENT_TABLE()

IMPLEMENT_CLASS(VSTEffectDialog, wxDialog)

VSTEffectDialog::VSTEffectDialog(wxWindow *parent,                            
                            wxString effectName,
                            int numParams,
                            AEffect *aEffect,
						    const wxPoint& pos)
  : wxDialog( parent, -1, effectName, pos, wxSize(320, 400), wxDEFAULT_DIALOG_STYLE )
{
  this->aEffect = aEffect;
  this->numParams = numParams;
  
  int y=10;
  
  new wxStaticText(this, 0, "VST Plug-in parameters:", wxPoint(10, y), wxSize(300, 15));
  y += 20;
  
  sliders = new wxSlider *[numParams];
  labels = new wxStaticText *[numParams];
  
  for(int p=0; p<numParams; p++) {

    char paramName[256];
    aEffect->dispatcher(aEffect, effGetParamName, p, 0, (void *)paramName, 0.0);
    new wxStaticText(this, 0, wxString(paramName), wxPoint(10, y), wxSize(300, 15));

    float val =
        aEffect->getParameter(aEffect, p);

    sliders[p] = 
        new wxSlider(this, VSTEFFECT_SLIDER_ID,
                1000*val, 0, 1000,
                wxPoint(10, y+15), wxSize(300,20));

    char label[256];
    aEffect->dispatcher(aEffect, effGetParamDisplay, p, 0, (void *)label, 0.0);
    labels[p] = 
        new wxStaticText(this, 0, wxString(label), wxPoint(200, y+35), wxSize(45, 15));                

    char units[256];
    aEffect->dispatcher(aEffect, effGetParamLabel, p, 0, (void *)units, 0.0);
    new wxStaticText(this, 0, wxString(units), wxPoint(250, y+35), wxSize(45, 15));                

    y += 55;
  }
  
  wxButton *ok = new wxButton(this, wxID_OK, "OK", wxPoint(110, y), wxSize(80,30));
  wxButton *cancel = new wxButton(this, wxID_CANCEL, "Cancel", wxPoint(210, y), wxSize(80,30));
  y+=40;

  wxSize size;
  size.x = 320;
  size.y = y;

  Centre(wxBOTH | wxCENTER_FRAME);

  SetSize( size );
}

VSTEffectDialog::~VSTEffectDialog()
{
  // TODO: proper disposal here

  delete[] sliders;
  delete[] labels;
}

void VSTEffectDialog::OnSlider(wxCommandEvent& WXUNUSED(event))
{
  for(int p=0; p<numParams; p++) {
    float val;
    
    val = sliders[p]->GetValue() / 1000.;    
    aEffect->setParameter(aEffect, p, val);

    char label[256];
    aEffect->dispatcher(aEffect, effGetParamDisplay, p, 0, (void *)label, 0.0);
    labels[p]->SetLabel(wxString(label));
  }
}

void VSTEffectDialog::OnOK(wxCommandEvent& WXUNUSED(event))
{
  EndModal(TRUE);
}

void VSTEffectDialog::OnCancel(wxCommandEvent& WXUNUSED(event))
{
  EndModal(FALSE);
}

