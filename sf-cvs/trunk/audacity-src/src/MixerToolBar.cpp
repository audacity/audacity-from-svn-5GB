/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerToolBar.cpp

  Dominic Mazzoni
 
  See MixerToolBar.h for details

**********************************************************************/

#include "MixerToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/defs.h>
#include <wx/choice.h>
#include <wx/dcmemory.h>
#include <wx/event.h>
#include <wx/brush.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/settings.h>
#endif

#include <wx/image.h>
#include <wx/tooltip.h>

#include "Audacity.h"
#include "ImageManipulation.h"
#include "widgets/ASlider.h"
#include "Prefs.h"

#if USE_PORTMIXER
#include "portmixer.h"
#endif

#include "../images/MixerImages.h"

////////////////////////////////////////////////////////////
/// Methods for MixerToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(MixerToolBar, wxWindow)
   EVT_PAINT(MixerToolBar::OnPaint)
END_EVENT_TABLE()

//Standard contructor
MixerToolBar::MixerToolBar(wxWindow * parent):
   ToolBar(parent, -1, wxPoint(1, 1), wxSize(520, 27))
{
   InitializeMixerToolBar();
}

//Another constructor
MixerToolBar::MixerToolBar(wxWindow * parent, wxWindowID id,
                           const wxPoint & pos,
                           const wxSize & size):ToolBar(parent, id,
                                                        pos, size)
{
   InitializeMixerToolBar();
}

#if USE_PORTMIXER
static int DummyCallbackFunc(void *inputBuffer, void *outputBuffer,
                             unsigned long framesPerBuffer,
                             PaTimestamp outTime, void *userData)
{
   return 0;
}
#endif

// This sets up the MixerToolBar, initializing all the important values
// and creating the buttons.
void MixerToolBar::InitializeMixerToolBar()
{
   mIdealSize = wxSize(520, 27);
   mTitle = _("Audacity Mixer Toolbar");
   mType = MixerToolBarID;

   wxColour backgroundColour =
       wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   wxColour origColour(204, 204, 204);

   wxImage *speaker = new wxImage(Speaker);
   wxImage *speakerAlpha = new wxImage(SpeakerAlpha);
   wxImage *bkgnd = CreateSysBackground(25, 25, 3,
                                        backgroundColour);
   wxImage *speakerFinal = OverlayImage(bkgnd, speaker, speakerAlpha, 0, 0);
   wxImage *mic = new wxImage(Mic);
   wxImage *micAlpha = new wxImage(MicAlpha);
   wxImage *micFinal = OverlayImage(bkgnd, mic, micAlpha, 0, 0);

   mPlayBitmap = new wxBitmap(speakerFinal);
   mRecordBitmap = new wxBitmap(micFinal);

   delete speaker;
   delete speakerAlpha;
   delete bkgnd;
   delete speakerFinal;
   delete mic;
   delete micAlpha;
   delete micFinal;

   mOutputSlider = new ASlider(this, 0, "Output Volume",
                               wxPoint(30, 1), wxSize(130, 25));

   mInputSlider = new ASlider(this, 0, "Input Volume",
                              wxPoint(210, 1), wxSize(130, 25));

   mInputSourceChoice = NULL;

   #if USE_PORTMIXER
   PaError          error;
   PortAudioStream *stream;
   PxMixer         *mixer;
   wxString        recDevice;
   wxString        playDevice;
   int             recDeviceNum;
   int             playDeviceNum;
   int             inputChannels = 2;
   int             j;

   recDeviceNum = Pa_GetDefaultInputDeviceID();
   playDeviceNum = Pa_GetDefaultOutputDeviceID();
   recDevice = gPrefs->Read("/AudioIO/RecordingDevice", "");
   playDevice = gPrefs->Read("/AudioIO/PlaybackDevice", "");

   for(j=0; j<Pa_CountDevices(); j++) {
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      if (info->name == playDevice && info->maxOutputChannels > 0)
         playDeviceNum = j;
      if (info->name == recDevice && info->maxInputChannels > 0)
         recDeviceNum = j;
   }

   // TODO: open two streams (one after another), for playback
   // and recording separately
   error = Pa_OpenStream(&stream, recDeviceNum, inputChannels, paFloat32, NULL,
                         paNoDevice, 0, paFloat32, NULL,
                         44100, 512, 1, paClipOff | paDitherOff,
                         DummyCallbackFunc, NULL);
   if (!error) {
      mixer = Px_OpenMixer(stream, 0);
      if (mixer) {
         //
         // Input sources choice
         //

         int numSources = Px_GetNumInputSources(mixer);
         if (numSources > 0) {
            wxString *sourceStrs = new wxString[numSources];
            for(j=0; j<numSources; j++)
               sourceStrs[j] = Px_GetInputSourceName(mixer, j);
            mInputSourceChoice = new wxChoice(this, 0,
                                              wxPoint(355, 2),
                                              wxSize(-1, 23),
                                              numSources, sourceStrs);
            delete[] sourceStrs;
            mInputSourceChoice->SetSelection(Px_GetCurrentInputSource(mixer));
         }
         
         //
         // Initial input/output volumes
         //

         mOutputSlider->Set(Px_GetPCMOutputVolume(mixer));
         mInputSlider->Set(Px_GetInputVolume(mixer));
         
         Px_CloseMixer(mixer);
      }
      Pa_CloseStream(stream);
   }

   #endif
}

MixerToolBar::~MixerToolBar()
{
   delete mPlayBitmap;
   delete mRecordBitmap;
   delete mInputSlider;
   delete mOutputSlider;
   if (mInputSourceChoice)
      delete mInputSourceChoice;
}

void MixerToolBar::RecreateTipWindows()
{
   // Hack to make sure they appear on top of other windows
   mInputSlider->RecreateTipWin();
   mOutputSlider->RecreateTipWin();
}

float MixerToolBar::GetInputVol()
{
   return mInputSlider->Get();
}

float MixerToolBar::GetOutputVol()
{
   return mOutputSlider->Get();
}

int MixerToolBar::GetInputSource()
{
   if (mInputSourceChoice)
      return mInputSourceChoice->GetSelection();
   else
      return 0;
}

void MixerToolBar::OnPaint(wxPaintEvent & evt)
{
   wxPaintDC dc(this);
   dc.DrawBitmap(*mPlayBitmap, 1, 1);
   dc.DrawBitmap(*mRecordBitmap, 181, 1);
}

void MixerToolBar::OnKeyEvent(wxKeyEvent & event)
{
   event.Skip();
}

void MixerToolBar::EnableDisableButtons()
{
}
