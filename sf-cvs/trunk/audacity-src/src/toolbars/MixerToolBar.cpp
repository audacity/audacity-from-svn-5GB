/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerToolBar.cpp

  Dominic Mazzoni
 
*******************************************************************//*!

\class MixerToolBar
\brief A ToolBar that provides the record and playback volume settings.

*//*******************************************************************/


#include "../Audacity.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/choice.h>
#include <wx/event.h>
#include <wx/intl.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/statbmp.h>
#include <wx/tooltip.h>
#endif

#include "MixerToolBar.h"

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../AudioIO.h"
#include "../ImageManipulation.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Theme.h"
#include "../widgets/ASlider.h"

IMPLEMENT_CLASS(MixerToolBar, ToolBar);

////////////////////////////////////////////////////////////
/// Methods for MixerToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(MixerToolBar, ToolBar)
   EVT_PAINT(MixerToolBar::OnPaint)
   EVT_SLIDER(wxID_ANY, MixerToolBar::SetMixer)
   EVT_SLIDER(wxID_ANY, MixerToolBar::SetMixer)
   EVT_CHOICE(wxID_ANY, MixerToolBar::SetMixer)
END_EVENT_TABLE()

//Standard contructor
MixerToolBar::MixerToolBar()
: ToolBar(MixerBarID, _("Mixer"), wxT("Mixer"))
{
}

MixerToolBar::~MixerToolBar()
{
   delete mPlayBitmap;
   delete mRecordBitmap;
}

void MixerToolBar::Create(wxWindow *parent)
{
   ToolBar::Create(parent);
}

void MixerToolBar::RecreateTipWindows()
{
   // Hack to make sure they appear on top of other windows
   mInputSlider->RecreateTipWin();
   mOutputSlider->RecreateTipWin();
}

void MixerToolBar::Populate()
{
   wxColour backgroundColour =
      wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE);

   mPlayBitmap = new wxBitmap(theTheme.Bitmap(bmpSpeaker));

   Add(new wxStaticBitmap(this,
                          wxID_ANY, 
                          *mPlayBitmap), 0, wxALIGN_CENTER);

   mOutputSlider = new ASlider(this, wxID_ANY, _("Output Volume"),
                               wxDefaultPosition, wxSize(130, 25));
   mOutputSlider->SetLabel(_("Slider-Output"));
   Add(mOutputSlider, 0, wxALIGN_CENTER);

   mRecordBitmap = new wxBitmap(theTheme.Bitmap(bmpMic));

   Add(new wxStaticBitmap(this,
                          wxID_ANY, 
                          *mRecordBitmap), 0, wxALIGN_CENTER);

   mInputSlider = new ASlider(this, wxID_ANY, _("Input Volume"),
                              wxDefaultPosition, wxSize(130, 25));
   mInputSlider->SetLabel(_("Slider-Input"));
   Add(mInputSlider, 0, wxALIGN_CENTER);

   mInputSourceChoice = NULL;

#if USE_PORTMIXER
   int leftPosition = 355;

   wxArrayString inputSources = gAudioIO->GetInputSourceNames();

   mInputSourceChoice = new wxChoice(this,
                                     wxID_ANY,
                                     wxDefaultPosition,
                                     wxDefaultSize,
                                     inputSources);
   mInputSourceChoice->SetName(_("Input Source"));
   Add(mInputSourceChoice, 0, wxALIGN_CENTER | wxLEFT, 2);

   // Set choice control to default value
   float inputVolume;
   float playbackVolume;
   int inputSource;
   gAudioIO->GetMixer(&inputSource, &inputVolume, &playbackVolume);
   mInputSourceChoice->SetSelection(inputSource);

   // Show or hide the control based on input sources
   mInputSourceChoice->Show( inputSources.GetCount() != 0 );

   UpdateControls();

#endif

   // Add a little space
   Add(2, -1);
}

void MixerToolBar::UpdatePrefs()
{
#if USE_PORTMIXER
   float inputVolume;
   float playbackVolume;
   int inputSource;

   wxArrayString inputSources = gAudioIO->GetInputSourceNames();

   // Repopulate the selections
   mInputSourceChoice->Clear();
   mInputSourceChoice->Append(inputSources);

   // Reset the selected source
   gAudioIO->GetMixer(&inputSource, &inputVolume, &playbackVolume);
   mInputSourceChoice->SetSelection(inputSource);

   // Resize the control
   mInputSourceChoice->SetSize(mInputSourceChoice->GetBestFittingSize());

   // Show or hide the control based on input sources
   mInputSourceChoice->Show( inputSources.GetCount() != 0 );

   // Layout the toolbar
   Layout();

   // Resize the toolbar to fit the contents
   Fit();

   // And make that size the minimum
   SetMinSize( wxWindow::GetSizer()->GetMinSize() );
   SetSize( GetMinSize() );

   // Notify someone that we've changed our size
   Updated();
#endif
}

void MixerToolBar::UpdateControls()
{
#if USE_PORTMIXER
   float inputVolume;
   float playbackVolume;
   int inputSource;

   gAudioIO->GetMixer(&inputSource, &inputVolume, &playbackVolume);

   // This causes weird GUI behavior and isn't really essential.
   // We could enable it again later.
   //if (inputSource != mInputSourceChoice->GetSelection())
   //    mInputSourceChoice->SetSelection(inputSource);

   mOutputSlider->Set(playbackVolume);
   mInputSlider->Set(inputVolume);
#endif // USE_PORTMIXER
}

void MixerToolBar::SetMixer(wxCommandEvent &event)
{
#if USE_PORTMIXER
   float inputVolume = mInputSlider->Get();
   float outputVolume = mOutputSlider->Get();
   int inputSource = mInputSourceChoice->GetSelection();

   gAudioIO->SetMixer(inputSource, inputVolume, outputVolume);
#endif // USE_PORTMIXER
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 6a50243e-9fc9-4f0f-b344-bd3044dc09ad

