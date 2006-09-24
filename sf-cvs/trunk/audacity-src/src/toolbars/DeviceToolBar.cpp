/**********************************************************************

  Audacity: A Digital Audio Editor

  DeviceToolBar.cpp

  Dominic Mazzoni
 
*******************************************************************//*!

\class DeviceToolBar
\brief A toobar to allow easier changing of input and output devices .

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

#include "DeviceToolBar.h"

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../AudioIO.h"
#include "../ImageManipulation.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Theme.h"

IMPLEMENT_CLASS(DeviceToolBar, ToolBar);

////////////////////////////////////////////////////////////
/// Methods for DeviceToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(DeviceToolBar, ToolBar)
   EVT_CHOICE(wxID_ANY, DeviceToolBar::OnChoice)
END_EVENT_TABLE()

//Standard contructor
DeviceToolBar::DeviceToolBar()
: ToolBar(DeviceBarID, _("Device"))
{
}

DeviceToolBar::~DeviceToolBar()
{
   delete mPlayBitmap;
   delete mRecordBitmap;
}

void DeviceToolBar::Create(wxWindow *parent)
{
   ToolBar::Create(parent);
}

void DeviceToolBar::RecreateTipWindows()
{
}

void DeviceToolBar::Populate()
{
   int i;
   wxArrayString inputs;
   wxArrayString outputs;

#if USE_PORTAUDIO_V19
   int nDevices = Pa_GetDeviceCount();
#else
   int nDevices = Pa_CountDevices();
#endif

   for (i = 0; i < nDevices; i++) {
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      wxString name = DeviceName(info);

      if (info->maxOutputChannels > 0) {
         outputs.Add(name);
      }

      if (info->maxInputChannels > 0) {
         inputs.Add(name);
      }
   }

   wxColour backgroundColour =
      wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE);

   // Output device
   mPlayBitmap = new wxBitmap(theTheme.Bitmap(bmpSpeaker));

   Add(new wxStaticBitmap(this,
                          wxID_ANY, 
                          *mPlayBitmap), 0, wxALIGN_CENTER);

   mOutput = new wxChoice(this,
                               wxID_ANY,
                               wxDefaultPosition,
                               wxDefaultSize,
                               outputs);
   mOutput->SetName(_("Output Device"));
#if wxUSE_TOOLTIPS
   mOutput->SetToolTip(_("Output Device"));
#endif
   Add(mOutput, 0, wxALIGN_CENTER);

   if (outputs.GetCount() == 0)
      mOutput->Enable(false);

   // Input device
   mRecordBitmap = new wxBitmap(theTheme.Bitmap(bmpMic));

   Add(new wxStaticBitmap(this,
                          wxID_ANY, 
                          *mRecordBitmap), 0, wxALIGN_CENTER);

   mInput = new wxChoice(this,
                         wxID_ANY,
                         wxDefaultPosition,
                         wxDefaultSize,
                         inputs);
   mInput->SetName(_("Input Device"));
#if wxUSE_TOOLTIPS
   mInput->SetToolTip(_("Input Device"));
#endif
   Add(mInput, 0, wxALIGN_CENTER);

   if (inputs.GetCount() == 0)
      mInput->Enable(false);

   UpdatePrefs();
}

void DeviceToolBar::UpdatePrefs()
{
   mInput->SetStringSelection(gPrefs->Read(wxT("/AudioIO/RecordingDevice"), wxT("")));

   mOutput->SetStringSelection(gPrefs->Read(wxT("/AudioIO/PlaybackDevice"), wxT("")));
}

void DeviceToolBar::OnChoice(wxCommandEvent &event)
{
   gPrefs->Write(wxT("/AudioIO/RecordingDevice"),
                 mInput->GetString(mInput->GetSelection()));

   gPrefs->Write(wxT("/AudioIO/PlaybackDevice"),
                 mOutput->GetString(mOutput->GetSelection()));

   if (gAudioIO)
      gAudioIO->HandleDeviceChange();

   GetActiveProject()->UpdatePrefs();
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

