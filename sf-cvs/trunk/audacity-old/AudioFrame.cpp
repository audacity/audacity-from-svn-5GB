/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioFrame.cpp

  Dominic Mazzoni

**********************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include "wx/wx.h"
#endif

#include "AudioView.h"
#include "AudioFrame.h"
#include "AboutDialog.h"

IMPLEMENT_CLASS(AudioFrame, wxDocParentFrame)

BEGIN_EVENT_TABLE(AudioFrame, wxDocParentFrame)
    EVT_MENU(wxID_ABOUT, AudioFrame::OnAbout)
END_EVENT_TABLE()

AudioFrame::AudioFrame(wxDocManager *manager, wxFrame *frame, wxWindowID id, const wxString& title,
    const wxPoint& pos, const wxSize& size, const long type):
  wxDocParentFrame(manager, frame, id, title, pos, size, type)
{
  editMenu = (wxMenu *) NULL;
}

void AudioFrame::OnQuit(wxCommandEvent& WXUNUSED(event))
{
  Close(TRUE);
}

void AudioFrame::OnAbout(wxCommandEvent& WXUNUSED(event))
{
  wxBitmap *logo;
  
  wxString fullMessage("Audacity:\n"
		       "A New Digital Audio Editor\n"
		       "by Dominic Mazzoni, Roger Dannenberg, "
		       "Jason Cohen, and Robert Leidle.\n"
		       "Version 0.8: May 28, 2000\n"
		       "http://www.cs.cmu.edu/~music/audacity/");
  
  wxString caption("A New Digital Audio Editor\n"
		   "by Dominic Mazzoni, Roger Dannenberg, "
		   "Jason Cohen, and Robert Leidle.\n"
		   "Version 0.8: May 28, 2000\n"
		   "http://www.cs.cmu.edu/~music/audacity/");
  
  logo = new wxBitmap();
  
#ifdef __WXMSW__
  if (logo->LoadFile("AudacityLogo",wxBITMAP_TYPE_BMP_RESOURCE)) {
#else
#ifdef __WXMAC__
  if (logo->LoadFile("AudacityLogo",wxBITMAP_TYPE_PICT_RESOURCE)) {
#else
  if (logo->LoadFile("icons/AudacityLogo.XPM",wxBITMAP_TYPE_XPM)) {
#endif
#endif
    AboutDialog about((wxWindow *)0, logo, caption);
    about.ShowModal();
    
  }
  else {
    wxMessageBox(fullMessage, "About Audacity",
		 wxOK | wxICON_INFORMATION );
  }
  
  delete logo;
}
  
      
