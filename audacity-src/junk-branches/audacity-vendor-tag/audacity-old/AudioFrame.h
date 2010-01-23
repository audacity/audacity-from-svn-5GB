/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioFrame.h

  Dominic Mazzoni

**********************************************************************/

class AudioFrame: public wxDocParentFrame
{
  DECLARE_CLASS(AudioFrame)
public:
  wxMenu *editMenu;

  AudioFrame(wxDocManager *manager, wxFrame *frame,
			 wxWindowID id, const wxString& title,
			 const wxPoint& pos, const wxSize& size,
			 const long type);

  void OnAbout(wxCommandEvent& event);
  void OnQuit(wxCommandEvent& event);

  AudioFrame GetMainFrame();

  DECLARE_EVENT_TABLE()
};
