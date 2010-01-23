/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioApp.h

  Dominic Mazzoni

**********************************************************************/

#include "wx/app.h"
#include "wx/docview.h"

class AudioFrame;

extern AudioFrame *GetMainFrame();

class AudioApp: public wxApp
{
public:
  AudioApp();
  ~AudioApp();

  virtual bool OnInit();
  int OnExit();

  virtual int MainLoop();

  wxFrame *CreateChildFrame(wxDocument *doc, wxView *view, bool isCanvas);

protected:
  wxDocManager* m_docManager;
};

DECLARE_APP(AudioApp)


