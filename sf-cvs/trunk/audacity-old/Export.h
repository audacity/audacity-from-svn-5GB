/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.h

  Dominic Mazzoni

**********************************************************************/

#ifndef _EXPORT_
#define _EXPORT_

class WaveTrack;
class DirManager;

class ExportDialog: public wxDialog {
DECLARE_DYNAMIC_CLASS(ExportDialog)

public:
  wxChoice *headerChoice;
  wxRadioButton *bitsButton[2];
  wxRadioButton *signButton[2];
  
  ExportDialog(wxWindow *parent,
			   const wxPoint& pos = wxDefaultPosition);
  
  void OnOK(wxCommandEvent& event);
  void OnCancel(wxCommandEvent& event);

DECLARE_EVENT_TABLE()
};

bool Export(WaveTrack *src1, WaveTrack *src2);

#endif
