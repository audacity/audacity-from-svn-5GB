/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportRaw.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_IMPORT_RAW__
#define __AUDACITY_IMPORT_RAW__

#include <wx/dialog.h>
#include <wx/panel.h>

class wxRadioButton;
class WaveTrack;
class DirManager;

bool ImportRaw(wxWindow * parent,
               wxString fName, WaveTrack ** dest1, WaveTrack ** dest2,
               DirManager * dirManager);

bool GuessPCMFormat(wxString fName,
                    bool & guess16bit,
                    bool & guessSigned,
                    bool & guessStereo,
                    bool & guessBigEndian,
                    bool & guessOffset,
                    char **sampleData = 0, int *sampleDataLen = 0);

class PreviewPanel:public wxPanel {
 private:
   int bitWidth;
   int bitHeight;
   wxBitmap *bitmap;

   int dataLen;
   char *rawData;
   float *data1;
   float *data2;
   int len1;
   int len2;

 public:
   bool param[5];

   PreviewPanel(char *rawData, int dataLen, wxWindow * parent,
                 const wxPoint & pos, const wxSize & size,
                 const long style);
   ~PreviewPanel();

   void OnEraseBackground(wxEraseEvent & ignore);

   void OnPaint(wxPaintEvent & evt);

   DECLARE_EVENT_TABLE()
};

class ImportDialog:public wxDialog {
   DECLARE_DYNAMIC_CLASS(ImportDialog)
  public:
   wxRadioButton * bits[2];
   wxRadioButton *sign[2];
   wxRadioButton *stereo[2];
   wxRadioButton *endian[2];
   wxRadioButton *offset[2];

   PreviewPanel *preview;

   ImportDialog(char *data, int dataLen,
                 wxWindow * parent,
                 const wxPoint & pos = wxDefaultPosition);
   ~ImportDialog();

   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

   void RadioButtonPushed(wxCommandEvent & event);
   void RefreshPreview();

   DECLARE_EVENT_TABLE()
};

#endif
