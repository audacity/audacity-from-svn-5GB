/**********************************************************************

  Audacity: A Digital Audio Editor

  HistoryWindow.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_HISTORY_WINDOW__
#define __AUDACITY_HISTORY_WINDOW__

#include <wx/dialog.h>
#include <wx/window.h>
#include <wx/event.h>
#include <wx/button.h>
#include <wx/notebook.h>
#include <wx/sizer.h>
#include <wx/spinctrl.h>

#include "UndoManager.h"

class HistoryWindow :public wxDialog {

 public:
   HistoryWindow(wxWindow * parent, UndoManager *manager);
   ~HistoryWindow();

   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
   wxStaticBoxSizer *mTopSizer;
   wxSpinCtrl *mDiscardNum;
   UndoManager *mManager;

 public:
   DECLARE_EVENT_TABLE()
};

#endif
