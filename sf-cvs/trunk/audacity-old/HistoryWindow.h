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
#include <wx/listctrl.h>
#include <wx/stattext.h>


class AudacityProject;
class UndoManager;

class HistoryWindow :public wxDialog {

 public:
   HistoryWindow(AudacityProject * parent, UndoManager *manager);
   ~HistoryWindow();

   void UpdateDisplay();

 private:

   void OnDiscard(wxCommandEvent & event);
   void OnLabelChanged(wxListEvent & event);
   void OnItemSelected(wxListEvent & event);

   AudacityProject *mProject;
   
   wxBoxSizer *mTopSizer;
   wxListCtrl *mList;
   wxSpinCtrl *mDiscardNum;
   wxButton   *mDiscard;
   wxStaticText *mLevelsAvailable;
   UndoManager *mManager;

   int mSelected;

 public:
   DECLARE_EVENT_TABLE()
};

#endif
