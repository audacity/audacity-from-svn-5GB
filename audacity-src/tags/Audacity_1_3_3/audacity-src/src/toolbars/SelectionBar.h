/**********************************************************************

  Audacity: A Digital Audio Editor

  SelectionBar.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SELECTION_BAR__
#define __AUDACITY_SELECTION_BAR__

#include <wx/defs.h>

#include "ToolBar.h"

class wxBitmap;
class wxComboBox;
class wxCommandEvent;
class wxDC;
class wxRadioButton;
class wxSizeEvent;

class TimeTextCtrl;

class SelectionBarListener {

 public:

   SelectionBarListener(){};
   virtual ~SelectionBarListener(){};

   virtual void AS_SetRate(double rate) = 0;
   virtual void AS_ModifySelection(double &start, double &end) = 0;
};

class SelectionBar:public ToolBar {

 public:

   SelectionBar();
   virtual ~SelectionBar();

   void Create(wxWindow *parent);

   virtual void Populate();
   virtual void Repaint(wxDC *dc) {};
   virtual void EnableDisableButtons() {};

   void SetTimes(double start, double end, double audio);
   double GetLeftTime();
   double GetRightTime();
   void SetField(const wxChar *msg, int fieldNum);
   void SetRate(double rate);
   void SetListener(SelectionBarListener *l) {mListener = l;};

   // msmeyer: Call this to enable/disable menu items
   // in the "rate" menu, f.e. if sound card selection
   // has changed.
   void UpdateRates();

 private:

   void ValuesToControls();
   void OnUpdate(wxCommandEvent &evt);
   void OnLeftTime(wxCommandEvent &evt);
   void OnRightTime(wxCommandEvent &evt);

   void OnEndRadio(wxCommandEvent &evt);
   void OnLengthRadio(wxCommandEvent &evt);

   void OnRate(wxCommandEvent & event);
   void OnRateFocus(wxFocusEvent &event);
   void OnRateCaptureKey(wxCommandEvent &event);

   void OnSize(wxSizeEvent &evt);

   void ModifySelection();

   SelectionBarListener * mListener;
   double mRate;
   double mStart, mEnd, mAudio;
   wxString mField[10];
   bool mModifyingSelection;

   TimeTextCtrl   *mLeftTime;
   TimeTextCtrl   *mRightTime;
   wxRadioButton  *mRightEndButton;
   wxRadioButton  *mRightLengthButton;
   TimeTextCtrl   *mAudioTime;

   wxComboBox     *mRateBox;

 public:

   DECLARE_CLASS(SelectionBar);
   DECLARE_EVENT_TABLE();
};

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 441b2b59-b3aa-4d30-bed2-7377cef491ab

