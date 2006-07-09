/**********************************************************************

  Audacity: A Digital Audio Editor

  SelectionBar.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SELECTION_BAR__
#define __AUDACITY_SELECTION_BAR__

#include <wx/panel.h>

class wxBitmap;
class wxChoice;
class wxComboBox;
class wxRadioButton;
class wxMenu;

class TimeTextCtrl;

class SelectionBarListener {
 public:
   SelectionBarListener(){};
   virtual ~SelectionBarListener(){};
   virtual void AS_SetRate(double rate) = 0;
   virtual void AS_ModifySelection(double &start, double &end) = 0;
};

class SelectionBar : public wxPanel {
 public:

   SelectionBar(wxWindow * parent, wxWindowID id,
           const wxPoint & pos,
           const wxSize & size, double rate, SelectionBarListener * listener);

   virtual ~ SelectionBar();

   void SetTimes(double start, double end, double audio);
   double GetLeftTime();
   double GetRightTime();
   void SetField(const wxChar *msg, int fieldNum);
   void SetRate(double rate);

   // msmeyer: Call this to enable/disable menu items
   // in the "rate" menu, f.e. if sound card selection
   // has changed.
   void UpdateRates();

 private:

   void ValuesToControls();

   void OnFormatChoice(wxCommandEvent &evt);
   void OnLeftTime(wxCommandEvent &evt);
   void OnRightTime(wxCommandEvent &evt);

   void OnEndRadio(wxCommandEvent &evt);
   void OnLengthRadio(wxCommandEvent &evt);

   void OnRate(wxCommandEvent & event);

   void OnSize(wxSizeEvent &evt);

#if defined(__WXGTK__)
   void OnKeyDown(wxKeyEvent &evt);
#endif

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
   wxControl      *mFormatChoice;

   wxComboBox     *mRateBox;
   
   wxSizer        *mMainSizer;

 public:

   DECLARE_EVENT_TABLE()
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

