/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerToolbar.h
 
  Dominic Mazzoni
 
**********************************************************************/

#ifndef __AUDACITY_MIXER_TOOLBAR__
#define __AUDACITY_MIXER_TOOLBAR__

#include "ToolBar.h"

class MixerToolBar;
class ToolBar;
class ToolBarFrame;
class ASlider;

class wxImage;
class wxSize;
class wxPoint;
class wxChoice;

class MixerToolBar:public ToolBar {
 public:
   MixerToolBar() {};
   MixerToolBar(wxWindow * parent, wxWindowID id,
                const wxPoint & pos, const wxSize & size);
   MixerToolBar(wxWindow * parent);
   virtual ~ MixerToolBar();

   void RecreateTipWindows();
   void UpdateControls();
   void SetMixer(wxCommandEvent &event);

   virtual void OnPaint(wxPaintEvent & event);
   virtual void OnKeyEvent(wxKeyEvent & event);
   virtual void EnableDisableButtons();
   virtual void PlaceButton(int i, wxWindow *pWind);

   void OnSlider(wxCommandEvent & event);

 private:

   void InitializeMixerToolBar();

   wxBitmap *mPlayBitmap;
   wxBitmap *mRecordBitmap;

   wxChoice *mInputSourceChoice;

   ASlider *mInputSlider;
   ASlider *mOutputSlider;

   DECLARE_EVENT_TABLE()
};

MixerToolBar *GetCurrentMixerToolBar();

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
// arch-tag: 3acba542-52ae-44eb-b0b3-e0645587b5c0

