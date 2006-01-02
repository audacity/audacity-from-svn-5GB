/**********************************************************************

  Audacity: A Digital Audio Editor
  
  MeterToolbar.h
 
  Dominic Mazzoni
 
  ToolBar to hold the VU Meter

**********************************************************************/

#ifndef __AUDACITY_METER_TOOLBAR__
#define __AUDACITY_METER_TOOLBAR__

#include "ToolBar.h"


class Meter;
class wxGridBagSizer;

class MeterToolBar:public ToolBar {
public:
   MeterToolBar() {};
   MeterToolBar(wxWindow * parent);
   virtual ~ MeterToolBar();

   virtual void Populate();
   virtual void Repaint( wxDC *dc ) {};
   virtual void EnableDisableButtons() {};

   void GetMeters(Meter **playMeter, Meter **recordMeter)
   {
      *playMeter = mPlayMeter;
      *recordMeter = mRecordMeter;
   }

   virtual void OnSize(wxSizeEvent & event);


   DECLARE_EVENT_TABLE()
   ;
      
private:

   wxGridBagSizer *mSizer;
   Meter *mPlayMeter;
   Meter *mRecordMeter;
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
// arch-tag: 5a2a21f8-6c9e-45a4-8718-c26cad5cfe65
