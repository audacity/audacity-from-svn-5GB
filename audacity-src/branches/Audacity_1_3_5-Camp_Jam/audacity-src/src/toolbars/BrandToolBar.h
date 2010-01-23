/**********************************************************************

  Audacity: A Digital Audio Editor

  BrandToolBar.h

  Copyright 2008 by Vaughan Johnson
  
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

**********************************************************************/

#include "../AudacityBranding.h"

#if WANT_BRAND_TOOLBAR

#ifndef __AUDACITY_BRAND_TOOLBAR__
#define __AUDACITY_BRAND_TOOLBAR__

#include "ToolBar.h"

class BrandToolBar : public ToolBar {
 public:
   BrandToolBar();
   virtual ~BrandToolBar();

   virtual void EnableDisableButtons() {};

 protected:
   virtual void Populate();
   virtual void Repaint(wxDC *dc) {};

 private:
   void OnButton_Logo(wxCommandEvent& event);
   void OnButton_Welcome(wxCommandEvent& event);

   AButton* mButton_Logo;
   wxButton* mButton_Welcome;

 public:
   DECLARE_CLASS(BrandToolBar);
   DECLARE_EVENT_TABLE();
};

#endif
#endif // WANT_BRAND_TOOLBAR
