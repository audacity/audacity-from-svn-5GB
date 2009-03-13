/**********************************************************************

  Audacity: A Digital Audio Editor

  AColor.cpp

  Dominic Mazzoni

  Manages color brushes and pens

**********************************************************************/

#include <wx/dc.h>
#include <wx/settings.h>
#include <wx/utils.h>

#include "AColor.h"

#if (AUDACITY_BRANDING == BRAND_UMIXIT)
   #include <wx/dynarray.h>
#endif

bool AColor::inited = false;

#if (AUDACITY_BRANDING == BRAND_THINKLABS)
   bool AColor::mWantDarkColorScheme = true; // Start with Dark Color Scheme.
#endif

wxBrush AColor::lightBrush[2];
wxBrush AColor::mediumBrush[2];
wxBrush AColor::darkBrush[2];
wxPen AColor::lightPen[2];
wxPen AColor::mediumPen[2];
wxPen AColor::darkPen[2];

wxPen AColor::cursorPen;
wxBrush AColor::indicatorBrush[3]; 
wxPen AColor::indicatorPen[3];

wxBrush AColor::muteBrush[2];
wxBrush AColor::soloBrush;

#if (AUDACITY_BRANDING == BRAND_UMIXIT)
   // For UmixIt, need color brush for off positions, too.
   wxBrush AColor::muteBrushOff;
   wxBrush AColor::soloBrushOff;
#endif

wxBrush AColor::envelopeBrush;
wxPen AColor::envelopePen;
wxPen AColor::WideEnvelopePen;

wxBrush AColor::labelFlagBrush;
wxBrush AColor::labelUnselectedBrush;
wxBrush AColor::labelSelectedBrush;
wxPen AColor::labelFlagPen;
wxPen AColor::labelUnselectedPen;
wxPen AColor::labelSelectedPen;

wxBrush AColor::tooltipBrush;

wxFont AColor::labelFont;

void AColor::SetLabelFont(wxDC & dc)
{
  if (!inited)
    Init(&dc);
  
  dc.SetFont(labelFont);
}

void AColor::Bevel(wxDC & dc, bool up, wxRect & r)
{
   if (up)
      AColor::Light(&dc, false);
   else
      AColor::Dark(&dc, false);

   dc.DrawLine(r.x, r.y, r.x + r.width, r.y);
   dc.DrawLine(r.x, r.y, r.x, r.y + r.height);

   if (!up)
      AColor::Light(&dc, false);
   else
      AColor::Dark(&dc, false);

   dc.DrawLine(r.x + r.width, r.y, r.x + r.width, r.y + r.height);
   dc.DrawLine(r.x, r.y + r.height, r.x + r.width + 1, r.y + r.height);
}

void AColor::Light(wxDC * dc, bool selected)
{
   if (!inited)
      Init(dc);
   int index = (int) selected;
   dc->SetBrush(lightBrush[index]);
   dc->SetPen(lightPen[index]);
}

void AColor::Medium(wxDC * dc, bool selected)
{
   if (!inited)
      Init(dc);
   int index = (int) selected;
   dc->SetBrush(mediumBrush[index]);
   dc->SetPen(mediumPen[index]);
}

void AColor::Dark(wxDC * dc, bool selected)
{
   if (!inited)
      Init(dc);
   int index = (int) selected;
   dc->SetBrush(darkBrush[index]);
   dc->SetPen(darkPen[index]);
}

void AColor::CursorColor(wxDC * dc)
{
   if (!inited)
      Init(dc);
   dc->SetLogicalFunction(wxINVERT);
   dc->SetPen(cursorPen);
}

void AColor::IndicatorColor(wxDC * dc, bool recording, bool bIsPaused)
{
   if (!inited)
      Init(dc);
   int index = 0; // playing
   if (recording)
      index = 1;
   else if (bIsPaused)
      index = 2;
   dc->SetPen(indicatorPen[index]);
   dc->SetBrush(indicatorBrush[index]);
}

void AColor::Mute(wxDC * dc, bool on, bool selected, bool soloing)
{
   if (!inited)
      Init(dc);
   int index = (int) selected;
   if (on) {
      dc->SetPen(*wxBLACK_PEN);
      dc->SetBrush(muteBrush[(int) soloing]);
   }
   else {
      dc->SetPen(*wxTRANSPARENT_PEN);
      #if (AUDACITY_BRANDING == BRAND_UMIXIT)
         dc->SetBrush(muteBrushOff);
      #else
         dc->SetBrush(mediumBrush[index]);
      #endif
   }
}

void AColor::Solo(wxDC * dc, bool on, bool selected)
{
   if (!inited)
      Init(dc);
   int index = (int) selected;
   if (on) {
      dc->SetPen(*wxBLACK_PEN);
      dc->SetBrush(soloBrush);
   }
   else {
      dc->SetPen(*wxTRANSPARENT_PEN);
      #if (AUDACITY_BRANDING == BRAND_UMIXIT)
         dc->SetBrush(soloBrushOff); 
      #else
         dc->SetBrush(mediumBrush[index]);
      #endif
   }
}

void AColor::Init(wxDC * dc)
{
   if (inited)
      return;

   int fontSize = 4;
   wxCoord strW, strH;
   wxString exampleText = wxT("Mute");
   int desiredPixelHeight = 16;

   #ifdef __WXMAC__
   desiredPixelHeight -= 3;
   #endif
   
   #ifdef __WXMSW__
   desiredPixelHeight -= 2;
   #endif
   
   // Keep making the font bigger until it's too big, then subtract one.
   dc->SetFont(wxFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL));
   dc->GetTextExtent(exampleText, &strW, &strH);
   while(strH <= desiredPixelHeight && fontSize < 40) {
     fontSize++;
     dc->SetFont(wxFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL));
     dc->GetTextExtent(exampleText, &strW, &strH);
   }
   fontSize--;

   labelFont = wxFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);

   wxColour light =
       wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DHIGHLIGHT);
   wxColour med = wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   wxColour dark =
       wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DSHADOW);

   envelopePen.SetColour(110, 110, 220);
   WideEnvelopePen.SetColour(110, 110, 220);
   envelopeBrush.SetColour(110, 110, 220);
   WideEnvelopePen.SetWidth( 3 );

   labelFlagBrush.SetColour(204, 0, 0);
   labelUnselectedBrush.SetColour(192, 192, 192);
   labelSelectedBrush.SetColour(148, 148, 170);

   labelFlagPen.SetColour(204, 0, 0);
   labelUnselectedPen.SetColour(192, 192, 192);
   labelSelectedPen.SetColour(148, 148, 170);

   // muteBrush[1] is used when solo is on, since solo overrides mute.
   #if (AUDACITY_BRANDING == BRAND_UMIXIT)
      // UmixIt mute & solo colors
      muteBrush[0].SetColour(220, 220, 0); // darker yellow
      muteBrush[1].SetColour(200, 200, 0); // dark yellow
      soloBrush.SetColour(0, 220, 0); // darker green

      muteBrushOff.SetColour(255, 255, 0); // yellow
      soloBrushOff.SetColour(0, 255, 0); // green
   #elif (AUDACITY_BRANDING == BRAND_THINKLABS)
      // no Mute buttons for Thinklabs
      soloBrush.SetColour(0, 255, 255); // cyan
   #else
      //   muteBrush[0].SetColour(110, 220, 110);
      //   muteBrush[1].SetColour(170, 180, 170);
      //   soloBrush.SetColour(255, 140, 140);
      // Colors modified to avoid using reserved colors red and green.
      muteBrush[0].SetColour(160, 170, 210);
      muteBrush[1].SetColour(160, 170, 190);
      soloBrush.SetColour(160, 170, 210);
   #endif

   cursorPen.SetColour(0, 0, 0);
//   indicatorPen[0].SetColour(255, 0, 51); //recording
//   indicatorPen[1].SetColour(0, 255, 51); //playback
   indicatorPen[0].SetColour( 36,  96,  46); // playback
   indicatorPen[1].SetColour(176,   0,  28); // recording
   indicatorPen[2].SetColour(176, 176,  28); // paused

   indicatorBrush[0].SetColour( 28, 171,  51); // playback
   indicatorBrush[1].SetColour(190, 129, 129); // recording
   indicatorBrush[2].SetColour(255, 255,   0); // paused

//Determine tooltip color
//TODO: Find out why the commented out version yields black.
//   tooltipBrush.SetColour( wxSystemSettings::GetSystemColour(wxSYS_COLOUR_INFOTEXT) );
   tooltipBrush.SetColour( 250,250,215); //pale shade of yellow, to match MS Windows tooltips

#if defined(__WXMSW__) || defined(__WXGTK__)
   // unselected
   lightBrush[0].SetColour(light);
   mediumBrush[0].SetColour(med);
   darkBrush[0].SetColour(dark);
   lightPen[0].SetColour(light);
   mediumPen[0].SetColour(med);
   darkPen[0].SetColour(dark);

   // selected
   lightBrush[1].SetColour(204, 204, 255);
   mediumBrush[1].SetColour(200, 200, 214);
   darkBrush[1].SetColour(148, 148, 170);
   lightPen[1].SetColour(204, 204, 255);
   mediumPen[1].SetColour(200, 200, 214);
   darkPen[1].SetColour(0, 0, 0);

#else

#if defined(__WXMAC__)          // && defined(TARGET_CARBON)

   // unselected
   lightBrush[0].SetColour(246, 246, 255);
   mediumBrush[0].SetColour(220, 220, 220);
   darkBrush[0].SetColour(140, 140, 160);
   lightPen[0].SetColour(246, 246, 255);
   mediumPen[0].SetColour(220, 220, 220);
   darkPen[0].SetColour(140, 140, 160);

   // selected
   lightBrush[1].SetColour(204, 204, 255);
   mediumBrush[1].SetColour(180, 180, 192);
   darkBrush[1].SetColour(148, 148, 170);
   lightPen[1].SetColour(204, 204, 255);
   mediumPen[1].SetColour(180, 180, 192);
   darkPen[1].SetColour(148, 148, 170);

#else

   // unselected
   lightBrush[0].SetColour(255, 255, 255);
   mediumBrush[0].SetColour(204, 204, 204);
   darkBrush[0].SetColour(130, 130, 130);
   lightPen[0].SetColour(255, 255, 255);
   mediumPen[0].SetColour(204, 204, 204);
   darkPen[0].SetColour(130, 130, 130);

   // selected
   lightBrush[1].SetColour(204, 204, 255);
   mediumBrush[1].SetColour(180, 180, 192);
   darkBrush[1].SetColour(148, 148, 170);
   lightPen[1].SetColour(204, 204, 255);
   mediumPen[1].SetColour(180, 180, 192);
   darkPen[1].SetColour(0, 0, 0);

#endif

#endif

   inited = true;
}

const int AColor_midicolors[16][3] = {
   {255, 102, 102},             // 1=salmon
   {204, 0, 0},                 // 2=red
   {255, 117, 23},              // 3=orange
   {255, 255, 0},               // 4=yellow    
   {0, 204, 0},                 // 5=green
   {0, 204, 204},               // 6=turquoise
   {0, 0, 204},                 // 7=blue
   {153, 0, 255},               // 8=blue-violet

   {140, 97, 54},               // 9=brown
   {120, 120, 120},             // 10=gray (drums)
   {255, 175, 40},              // 11=lt orange
   {102, 255, 102},             // 12=lt green
   {153, 255, 255},             // 13=lt turquoise
   {153, 153, 255},             // 14=lt blue
   {204, 102, 255},             // 15=lt blue-violet
   {255, 51, 204}
};                              // 16=lt red-violet

void AColor::MIDIChannel(wxDC * dc, int channel /* 1 - 16 */ )
{
   if (channel >= 1 && channel <= 16) {
      const int *colors = AColor_midicolors[channel - 1];

      dc->SetPen(wxPen(wxColour(colors[0],
                                colors[1], colors[2]), 1, wxSOLID));
      dc->SetBrush(wxBrush(wxColour(colors[0],
                                    colors[1], colors[2]), wxSOLID));
   } else {
      dc->SetPen(wxPen(wxColour(153, 153, 153), 1, wxSOLID));
      dc->SetBrush(wxBrush(wxColour(153, 153, 153), wxSOLID));
   }

}

void AColor::LightMIDIChannel(wxDC * dc, int channel /* 1 - 16 */ )
{
   if (channel >= 1 && channel <= 16) {
      const int *colors = AColor_midicolors[channel - 1];

      dc->SetPen(wxPen(wxColour(127 + colors[0] / 2,
                                127 + colors[1] / 2,
                                127 + colors[2] / 2), 1, wxSOLID));
      dc->SetBrush(wxBrush(wxColour(127 + colors[0] / 2,
                                    127 + colors[1] / 2,
                                    127 + colors[2] / 2), wxSOLID));
   } else {
      dc->SetPen(wxPen(wxColour(204, 204, 204), 1, wxSOLID));
      dc->SetBrush(wxBrush(wxColour(204, 204, 204), wxSOLID));
   }

}

void AColor::DarkMIDIChannel(wxDC * dc, int channel /* 1 - 16 */ )
{
   if (channel >= 1 && channel <= 16) {
      const int *colors = AColor_midicolors[channel - 1];

      dc->SetPen(wxPen(wxColour(colors[0] / 2,
                                colors[1] / 2,
                                colors[2] / 2), 1, wxSOLID));
      dc->SetBrush(wxBrush(wxColour(colors[0] / 2,
                                    colors[1] / 2,
                                    colors[2] / 2), wxSOLID));
   } else {
      dc->SetPen(wxPen(wxColour(102, 102, 102), 1, wxSOLID));
      dc->SetBrush(wxBrush(wxColour(102, 102, 102), wxSOLID));
   }

}

#if (AUDACITY_BRANDING == BRAND_UMIXIT)
   WX_DEFINE_ARRAY(void*, trackPtrsArray);
   const wxColour gRed = wxColour(255, 130, 140); // red
   const wxColour gOrange = wxColour(255, 200, 130); // orange
   const wxColour gYellow = wxColour(255, 240, 120); // yellow
   const wxColour gGreen = wxColour(120, 255, 120); // green
   const wxColour gCyan = wxColour(120, 255, 255); // cyan
   const wxColour gBlue = wxColour(180, 200, 255); // blue
   const wxColour gPurple = wxColour(255, 180, 255); // purple

   // rainbow pastel color based on track's pointer -- so it's unique to track
   wxColour AColor::GetTrackColor(void* pTrack) 
   { 
      static trackPtrsArray trackPtrs; 
      int nCase = trackPtrs.Index(pTrack);
      if (nCase == wxNOT_FOUND) trackPtrs.Add(pTrack);
      nCase = trackPtrs.Index(pTrack);
      switch (nCase % 7) { 
         case 0: return gRed; 
         case 1: return gOrange;
         case 2: return gYellow;
         case 3: return gGreen;
         case 4: return gCyan;
         case 5: return gBlue;
         case 6: return gPurple;
      }
      return gRed;
   }
#endif

void GetColorGradient(float value,
                      bool selected,
                      bool grayscale,
                      unsigned char *red,
                      unsigned char *green, unsigned char *blue)
{
   float r, g, b;

   if (grayscale) {
      r = g = b = 0.84 - 0.84 * value;
   } else {
      const int gsteps = 4;
      float gradient[gsteps + 1][3] = {
         {float(0.75), float(0.75), float(0.75)},    // lt gray
         {float(0.30), float(0.60), float(1.00)},    // lt blue
         {float(0.90), float(0.10), float(0.90)},    // violet
         {float(1.00), float(0.00), float(0.00)},    // red
         {float(1.00), float(1.00), float(1.00)}     // white
      };                        

      int left = int (value * gsteps);
      int right = (left == gsteps ? gsteps : left + 1);

      float rweight = (value * gsteps) - left;
      float lweight = 1.0 - rweight;

      r = (gradient[left][0] * lweight) + (gradient[right][0] * rweight);
      g = (gradient[left][1] * lweight) + (gradient[right][1] * rweight);
      b = (gradient[left][2] * lweight) + (gradient[right][2] * rweight);
   }

   if (selected) {
      r *= 0.77f;
      g *= 0.77f;
      b *= 0.885f;
   }

   *red = (unsigned char) (255 * r);
   *green = (unsigned char) (255 * g);
   *blue = (unsigned char) (255 * b);
}
