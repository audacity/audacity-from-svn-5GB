/**********************************************************************

  Audacity: A Digital Audio Editor

  AStatus.cpp

  Dominic Mazzoni

**********************************************************************/

#include "AStatus.h"

#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/intl.h>
#include <wx/gdicmn.h>
#include <wx/menu.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>

#include "AColor.h"

int GetStatusHeight()
{
   return 55;
}

enum {
   AStatusFirstID = 2700,

   OnRate8ID,
   OnRate11ID,
   OnRate16ID,
   OnRate22ID,
   OnRate44ID,
   OnRate48ID,
   OnRateOtherID
};

BEGIN_EVENT_TABLE(AStatus, wxWindow)
    EVT_MOUSE_EVENTS(AStatus::OnMouseEvent)
    EVT_PAINT(AStatus::OnPaint)

    EVT_MENU(OnRate8ID, AStatus::OnRate8)
    EVT_MENU(OnRate11ID, AStatus::OnRate11)
    EVT_MENU(OnRate16ID, AStatus::OnRate16)
    EVT_MENU(OnRate22ID, AStatus::OnRate22)
    EVT_MENU(OnRate44ID, AStatus::OnRate44)
    EVT_MENU(OnRate48ID, AStatus::OnRate48)
    EVT_MENU(OnRateOtherID, AStatus::OnRateOther)
    END_EVENT_TABLE()

AStatus::AStatus(wxWindow * parent, wxWindowID id,
                     const wxPoint & pos,
                     const wxSize & size,
                     double rate,
                     AStatusListener * listener):wxWindow(parent, id, pos,
                                                          size),
mListener(listener), mBitmap(NULL), mRate(rate)
{
   GetSize(&mWidth, &mHeight);

   mRateMenu = new wxMenu();
   mRateMenu->AppendCheckItem(OnRate8ID, "8000 Hz");
   mRateMenu->AppendCheckItem(OnRate11ID, "11025 Hz");
   mRateMenu->AppendCheckItem(OnRate16ID, "16000 Hz");
   mRateMenu->AppendCheckItem(OnRate22ID, "22050 Hz");
   mRateMenu->AppendCheckItem(OnRate44ID, "44100 Hz");
   mRateMenu->AppendCheckItem(OnRate48ID, "48000 Hz");
   mRateMenu->AppendCheckItem(OnRateOtherID, _("Other..."));

   mRateField.x = 0;
   mRateField.y = 0;
   mRateField.width = 0;
   mRateField.height = 0;
}

AStatus::~AStatus()
{
   if (mBitmap)
      delete mBitmap;

   delete mRateMenu;
}

void AStatus::SetField(const char *msg, int fieldNum)
{
   if (fieldNum < 0 || fieldNum >= 10)
      return;

   if (mField[fieldNum] != msg) {
      mField[fieldNum] = msg;
      Refresh(false);
   }
}

void AStatus::SetRate(double rate)
{
   if (rate != mRate) {
      mRate = rate;
      Refresh(false);
   }
}

void AStatus::OnPaint(wxPaintEvent & event)
{
   wxPaintDC dc(this);

   int width, height;
   GetSize(&width, &height);

   if (width != mWidth || height != mHeight) {
      mWidth = width;
      mHeight = height;

      delete mBitmap;
      mBitmap = NULL;
   }

   if (!mBitmap)
      mBitmap = new wxBitmap(mWidth, mHeight);

   wxMemoryDC memDC;
   memDC.SelectObject(*mBitmap);

   AColor::Medium(&memDC, false);
   memDC.DrawRectangle(0, 0, mWidth, mHeight);

   int fontSize = 10;
#ifdef __WXMSW__
   fontSize = 8;
#endif

   memDC.SetPen(*wxBLACK_PEN);
   memDC.DrawLine(0, 0, mWidth, 0);

   wxRect outline;
   outline.x = 0;
   outline.y = 1;
   outline.width = mWidth - 1;
   outline.height = mHeight - 2;
   AColor::Bevel(memDC, true, outline);

   wxFont statusFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);
   memDC.SetFont(statusFont);

   wxRect msgField;
   msgField.x = 4;
   msgField.y = 6;
   msgField.width = mWidth - 8;
   msgField.height = 17;

   AColor::Bevel(memDC, false, msgField);
   wxString msg = mField[0];
   long textWidth, textHeight;
   memDC.GetTextExtent(msg, &textWidth, &textHeight);
   while (msg != "" && textWidth > msgField.width) {
      msg = msg.Left(msg.Length() - 1);
      memDC.GetTextExtent(msg, &textWidth, &textHeight);
   }
   memDC.DrawText(msg, msgField.x + 3, msgField.y + 2);

   wxRect cursorField;
   cursorField.x = 140;
   cursorField.y = 29;
   cursorField.width = mWidth - 144;
   cursorField.height = 17;

#ifdef __WXMAC__
   cursorField.width -= 15;
#endif

   AColor::Bevel(memDC, false, cursorField);
   msg = mField[1];
   memDC.GetTextExtent(msg, &textWidth, &textHeight);
   while (msg != "" && textWidth > cursorField.width) {
      msg = msg.Left(msg.Length() - 1);
      memDC.GetTextExtent(msg, &textWidth, &textHeight);
   }
   memDC.DrawText(msg, cursorField.x + 3, cursorField.y + 2);

   mRateField.x = 80;
   mRateField.y = 29;
   mRateField.width = 50;
   mRateField.height = 17;
   AColor::Bevel(memDC, true, mRateField);

   memDC.DrawText(_("Project rate:"), 3, mRateField.y + 2);

   memDC.DrawText(wxString::Format("%d", int (mRate + 0.5)),
                  mRateField.x + 3, mRateField.y + 2);

   dc.Blit(0, 0, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE);

}

void AStatus::OnMouseEvent(wxMouseEvent & event)
{
   if (event.ButtonDown() && mRateField.Inside(event.m_x, event.m_y)) {

      {
         wxClientDC dc(this);
         AColor::Bevel(dc, false, mRateField);
      }

      PopupMenu(mRateMenu, mRateField.x, mRateField.y + mRateField.height);

      {
         wxClientDC dc(this);
         AColor::Bevel(dc, true, mRateField);
      }
   }
}

void AStatus::OnRate8(wxCommandEvent & WXUNUSED(event))
{
   mRate = 8000.0;
   mListener->AS_SetRate(mRate);
   Refresh(false);
}

void AStatus::OnRate11(wxCommandEvent & WXUNUSED(event))
{
   mRate = 11025.0;
   mListener->AS_SetRate(mRate);
   Refresh(false);
}

void AStatus::OnRate16(wxCommandEvent & WXUNUSED(event))
{
   mRate = 16000.0;
   mListener->AS_SetRate(mRate);
   Refresh(false);
}

void AStatus::OnRate22(wxCommandEvent & WXUNUSED(event))
{
   mRate = 22050.0;
   mListener->AS_SetRate(mRate);
   Refresh(false);
}

void AStatus::OnRate44(wxCommandEvent & WXUNUSED(event))
{
   mRate = 44100.0;
   mListener->AS_SetRate(mRate);
   Refresh(false);
}

void AStatus::OnRate48(wxCommandEvent & WXUNUSED(event))
{
   mRate = 48000.0;
   mListener->AS_SetRate(mRate);
   Refresh(false);
}

void AStatus::OnRateOther(wxCommandEvent & WXUNUSED(event))
{
   wxString defaultStr;
   defaultStr.Printf("%d", (int) (mRate + 0.5));
   wxString rateStr =
       wxGetTextFromUser(_("Enter a rate in Hz (samples per second):"),
                         _("Set Rate"),
                         defaultStr);

   if (rateStr != "") {
      double theRate;
      if (rateStr.ToDouble(&theRate) && theRate >= 1 && theRate <= 100000) {
         mRate = theRate;
         mListener->AS_SetRate(mRate);
         Refresh(false);
      } else
         wxMessageBox(_("Invalid rate."));
   }
}
