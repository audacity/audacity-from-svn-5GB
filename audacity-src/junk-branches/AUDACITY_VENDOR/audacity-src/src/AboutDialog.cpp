/**********************************************************************

  Audacity: A Digital Audio Editor

  AboutDialog.cpp

  Dominic Mazzoni

**********************************************************************/

#include "wx/dialog.h"
#include "wx/html/htmlwin.h"

#include "AboutDialog.h"
#include "Audacity.h"

#include "../images/AudacityLogo.xpm"

#ifdef __WXMSW__
#define DLOG_HEIGHT 430
#else
#define DLOG_HEIGHT 400
#endif

const char *creditText =
  "<html>"
  "<body bgcolor=\"#ffffff\">"
  "<font size=1>"
  "<center>"
  "<h3>Audacity " AUDACITY_VERSION_STRING "</h3>"
  "A New Digital Audio Editor"
  "</center>"
  "<p>"
  "Audacity is a free program being written by a team of developers using "
  "Sourceforge, an online service for open-source projects.  "
  "It is based on the wxWindows toolkit and is available for "
  "Windows, MacOS, and Linux."
  "<p>"
  "This program is still in beta!  Many people find it very useful "
  "and stable enough for everyday use, but it is not finished and "
  "comes with no guarantee!  We depend on your feedback, so "
  "please visit our website and give us your bug reports and "
  "feature requests."
  "<p>"
  "http://audacity.sourceforge.net/"
  "<p>"
  "<center><b>Credits</b></center>"
  "<p>"
  "<table border=0>"
  "<tr>"
  "<td>Dominic Mazzoni</td>"
  "<td>Project leader and primary programmer</td>"
  "</tr>"
  "<tr>"
  "<td>Joshua Haberman</td>"
  "<td>Preferences dialog, MP3 importing and exporting, and general development</td>"
  "</tr>"
  "<tr>"
  "<td>Roger Dannenberg</td>"
  "<td>Audio and MIDI I/O Libraries</td>"
  "</tr>"
  "<tr>"
  "<td>Nasca Octavian Paul</td>"
  "<td>Effects programming</td>"
  "</tr>"
  "<tr>"
  "<td>Harvey Lubin</td>"
  "<td>Main Logo</td>"
  "</tr>"
  "<tr>"
  "<td>Tom Woodhams</td>"
  "<td>Aqua Graphics (MacOS)</td>"
  "</tr>"
  "</table>"
  "<p>"
  "<center>"
  "<b>Other contributors:</b>"
  "<p>"
  "<br>"
  "Dave Beydler<br>"
  "Jason Cohen<br>"
  "Robert Leidle<br>"
  "Logan Lewis<br>" 
  "Jason Pepas<br>"
  "Mark Tomlinson<br>"
  "David Topper<br>"
  "Rudy Trubitt<br>"
  "</center>"
  "</font>"
  "</body>"
  "</html>";

class Eraser:public wxWindow {
 public:

   Eraser(wxWindow * parent, wxWindowID id,
          const wxPoint & pos,
          const wxSize & size):wxWindow(parent, id, pos, size) {
   } virtual void OnPaint(wxPaintEvent & event) {
      wxPaintDC dc(this);
      dc.SetPen(*wxWHITE_PEN);
      dc.SetBrush(*wxWHITE_BRUSH);
      int x, y;
      GetClientSize(&x, &y);
      dc.DrawRectangle(0, 0, x, y);
   }
 public:

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(Eraser, wxWindow)
    EVT_PAINT(Eraser::OnPaint)
    END_EVENT_TABLE()

// ----------------------------------------------------------------------------
// icons
// ----------------------------------------------------------------------------
    BEGIN_EVENT_TABLE(AboutDialog, wxDialog)
    EVT_BUTTON(wxID_OK, AboutDialog::OnOK)
    EVT_BUTTON(wxID_YES, AboutDialog::OnOK)
    EVT_BUTTON(wxID_NO, AboutDialog::OnOK)
    EVT_BUTTON(wxID_CANCEL, AboutDialog::OnOK)
    END_EVENT_TABLE()

    IMPLEMENT_CLASS(AboutDialog, wxDialog)

    AboutDialog::AboutDialog(wxWindow * parent)
:  wxDialog(parent, -1, "About Audacity...",
         wxDefaultPosition, wxSize(400, DLOG_HEIGHT))
{
   Centre();

   Eraser *panel = new Eraser(this, -1,
                              wxPoint(0, 0),
                              wxSize(400, DLOG_HEIGHT));
   panel->SetBackgroundColour(wxColour(255, 255, 255));

   wxString creditStr = creditText;
   wxHtmlWindow *html = new wxHtmlWindow(panel, -1,
                                         wxPoint(10, 210),
                                         wxSize(380, 150));
   html->SetPage(creditStr);

   wxButton *ok = new wxButton(panel, wxID_OK,
                               "Audacious!",
                               wxPoint(150, 370),
                               wxSize(100, 20));
#ifndef TARGET_CARBON
   ok->SetDefault();
   ok->SetFocus();
#endif

   logo = new wxBitmap((const char **) AudacityLogo_xpm);

   icon =
       new wxStaticBitmap(panel, -1, *logo, wxPoint(93, 10),
                          wxSize(215, 190));
}

AboutDialog::~AboutDialog()
{
   delete icon;
   delete logo;
}

void AboutDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   EndModal(wxID_YES);
}
