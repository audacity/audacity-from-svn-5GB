/**********************************************************************

  Audacity: A Digital Audio Editor

  AboutDialog.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/dialog.h>
#include <wx/html/htmlwin.h>
#include <wx/button.h>
#include <wx/dcclient.h>
#include <wx/statbmp.h>
#include <wx/intl.h>

#include "AboutDialog.h"
#include "Audacity.h"

#include "../images/AudacityLogo.xpm"

#ifdef __WXMSW__
#define DLOG_HEIGHT 430
#else
#define DLOG_HEIGHT 400
#endif


class Eraser:public wxWindow {
 public:

   Eraser(wxWindow * parent, wxWindowID id,
          const wxPoint & pos,
          const wxSize & size):wxWindow(parent, id, pos, size) {
   }
  
   virtual void OnPaint(wxPaintEvent & /* event */) {
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
:  wxDialog(parent, -1, _("About Audacity..."),
         wxDefaultPosition, wxSize(400, DLOG_HEIGHT))
{
   wxString versionStr = AUDACITY_VERSION_STRING;

   wxString informationStr;

   #ifdef MP3SUPPORT
   informationStr += _("MP3 importing enabled");
   #else
   informationStr += _("MP3 importing disabled");
   #endif
   informationStr += "<br>\n";

   #ifdef USE_LIBVORBIS
   informationStr += _("Ogg Vorbis importing enabled");
   #else
   informationStr += _("Ogg Vorbis importing disabled");
   #endif
   informationStr += "<br>\n";

   #ifdef USE_LIBID3TAG
   informationStr += _("ID3 tag exporting enabled");
   #else
   informationStr += _("ID3 tag exporting disabled");
   #endif
   informationStr += "<br>\n";

   #ifdef __WXGTK__
   # if USE_LADSPA
   informationStr += _("LADSPA plug-in support enabled");
   # else
   informationStr += _("LADSPA plug-in support disabled");
   # endif
   informationStr += "<br>\n";
   #endif

   // wxWindows version:
   informationStr += wxVERSION_STRING;
   informationStr += "<br>\n";

   // Current date
   informationStr += _("Program build date:");
   informationStr += " " __DATE__ "<br>\n";

   wxString par1Str = _(
     "Audacity is a free program written by a team of volunteer developers "
     "around the world.  Coordination happens thanks to SourceForge.net, "
     "an online service that provides free tools to open-source "
     "software projects.  Audacity is available for Windows 98 and "
     "newer, Mac OS X, Linux, and other Unix-like operating systems.  "
     "Older versions of Audacity are available for Mac OS 9.");

   #if 0 // Not beta anymore
   wxString par2Str = _(
     "This is a beta version of the program.  It may contain "
     "bugs and unfinished features.  We depend on your feedback, so "
     "please visit our website and give us your bug reports and "
     "feature requests." );
   #else
   wxString par2Str = _(
     "This is a stable, completed release of Audacity.  However, if "
     "you find a bug or have a suggestion, please contact us.  We "
     "depend on feedback from users in order to continue to improve "
     "Audacity.  For more information, visit our website.");
   #endif

   wxString creditStr = 
      "<html>"
      "<body bgcolor=\"#ffffff\">"
      "<font size=1>"
      "<center>"
      "<h3>Audacity " + versionStr + "</h3>"
      + _("A Free Digital Audio Editor") +
      "</center>"
      "<p>"
      + par1Str +
      "<p>"
      + par2Str +
      "<p>"
      "http://audacity.sourceforge.net/"
      "<p>"
      "<center><b>" + _("Information") + "</b></center>"
      "<p><br>"
      + informationStr +
      "<p>"
      "<center><b>" + _("Credits") + "</b></center>"
      "<p>"
      "<table border=0>"
      "<tr>"
      "<td>Dominic Mazzoni</td>"
      "<td>" + _("Project leader and primary programmer") + "</td>"
      "</tr>"
      "<tr>"
      "<td>Joshua Haberman</td>"
      "<td>" + _("Primary developer") + "</td>"
      "</tr>"
      "<tr>"
      "<td>Matt Brubeck</td>"
      "<td>" + _("Primary developer") + "</td>"
      "</tr>"
      "<tr>"
      "<td>Brian Gunlogson</td>"
      "<td>" + _("Programmer") + "</td>"
      "</tr>"
      "<tr>"
      "<td>Shane Mueller</td>"
      "<td>" + _("Programmer") + "</td>"
      "</tr>"
      "<tr>"
      "<td>Vaughan Johnson</td>"
      "<td>" + _("Programmer") + "</td>"
      "</tr>"
      "<tr>"
      "<td>Greg Mekkes</td>"
      "<td>" + _("Programmer") + "</td>"
      "</tr>"
      "<tr>"
      "<td>James Crook</td>"
      "<td>" + _("Programmer") + "</td>"
      "</tr>"
      "<tr>"
      "<td>Vince Busam</td>"
      "<td>" + _("Programmer") + "</td>"
      "</tr>"
      "<tr>"
      "<td>Augustus Saunders</td>"
      "<td>" + _("Programmer") + "</td>"
      "</tr>"
      "<tr>"
      "<td>Markus Meyer</td>"
      "<td>" + _("Programmer") + "</td>"
      "</tr>"
      "<tr>"
      "<td>Tony Oetzmann </td>"
      "<td>" + _("Documentation Writer") + "</td>"
      "</tr>"
      "<tr>"
      "<td>Paul Nasca</td>"
      "<td>" + _("Effects programmer") + "</td>"
      "</tr>"
      "<tr>"
      "<td>Harvey Lubin</td>"
      "<td>" + _("Main Logo") + "</td>"
      "</tr>"
      "<tr>"
      "<td>Roger Dannenberg</td>"
      "<td>" + _("Algorithms, MIDI & Audio I/O programming") + "</td>"
      "</tr>"

      "</table>"
      "<p>"
      "<center>"
      "<b>" + _("Other contributors:") + "</b>"
      "<p>"
      "<br>"
      "Dave Beydler<br>"
      "William Bland<br>"
      "Jason Cohen<br>"
      "Dave Fancella<br>"
      "Steve Harris<br>"
      "Daniel James<br>"
      "Steve Jolly<br>"
      "Daniil Kolpakov<br>"
      "Robert Leidle<br>"
      "Logan Lewis<br>" 
      "Monty<br>"
      "Tino Meinen<br>"
      "Jason Pepas<br>"
      "Mark Phillips<br>"
      "Alexandre Prokoudine<br>"
      "Jonathan Ryshpan<br>"
      "Patrick Shirkey<br>"
      "Mark Tomlinson<br>"
      "David Topper<br>"
      "Rudy Trubitt<br>"
      "Juhana Sadeharju<br>"
      "Otto Wyss<br>"
      "<p>"
      "<b>" + _("Special thanks:") + "</b>"
      "<p><br>"
      "The wxWindows Team<br>"
      "The Ogg Vorbis Team<br>"
      "Rob Leslie (libmad)<br>"
      "Ross Bencina and Phil Burk (PortAudio)<br>"
      "Erik de Castro Lopo (libsndfile and libsamplerate)<br>"
      "Olli Parviainen (soundtouch)<br>"
      "Verilogix, Inc.<br>"

      "</center>"
      "</font>"
      "</body>"
      "</html>";
   
   Centre();

   Eraser *panel = new Eraser(this, -1,
                              wxPoint(0, 0),
                              wxSize(400, DLOG_HEIGHT));
   panel->SetBackgroundColour(wxColour(255, 255, 255));

   wxHtmlWindow *html = new wxHtmlWindow(panel, -1,
                                         wxPoint(10, 210),
                                         wxSize(380, 150));
   html->SetPage(creditStr);

   wxButton *ok = new wxButton(panel, wxID_OK,
                               _("Audacious!"),
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
