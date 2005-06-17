/**********************************************************************

  Audacity: A Digital Audio Editor

  AboutDialog.cpp

  Dominic Mazzoni
  Vaughan Johnson

**********************************************************************/

#include <wx/dialog.h>
#include <wx/html/htmlwin.h>
#include <wx/button.h>
#include <wx/dcclient.h>
#include <wx/sizer.h>
#include <wx/statbmp.h>
#include <wx/intl.h>

#include "AboutDialog.h"
#include "Audacity.h"

#include "../images/AudacityLogo.xpm"

// ----------------------------------------------------------------------------
// icons
// ----------------------------------------------------------------------------
BEGIN_EVENT_TABLE(AboutDialog, wxDialog)
   EVT_BUTTON(wxID_OK, AboutDialog::OnOK)
END_EVENT_TABLE()

IMPLEMENT_CLASS(AboutDialog, wxDialog)

AboutDialog::AboutDialog(wxWindow * parent)
   :  wxDialog(parent, -1, _("About Audacity..."),
               wxDefaultPosition, wxDefaultSize)
{
   wxString versionStr = wxT(AUDACITY_VERSION_STRING);

   wxString informationStr;

   #ifdef USE_LIBMAD
   informationStr += _("MP3 importing enabled");
   #else
   informationStr += _("MP3 importing disabled");
   #endif
   informationStr += wxT("<br>\n");

   #ifdef USE_LIBVORBIS
   informationStr += _("Ogg Vorbis importing enabled");
   #else
   informationStr += _("Ogg Vorbis importing disabled");
   #endif
   informationStr += wxT("<br>\n");

   #ifdef USE_LIBID3TAG
   informationStr += _("ID3 tag exporting enabled");
   #else
   informationStr += _("ID3 tag exporting disabled");
   #endif
   informationStr += wxT("<br>\n");

   # if USE_LADSPA
   informationStr += _("LADSPA plug-in support enabled");
   # else
   informationStr += _("LADSPA plug-in support disabled");
   # endif
   informationStr += wxT("<br>\n");

   #if USE_LIBRESAMPLE
   informationStr += _("Libresample support enabled");
   #elif USE_LIBSAMPLERATE
   informationStr += _("Libsamplerate support enabled");
   #else
   informationStr += _("No resampling support enabled");
   #endif
   informationStr += wxT("<br>\n");

   // wxWindows version:
   informationStr += wxVERSION_STRING;
   informationStr += wxT("<br>\n");

   // Current date
   informationStr += _("Program build date: ");
   informationStr += __TDATE__;
   informationStr += wxT("<br>\n");

   wxString par1Str = _(
     "Audacity is a free program written by a team of volunteer developers around the world. Coordination happens thanks to SourceForge.net, an online service that provides free tools to open-source software projects. Audacity is available for Windows 98 and newer, Mac OS X, Linux, and other Unix-like operating systems. Older versions of Audacity are available for Mac OS 9.");

   #if 1 // Not beta anymore
   wxString par2Str = _(
     "This is a beta version of the program.  It may contain bugs and unfinished features.  We depend on your feedback, so please visit our website and give us your bug reports and feature requests." );
   #else
   wxString par2Str = _(
     "This is a stable, completed release of Audacity. However, if you find a bug or have a suggestion, please contact us. We depend on feedback from users in order to continue to improve Audacity. For more information, visit our website.");
   #endif

   wxString translatorCredits;
   /* i18n-hint: The translation of "translator_credits" will appear
      in the credits in the About Audacity window.  Use this to add
      your own name(s) to the credits.

      For example:  "English translation by Dominic Mazzoni."
      */
   if (_("translator_credits") != wxString(wxT("translator_credits"))) {
      translatorCredits += wxT("<p><center>");
      translatorCredits += _("translator_credits");
      translatorCredits += wxT("</center>");
   }
   wxString localeStr = wxLocale::GetSystemEncodingName();

   wxString creditStr = 
      wxT("<html><head><META http-equiv=\"Content-Type\" content=\"text/html; charset=") + 
         localeStr + 
         wxT("\"></head>") + 
      wxT("<body bgcolor=\"#ffffff\"><font size=1><center>") + 
      wxT("<h3>Audacity ") + versionStr + wxT("</h3>")+ 
      _("A Free Digital Audio Editor") + 
      wxT("</center><p>") + par1Str +
      wxT("<p>") + par2Str +
      wxT("<p>http://audacity.sourceforge.net/") + 
      wxT("<p><center><b>") + _("Information") + wxT("</b></center>") + 
      wxT("<p><br>") + informationStr + 
      wxT("<p><center><b>") + _("Credits") + wxT("</b></center>")
      + translatorCredits +
      wxT("<p><table border=0>") + 
         wxT("<tr><td>Dominic Mazzoni</td><td>") + 
            _("Project leader and primary programmer") + wxT("</td></tr>") + 
         wxT("<tr><td>Joshua Haberman</td><td>") + 
            _("Primary developer") + wxT("</td></tr>") + 
         wxT("<tr><td>Matt Brubeck</td><td>") + 
            _("Primary developer") + wxT("</td></tr>") + 
         wxT("<tr><td>James Crook</td><td>") + _("Programmer") + wxT("</td></tr>") + 
         wxT("<tr><td>Vaughan Johnson</td><td>") + _("Programmer") + wxT("</td></tr>") + 
         wxT("<tr><td>Markus Meyer</td><td>") + _("Programmer") + wxT("</td></tr>") + 
         wxT("<tr><td>Shane Mueller</td><td>") + _("Programmer") + wxT("</td></tr>") + 
         wxT("<tr><td>Lynn Allan</td><td>") + _("Programmer (CleanSpeech)") + wxT("</td></tr>") + 
         wxT("<tr><td>Tony Oetzmann</td><td>") + 
            _("Documentation Writer") + wxT("</td></tr>") + 
         wxT("<tr><td>Harvey Lubin</td><td>") + _("Main Logo") + wxT("</td></tr>") + 
      wxT("</table>") + 
      wxT("<p><center><b>") + _("Developers:") + wxT("</b><p><br>") + 
         wxT("William Bland<br>") + 
         wxT("Vince Busam<br>") + 
         wxT("Brian Gunlogson<br>") + 
         wxT("Greg Mekkes<br>") + 
         wxT("Augustus Saunders<br>") + 
      wxT("<br><p><center><b>") + _("Other contributors:") + wxT("</b><p><br>") + 
         wxT("Dave Beydler<br>") + 
         wxT("Jason Cohen<br>") + 
         wxT("Roger Dannenberg<br>") + 
         wxT("Dave Fancella<br>") + 
         wxT("Steve Harris<br>") + 
         wxT("Daniel James<br>") + 
         wxT("Steve Jolly<br>") + 
         wxT("Daniil Kolpakov<br>") + 
         wxT("Robert Leidle<br>") + 
         wxT("Logan Lewis<br>") +  
         wxT("Tino Meinen<br>") + 
         wxT("Abe Milde<br>") + 
         wxT("Monty<br>") + 
         wxT("Paul Nasca<br>") + 
         wxT("Jason Pepas<br>") + 
         wxT("Mark Phillips<br>") + 
         wxT("Alexandre Prokoudine<br>") + 
         wxT("Jonathan Ryshpan<br>") + 
         wxT("Juhana Sadeharju<br>") + 
         wxT("Patrick Shirkey<br>") + 
         wxT("Mark Tomlinson<br>") + 
         wxT("David Topper<br>") + 
         wxT("Rudy Trubitt<br>") + 
         wxT("Tom Woodhams<br>") + 
         wxT("Otto Wyss<br>") + 
      wxT("<p><b>") + _("Special thanks:") + wxT("</b><p><br>") + 
         wxT("The wxWidgets Team<br>") + 
         wxT("The Ogg Vorbis Team<br>") + 
         wxT("Rob Leslie (libmad)<br>") + 
         wxT("Ross Bencina and Phil Burk (PortAudio)<br>") + 
         wxT("Erik de Castro Lopo (libsndfile)<br>") + 
         wxT("Olli Parviainen (soundtouch)<br>") + 
         wxT("Verilogix, Inc.<br>") + 
      wxT("</center></font></body></html>");
   
   this->SetBackgroundColour(wxColour(255, 255, 255));

   wxBoxSizer * pBoxSizer = new wxBoxSizer(wxVERTICAL);

   logo = new wxBitmap((const char **) AudacityLogo_xpm);
   icon =
       new wxStaticBitmap(this, -1, *logo, wxPoint(93, 10),
                          wxSize(215, 190));
   pBoxSizer->Add(icon, 0, wxALIGN_CENTER | wxALL, 8);

   wxHtmlWindow *html = new wxHtmlWindow(this, -1,
                                         wxDefaultPosition,
                                         wxSize(400, 150),
                                         wxHW_SCROLLBAR_AUTO | wxSUNKEN_BORDER);
   html->SetPage(creditStr);
   pBoxSizer->Add(html, 0, wxALIGN_CENTER | wxALL, 8);

   wxButton *ok = new wxButton(this, wxID_OK, _("Audacious!"));
   ok->SetDefault();
   ok->SetFocus();
   pBoxSizer->Add(ok, 0, wxALIGN_CENTER | wxALL, 8);

   this->SetAutoLayout(true);
   this->SetSizer(pBoxSizer);
   pBoxSizer->Fit(this);
   pBoxSizer->SetSizeHints(this);

   this->Centre();
}

AboutDialog::~AboutDialog()
{
   delete icon;
   delete logo;
}

void AboutDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   EndModal(wxID_OK);
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: a8955864-40e2-47aa-923b-cace3994493a

