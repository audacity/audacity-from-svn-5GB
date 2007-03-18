/**********************************************************************

  Audacity: A Digital Audio Editor

  AboutDialog.cpp

  Dominic Mazzoni
  Vaughan Johnson

********************************************************************//**

\class AboutDialog
\brief The AboutDialog shows the program version and developer credits.

It is a simple scrolling window with an 'Audacious' button to
close it.

*//*****************************************************************//**

\class AboutDialogCreditItem
\brief AboutDialogCreditItem is a structure used by the AboutDialog to 
hold information about one contributor to Audacity.

*//********************************************************************/


#include "Audacity.h"

#include <wx/dialog.h>
#include <wx/button.h>
#include <wx/dcclient.h>
#include <wx/sizer.h>
#include <wx/statbmp.h>
#include <wx/intl.h>

#include "AboutDialog.h"
#include "FileNames.h"
#include "Internat.h"
#include "widgets/LinkingHtmlWindow.h"

#include "portaudio.h"

#include <wx/listimpl.cpp>
WX_DEFINE_LIST(AboutDialogCreditItemsList);

#include "Theme.h"
#include "AllThemeResources.h"

//#include "../images/AudacityLogo.xpm"

void AboutDialog::CreateCreditsList()
{
   // The Audacity 1.3.x and 1.4.x team: developers and support
   // These will be split into two separate categories

   AddCredit(wxT("Gale Andrews"), roleTeamSupport);
   AddCredit(wxT("Richard Ash"), roleTeamSupport);
   AddCredit(wxT("Christian Brochec"), roleTeamSupport);
   AddCredit(wxT("James Crook"), roleTeamDeveloper);
   AddCredit(wxT("Vaughan Johnson"), roleTeamDeveloper);
   AddCredit(wxT("Leland Lucius"), roleTeamDeveloper);
   AddCredit(wxT("Dominic Mazzoni"), roleTeamDeveloper);
   AddCredit(wxT("Markus Meyer"), roleTeamDeveloper);
   AddCredit(wxT("Alexandre Prokoudine"), roleTeamSupport);
   AddCredit(wxT("Martyn Shaw"), roleTeamDeveloper);

   // Emeritus: people who were "lead developers" or made an
   // otherwise distinguished contribution, but who are no
   // longer active.
 
   AddCredit(wxT("Matt Brubeck"), roleEmeritusDeveloper);
   AddCredit(wxT("Roger Dannenberg"), roleEmeritusDeveloper);
   AddCredit(wxT("Joshua Haberman"), roleEmeritusDeveloper);
   AddCredit(wxT("Monty Montgomery"), roleEmeritusDeveloper);
   AddCredit(wxT("Shane Mueller"), roleEmeritusDeveloper);
   AddCredit(wxT("Tony Oetzmann"), roleEmeritusSupport);

   // All other contributors
   
   AddCredit(wxT("Lynn Allan"), roleContributor);
   AddCredit(wxT("William Bland"), roleContributor);
   AddCredit(wxT("Brian Gunlogson"), roleContributor);
   AddCredit(wxT("Arun Kishore"), roleContributor);
   AddCredit(wxT("Harvey Lubin"), roleContributor);
   AddCredit(wxT("Grek Mekkes"), roleContributor);
   AddCredit(wxT("Abe Milde"), roleContributor);
   AddCredit(wxT("Paul Nasca"), roleContributor);
   AddCredit(wxT("Augustus Saunders"), roleContributor);
   AddCredit(wxT("Mike Underwood"), roleContributor);
   AddCredit(wxT("Jun Wan"), roleContributor);
   AddCredit(wxT("Tom Woodhams"), roleContributor);
   AddCredit(wxT("Wing Yu"), roleContributor);
   
   AddCredit(wxT("expat"), roleLibrary);
   AddCredit(wxT("FLAC"), roleLibrary);
   AddCredit(wxT("LAME"), roleLibrary);
   AddCredit(wxT("libmad"), roleLibrary);
   AddCredit(wxT("libsndfile"), roleLibrary);
   AddCredit(wxT("Nyquist"), roleLibrary);
   AddCredit(wxT("Ogg Vorbis"), roleLibrary);
   AddCredit(wxT("PortAudio"), roleLibrary);
   AddCredit(wxT("Resample"), roleLibrary);
   AddCredit(wxT("SoundTouch"), roleLibrary);
   AddCredit(wxT("wxWidgets"), roleLibrary);

   AddCredit(wxT("Dave Beydler"), roleThanks);
   AddCredit(wxT("Jason Cohen"), roleThanks);
   AddCredit(wxT("Dave Fancella"), roleThanks);
   AddCredit(wxT("Steve Harris"), roleThanks);
   AddCredit(wxT("Daniel James"), roleThanks);
   AddCredit(wxT("Daniil Kolpakov"), roleThanks);
   AddCredit(wxT("Robert Leidle"), roleThanks);
   AddCredit(wxT("Logan Lewis"), roleThanks);
   AddCredit(wxT("David Luff"), roleThanks);
   AddCredit(wxT("Jason Pepas"), roleThanks);
   AddCredit(wxT("Mark Phillips"), roleThanks);
   AddCredit(wxT("Jonathan Ryshpan"), roleThanks);
   AddCredit(wxT("Patrick Shirkey"), roleThanks);
   AddCredit(wxT("David R. Sky"), roleThanks);
   AddCredit(wxT("Tuomas Suutari"), roleThanks);
   AddCredit(wxT("Mark Tomlinson"), roleThanks);
   AddCredit(wxT("David Topper"), roleThanks);
   AddCredit(wxT("Rudy Trubitt"), roleThanks);
   AddCredit(wxT("StreetIQ.com"), roleThanks);
   AddCredit(wxT("UmixIt Technologies, LLC"), roleThanks);
   AddCredit(wxT("Verilogix, Inc."), roleThanks);
}

// ----------------------------------------------------------------------------
// icons
// ----------------------------------------------------------------------------
BEGIN_EVENT_TABLE(AboutDialog, wxDialog)
   EVT_BUTTON(wxID_OK, AboutDialog::OnOK)
END_EVENT_TABLE()

IMPLEMENT_CLASS(AboutDialog, wxDialog)

AboutDialog::AboutDialog(wxWindow * parent)
   :  wxDialog(parent, -1, _NoAcc("&About Audacity..."),
               wxDefaultPosition, wxDefaultSize)
{
   wxString versionStr = AUDACITY_VERSION_STRING;
   wxString informationStr;

   creditItems.DeleteContents(true); // switchon automatic deletion of list items
   CreateCreditsList();
   
   wxString enabled = wxString(wxT(": ")) + _("Enabled");
   wxString disabled = wxString(wxT(": ")) + _("Disabled");

   #ifdef USE_LIBMAD
   informationStr += wxT("Libmad") + enabled;
   #else
   informationStr += wxT("Libmad") + disabled;
   #endif
   informationStr += wxT("<br>\n");

   #ifdef USE_LIBVORBIS
   informationStr += wxT("Ogg Vorbis") + enabled;
   #else
   informationStr += wxT("Ogg Vorbis") + disabled;
   #endif
   informationStr += wxT("<br>\n");

   #ifdef USE_LIBID3TAG
   informationStr += wxT("LibID3Tag") + enabled;
   #else
   informationStr += wxT("LibID3Tag") + disabled;
   #endif
   informationStr += wxT("<br>\n");

   # if USE_LIBFLAC
   informationStr += wxT("FLAC") + enabled;
   # else
   informationStr += wxT("FLAC") + disabled;
   # endif
   informationStr += wxT("<br>\n");

   # if USE_LADSPA
   informationStr += wxT("LADSPA") + enabled;
   # else
   informationStr += wxT("LADSPA") + disabled;
   # endif
   informationStr += wxT("<br>\n");

   #if USE_LIBRESAMPLE
   informationStr += wxT("Libresample") + enabled;
   #elif USE_LIBSAMPLERATE
   informationStr += wxT("Libsamplerate") + enabled;
   #endif
   informationStr += wxT("<br>\n");

   #if USE_PORTAUDIO_V19
   informationStr += wxT("PortAudio v19");
   #else
   informationStr += wxT("PortAudio v18");
   #endif
   informationStr += wxT("<br>\n");

   // wxWindows version:
   informationStr += wxVERSION_STRING;
   informationStr += wxT("<br>\n");

   // Current date
   informationStr += _("Program build date: ");
   informationStr += __TDATE__;
   informationStr += wxT("<br>\n");
   
   // Location of settings
   informationStr += _("Settings folder: ");
   informationStr += FileNames::DataDir();
   informationStr += wxT("<br>\n");

   wxString par1Str = _(
     "Audacity is a free program written by a team of volunteer developers around the world. Coordination happens thanks to SourceForge.net, an online service that provides free tools to open-source software projects. Audacity is available for Windows 98 and newer, Mac OS X, Linux, and other Unix-like operating systems. Older versions of Audacity are available for Mac OS 9.");

   #if 1 // Is this beta or not?
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

   wxString csetStr = wxUSE_UNICODE ? wxT("(Unicode)") : wxT("(ANSI)");

   wxString creditStr = 
      wxT("<html><head><META http-equiv=\"Content-Type\" content=\"text/html; charset=") + 
         localeStr + 
         wxT("\"></head>") + 
      wxT("<body bgcolor=\"#ffffff\"><font size=1><center>") + 
      wxT("<h3>Audacity &reg; ") + versionStr + wxT(" " ) + csetStr + wxT("</h3>")+ 
      _("A Free Digital Audio Editor") + 
      wxT("</center><p>") + par1Str +
      wxT("<p>") + par2Str +
      wxT("<p><center><a href=\"http://audacity.sourceforge.net/\">http://audacity.sourceforge.net/</a></center>") + 
      wxT("<p><center><b>") + _("Information") + wxT("</b></center>") + 
      wxT("<p><br>") + informationStr + 
      wxT("<p><center><b>") + _("Credits") + wxT("</b></center>")
      + translatorCredits +
      wxT("<p><center><b>") +

      wxString::Format(_("Audacity %s Development Team"), versionStr.c_str()) +
      wxT("</b><p><br>") +
      GetCreditsByRole(roleTeamDeveloper) +
      wxT("<p><br><b>") +

      wxString::Format(_("Audacity %s Support Team"), versionStr.c_str()) +
      wxT("</b><p><br>") +
      GetCreditsByRole(roleTeamSupport) +
      wxT("<p><br><b>") +

      _("Emeritus Developers") +
      wxT("</b><p><br>") +
      GetCreditsByRole(roleEmeritusDeveloper) +
      wxT("<p><br><b>") +

      _("Other Emeritus Team Members") +
      wxT("</b><p><br>") +
      GetCreditsByRole(roleEmeritusSupport) +
      wxT("<p><br><b>") +

      _("Other Contributors") +
      wxT("</b><p><br>") +
      GetCreditsByRole(roleContributor) +
      wxT("<p><br><b>") +

      _("Audacity is based on code from the following projects:") +
      wxT("</b><p><br>") +
      GetCreditsByRole(roleLibrary) +
      wxT("<p><br><b>") +

      _("Special thanks:") +
      wxT("</b><p><br>") +
      GetCreditsByRole(roleThanks) +
      wxT("</center></font></body></html>");
   
   this->SetBackgroundColour(theTheme.Colour( clrAboutBoxBackground ));

   wxBoxSizer * pBoxSizer = new wxBoxSizer(wxVERTICAL);

//   logo = new wxBitmap((const char **) AudacityLogo_xpm);
   icon =
       new wxStaticBitmap(this, -1, theTheme.Bitmap(bmpAudacityLogo), wxPoint(93, 10),
                          wxSize(215, 190));
   pBoxSizer->Add(icon, 0, wxALIGN_CENTER | wxALL, 8);

   wxHtmlWindow *html = 
      new LinkingHtmlWindow(this, -1,
                             wxDefaultPosition,
                             wxSize(480, 240),
                             wxHW_SCROLLBAR_AUTO | wxSUNKEN_BORDER);
   html->SetPage(creditStr);
   pBoxSizer->Add(html, 0, wxALIGN_CENTER | wxALL, 8);

   /* i18n-hint: Rather than a literal translation, consider adding the
   appropriate suffix or prefix to create a word meaning 'something which
   has Audacity' */
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

void AboutDialog::AddCredit(const wxString& description, Role role)
{
   AboutDialogCreditItem* item = new AboutDialogCreditItem();
   item->description = description;
   item->role = role;
   creditItems.Append(item);
}

wxString AboutDialog::GetCreditsByRole(AboutDialog::Role role)
{
   wxString s;
   
   for (AboutDialogCreditItemsList::Node* p=creditItems.GetFirst(); p; p = p->GetNext())
   {
      AboutDialogCreditItem* item = p->GetData();
      if (item->role == role)
      {
         s += item->description;
         s += wxT("<br>");
      }
   }
   
   // Strip last <br>, if any
   if (s.Right(4) == wxT("<br>"))
      s = s.Left(s.Length() - 4);

   return s;
}

AboutDialog::~AboutDialog()
{
   delete icon;
//   delete logo;
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

