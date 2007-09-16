/**********************************************************************

  Audacity: A Digital Audio Editor

  HelpText.cpp

  James Crook

********************************************************************//**

\file HelpText.cpp
\brief Given a key, returns some html.
*//********************************************************************/

#include <wx/string.h>
#include <wx/intl.h>

#include "Audacity.h"
#include "HelpText.h"

wxString WrapText( const wxString & Text )
{
   return wxString(wxT(""))+
      wxT("<html><head></head>") + 
      wxT("<body bgcolor=\"#ffffff\">") + 
      wxT("<p>") + Text +
      wxT("</body></html>");
}

wxString Link( const wxString &Key, const wxString& Text )
{
   return wxString(wxT("")) +
      wxT("<a href='innerlink:") +
      Key + 
      wxT("'>") + 
      Text +
      wxT("</a>");
}

wxString WikiLink( const wxString& Text )
{
   return wxString(wxT("")) +
      wxT("<a href='http://www.audacityteam.org/wiki/index.php?title=") +
      Text + 
      wxT("'>") + 
      Text +
      wxT("</a>");
}

wxString ToWelcome( )
{
   return wxString(wxT("")) +
_("Back to ") + Link( wxT("welcome"), _("Welcome") ) + _(" page.");
}

wxString TitleText( const wxString & Key )
{
   if(Key==wxT("welcome"))
   {
      return _("Welcome!");
   }

   if(Key ==wxT("play") )
   {
      return _("Playing Audio");
   }
   if((Key ==wxT("record") ) || (Key ==wxT("norecord") ))
   {
      return _("Recording Audio");
   }
   if((Key ==wxT("edit") ) || (Key==wxT("grey")))
   {
      return _("Greyed Out Items");
   }
   if(Key ==wxT("export") )
   {
      return _("Export");
   }
   if(Key ==wxT("wma-proprietary") )
   {
      return _("Supported Formats");
   }
   return Key;
}

wxString HelpTextBuiltIn( const wxString & Key )
{
   if(Key==wxT("welcome"))
   {
      return WrapText( 
wxString(wxT("")) + 
wxT("<p>") +
_("Welcome to Audacity 1.4.0! Let's get started!") +
wxT("<p>") +
wxT("<p>") +
_("You may want to click on the hyperlinks below to read:") +
wxT("<ul>" )+
wxT("<li>" )+
_("How to ") + Link( wxT("play"), _("play back") ) + _(" an existing sound file.")+
wxT("<li>" )+
_("How to ") + Link( wxT("record"), _("record")) + _(" a new sound file.")+
wxT("<li>" )+
_("How to ") + Link( wxT("edit"), _("edit" )) + _(" sound and ") + 
Link( wxT("export"), _("export")) + _(" for an mp3 player.")+
wxT("</ul><br><br>") +
_("This could save you from puzzling over why some functions in the ") +
Link( wxT("grey"), _("menus are greyed out"))+_(" or why Audacity ") +
Link( wxT("norecord"), _("isn't recording anything"))+ _( " when ")+
_("you expect it to.  Or you may prefer to just try using Audacity and see ")+ 
_("what you can do.  You can always get back to these instructions by using ")+
_("the 'Help' option in the menus, and then clicking on 'Start Up Message' ")+
wxT("<br><br>")+
_("Welcome to Audacity.")
         );
   }

   if(Key ==wxT("play") )
   {
      return WrapText(
wxString(wxT(""))+
_("You can import files into Audacity by dragging them in, or use the File > ")+
_("Import > Audio command.<br><br>File > Open does the same, but opens a new ")+
_("Audacity window.<br><br>These are the main formats Audacity will import for ")+
_("you: AIFF, AU, FLAC, [M4A (only on a Mac)], MP2/MP3, OGG Vorbis, WAV. ")+
_("If your file is in some other format see (format help).<br><br>If you want to import an ")+
_("audio CD, see ")+ WikiLink( wxT("how to import CDs")) + _(". To start playing your imported audio, ")+
_("press the green Play button. ")+
wxT("<br><br>")+
ToWelcome()
         );
   }
   if((Key ==wxT("record") ) || (Key ==wxT("norecord") ))
   {
      return WrapText(
wxString(wxT(""))+
_("To start recording, press the red Record button. Of course it's not quite ")+
_("that easy. There are three crucial settings you need to make:" )+
wxT("<br><br>")+
_("(1) The \"Recording Device\", set in the Audio I/O tab of Preferences. ")+
_("By default Audacity will choose the current system device, usually your ")+ 
_("inbuilt sound device, so you may not need to set this. But if you're using an ")+
_("external USB or Firewire device such as a USB turntable, please check this ")+
_("device is selected as your recording device. ")+
wxT("<br><br>")+
_("(2) The input source for your device, such as microphone, line-in [or ")+
_("stereo mix]. [If you're on Vista, choose your source in the \"Recording ")+ 
_("Device\" on the Audio I/O tab as above.] [Mac info]. Otherwise, choose ")+
_("your input in the dropdown selector on the right of the Mixer Toolbar. ")+
_("Note: many USB or Firewire devices won't have a choice of inputs, so ")+
_("you can ignore this step - see the documentation for your device. ")+
wxT("<br><br>")+
_("(3) Input Volume: Before recording for real, make a test recording to set ")+
_("the input level of your recording so that it's neither too soft or too loud. ")+
_("To do this, adjust the input volume slider on the Mixer Toolbar (by the ")+
_("microphone symbol) so that when you record, the levels on the red VU ")+
_("recording meter in the Meter Toolbar are close to (but not touching) ")+
_("the right edge of the scale. ")+
wxT("<br><br>")+
_("There is more help if you're stuck: our Recording FAQs, our Tutorial  \"Your ")+
_("First Recording\" in the Manual, and our Wiki ")+WikiLink(wxT("Recording Tips"))+ _(" especially ")+
_("the ")+ WikiLink(wxT("Troubleshooting"))+ _(" section. ")+
wxT("<br><br>")+
ToWelcome()
         );
   }
   if((Key ==wxT("edit") ) || (Key==wxT("grey")))
   {
      return WrapText(
wxString(wxT(""))+
_("Editing: Audacity is a powerful editor, so you'll want to see what it can do with ")+
_("your imported file or recording.  The main commands you will use are under ")+
_("the Edit menu (such as cut, copy and paste) and under the Effect menu (you ")+
_("can do things like boost the bass, change pitch or tempo, or remove noise). ")+
_("There are two rules when editing:")+
wxT("<br><br>")+
_("(1) if you're still playing or recording, use ")+
_("the yellow Stop button, because you can't edit a moving track! ")+
wxT("<br><br>")+
_("(2) You'll see that on occasion some of the menus or buttons are greyed out or say ")+
_("\"Disabled\", according to what you are trying to do. This is normal. For example ")+
_("because you can't edit the audio while playing or recording, we grey out or ")+
_("disable commands until you press Stop.  Commands can sometimes be greyed ")+
_("out or disabled for other reasons - for example you can't run effects until ")+ 
_("you have audio on the screen, and you obviously cannot paste audio until you've ")+
_("cut or copied it to Audacity's clipboard. ")+
wxT("<br><br>")+
ToWelcome()
         );
   }
   if(Key ==wxT("export") )
   {
      return WrapText(
         wxString(wxT(""))+ 
// FIX-ME: We need some text here!
_("Text about exporting still to be written...")+
wxT("<br><br>")+
ToWelcome()
         );
   }
   if(Key ==wxT("wma-proprietary") )
   {
      return WrapText(
         wxString(wxT(""))+

#ifdef EXACTLY_AS_IN_FAQ
_("<h2>Can Audacity import file formats like WMA, AAC, FLAC, etc.?</h2> ") +
_("<p>Audacity <b>cannot</b> import or export files in ") +
_("<b>WMA, AAC, RealAudio, Shorten (SHN)</b>, ") +
_("or most other proprietary formats, or any kind of Digital Rights ") +
_("Management (DRM) protected file, including many purchased online such ") +
_("as on iTunes or Napster. Because of licensing and patent restrictions, ") +
_("we are not allowed to add these formats to Audacity. Future versions of ") +
_("Audacity might be able to support these formats using codecs installed ") +
_("in your operating system.</p> ") +
_("<p>Some open formats are not yet supported by Audacity, including <b>Ogg Speex</b> ") +
_("and <b>FLAC</b>.  We hope to support these formats in future versions of Audacity.</p> ") +
_("<p>Audacity can currently import WAV, AIFF, AU, MP2/MP3 and OGG Vorbis ") +
_("files. If you cannot import your file into Audacity, you can as a ") +
_("workround convert it to WAV or AIFF. As long as it's not a ") +
_("DRM-protected file, you could do this with iTunes® or with ") +
_("<a href=\"http://www.erightsoft.com/SUPER.html#Dnload\">SUPER</a> ") +
_("player. If it's a DRM-protected file, you can burn it to an audio CD in ") +
_("the application that is licensed to play it, then extract (rip) the CD ") +
_("track to WAV or AIFF. On Windows, you can use Windows Media Player 11, ") +
_("iTunes, or <a href=\"http://cdexos.sourceforge.net/?q=download\">CDex</a> to ") +
_("extract to WAV. Or you can play the file on your computer and record it – ") +
_("see <a href=\"http://audacity.sourceforge.net/help/faq?s=recording&amp;i=streaming\"> ") +
_("Can Audacity record RealAudio or other streaming audio?</a>.</p> ") +
_("<p>For more detail on supported formats, see the ") +
_("<a href=\"http://audacity.sourceforge.net/about/features\">feature list</a>.</p> ") +
_("<p>See also: <a href=\"http://audacity.sourceforge.net/help/faq?s=files&amp;i=midi\"> ") +
_("Why can't I play MIDI files?</a></p>")
#else
_("<p>Audacity can currently import WAV, AIFF, AU, MP2/MP3 and OGG Vorbis ") +
_("files.") +
_("<p>Audacity <b>cannot</b> import or export files in ") +
_("<b>WMA, AAC, RealAudio, Shorten (SHN)</b>, ") +
_("or most other proprietary formats, or any kind of Digital Rights ") +
_("Management (DRM) protected file, including many purchased online such ") +
_("as on iTunes or Napster. Because of licensing and patent restrictions, ") +
_("we are not allowed to add these formats to Audacity. Future versions of ") +
_("Audacity might be able to support these formats using codecs installed ") +
_("in your operating system.</p> ") +
_("<p>Some open formats are not yet supported by Audacity, including <b>Ogg Speex</b> ") +
_("and <b>FLAC</b>.") +
_("<p>More information is available on line ") +
_("<a href=\"http://audacity.sourceforge.net/help/faq?s=files&i=wma-proprietary\">here</a> ")
#endif
         );
   }
   return wxT("");
}

wxString HelpText( const wxString & Key )
{

   // Possible future enhancement...
   // We could look for the text as a local file and use
   // that if we find it...
   // if( wxFileExists( Path+Key ) )
   // ...

   wxString Text = HelpTextBuiltIn( Key );
   if( !Text.IsEmpty())
      return Text;

   // Perhaps useful for debugging - we'll return key that we didn't find.
   return WrapText( Key );
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

