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
   if(Key ==wxT("burncd") )
   {
      return _("Burn to CD" );
   }
   if(Key ==  wxT("remotehelp") )
   {
      return _("No Local Help");
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
+_("Welcome to Audacity ") + AUDACITY_VERSION_STRING + _("! Let's get started!") +
wxT("<p>") +
wxT("<p>") +
_("You can click on the links below to learn how to:") +
wxT("<ul>" )+
wxT("<li>" )+
Link( wxT("play"), _("play back") ) + _(" an existing sound file.")+
wxT("<li>" )+
_("How to ") + Link( wxT("record"), _("record")) + _(" a new sound file.")+
wxT("<li>" )+
_("How to ") + Link( wxT("edit"), _("edit" )) + _(" sound and ") + 
Link( wxT("export"), _("export")) + _(" to a sound file like MP3.")+
wxT("</ul><br><br>") +
_("Those links will help if you find a problem such as a ") +
Link( wxT("grey"), _("menu option greyed out"))+_(" or that Audacity ") +
Link( wxT("norecord"), _("isn't recording properly"))+ _( " or that you ")+
_("can't ") + Link( wxT("burncd"), _("burn to a CD")) + _(". You can ") +
_("always get back to these instructions by clicking the 'Help' menu, ") +
_("then 'Show Welcome Message.' ")+
wxT("<br><br>")+
_("Welcome again to Audacity.")
         );
   }

   if(Key ==wxT("play") )
   {
      return WrapText(
wxString(wxT(""))+
_("You can either drag existing files into Audacity, or use the File >  ")+
_("Import > Audio command. File > Open does the same, but opens a new ")+
_("Audacity window for any file after the first.<br><br>These are the main ")+
_("formats Audacity will import for you: <b>AIFF, AU, FLAC, M4A</b> (only ")+
_("on a Mac), <b>MP2/MP3, OGG Vorbis, WAV</b>.  If your file is in some ")+
_("other format see (format help). If you want to import an audio CD, see ")+
WikiLink( wxT("How to import CDs")) + _(". <br><br>To start playing ")+
_("your audio, press the green Play button. To stop, press the yellow ")+
_("Stop button. You can also use spacebar to either play or stop. ")+
_("When you start playing again after stopping, playback resumes from its ")+
_("previous starting point. To change the starting point, use the |<< and |>> ")+
_("buttons to skip to the start or end respectively, or click anywhere in the ")+
_("track to restart from that point.")+
wxT("<br><br>")+
ToWelcome()
         );
   }
   if((Key ==wxT("record") ) || (Key ==wxT("norecord") ))
   {
      return WrapText(
wxString(wxT(""))+
ToWelcome()+
wxT("<br><br>")+
_("<b>To start recording, press the red Record button</b>. Before doing so ")+
_("however, there are three crucial settings you must check:" )+
wxT("<br><br>")+
_("(1) The <b>Recording Device</b>, set in the Audio I/O tab of Preferences. ")+
_("By default Audacity will use the current system device, which is usually ")+ 
_("your inbuilt sound device, so you may not need to set this. But if you're using ")+
_("an external USB or Firewire device such as a USB turntable, make sure this ")+
_("device is selected as your recording device. ")+
wxT("<br><br>")+
_("(2) The <b>input source</b> for your device, such as microphone or line-in. ")+
_("<ul><li><b>On Windows</b>, you normally choose your input in the dropdown selector on ")+
_("the right of the <b>Mixer Toolbar<b> <i>note: on Vista, you must do so at ")+
_("Recording Device on the Audio I/O Preferences tab.</i></li><li><b>On a Mac</b>, you normally ")+
_("choose input sources outside Audacity, in Apple Audio-MIDI Setup.</li></ul><i>Note: many ")+
_("USB or Firewire devices don't have a choice of inputs, so you can ignore this step - ")+
_("see the documentation for your device. ")+
wxT("<br><br>")+
_("(3) <b>Input volume</b>. Before recording for real, make a test recording to set ")+
_("the input level, so that it's neither too soft or too loud. To do this: ")+
wxT("<ol>" )+
wxT("<li>" )+
_("Turn on <b>monitoring</b>, by either double-clicking over the right-hand of the two ")+
_("VU Meters, or right-clicking over it and choosing \"Start Monitoring\". If you do not see ")+
_("the meters, click View > Toolbars and check \"Meter Toolbar\". ")+
wxT("<li>" )+
_("Press Record, and adjust the input volume slider on the Mixer Toolbar (by the ")+
_("microphone symbol), so that the red bars in the meter come close to (but do not touch) ")+
_("the right edge of the scale. If you can't hear the sound, go to the Audio I/O tab of ")+
_("Preferences and check \"Software Playthrough\". ")+
wxT("</ol><br><br>") +
_("There is more help if you're stuck: our Recording FAQs, our Tutorial  <b>Your ")+
_("First Recording </b> in the Manual, and our Wiki ")+WikiLink(wxT("Recording Tips"))+ 
_(", especially the ")+ WikiLink(wxT("Troubleshooting Recordings"))+ _(" section. ")+
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
wxT("<br><br>")+
_("Audacity applies edits to areas of the audio track. You can select a particular area ")+
_("of audio by clicking in the track and dragging the shaded area with your mouse. If  ")+
_("you don't select any audio, Audacity will select all that you have on the screen.")+  
wxT("<br><br>")+
_("Two important points when editing:")+
wxT("<br><br>")+
_("(1) If you're still playing or recording, use ")+
_("the yellow Stop button, because you can't edit or export a moving track! ")+
wxT("<br><br>")+
_("(2) On occasion, some of the menus or buttons are greyed out or say ")+
_("\"Disabled\" -  this is normal. For example, because you can't edit audio while ")+
_("playing or recording, we grey out or disable commands until you press Stop. ")+
_("Commands can sometimes be unavailable for other reasons - for example you ")+
_("can't run effects until you have audio on the screen, and you obviously can't ")+
_("paste audio until you've cut or copied it to Audacity's clipboard. ")+
wxT("<br><br>")+
ToWelcome()
         );
   }
   if(Key ==wxT("export") )
   {
      return WrapText(
wxString(wxT(""))+ 
_("Exporting and Saving. have different purposes.  You \" save\" an Audacity project ")+
_("file when you want to return to your work in Audacity later. This means you don't then ")+
_("have to re-import or re-record audio, or redo editing you have done so far. When you ")+
_("want to listen to your work in other computer programs or burn it to CD, you \"export\" ")+
_("it as an audio file such as a WAV, AIFF or MP3. ")+ 
wxT("<br><br>")+
_("To save an Audacity project, use <b>File > Save Project</b>. This will save an ")+
_("<b>.aup</b> Project file, plus a <b>_data</b> folder containing the actual audio. ")+
_("Make sure you don't rename or move either of these, because Audacity needs ")+
_("to refer to both. Only use File > Save Project As. when you either need to save ")+
_("an empty Project, or if you want to save an existing Project to a <i> new</i> name. ")+
_("When you want to re-open a saved Project, simply click File > Open and open the .aup file. ")+    
wxT("<br><br>")+
_("When you want to export your work to an audio file, use <b>File > Export</b> and ")+
_("choose your desired format in the \"Save as type\" dropdown. Click the Options button ")+
_("for advanced choices, such as the bit rate to use for MP3 export. There are two other ")+
_("Export commands under the File menu. \"Export Selection\" exports only a selected ")+
_("area of audio. \"Export Multiple\" exports multiple files, for example where you have ")+
_("more than one track on screen and want to export each as a separate file. ")+
wxT("<br><br>")+
_("Before you can export as <b>MP3</b>, you need to add the LAME MP3 encoder to ")+
_("your computer.  For help with LAME, see ")+  WikiLink( wxT("Lame_Installation"))+ 
_(". There is also some help on how to ")+ Link( wxT("burncd"), _("burn to a CD."))+
wxT("<br><br>")+
ToWelcome()
         );
   }
   if(Key ==wxT("wma-proprietary") )
   {
      return WrapText(
         wxString(wxT(""))+

//#ifdef NOT EXACTLY_AS_IN_FAQ_AS_FAQ_INCORRECT_FOR_1.4._TRY_THIS_LOCALLY?  
_("<h2>Can Audacity import file formats like WMA, AAC, RealAudio  etc.?</h2> ") +
_("<p>Audacity cannot import or export files in ") +
_("<b>WMA, RealAudio, Shorten (SHN)</b>, ") +
_("or most other proprietary formats, because of licensing and patent ")+
_("restrictions . Audacity also cannot import any kind of Digital Rights ") +
_("Management (DRM) protected file, including most purchased online such ") +
_("as from iTunes or Napster. If you are on a Mac, you can import <b>AAC</b>  ") +
_("encoded files (such as <b>M4A</b>) as long as they are not DRM protected, ")+
_("but you cannot export to these formats.</p>")+
_("<p>Some open formats are not yet supported by Audacity, including <b>Ogg Speex</b> ") +
_("and <b>Musepack</b>.  We hope to support these formats in future versions of Audacity.</p> ") +
_("<p>If you cannot import your file into Audacity, you can as a ") +
_("workround convert it to WAV or AIFF. As long as it's not a ") +
_("DRM-protected file, you could do this with iTunes© or with ") +
_("<a href=\"http://www.erightsoft.com/SUPER.html#Dnload\">SUPER</a> ") +
_("player. If it's a DRM-protected file, you can burn it to an audio CD in ") +
_("the application that is licensed to play it, then extract (rip) the CD ") +
_("track to WAV or AIFF. You can use iTunes to extract to WAV or AIFF, plus ")+
_("Windows Media Player 11 or <a href=\"http://cdexos.sourceforge.net/?q=download\">")+
_("CDex</a> on Windows. Or you can play the file on your computer and record it - ") +
_("see <a href=\"http://audacity.sourceforge.net/help/faq?s=recording&amp;i=streaming\">. ") 
//#endif
         );
   }
   if(Key ==wxT("burncd") )
   {
      return WrapText(
         wxString(wxT(""))+
_("If you want to burn your work to an audio CD, you must first ")+
 Link( wxT("export"), _("export as WAV or AIFF")) + _(" and then ")+
_("burn that file to CD - for more help, see ")+ WikiLink( wxT("How to burn CDs"))+ (".")+
wxT("<br><br>")+
ToWelcome()
         );
   }
   // Remote help allows us to link to a local copy of the help if it exists,
   // or provide a message that takes you to the internet if it does not.
   // It's used by the menu item Help > Index
   if(Key ==  wxT("remotehelp") )
   {
// *URL* will be replaced by whatever URL we are looking for.
      return WrapText(
         wxString(wxT(""))+
_("<p>You don't appear to have Audacity help files on your machine. ") +
_("You can download the help files and then access them from ") +
_("Audacity or you can click <a href=\"*URL*\">here</a> to read ") +
_("the help online.")
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

