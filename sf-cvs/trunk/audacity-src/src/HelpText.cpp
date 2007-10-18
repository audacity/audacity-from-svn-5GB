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
+_("Welcome to Audacity ") + AUDACITY_VERSION_STRING + wxT("! ") +
_("Let's get started!") +
wxT("</p><p>") +
_("You can click on the links below to learn how to:") +
wxT("<ul><li>" )+
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
_("can't ") + Link( wxT("burncd"), _("burn to a CD")) + 
_(". You can always get back to these instructions by clicking the 'Help' menu, then 'Show Welcome Message.' ")+
wxT("<br><br>")+
_("Welcome again to Audacity.")
         );
   }

   if(Key ==wxT("play") )
   {
      return WrapText(
wxString(wxT(""))+
_("You can either drag existing files into Audacity, or use the File > Import > Audio command.")+
_("File > Open does the same, but opens a new Audacity window for any file after the first.")+
wxT("<br/><br/>")+
_("These are the main formats Audacity will import for you: <b>AIFF, AU, FLAC, M4A</b> (only on a Mac), <b>MP2/MP3, OGG Vorbis, WAV</b>.")+
_("If your file is in some other format see (format help). If you want to import an audio CD, see ")+
WikiLink( wxT("How to import CDs")) + wxT(". <br><br>")+
_("To start playing your audio, press the green Play button. ") +
_("To stop, press the yellow Stop button. ") +
_("You can also use spacebar to either play or stop. ") +
_("When you start playing again after stopping, playback resumes from its previous starting point.") +
_("To change the starting point, use the |<< and |>> buttons to skip to the start or end respectively, or click anywhere in the track to restart from that point.")+
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
/** i18n-hint: this is the introduction to three items in a list*/
_("<b>To start recording, press the red Record button</b>. Before doing so however, there are three crucial settings you must check:" )+
wxT("<br><br>")+
/** i18n-hint: First item in a list */
_("(1) The <b>Recording Device</b>, set in the Audio I/O tab of Preferences. ") +
_("By default Audacity will use the current system device, which is usually your inbuilt sound device, so you may not need to set this. ") +
_("But if you're using an external USB or Firewire device such as a USB turntable, make sure this device is selected as your recording device. ")+
wxT("<br><br>")+
/* i18n-hint: 2nd item in the list */
_("(2) The <b>input source</b> for your device, such as microphone or line-in. ")+
wxT("<ul><li>") +
_("<b>On Windows</b>, you normally choose your input in the dropdown selector on the right of the <b>Mixer Toolbar<b> <i>note: on Vista, you must do so at Recording Device on the Audio I/O Preferences tab.</i>") +
wxT("</li><li>") +
_("<b>On a Mac</b>, you normally choose input sources outside Audacity, in Apple Audio-MIDI Setup.") +
wxT("</li></ul>") +
_("<i>Note: many USB or Firewire devices don't have a choice of inputs, so you can ignore this step - see the documentation for your device. ")+
wxT("<br><br>")+
/* i18n-hint: Third and final item in list */
_("(3) <b>Input volume</b>. Before recording for real, make a test recording to set the input level, so that it's neither too soft or too loud.") +
/* i18n-hint: introduction to a set of steps to do something we have already
 * talked about */
_("To do this: ")+
wxT("<ol><li>" )+
_("Turn on <b>monitoring</b>, by either double-clicking over the right-hand of the two VU Meters, or right-clicking over it and choosing \"Start Monitoring\". ") +
_("If you do not see the meters, click View > Toolbars and check \"Meter Toolbar\". ")+
wxT("<li>" )+
_("Press Record, and adjust the input volume slider on the Mixer Toolbar (by the microphone symbol), so that the red bars in the meter come close to (but do not touch) the right edge of the scale. ") +
_("If you can't hear the sound, go to the Audio I/O tab of Preferences and check \"Software Playthrough\". ")+
wxT("</ol><br><br>") +
_("There is more help if you're stuck: our Recording FAQs, our Tutorial  <b>Your First Recording </b> in the Manual, and our Wiki ")+WikiLink(wxT("Recording Tips"))+ 
_(", especially the ")+ WikiLink(wxT("Troubleshooting Recordings"))+ _(" section. ")+
wxT("<br><br>")+
ToWelcome()
         );
   }
   if((Key ==wxT("edit") ) || (Key==wxT("grey")))
   {
      return WrapText(
wxString(wxT(""))+
_("Editing: Audacity is a powerful editor, so you'll want to see what it can do with your imported file or recording.  ") + 
_("The main commands you will use are under the Edit menu (such as cut, copy and paste) and under the Effect menu (you can do things like boost the bass, change pitch or tempo, or remove noise). ")+
wxT("<br><br>")+
_("Audacity applies edits to areas of the audio track. You can select a particular area of audio by clicking in the track and dragging the shaded area with your mouse. ") +
_("If you don't select any audio, Audacity will select all that you have on the screen.")+  
wxT("<br><br>")+
_("Two important points when editing:")+
wxT("<br><br>")+
_("(1) If you're still playing or recording, use the yellow Stop button, because you can't edit or export a moving track! ")+
wxT("<br><br>")+
_("(2) On occasion, some of the menus or buttons are greyed out or say \"Disabled\" -  this is normal. ") +
_("For example, because you can't edit audio while playing or recording, we grey out or disable commands until you press Stop. ")+
_("Commands can sometimes be unavailable for other reasons - for example you can't run effects until you have audio on the screen, and you obviously can't paste audio until you've cut or copied it to Audacity's clipboard. ")+
wxT("<br><br>")+
ToWelcome()
         );
   }
   if(Key ==wxT("export") )
   {
      return WrapText(
wxString(wxT(""))+ 
_("Exporting and Saving. have different purposes.")+
_("You \" save\" an Audacity project file when you want to return to your work in Audacity later.")+
_("This means you don't then have to re-import or re-record audio, or redo editing you have done so far.")+
_("When you want to listen to your work in other computer programs or burn it to CD, you \"export\" it as an audio file such as a WAV, AIFF or MP3. ")+ 
wxT("<br><br>")+
_("To save an Audacity project, use <b>File > Save Project</b>. This will save an <b>.aup</b> Project file, plus a <b>_data</b> folder containing the actual audio. ")+
_("Make sure you don't rename or move either of these, because Audacity needs to refer to both.") +
_("Only use File > Save Project As. when you either need to save an empty Project, or if you want to save an existing Project to a <i> new</i> name. ")+
_("When you want to re-open a saved Project, simply click File > Open and open the .aup file. ")+    
wxT("<br><br>")+
_("When you want to export your work to an audio file, use <b>File > Export</b> and choose your desired format in the \"Save as type\" dropdown.")+
_("Click the Options button for advanced choices, such as the bit rate to use for MP3 export.")+
_("There are two other Export commands under the File menu.")+
_("\"Export Selection\" exports only a selected area of audio.")+
_("\"Export Multiple\" exports multiple files, for example where you have more than one track on screen and want to export each as a separate file. ")+
wxT("<br><br>")+
_("Before you can export as <b>MP3</b>, you need to add the LAME MP3 encoder to your computer.")+
_("For help with LAME, see ")+  WikiLink( wxT("Lame_Installation"))+ 
wxT(". ")+
_("There is also some help on how to ")+ Link( wxT("burncd"), _("burn to a CD."))+
wxT("<br><br>")+
ToWelcome()
         );
   }
   if(Key ==wxT("wma-proprietary") )
   {
      return WrapText(
         wxString(wxT(""))+

//#ifdef NOT EXACTLY_AS_IN_FAQ_AS_FAQ_INCORRECT_FOR_1.4._TRY_THIS_LOCALLY?  
wxT("<h2>") +
_("Can Audacity import file formats like WMA, AAC, RealAudio  etc.?") +
wxT("</h2> <p>") +
_("Audacity cannot import or export files in <b>WMA, RealAudio, Shorten (SHN)</b>, or most other proprietary formats, because of licensing and patent restrictions.") +
_("Audacity also cannot import any kind of Digital Rights Management (DRM) protected file, including most purchased online such as from iTunes or Napster.") +
_("If you are on a Mac, you can import <b>AAC</b>  encoded files (such as <b>M4A</b>) as long as they are not DRM protected, but you cannot export to these formats.") +
wxT("</p> <p>")+
_("Some open formats are not yet supported by Audacity, including <b>Ogg Speex</b> and <b>Musepack</b>.") +
_("We hope to support these formats in future versions of Audacity.") +
wxT("</p> <p>")+
_("If you cannot import your file into Audacity, you can as a workround convert it to WAV or AIFF.") +
/* 18n-hint: The URL / name for a piece of software called Super Player gets
 * put on the end of this to complete the sentance */
_("As long as it's not a DRM-protected file, you could do this with iTunes or with ") +
wxT("<a href=\"http://www.erightsoft.com/SUPER.html#Dnload\">SUPER</a> player.") +
_("If it's a DRM-protected file, you can burn it to an audio CD in the application that is licensed to play it, then extract (rip) the CD track to WAV or AIFF.") +
_("You can use iTunes to extract to WAV or AIFF, plus Windows Media Player 11 or <a href=\"http://cdexos.sourceforge.net/?q=download\">CDex</a> on Windows.") +
_("Or you can play the file on your computer and record it - see <a href=\"http://audacity.sourceforge.net/help/faq?s=recording&amp;i=streaming\">. ") 
//#endif
         );
   }
   if(Key ==wxT("burncd") )
   {
      return WrapText(
         wxString(wxT(""))+
_("If you want to burn your work to an audio CD, you must first ")+
 Link( wxT("export"), _("export as WAV or AIFF")) +
 _(" and then burn that file to CD - for more help, see ")+ 
 WikiLink( wxT("How to burn CDs"))+ wxT(".")+
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
_("You can download the help files and then access them from Audacity or you can click <a href=\"*URL*\">here</a> to read the help online.")
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

