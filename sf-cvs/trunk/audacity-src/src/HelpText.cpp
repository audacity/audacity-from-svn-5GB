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

wxString WikiLink( const wxString &Key, const wxString& Text )
{
   return wxString(wxT("")) +
      wxT("<a href='http://www.audacityteam.org/wiki/index.php?title=") +
      Key + 
      wxT("'>") + 
      Text +
      wxT("</a>");
}

wxString LinkExpand( const wxString & Text )
{
   wxString Temp = Text;
   int i,j,k;
   while( (i=Temp.First( wxT("[[") ))!= wxNOT_FOUND )
   {
      wxString Key = Temp.Mid(i+2); 
      j = Key.First( wxT("|") );
      if( j==wxNOT_FOUND )
         return Temp;
      wxString LinkText = Key.Mid( j+1);
      k = LinkText.First( wxT("]]") );
      if( k==wxNOT_FOUND )
         return Temp;
      Key = Key.Mid( 0, j );
      LinkText = LinkText.Mid( 0, k );

      wxString Replacement;
      if( Key.StartsWith( wxT("wiki:") ))
      {
         Replacement = WikiLink( Key.Mid( 5 ), LinkText );
      }
      else
      {
         Replacement = Link( Key, LinkText );
      }

      Temp = Temp.Mid( 0, i ) + Replacement + Temp.Mid( i + j + k + 5 );// 5 for the [[|]]
   }
   return Temp;
}

wxString ToWelcome( )
{
   return _("Back to [[welcome|Welcome]] page.");
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
      return _("Editing and Greyed out Menus");	
   }
   if(Key ==wxT("export") )
   {
      return _("Export and Saving");
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
_("<p>Welcome to Audacity ") + AUDACITY_VERSION_STRING + wxT("! ") +
/** i18n-hint: where you see [[key|text]] translate 'text' and don't translate 'key' */
_("Let's get started!</p><p>\
You can click on the links below to learn how to:\
<ul><li>\
[[play|play back]] an existing sound file.\
<li>How to [[record|record]] a new sound file.\
<li>How to [[edit|edit]] sound and [[export|export]] to a sound file like MP3.\
</ul><br><br>\
Those links will help if you find a problem such as a \
[[grey|menu option greyed out]] or that Audacity \
[[norecord|isn't recording properly]] or that you \
can't [[burncd|burn to a CD]]. You can always get back to these \
instructions by clicking the 'Help' menu, then 'Show Welcome Message.' \
For more detailed reading, please read our full Help by clicking the \
'Help' menu, then 'Index', or <a href=\"http://audacity.sourceforge.net/help/documentation\">download our Manual</a> \
in PDF format.\
<br><br>\
Welcome again to Audacity.")
         );
   }

   if(Key ==wxT("play") )
   {
      return WrapText(
wxString(wxT(""))+
_("You can either drag existing files into Audacity, or use the <b>File > Import > Audio</b> command. \
<b>File > Open</b> does the same, but opens a new Audacity window for any file after the first.\
<br/><br/>\
These are the main formats Audacity will play: <b>AIFF, AU, FLAC, M4A</b> (only on a \
Mac), <b>MP2/MP3, OGG Vorbis, WAV</b>.  If your file is in some other format see (format help). \
If you want to import an audio CD, see [[wiki:How to import CDs|How to import CDs]]<br><br>\
To start playing your audio, press the green Play button. \
To stop, press the yellow Stop button. \
You can also use spacebar to either play or stop. \
When you start playing again after stopping, playback resumes from its previous starting point. \
To change the starting point, use the <b>|&lt;&lt;</b> and <b>&gt;&gt;|</b> buttons \
to skip to the start or end respectively, \
or click anywhere in the track to restart from that point.\
<br><br>")+
ToWelcome()
         );
   }
   if((Key ==wxT("record") ) || (Key ==wxT("norecord") ))
   {
      return WrapText(
wxString(wxT(""))+
ToWelcome()+
wxT("<br><br>")+
_("<b>To start recording, press the red Record button</b>. \
Before doing so however, there are three crucial settings you must check:\
<br><br>\
(1) The <b>Recording Device</b>, set in the Audio I/O tab of Preferences. \
By default Audacity will use the current system device, which is usually your \
inbuilt sound device, so you may not need to set this. \
But if you're using an external USB or Firewire device such as a USB turntable, \
make sure this device is selected as your recording device. \
<br><br>\
(2) The <b>input source</b> for your device, such as microphone or line-in. \
<ul><li>\
<b>On Windows</b>, you normally choose your input in the dropdown selector on \
the right of the <b>Mixer Toolbar<b> <i>note: on Vista, you must do so at Recording \
Device on the Audio I/O Preferences tab.</i>\
</li><li>\
<b>On a Mac</b>, you normally choose input sources outside Audacity, in Apple Audio-MIDI Setup.\
</li></ul>\
<i>Note: many USB or Firewire devices don't have a choice of inputs, so you can ignore this step \
- see the documentation for your device. \
<br><br>\
(3) <b>Input volume</b>. Before recording for real, make a test recording to set the input level, \
so that it's neither too soft or too loud.\
To do this, see this [[wiki:Recording levels|illustrated guide]] or follow these two steps: \
<ol><li>\
Turn on <b>monitoring</b>, by either double-clicking over the right-hand of the two VU Meters, or \
right-clicking over it and choosing \"Start Monitoring\". \
If you do not see the meters, click View > Toolbars and check \"Meter Toolbar\". \
<li>Press Record, and adjust the input volume slider on the Mixer Toolbar (by the microphone \
symbol), so that the red bars in the meter come close to (but do not touch) the right edge of the scale. \
If you can't hear the sound, go to the Audio I/O tab of Preferences and check \"Software Playthrough\". \
</ol><br><br>\
There is more help if you're stuck: our Recording FAQs, our Tutorial  <b>Your First Recording </b> in the \
Manual, and our Wiki [[wiki:Recording Tips|Recording Tips]], especially the \
[[wiki:Troubleshooting Recordings|Troubleshooting Recordings]] section. \
<br><br>") +
ToWelcome()
         );
   }
   if((Key ==wxT("edit") ) || (Key==wxT("grey")))
   {
      return WrapText(
wxString(wxT(""))+
_("Editing: Audacity is a powerful editor, so you'll want to see what \
it can do with your imported file or recording.  \
The main commands you will use are under the <b>Edit</b> menu (such as \
cut, copy and paste) and under the <b>Effect</b> \
menu (you can do things like boost the bass, change pitch or tempo, or remove noise). \
<br><br>\
Audacity applies edits to areas of the audio track. You can select a particular area of \
audio by clicking in the track and dragging the shaded area with your mouse. \
If you don't select any audio, Audacity will select all that you have on the screen.\
<br><br>\
Two important points when editing:\
<br><br>\
(1) If you're still playing or recording, use the yellow Stop button, because you can't \
edit or export a moving track! \
<br><br>\
(2) On occasion, some of the menus or buttons are greyed out or \
say \"Disabled\" -  this is normal. \
For example, because you can't edit audio while playing or recording, \
we grey out or disable commands until you press Stop. \
Commands can sometimes be unavailable for other reasons - \
for example you can't run effects until you \
have audio on the screen, and you obviously can't paste audio until \
you've cut or copied it to Audacity's clipboard. \
<br><br>")+
ToWelcome()
         );
   }
   if(Key ==wxT("export") )
   {
      return WrapText(
wxString(wxT(""))+ 
_("Exporting and Saving have different purposes.\
You \"save\" an Audacity project file when you want to return to your work in Audacity later.\
This means you don't then have to re-import or re-record audio, or redo editing you have done so far.\
When you want to listen to your work in other computer programs you \"export\" it as an audio file such as \
a WAV, AIFF or MP3. When burning to CD, you export as WAV or AIFF.\
<br><br>\
To save an Audacity project, use <b>File > Save Project</b>. This will save an <b>.aup</b> Project file, \
plus a <b>_data</b> folder containing the actual audio. \
Make sure you don't rename or move either of these, because Audacity needs to refer to both.\
Only use File > Save Project As... when you either need to save an empty Project, or if you want \
to save an existing Project to a <i> new</i> name. \
When you want to re-open a saved Project, simply click File > Open and open the .aup file. \
<br><br>\
When you want to export your work to an audio file, use <b>File > Export</b> and choose your desired \
format in the \"Save as type\" dropdown.\
Click the Options button for advanced choices, such as the bit rate to use for MP3 export.\
There are two other Export commands under the File menu.\
\"Export Selection\" exports only a selected area of audio.\
\"Export Multiple\" exports multiple files, for example where you have more than one track on \
screen and want to export each as a separate file. If exporting tracks from a recorded LP or tape to multiple files, \
use [[wiki:Splitting recordings into separate tracks|labels with Export Multiple]]. \
<br><br>\
Before you can export as <b>MP3</b>, you may need to add the LAME MP3 encoder to your computer.\
For help with LAME, see [[wiki:Lame_Installation|Lame Installation]]. \
There is also some help on how to [[burncd|burn to a CD]].\
<br><br>")+
ToWelcome()
         );
   }
   if(Key ==wxT("wma-proprietary") )
   {
      return WrapText(
         wxString(wxT(""))+

//#ifdef NOT EXACTLY_AS_IN_FAQ_AS_FAQ_INCORRECT_FOR_1.4._TRY_THIS_LOCALLY?  
_("<h2>Can Audacity import file formats like WMA, AAC, RealAudio  etc.?</h2> <p>\
Audacity cannot import or export files in <b>WMA, RealAudio, Shorten (SHN)</b>, or\
most other proprietary formats, because of licensing and patent restrictions. \
Audacity also cannot import any kind of Digital Rights Management (DRM) protected \
file, including most purchased online such as from iTunes or Napster.\
If you are on a Mac, you can import <b>AAC</b>  encoded files (such as <b>M4A</b>) \
as long as they are not DRM protected, but you cannot export to these formats.\
</p> <p>\
Some open formats are not yet supported by Audacity, including <b>Ogg Speex</b> and <b>Musepack</b>.\
We hope to support these formats in future versions of Audacity.\
</p> <p>\
If you cannot import your file into Audacity, you can as a workround convert it to WAV or AIFF.\
As long as it's not a DRM-protected file, you could do this with iTunes or with \
<a href=\"http://www.erightsoft.com/SUPER.html#Dnload\">SUPER</a> player.\
If it's a DRM-protected file, you can burn it to an audio CD in the application \
that is licensed to play it, then extract (rip) the CD track to WAV or AIFF.\
You can use iTunes to extract to WAV or AIFF, plus Windows Media Player 11 or \
<a href=\"http://cdexos.sourceforge.net/?q=download\">CDex</a> on Windows.\
Or you can play the file on your computer and record it - \
see <a href=\"http://audacity.sourceforge.net/help/faq?s=recording&amp;i=streaming\">.")
//#endif
         );
   }
   if(Key ==wxT("burncd") )
   {
      return WrapText(
         wxString(wxT(""))+
_("If you want to burn your work to an audio CD, you must first \
[[export|export as WAV or AIFF]] \
and then burn that file to CD with a CD burning program like \
iTunes - for more help, see [[wiki:How to burn CDs|How to burn CDs]].\
<br><br>")+
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
_("<p>You don't appear to have Audacity help files on your machine. \
You can <a href=\"http://audacity.sourceforge.net/help/documentation\">download</a> the help files and then access \
them from Audacity as you have just attempted, or you can \
click <a href=\"*URL*\">here</a> to read the help online.")
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

   wxString Text;
   Text = HelpTextBuiltIn( Key );

   if( !Text.IsEmpty())
      return LinkExpand( Text );

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

