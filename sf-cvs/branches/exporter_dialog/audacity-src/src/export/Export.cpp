/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/defs.h>
#include <wx/file.h>
#include <wx/timer.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/sizer.h>

#include "Exporter.h"
#include "CLExporter.h"
#include "FLACExporter.h"
#include "MP3Exporter.h"
#include "LabelExporter.h"
#include "OggExporter.h"
#include "PCMExporter.h"


#include "Export.h"

//Use of the next files is deprecated.
#include "ExportPCM.h"
//#include "ExportMP3.h"
#include "ExportOGG.h"
#include "ExportCL.h"


#include "sndfile.h"

#include "../Audacity.h"
#include "../AudioIO.h"
#include "../DirManager.h"
#include "../FileFormats.h"
#include "../LabelTrack.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Track.h"
#include "../WaveTrack.h"

 
#include <iostream>


//These get called by menu functions. To be fixed.
bool Export(AudacityProject *project,
            bool selectionOnly, double t0, double t1)
{
  FormatSelectionDialog *  select = new FormatSelectionDialog(project, selectionOnly,t0,t1);
  if(select->ShowModal() == wxID_CANCEL)
    {
      return false;
    }
  return true;
}

bool ExportLossy(AudacityProject *project,
                 bool selectionOnly, double t0, double t1)
{
  FormatSelectionDialog *  select = new FormatSelectionDialog(project, selectionOnly,t0,t1);
  if(select->ShowModal() == wxID_CANCEL)
    {
      return false;
    }
  return true;

}


//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
BEGIN_EVENT_TABLE(FormatSelectionDialog, wxDialog)
  EVT_CHOICE(FSD_FORMAT_CHOOSER,       FormatSelectionDialog::OnSelectFormat)
  EVT_CHOICE(FSD_SECONDARY_CHOOSER,    FormatSelectionDialog::OnOptions)
  EVT_CHOICE(FSD_SAMPLE_RATE_CHOOSER,  FormatSelectionDialog::OnRate)
  EVT_BUTTON(FSD_OK,  FormatSelectionDialog::OnOK)
  EVT_BUTTON(FSD_CANCEL, FormatSelectionDialog::OnCancel)
  EVT_SIZE(FormatSelectionDialog::OnSize)

END_EVENT_TABLE()
  ;


FormatSelectionDialog::FormatSelectionDialog(AudacityProject * project, bool selection,double t0, double t1):
  wxDialog(project, -1, _("Select Export Format"), wxDefaultPosition,
	   wxSize(500,500),  wxTHICK_FRAME, _("Selection Format Dialog")),
  mProject(project),
  mCurrentFormatChoice(FORMAT_TYPE_UNKNOWN),
  mFileName(""),
  m_sf_Format(0),
  m_sf_Encoding(0),
  m_sf_Endianness(0),
  mEncodings(NULL),
  mSelectionOnly(selection),
  m_t0(t0),
  m_t1(t1)
{

  //Create a top-level sizer, which hase to sizers inside it, 
  //spaced horizontally.
  mainSizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer * topSizer = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer * leftSizer = new wxBoxSizer(wxVERTICAL);
  mSecondaryOptionSizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer * bottomSizer = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer * tmpSizer;
    

  //------------------------------------------------------  
  //Create the format chooser.
  //------------------------------------------------------  
  //numFormats will be all formats provided by lsf, plus
  //.mp3, .ogg, and .flac, command-line, and text labels. 
  //mNumAvailableFormats tells us how many are available currently

  int numFormats = sf_num_headers() + 5;
  
  //Hold the names of each format string in this array:
  wxString *formatStrings = new wxString[numFormats];
  mFormats = new FormatType[numFormats];


  //Iterate through each format, adding it to the string array.
  mNumAvailableFormats = 0;
  //Do the non-SF formats first.

  if(USE_LIBVORBIS)
    {
      formatStrings[mNumAvailableFormats] = _("Ogg Vorbis");
      mFormats[mNumAvailableFormats] = FORMAT_TYPE_OGG;
      mNumAvailableFormats++;
    }
  
  formatStrings[mNumAvailableFormats] = _("MP3 (Lame Encoder)");
  mFormats[mNumAvailableFormats] = FORMAT_TYPE_MP3;
  mNumAvailableFormats++;


  //Is there a USE_FLAC or something defined when FLAC is available?
#ifndef USE_FLAC
#define USE_FLAC 0
#endif

  if(USE_FLAC) //This should not be loaded if unavailable: 
    {
      formatStrings[mNumAvailableFormats] = _("FLAC (Lossless format)");
      mFormats[mNumAvailableFormats] = FORMAT_TYPE_FLAC;
      mNumAvailableFormats++;
    }
  
  formatStrings[mNumAvailableFormats] = _("Text Labels");
  mFormats[mNumAvailableFormats] = FORMAT_TYPE_LABELS;
  mNumAvailableFormats++;


  formatStrings[mNumAvailableFormats] = _("Command-Line Exporter");
  mFormats[mNumAvailableFormats] = FORMAT_TYPE_COMMANDLINE;
  mNumAvailableFormats++;
  
  for(int i =0; i < sf_num_headers(); i++)
    {
      formatStrings[i+mNumAvailableFormats] = sf_header_index_name(i);
      
      //This is a nasty kludge. Convert sf format types with a cast to our format, which happens to be identical.
      mFormats[i+mNumAvailableFormats]      = (FormatType)sf_header_index_to_type(i);
    }
  
  
  //Create a chooser from the formatStrings array:
  mFormatChooser = new wxChoice(this, FSD_FORMAT_CHOOSER,
				wxDefaultPosition, wxDefaultSize,
				mNumAvailableFormats+sf_num_headers(), formatStrings);

  //By default, export to microsoft wave.... (should be whatever was exported to last, or 
  //what is stored in the preferences.)
  mFormatChooser->SetSelection(mNumAvailableFormats + 10);
  

  //Create new tmpSizer, add label and chooser to it, then add it to panel.
  tmpSizer= new wxBoxSizer(wxHORIZONTAL);
  tmpSizer->Add(new wxStaticText(this, -1, _("Select type of file:")), 0, wxALL|wxALIGN_CENTER_VERTICAL, 0);
 //Add the primary format chooser to the left sizer.
  tmpSizer->Add(mFormatChooser,1,wxALL | wxALIGN_CENTER, 0);
  leftSizer->Add(tmpSizer,1,wxALL | wxALIGN_CENTER,10);

  
  //------------------------------------------------------  
  //Create the channels chooser.  
  //------------------------------------------------------  
  
  // This chooser gives you the option of exporting mone, stereo,
  // or with each track in its own channel. It defaults to whatever
  // is set in the preferences.

  //Determine how many tracks (of each kind) are selected.
  TrackList *tracks = project->GetTracks();
  TrackListIterator iter1(tracks);
  Track *tr = iter1.First();
  int numSelected = 0, numLeft = 0, numRight = 0, numMono = 0;
  float earliestBegin = m_t1;
  float latestEnd = m_t0;


  //Iterate through all the tracks, keeping track of how many of each type
  //exist and determine what the left and right export regions are.
  while (tr) {
    if (tr->GetKind() == Track::Wave) {
      if (tr->GetSelected() || !mSelectionOnly) {
	
	numSelected++;

	if (tr->GetChannel() == Track::LeftChannel)
	  numLeft++;
	else if (tr->GetChannel() == Track::RightChannel)
	  numRight++;
	else if (tr->GetChannel() == Track::MonoChannel)
	  numMono++;


	if(tr->GetOffset() < earliestBegin)
	  earliestBegin = tr->GetOffset();
	
	if(tr->GetEndTime() > latestEnd)
	  latestEnd = tr->GetEndTime();

         }
    }

    
    
    if(m_t0 < earliestBegin)
      m_t0 = earliestBegin;
    
    if(m_t1 > latestEnd)
      m_t1 = latestEnd;
    
    tr = iter1.Next();
  }
  mNumTotalChannels = numSelected;



  mChannelChooser = new wxChoice(this, FSD_CHANNEL_CHOOSER,
				 wxDefaultPosition,  wxSize(200, -1),
				 0, NULL);

  
  mChannelChooser->Append(_("1 (mono)"));
  mChannelChooser->Append(_("2 (stereo--Mixed)"));

  if(numSelected > 2)
    {
      wxString tmp = numSelected + _(" (each a separate track)");
      mChannelChooser->Append(tmp);
    }
  //The default selection should depend on what was in the project.  If there were
  //no left or right tracks, export mono by default.  If there were any
  //left or right tracks, export stereo.
  
  if((numLeft + numRight ) == 0)       //Mono
    {
      mChannelChooser->SetSelection(0);
      mNumExportedChannels = 1;
    }
  else                                 //Stereo
    {
      mChannelChooser->SetSelection(1);
      mNumExportedChannels = 2;
    }
  
  //Add controls to a sizer and add sizer to left panel.
  tmpSizer= new wxBoxSizer(wxHORIZONTAL);
  tmpSizer->Add(new wxStaticText(this, -1, _("Select number of channels to export:")), 0, 
		 wxALL|wxALIGN_CENTER_VERTICAL, 0);
  tmpSizer->Add(mChannelChooser,1,wxALL | wxALIGN_CENTER, 0);
  leftSizer->Add(tmpSizer, 0, wxALL|wxALIGN_CENTER_VERTICAL, 10);

  //------------------------------------------------------  
  //Create a sample rate selector.
  //------------------------------------------------------  
  wxArrayLong sampleRates = AudioIO::GetSupportedSampleRates();
  

  //Get the project sample rate and set the default export rate to the project rate.
  double rate = mProject->GetRate();
  
  int pos = sampleRates.GetCount(); // Fall back to "Other..."
  for (int i = 0; i < (int)sampleRates.GetCount(); i++)
    {
      if (rate == sampleRates[i]) 
	{
	  pos = i;
	  break;
	}
    }
  
  wxString *stringRates = new wxString[sampleRates.GetCount() + 1];
      
   
  for (int i = 0; i < (int)sampleRates.GetCount(); i++)
    {
      int sampleRate = (int)sampleRates[i];
      stringRates[i] = wxString::Format("%i Hz", sampleRate);
    }
  
  //The last choice control option is "Other...", which if selected will allow user to enter their own rate.
  stringRates[sampleRates.GetCount()] = _("Other...");
  mSampleRates = new wxChoice(this, FSD_SAMPLE_RATE_CHOOSER, wxDefaultPosition, wxDefaultSize,
			      (int)sampleRates.GetCount() + 1, stringRates);
  mSampleRates->SetSelection(pos);

  
  mOtherSampleRate = NULL;
  mOtherSampleRate = new wxTextCtrl( this, -1, wxString::Format("%i", static_cast<int>(rate)),
				     wxDefaultPosition, wxSize(50, -1), 0 );
  
  mOtherSampleRate->Enable(pos == (int)sampleRates.GetCount() + 1);
  

  
 //(re)create a sizer, and add label, chooser, and textbox to it. Then add it to left panel.
  tmpSizer= new wxBoxSizer(wxHORIZONTAL);
  tmpSizer->Add(new wxStaticText(this, -1, _("Select Sample Rate:")), 0,  wxALL|wxALIGN_CENTER_VERTICAL, 0);
  tmpSizer->Add( mSampleRates, 0, wxALL|wxALIGN_CENTER_VERTICAL, 0 );
  tmpSizer->Add( mOtherSampleRate, 0, wxALL|wxALIGN_CENTER_VERTICAL, 0 );
  leftSizer->Add( tmpSizer, 0, wxALL|wxALIGN_CENTER_VERTICAL, 10 );
  
  
  delete[] stringRates;
  //-------------------------------------------------------------------
  //Create a (blank) encoding chooser. Filled in when a format is chosen
  //--------------------------------------------------------------------
  
  mOptionChooser = new wxChoice(this, FSD_SECONDARY_CHOOSER,
				wxDefaultPosition,wxSize(200, -1),
				0, NULL);
  mSecondaryOptionSizer->Add(mOptionChooser,1, wxALL | wxALIGN_CENTER, 10);


  topSizer->Add(leftSizer,1, wxALL | wxALIGN_CENTER, 10);
  topSizer->Add(mSecondaryOptionSizer,1, wxALL | wxALIGN_CENTER, 10);
  
  mainSizer->Add(topSizer,1, wxALL | wxALIGN_CENTER,10);
  
  //-----------------
  //Now, the bottom sizer.

  mOK = new wxButton(this, FSD_OK, _("OK") );
  mCancel = new wxButton(this, FSD_CANCEL, _("Cancel") );
  bottomSizer->Add(mCancel,1,wxALL | wxALIGN_CENTER,10);
  bottomSizer->Add(mOK,1, wxALL | wxALIGN_CENTER,10);
  mainSizer->Add(bottomSizer,1, wxALL | wxALIGN_CENTER,10);


  SetAutoLayout(true);
  
  wxCommandEvent evt;
  OnSelectFormat(evt);
  OnResize(evt);
}


///Standard destructor.
FormatSelectionDialog::~FormatSelectionDialog()
{
}



/// This callback gets executed whenever the main option
/// selecting the primary format for exporting changes.
void FormatSelectionDialog::OnSelectFormat(wxCommandEvent &event)
{

  //Recode the selected index of the choice box
  //to FormatType and sf formats.

  mCurrentFormatChoice = mFormats[mFormatChooser->GetSelection()];
  
  //Get rid of all the items in mOptionChooser
  mOptionChooser->Clear();
  

  
  switch(mCurrentFormatChoice)
    {
    case FORMAT_TYPE_OGG:         //Ogg Vorbis
      break;
      
    case FORMAT_TYPE_MP3:         //MP3
      {

	//Give user option of these bitrates to export to:
	int numBitrates = 16;
	wxString bitrates[] = { "16", "24", "32", "40", "48", "56", "64",
				"80", "96", "112", "128", "160",
				"192", "224", "256", "320" };
	mEncodings = new int[numBitrates];
	

	//Rebuild the chooser with the above options
	for(int i = 0; i< numBitrates; i++)
	  {
	    mOptionChooser->Append(bitrates[i]);
	    mEncodings[i]=atoi(bitrates[i]);
	  }
	
	//This sets the initial selection. It *should* see what
	//the project or default is and use that by default.
	mOptionChooser->SetSelection(12);
	
	wxCommandEvent evt;
	OnOptions(evt);
	
      }
      break;


    case FORMAT_TYPE_FLAC:         //FLAC
      break;

    case FORMAT_TYPE_LABELS:        //Text labels 
      break;
 
    case FORMAT_TYPE_COMMANDLINE:   //Command-line export
      break;


    case FORMAT_TYPE_SF_WAV:   //Start of LibSndFile Formats
    case FORMAT_TYPE_SF_AIFF:
    case FORMAT_TYPE_SF_AU:
    case FORMAT_TYPE_SF_RAW:
    case FORMAT_TYPE_SF_PAF:
    case FORMAT_TYPE_SF_SVX:
    case FORMAT_TYPE_SF_NIST:
    case FORMAT_TYPE_SF_VOC:
    case FORMAT_TYPE_SF_IRCAM:
    case FORMAT_TYPE_SF_W64:
    case FORMAT_TYPE_SF_MAT4:
    case FORMAT_TYPE_SF_MAT5:  

      
      {
	//Create an array of labels.  We may not use all of them.
	//To do this, make a SF_INFO structure for each type, and the
	//call sf_format_check() on it.  If it comes back true, the
	//parameters were valid, and we should add it to the array.

	m_sf_Format = mCurrentFormatChoice;


	int numEncodings = sf_num_encodings();
	mEncodings = new int[numEncodings];
	

	int usedEncodings = 0;
	int tmpFormat=0;
	for(int i=0; i<numEncodings; i++)
	  {
	    //The format is a bitwise or (|) of the format and the subtype.
	    tmpFormat =m_sf_Format | sf_encoding_index_to_subtype(i);
	    SF_INFO info = {44100, 1, mNumExportedChannels, tmpFormat,1,1};

	    if(sf_format_check(&info))
	      {
		//Add the valid name to the chooser.
		mOptionChooser->Append(sf_encoding_index_name(i));
		
		//Keep track of its actual sf code.
		mEncodings[usedEncodings]=sf_encoding_index_to_subtype(i);
		usedEncodings++;
	      }
	  }
	
	
	//This sets the initial selection. It *should* see what
	//the project or default is and use that by default.
	mOptionChooser->SetSelection(1);
	
	wxCommandEvent evt;
	OnOptions(evt);
	

      }

      break;
    default:
      break;
    }


}


/// This callback gets executed whenever the rate-change slider moves.
void FormatSelectionDialog::OnRate(wxCommandEvent & event)
{
  int sel = mSampleRates->GetSelection();
  mOtherSampleRate->Enable(sel == mSampleRates->GetCount() - 1);
  mRate = 44100;
}

void FormatSelectionDialog::OnChannelsChange(wxCommandEvent & event){

  int index = mChannelChooser->GetSelection();
  if (index < 2)
    mNumExportedChannels = index + 1;
  else
    mNumExportedChannels = mNumTotalChannels;
  
  wxCommandEvent evt;
  OnSelectFormat(evt);
}


/// This callback gets executed whenever the secondary option
/// choice box is acted on.
void FormatSelectionDialog::OnOptions(wxCommandEvent &event)
{
  //The selected encoding has changed.  update m_sf_Encoding.
  int index = mOptionChooser->GetSelection();

  if( mCurrentFormatChoice == FORMAT_TYPE_SF_WAV 
      || mCurrentFormatChoice == FORMAT_TYPE_SF_AIFF
      || mCurrentFormatChoice == FORMAT_TYPE_SF_AU
      || mCurrentFormatChoice == FORMAT_TYPE_SF_RAW
      || mCurrentFormatChoice == FORMAT_TYPE_SF_PAF
      || mCurrentFormatChoice == FORMAT_TYPE_SF_SVX
      || mCurrentFormatChoice == FORMAT_TYPE_SF_NIST
      || mCurrentFormatChoice == FORMAT_TYPE_SF_VOC
      || mCurrentFormatChoice == FORMAT_TYPE_SF_IRCAM
      || mCurrentFormatChoice == FORMAT_TYPE_SF_W64
      || mCurrentFormatChoice == FORMAT_TYPE_SF_MAT4
      || mCurrentFormatChoice == FORMAT_TYPE_SF_MAT5)  
    
    m_sf_Encoding = mEncodings[index];
  

}


/// This callback gets executed whenever the Cancel button gets clicked.
void FormatSelectionDialog::OnCancel(wxCommandEvent &event)
{
  EndModal(wxID_CANCEL);
}


/// This callback gets executed whenever the OK button gets clicked.
void FormatSelectionDialog::OnOK(wxCommandEvent &event)
{

  //The OK button should create the proper export handler and export the stuff.



  //Now, create an exporter with proper parameters; also create the proper
  //filename extension to send to the file dialog.
  Exporter * tmpExporter = NULL;
  wxString defaultExtension;
  wxString formatStr;
  
  switch(mCurrentFormatChoice)
    {
    case FORMAT_TYPE_OGG:         //Ogg Vorbis
      formatStr = _("Ogg Vorbis");
      defaultExtension = ".ogg";
	break;
      
    case FORMAT_TYPE_MP3:         //MP3


      formatStr = "MP3";
      defaultExtension = ".mp3";
      
      //Determine what platform you are on. Depending on
      //this, we use a different mp3exporter subclass.

      

      tmpExporter = new PlatformMP3Exporter(mProject, m_t0, m_t1, 
					    mSelectionOnly, mRate, 
					    mNumExportedChannels,
					    128,
					    5);


      break;


    case FORMAT_TYPE_FLAC:         //FLAC
      formatStr = _("Free Lossless Audio Compression (flac)");
      defaultExtension = ".flac";
      break;



    case FORMAT_TYPE_SF_WAV:   //Start of LibSndFile Formats
    case FORMAT_TYPE_SF_AIFF:
    case FORMAT_TYPE_SF_AU:
    case FORMAT_TYPE_SF_RAW:
    case FORMAT_TYPE_SF_PAF:
    case FORMAT_TYPE_SF_SVX:
    case FORMAT_TYPE_SF_NIST:
    case FORMAT_TYPE_SF_VOC:
    case FORMAT_TYPE_SF_IRCAM:
    case FORMAT_TYPE_SF_W64:
    case FORMAT_TYPE_SF_MAT4:
    case FORMAT_TYPE_SF_MAT5:  
      
      {
	//Libsndfile keeps track of the format with an or'ed
	//format code (i.e., the header format),
	//  along with an encoding format for that type (float, int, etc.)
	int format = m_sf_Format | m_sf_Encoding;
	
	
	tmpExporter = new PCMExporter(mProject, m_t0, m_t1,
				      mSelectionOnly, mRate,
				      mNumExportedChannels, format);
	
	formatStr = sf_header_name(m_sf_Format);
	defaultExtension = "." + sf_header_extension(m_sf_Format);

      }
      break;
    case FORMAT_TYPE_LABELS:
      formatStr = _("Text Labels");
      defaultExtension = ".txt";

      break;


    case FORMAT_TYPE_COMMANDLINE:
      formatStr = _("Command-Line Exporter");
      defaultExtension = ".dat";

      break;
      
    default:
      return EndModal(wxID_CANCEL);
      
    }
  
  //We need to get the filename. Call a common dialog box, with appropriate
  //file extensions:
  /* Prepare and display the filename selection dialog */

   wxString path = gPrefs->Read("/DefaultExportPath",::wxGetCwd());
   wxString nameOnly;
   wxString extension;
   wxString defaultName = mProject->GetName();
   wxString fName;
   wxString maskString;

   if (defaultExtension.Left(1) == ".")
      defaultExtension =
         defaultExtension.Right(defaultExtension.Length()-1);

   maskString.Printf("%s files (*.%s)|*.%s|All files (*.*)|*.*", 
		     (const char *)formatStr,
                     (const char *)defaultExtension,
		     (const char *)defaultExtension);

   bool fileOkay;

   do {
      fileOkay = true;

      fName = defaultName + "." + defaultExtension;
      fName = wxFileSelector(wxString::Format(_("Save %s File As:"),
                                              (const char *) formatStr),
                             path,
                             fName,       // default file name
                             defaultExtension,
                             maskString,
                             wxSAVE | wxOVERWRITE_PROMPT);
      
      if (fName.Length() >= 256)
	{
	  wxMessageBox
            (_("Sorry, pathnames longer than 256 characters not supported."));
	  delete tmpExporter;
	  return;
	}
      
      if (fName == "")
	{
	  delete tmpExporter;
	  return;
	}

 
      ::wxSplitPath(fName, &path, &nameOnly, &extension);

      //
      // Make sure the user doesn't accidentally save the file
      // as an extension with no name, like just plain ".wav".
      //
 
      if ((nameOnly.Left(1)=="." && extension=="") ||
          (nameOnly=="" && extension!="")) {
         wxString prompt =
            "Are you sure you want to save the file as \""+
            ::wxFileNameFromPath(fName)+"\"?\n";
         
         int action = wxMessageBox(prompt,
                                   "Warning",
                                   wxYES_NO | wxICON_EXCLAMATION,
                                   mProject);
         
         fileOkay = (action == wxYES);
         continue;
      }

      //
      // Check the extension - add the default if it's not there,
      // and warn user if it's abnormal.
      //
 
      wxString defaultExtension3 = defaultExtension;
      if (defaultExtension.Length() > 3)
         defaultExtension = defaultExtension.Left(3);
      
      if (extension == "") {
#ifdef __WXMSW__
	// Windows prefers 3-char uppercase extensions
         extension = defaultExtension;
#else
         // Linux and Mac prefer lowercase extensions
         extension = defaultExtension.Lower();
#endif
      }
      else if (extension.Upper() != defaultExtension.Upper() &&
               extension.Upper() != defaultExtension3.Upper()) {
#ifdef __WXMSW__
         // Windows prefers 3-char extensions
         defaultExtension3 = defaultExtension3;
#endif
 
         wxString prompt;
         prompt.Printf("You are about to save a %s file with the name %s.\n"
                       "Normally these files end in %s, and some programs "
                       "will not open files with nonstandard extensions.\n"
                       "Are you sure you want to save the file "
                       "under this name?",
                       (const char *)formatStr,
                       (const char *)("\""+nameOnly+"."+ extension +"\""),
                       (const char *)("\"."+defaultExtension+"\""));

         int action = wxMessageBox(prompt,
                                   "Warning",
                                   wxYES_NO | wxICON_EXCLAMATION,
                                   mProject);
 
         if (action == wxYES)
            fileOkay = true;
         else {
            fileOkay = false;
            defaultName = nameOnly + "." + extension;
         }
      }

      mFileName = path + wxFILE_SEP_PATH + nameOnly + "." + extension;
   } while(!fileOkay);

   /*
    * Ensure that exporting a file by this name doesn't overwrite
    * one of the existing files in the project.  (If it would
    * overwrite an existing file, DirManager tries to rename the
    * existing file.)
    */

   if (!mProject->GetDirManager()->EnsureSafeFilename(wxFileName(fName)))
     {
       wxMessageBox("Trying to overwrite another file.");
       delete tmpExporter;
       return EndModal(wxID_CANCEL);
     }
   gPrefs->Write("/DefaultExportPath", path);


   /*
    * To be even MORE safe, create a temporary file name based
    * on this one...
    */

   wxString actualName = fName;

   int suffix = 0;
   while(::wxFileExists(fName)) {
      fName = path + wxFILE_SEP_PATH + 
         nameOnly + wxString::Format("%d", suffix) + "." + extension;
      suffix++;
   }

   bool success = false;
   // Verify whether the format is OK, and then
   // export the file if it is.
   if(tmpExporter->Verify())
     {

       success = tmpExporter->Export(fName);
     }
   else
     {

       //Should have a 
       wxMessageBox(_("Unable to export in selected format."));
       delete tmpExporter;
       EndModal(wxID_OK);
     }

   if (success && actualName != fName)
      ::wxRenameFile(fName, actualName);
   delete tmpExporter;
  EndModal(wxID_OK);
}


void FormatSelectionDialog::OnResize(wxCommandEvent & event)
{
  //Make the mainSizer be the primary sizer for the dialog.

  mainSizer->Fit(this);
  mainSizer->SetSizeHints(this);
  SetSizer(mainSizer);
}
