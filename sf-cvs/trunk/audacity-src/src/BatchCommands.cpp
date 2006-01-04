/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchCommands.cpp

  Dominic Mazzoni
  James Crook

**********************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/msgdlg.h>
#include <wx/filedlg.h>
#include <wx/textfile.h>

#include "Project.h"
#include "BatchCommands.h"
#include "commands/CommandManager.h"
#include "effects/Effect.h"
#include "../images/Arrow.xpm"
#include "Internat.h"
#include "../Prefs.h"
#include "Shuttle.h"
#include "../export/ExportMP3.h"
#include "../export/ExportOGG.h"
#include "../export/ExportPCM.h"

// KLUDGE: All commands should be on the same footing
// however, for historical reasons we distinguish between
//    - Effects (which are looked up in effects lists)
//    - Menu commands (which are held in command manager)
//    - Specials (which we deal with specially here)
enum eCommandType { CtEffect, CtMenu, CtSpecial };

// TIDY-ME: Not currently translated, 
// but there are issues to address if we do.  
wxString SpecialCommands[] = {
   wxT("No Action"),
   wxT("Import"),
   wxT("Save Hq Master1"),
   wxT("Save Hq Master2"),
   wxT("Stereo To Mono"),
   wxT("ExportMp3"),
   wxT("ExportWav")
};


BatchCommands::BatchCommands()
{
   ResetChain();
}

void BatchCommands::WriteChain()
{
   int i;
   wxString PrefName;
   gPrefs->DeleteGroup( wxT("Batch/Chain") );
   for(i=0;i<(int)mCommandChain.GetCount();i++)
   {
      PrefName = wxString::Format( wxT("/Batch/Chain/Command%02i"), i);
      gPrefs->Write(PrefName, mCommandChain[i] + wxT(":") + mParamsChain[i]);
   }
}

void BatchCommands::ReadChain()
{
//   SetCleanSpeechChain();
   ResetChain();
   int i;
   int splitAt;
   const int maxCommands = 20;
   wxString PrefName;
   wxString Value;
   for(i=0;i<maxCommands;i++)
   {
      PrefName = wxString::Format( wxT("/Batch/Chain/Command%02i"), i);
      Value = gPrefs->Read(PrefName, wxT(""));
      if( Value.IsEmpty() )
         return;
      splitAt = Value.Find( wxT(':') );
      if( splitAt < 0 )
         return;
      mCommandChain.Add( Value.Mid( 0,splitAt ));
      mParamsChain.Add( Value.Mid( splitAt+1));
   }
}

void BatchCommands::LoadChain( wxWindow *parent )
{
   // Get the initial path
   wxString pName = gPrefs->Read( wxT("/Batch/DefaultChainPath"),
                                  FROMFILENAME( ::wxGetCwd() ) );

   // Prompt user for a file name
   wxString fName = wxFileSelector( _("Select a command chain file..."),
                                    pName,         // Path
                                    wxT(""),       // Name
                                    wxT(""),       // Extension
                                    _("Text files (*.txt)|*.txt|All files (*.*)|*.*"),
                                    0,             // Flags
                                    parent);       // Parent

   // Nothing to do
   if( !fName )
      return;

   // Convert name
   fName = FROMFILENAME( fName );

   // Remember path
   gPrefs->Write( wxT("/Batch/DefaultChainPath"), wxPathOnly( fName ) );

   // Remember last filename
   gPrefs->Write( wxT("/Batch/LastFile"), fName );

   // Set the file name
   wxTextFile tf( fName );

   // Open and check
   tf.Open();
   if( !tf.IsOpened() )
   {
      // wxTextFile will display any errors
      return;
   }

   // Clear any previous chain
   ResetChain();

   // Load commands from the file
   int i;
   int lines = tf.GetLineCount();
   int splitAt;
   wxString cmd;
   wxString parm;
   for( i = 0; i < lines; i++ )
   {
      // Find the command name terminator...ingore line if not found
      splitAt = tf[ i ].Find( wxT(':') );
      if( splitAt < 0 )
         continue;

      // Parse and clean
      cmd = tf[ i ].Left( splitAt ).Trim( false ).Trim( true );
      parm = tf[ i ].Mid( splitAt + 1 ).Trim( false ).Trim( true );

      // Add to lists
      mCommandChain.Add( cmd );
      mParamsChain.Add( parm );
   }

   // Done with the file
   tf.Close();
}

void BatchCommands::SaveChain( wxWindow *parent )
{
   // Get the initial path
   wxString pName = gPrefs->Read( wxT("/DefaultExportPath"),
                                  FROMFILENAME( ::wxGetCwd() ) );

   // Get last used filename
   wxString fName = gPrefs->Read( wxT("/Batch/LastFile"),
                                  FILENAME( wxT("Chain.txt") ) );

   // Prompt user for a file name
   fName = wxFileSelector( _("Save command chain As:"),
                           NULL,                         // Path
                           fName,                        // Name
                           wxT("txt"),                   // Extension
                           _("Text files (*.txt)|*.txt|All files (*.*)|*.*"),
                           wxSAVE | wxOVERWRITE_PROMPT,  // Flags
                           parent );                     // Parent

   // Nothing to do
   if( !fName )
      return;

   // Convert name
   fName = FROMFILENAME( fName );

   // Remember path
   gPrefs->Write( wxT("/DefaultChainPath"), wxPathOnly( fName ) );

   // Remember last filename
   gPrefs->Write( wxT("/Batch/LastFile"), fName );

   // Set the file name
   wxTextFile tf( fName );

   // (Possibly) create new file
   if( !tf.Exists() )
   {
      tf.Create();
   }

   // Open and check
   tf.Open();
   if( !tf.IsOpened() )
   {
      // wxTextFile will display any errors
      return;
   }

   // Start with a clean slate
   tf.Clear();

   // Copy over the commands
   int i;
   int lines = mCommandChain.GetCount();
   for( i = 0; i < lines; i++ )
   {
      tf.AddLine( mCommandChain[i] + wxT(":") + mParamsChain[ i ] );
   }

   // Write the chain
   tf.Write();

   // Done with the file
   tf.Close();
}



void BatchCommands::SetCleanSpeechChain()
{
   ResetChain();

// TIDY-ME: Effects change their name with localisation.
// Commands (at least currently) don't.  Messy.

/* i18n-hint: Effect name translations must agree with those used elsewhere, or batch won't find them */
   AddToChain( wxT("Import") );
   AddToChain(   _("Stereo To Mono") );
   AddToChain(   _("Normalize") );
   AddToChain( wxT("Save Hq Master1") );
   AddToChain(   _("Noise Removal") );
   AddToChain(   _("Truncate Silence") );
   AddToChain(   _("Leveller") );
   AddToChain(   _("Normalize") );
   AddToChain( wxT("ExportMp3") );
}

void BatchCommands::SetWavToMp3Chain()
{
   ResetChain();
   AddToChain( wxT("Import") );
   AddToChain(   _("Normalize") );
   AddToChain( wxT("ExportMp3") );
}

// Gets all commands that are valid for this mode.
wxArrayString BatchCommands::GetAllCommands()
{
   wxArrayString commands;
   wxString command;
   commands.Clear();

   AudacityProject *project = GetActiveProject();
   if (!project)
      return commands;

   EffectArray * effects;
   unsigned int i;

   for(i=0;i<sizeof(SpecialCommands)/sizeof(SpecialCommands[0]);i++)
   {
      commands.Add( SpecialCommands[i] );
   }
   
   int additionalEffects=ADVANCED_EFFECT;
   if( project->GetCleanSpeechMode() )
       additionalEffects = 0;
   effects = Effect::GetEffects(PROCESS_EFFECT | BUILTIN_EFFECT | additionalEffects);
   for(i=0; i<effects->GetCount(); i++) {
      command=(*effects)[i]->GetEffectName();
      command.Replace( wxT("..."), wxT(""));
      commands.Add( command);
   }
   delete effects;

/* This is for later in development: include the menu commands.
   CommandManager * mManager = project->GetCommandManager();
   wxArrayString mNames;
   mNames.Clear();
   mManager->GetAllCommandNames(mNames, false);
   for(i=0; i<mNames.GetCount(); i++) {
      commands.Add( mNames[i] );
   }
*/
   return commands;
}


Effect * BatchCommands::GetEffectFromCommandName(wxString inCommand)
{
   unsigned int i;
   wxString command;
   Effect * f;
   EffectArray * effects = Effect::GetEffects( ALL_EFFECTS );
   for(i=0; i<effects->GetCount(); i++) {
      f = (*effects)[i];
      command=f->GetEffectName();
      command.Replace( wxT("..."), wxT(""));
      if( command.IsSameAs( inCommand ))
      {  
         delete effects;
         return f;
      }
   }
   delete effects;
   return NULL;
}

wxString BatchCommands::GetCurrentParamsFor(wxString command)
{
   Effect * f;
   f=GetEffectFromCommandName( command );
   if( f==NULL )
      return wxT("");// effect not found.
   ShuttleCli shuttle;
   shuttle.mbStoreInClient=false;
   f->TransferParameters( shuttle );
   if( shuttle.mParams.IsEmpty() )
      return wxT("");// effect had no parameters.

   return shuttle.mParams;
}

bool BatchCommands::PromptForParamsFor(wxString command)
{
   Effect * f;
   f=GetEffectFromCommandName( command );
   if( f==NULL )
      return false;

   //mFactory = factory;
   //mProjectRate = projectRate;
   f->mParent = NULL;
   f->mWaveTracks = NULL;
   //mTracks = list;
   return f->PromptUser();
}

double BatchCommands::GetEndTime()
{
   AudacityProject *project = GetActiveProject();
   if( project == NULL )
   {
      wxMessageBox( _("No project and no Audio to process!") );
      return -1.0;
   }
   TrackList * tracks = project->GetTracks();
   if( tracks == NULL )
   {
      wxMessageBox( _("No tracks to process!") );
      return -1.0;
   }

   double endTime = tracks->GetEndTime();
   return endTime;
}

bool BatchCommands::WriteMp3File( const wxString Name, int bitrate )
{
   double endTime = GetEndTime();
   if( endTime <= 0.0f )
      return false;
   AudacityProject *project = GetActiveProject();
   if( bitrate <=0 )
   {
      // 'No' bitrate given, use the current default.
      // Use mMp3Stereo to control if export is to a stereo or mono file
      return ::ExportMP3(project, mMp3Stereo, Name, false, 0.0, endTime);
   }

   bool rc;
   long prevBitRate = gPrefs->Read(wxT("/FileFormats/MP3Bitrate"), 128);
   gPrefs->Write(wxT("/FileFormats/MP3Bitrate"), bitrate);
   // Use mMp3Stereo to control if export is to a stereo or mono file
   rc = ::ExportMP3(project, mMp3Stereo, Name, false, 0.0, endTime);
   gPrefs->Write(wxT("/FileFormats/MP3Bitrate"), prevBitRate);
   return rc;
}

// TIDY-ME: Get rid of special commands and make them part of the
// 'menu' system (but not showing on the menu)
//
// ======= IMPORTANT ========
// Special Commands are a KLUDGE whilst we wait for a better system to handle the menu
// commands from batch mode.
//
// Really we should be using a similar (or same) system to that used for effects
// so that parameters can be passed to the commands.  Many of the menu
// commands take a selection as their parameter.
//
// If you find yourself adding lots of existing commands from the menus here, STOP
// and think again.  
// ======= IMPORTANT ========
bool BatchCommands::ApplySpecialCommand(int iCommand, const wxString command,const wxString params)
{
   AudacityProject *project = GetActiveProject();
   wxString filename = project->BuildCleanFileName(mFileName);

   if( ReportAndSkip(command, params))
      return true;

   // We have a command index, but we don't use it!
   // TODO: Make this special-batch-command code use the menu item code....
   // FIX-ME: No error reporting on write file failure in batch mode.
   if( command == wxT("No Action")){
      return true;
   } else if (command == wxT("Import") ){
      project->OnRemoveTracks();
      project->Import(mFileName);
      project->OnSelectAll();
      return true;
   } else if (command == wxT("Save Hq Master1")){
      filename.Replace(wxT("cleaned/"), wxT("cleaned/MasterBefore_"), false);
      return WriteMp3File( filename, 56 );
   } else if (command == wxT("Save Hq Master2")){
      filename.Replace(wxT("cleaned/"), wxT("cleaned/MasterAfter_"), false);
      return WriteMp3File ( filename, 56 );
   } else if (command == wxT("Stereo To Mono")){
      // StereoToMono is an effect masquerading as a menu item.
      Effect * f=GetEffectFromCommandName( _("Stereo To Mono") );
      if( f!=NULL )
         return ApplyEffectCommand( f, command, params );
      wxMessageBox( _("Stereo To Mono Effect not found"));
      return false;
   } else if (command == wxT("ExportMp3") ){
      mMp3Stereo = !(params == wxT("Mono=yes")); //Should be created in a proper editor
      return WriteMp3File ( filename, 0 ); // 0 bitrate means use default/current
   } else if (command == wxT("ExportWav") ){
      filename.Replace(wxT(".mp3"), wxT(".wav"), false);
      double endTime = GetEndTime();
      if( endTime <= 0.0f )
         return false;
      return ::ExportPCM(project, false, filename, false, 0.0, endTime);
   } else if (command == wxT("ExportOgg")){
      filename.Replace(wxT(".mp3"), wxT(".ogg"), false);
      double endTime = GetEndTime();
      if( endTime <= 0.0f )
         return false;
      return ::ExportOGG(project, false, filename, false, 0.0, endTime);
   } 
   wxMessageBox( wxString::Format(_("Command %s not implemented yet"),command.c_str()) );
   return false;
}

bool BatchCommands::SetCurrentParametersFor( Effect * f, const wxString command, const wxString params)
{
   // transfer the parameters to the effect...
   if( !params.IsEmpty() )
   {
      ShuttleCli shuttle;
      shuttle.mParams = params;
      shuttle.mbStoreInClient=true;
      if( !f->TransferParameters( shuttle ))
      {
         wxMessageBox(
            wxString::Format(
            _("Could not set parameters of effect %s\n to %s."), command.c_str(),params.c_str() ));
         return false;
      }
   }
   return true;
}

bool BatchCommands::ApplyEffectCommand(   Effect * f, const wxString command, const wxString params)
{
   if( !SetCurrentParametersFor( f, command, params ))
      return false;
   //Possibly end processing here, if in batch-debug
   if( ReportAndSkip(command, params))
      return true;

   //Parmeters are set, nearly ready to apply the effect...
   AudacityProject *project = GetActiveProject();

   //FIX-ME: for later versions may want to not select-all in batch mode.
   //IF nothing selected, THEN select everything 
   // (most effects require that you have something selected).
   project->SelectAllIfNone();

   // NOW actually apply the effect.
   return project->OnEffect(ALL_EFFECTS | CONFIGURED_EFFECT , f);
}

bool BatchCommands::ApplyMenuCommand(const wxString command, const wxString params)
{
   if( ReportAndSkip(command, params))
      return true;
   return true;
}

bool BatchCommands::ApplyCommand(const wxString command, const wxString params)
{

   unsigned int i;
   // Test for a special command.
   for(i=0;i<sizeof(SpecialCommands)/sizeof(SpecialCommands[0]);i++)
   {
      if( command == SpecialCommands[i] )
         return ApplySpecialCommand( i, command, params );
   }
   
   // Test for an effect.
   Effect * f;
   f=GetEffectFromCommandName( command );
   if( f!=NULL )
      return ApplyEffectCommand( f, command, params );

//   return ApplyMenuCommand( command, params );
   wxMessageBox(
      wxString::Format(
      _("Your batch command of %s was not recognised."), command.c_str() ));
   return false;
}

bool BatchCommands::ApplyCommandInBatchMode(const wxString & command, const wxString &params)
{
   AudacityProject *project = GetActiveProject();
   bool rc;

   // enter batch mode...
   bool prevShowMode = project->GetShowId3Dialog();

   rc = ApplyCommand( command, params );

   // exit batch mode...
   project->SetShowId3Dialog(prevShowMode);

   return rc;
}


// ApplyBatchToNamedFile returns true on success, false otherwise.
// Any error reporting to the user has already been done.
bool BatchCommands::ApplyBatchToNamedFile(const wxString & filename)
{
   mFileName = filename;
   unsigned int i;

   mAbort = false;

   for(i=0;i<mCommandChain.GetCount();i++)
   {
      if(!ApplyCommandInBatchMode( mCommandChain[i], mParamsChain[i]) || mAbort)
         return false;
   }
   return true;
}

// AbortBatch() allows a premature terminatation of a batch.
void BatchCommands::AbortBatch()
{
   mAbort = true;
}

void BatchCommands::AddToChain(const wxString &command)
{
   mCommandChain.Add( command );
   mParamsChain.Add( GetCurrentParamsFor( command ));
}

void BatchCommands::ResetChain()
{
   mCommandChain.Clear();
   mParamsChain.Clear();
}

// TODO: Add warnings for non-useful batch chains.
// e.g. every batch chain should have an import and 
// and an export.
wxString BatchCommands::GetChainWarnings()
{
   if( mCommandChain.GetCount() == 0)
   {
      return _("No batch command chain defined.");
   }
   return wxT("");
}


// ReportAndSkip() is a diagnostic function that avoids actually
// applying the requested effect if in batch-debug mode.
bool BatchCommands::ReportAndSkip(const wxString command, const wxString params)
{
   int bDebug;
   gPrefs->Read(wxT("/Batch/Debug"), &bDebug, false);
   if( bDebug == 0 )
      return false;

   //TODO: Add a cancel button to these, and add the logic so that we can abort.
   if( params != wxT("") )
   {
      wxMessageBox( wxString::Format(_("Apply %s with parameter(s)\n\n%s"),command.c_str(), params.c_str()),
         _("Test Mode"));
   }
   else
   {
      wxMessageBox( wxString::Format(_("Apply %s"),command.c_str()),
         _("Test Mode"));
   }
   return true;
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
// arch-tag: TBD

