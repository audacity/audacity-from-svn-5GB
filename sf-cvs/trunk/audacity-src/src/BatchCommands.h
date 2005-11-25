/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchCommands.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_BATCH_COMMANDS_DIALOG__
#define __AUDACITY_BATCH_COMMANDS_DIALOG__

#include <wx/defs.h>
#include <wx/string.h>

class Effect;

class BatchCommands {
 public:
   // constructors and destructors
   BatchCommands();
 public:
	bool ReportAndSkip( const wxString command, const wxString params );
   bool ApplyCommand( const wxString command, const wxString params );
   bool ApplyCommandInBatchMode(const wxString & command, const wxString &params);
   bool ApplySpecialCommand(int iCommand, const wxString command,const wxString params);
   bool ApplyEffectCommand(Effect * f, const wxString command, const wxString params);
   bool ApplyMenuCommand(const wxString command, const wxString params);
   wxString mFileName;

   // Utility functions for the special commands.
   bool WriteMp3File( const wxString Name, int bitrate );
   double GetEndTime();

// These commands do not depend on the command list.
	static bool PromptForParamsFor( wxString command );
	static Effect * GetEffectFromCommandName( wxString command );
	static wxString GetCurrentParamsFor( wxString command );
   static bool SetCurrentParametersFor( Effect * f, const wxString command, const wxString params);
   static wxArrayString GetAllCommands();

// These commands do depend on the command list.    
	wxString GetChainWarnings();
	void ResetChain();
	void AddToChain( const wxString & command );
   bool ApplyBatchToNamedFile( const wxString & filename );
   void AbortBatch();
	void ReadChain();
	void SaveChain();
	void SetWavToMp3Chain();
	void SetCleanSpeechChain();
   wxArrayString mCommandChain;
   wxArrayString mParamsChain;
   bool mAbort;
};


#endif

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

