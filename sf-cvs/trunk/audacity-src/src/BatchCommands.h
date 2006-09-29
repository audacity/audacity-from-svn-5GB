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

WX_DECLARE_STRING_HASH_MAP(wxArrayString, CommandChains);

class BatchCommands {
 public:
   // constructors and destructors
   BatchCommands();
 public:
	bool ReportAndSkip( const wxString command, const wxString params );
   bool ApplyCommand( const wxString command, const wxString params );
   bool ApplyCommandInBatchMode(const wxString & command, const wxString &params);
   bool ApplyChain(const wxString & name, const wxString & filename);
   bool ApplySpecialCommand(int iCommand, const wxString command,const wxString params);
   bool ApplyEffectCommand(Effect * f, const wxString command, const wxString params);
   bool ApplyMenuCommand(const wxString command, const wxString params);
   wxString mFileName;

   // Utility functions for the special commands.
   bool WriteMp3File( const wxString Name, int bitrate );
   double GetEndTime();
   bool IsMono();

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
   void AbortBatch();
	void ReadChain();
	void WriteChain();
	void LoadChain( wxWindow * parent );
	void SaveChain( wxWindow * parent );
	void SetWavToMp3Chain();
	void SetCleanSpeechChain();

   void ReadChains();
   void WriteChains();
   void FlushChains();
   wxArrayString GetChainNames();
   int GetChainCount();

   wxArrayString * GetChain(const wxString & name);
   wxArrayString * RenameChain(const wxString & oldname, const wxString & newname);
   void DeleteChain(const wxString & name);

   bool IsFixed(const wxString & name);

   wxArrayString * RestoreChain(const wxString & name);

   void ImportChain(wxWindow *parent, const wxString & name);
   void ExportChain(wxWindow *parent, const wxString & name);

   void Split(const wxString & str, wxString & command, wxString & param);
   wxString Join(const wxString & command, const wxString & param);
	wxString Defaults(const wxString & command);

   wxArrayString mCommandChain;
   wxArrayString mParamsChain;
   bool mAbort;

   CommandChains mCommandChains;
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

