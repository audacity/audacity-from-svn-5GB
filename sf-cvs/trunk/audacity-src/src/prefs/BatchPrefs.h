/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchPrefs.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_BATCH_PREFS__
#define __AUDACITY_BATCH_PREFS__

#include <wx/defs.h>
#include <wx/string.h>
#include <wx/button.h>

#include "PrefsPanel.h"
#include "../BatchCommands.h"

class wxWindow;
class wxListCtrl;
class ShuttleGui;
class wxListEvent;

class BatchPrefs : public PrefsPanel 
{
public:
   BatchPrefs(wxWindow * parent);
   ~BatchPrefs();
   virtual bool Apply();
   virtual void Cancel();

private:
   void Populate();
   void PopulateOrExchange( ShuttleGui & S );
   void PopulateChains();
	void PopulateList();
   void AddItem( wxString const & Action, wxString const & Params);
	void SetItem( int ItemNo, const wxString command, const wxString params );

   void OnAdd(wxCommandEvent &event);
   void OnRemove(wxCommandEvent &event);
   void OnRename(wxCommandEvent &event);
   void OnUp(wxCommandEvent &event);
   void OnDown(wxCommandEvent &event);
   void OnImport(wxCommandEvent &event);
   void OnExport(wxCommandEvent &event);
   void OnDefaults(wxCommandEvent &event);
   void OnChainSelected(wxListEvent &event);
   void OnChainsBeginEdit(wxListEvent &event);
   void OnChainsEndEdit(wxListEvent &event);
   void OnItemSelected(wxListEvent &event);

   wxListCtrl * mChains; /// List of chains.
   wxListCtrl * mList;   /// List of commands in current command chain.
   wxButton * mRemove;
   wxButton * mRename;
   wxButton * mDefaults;

   BatchCommands mBatchCommands;  /// Provides list of available commands.
   wxString mActiveChain;
   wxArrayString *mChain;

   DECLARE_EVENT_TABLE();
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
// arch-tag: 57018e2b-d264-4f93-bfa7-06752ebf631e

