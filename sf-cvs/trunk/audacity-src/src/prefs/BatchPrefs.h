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

private:
   void Populate();
   void PopulateOrExchange( ShuttleGui & S );
   void AddItem( wxString const & Action, wxString const & Params);
	void SetItem( int ItemNo, const wxString command, const wxString params );
	void PopulateList();
   void CreateList();

   void OnItemSelected(wxListEvent &event);
   void OnDrag(wxListEvent &event);
   void OnDragEnd(wxListEvent &event);
   void OnSetChainMp3(wxCommandEvent &event);
   void OnSetChainCleanSpeech(wxCommandEvent &event);
   void OnSetChainEmpty(wxCommandEvent &event);
   void OnLoad(wxCommandEvent &event);
   void OnSave(wxCommandEvent &event);

   wxListCtrl * mList;   /// List of commands in current command chain.
   BatchCommands mBatchCommands;  /// Provides list of available commands.
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

