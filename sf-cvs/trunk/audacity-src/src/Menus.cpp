/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.cpp

  Dominic Mazzoni

  This file implements all of the methods that get called when you
  select an item from a menu, along with routines to create the
  menu bar and enable/disable items.  All of the methods here are
  methods of AudacityProject, so for the header file, see Project.h.

**********************************************************************/

#include <wx/wxprec.h>
#include <wx/tokenzr.h>

#ifndef WX_PRECOMP
#include <wx/app.h>
#include <wx/intl.h>
#include <wx/menu.h>
#include <wx/string.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>

#endif // WX_PRECOMP

#include "sndfile.h"

#include <wx/textfile.h>

#include "Audacity.h"
#include "AboutDialog.h"
#include "ToolBar.h"
#include "ControlToolBar.h"
#include "EditToolBar.h"
#include "Benchmark.h"
#include "export/Export.h"
#include "FileFormats.h"
#include "FreqWindow.h"
#include "Help.h"
#include "import/Import.h"
#include "import/ImportRaw.h"
#include "import/ImportMIDI.h"
#include "LabelTrack.h"
#include "Mix.h"
#include "NoteTrack.h"
#include "Prefs.h"
#include "Project.h"
#include "Tags.h"
#include "Track.h"
#include "TrackPanel.h"
#include "WaveTrack.h"
#include "HistoryWindow.h"
#include "effects/Effect.h"
#include "prefs/PrefsDialog.h"


#define AUDACITY_MENUS_GLOBALS
#include "Menus.h"
#undef AUDACITY_MENUS_GLOBALS

#ifdef __WXGTK__
#define MAX_MENU_LEN 20
#define MAX_SUBMENU_LEN 10
#else
#define MAX_MENU_LEN 1000
#define MAX_SUBMENU_LEN 1000
#endif

void AudacityProject::CreateMenuBar()
{
   mMenusDirtyCheck = gMenusDirty;
   mFirstTimeUpdateMenus = true;

#define AUDACITY_MENUS_COMMANDS_EVENT_TABLE
// BG: Generate an array of command names, and their corresponding functions
#include "commands.h"
#undef AUDACITY_MENUS_COMMANDS_EVENT_TABLE

   mMenuBar = new wxMenuBar();

   mFileMenu = new wxMenu();
   mEditMenu = new wxMenu();
   mViewMenu = new wxMenu();
   mProjectMenu = new wxMenu();
   mInsertMenu = new wxMenu();
   mEffectMenu = new wxMenu();
   mAnalyzeMenu = new wxMenu();
   mHelpMenu = new wxMenu();

   mMenuBar->Append(mFileMenu, _("&File"));
   mMenuBar->Append(mEditMenu, _("&Edit"));
   mMenuBar->Append(mViewMenu, _("&View"));
   mMenuBar->Append(mProjectMenu, _("&Project"));
   mMenuBar->Append(mInsertMenu, _("&Insert"));
   mMenuBar->Append(mEffectMenu, _("Effec&t"));
   mMenuBar->Append(mAnalyzeMenu, _("&Analyze"));
   mMenuBar->Append(mHelpMenu, _("&Help"));

   SetMenuBar(mMenuBar);
   BuildMenuBar();
}

void AudacityProject::RebuildMenuBar()
{
// BG: Generate an array of keys combos that cannot be used
#include "commandkeys.h"

   mFileMenu = new wxMenu();
   mEditMenu = new wxMenu();
   mViewMenu = new wxMenu();
   mProjectMenu = new wxMenu();
   mHelpMenu = new wxMenu();

   delete mMenuBar->Replace(fileMenu, mFileMenu, _("&File"));
   delete mMenuBar->Replace(editMenu, mEditMenu, _("&Edit"));
   delete mMenuBar->Replace(viewMenu, mViewMenu, _("&View"));
   delete mMenuBar->Replace(projectMenu, mProjectMenu, _("&Project"));
   delete mMenuBar->Replace(helpMenu, mHelpMenu, _("&Help"));

   BuildMenuBar();
}

void AudacityProject::AppendEffects(EffectArray *effs, wxMenu *menu,
                                    bool spill)
{
   unsigned int currentLen = menu->GetMenuItemCount();
   unsigned int effLen = effs->GetCount();
   unsigned int i;

   if (!spill || currentLen + effLen <= MAX_MENU_LEN) {
      for(i=0; i<effLen; i++)
         menu->Append(FirstEffectID + (*effs)[i]->GetID(),
                      (*effs)[i]->GetEffectName());
      return;
   }

   // There were too many effects in this menu.  Put the
   // extras (plug-ins) in submenus.

   wxMenu * tmp;
   wxString label;
   tmp = new wxMenu;
   int lower = 1;
   int upper = MAX_SUBMENU_LEN;
   int submenu = 0;

   for (i=0; i<effLen; i++) {
      tmp->Append(FirstEffectID + (*effs)[i]->GetID(),
                  (*effs)[i]->GetEffectName());

      if( ((i+1) % MAX_SUBMENU_LEN) == 0
          || i == (effLen - 1)) {
         //upper limit should be the current plugin number (+1)
         upper = i + 1;
         //make submenu label
         label.Printf(_("Plugins %d to %d"), lower, upper);

         // Add the submenu to the menu
         menu->Append(FirstPluginSubMenuID + submenu, label, tmp, label);

         //reset lower for next time.
         lower += MAX_SUBMENU_LEN;

         submenu++;
         if(i != (effLen - 1))
            tmp = new wxMenu;      // go on to next temp menu
      }

   }
}

void AudacityProject::BuildMenuBar()
{
   unsigned int i;
   for (i = 0; i < mCommandMenuItem.Count(); i++) {

      wxMenu *menu = 0;
      //This determines which menu the current MenuItem belongs to.
      switch (mCommandMenuItem[i]->category) {
         case fileMenu:
            menu = mFileMenu;
            break;
         case editMenu:
            menu = mEditMenu;
            break;
         case viewMenu:
            menu = mViewMenu;
            break;
         case projectMenu:
            menu = mProjectMenu;
            break;
         case helpMenu:
            menu = mHelpMenu;
            break;
         default:
            // ERROR -- should not happen
            break;
      }

      //BG: Tokenize menu shortcuts
      TokenizeCommandStrings(i);

      wxString menuString = mCommandMenuItem[i]->commandString;

      //BG: Pick first shortcut
      wxStringTokenizer tName(mCommandMenuItem[i]->comboStrings, ":");
      if(tName.HasMoreTokens())
      {
          wxString token = tName.GetNextToken();
          menuString += "\t" + token;
      }

      const wxString dummy = "";

      //This adds an element of the correct type:
      switch(mCommandMenuItem[i]->type) {
      case typeSeparator:

         menu->AppendSeparator();

         break;
      
      case typeNormal:

         menu->Append(i + MenuBaseID, menuString);
         break;
      case typeCheckItem:
         menu->Append(i + MenuBaseID,(const wxString &) menuString, dummy,true);
         //menu->AppendCheckItem() can be used with wxWindows 2.3
         //menu->AppendCheckItem(i + MenuBaseID,(const wxString &) mCommandMenuItem[i]->commandString, dummy);
         break;
      case typeRadioItem:
         //This can't be done until wxWindows 2.3
         //menu->AppendRadioItem(i + MenuBaseID,(const wxString &) mCommandMenuItem[i]->commandString, dummy);
         //Put in a normal item instead.
         menu->Append(i + MenuBaseID, menuString);
         break;
      default:
         //Error--this shouldn't happen
         break;
      }
   }

#ifdef __WXMAC__
   wxApp::s_macAboutMenuItemId = AboutID;
#endif

   mInsertSilenceAmount = 1.0;

   if (mInsertMenu->GetMenuItemCount() > 0)
      return;

   EffectArray *effs;

   effs = Effect::GetEffects(BUILTIN_EFFECT | INSERT_EFFECT);
   AppendEffects(effs, mInsertMenu, false);
   delete effs;

   effs = Effect::GetEffects(PLUGIN_EFFECT | INSERT_EFFECT);
   if (effs->GetCount() > 0) {
      mInsertMenu->AppendSeparator();
      AppendEffects(effs, mInsertMenu, true);
   }
   delete effs;

   effs = Effect::GetEffects(BUILTIN_EFFECT | PROCESS_EFFECT);
   AppendEffects(effs, mEffectMenu, false);
   delete effs;

   effs = Effect::GetEffects(PLUGIN_EFFECT | PROCESS_EFFECT);
   if (effs->GetCount() > 0) {
      mEffectMenu->AppendSeparator();
      AppendEffects(effs, mEffectMenu, true);
   }
   delete effs;

   effs = Effect::GetEffects(BUILTIN_EFFECT | ANALYZE_EFFECT);
   AppendEffects(effs, mAnalyzeMenu, false);
   delete effs;

   effs = Effect::GetEffects(PLUGIN_EFFECT | ANALYZE_EFFECT);
   if (effs->GetCount() > 0) {
      mAnalyzeMenu->AppendSeparator();
      AppendEffects(effs, mAnalyzeMenu, true);
   }
   delete effs;
}

void AudacityProject::AssignDefaults()
{
#define AUDACITY_MENUS_COMMANDS_DEFAULT_SHORTCUTS
#include "commands.h"
#undef AUDACITY_MENUS_COMMANDS_DEFAULT_SHORTCUTS

   TokenizeCommandStrings(-1);
}

void AudacityProject::TokenizeCommandStrings(int mVal)
{
   int startVal = mVal;
   int maxVal = mVal;

   if(mVal < 0)
   {
      startVal = 0;
      maxVal = mCommandMenuItem.Count()-1;
   }

   for(int i = startVal; i <= maxVal; i++)
   {
      gPrefs->SetPath("/Keyboard/" + wxString::Format("%i", i));
      long keyIndex;
      wxString keyString;

      mCommandMenuItem[i]->comboStrings = "";

      if(gPrefs->GetFirstEntry(keyString, keyIndex))
      {
         mCommandMenuItem[i]->comboStrings = keyString;
         while(gPrefs->GetNextEntry(keyString, keyIndex))
         {
            mCommandMenuItem[i]->comboStrings += ":" + keyString;
         }
      }
   }

   gPrefs->SetPath("/");
}

wxString AudacityProject::GetCommandName(int nIndex)
{
   wxMenu holderMenu;
   holderMenu.Append(58326, mCommandMenuItem[nIndex]->commandString);
   return (holderMenu.FindItem(58326))->GetLabel();
}

wxString AudacityProject::GetCommandDesc(int nIndex)
{
   return mCommandMenuItem[nIndex]->descriptionString;
}

menuType AudacityProject::GetMenuType(int nIndex)
{
   return mCommandMenuItem[nIndex]->type;
}

audEventFunction AudacityProject::GetCommandFunc(int nIndex)
{
   if((nIndex < 0) || (nIndex >= GetNumCommands())) return NULL;
   return mCommandMenuItem[nIndex]->callbackFunction;
}

int AudacityProject::GetNumCommands()
{
   return mCommandMenuItem.GetCount();
}

void AudacityProject::SetMenuState(wxMenu *menu, int id, bool enable)
{
   menu->Enable(id, enable);
   SetCommandState(id, enable);
}

void AudacityProject::SetCommandState(int nID, int iVal)
{
   int idz = (nID - MenuBaseID);
   if ((idz >= 0) && (idz < GetNumCommands())) {
      mCommandMenuItem[idz]->state = (menuState) iVal;
   }
}

int AudacityProject::GetCommandState(int nIndex)
{
   if((nIndex < 0) || (nIndex >= GetNumCommands())) return disabledMenu;
   return mCommandMenuItem[nIndex]->state;
}

void AudacityProject::SetCommandValue(int nID, wxString sName)
{
   int idz = (nID - MenuBaseID);
   if ((idz >= 0) && (idz < GetNumCommands())) {
      mCommandMenuItem[idz]->commandString = sName;
   }
}

int AudacityProject::FindCommandByCombos(wxString cName)
{
   for(int i = 0; i < GetNumCommands(); i++)
   {
      wxStringTokenizer tName(mCommandMenuItem[i]->comboStrings, ":");
      while (tName.HasMoreTokens())
      {
          wxString token = tName.GetNextToken();

          if(!cName.CmpNoCase(token))
             return i;
      }
   }

   return -1;
}

void AudacityProject::OnUpdateMenus(wxUpdateUIEvent & event)
{
   if (mMenusDirtyCheck != gMenusDirty) {
      /*
         TODO!
       */
   }

/*
#define AUDACITY_MENUS_COMMANDS_ACCELL_TABLE
#include "commands.h"
#undef AUDACITY_MENUS_COMMANDS_ACCELL_TABLE
*/

   SetMenuState(mFileMenu, SaveID, mUndoManager.UnsavedChanges());

   bool nonZeroRegionSelected = (mViewInfo.sel1 > mViewInfo.sel0);

   int numTracks = 0;
   int numTracksSelected = 0;
   int numWaveTracks = 0;
   int numWaveTracksSelected = 0;
   int numLabelTracks = 0;
   int numLabelTracksSelected = 0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   while (t) {
      numTracks++;
      // JH: logically, we only want to count a stereo pair as one track. Right??
      // I'm changing it and hoping I don't break anything
      if (t->GetKind() == Track::Wave && t->GetLinked() == false)
         numWaveTracks++;
      if (t->GetKind() == Track::Label)
         numLabelTracks++;
      if (t->GetSelected()) {
         numTracksSelected++;
         // JH: logically, we only want to count a stereo pair as one track. Right??
         // I'm changing it and hoping I don't break anything
         if (t->GetKind() == Track::Wave && t->GetLinked() == false)
            numWaveTracksSelected++;
         else if(t->GetKind() == Track::Label)
            numLabelTracksSelected++;

      }
      t = iter.Next();
   }

   SetMenuState(mEditMenu, PasteID, numTracksSelected > 0 && msClipLen > 0.0);

   //Calculate the ToolBarCheckSum (uniquely specifies state of all toolbars):
   int toolBarCheckSum = 0;
   toolBarCheckSum += gControlToolBarStub->GetWindowedStatus() ? 2 : 1;
   if (gEditToolBarStub) {
      if (gEditToolBarStub->GetLoadedStatus()) {
         if(gEditToolBarStub->GetWindowedStatus())
            toolBarCheckSum += 6;
         else
            toolBarCheckSum += 3;
      }
   }
   
   
   // Get ahold of the clipboard status
   bool clipboardStatus = static_cast<bool>(GetActiveProject()->Clipboard());

   // Return from this function if nothing's changed since
   // the last time we were here.
 
   if (!mFirstTimeUpdateMenus &&
       mLastNonZeroRegionSelected == nonZeroRegionSelected &&
       mLastNumTracks == numTracks &&
       mLastNumTracksSelected == numTracksSelected &&
       mLastNumWaveTracks == numWaveTracks &&
       mLastNumWaveTracksSelected == numWaveTracksSelected &&
       mLastNumLabelTracks == numLabelTracks &&
       mLastZoomLevel == mViewInfo.zoom &&
       mLastToolBarCheckSum == toolBarCheckSum &&
       mLastUndoState == mUndoManager.UndoAvailable() &&
       mLastRedoState == mUndoManager.RedoAvailable() &&
       mLastClipboardState == clipboardStatus  ) 
      return;
   
   // Otherwise, save state and then update all of the menus

   mFirstTimeUpdateMenus = false;
   mLastNonZeroRegionSelected = nonZeroRegionSelected;
   mLastNumTracks = numTracks;
   mLastNumTracksSelected = numTracksSelected;
   mLastNumWaveTracks = numWaveTracks;
   mLastNumWaveTracksSelected = numWaveTracksSelected;
   mLastNumLabelTracks = numLabelTracks;
   mLastZoomLevel = mViewInfo.zoom;
   mLastToolBarCheckSum = toolBarCheckSum;
   mLastUndoState = mUndoManager.UndoAvailable();
   mLastRedoState = mUndoManager.RedoAvailable();  
   mLastClipboardState = clipboardStatus;

   bool anySelection = numTracksSelected > 0 && nonZeroRegionSelected;

   SetMenuState(mFileMenu, ExportMixID, numTracks > 0);
   SetMenuState(mFileMenu, ExportSelectionID, anySelection);
   SetMenuState(mFileMenu, ExportLossyMixID, numTracks > 0);
   SetMenuState(mFileMenu, ExportLossySelectionID, anySelection);
   SetMenuState(mFileMenu, ExportLabelsID, numLabelTracks > 0);

   SetMenuState(mEditMenu, CutID, anySelection);
   SetMenuState(mEditMenu, CopyID, anySelection);
   SetMenuState(mEditMenu, TrimID, anySelection);
   SetMenuState(mEditMenu, DeleteID, anySelection);
   SetMenuState(mEditMenu, SilenceID, anySelection);
   SetMenuState(mEditMenu, InsertSilenceID, numTracksSelected > 0);
   SetMenuState(mEditMenu, SplitID, anySelection);
   SetMenuState(mEditMenu, SplitLabelsID, numLabelTracksSelected == 1 && numWaveTracksSelected == 1);
   SetMenuState(mEditMenu, DuplicateID, anySelection);
   SetMenuState(mEditMenu, SelectAllID, numTracks > 0);

   SetMenuState(mEditMenu, UndoID, mUndoManager.UndoAvailable());
   SetMenuState(mEditMenu, RedoID, mUndoManager.RedoAvailable());


   SetMenuState(mViewMenu, PlotSpectrumID, numWaveTracksSelected > 0
                     && nonZeroRegionSelected);

#ifndef __WXMAC__
   //Modify toolbar-specific Menus

   wxMenuItemBase   *dock = mViewMenu->FindItem(FloatEditToolBarID);
 
   if (gEditToolBarStub) {

      // Loaded or unloaded?
      if (gEditToolBarStub->GetLoadedStatus()) {
         dock->Enable(true);
      } else {
         dock->Enable(false);
      }

      // Floating or docked?
      if (gEditToolBarStub->GetWindowedStatus())
         dock->SetName(_("Dock Edit Toolbar"));
      else
         dock->SetName(_("Float Edit Toolbar"));
   }
   else
      {
         dock->Enable(false);
      }
#endif

   SetMenuState(mProjectMenu, QuickMixID, numWaveTracksSelected > 1);
   SetMenuState(mProjectMenu, AlignID, numTracksSelected > 1);
   SetMenuState(mProjectMenu, AlignZeroID, numTracksSelected > 0);
   SetMenuState(mProjectMenu, RemoveTracksID, numTracksSelected > 0);

   // Effects menus

   EffectArray *effs;
   unsigned int e;

   effs = Effect::GetEffects(BUILTIN_EFFECT | PLUGIN_EFFECT | INSERT_EFFECT);
   for(e=0; e<effs->GetCount(); e++)
      mInsertMenu->Enable(FirstEffectID + (*effs)[e]->GetID(),
                          numWaveTracksSelected > 0);
   delete effs;

   effs = Effect::GetEffects(BUILTIN_EFFECT | PLUGIN_EFFECT | PROCESS_EFFECT);
   for(e=0; e<effs->GetCount(); e++)
      mEffectMenu->Enable(FirstEffectID + (*effs)[e]->GetID(),
                          numWaveTracksSelected > 0
                          && nonZeroRegionSelected);
   delete effs;

   effs = Effect::GetEffects(BUILTIN_EFFECT | PLUGIN_EFFECT | ANALYZE_EFFECT);
   for(e=0; e<effs->GetCount(); e++)
      mAnalyzeMenu->Enable(FirstEffectID + (*effs)[e]->GetID(),
                          numWaveTracksSelected > 0
                           && nonZeroRegionSelected);
   delete effs;

   //Now, go through each toolbar, and and call EnableDisableButtons()
   unsigned int i;
   for (i = 0; i < mToolBarArray.GetCount(); i++) {
      mToolBarArray[i]->EnableDisableButtons();
   }

   //Now, do the same thing for the (possibly invisible) floating toolbars
   gControlToolBarStub->GetToolBar()->EnableDisableButtons();

   //gEditToolBarStub might be null:
   if(gEditToolBarStub){
      gEditToolBarStub->GetToolBar()->EnableDisableButtons();
   }
}

//
// Generic menu event handler
//

bool AudacityProject::HandleMenuEvent(wxEvent & event)
{
   int idz = (event.GetId() - MenuBaseID);
   if ((idz >= 0) && (idz < GetNumCommands())) {
      (this->*((audEventFunction) (this->GetCommandFunc(idz)))) (event);
   }

   return true;                 //handled message
}

//
// File Menu
//

void AudacityProject::OnNew(wxEvent & event)
{
   CreateNewAudacityProject(gParentWindow);
}

void AudacityProject::OnOpen(wxEvent & event)
{
   ShowOpenDialog(this);
}

void AudacityProject::OnSave(wxEvent & event)
{
   Save();
}

void AudacityProject::OnSaveAs(wxEvent & event)
{
   SaveAs();
}

void AudacityProject::OnExit(wxEvent & event)
{
   QuitAudacity();
}

void AudacityProject::OnExportLabels(wxEvent & event)
{
   Track *t;
   int numLabelTracks = 0;

   TrackListIterator iter(mTracks);

   t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Label)
         numLabelTracks++;
      t = iter.Next();
   }

   if (numLabelTracks == 0) {
      wxMessageBox(_("There are no label tracks to export."));
      return;
   }

   wxString fName = _("labels.txt");

   fName = wxFileSelector(_("Export Labels As:"),
                          NULL,
                          fName,
                          "txt",
                          "*.txt", wxSAVE | wxOVERWRITE_PROMPT, this);

   if (fName == "")
      return;

   wxTextFile f(fName);
#ifdef __WXMAC__
   wxFile *temp = new wxFile();
   temp->Create(fName);
   delete temp;
#else
   f.Create();
#endif
   f.Open();
   if (!f.IsOpened()) {
      wxMessageBox(_("Couldn't write to file: ") + fName);
      return;
   }

   t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Label)
         ((LabelTrack *) t)->Export(f);

      t = iter.Next();
   }

#ifdef __WXMAC__
   f.Write(wxTextFileType_Mac);
#else
   f.Write();
#endif
   f.Close();
}

void AudacityProject::OnExportMix(wxEvent & event)
{
   ::Export(this, false, 0.0, mTracks->GetEndTime());
}

void AudacityProject::OnExportSelection(wxEvent & event)
{
   ::Export(this, true, mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnExportLossyMix(wxEvent & event)
{
   ::ExportLossy(this, false, 0.0, mTracks->GetEndTime());
}

void AudacityProject::OnExportLossySelection(wxEvent & event)
{
   ::ExportLossy(this, true, mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnPreferences(wxEvent & event)
{
   PrefsDialog dialog(this /* parent */ );
   dialog.ShowModal();
}

//
// Edit Menu
//

void AudacityProject::Undo(wxEvent & event)
{
   if (!mUndoManager.UndoAvailable()) {
      wxMessageBox(_("Nothing to undo"));
      return;
   }

   TrackList *l = mUndoManager.Undo(&mViewInfo.sel0, &mViewInfo.sel1);
   PopState(l);

   FixScrollbars();
   mTrackPanel->Refresh(false);

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();
}

void AudacityProject::Redo(wxEvent & event)
{
   if (!mUndoManager.RedoAvailable()) {
      wxMessageBox(_("Nothing to redo"));
      return;
   }

   TrackList *l = mUndoManager.Redo(&mViewInfo.sel0, &mViewInfo.sel1);
   PopState(l);

   FixScrollbars();
   mTrackPanel->Refresh(false);

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();
}

void AudacityProject::UndoHistory(wxEvent & event)
{
   if (mHistoryWindow)
      mHistoryWindow->Show(true);
   else {
      mHistoryWindow = new HistoryWindow(this, &mUndoManager);
      mHistoryWindow->Show(true);
   }
}


void AudacityProject::Cut(wxEvent & event)
{
   ClearClipboard();

   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *dest = 0;

   while (n) {
      if (n->GetSelected()) {
         n->Cut(mViewInfo.sel0, mViewInfo.sel1, &dest);
         if (dest)
            msClipboard->Add(dest);
      }
      n = iter.Next();
   }

   msClipLen = (mViewInfo.sel1 - mViewInfo.sel0);
   msClipProject = this;

   mViewInfo.sel1 = mViewInfo.sel0;

   PushState(_("Cut to the clipboard"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}



void AudacityProject::Copy(wxEvent & event)
{
   ClearClipboard();

   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *dest = 0;

   while (n) {
      if (n->GetSelected()) {
         n->Copy(mViewInfo.sel0, mViewInfo.sel1, &dest);
         if (dest)
            msClipboard->Add(dest);
      }
      n = iter.Next();
   }

   msClipLen = (mViewInfo.sel1 - mViewInfo.sel0);
   msClipProject = this;
   
   //Make sure the menus/toolbar states get updated
   mTrackPanel->Refresh(false);
}

void AudacityProject::Paste(wxEvent & event)
{
   if (mViewInfo.sel0 != mViewInfo.sel1)
      Clear();

   wxASSERT(mViewInfo.sel0 == mViewInfo.sel1);

   double tsel = mViewInfo.sel0;

   TrackListIterator iter(mTracks);
   TrackListIterator clipIter(msClipboard);

   Track *n = iter.First();
   Track *c = clipIter.First();

   while (n && c) {
      if (n->GetSelected()) {
         if (msClipProject != this && c->GetKind() == Track::Wave)
            ((WaveTrack *) c)->Lock();

         n->Paste(tsel, c);

         if (msClipProject != this && c->GetKind() == Track::Wave)
            ((WaveTrack *) c)->Unlock();

         c = clipIter.Next();
      }

      n = iter.Next();
   }

   // TODO: What if we clicked past the end of the track?

   mViewInfo.sel0 = tsel;
   mViewInfo.sel1 = tsel + msClipLen;

   PushState(_("Pasted from the clipboard"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}



void AudacityProject::Trim(wxEvent & event)
{
   if (mViewInfo.sel0 >= mViewInfo.sel1)
      return;

   TrackListIterator iter(mTracks);
   Track *n = iter.First();

   while (n) {
      if (n->GetSelected()) {
         //Delete the section before the left selector
         n->Clear(n->GetOffset(), mViewInfo.sel0);
         if (mViewInfo.sel0 > n->GetOffset())
            n->SetOffset(mViewInfo.sel0);

         //Delete the section after the right selector
         n->Clear(mViewInfo.sel1, n->GetEndTime());
      }
      n = iter.Next();
   }

   FixScrollbars();
   mTrackPanel->Refresh(false);
   PushState(_("Trim file to selection"));
}



void AudacityProject::OnDelete(wxEvent & event)
{
   Clear();
}

void AudacityProject::OnSilence(wxEvent & event)
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();

   while (n) {
      if (n->GetSelected())
         n->Silence(mViewInfo.sel0, mViewInfo.sel1);

      n = iter.Next();
   }

   PushState(wxString::
             Format(_("Silenced selected tracks for %.2f seconds at %.2f"),
                    mViewInfo.sel1 - mViewInfo.sel0, mViewInfo.sel0));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnDuplicate(wxEvent & event)
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *dest = 0;

   TrackList newTracks;

   while (n) {
      if (n->GetSelected()) {
         n->Copy(mViewInfo.sel0, mViewInfo.sel1, &dest);
         if (dest) {
            dest->Init(*n);
            dest->SetOffset(wxMax(mViewInfo.sel0, n->GetOffset()));
            newTracks.Add(dest);
         }
      }
      n = iter.Next();
   }

   TrackListIterator nIter(&newTracks);
   n = nIter.First();
   while (n) {
      mTracks->Add(n);
      n = nIter.Next();
   }

   PushState(_("Duplicated"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSplit(wxEvent & event)
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *dest = 0;

   TrackList newTracks;

   while (n) {
      if (n->GetSelected()) {
         double sel0 = mViewInfo.sel0;
         double sel1 = mViewInfo.sel1;

         n->Copy(sel0, sel1, &dest);
         if (dest) {
            dest->Init(*n);
            dest->SetOffset(wxMax(sel0, n->GetOffset()));

            if (sel1 >= n->GetEndTime())
               n->Clear(sel0, sel1);
            else if (sel0 <= n->GetOffset()) {
               n->Clear(sel0, sel1);
               n->SetOffset(sel1);
            } else
               n->Silence(sel0, sel1);

            newTracks.Add(dest);
         }
      }
      n = iter.Next();
   }

   TrackListIterator nIter(&newTracks);
   n = nIter.First();
   while (n) {
      mTracks->Add(n);
      n = nIter.Next();
   }

   PushState(_("Split"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSplitLabels(wxEvent & event)
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *srcRight = 0;
   Track *srcLeft = 0;
   bool stereo = false;
   LabelTrack *label = 0;

   while(n) {
      if(n->GetSelected()) {
         if(n->GetKind() == Track::Wave) {
            if(n->GetLinked() == true) {
               stereo = true;
               srcLeft = n;
               srcRight  = iter.Next();
            }
            else {
               srcRight = n;
               stereo = false;
            }
         }
         else if(n->GetKind() == Track::Label)
            label = (LabelTrack*)n;  // cast necessary to call LabelTrack specific methods
      }
      n = iter.Next();
   }

   // one new track for every label, from that label to the next
   
   TrackList newTracks;

   for(int i = 0; i < label->GetNumLabels(); i++) {
      wxString name = label->GetLabel(i)->title;
      double begin = label->GetLabel(i)->t;
      double end;

      // if on the last label, extend to the end of the wavetrack
      if(i == label->GetNumLabels() - 1) {
         if(stereo)
            end = wxMax(srcLeft->GetEndTime(), srcRight->GetEndTime());
         else
            end = srcLeft->GetEndTime();
      }
      else
         end = label->GetLabel(i+1)->t;

      Track *destLeft = 0;
      Track *destRight = 0;

      srcLeft->Copy(begin, end, &destLeft);
      if (destLeft) {
         destLeft->Init(*srcLeft);
         destLeft->SetOffset(wxMax(begin, srcLeft->GetOffset()));
         destLeft->SetName(name);
         
         mTracks->Add(destLeft);
      }

      if(stereo) {
         srcRight->Copy(begin, end, &destRight);
         if (destRight) {
            destRight->Init(*srcRight);
            destRight->SetOffset(wxMax(begin, srcRight->GetOffset()));
            destRight->SetName(name);
            
            mTracks->Add(destRight);
         }
         else if(destLeft)
            // account for possibility of a non-aligned linked track, which could
            // cause the left channel to be eligible for creating a new track,
            // but not the right.
            destLeft->SetLinked(false);
      }
   }

   PushState(_("Split at labels"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnInsertSilence(wxEvent & event)
{
   wxString amountStr =
       wxGetTextFromUser(_("Number of seconds of silence to insert:"),
                         _("Insert Silence"),
                         wxString::Format("%lf", mInsertSilenceAmount),
                         this, -1, -1, TRUE);

   if (amountStr == "")
      return;
   if (!amountStr.ToDouble(&mInsertSilenceAmount)) {
      wxMessageBox(_("Bad number of seconds."));
      return;
   }
   if (mInsertSilenceAmount <= 0.0) {
      wxMessageBox(_("Bad number of seconds."));
      return;
   }

   if (mViewInfo.sel0 != mViewInfo.sel1)
      Clear();

   wxASSERT(mViewInfo.sel0 == mViewInfo.sel1);

   TrackListIterator iter(mTracks);

   Track *n = iter.First();

   while (n) {
      if (n->GetSelected())
         n->InsertSilence(mViewInfo.sel0, mInsertSilenceAmount);

      n = iter.Next();
   }

   PushState(wxString::
             Format(_("Inserted %.2f seconds of silence at %.2f"),
                    mInsertSilenceAmount, mViewInfo.sel0));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelectAll(wxEvent & event)
{
   TrackListIterator iter(mTracks);

   Track *t = iter.First();
   while (t) {
      t->SetSelected(true);
      t = iter.Next();
   }
   mViewInfo.sel0 = mTracks->GetMinOffset();
   mViewInfo.sel1 = mTracks->GetEndTime();

   mTrackPanel->Refresh(false);
   mTrackPanel->DisplaySelection();
}

//
// View Menu
//

// Utility function called by other zoom methods
void AudacityProject::Zoom(double level)
{
   if (level > gMaxZoom)
      level = gMaxZoom;
   if (level <= gMinZoom)
      level = gMinZoom;

   mViewInfo.zoom = level;
   FixScrollbars();
}

void AudacityProject::OnZoomIn(wxEvent & event)
{
   double origLeft = mViewInfo.h;
   double origWidth = mViewInfo.screen;
   Zoom(mViewInfo.zoom *= 2.0);
   
   double newh = origLeft + (origWidth - mViewInfo.screen) / 2;
   
   // make sure that the *right-hand* end of the selection is
   // no further *left* than 1/3 of the way across the screen
   if (mViewInfo.sel1 < newh + mViewInfo.screen / 3)
      newh = mViewInfo.sel1 - mViewInfo.screen / 3;

   // make sure that the *left-hand* end of the selection is
   // no further *right* than 2/3 of the way across the screen
   if (mViewInfo.sel0 > newh + mViewInfo.screen * 2 / 3)
      newh = mViewInfo.sel0 - mViewInfo.screen * 2 / 3;

   TP_ScrollWindow(newh);
}

void AudacityProject::OnZoomOut(wxEvent & event)
{  
   //Zoom() may change these, so record original values:
   double origLeft = mViewInfo.h;
   double origWidth = mViewInfo.screen;

   Zoom(mViewInfo.zoom /= 2.0);

   double newh = origLeft + (origWidth - mViewInfo.screen) / 2;
   // newh = (newh > 0) ? newh : 0;
   TP_ScrollWindow(newh);

}

void AudacityProject::OnZoomNormal(wxEvent & event)
{
   Zoom(44100.0 / 512.0);
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnZoomFit(wxEvent & event)
{
   double len = mTracks->GetEndTime();

   if (len <= 0.0)
      return;

   int w, h;
   mTrackPanel->GetTracksUsableArea(&w, &h);
   w -= 10;

   Zoom(w / len);
   TP_ScrollWindow(0.0);
}

void AudacityProject::OnZoomSel(wxEvent & event)
{
   if (mViewInfo.sel1 <= mViewInfo.sel0)
      return;

   Zoom(mViewInfo.zoom * mViewInfo.screen / (mViewInfo.sel1 - mViewInfo.sel0)),
   TP_ScrollWindow(mViewInfo.sel0);
}

void AudacityProject::OnPlotSpectrum(wxEvent & event)
{
   int selcount = 0;
   int i;
   double rate = 0;
   sampleCount len = 0;
   float *buffer = NULL;
   bool warning = false;
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   while (t) {
      if (t->GetSelected() && t->GetKind() == Track::Wave) {
         WaveTrack *track = (WaveTrack *)t;
         if (selcount==0) {
            rate = track->GetRate();
            longSampleCount start, end;
            start = track->TimeToLongSamples(mViewInfo.sel0);
            end = track->TimeToLongSamples(mViewInfo.sel1);
            len = (sampleCount)(end - start);
            if (len > 1048576) {
               warning = true;
               len = 1048576;
            }
            buffer = new float[len];
            track->Get((samplePtr)buffer, floatSample, start, len);
         }
         else {
            if (track->GetRate() != rate) {
               wxMessageBox(_("To plot the spectrum, all selected tracks must be the same sample rate."));
               delete[] buffer;
               return;
            }
            longSampleCount start;
            start = track->TimeToLongSamples(mViewInfo.sel0);
            float *buffer2 = new float[len];
            track->Get((samplePtr)buffer2, floatSample, start, len);
            for(i=0; i<len; i++)
               buffer[i] += buffer2[i];
            delete[] buffer2;
         }
         selcount++;
      }
      t = iter.Next();
   }
   
   if (selcount == 0)
      return;
   
   if (selcount > 1)
      for(i=0; i<len; i++)
         buffer[i] /= selcount;
   
   if (warning) {
      wxString msg;
      msg.Printf(_("Too much audio was selected.  Only the first %.1f seconds of audio will be analyzed."),
                          (len / rate));
      wxMessageBox(msg);
   }

   gFreqWindow->Plot(len, buffer, rate);
   gFreqWindow->Show(true);
   gFreqWindow->Raise();

   delete[] buffer;
}


void AudacityProject::OnFloatControlToolBar(wxEvent & event)
{
   if (gControlToolBarStub->GetWindowedStatus()) {

      gControlToolBarStub->HideWindowedToolBar();
      gControlToolBarStub->LoadAll();
   } else {
      gControlToolBarStub->ShowWindowedToolBar();
      gControlToolBarStub->UnloadAll();
   }
}


void AudacityProject::OnLoadEditToolBar(wxEvent & event)
{
   if (gEditToolBarStub) {
      if (gEditToolBarStub->GetLoadedStatus()) {

         //the toolbar is "loaded", meaning its visible either in the window
         //or floating

         gEditToolBarStub->SetLoadedStatus(false);
         gEditToolBarStub->HideWindowedToolBar();
         gEditToolBarStub->UnloadAll();

      } else {

         //the toolbar is not "loaded", meaning that although the stub exists, 
         //the toolbar is not visible either in a window or floating around
         gEditToolBarStub->SetLoadedStatus(true);

         if (gEditToolBarStub->GetWindowedStatus()) {
            //Make the floating toolbar appear
            gEditToolBarStub->ShowWindowedToolBar();
            gEditToolBarStub->LoadAll();
         } else {
            //Make it appear in all the windows
            gEditToolBarStub->LoadAll();
         }

      }
   } else {
      gEditToolBarStub = new ToolBarStub(gParentWindow, EditToolBarID);
      gEditToolBarStub->LoadAll();
   }
}


void AudacityProject::OnFloatEditToolBar(wxEvent & event)
{
   if (gEditToolBarStub) {

      if (gEditToolBarStub->GetWindowedStatus()) {

         gEditToolBarStub->HideWindowedToolBar();
         gEditToolBarStub->LoadAll();

      } else {

         gEditToolBarStub->ShowWindowedToolBar();
         gEditToolBarStub->UnloadAll();
      }
   }
}


//
// Project Menu
//

void AudacityProject::OnImport(wxEvent & event)
{
   wxString path = gPrefs->Read("/DefaultOpenPath",::wxGetCwd());

   // TODO: Build the list of file types dynamically
   wxString fileName = wxFileSelector(_("Select an audio file..."),
                                      path,     // Path
                                      "",       // Name
                                      "",       // Extension
                                      _("All files (*.*)|*.*|"
                                        "WAV files (*.wav)|*.wav|"
                                        "AIFF files (*.aif)|*.aif|"
                                        "AU files (*.au)|*.au|"
                                        "IRCAM files (*.snd)|*.snd|"
                                        "MP3 files (*.mp3)|*.mp3|"
                                        "Ogg Vorbis files (*.ogg)|*.ogg"),
                                      0,        // Flags
                                      this);    // Parent

   if (fileName != "") {
      path =::wxPathOnly(fileName);
      gPrefs->Write("/DefaultOpenPath", path);

      Import(fileName);
   }
}

void AudacityProject::OnImportLabels(wxEvent & event)
{
   wxString path = gPrefs->Read("/DefaultOpenPath",::wxGetCwd());

   wxString fileName =
       wxFileSelector(_("Select a text file containing labels..."),
                      path,     // Path
                      "",       // Name
                      ".txt",   // Extension
                      _("Text files (*.txt)|*.txt|" "All files (*.*)|*.*"),
                      0,        // Flags
                      this);    // Parent

   if (fileName != "") {
      path =::wxPathOnly(fileName);
      gPrefs->Write("/DefaultOpenPath", path);

      wxTextFile f;

      f.Open(fileName);
      if (!f.IsOpened()) {
         wxMessageBox(_("Could not open file: ") + fileName);
         return;
      }

      LabelTrack *newTrack = new LabelTrack(&mDirManager);

      newTrack->Import(f);

      SelectNone();
      mTracks->Add(newTrack);
      newTrack->SetSelected(true);

      PushState(wxString::
                Format(_("Imported labels from '%s'"), fileName.c_str()));

      FixScrollbars();
      mTrackPanel->Refresh(false);
   }
}

void AudacityProject::OnImportMIDI(wxEvent & event)
{
   wxString path = gPrefs->Read("/DefaultOpenPath",::wxGetCwd());

   wxString fileName = wxFileSelector(_("Select a MIDI file..."),
                                      path,     // Path
                                      "",       // Name
                                      "",       // Extension
                                      _("All files (*.*)|*.*|"
                                        "MIDI files (*.mid)|*.mid|"
                                        "Allegro files (*.gro)|*.gro"),
                                      0,        // Flags
                                      this);    // Parent

   if (fileName != "") {
      path =::wxPathOnly(fileName);
      gPrefs->Write("/DefaultOpenPath", path);

      NoteTrack *newTrack = new NoteTrack(&mDirManager);

      if (0/*::ImportMIDI(fileName, newTrack)*/) {

         SelectNone();
         mTracks->Add(newTrack);
         newTrack->SetSelected(true);

         PushState(wxString::Format(_("Imported MIDI from '%s'"),
                                    fileName.c_str()));

         FixScrollbars();
         mTrackPanel->Refresh(false);
      }
   }
}

void AudacityProject::OnImportRaw(wxEvent & event)
{
   wxString path = gPrefs->Read("/DefaultOpenPath",::wxGetCwd());

   wxString fileName =
       wxFileSelector(_("Select any uncompressed audio file..."),
                      path,     // Path
                      "",       // Name
                      "",       // Extension
                      _("All files (*.*)|*.*"),
                      0,        // Flags
                      this);    // Parent

   if (fileName != "") {
      path =::wxPathOnly(fileName);
      gPrefs->Write("/DefaultOpenPath", path);

      WaveTrack *left = 0;
      WaveTrack *right = 0;

      if (0/*::ImportRaw(this, fileName, &left, &right, &mDirManager)*/) {

         SelectNone();

         if (left) {
            mTracks->Add(left);
            left->SetSelected(true);
         }

         if (right) {
            mTracks->Add(right);
            right->SetSelected(true);
         }

         PushState(wxString::Format(_("Imported raw audio from '%s'"),
                                    fileName.c_str()));

         FixScrollbars();
         mTrackPanel->Refresh(false);
      }
   }
}

void AudacityProject::OnEditID3(wxEvent & event)
{
   mTags->ShowEditDialog(this, _("Edit ID3 Tags (for MP3 exporting)"));
}

void AudacityProject::OnQuickMix(wxEvent & event)
{
   if (::QuickMix(mTracks, mTrackFactory, mRate, mDefaultFormat)) {

      // After the tracks have been mixed, remove the originals

      TrackListIterator iter(mTracks);
      Track *t = iter.First();

      while (t) {
         if (t->GetSelected())
            t = iter.RemoveCurrent();
         else
            t = iter.Next();
      }

      PushState(_("Quick mix"));

      FixScrollbars();
      mTrackPanel->Refresh(false);
   }
}

void AudacityProject::OnAlignZero(wxEvent & event)
{
   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected())
         t->SetOffset(0.0);

      t = iter.Next();
   }

   PushState(_("Aligned with zero"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlign(wxEvent & event)
{
   double avg = 0.0;
   int num = 0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         avg += t->GetOffset();
         num++;
      }

      t = iter.Next();
   }

   if (num) {
      avg /= num;

      TrackListIterator iter2(mTracks);
      t = iter2.First();

      while (t) {
         if (t->GetSelected())
            t->SetOffset(avg);

         t = iter2.Next();
      }
   }

   PushState(_("Aligned tracks"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnNewWaveTrack(wxEvent & event)
{
   WaveTrack *t = mTrackFactory->NewWaveTrack(mDefaultFormat);
   t->SetRate(mRate);
   SelectNone();

   mTracks->Add(t);
   t->SetSelected(true);

   PushState(_("Created new audio track"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnNewLabelTrack(wxEvent & event)
{
   LabelTrack *t = new LabelTrack(&mDirManager);

   SelectNone();

   mTracks->Add(t);
   t->SetSelected(true);

   PushState(_("Created new label track"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnRemoveTracks(wxEvent & event)
{
   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected())
         t = iter.RemoveCurrent();
      else
         t = iter.Next();
   }

   PushState(_("Removed audio track(s)"));

   mTrackPanel->Refresh(false);
}

//
// Help Menu
//

void AudacityProject::OnAbout(wxEvent & event)
{
   AboutDialog dlog(this);
   dlog.ShowModal();
}

void AudacityProject::OnHelp(wxEvent & event)
{
   ::ShowHelp(this);
}

void AudacityProject::OnHelpIndex(wxEvent & event)
{
   ::ShowHelpIndex(this);
}

void AudacityProject::OnHelpSearch(wxEvent & event)
{
   ::SearchHelp(this);
}

void AudacityProject::OnBenchmark(wxEvent & event)
{
   ::RunBenchmark(this);
}

//

void AudacityProject::OnSeparator(wxEvent & event)
{

}
