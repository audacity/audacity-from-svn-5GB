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

#ifndef WX_PRECOMP
#include <wx/app.h>
#include <wx/intl.h>
#include <wx/menu.h>
#include <wx/string.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>

#endif

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


#define AUDACITY_MENUS_ENUM
#include "Menus.h"
#undef AUDACITY_MENUS_ENUM


void AudacityProject::CreateMenuBar()
{
   unsigned int i;

   mMenusDirtyCheck = gMenusDirty;
   mFirstTimeUpdateMenus = true;

   mMenuBar = new wxMenuBar();

#define AUDACITY_MENUS_COMMANDS_EVENT_TABLE
#include "commands.h"           // BG: Generate an array of command names, and their corresponding functions
#undef AUDACITY_MENUS_COMMANDS_EVENT_TABLE
#include "commandkeys.h"        // BG: Generate an array of keys combos that cannot be used

   mFileMenu = new wxMenu();
   mEditMenu = new wxMenu();
   mViewMenu = new wxMenu();
   mProjectMenu = new wxMenu();
   mHelpMenu = new wxMenu();

   wxMenu *menu = 0;
   for (i = 0; i < mCommandMenuItem.Count(); i++) {
      if (mCommandMenuItem[i]->separatorPrev)
         menu->AppendSeparator();

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

      menu->Append(i + MenuBaseID, mCommandMenuItem[i]->commandString);
   }

   mEffectMenu = new wxMenu();

   int numEffects = Effect::GetNumEffects(false);
   int fi;
   for (fi = 0; fi < numEffects; fi++)
      mEffectMenu->Append(FirstEffectID + fi,
                          (Effect::GetEffect(fi, false))->GetEffectName());

   mPluginMenu = new wxMenu();

   int numPlugins = Effect::GetNumEffects(true);
   for (fi = 0; fi < numPlugins; fi++)
      mPluginMenu->Append(FirstPluginID + fi,
                          (Effect::GetEffect(fi, true))->GetEffectName());

#ifdef __WXMAC__
   wxApp::s_macAboutMenuItemId = AboutID;
#endif

   mMenuBar->Append(mFileMenu, _("&File"));
   mMenuBar->Append(mEditMenu, _("&Edit"));
   mMenuBar->Append(mViewMenu, _("&View"));
   mMenuBar->Append(mProjectMenu, _("&Project"));
   mMenuBar->Append(mEffectMenu, _("Effec&t"));
   mMenuBar->Append(mPluginMenu, _("Plugin&s"));
   mMenuBar->Append(mHelpMenu, _("&Help"));

   SetMenuBar(mMenuBar);

   mInsertSilenceAmount = 1.0;
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

audEventFunction AudacityProject::GetCommandFunc(int nIndex)
{
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
   return mCommandMenuItem[nIndex]->state;
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

   TrackListIterator iter(mTracks);
   VTrack *t = iter.First();
   while (t) {
      numTracks++;
      if (t->GetKind() == VTrack::Wave)
         numWaveTracks++;
      if (t->GetKind() == VTrack::Label)
         numLabelTracks++;
      if (t->GetSelected()) {
         numTracksSelected++;
         if (t->GetKind() == VTrack::Wave)
            numWaveTracksSelected++;
      }
      t = iter.Next();
   }

   SetMenuState(mEditMenu, PasteID, numTracksSelected > 0 && msClipLen > 0.0);

   //Calculate the ToolBarCheckSum (uniquely specifies state of all toolbars):
   int toolBarCheckSum = 0;
   toolBarCheckSum += gControlToolBarStub->GetWindowedStatus() ? 2 : 1;
   if (gEditToolBarStub->GetLoadedStatus()) {
      if(gEditToolBarStub->GetWindowedStatus())
         toolBarCheckSum += 6;
      else
         toolBarCheckSum += 3;
   }
   
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
       mLastRedoState == mUndoManager.RedoAvailable()  
       )
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
   SetMenuState(mEditMenu, DuplicateID, anySelection);
   SetMenuState(mEditMenu, SelectAllID, numTracks > 0);

   SetMenuState(mEditMenu, UndoID, mUndoManager.UndoAvailable());
   SetMenuState(mEditMenu, RedoID, mUndoManager.RedoAvailable());


   SetMenuState(mViewMenu, PlotSpectrumID, numWaveTracksSelected > 0
                     && nonZeroRegionSelected);

#ifndef __WXMAC__
   //Modify toolbar-specific Menus

   if (gEditToolBarStub) {
      wxMenuItemBase *load = mViewMenu->FindItem(LoadEditToolBarID),
                     *dock = mViewMenu->FindItem(FloatEditToolBarID);

      // Loaded or unloaded?
      if (gEditToolBarStub->GetLoadedStatus()) {
         load->SetName(_("Unload Edit Toolbar"));
         dock->Enable(true);
      } else {
         load->SetName(_("Load Edit Toolbar"));
         dock->Enable(false);
      }

      // Floating or docked?
      if (gEditToolBarStub->GetWindowedStatus())
         dock->SetName(_("Dock Edit Toolbar"));
      else
         dock->SetName(_("Float Edit Toolbar"));
   }
#endif

   SetMenuState(mProjectMenu, QuickMixID, numWaveTracksSelected > 1);
   SetMenuState(mProjectMenu, AlignID, numTracksSelected > 1);
   SetMenuState(mProjectMenu, AlignZeroID, numTracksSelected > 0);
   SetMenuState(mProjectMenu, RemoveTracksID, numTracksSelected > 0);

   int e;
   for (e = 0; e < Effect::GetNumEffects(false); e++) {
      mEffectMenu->Enable(FirstEffectID + e,
                          numWaveTracksSelected > 0
                          && nonZeroRegionSelected);
   }

   for (e = 0; e < Effect::GetNumEffects(true); e++) {
      mPluginMenu->Enable(FirstPluginID + e,
                          numWaveTracksSelected > 0
                          && nonZeroRegionSelected);
   }

   //Now, go through each toolbar, and and call EnableDisableButtons()
   unsigned int i;
   for (i = 0; i < mToolBarArray.GetCount(); i++) {
      mToolBarArray[i]->EnableDisableButtons();
   }

   //Now, do the same thing for the (possibly invisible) floating toolbars
   gControlToolBarStub->GetToolBar()->EnableDisableButtons();
   gEditToolBarStub->GetToolBar()->EnableDisableButtons();
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
   wxString path = gPrefs->Read("/DefaultOpenPath",::wxGetCwd());

   wxString fileName = wxFileSelector(_("Select an audio file..."),
                                      path,     // Path
                                      "",       // Name
                                      "",       // Extension
                                      _("All files (*.*)|*.*|"
                                        "Audacity projects (*.aup)|*.aup|"
                                        "WAV files (*.wav)|*.wav|"
                                        "AIFF files (*.aif)|*.aif|"
                                        "AU files (*.au)|*.au|"
                                        "IRCAM files (*.snd)|*.snd|"
                                        "MP3 files (*.mp3)|*.mp3"),
                                      0,        // Flags
                                      this);    // Parent

   if (fileName != "") {
      gPrefs->Write("/DefaultOpenPath", wxPathOnly(fileName));

      // Make sure it isn't already open
      int numProjects = gAudacityProjects.Count();
      for (int i = 0; i < numProjects; i++)
         if (gAudacityProjects[i]->mFileName == fileName) {
            wxMessageBox
                ("That project is already open in another window.");
            return;
         }
      // Open in a new window if this one is in use
      if (mDirty || !mTracks->IsEmpty()) {
         AudacityProject *project =
             CreateNewAudacityProject(gParentWindow);
         project->OpenFile(fileName);
      } else
         OpenFile(fileName);
   }
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
   VTrack *t;
   int numLabelTracks = 0;

   TrackListIterator iter(mTracks);

   t = iter.First();
   while (t) {
      if (t->GetKind() == VTrack::Label)
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
      if (t->GetKind() == VTrack::Label)
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
   ::Export(this, false, 0.0, mTracks->GetMaxLen());
}

void AudacityProject::OnExportSelection(wxEvent & event)
{
   ::Export(this, true, mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnExportLossyMix(wxEvent & event)
{
   ::ExportLossy(this, false, 0.0, mTracks->GetMaxLen());
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

   VTrack *n = iter.First();
   VTrack *dest = 0;

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

   VTrack *n = iter.First();
   VTrack *dest = 0;

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
}

void AudacityProject::Paste(wxEvent & event)
{
   if (mViewInfo.sel0 != mViewInfo.sel1)
      Clear();

   wxASSERT(mViewInfo.sel0 == mViewInfo.sel1);

   double tsel = mViewInfo.sel0;

   TrackListIterator iter(mTracks);
   TrackListIterator clipIter(msClipboard);

   VTrack *n = iter.First();
   VTrack *c = clipIter.First();

   while (n && c) {
      if (n->GetSelected()) {
         if (msClipProject != this && c->GetKind() == VTrack::Wave)
            ((WaveTrack *) c)->Lock();

         n->Paste(tsel, c);

         if (msClipProject != this && c->GetKind() == VTrack::Wave)
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
   VTrack *n = iter.First();

   while (n) {
      if (n->GetSelected()) {
         //Delete the section before the left selector
         n->Clear(n->GetOffset(), mViewInfo.sel0);
         if (mViewInfo.sel0 > n->GetOffset())
            n->SetOffset(mViewInfo.sel0);

         //Delete the section after the right selector
         n->Clear(mViewInfo.sel1, n->GetMaxLen());
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

   VTrack *n = iter.First();

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

   VTrack *n = iter.First();
   VTrack *dest = 0;

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

   VTrack *n = iter.First();
   VTrack *dest = 0;

   TrackList newTracks;

   while (n) {
      if (n->GetSelected()) {
         double sel0 = mViewInfo.sel0;
         double sel1 = mViewInfo.sel1;

         n->Copy(sel0, sel1, &dest);
         if (dest) {
            dest->Init(*n);
            dest->SetOffset(wxMax(sel0, n->GetOffset()));

            if (sel1 >= n->GetMaxLen())
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

   VTrack *n = iter.First();

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

   VTrack *t = iter.First();
   while (t) {
      t->SetSelected(true);
      t = iter.Next();
   }
   mViewInfo.sel0 = mTracks->GetMinOffset();
   mViewInfo.sel1 = mTracks->GetMaxLen();

   mTrackPanel->Refresh(false);
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
   TP_ScrollWindow(origLeft + (origWidth - mViewInfo.screen) / 2 );
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
   double len = mTracks->GetMaxLen();

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
#if 0


   TODO NOW int selcount = 0;
   WaveTrack *selt = NULL;
   TrackListIterator iter(mTracks);
   VTrack *t = iter.First();
   while (t) {
      if (t->GetSelected())
         selcount++;
      if (t->GetKind() == VTrack::Wave)
         selt = (WaveTrack *) t;
      t = iter.Next();
   }
   if (selcount != 1) {
      wxMessageBox(_("Please select a single track first.\n"));
      return;
   }

   /* This shouldn't be possible, since the menu is grayed out.
    * But we'll check just in case it does happen, to prevent
    * the crash that would result. */

   if (!selt) {
      wxMessageBox(_("Please select a track first.\n"));
      return;
   }


   sampleCount s0 = (sampleCount) ((mViewInfo.sel0 - selt->GetOffset())
                                   * selt->GetRate());
   sampleCount s1 = (sampleCount) ((mViewInfo.sel1 - selt->GetOffset())
                                   * selt->GetRate());
   sampleCount slen = s1 - s0;

   if (slen > 1048576)
      slen = 1048576;

   float *data = new float[slen];
   sampleType *data_sample = new sampleType[slen];

   if (s0 >= selt->GetNumSamples() || s0 + slen > selt->GetNumSamples()) {
      wxMessageBox(_("Not enough samples selected.\n"));
      delete[]data;
      delete[]data_sample;
      return;
   }

   selt->Get(data_sample, s0, slen);

   for (sampleCount i = 0; i < slen; i++)
      data[i] = data_sample[i] / 32767.;

   gFreqWindow->Plot(slen, data, selt->GetRate());
   gFreqWindow->Show(true);
   gFreqWindow->Raise();

   delete[]data;
   delete[]data_sample;

#endif
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

      if (::ImportMIDI(fileName, newTrack)) {

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

      if (::ImportRaw(this, fileName, &left, &right, &mDirManager)) {

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
   if (::QuickMix(mTracks, &mDirManager, mRate, mDefaultFormat)) {

      // After the tracks have been mixed, remove the originals

      TrackListIterator iter(mTracks);
      VTrack *t = iter.First();

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
   VTrack *t = iter.First();

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
   VTrack *t = iter.First();

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
   WaveTrack *t = new WaveTrack(&mDirManager);

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
   VTrack *t = iter.First();

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
