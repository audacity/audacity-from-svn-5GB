/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandsCallback.cpp

  Dominic Mazzoni
  Brian Gunlogson
  et. al.

  This file implements all of the methods that get called when you
  select an item from a menu.

**********************************************************************/

#include <wx/defs.h>
#include <wx/msgdlg.h>
#include <wx/filedlg.h>
#include <wx/textfile.h>

#include "../Project.h"

#include "../LabelTrack.h"
#include "../export/Export.h"
#include "../prefs/PrefsDialog.h"
#include "../HistoryWindow.h"
#include "../FreqWindow.h"
#include "../Prefs.h"
#include "../NoteTrack.h"
#include "../Tags.h"
#include "../Mix.h"
#include "../AboutDialog.h"
#include "../Help.h"
#include "../Benchmark.h"

// global variables used in routines OnSelectionSave
// and OnSelectionRestore
double sel0save = 0.0;
double sel1save = 0.0;

///////////////////////////////////////////////////////////////////////
// The actucal callbacks

//
// File Menu
//

void AudacityProject::OnNew()
{
   CreateNewAudacityProject(gParentWindow);
}

void AudacityProject::OnOpen()
{
   ShowOpenDialog(this);
}

void AudacityProject::OnClose()
{
   Close();
}

void AudacityProject::OnSave()
{
   Save();
}

void AudacityProject::OnSaveAs()
{
   SaveAs();
}

void AudacityProject::OnExit()
{
   QuitAudacity();
}

void AudacityProject::OnExportLabels()
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

void AudacityProject::OnExportMix()
{
   ::Export(this, false, 0.0, mTracks->GetEndTime());
}

void AudacityProject::OnExportSelection()
{
   ::Export(this, true, mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnExportLossyMix()
{
   ::ExportLossy(this, false, 0.0, mTracks->GetEndTime());
}

void AudacityProject::OnExportLossySelection()
{
   ::ExportLossy(this, true, mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnPreferences()
{
   PrefsDialog dialog(this /* parent */ );
   dialog.ShowModal();
}

//
// Edit Menu
//

void AudacityProject::Undo()
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

void AudacityProject::Redo()
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

void AudacityProject::UndoHistory()
{
   if (mHistoryWindow)
      mHistoryWindow->Show(true);
   else {
      mHistoryWindow = new HistoryWindow(this, &mUndoManager);
      mHistoryWindow->Show(true);
   }
}


void AudacityProject::Cut()
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



void AudacityProject::Copy()
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

void AudacityProject::Paste()
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

void AudacityProject::PasteOver()
{
   if(msClipLen>0.0)
   {
      mViewInfo.sel1=mViewInfo.sel0+msClipLen;
      Paste();
   }

   return;
}

void AudacityProject::Trim()
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



void AudacityProject::OnDelete()
{
   Clear();
}

void AudacityProject::OnSilence()
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

void AudacityProject::OnDuplicate()
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

void AudacityProject::OnSplit()
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

void AudacityProject::OnSplitLabels()
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

void AudacityProject::OnSelectAll()
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
}

void AudacityProject::OnSelectCursorEnd()
{
   double maxEndOffset = -1000000.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetEndTime() > maxEndOffset)
            maxEndOffset = t->GetEndTime();
      }

      t = iter.Next();
   }

   mViewInfo.sel1 = maxEndOffset;

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelectStartCursor()
{
   double minOffset = 1000000.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetOffset() < minOffset)
            minOffset = t->GetOffset();
      }

      t = iter.Next();
   }

   mViewInfo.sel0 = minOffset;

   mTrackPanel->Refresh(false);
}

//
// View Menu
//

void AudacityProject::OnZoomIn()
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

void AudacityProject::OnZoomOut()
{  
   //Zoom() may change these, so record original values:
   double origLeft = mViewInfo.h;
   double origWidth = mViewInfo.screen;

   Zoom(mViewInfo.zoom /= 2.0);

   double newh = origLeft + (origWidth - mViewInfo.screen) / 2;
   // newh = (newh > 0) ? newh : 0;
   TP_ScrollWindow(newh);

}

void AudacityProject::OnZoomNormal()
{
   Zoom(44100.0 / 512.0);
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnZoomFit()
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

void AudacityProject::OnZoomSel()
{
   if (mViewInfo.sel1 <= mViewInfo.sel0)
      return;

   Zoom(mViewInfo.zoom * mViewInfo.screen / (mViewInfo.sel1 - mViewInfo.sel0)),
   TP_ScrollWindow(mViewInfo.sel0);
}

void AudacityProject::OnPlotSpectrum()
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


void AudacityProject::OnFloatControlToolBar()
{
   if (gControlToolBarStub->GetWindowedStatus()) {

      gControlToolBarStub->HideWindowedToolBar();
      gControlToolBarStub->LoadAll();
   } else {
      gControlToolBarStub->ShowWindowedToolBar();
      gControlToolBarStub->UnloadAll();
   }
}


void AudacityProject::OnLoadEditToolBar()
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


void AudacityProject::OnFloatEditToolBar()
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

void AudacityProject::OnImport()
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

void AudacityProject::OnImportLabels()
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

void AudacityProject::OnImportMIDI()
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

void AudacityProject::OnImportRaw()
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

void AudacityProject::OnEditID3()
{
   mTags->ShowEditDialog(this, _("Edit ID3 Tags (for MP3 exporting)"));
}

void AudacityProject::OnQuickMix()
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

void AudacityProject::OnSelectionSave()
{
   sel0save = mViewInfo.sel0;
   sel1save = mViewInfo.sel1;
}

void AudacityProject::OnSelectionRestore()
{
   mViewInfo.sel0 = sel0save;
   mViewInfo.sel1 = sel1save;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorTrackStart()
{
   double minOffset = 1000000.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetOffset() < minOffset)
            minOffset = t->GetOffset();
      }

      t = iter.Next();
   }

   if (minOffset < 0.0) minOffset = 0.0;
   mViewInfo.sel0 = minOffset;
   mViewInfo.sel1 = minOffset;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorTrackEnd()
{
   double maxEndOffset = -1000000.0;
   double thisEndOffset = 0.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         thisEndOffset = t->GetEndTime();
         if (thisEndOffset > maxEndOffset)
            maxEndOffset = thisEndOffset;
      }

      t = iter.Next();
   }

   mViewInfo.sel0 = maxEndOffset;
   mViewInfo.sel1 = maxEndOffset;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorSelStart()
{
   mViewInfo.sel1 = mViewInfo.sel0;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorSelEnd()
{
   mViewInfo.sel0 = mViewInfo.sel1;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlignZero()
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

void AudacityProject::OnAlign()
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

void AudacityProject::OnAlignCursor()
{

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected())
         t->SetOffset(mViewInfo.sel0);

      t = iter.Next();
   }

   PushState(_("Aligned with sel0"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlignSelStart()
{

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected())
         t->SetOffset(mViewInfo.sel0);

      t = iter.Next();
   }

   PushState(_("Aligned with sel0"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlignSelEnd()
{

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected())
         t->SetOffset(mViewInfo.sel1);

      t = iter.Next();
   }

   PushState(_("Aligned with sel1"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlignEndCursor()
{

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
            t->SetOffset(0.0);
            t->SetOffset(mViewInfo.sel0 - t->GetEndTime() );
         }

      t = iter.Next();
   }

   PushState(_("Aligned end with sel0"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlignEndSelStart()
{

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
            t->SetOffset(0.0);
            t->SetOffset(mViewInfo.sel0 - t->GetEndTime() );
         }

      t = iter.Next();
   }

   PushState(_("Aligned end with sel0"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlignEndSelEnd()
{

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         t->SetOffset(0.0);
         t->SetOffset(mViewInfo.sel1 - t->GetEndTime() );
      }

      t = iter.Next();
   }

   PushState(_("Aligned end with sel1"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlignGroupCursor()
{
   double minOffset = 1000000.0;
   double delta = 0.0;
   double thisOffset = 0.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetOffset() < minOffset)
            minOffset = t->GetOffset();
      }

      t = iter.Next();
   }

   delta = mViewInfo.sel0 - minOffset;


   TrackListIterator iter2(mTracks);
   t = iter2.First();

   while (t) {
      if (t->GetSelected()) {
         thisOffset = t->GetOffset();
         t->SetOffset(thisOffset + delta);
      }

      t = iter2.Next();
   }

   PushState(_("Aligned group with sel0"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlignGroupSelStart()
{
   double minOffset = 1000000.0;
   double delta = 0.0;
   double thisOffset = 0.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetOffset() < minOffset)
            minOffset = t->GetOffset();
      }

      t = iter.Next();
   }

   delta = mViewInfo.sel0 - minOffset;


   TrackListIterator iter2(mTracks);
   t = iter2.First();

   while (t) {
      if (t->GetSelected()) {
         thisOffset = t->GetOffset();
         t->SetOffset(thisOffset + delta);
      }

      t = iter2.Next();
   }

   PushState(_("Aligned group with sel0"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlignGroupSelEnd()
{
   double minOffset = 1000000.0;
   double delta = 0.0;
   double thisOffset = 0.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetOffset() < minOffset)
            minOffset = t->GetOffset();
      }

      t = iter.Next();
   }

   delta = mViewInfo.sel1 - minOffset;


   TrackListIterator iter2(mTracks);
   t = iter2.First();

   while (t) {
      if (t->GetSelected()) {
         thisOffset = t->GetOffset();
         t->SetOffset(thisOffset + delta);
      }

      t = iter2.Next();
   }

   PushState(_("Aligned group with sel1"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlignGroupEndCursor()
{
   double maxEndOffset = -1000000.0;
   double delta = 0.0;
   double thisOffset = 0.0;
   double thisEndOffset = 0.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         thisEndOffset = t->GetEndTime();
         if (thisEndOffset > maxEndOffset)
            maxEndOffset = thisEndOffset;
      }

      t = iter.Next();
   }

   delta = mViewInfo.sel0 - maxEndOffset;

   TrackListIterator iter2(mTracks);
   t = iter2.First();

   while (t) {
      if (t->GetSelected()) {
         thisOffset = t->GetOffset();
         t->SetOffset(thisOffset + delta);
      }

      t = iter2.Next();
   }

   PushState(_("Aligned group end with sel0"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlignGroupEndSelStart()
{
   double maxEndOffset = -1000000.0;
   double delta = 0.0;
   double thisOffset = 0.0;
   double thisEndOffset = 0.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         thisEndOffset = t->GetEndTime();
         if (thisEndOffset > maxEndOffset)
            maxEndOffset = thisEndOffset;
      }

      t = iter.Next();
   }

   delta = mViewInfo.sel0 - maxEndOffset;

   TrackListIterator iter2(mTracks);
   t = iter2.First();

   while (t) {
      if (t->GetSelected()) {
         thisOffset = t->GetOffset();
         t->SetOffset(thisOffset + delta);
      }

      t = iter2.Next();
   }

   PushState(_("Aligned group end with sel0"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlignGroupEndSelEnd()
{
   double maxEndOffset = -1000000.0;
   double delta = 0.0;
   double thisOffset = 0.0;
   double thisEndOffset = 0.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         thisEndOffset = t->GetEndTime();
         if (thisEndOffset > maxEndOffset)
            maxEndOffset = thisEndOffset;
      }

      t = iter.Next();
   }

   delta = mViewInfo.sel1 - maxEndOffset;

   TrackListIterator iter2(mTracks);
   t = iter2.First();

   while (t) {
      if (t->GetSelected()) {
         thisOffset = t->GetOffset();
         t->SetOffset(thisOffset + delta);
      }

      t = iter2.Next();
   }

   PushState(_("Aligned group end with sel0"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnNewWaveTrack()
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

void AudacityProject::OnNewLabelTrack()
{
   LabelTrack *t = new LabelTrack(&mDirManager);

   SelectNone();

   mTracks->Add(t);
   t->SetSelected(true);

   PushState(_("Created new label track"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

//FIXME: This does not work for selections.
//LabelTrack appears to be missing functionality
void AudacityProject::OnAddLabel()
{
   TrackListIterator iter(mTracks);
   LabelTrack *lt = NULL;

   Track *t = iter.First();
   while (t && !lt) {
      if (t->GetKind() == Track::Label)
         lt = (LabelTrack *)t;
      else
         t = iter.Next();
   }

   if (!lt) {
      lt = new LabelTrack(&mDirManager);

      SelectNone();

      mTracks->Add(lt);
   }

   lt->Add(mViewInfo.sel0, "");

   PushState(_("Added label"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnRemoveTracks()
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

void AudacityProject::OnAbout()
{
   AboutDialog dlog(this);
   dlog.ShowModal();
}

void AudacityProject::OnHelp()
{
   ::ShowHelp(this);
}

void AudacityProject::OnHelpIndex()
{
   ::ShowHelpIndex(this);
}

void AudacityProject::OnHelpSearch()
{
   ::SearchHelp(this);
}

void AudacityProject::OnBenchmark()
{
   ::RunBenchmark(this);
}

//

void AudacityProject::OnSeparator()
{

}
