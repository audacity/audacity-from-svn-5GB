/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.cpp

  Dominic Mazzoni
  Brian Gunlogson
  et. al.

  This file implements the method that creates the menu bar, plus
  all of the methods that get called when you select an item
  from a menu.

**********************************************************************/

#include "../Audacity.h"

#include <math.h>

#include <wx/defs.h>
#include <wx/msgdlg.h>
#include <wx/filedlg.h>
#include <wx/textfile.h>
#include <wx/progdlg.h>

#include "../Project.h"

#include "../AudioIO.h"
#include "../LabelTrack.h"
#include "../import/ImportMIDI.h"
#include "../import/ImportRaw.h"
#include "../export/Export.h"
#include "../prefs/PrefsDialog.h"
#include "../HistoryWindow.h"
#include "../FileFormats.h"
#include "../FormatSelection.h"
#include "../FreqWindow.h"
#include "../Prefs.h"
#include "../NoteTrack.h"
#include "../Tags.h"
#include "../Mix.h"
#include "../AboutDialog.h"
#include "../Help.h"
#include "../Benchmark.h"

enum {
   kAlignZero=0,
   kAlignCursor,
   kAlignSelStart,
   kAlignSelEnd,
   kAlignEndCursor,
   kAlignEndSelStart,
   kAlignEndSelEnd,
   kAlign
};

typedef void (AudacityProject::*audCommandFunction)();
typedef void (AudacityProject::*audCommandListFunction)(int);

class AudacityProjectCommandFunctor:public CommandFunctor
{
public:
   AudacityProjectCommandFunctor(AudacityProject *project,
                                 audCommandFunction commandFunction)
   {
      mProject = project;
      mCommandFunction = commandFunction;
      mCommandListFunction = NULL;
   }

   AudacityProjectCommandFunctor(AudacityProject *project,
                                 audCommandListFunction commandFunction)
   {
      mProject = project;
      mCommandFunction = NULL;
      mCommandListFunction = commandFunction;
   }

   virtual void operator()(int index = 0)
   {
      if (mCommandListFunction)
         (mProject->*(mCommandListFunction)) (index);
      else
         (mProject->*(mCommandFunction)) ();
   }

private:
   AudacityProject *mProject;
   audCommandFunction mCommandFunction;
   audCommandListFunction mCommandListFunction;
};

#define FN(X) new AudacityProjectCommandFunctor(this, &AudacityProject:: X )

void AudacityProject::CreateMenusAndCommands()
{
   CommandManager *c = &mCommandManager;
   EffectArray *effects;
   wxArrayString names;
   unsigned int i;

// Use the following to build only a tiny part of the interface
// So as to track down the memory leak.
//#define BuildInterface 1
#define BuildInterface 0

   wxMenuBar *menubar = c->AddMenuBar("appmenu");

   c->BeginMenu(_("&File"));
   c->AddItem("New",            _("&New\tCtrl+N"),                   FN(OnNew));
   c->AddItem("Open",           _("&Open...\tCtrl+O"),               FN(OnOpen));
   c->AddItem("Close",          _("&Close\tCtrl+W"),                 FN(OnClose));
   c->AddItem("Save",           _("&Save Project\tCtrl+S"),          FN(OnSave));
   c->AddItem("SaveAs",         _("Save Project &As..."),            FN(OnSaveAs));
   c->AddSeparator();
   c->AddItem("Export",         "Export As...",                      FN(OnExportMix));
   c->AddItem("ExportSel",      "Export Selection As...",            FN(OnExportSelection));
   c->AddSeparator();
   c->AddItem("ExportLossy",    "Export As Lossy...",                FN(OnExportLossyMix));
   c->AddItem("ExportLossySel", "Export Selection As Lossy...",      FN(OnExportLossySelection));
   c->AddSeparator();
   c->AddItem("ExportLabels",   _("Export &Labels..."),              FN(OnExportLabels));

   //
   // On Mac OS X, Preferences and Quit are in the application menu,
   // not the File menu.
   //

#ifndef __WXMAC__
   c->AddSeparator();
   c->AddItem("Preferences",    _("&Preferences...\tCtrl+P"),        FN(OnPreferences));
   c->AddSeparator();

   c->AddItem("Exit",           _("E&xit"),                          FN(OnExit));
#endif

   c->EndMenu();

   c->BeginMenu(_("&Edit"));
   c->AddItem("Undo",           _("&Undo\tCtrl+Z"),                  FN(OnUndo));

   // The default shortcut key for Redo is different
   // on different platforms.

   wxString redoLabel = _("&Redo");
   #ifdef __WXMSW__
   redoLabel += "\tCtrl+Y";
   #else
   redoLabel += "\tCtrl+Shift+Z";
   #endif

   c->AddItem("Redo",           redoLabel,                           FN(OnRedo));

   c->AddSeparator();
   c->AddItem("Cut",            _("Cu&t\tCtrl+X"),                   FN(OnCut));
   c->AddItem("Copy",           _("&Copy\tCtrl+C"),                  FN(OnCopy)); 
   c->AddItem("Paste",          _("&Paste\tCtrl+V"),                 FN(OnPaste));
   c->AddItem("Trim",           _("&Trim\tCtrl+T"),                  FN(OnTrim));
   c->AddSeparator();
   c->AddItem("Delete",         _("&Delete\tCtrl+K"),                FN(OnDelete));
   c->AddItem("Silence",        _("&Silence\tCtrl+L"),               FN(OnSilence));
   c->AddSeparator();
   c->AddItem("Split",          _("Spl&it"),                         FN(OnSplit));
   c->AddItem("Duplicate",      _("D&uplicate\tCtrl+D"),             FN(OnDuplicate));
   c->AddItem("SelectAll",      _("Select &All\tCtrl+A"),            FN(OnSelectAll));
   c->AddItem("SelStartCursor", _("Select Start to Cursor"),         FN(OnSelectStartCursor));
   c->AddItem("SelCursorEnd",   _("Select Cursor to End"),           FN(OnSelectCursorEnd));
   c->EndMenu();

   c->BeginMenu(_("&View"));
   c->AddItem("ZoomIn",         _("Zoom &In\tCtrl+1"),               FN(OnZoomIn));
   c->AddItem("ZoomNormal",     _("Zoom &Normal\tCtrl+2"),           FN(OnZoomNormal));
   c->AddItem("ZoomOut",        _("Zoom &Out\tCtrl+3"),              FN(OnZoomOut));
   c->AddItem("FitInWindow",    _("&Fit in Window\tCtrl+F"),         FN(OnZoomFit));
   c->AddItem("ZoomSel",        _("&Zoom to Selection\tCtrl+E"),     FN(OnZoomSel));
   c->AddSeparator();

   c->BeginSubMenu(_("Set Selection Format"));
   c->AddItemList("SelectionFormat", GetSelectionFormats(), FN(OnSelectionFormat));
   c->EndSubMenu();

   c->BeginSubMenu(_("Set Snap-to mode"));

   /* i18n-hint: Set snap-to mode on or off */
   c->AddItem("SnapOn",         _("On"),                             FN(OnSnapOn));
   /* i18n-hint: Set snap-to mode on or off */
   c->AddItem("SnapOff",        _("Off"),                            FN(OnSnapOff));
   c->EndSubMenu();

   c->AddSeparator();
   c->AddItem("UndoHistory",    _("&History..."),               FN(OnHistory));
   c->AddItem("PlotSpectrum",   _("&Plot Spectrum"),                 FN(OnPlotSpectrum));
#ifndef __WXMAC__
   c->AddSeparator();
   c->AddItem("FloatControlTB", _("Float Control Toolbar"),          FN(OnFloatControlToolBar));
   c->AddItem("FloatEditTB",    _("Float Edit Toolbar"),             FN(OnFloatEditToolBar));
   c->AddItem("FloatMixerTB",   _("Float Mixer Toolbar"),            FN(OnFloatMixerToolBar));
#endif
   c->EndMenu();

   c->BeginMenu(_("&Project"));
   c->AddItem("ImportAudio",    _("&Import Audio...\tCtrl+I"),       FN(OnImport));
   c->AddItem("ImportLabels",   _("Import &Labels..."),              FN(OnImportLabels));
   c->AddItem("ImportMIDI",     _("Import &MIDI..."),                FN(OnImportMIDI));
   c->AddItem("ImportRaw",      _("Import &Raw Data..."),            FN(OnImportRaw));
   c->AddSeparator();
   c->AddItem("EditID3",        _("&Edit ID3 Tags..."),              FN(OnEditID3));
   c->AddSeparator();
   c->AddItem("QuickMix",       _("&Quick Mix"),                     FN(OnQuickMix));
   c->AddSeparator();
   c->AddItem("NewAudioTrack",  _("New &Audio Track"),               FN(OnNewWaveTrack));
   c->AddItem("NewStereoTrack", _("New &Stereo Track"),              FN(OnNewStereoTrack));
   c->AddItem("NewLabelTrack",  _("New La&bel Track"),               FN(OnNewLabelTrack));
   c->AddItem("NewTimeTrack",   _("New &Time Track"),                FN(OnNewTimeTrack));
   c->AddSeparator();
   c->AddItem("RemoveTracks",   _("Remo&ve Tracks"),                 FN(OnRemoveTracks));
   c->AddSeparator();
   c->AddItem("SelSave",        _("Cursor or Selection Save"),       FN(OnSelectionSave));
   c->AddItem("SelRestore",     _("Cursor or Selection Restore"),    FN(OnSelectionRestore));
   c->AddSeparator();
   c->AddItem("CursTrackStart", _("Cursor to Track Start"),          FN(OnCursorTrackStart));
   c->AddItem("CursTrackEnd",   _("Cursor to Track End"),            FN(OnCursorTrackEnd));
   c->AddItem("CursSelStart",   _("Cursor to Selection Start"),      FN(OnCursorSelStart));
   c->AddItem("CursSelEnd",     _("Cursor to Selection End"),        FN(OnCursorSelEnd));

   wxArrayString alignLabels;
   alignLabels.Add(_("Align with &Zero"));
   alignLabels.Add(_("Align with &Cursor"));
   alignLabels.Add(_("Align with Selection &Start"));
   alignLabels.Add(_("Align with Selection &End"));
   alignLabels.Add(_("Align End with Cursor"));
   alignLabels.Add(_("Align End with Selection Start"));
   alignLabels.Add(_("Align End with Selection End"));
   alignLabels.Add(_("Align Tracks Together"));

   c->BeginSubMenu(_("Align..."));
   c->AddItemList("Align", alignLabels, FN(OnAlign));
   c->EndSubMenu();

   alignLabels.Remove(7); // Can't align together and move cursor

   c->BeginSubMenu(_("Align and move cursor..."));
   c->AddItemList("Align", alignLabels, FN(OnAlignMoveSel));
   c->EndSubMenu();

   c->AddSeparator();   
   c->AddItem("AddLabel",       _("Add Label At Selection\tCtrl+B"), FN(OnAddLabel));
   c->EndMenu();

   c->BeginMenu(_("&Generate"));
   effects = Effect::GetEffects(INSERT_EFFECT | BUILTIN_EFFECT);
   if(effects->GetCount()){
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList("Generate", names, FN(OnGenerateEffect));
   }
   delete effects;

   effects = Effect::GetEffects(INSERT_EFFECT | PLUGIN_EFFECT);
   if (effects->GetCount()) {
      c->AddSeparator();
      names.Clear();
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList("GeneratePlugin", names, FN(OnGeneratePlugin), true);
   }
   delete effects;
   c->EndMenu();

   c->BeginMenu(_("&Effect"));
   c->AddItem("RepeatLastEffect",     _("Repeat Last Effect\tCtrl+R"),    FN(OnRepeatLastEffect));
   c->AddSeparator();

   effects = Effect::GetEffects(PROCESS_EFFECT | BUILTIN_EFFECT);
   if(effects->GetCount()){
      names.Clear();
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList("Effect", names, FN(OnProcessEffect));
   }
   delete effects;

   effects = Effect::GetEffects(PROCESS_EFFECT | PLUGIN_EFFECT);
   if (effects->GetCount()) {
      c->AddSeparator();
      names.Clear();
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList("EffectPlugin", names, FN(OnProcessPlugin), true);
   }
   delete effects;
   c->EndMenu();

   c->BeginMenu(_("&Analyze"));

   effects = Effect::GetEffects(ANALYZE_EFFECT | BUILTIN_EFFECT);
   if(effects->GetCount()){
      names.Clear();
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList("Analyze", names, FN(OnAnalyzeEffect));
   }
   delete effects;

   effects = Effect::GetEffects(ANALYZE_EFFECT | PLUGIN_EFFECT);
   if (effects->GetCount()) {
      c->AddSeparator();
      names.Clear();
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList("AnalyzePlugin", names, FN(OnAnalyzePlugin), true);
   }
   delete effects;
   c->EndMenu();

   c->BeginMenu(_("&Help"));
   c->AddItem("About",          _("&About Audacity..."),          FN(OnAbout));
   c->AddSeparator();   
   c->AddItem("Help",           _("&Online Help..."),             FN(OnHelp));
   c->AddSeparator();   
   c->AddItem("Benchmark",      _("&Run Benchmark..."),           FN(OnBenchmark));
 
   c->EndMenu();

   ModifyExportMenus();

   SetMenuBar(menubar);

   c->AddCommand("Play/Stop",   _("Play/Stop\tSpacebar"),         FN(OnPlayStop));
   c->AddCommand("Stop",        _("Stop\tS"),                     FN(OnStop));
   c->AddCommand("Pause",       _("Pause\tP"),                    FN(OnPause));
   c->AddCommand("Record",      _("Record\tR"),                   FN(OnRecord));
   
   c->AddCommand("PlayOneSec",     _("Play One Second\t1"),       FN(OnPlayOneSecond));
   c->AddCommand("PlayToSelection",_("Play One Second\tB"),       FN(OnPlayToSelection));
   c->AddCommand("PlayLooped",     _("Play Looped\tL"),           FN(OnPlayLooped));

   c->AddCommand("SkipStart",   _("Skip to Start\tHome"),         FN(OnSkipStart));
   c->AddCommand("SkipEnd",     _("Skip to End\tEnd"),            FN(OnSkipEnd));

   c->AddCommand("SelStart",    _("Selection to Start\tShift+Home"), FN(OnSelToStart));
   c->AddCommand("SelEnd",      _("Selection to End\tShift+End"),    FN(OnSelToEnd));

   c->AddCommand("DeleteKey",      _("DeleteKey\tBackspace"),           FN(OnDelete));

   c->AddCommand("CursorLeft",  _("Cursor Left\tLeft"),           FN(OnCursorLeft));
   c->AddCommand("CursorRight", _("Cursor Right\tRight"),         FN(OnCursorRight));
   c->AddCommand("SelExtLeft",  _("Selection Extend Left\tShift+Left"),     FN(OnSelExtendLeft));
   c->AddCommand("SelExtRight", _("Selection Extend Right\tShift+Right"),   FN(OnSelExtendRight));
   c->AddCommand("SelCntrLeft", _("Selection Contract Left\tCtrl+Shift+Right"),   FN(OnSelContractLeft));
   c->AddCommand("SelCntrRight",_("Selection Contract Right\tCtrl+Shift+Left"), FN(OnSelContractRight));
   c->AddCommand("ZeroCross",   _("Selection to Nearest Zero Crossings\tZ"), FN(OnZeroCrossing));

   mSel0save = 0;
   mSel1save = 0;
}

void AudacityProject::ModifyExportMenus()
{
   int format = ReadExportFormatPref();
   wxString pcmFormat = sf_header_shortname(format & SF_FORMAT_TYPEMASK);
   wxString lossyFormat = gPrefs->Read("/FileFormats/LossyExportFormat", "MP3");

   mCommandManager.Modify("Export",
                          wxString::Format(_("&Export as %s..."),
                                           (const char *)pcmFormat));
   mCommandManager.Modify("ExportSel",
                          wxString::Format(_("&Export Selection as %s..."),
                                           (const char *)pcmFormat));
   mCommandManager.Modify("ExportLossy",
                          wxString::Format(_("&Export as %s..."),
                                           (const char *)lossyFormat));
   mCommandManager.Modify("ExportLossySel",
                          wxString::Format(_("&Export Selection as %s..."),
                                           (const char *)lossyFormat));
}

void AudacityProject::ModifyUndoMenus()
{
   wxString desc;
   int cur = mUndoManager.GetCurrentState();

   if (mUndoManager.UndoAvailable()) {
      mUndoManager.GetShortDescription(cur, &desc);
      mCommandManager.Modify("Undo",
                             wxString::Format(_("&Undo %s"),
                                              (const char *)desc));
      mCommandManager.Enable("Undo", true);
   }
   else {
      mCommandManager.Modify("Undo", 
                             wxString::Format(_("Can't Undo")));
      mCommandManager.Enable("Undo", false);
   }

   if (mUndoManager.RedoAvailable()) {
      mUndoManager.GetShortDescription(cur+1, &desc);
      mCommandManager.Modify("Redo",
                             wxString::Format(_("&Redo %s"),
                                              (const char *)desc));
      mCommandManager.Enable("Redo", true);
   }
   else {
      mCommandManager.Modify("Redo",
                             wxString::Format(_("Can't Redo")));
      mCommandManager.Enable("Redo", false);
   }
}

void AudacityProject::RebuildMenuBar()
{
   wxMenuBar *menuBar = GetMenuBar();
   DetachMenuBar();
   delete menuBar;
   mCommandManager.PurgeData();
   CreateMenusAndCommands();
}

void AudacityProject::UpdateMenus()
{
   if (!gControlToolBarStub)
      return;

   mCommandManager.Enable("Save", mUndoManager.UnsavedChanges());

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

   mCommandManager.Enable("Paste", numTracksSelected > 0 && msClipLen > 0.0);

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
   if (gMixerToolBarStub) {
      if (gMixerToolBarStub->GetLoadedStatus()) {
         if(gMixerToolBarStub->GetWindowedStatus())
            toolBarCheckSum += 12;
         else
            toolBarCheckSum += 24;
      }
   }
   
   // Get ahold of the clipboard status
   bool clipboardStatus = static_cast<bool>(GetActiveProject()->Clipboard());

   bool hasLastEffect = (Effect::GetLastEffect() != NULL);

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
       mLastClipboardState == clipboardStatus &&
       mLastHasLastEffect == hasLastEffect) 
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
   mLastHasLastEffect = hasLastEffect;

   bool anySelection = numTracksSelected > 0 && nonZeroRegionSelected;

   mCommandManager.Enable("Export", numTracks > 0);
   mCommandManager.Enable("ExportSel", anySelection);
   mCommandManager.Enable("ExportLossy", numTracks > 0);
   mCommandManager.Enable("ExportLossySel", anySelection);
   mCommandManager.Enable("ExportLabels", numLabelTracks > 0);

   mCommandManager.Enable("Cut", anySelection);
   mCommandManager.Enable("Copy", anySelection);
   mCommandManager.Enable("Trim", anySelection);
   mCommandManager.Enable("Delete", anySelection);
   mCommandManager.Enable("Silence", anySelection);
   mCommandManager.Enable("Split", anySelection);
   //mCommandManager.Enable("SplitLabels", numLabelTracksSelected == 1 && numWaveTracksSelected == 1);
   mCommandManager.Enable("Duplicate", anySelection);
   mCommandManager.Enable("SelectAll", numTracks > 0);
   mCommandManager.Enable("SelStartCursor", numWaveTracksSelected > 0 && !nonZeroRegionSelected);
   mCommandManager.Enable("SelCursorEnd", numWaveTracksSelected > 0 && !nonZeroRegionSelected);

   mCommandManager.Enable("Undo", mUndoManager.UndoAvailable());
   mCommandManager.Enable("Redo", mUndoManager.RedoAvailable());

   mCommandManager.Enable("PlotSpectrum", numWaveTracksSelected > 0
                     && nonZeroRegionSelected);

#ifndef __WXMAC__
   //Modify toolbar-specific Menus

   if (gEditToolBarStub) {

     // Loaded or unloaded?
     mCommandManager.Enable("FloatEditTB", gEditToolBarStub->GetLoadedStatus());

     // Floating or docked?
     if (gEditToolBarStub->GetWindowedStatus())
        mCommandManager.Modify("FloatEditTB", _("Dock Edit Toolbar"));
     else
        mCommandManager.Modify("FloatEditTB", _("Float Edit Toolbar"));
   }
   else {
      mCommandManager.Enable("FloatEditTB", false);
   }   

   if (gMixerToolBarStub) {
     
     // Loaded or unloaded?
     mCommandManager.Enable("FloatMixerTB", gMixerToolBarStub->GetLoadedStatus());
     
     // Floating or docked?
     if (gMixerToolBarStub->GetWindowedStatus())
        mCommandManager.Modify("FloatMixerTB", _("Dock Mixer Toolbar"));
     else
        mCommandManager.Modify("FloatMixerTB", _("Float Mixer Toolbar"));
   }
   else {
      mCommandManager.Enable("FloatMixerTB", false);
   }

#endif

   mCommandManager.Enable("QuickMix", numWaveTracksSelected > 0);
   mCommandManager.Enable("SelSave", numWaveTracksSelected > 0);
   mCommandManager.Enable("SelRestore", numWaveTracksSelected > 0);
   mCommandManager.Enable("CursTrackStart", numWaveTracksSelected > 0);
   mCommandManager.Enable("CursTrackEnd", numWaveTracksSelected > 0);
   mCommandManager.Enable("CursSelStart", numWaveTracksSelected > 0 && nonZeroRegionSelected);
   mCommandManager.Enable("CursSelEnd", numWaveTracksSelected > 0 && nonZeroRegionSelected);
   mCommandManager.Enable("Align", numWaveTracksSelected > 0);
   mCommandManager.Enable("RemoveTracks", numTracksSelected > 0);

   // Effects menus

   mCommandManager.Enable("RepeatLastEffect", numWaveTracksSelected > 0 &&
                          nonZeroRegionSelected &&
                          hasLastEffect);
   mCommandManager.Enable("Effect", numWaveTracksSelected > 0 && nonZeroRegionSelected);
   mCommandManager.Enable("EffectPlugin", numWaveTracksSelected > 0 && nonZeroRegionSelected);
   mCommandManager.Enable("Analyze", numWaveTracksSelected > 0 && nonZeroRegionSelected);
   mCommandManager.Enable("AnalyzePlugin", numWaveTracksSelected > 0 && nonZeroRegionSelected);

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
// Audio I/O Commands
//

// TODO: Should all these functions which involve
// the toolbar actually move into ControlToolBar?

/// MakeReadyToPlay stops whatever is currently playing 
/// and pops the play button up.  Then, if nothing is now
/// playing, it pushes the play button down and enables
/// the stop button.
bool AudacityProject::MakeReadyToPlay()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   // If this project is playing, stop playing
   if (gAudioIO->IsStreamActive(GetAudioIOToken())) {
      toolbar->SetPlay(false);        //Pops
      toolbar->SetStop(true);         //Pushes stop down
      toolbar->OnStop(evt);

      ::wxUsleep(100);
   }

   // If it didn't stop playing quickly, or if some other
   // project is playing, return
   if (gAudioIO->IsBusy())
      return false;

   toolbar->SetPlay(true);
   toolbar->SetStop(false);

   return true;
}

void AudacityProject::OnPlayOneSecond()
{
   if( !MakeReadyToPlay() )
      return;

   ControlToolBar *toolbar = GetControlToolBar();
   double pos = mTrackPanel->GetMostRecentXPos();
   mLastPlayMode = oneSecondPlay;
   toolbar->PlayPlayRegion(pos - 0.5, pos + 0.5);
}


/// The idea for this function (and first implementation)
/// was from Juhana Sadeharju.  The function plays the 
/// sound between the current mouse position and the 
/// nearest selection boundary.  This gives four possible 
/// play regions depending on where the current mouse 
/// position is relative to the left and right boundaries 
/// of the selection region.
void AudacityProject::OnPlayToSelection()
{
   if( !MakeReadyToPlay() )
      return;

   ControlToolBar *toolbar = GetControlToolBar();
   double pos = mTrackPanel->GetMostRecentXPos();

   double t0,t1;
   // check region between pointer and the nearest selection edge
   if (fabs(pos - mViewInfo.sel0) < fabs(pos - mViewInfo.sel1)) {
      t0 = t1 = mViewInfo.sel0;
   } else {
      t0 = t1 = mViewInfo.sel1;
   }
   if( pos < t1) 
      t0=pos;
   else
      t1=pos;

   // JKC: oneSecondPlay mode disables auto scrolling
   // On balance I think we should always do this in this function
   // since you are typically interested in the sound EXACTLY 
   // where the cursor is.
   // TODO: have 'playing attributes' such as 'with_autoscroll'
   // rather than modes, since that's how we're now using the modes.
   mLastPlayMode = oneSecondPlay;

   // An alternative, commented out below, is to disable autoscroll
   // only when playing a short region, less than or equal to a second.
//   mLastPlayMode = ((t1-t0) > 1.0) ? normalPlay : oneSecondPlay;

   toolbar->PlayPlayRegion(t0, t1);
}

void AudacityProject::OnPlayLooped()
{
   if( !MakeReadyToPlay() )
      return;

   // Now play in a loop
   // Will automatically set mLastPlayMode
   ControlToolBar *toolbar = GetControlToolBar();
   toolbar->PlayCurrentRegion(true);
}

void AudacityProject::OnPlayStop()
{
   ControlToolBar *toolbar = GetControlToolBar();

   //If busy, stop playing, make sure everything is unpaused.
   if (gAudioIO->IsStreamActive(GetAudioIOToken())) {
      toolbar->SetPlay(false);        //Pops
      toolbar->SetStop(true);         //Pushes stop down
      toolbar->StopPlaying();
   }
   else if (!gAudioIO->IsBusy()) {
      //Otherwise, start playing (assuming audio I/O isn't busy)
      toolbar->SetPlay(true);
      toolbar->SetStop(false);

      // Will automatically set mLastPlayMode
      toolbar->PlayCurrentRegion(false);
   }
}

void AudacityProject::OnStop()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   if (gAudioIO->IsStreamActive())
      toolbar->OnStop(evt);
}

void AudacityProject::OnPause()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   toolbar->OnPause(evt);
}

void AudacityProject::OnRecord()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   toolbar->OnRecord(evt);
}

void AudacityProject::OnSkipStart()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   toolbar->OnRewind(evt);
}

void AudacityProject::OnSkipEnd()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   toolbar->OnFF(evt);
}

void AudacityProject::OnSelToStart()
{
   mViewInfo.sel0 = 0;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelToEnd()
{
   SkipEnd(true);
}

void AudacityProject::OnCursorLeft()
{
   if (mViewInfo.sel0 == mViewInfo.sel1) {
      mViewInfo.sel0 -= 1/mViewInfo.zoom;
      mViewInfo.sel1 -= 1/mViewInfo.zoom;
      if (mViewInfo.sel0 < 0.0)
         mViewInfo.sel0 = 0.0;
      if (mViewInfo.sel1 < 0.0)
         mViewInfo.sel1 = 0.0;
   }
   else
      mViewInfo.sel1 = mViewInfo.sel0;

   mTrackPanel->Refresh(false);   
}

void AudacityProject::OnCursorRight()
{
   if (mViewInfo.sel0 == mViewInfo.sel1) {
      mViewInfo.sel0 += 1/mViewInfo.zoom;
      mViewInfo.sel1 += 1/mViewInfo.zoom;
   }
   else
      mViewInfo.sel0 = mViewInfo.sel1;

   mTrackPanel->Refresh(false);   
}

void AudacityProject::OnSelExtendLeft()
{
   mViewInfo.sel0 -= 1/mViewInfo.zoom;
   if (mViewInfo.sel0 < 0.0)
      mViewInfo.sel0 = 0.0;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelExtendRight()
{
   mViewInfo.sel1 += 1/mViewInfo.zoom;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelContractLeft()
{
   mViewInfo.sel0 += 1/mViewInfo.zoom;
   if (mViewInfo.sel0 > mViewInfo.sel1)
      mViewInfo.sel0 = mViewInfo.sel1;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelContractRight()
{
   mViewInfo.sel1 -= 1/mViewInfo.zoom;
   if (mViewInfo.sel1 < mViewInfo.sel0)
      mViewInfo.sel1 = mViewInfo.sel0;
   mTrackPanel->Refresh(false);
}

double AudacityProject::NearestZeroCrossing(double t0)
{
   int windowSize = (int)(GetRate() / 100);
   float *dist = new float[windowSize];
   int i, j;

   for(i=0; i<windowSize; i++)
      dist[i] = 0.0;

   TrackListIterator iter(mTracks);
   Track *track = iter.First();
   while (track) {
      if (!track->GetSelected() || track->GetKind() != (Track::Wave)) {
         track = iter.Next();
         continue;
      }
      WaveTrack *one = (WaveTrack *)track;
      int oneWindowSize = (int)(one->GetRate() / 100);
      float *oneDist = new float[oneWindowSize];
      longSampleCount s = one->TimeToLongSamples(t0);
      one->Get((samplePtr)oneDist, floatSample,
               s - oneWindowSize/2, oneWindowSize);

      // Start by penalizing downward motion.  We prefer upward
      // zero crossings.
      if (oneDist[1] - oneDist[0] < 0)
         oneDist[0] = oneDist[0]*6 + 0.3;
      for(i=1; i<oneWindowSize; i++)
         if (oneDist[i] - oneDist[i-1] < 0)
            oneDist[i] = oneDist[i]*6 + 0.3;

      for(i=0; i<oneWindowSize; i++)
         oneDist[i] = fabs(oneDist[i]);

      for(i=0; i<windowSize; i++) {
         if (windowSize != oneWindowSize)
            j = i * (oneWindowSize-1) / (windowSize-1);
         else
            j = i;

         dist[i] += oneDist[j];
      }
         
      track = iter.Next();
   }

   int argmin = windowSize/2; // Start at default pos in center
   float min = dist[argmin];
   for(i=0; i<windowSize; i++) {
      if (dist[i] < min) {
         argmin = i;
         min = dist[i];
      }
   }

   return t0 + (argmin - windowSize/2)/GetRate();
}

void AudacityProject::OnZeroCrossing()
{
   if (mViewInfo.sel0 == mViewInfo.sel1)
      mViewInfo.sel0 = mViewInfo.sel1 =
         NearestZeroCrossing(mViewInfo.sel0);
   else {
      mViewInfo.sel0 = NearestZeroCrossing(mViewInfo.sel0);
      mViewInfo.sel1 = NearestZeroCrossing(mViewInfo.sel1);

      if (mViewInfo.sel1 < mViewInfo.sel0)
         mViewInfo.sel1 = mViewInfo.sel0;
   }

   mTrackPanel->Refresh(false);
}

//
// Effect Menus
//

void AudacityProject::OnEffect(int type, int index)
{
   EffectArray *effects;
   Effect *f = NULL;

   effects = Effect::GetEffects(type);
   f = (*effects)[index];
   delete effects;

   if (!f)
      return;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   double prevEndTime = mTracks->GetEndTime();
   int count = 0;
   
   while (t) {
      if (t->GetSelected() && t->GetKind() == (Track::Wave))
         count++;
      t = iter.Next();
   }
   
   if (count == 0) {
      // No tracks were selected...
      if (f->GetEffectFlags() & INSERT_EFFECT) {
         // Create a new track for the generated audio...
         WaveTrack *newTrack = mTrackFactory->NewWaveTrack();
         mTracks->Add(newTrack);
         newTrack->SetSelected(true);
      }
      else {
         wxMessageBox(_("You must select a track first."));
         return;
      }
   }
   
   if (f->DoEffect(this, type, mTracks, mTrackFactory,
                   &mViewInfo.sel0, &mViewInfo.sel1)) {
      wxString longDesc = f->GetEffectDescription();
      wxString shortDesc = f->GetEffectName();

      if (shortDesc.Length() > 3 && shortDesc.Right(3)=="...")
         shortDesc = shortDesc.Left(shortDesc.Length()-3);

      PushState(longDesc, shortDesc);
      if (mTracks->GetEndTime() > prevEndTime)
         OnZoomFit();
      FixScrollbars();

      // Only remember a successful effect, don't rmemeber insert,
      // or analyze effects.
      if ((f->GetEffectFlags() & (INSERT_EFFECT | ANALYZE_EFFECT))==0) {
         Effect::SetLastEffect( type, index, f );
         mCommandManager.Modify("RepeatLastEffect",
            wxString::Format(_("Repeat %s\tCtrl+R"),
            (const char *)shortDesc));
      }
      
      mTrackPanel->Refresh(false);
   } else {
      // TODO: undo the effect if necessary?
   }

   
   return;
}

void AudacityProject::OnGenerateEffect(int index)
{
   OnEffect(BUILTIN_EFFECT | INSERT_EFFECT, index);
}

void AudacityProject::OnGeneratePlugin(int index)
{
   OnEffect(PLUGIN_EFFECT | INSERT_EFFECT, index);
}

void AudacityProject::OnRepeatLastEffect(int index)
{
   if( Effect::GetLastEffect()  != NULL )
   {
      // Setting the CONFIGURED_EFFECT bit prevents
      // prompting for parameters.
      OnEffect( 
         Effect::GetLastEffectType() | CONFIGURED_EFFECT,
         Effect::GetLastEffectIndex());
   }
}

void AudacityProject::OnProcessEffect(int index)
{
   OnEffect(BUILTIN_EFFECT | PROCESS_EFFECT, index);
}

void AudacityProject::OnProcessPlugin(int index)
{
   OnEffect(PLUGIN_EFFECT | PROCESS_EFFECT, index);
}

void AudacityProject::OnAnalyzeEffect(int index)
{
   OnEffect(BUILTIN_EFFECT | ANALYZE_EFFECT, index);
}

void AudacityProject::OnAnalyzePlugin(int index)
{
   OnEffect(PLUGIN_EFFECT | ANALYZE_EFFECT, index);
}

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

void AudacityProject::OnUndo()
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

   ModifyUndoMenus();   
}

void AudacityProject::OnRedo()
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

   ModifyUndoMenus();
}

void AudacityProject::OnHistory()
{
   if (mHistoryWindow)
      mHistoryWindow->Show(true);
   else {
      mHistoryWindow = new HistoryWindow(this, &mUndoManager);
      mHistoryWindow->Show(true);
   }
}


void AudacityProject::OnCut()
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

   PushState(_("Cut to the clipboard"), _("Cut"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}



void AudacityProject::OnCopy()
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

void AudacityProject::OnPaste()
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

   PushState(_("Pasted from the clipboard"), _("Paste"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnPasteOver()
{
   if(msClipLen>0.0)
   {
      mViewInfo.sel1=mViewInfo.sel0+msClipLen;
      OnPaste();
   }

   return;
}

void AudacityProject::OnTrim()
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
   PushState(_("Trim file to selection"), _("Trim"));
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
                    mViewInfo.sel1 - mViewInfo.sel0, mViewInfo.sel0),
             _("Silence"));

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

   PushState(_("Duplicated"), _("Duplicate"));

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

   PushState(_("Split"), _("Split"));

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

   PushState(_("Split at labels"), _("Split at labels"));

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
   
   // MM: Commented this out because it was confusing users
   /*
   // make sure that the *right-hand* end of the selection is
   // no further *left* than 1/3 of the way across the screen
   if (mViewInfo.sel1 < newh + mViewInfo.screen / 3)
      newh = mViewInfo.sel1 - mViewInfo.screen / 3;

   // make sure that the *left-hand* end of the selection is
   // no further *right* than 2/3 of the way across the screen
   if (mViewInfo.sel0 > newh + mViewInfo.screen * 2 / 3)
      newh = mViewInfo.sel0 - mViewInfo.screen * 2 / 3;
      */

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

static double OldZooms[2]={ 44100.0/512.0, 4410.0/512.0 };
void AudacityProject::OnZoomToggle()
{
   double origLeft = mViewInfo.h;
   double origWidth = mViewInfo.screen;

   float f;
   // look at percentage difference.  We add a small fudge factor
   // to avoid testing for zero divisor.
   f = mViewInfo.zoom / (OldZooms[0] + 0.0001f);
   // If old zoom is more than 10 percent different, use it.
   if( (0.90f > f) || (f >1.10) ){
      OldZooms[1]=OldZooms[0];
      OldZooms[0]=mViewInfo.zoom;
   }
   Zoom( OldZooms[1] );
   double newh = origLeft + (origWidth - mViewInfo.screen) / 2;
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

void AudacityProject::OnSelectionFormat(int index)
{
   mSelectionFormat = index;
   TP_DisplaySelection();
}

void AudacityProject::OnSnapOn()
{
   mSnapTo = 1;
   TP_DisplaySelection();
}

void AudacityProject::OnSnapOff()
{
   mSnapTo = 0;
   TP_DisplaySelection();
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


void AudacityProject::OnLoadMixerToolBar()
{
   if (gMixerToolBarStub) {
      if (gMixerToolBarStub->GetLoadedStatus()) {

         //the toolbar is "loaded", meaning its visible either in the window
         //or floating

         gMixerToolBarStub->SetLoadedStatus(false);
         gMixerToolBarStub->HideWindowedToolBar();
         gMixerToolBarStub->UnloadAll();

      } else {

         //the toolbar is not "loaded", meaning that although the stub exists, 
         //the toolbar is not visible either in a window or floating around
         gMixerToolBarStub->SetLoadedStatus(true);

         if (gMixerToolBarStub->GetWindowedStatus()) {
            //Make the floating toolbar appear
            gMixerToolBarStub->ShowWindowedToolBar();
            gMixerToolBarStub->LoadAll();
         } else {
            //Make it appear in all the windows
            gMixerToolBarStub->LoadAll();
         }

      }
   } else {
      gMixerToolBarStub = new ToolBarStub(gParentWindow, MixerToolBarID);
      gMixerToolBarStub->LoadAll();
   }
}


void AudacityProject::OnFloatMixerToolBar()
{
   if (gMixerToolBarStub) {

      if (gMixerToolBarStub->GetWindowedStatus()) {

         gMixerToolBarStub->HideWindowedToolBar();
         gMixerToolBarStub->LoadAll();

      } else {

         gMixerToolBarStub->ShowWindowedToolBar();
         gMixerToolBarStub->UnloadAll();
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
   
   wxFileDialog dlog(this, _("Select one or more audio files..."),
                     path, "",
                     _("All files (*.*)|*.*|"
                       "WAV files (*.wav)|*.wav|"
                       "AIFF files (*.aif)|*.aif|"
                       "AU files (*.au)|*.au|"
                       "MP3 files (*.mp3)|*.mp3|"
                       "Ogg Vorbis files (*.ogg)|*.ogg|"
                       "List of Files (*.lof)|*.lof"),
                     wxOPEN | wxMULTIPLE);

   int result = dlog.ShowModal();

   if (result != wxID_OK)
      return;

   wxArrayString selectedFiles;
   unsigned int ff;

   dlog.GetPaths(selectedFiles);

   for(ff=0; ff<selectedFiles.GetCount(); ff++) {
      wxString fileName = selectedFiles[ff];

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

      LabelTrack *newTrack = new LabelTrack(mDirManager);

      newTrack->Import(f);

      SelectNone();
      mTracks->Add(newTrack);
      newTrack->SetSelected(true);

      PushState(wxString::
                Format(_("Imported labels from '%s'"), fileName.c_str()),
                _("Import Labels"));

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

      NoteTrack *newTrack = new NoteTrack(mDirManager);

      if (::ImportMIDI(fileName, newTrack)) {

         SelectNone();
         mTracks->Add(newTrack);
         newTrack->SetSelected(true);

         PushState(wxString::Format(_("Imported MIDI from '%s'"),
                                    fileName.c_str()),
                   _("Import MIDI"));

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
                      _("All files (*)|*"),
                      0,        // Flags
                      this);    // Parent

   if (fileName == "")
      return;

   path =::wxPathOnly(fileName);
   gPrefs->Write("/DefaultOpenPath", path);
   
   Track **newTracks;
   int numTracks;
   
   wxStartTimer();

   wxASSERT(!mImportProgressDialog);

   mImportingRaw = true;

   numTracks = ::ImportRaw(this, fileName, mTrackFactory, &newTracks,
                           AudacityProject::ImportProgressCallback,
                           this);

   if(mImportProgressDialog) {
      delete mImportProgressDialog;
      mImportProgressDialog = NULL;
   }

   mImportingRaw = false;
   
   if (numTracks <= 0)
      return;

   AddImportedTracks(fileName, newTracks, numTracks);
}

void AudacityProject::OnEditID3()
{
   mTags->ShowEditDialog(this, _("Edit ID3 Tags (for MP3 exporting)"));
}

void AudacityProject::OnQuickMix()
{
   WaveTrack *newLeft = NULL;
   WaveTrack *newRight = NULL;

   if (::QuickMix(mTracks, mTrackFactory, mRate, mDefaultFormat, 0.0, 0.0,
                  &newLeft, &newRight)) {

      // Remove originals

      TrackListIterator iter(mTracks);
      Track *t = iter.First();

      while (t) {
         if (t->GetSelected())
            t = iter.RemoveCurrent();
         else
            t = iter.Next();
      }

      // Add new tracks

      mTracks->Add(newLeft);
      if (newRight)
         mTracks->Add(newRight);

      PushState(_("Quick mix"), _("Quick mix"));

      FixScrollbars();
      mTrackPanel->Refresh(false);
   }
}

void AudacityProject::OnSelectionSave()
{
   mSel0save = mViewInfo.sel0;
   mSel1save = mViewInfo.sel1;
}

void AudacityProject::OnSelectionRestore()
{
   mViewInfo.sel0 = mSel0save;
   mViewInfo.sel1 = mSel1save;
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

void AudacityProject::HandleAlign(int index, bool moveSel)
{
   TrackListIterator iter(mTracks);
   wxString action;
   double offset;
   double minOffset = 1000000000.0;
   double maxEndOffset = 0.0;
   double avgOffset = 0.0;
   int numSelected = 0;
   Track *t = iter.First();
   double delta = 0.0;
   double newPos = -1.0;

   while (t) {
      if (t->GetSelected()) {
         numSelected++;

         offset = t->GetOffset();
         avgOffset += offset;
         if (offset < minOffset)
            minOffset = offset;
         if (t->GetEndTime() > maxEndOffset)
            maxEndOffset = t->GetEndTime();
      }
      t = iter.Next();
   }

   avgOffset /= numSelected;

   switch(index) {
   case kAlignZero:
      delta = -minOffset;
      action = _("Aligned with zero");
      break;
   case kAlignCursor:
      delta = mViewInfo.sel0 - minOffset;
      action = _("Aligned cursor");
      break;
   case kAlignSelStart:
      delta = mViewInfo.sel0 - minOffset;
      action = _("Aligned with selection start");
      break;
   case kAlignSelEnd:
      delta = mViewInfo.sel1 - minOffset;
      action = _("Aligned with selection end");
      break;
   case kAlignEndCursor:
      delta = mViewInfo.sel0 - maxEndOffset;
      action = _("Aligned end with cursor");
      break;
   case kAlignEndSelStart:
      delta = mViewInfo.sel0 - maxEndOffset;
      action = _("Aligned end with selection start");
      break;
   case kAlignEndSelEnd:
      delta = mViewInfo.sel1 - maxEndOffset;
      action = _("Aligned end with selection end");
      break;
   case kAlign:
      newPos = avgOffset;
      action = _("Aligned");
      break;
   }

   if (newPos >= 0.0) {
      TrackListIterator iter(mTracks);
      Track *t = iter.First();
      
      while (t) {
         if (t->GetSelected()) {
            t->SetOffset(newPos);
         }
         t = iter.Next();
      }
   }

   if (delta != 0.0) {
      TrackListIterator iter(mTracks);
      Track *t = iter.First();
      
      while (t) {
         if (t->GetSelected()) {
            t->SetOffset(t->GetOffset() + delta);
         }
         t = iter.Next();
      }
   }

   if (moveSel) {
      mViewInfo.sel0 += delta;
      mViewInfo.sel1 += delta;
   }

   PushState(action, _("Align"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlign(int index)
{
   HandleAlign(index, false);
}

void AudacityProject::OnAlignMoveSel(int index)
{
   HandleAlign(index, true);
}

void AudacityProject::OnNewWaveTrack()
{
   WaveTrack *t = mTrackFactory->NewWaveTrack(mDefaultFormat);
   t->SetRate(mRate);
   SelectNone();

   mTracks->Add(t);
   t->SetSelected(true);

   PushState(_("Created new audio track"), _("New Track"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnNewStereoTrack()
{
   WaveTrack *t = mTrackFactory->NewWaveTrack(mDefaultFormat);
   t->SetRate(mRate);
   SelectNone();
   
   mTracks->Add(t);
   t->SetSelected(true);
   t->SetLinked (true);
   
   t = mTrackFactory->NewWaveTrack(mDefaultFormat);
   t->SetRate(mRate);
   
   mTracks->Add(t);
   t->SetSelected (true);
   
   PushState(_("Created new stereo audio track"), _("New Track"));
   
   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnNewLabelTrack()
{
   LabelTrack *t = new LabelTrack(mDirManager);

   SelectNone();

   mTracks->Add(t);
   t->SetSelected(true);

   PushState(_("Created new label track"), _("New Track"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnNewTimeTrack()
{
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   bool alreadyHaveTimeTrack = false;
   
   while (t)
      {
         if (t->GetKind() == Track::Time)
            {
               alreadyHaveTimeTrack = true;
               break;
            }
         t = iter.Next();
      }
   
   if( alreadyHaveTimeTrack )
      {
         wxString msg;
         msg.Printf(_("The version of Audacity you are using does not support multiple time tracks."));
         wxMessageBox(msg);
      }
   else
      {
         TimeTrack *t = new TimeTrack(mDirManager);

         #if USE_LIBSAMPLERATE
         const char *str = NULL;
         for( int i = 0 ; (str = src_get_name (i)) != NULL; i++ )
            t->addConverter( i, str );
         #else
         t->addConverter(0, "No Sample Rate Conversion Available");
         #endif
         
         SelectNone();
         mTracks->AddToHead(t);
         t->SetSelected(true);
         
         PushState(_("Created new time track"), _("New Track"));

         /*
         TrackListIterator iter(mTracks);
         for( Track *tr = iter.First(); (tr); tr = iter.Next() )
            tr->SetTimeTrack( t );
         */
         
         FixScrollbars();
         mTrackPanel->Refresh(false);
      }
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
      lt = new LabelTrack(mDirManager);

      SelectNone();

      mTracks->Add(lt);
   }

   lt->Add(mViewInfo.sel0, "");

   PushState(_("Added label"), _("Label"));

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

   PushState(_("Removed audio track(s)"), _("Remove Track"));

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

