/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandsCallback.h

  Dominic Mazzoni
  Brian Gunlogson
  et. al.

  This file implements all of the methods that get called when you
  select an item from a menu.

**********************************************************************/

/*
  HOW TO ADD A CALLBACK

  1) Put the function body in commands/CommandsCallback.cpp
  2) Put the function declaration in this file
  3) Update the lists in this file
*/

#ifdef AUDACITY_COMMANDS_CALLBACK_POINTERS

#define NUM_CALLBACK_FUNCTIONS 88

audEventFunction callback_function_pointers[] = {&AudacityProject::OnNew,&AudacityProject::OnOpen,&AudacityProject::OnClose,
&AudacityProject::OnSave,&AudacityProject::OnSaveAs,&AudacityProject::OnExportMix,&AudacityProject::OnExportSelection,
&AudacityProject::OnExportLossyMix,&AudacityProject::OnExportLossySelection,&AudacityProject::OnExportLabels,
&AudacityProject::OnPreferences,&AudacityProject::OnExit,&AudacityProject::Undo,&AudacityProject::Redo,
&AudacityProject::UndoHistory,&AudacityProject::Cut,&AudacityProject::Copy,&AudacityProject::Paste,
&AudacityProject::PasteOver,&AudacityProject::Trim,&AudacityProject::OnDelete,&AudacityProject::OnSilence,
&AudacityProject::OnSplit,&AudacityProject::OnSplitLabels,&AudacityProject::OnDuplicate,&AudacityProject::OnSelectAll,
&AudacityProject::OnSelectCursorEnd,&AudacityProject::OnSelectStartCursor,&AudacityProject::OnZoomIn,
&AudacityProject::OnZoomOut,&AudacityProject::OnZoomNormal,&AudacityProject::OnZoomFit,&AudacityProject::OnZoomSel,
&AudacityProject::OnPlotSpectrum,&AudacityProject::OnFloatControlToolBar,&AudacityProject::OnLoadEditToolBar,
&AudacityProject::OnFloatEditToolBar,&AudacityProject::OnImport,&AudacityProject::OnImportLabels,
&AudacityProject::OnImportMIDI,&AudacityProject::OnImportRaw,&AudacityProject::OnEditID3,&AudacityProject::OnQuickMix,
&AudacityProject::OnSelectionSave,&AudacityProject::OnSelectionRestore,&AudacityProject::OnCursorTrackStart,
&AudacityProject::OnCursorTrackEnd,&AudacityProject::OnCursorSelStart,&AudacityProject::OnCursorSelEnd,
&AudacityProject::OnAlignZero,&AudacityProject::OnAlignGroupZero,&AudacityProject::OnAlign,&AudacityProject::OnAlignCursor,
&AudacityProject::OnAlignSelStart,&AudacityProject::OnAlignSelEnd,&AudacityProject::OnAlignEndCursor,
&AudacityProject::OnAlignEndSelStart,&AudacityProject::OnAlignEndSelEnd,&AudacityProject::OnAlignGroupCursor,
&AudacityProject::OnAlignGroupSelStart,&AudacityProject::OnAlignGroupSelEnd,&AudacityProject::OnAlignGroupEndCursor,
&AudacityProject::OnAlignGroupEndSelStart,&AudacityProject::OnAlignGroupEndSelEnd,
&AudacityProject::OnAlignZeroMoveSel,&AudacityProject::OnAlignGroupZeroMoveSel,
&AudacityProject::OnAlignCursorMoveSel,&AudacityProject::OnAlignSelStartMoveSel,&AudacityProject::OnAlignSelEndMoveSel,
&AudacityProject::OnAlignEndCursorMoveSel,&AudacityProject::OnAlignEndSelStartMoveSel,&AudacityProject::OnAlignEndSelEndMoveSel,
&AudacityProject::OnAlignGroupCursorMoveSel,&AudacityProject::OnAlignGroupSelStartMoveSel,&AudacityProject::OnAlignGroupSelEndMoveSel,
&AudacityProject::OnAlignGroupEndCursorMoveSel,&AudacityProject::OnAlignGroupEndSelStartMoveSel,&AudacityProject::OnAlignGroupEndSelEndMoveSel,
&AudacityProject::OnNewWaveTrack,&AudacityProject::OnNewLabelTrack,&AudacityProject::OnRemoveTracks,
&AudacityProject::OnAbout,&AudacityProject::OnHelp,&AudacityProject::OnHelpIndex,&AudacityProject::OnHelpSearch,
&AudacityProject::OnBenchmark,&AudacityProject::OnSeparator,&AudacityProject::OnAddLabel};

const char *callback_function_strings[] = {"OnNew","OnOpen","OnClose","OnSave","OnSaveAs","OnExportMix","OnExportSelection",
"OnExportLossyMix","OnExportLossySelection","OnExportLabels","OnPreferences","OnExit","Undo","Redo","UndoHistory","Cut",
"Copy","Paste","PasteOver","Trim","OnDelete","OnSilence","OnSplit","OnSplitLabels","OnDuplicate","OnSelectAll","OnSelectCursorEnd",
"OnSelectStartCursor","OnZoomIn","OnZoomOut","OnZoomNormal","OnZoomFit","OnZoomSel","OnPlotSpectrum",
"OnFloatControlToolBar","OnLoadEditToolBar","OnFloatEditToolBar","OnImport","OnImportLabels","OnImportMIDI","OnImportRaw",
"OnEditID3","OnQuickMix","OnSelectionSave","OnSelectionRestore","OnCursorTrackStart","OnCursorTrackEnd","OnCursorSelStart",
"OnCursorSelEnd","OnAlignZero","OnAlignGroupZero","OnAlign","OnAlignCursor","OnAlignSelStart","OnAlignSelEnd","OnAlignEndCursor",
"OnAlignEndSelStart","OnAlignEndSelEnd", "OnAlignGroupCursor","OnAlignGroupSelStart","OnAlignGroupSelEnd",
"OnAlignGroupEndCursor","OnAlignGroupEndSelStart","OnAlignGroupEndSelEnd","OnAlignZeroMoveSel","OnAlignGroupZeroMoveSel",
"OnAlignCursorMoveSel","OnAlignSelStartMoveSel","OnAlignSelEndMoveSel","OnAlignEndCursorMoveSel","OnAlignEndSelStartMoveSel",
"OnAlignEndSelEndMoveSel","OnAlignGroupCursorMoveSel","OnAlignGroupSelStartMoveSel","OnAlignGroupSelEndMoveSel",
"OnAlignGroupEndCursorMoveSel","OnAlignGroupEndSelStartMoveSel","OnAlignGroupEndSelEndMoveSel",
"OnNewWaveTrack", "OnNewLabelTrack","OnRemoveTracks","OnAbout","OnHelp","OnHelpIndex","OnHelpSearch","OnBenchmark","OnSeparator","OnAddLabel"};

#endif

#ifdef AUDACITY_COMMANDS_CALLBACK_CALLBACKS

// This part gets directly included into the AudacityProject class

        // File Menu

void OnNew();
void OnOpen();
void OnClose();
void OnSave();
void OnSaveAs();

void OnExportMix();
void OnExportSelection();
void OnExportLossyMix();
void OnExportLossySelection();

void OnExportLabels();

void OnPreferences();

void OnExit();

        // Edit Menu

void Undo();
void Redo();
void UndoHistory();

void Cut();
void Copy();
void Paste();
void PasteOver();
void Trim();

void OnDelete();
void OnSilence();

void OnSplit();
void OnSplitLabels();
void OnDuplicate();

void OnSelectAll();
void OnSelectCursorEnd();
void OnSelectStartCursor();

        // View Menu

void OnZoomIn();
void OnZoomOut();
void OnZoomNormal();
void OnZoomFit();
void OnZoomSel();

void OnPlotSpectrum();

void OnFloatControlToolBar();
void OnLoadEditToolBar();
void OnFloatEditToolBar();

        // Project Menu

void OnImport();
void OnImportLabels();
void OnImportMIDI();
void OnImportRaw();

void OnEditID3();

void OnQuickMix();

void OnSelectionSave();
void OnSelectionRestore();

void OnCursorTrackStart();
void OnCursorTrackEnd();
void OnCursorSelStart();
void OnCursorSelEnd();

void OnAlignZero();
void OnAlignZeroMoveSel();
void OnAlignGroupZero();
void OnAlignGroupZeroMoveSel();
void OnAlign();
void OnAlignCursor();
void OnAlignCursorMoveSel();
void OnAlignSelStart();
void OnAlignSelStartMoveSel();
void OnAlignSelEnd();
void OnAlignSelEndMoveSel();
void OnAlignEndCursor();
void OnAlignEndCursorMoveSel();
void OnAlignEndSelStart();
void OnAlignEndSelStartMoveSel();
void OnAlignEndSelEnd();
void OnAlignEndSelEndMoveSel();
void OnAlignGroupCursor();
void OnAlignGroupCursorMoveSel();
void OnAlignGroupSelStart();
void OnAlignGroupSelStartMoveSel();
void OnAlignGroupSelEnd();
void OnAlignGroupSelEndMoveSel();
void OnAlignGroupEndCursor();
void OnAlignGroupEndCursorMoveSel();
void OnAlignGroupEndSelStart();
void OnAlignGroupEndSelStartMoveSel();
void OnAlignGroupEndSelEnd();
void OnAlignGroupEndSelEndMoveSel();

void OnNewWaveTrack();
void OnNewLabelTrack();
void OnRemoveTracks();
void OnAddLabel();

        // Help Menu

void OnAbout();
void OnHelp();
void OnHelpIndex();
void OnHelpSearch();
void OnBenchmark();

       //

void OnSeparator();

#endif
