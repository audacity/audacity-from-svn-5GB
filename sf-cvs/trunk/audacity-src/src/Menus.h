/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.h

  Dominic Mazzoni

  All of the menu bar handling is part of the class AudacityProject,
  but the event handlers for all of the menu items have been moved
  to Menus.h and Menus.cpp for clarity.

**********************************************************************/

#ifdef AUDACITY_MENUS_GLOBALS

#define AUDACITY_MENUS_COMMANDS_GLOBALS
#include "commands.h"
#undef AUDACITY_MENUS_COMMANDS_GLOBALS

#endif

#ifdef AUDACITY_MENUS_METHODS
private:
double mInsertSilenceAmount;

wxString mExportString;
wxString mExportSelectionString;
wxString mExportLossyString;
wxString mExportSelectionLossyString;

int mMenusDirtyCheck;

bool mLastNonZeroRegionSelected;

int mLastNumTracks;
int mLastNumTracksSelected;
int mLastNumWaveTracks;
int mLastNumWaveTracksSelected;
int mLastNumLabelTracks;
double mLastZoomLevel;
int mLastToolBarCheckSum;   //This finds the state of the toolbars:
                            //Base three for ControlToolBar, EditToolBar, etc
                            // 0: unloaded, 1: docked, 2: floating (* 3 for EditToolBar)
bool mLastUndoState;
bool mLastRedoState;
bool mLastClipboardState;   // true: clipboard full; false: clipboard empty.

bool mFirstTimeUpdateMenus;

#define AUDACITY_MENUS_COMMANDS_METHODS
#include "commands.h"
#undef AUDACITY_MENUS_COMMANDS_METHODS

public:
void CreateMenuBar();
void BuildMenuBar();
void RebuildMenuBar();
void AppendEffects(EffectArray *effs, wxMenu *menu,
                   bool spill);

void AssignDefaults();

void TokenizeCommandStrings(int mVal);

wxString GetCommandName(int nIndex);
wxString GetCommandDesc(int nIndex);
menuType GetMenuType(int nIndex);
audEventFunction GetCommandFunc(int nIndex);
int GetNumCommands();

void SetMenuState(wxMenu *menu, int id, bool enable);
void SetCommandState(int nID, int iVal);
int GetCommandState(int nIndex);

void SetCommandValue(int nID, wxString sName);

int FindCommandByCombos(wxString cName);

void OnUpdateMenus(wxUpdateUIEvent & event);

// Generic menu handler
bool HandleMenuEvent(wxEvent & event);

        // File Menu

void OnNew(wxEvent & event);
void OnOpen(wxEvent & event);
void OnClose(wxEvent & event);
void OnSave(wxEvent & event);
void OnSaveAs(wxEvent & event);

void OnExportMix(wxEvent & event);
void OnExportSelection(wxEvent & event);
void OnExportLossyMix(wxEvent & event);
void OnExportLossySelection(wxEvent & event);

void OnExportLabels(wxEvent & event);

void OnPreferences(wxEvent & event);

void OnExit(wxEvent & event);

        // Edit Menu

void Undo(wxEvent & event);
void Redo(wxEvent & event);
void UndoHistory(wxEvent & event);

void Cut(wxEvent & event);
void Copy(wxEvent & event);
void Paste(wxEvent & event);
void Trim(wxEvent & event);

void OnDelete(wxEvent & event);
void OnSilence(wxEvent & event);

void OnInsertSilence(wxEvent & event);
void OnSplit(wxEvent & event);
void OnSplitLabels(wxEvent & event);
void OnDuplicate(wxEvent & event);

void OnSelectAll(wxEvent & event);

        // View Menu

void Zoom(double level);
void OnZoomIn(wxEvent & event);
void OnZoomOut(wxEvent & event);
void OnZoomNormal(wxEvent & event);
void OnZoomFit(wxEvent & event);
void OnZoomSel(wxEvent & event);

void OnPlotSpectrum(wxEvent & event);

void OnFloatControlToolBar(wxEvent & event);
void OnLoadEditToolBar(wxEvent & event);
void OnFloatEditToolBar(wxEvent & event);

        // Project Menu

void OnImport(wxEvent & event);
void OnImportLabels(wxEvent & event);
void OnImportMIDI(wxEvent & event);
void OnImportRaw(wxEvent & event);

void OnEditID3(wxEvent & event);

void OnQuickMix(wxEvent & event);

void OnSelectionSave(wxEvent & event);
void OnSelectionRestore(wxEvent & event);

void OnCursorTrackStart(wxEvent & event);
void OnCursorTrackEnd(wxEvent & event);
void OnCursorSelStart(wxEvent & event);
void OnCursorSelEnd(wxEvent & event);

void OnAlignZero(wxEvent & event);
void OnAlign(wxEvent & event);
void OnAlignSelStart(wxEvent & event);
void OnAlignSelEnd(wxEvent & event);
void OnAlignEndSelStart(wxEvent & event);
void OnAlignEndSelEnd(wxEvent & event);
void OnAlignGroupSelStart(wxEvent & event);
void OnAlignGroupSelEnd(wxEvent & event);
void OnAlignGroupEndSelStart(wxEvent & event);
void OnAlignGroupEndSelEnd(wxEvent & event);

void OnNewWaveTrack(wxEvent & event);
void OnNewLabelTrack(wxEvent & event);
void OnRemoveTracks(wxEvent & event);

        // Help Menu

void OnAbout(wxEvent & event);
void OnHelp(wxEvent & event);
void OnHelpIndex(wxEvent & event);
void OnHelpSearch(wxEvent & event);
void OnBenchmark(wxEvent & event);

       //

void OnSeparator(wxEvent & event);

#endif
