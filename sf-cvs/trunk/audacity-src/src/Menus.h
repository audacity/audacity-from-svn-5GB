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

#ifdef AUDACITY_MENUS_ENUM

#define AUDACITY_MENUS_COMMANDS_ENUM
#include "commands.h"
#undef AUDACITY_MENUS_COMMANDS_ENUM

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

bool mFirstTimeUpdateMenus;

#define AUDACITY_MENUS_COMMANDS_METHODS
#include "commands.h"
#undef AUDACITY_MENUS_COMMANDS_METHODS

public:
void CreateMenuBar();

wxString GetCommandName(int nIndex);
wxString GetCommandDesc(int nIndex);
audEventFunction GetCommandFunc(int nIndex);
int GetNumCommands();

void SetCommandState(int nID, int iVal);
int GetCommandState(int nIndex);

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
void OnDuplicate(wxEvent & event);

void OnSelectAll(wxEvent & event);

        // View Menu

void OnZoomIn(wxEvent & event);
void OnZoomOut(wxEvent & event);
void OnZoomNormal(wxEvent & event);
void OnZoomFit(wxEvent & event);
void OnZoomSel(wxEvent & event);

void ZoomFit();
void ZoomSel();

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

void OnAlignZero(wxEvent & event);
void OnAlign(wxEvent & event);

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

void OnSeperator(wxEvent & event);

#endif
