/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.h

  Dominic Mazzoni

  All of the menu bar handling is part of the class AudacityProject,
  but the event handlers for all of the menu items have been moved
  to Menus.h and Menus.cpp for clarity.

**********************************************************************/

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

int      mMenusDirtyCheck;

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

wxString &GetCommandName(int nIndex);
wxString &GetCommandDesc(int nIndex);
wxObjectEventFunction &GetCommandFunc(int nIndex);
int GetNumCommands();

void SetCommandState(int idItem, int iVal);
int GetCommandState(int nIndex);

bool GetCommandKeyText(int idItem, wxString *retName);

void OnUpdateMenus(wxUpdateUIEvent & event);

// Generic menu handler
bool HandleMenuEvent(wxEvent & event);

        // File Menu

void OnNew(wxCommandEvent & event);
void OnOpen(wxCommandEvent & event);
void OnClose(wxCommandEvent & event);
void OnSave(wxCommandEvent & event);
void OnSaveAs(wxCommandEvent & event);

void OnExportMix(wxCommandEvent & event);
void OnExportSelection(wxCommandEvent & event);
void OnExportLossyMix(wxCommandEvent & event);
void OnExportLossySelection(wxCommandEvent & event);

void OnExportLabels(wxCommandEvent & event);

void OnPreferences(wxCommandEvent & event);

void OnExit(wxCommandEvent & event);

        // Edit Menu

void Undo(wxCommandEvent & event);
void Redo(wxCommandEvent & event);
void UndoHistory(wxCommandEvent & event);

void Cut(wxCommandEvent & event);
void Copy(wxCommandEvent & event);
void Paste(wxCommandEvent & event);

void OnDelete(wxCommandEvent & event);
void OnSilence(wxCommandEvent & event);

void OnInsertSilence(wxCommandEvent & event);
void OnSplit(wxCommandEvent & event);
void OnDuplicate(wxCommandEvent & event);

void OnSelectAll(wxCommandEvent & event);

        // View Menu

void OnZoomIn(wxCommandEvent & event);
void OnZoomOut(wxCommandEvent & event);
void OnZoomNormal(wxCommandEvent & event);
void OnZoomFit(wxCommandEvent & event);
void OnZoomSel(wxCommandEvent & event);

void ZoomFit();
void ZoomSel();

void OnPlotSpectrum(wxCommandEvent & event);

void OnFloatPalette(wxCommandEvent & event);

        // Project Menu

void OnImport(wxCommandEvent & event);
void OnImportLabels(wxCommandEvent & event);
void OnImportMIDI(wxCommandEvent & event);
void OnImportRaw(wxCommandEvent & event);

void OnEditID3(wxCommandEvent & event);

void OnQuickMix(wxCommandEvent & event);

void OnAlignZero(wxCommandEvent & event);
void OnAlign(wxCommandEvent & event);

void OnNewWaveTrack(wxCommandEvent & event);
void OnNewLabelTrack(wxCommandEvent & event);
void OnRemoveTracks(wxCommandEvent & event);

        // Help Menu

void OnAbout(wxCommandEvent & event);
void OnHelp(wxCommandEvent & event);
void OnHelpIndex(wxCommandEvent & event);
void OnHelpSearch(wxCommandEvent & event);
void OnBenchmark(wxCommandEvent & event);

#endif
