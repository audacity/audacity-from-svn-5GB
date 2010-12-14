/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.h

  Dominic Mazzoni

**********************************************************************/
#ifndef __AUDACITY_MENUS__
#define __AUDACITY_MENUS__

// These are all member functions of class AudacityProject.

void CommandManagerCallback(void *fptr);
void CommandManagerListCallback(void *fptr, int index);

void CreateMenusAndCommands();

void CreateRecentFilesMenu(CommandManager *c);
void ModifyUndoMenus();
void ModifyToolbarMenus();

int GetFocusedFrame();
wxUint32 GetUpdateFlags();

double NearestZeroCrossing(double t0);


//Adds label and returns index of label in labeltrack.
int DoAddLabel(double left, double right);

// used in routines OnSelectionSave
// and OnSelectionRestore
double mSel0save;
double mSel1save;
 
        // Selecting a tool from the keyboard

void SetTool(int tool);
void OnSelectTool();
void OnZoomTool();
void OnEnvelopeTool();
void OnTimeShiftTool();
void OnDrawTool();
void OnMultiTool();

void OnNextTool();
void OnPrevTool();

        // Audio I/O Commands

void OnStop();
void OnPause();
void OnRecord();
void OnRecordAppend();
void OnStopSelect();
void OnSkipStart();
void OnSkipEnd();
void OnSeekLeftShort();
void OnSeekRightShort();
void OnSeekLeftLong();
void OnSeekRightLong();

        // Different posibilities for playing sound

bool MakeReadyToPlay(); // Helper function that sets button states etc.
void OnPlayStop();
void OnPlayStopSelect();
void OnPlayOneSecond();
void OnPlayToSelection();
void OnPlayLooped();
void OnPlayCutPreview();

        //

void OnTrackPan();
void OnTrackPanLeft();
void OnTrackPanRight();
void OnTrackGain();
void OnTrackGainInc();
void OnTrackGainDec();
void OnTrackMenu();
void OnTrackMute();
void OnTrackSolo();
void OnTrackClose();


        // Selection-Editing Commands

void OnCursorUp();
void OnShiftUp();
void OnCursorDown();
void OnShiftDown();
void OnToggle();

void OnCursorLeft();
void OnCursorRight();
void OnSelExtendLeft();
void OnSelExtendRight();
void OnSelContractLeft();
void OnSelContractRight();

void OnCursorShortJumpLeft();
void OnCursorShortJumpRight();
void OnCursorLongJumpLeft();
void OnCursorLongJumpRight();
void OnSelSetExtendLeft();
void OnSelSetExtendRight();

void OnSetLeftSelection();
void OnSetRightSelection();

void OnSelToStart();
void OnSelToEnd();

void OnZeroCrossing();

void OnLockPlayRegion();
void OnUnlockPlayRegion();

double GetTime(Track *t);
void OnSortTime();
void OnSortName();

void OnSnapToOn();
void OnSnapToOff();

        // File Menu

void OnNew();
void OnOpen();
void OnClose();
void OnSave();
void OnSaveAs();
#ifdef USE_LIBVORBIS
   void OnSaveCompressed();
#endif

void OnCheckDependencies();

void OnExport();
void OnExportSelection();
void OnExportMultiple();
void OnExportLabels();

void OnUpload();

void OnPreferences();

void OnPageSetup();
void OnPrint();

void OnExit();

        // Edit Menu

void OnUndo();
void OnRedo();
void OnHistory();

void OnCut();
void OnSplitCut();
void OnCopy();
void OnPaste();
void OnPasteOver();
void OnTrim();

void OnDelete();
void OnSplitDelete();
void OnSilence();

void OnSplit();
void OnSplitNew();
void OnSplitLabelsToTracks();
void OnJoin();
void OnDisjoin();
void OnDuplicate();

void OnCutLabels();
void OnSplitCutLabels();
void OnCopyLabels();
void OnDeleteLabels();
void OnSplitDeleteLabels();
void OnSilenceLabels();
void OnSplitLabels();
void OnJoinLabels();
void OnDisjoinLabels();

void OnSelectAll();
void OnSelectNone();
void OnSelectCursorEnd();
void OnSelectStartCursor();

        // View Menu

void OnZoomIn();
void OnZoomOut();
void OnZoomToggle();
void OnZoomNormal();
void OnZoomFit();
void OnZoomFitV();
void DoZoomFitV();
void OnZoomSel();

void OnExpandAllTracks();
void OnCollapseAllTracks();

void OnMuteAllTracks();
void OnUnMuteAllTracks();

void OnShowClipping();

void OnSelectionFormat(int index);

void OnPlotSpectrum();

void OnShowControlToolBar();
void OnShowDeviceToolBar();
void OnShowEditToolBar();
void OnShowMeterToolBar();
void OnShowMixerToolBar();
void OnShowSelectionToolBar();
void OnShowToolsToolBar();
void OnShowTranscriptionToolBar();
void OnResetToolBars();
void OnSimplifiedView();


        // Project Menu

void OnImport();
void OnImportLabels();
void OnImportMIDI();
void OnImportRaw();

void OnEditMetadata();

void OnMixAndRender();
void OnMixAndRenderToNewTrack();
void HandleMixAndRender(bool toNewTrack);

void OnSelectionSave();
void OnSelectionRestore();

void OnCursorTrackStart();
void OnCursorTrackEnd();
void OnCursorSelStart();
void OnCursorSelEnd();

void OnAlign(int index);
void OnAlignMoveSel(int index);
void HandleAlign(int index, bool moveSel);

void OnNewWaveTrack();
void OnNewStereoTrack();
void OnNewLabelTrack();
void OnNewTimeTrack();
void OnSmartRecord();
void OnRemoveTracks();
void OnAddLabel();
void OnAddLabelPlaying();
void OnEditLabels();

        // Effect Menu

bool OnEffect(int type, Effect * f);
void OnEffect(int type, int index);
void OnGenerateEffect(int index);
void OnGeneratePlugin(int index);
void OnRepeatLastEffect(int index);
void OnProcessEffect(int index);
void OnProcessPlugin(int index);
void OnAnalyzeEffect(int index);
void OnAnalyzePlugin(int index);

        //lda CleanSpeech Menu

void OnApplyChain();
void OnEditChains();
void OnImportCleanSpeechPresets();
void OnExportCleanSpeechPresets();
void OnStereoToMono(int index);
void ResolveEffectIndices(EffectArray *effects);
wxString BuildCleanFileName(wxString fileName);

int  mNormalizeIndex;
int  mStereoToMonoIndex;

        // Help Menu

void OnAbout();
void OnHelp();
void OnHelpWelcome();
void OnBenchmark();
void OnScreenshot();
void OnAudioDeviceInfo();

       //

void OnSeparator();

      // Keyboard navigation

void PrevFrame();
void NextFrame();

void OnResample();

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
// arch-tag: 21c627ef-9d3a-4836-b10a-7b0d67b97cb5

