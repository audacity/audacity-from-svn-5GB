// BG: Generate an array of command names, and their corresponding functions
// BG: Included inside of Menu.cpp

// BG: This is inside a function. It is included to make it easier to edit

#ifdef AUDACITY_MENUS_COMMANDS_METHODS

   WX_DEFINE_ARRAY(wxKeyEvent *, CommandAssignedKeyArray);
   CommandAssignedKeyArray mCommandAssignedKey;

   WX_DEFINE_ARRAY(wxKeyEvent *, CommandUsedKeyArray);
   CommandUsedKeyArray mCommandUsedKey;

   WX_DEFINE_ARRAY(int *, CommandStateArray);
   CommandStateArray mCommandState;

   WX_DEFINE_ARRAY(int *, CommandIDArray);
   CommandIDArray mCommandIDs;

   WX_DEFINE_ARRAY(wxObjectEventFunction *, CommandFuncArray);
   CommandFuncArray mCommandFunctions;

   WX_DEFINE_ARRAY(wxString *, CommandNameArray);
   CommandNameArray mCommandNames;

   WX_DEFINE_ARRAY(wxString *, CommandDescArray);
   CommandDescArray mCommandDesc;

#endif

#ifdef AUDACITY_MENUS_COMMANDS_ENUM

typedef void (AudacityProject::*audEventFunction)(wxCommandEvent&);

enum {
   MenusFirstID = 1100,

   // File Menu

   NewID,
   OpenID,
   CloseID,
   SaveID,
   SaveAsID,
   ExportMixID,
   ExportSelectionID,
   ExportLossyMixID,
   ExportLossySelectionID,
   ExportLabelsID,
   PreferencesID,
   ExitID,

   // Edit Menu

   UndoID,
   RedoID,
   UndoHistoryID,

   CutID,
   CopyID,
   PasteID,

   DeleteID,
   SilenceID,

   InsertSilenceID,
   SplitID,
   DuplicateID,

   SelectAllID,

   // View Menu

   ZoomInID,
   ZoomOutID,
   ZoomNormalID,
   ZoomFitID,
   ZoomSelID,

   PlotSpectrumID,

   FloatPaletteID,

   // Project Menu

   ImportID,
   ImportLabelsID,
   ImportMIDIID,
   ImportRawID,
   
   EditID3ID,

   AlignZeroID,
   AlignID,

   NewWaveTrackID,
   NewLabelTrackID,
   RemoveTracksID,

   WaveDisplayID,
   SpectrumDisplayID,
   AutoCorrelateID,
   PitchID,
   QuickMixID,

   // Help Menu

   AboutID,
   HelpID,
   HelpIndexID,
   HelpSearchID,
   BenchmarkID,

   // Effect Menu

   FirstEffectID = 2000,

   // Plugin Menu

   FirstPluginID = 3000,
};

#endif

#ifdef AUDACITY_MENUS_COMMANDS_EVENT_TABLE

{
   #define CMD_CETINT(a) ( (int *) a )
   #define CMD_CACFUNC(a) ( (wxObjectEventFunction *)memcpy(malloc(sizeof(audEventFunction)), &((audEventFunction)&a), sizeof(audEventFunction)) )
   #define CMD_CSTNAME(a) ( new wxString( a ) )
   #define CMD_ASSIGNKEY(keyCode, controlDown, shiftDown, altDown) { wxKeyEvent tmpKeyEvent; tmpKeyEvent.m_x = 0; tmpKeyEvent.m_y = 0; tmpKeyEvent.m_keyCode = (wxKeyCode)keyCode; tmpKeyEvent.m_controlDown = controlDown; tmpKeyEvent.m_shiftDown = shiftDown; tmpKeyEvent.m_altDown = altDown; tmpKeyEvent.m_metaDown = FALSE; tmpKeyEvent.m_scanCode = 0; mCommandAssignedKey.Add( ( (wxKeyEvent *)memcpy(malloc(sizeof(wxKeyEvent)), &tmpKeyEvent, sizeof(wxKeyEvent)) ) ); }

   // File menu
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnNew) );
   CMD_ASSIGNKEY('n', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(NewID) );
   mCommandNames.Add( CMD_CSTNAME("New") );
   mCommandDesc.Add( CMD_CSTNAME("New") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnOpen) );
   CMD_ASSIGNKEY('o', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(OpenID) );
   mCommandNames.Add( CMD_CSTNAME("Open") );
   mCommandDesc.Add( CMD_CSTNAME("Open") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnClose) );
   CMD_ASSIGNKEY('w', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(CloseID) );
   mCommandNames.Add( CMD_CSTNAME("Close") );
   mCommandDesc.Add( CMD_CSTNAME("Close") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnSave) );
   CMD_ASSIGNKEY('s', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(SaveID) );
   mCommandNames.Add( CMD_CSTNAME("Save") );
   mCommandDesc.Add( CMD_CSTNAME("Save") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnSaveAs) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(SaveAsID) );
   mCommandNames.Add( CMD_CSTNAME("Save As") );
   mCommandDesc.Add( CMD_CSTNAME("Save As") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExportMix) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ExportMixID) );
   mCommandNames.Add( CMD_CSTNAME("Export Mix") );
   mCommandDesc.Add( CMD_CSTNAME("Export Mix") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExportSelection) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ExportSelectionID) );
   mCommandNames.Add( CMD_CSTNAME("Export Selection") );
   mCommandDesc.Add( CMD_CSTNAME("Export Selection") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExportLossyMix) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ExportLossyMixID) );
   mCommandNames.Add( CMD_CSTNAME("Export Lossy Mix") );
   mCommandDesc.Add( CMD_CSTNAME("Export Lossy Mix") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExportLossySelection) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ExportLossySelectionID) );
   mCommandNames.Add( CMD_CSTNAME("Export Lossy Selection") );
   mCommandDesc.Add( CMD_CSTNAME("Export Lossy Selection") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExportLabels) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ExportLabelsID) );
   mCommandNames.Add( CMD_CSTNAME("Export Labels") );
   mCommandDesc.Add( CMD_CSTNAME("Export Labels") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnPreferences) );
   CMD_ASSIGNKEY('p', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(PreferencesID) );
   mCommandNames.Add( CMD_CSTNAME("Preferences") );
   mCommandDesc.Add( CMD_CSTNAME("Preferences") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExit) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ExitID) );
   mCommandNames.Add( CMD_CSTNAME("Exit") );
   mCommandDesc.Add( CMD_CSTNAME("Exit") );

   // Edit menu
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::Undo) );
   CMD_ASSIGNKEY('z', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(UndoID) );
   mCommandNames.Add( CMD_CSTNAME("Undo") );
   mCommandDesc.Add( CMD_CSTNAME("Undo") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::Redo) );
   CMD_ASSIGNKEY('r', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(RedoID) );
   mCommandNames.Add( CMD_CSTNAME("Redo") );
   mCommandDesc.Add( CMD_CSTNAME("Redo") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::UndoHistory) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(UndoHistoryID) );
   mCommandNames.Add( CMD_CSTNAME("Undo History") );
   mCommandDesc.Add( CMD_CSTNAME("Undo History") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::Cut) );
   CMD_ASSIGNKEY('x', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(CutID) );
   mCommandNames.Add( CMD_CSTNAME("Cut") );
   mCommandDesc.Add( CMD_CSTNAME("Cut") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::Copy) );
   CMD_ASSIGNKEY('c', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(CopyID) );
   mCommandNames.Add( CMD_CSTNAME("Copy") );
   mCommandDesc.Add( CMD_CSTNAME("Copy") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::Paste) );
   CMD_ASSIGNKEY('v', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(PasteID) );
   mCommandNames.Add( CMD_CSTNAME("Paste") );
   mCommandDesc.Add( CMD_CSTNAME("Paste") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnDelete) );
   CMD_ASSIGNKEY('k', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(DeleteID) );
   mCommandNames.Add( CMD_CSTNAME("Delete") );
   mCommandDesc.Add( CMD_CSTNAME("Delete") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnSilence) );
   CMD_ASSIGNKEY('l', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(SilenceID) );
   mCommandNames.Add( CMD_CSTNAME("Silence") );
   mCommandDesc.Add( CMD_CSTNAME("Silence") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnSplit) );
   CMD_ASSIGNKEY('y', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(SplitID) );
   mCommandNames.Add( CMD_CSTNAME("Split") );
   mCommandDesc.Add( CMD_CSTNAME("Split") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnDuplicate) );
   CMD_ASSIGNKEY('d', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(DuplicateID) );
   mCommandNames.Add( CMD_CSTNAME("Duplicate") );
   mCommandDesc.Add( CMD_CSTNAME("Duplicate") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnInsertSilence) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(InsertSilenceID) );
   mCommandNames.Add( CMD_CSTNAME("Insert Silence") );
   mCommandDesc.Add( CMD_CSTNAME("Insert Silence") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnSelectAll) );
   CMD_ASSIGNKEY('a', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(SelectAllID) );
   mCommandNames.Add( CMD_CSTNAME("Select All") );
   mCommandDesc.Add( CMD_CSTNAME("Select All") );

   // View menu
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnZoomIn) );
   CMD_ASSIGNKEY('1', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ZoomInID) );
   mCommandNames.Add( CMD_CSTNAME("Zoom In") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom In") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnZoomOut) );
   CMD_ASSIGNKEY('3', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ZoomOutID) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Out") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Out") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnZoomNormal) );
   CMD_ASSIGNKEY('2', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ZoomNormalID) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Normal") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Normal") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnZoomFit) );
   CMD_ASSIGNKEY('f', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ZoomFitID) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Fit") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Fit") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnZoomSel) );
   CMD_ASSIGNKEY('e', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ZoomSelID) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Sel") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Sel") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnPlotSpectrum) );
   CMD_ASSIGNKEY('u', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(PlotSpectrumID) );
   mCommandNames.Add( CMD_CSTNAME("Plot Spectrum") );
   mCommandDesc.Add( CMD_CSTNAME("Plot Spectrum") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnFloatPalette) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(FloatPaletteID) );
   mCommandNames.Add( CMD_CSTNAME("Float Palette") );
   mCommandDesc.Add( CMD_CSTNAME("Float Palette") );

   // Project menu
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnImport) );
   CMD_ASSIGNKEY('i', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ImportID) );
   mCommandNames.Add( CMD_CSTNAME("Import") );
   mCommandDesc.Add( CMD_CSTNAME("Import") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnImportLabels) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ImportLabelsID) );
   mCommandNames.Add( CMD_CSTNAME("Import Labels") );
   mCommandDesc.Add( CMD_CSTNAME("Import Labels") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnImportMIDI) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ImportMIDIID) );
   mCommandNames.Add( CMD_CSTNAME("Import MIDI") );
   mCommandDesc.Add( CMD_CSTNAME("Import MIDI") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnImportRaw) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ImportRawID) );
   mCommandNames.Add( CMD_CSTNAME("Import Raw") );
   mCommandDesc.Add( CMD_CSTNAME("Import Raw") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnEditID3) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(EditID3ID) );
   mCommandNames.Add( CMD_CSTNAME("Edit ID3") );
   mCommandDesc.Add( CMD_CSTNAME("Edit ID3") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnAlign) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(AlignID) );
   mCommandNames.Add( CMD_CSTNAME("Align") );
   mCommandDesc.Add( CMD_CSTNAME("Align") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnAlignZero) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(AlignZeroID) );
   mCommandNames.Add( CMD_CSTNAME("Align Zero") );
   mCommandDesc.Add( CMD_CSTNAME("Align Zero") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnQuickMix) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(QuickMixID) );
   mCommandNames.Add( CMD_CSTNAME("Quick Mix") );
   mCommandDesc.Add( CMD_CSTNAME("Quick Mix") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnNewWaveTrack) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(NewWaveTrackID) );
   mCommandNames.Add( CMD_CSTNAME("New Wave Track") );
   mCommandDesc.Add( CMD_CSTNAME("New Wave Track") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnNewLabelTrack) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(NewLabelTrackID) );
   mCommandNames.Add( CMD_CSTNAME("New Label Track") );
   mCommandDesc.Add( CMD_CSTNAME("New Label Track") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnRemoveTracks) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(RemoveTracksID) );
   mCommandNames.Add( CMD_CSTNAME("Remove Tracks") );
   mCommandDesc.Add( CMD_CSTNAME("Remove Tracks") );

   // Help menu
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnAbout) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(AboutID) );
   mCommandNames.Add( CMD_CSTNAME("About") );
   mCommandDesc.Add( CMD_CSTNAME("About") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnHelp) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(HelpID) );
   mCommandNames.Add( CMD_CSTNAME("Help") );
   mCommandDesc.Add( CMD_CSTNAME("Help") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnHelpIndex) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(HelpIndexID) );
   mCommandNames.Add( CMD_CSTNAME("Help Index") );
   mCommandDesc.Add( CMD_CSTNAME("Help Index") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnHelpSearch) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(HelpSearchID) );
   mCommandNames.Add( CMD_CSTNAME("Help Search") );
   mCommandDesc.Add( CMD_CSTNAME("Help Search") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnBenchmark) );
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(BenchmarkID) );
   mCommandNames.Add( CMD_CSTNAME("Benchmark") );
   mCommandDesc.Add( CMD_CSTNAME("Benchmark") );

   for(int i = 0; i < GetNumCommands(); i++)
   {
      mCommandState.Add( CMD_CETINT(1) );
   }
}

#endif

#ifdef AUDACITY_MENUS_COMMANDS_ACCELL_TABLE

   #define MNU_UPDATE_ACCELL(menuClass, ID) { wxString mnuText; mnuText = (menuClass->FindItem(ID))->GetLabel(); if(GetCommandKeyText(ID, &mnuText)) { (menuClass->FindItem(ID))->SetText(mnuText); } }

   MNU_UPDATE_ACCELL(mFileMenu, NewID)
   MNU_UPDATE_ACCELL(mFileMenu, OpenID)
   MNU_UPDATE_ACCELL(mFileMenu, CloseID)
   MNU_UPDATE_ACCELL(mFileMenu, SaveID)
   MNU_UPDATE_ACCELL(mFileMenu, SaveAsID)
   MNU_UPDATE_ACCELL(mFileMenu, ExportMixID)
   MNU_UPDATE_ACCELL(mFileMenu, ExportSelectionID)
   MNU_UPDATE_ACCELL(mFileMenu, ExportLossyMixID)
   MNU_UPDATE_ACCELL(mFileMenu, ExportLossySelectionID)
   MNU_UPDATE_ACCELL(mFileMenu, ExportLabelsID)
   MNU_UPDATE_ACCELL(mFileMenu, PreferencesID)
   MNU_UPDATE_ACCELL(mFileMenu, ExitID)
   MNU_UPDATE_ACCELL(mEditMenu, UndoID)
   MNU_UPDATE_ACCELL(mEditMenu, RedoID)
   MNU_UPDATE_ACCELL(mViewMenu, UndoHistoryID)
   MNU_UPDATE_ACCELL(mEditMenu, CutID)
   MNU_UPDATE_ACCELL(mEditMenu, CopyID)
   MNU_UPDATE_ACCELL(mEditMenu, PasteID)
   MNU_UPDATE_ACCELL(mEditMenu, DeleteID)
   MNU_UPDATE_ACCELL(mEditMenu, SilenceID)
   MNU_UPDATE_ACCELL(mEditMenu, SplitID)
   MNU_UPDATE_ACCELL(mEditMenu, DuplicateID)
   MNU_UPDATE_ACCELL(mEditMenu, InsertSilenceID)
   MNU_UPDATE_ACCELL(mEditMenu, SelectAllID)
   MNU_UPDATE_ACCELL(mViewMenu, ZoomInID)
   MNU_UPDATE_ACCELL(mViewMenu, ZoomOutID)
   MNU_UPDATE_ACCELL(mViewMenu, ZoomNormalID)
   MNU_UPDATE_ACCELL(mViewMenu, ZoomFitID)
   MNU_UPDATE_ACCELL(mViewMenu, ZoomSelID)
   MNU_UPDATE_ACCELL(mViewMenu, PlotSpectrumID)
   MNU_UPDATE_ACCELL(mViewMenu, FloatPaletteID)
   MNU_UPDATE_ACCELL(mProjectMenu, ImportID)
   MNU_UPDATE_ACCELL(mProjectMenu, ImportLabelsID)
   MNU_UPDATE_ACCELL(mProjectMenu, ImportMIDIID)
   MNU_UPDATE_ACCELL(mProjectMenu, ImportRawID)
   MNU_UPDATE_ACCELL(mProjectMenu, AlignID)
   MNU_UPDATE_ACCELL(mProjectMenu, AlignZeroID)
   MNU_UPDATE_ACCELL(mProjectMenu, QuickMixID)
   MNU_UPDATE_ACCELL(mProjectMenu, NewWaveTrackID)
   MNU_UPDATE_ACCELL(mProjectMenu, NewLabelTrackID)
   MNU_UPDATE_ACCELL(mProjectMenu, RemoveTracksID)
   MNU_UPDATE_ACCELL(mHelpMenu, AboutID)
   MNU_UPDATE_ACCELL(mHelpMenu, HelpID)
   MNU_UPDATE_ACCELL(mHelpMenu, HelpIndexID)
   MNU_UPDATE_ACCELL(mHelpMenu, HelpSearchID)
   MNU_UPDATE_ACCELL(mHelpMenu, BenchmarkID)

#endif
