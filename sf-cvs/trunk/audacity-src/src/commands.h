// BG: Generate an array of command names, and their corresponding functions
// BG: Included inside of Menu.cpp

// BG: This is inside a function. It is included to make it easier to edit

#ifdef AUDACITY_MENUS_COMMANDS_METHODS

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
   #define CMD_CACFUNC(a) ( (wxObjectEventFunction *)memcpy(malloc(sizeof(wxObjectEventFunction)), &(&a), sizeof(wxObjectEventFunction)) )
   #define CMD_CSTNAME(a) ( new wxString( a ) )

   // File menu
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnNew) );
   mCommandIDs.Add( CMD_CETINT(NewID) );
   mCommandNames.Add( CMD_CSTNAME("New") );
   mCommandDesc.Add( CMD_CSTNAME("New") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnOpen) );
   mCommandIDs.Add( CMD_CETINT(OpenID) );
   mCommandNames.Add( CMD_CSTNAME("Open") );
   mCommandDesc.Add( CMD_CSTNAME("Open") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnClose) );
   mCommandIDs.Add( CMD_CETINT(CloseID) );
   mCommandNames.Add( CMD_CSTNAME("Close") );
   mCommandDesc.Add( CMD_CSTNAME("Close") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnSave) );
   mCommandIDs.Add( CMD_CETINT(SaveID) );
   mCommandNames.Add( CMD_CSTNAME("Save") );
   mCommandDesc.Add( CMD_CSTNAME("Save") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnSaveAs) );
   mCommandIDs.Add( CMD_CETINT(SaveAsID) );
   mCommandNames.Add( CMD_CSTNAME("Save As") );
   mCommandDesc.Add( CMD_CSTNAME("Save As") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExportMix) );
   mCommandIDs.Add( CMD_CETINT(ExportMixID) );
   mCommandNames.Add( CMD_CSTNAME("Export Mix") );
   mCommandDesc.Add( CMD_CSTNAME("Export Mix") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExportSelection) );
   mCommandIDs.Add( CMD_CETINT(ExportSelectionID) );
   mCommandNames.Add( CMD_CSTNAME("Export Selection") );
   mCommandDesc.Add( CMD_CSTNAME("Export Selection") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExportLossyMix) );
   mCommandIDs.Add( CMD_CETINT(ExportLossyMixID) );
   mCommandNames.Add( CMD_CSTNAME("Export Lossy Mix") );
   mCommandDesc.Add( CMD_CSTNAME("Export Lossy Mix") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExportLossySelection) );
   mCommandIDs.Add( CMD_CETINT(ExportLossySelectionID) );
   mCommandNames.Add( CMD_CSTNAME("Export Lossy Selection") );
   mCommandDesc.Add( CMD_CSTNAME("Export Lossy Selection") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExportLabels) );
   mCommandIDs.Add( CMD_CETINT(ExportLabelsID) );
   mCommandNames.Add( CMD_CSTNAME("Export Labels") );
   mCommandDesc.Add( CMD_CSTNAME("Export Labels") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnPreferences) );
   mCommandIDs.Add( CMD_CETINT(PreferencesID) );
   mCommandNames.Add( CMD_CSTNAME("Preferences") );
   mCommandDesc.Add( CMD_CSTNAME("Preferences") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExit) );
   mCommandIDs.Add( CMD_CETINT(ExitID) );
   mCommandNames.Add( CMD_CSTNAME("Exit") );
   mCommandDesc.Add( CMD_CSTNAME("Exit") );
   // Edit menu
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::Undo) );
   mCommandIDs.Add( CMD_CETINT(UndoID) );
   mCommandNames.Add( CMD_CSTNAME("Undo") );
   mCommandDesc.Add( CMD_CSTNAME("Undo") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::Redo) );
   mCommandIDs.Add( CMD_CETINT(RedoID) );
   mCommandNames.Add( CMD_CSTNAME("Redo") );
   mCommandDesc.Add( CMD_CSTNAME("Redo") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::UndoHistory) );
   mCommandIDs.Add( CMD_CETINT(UndoHistoryID) );
   mCommandNames.Add( CMD_CSTNAME("Undo History") );
   mCommandDesc.Add( CMD_CSTNAME("Undo History") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::Cut) );
   mCommandIDs.Add( CMD_CETINT(CutID) );
   mCommandNames.Add( CMD_CSTNAME("Cut") );
   mCommandDesc.Add( CMD_CSTNAME("Cut") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::Copy) );
   mCommandIDs.Add( CMD_CETINT(CopyID) );
   mCommandNames.Add( CMD_CSTNAME("Copy") );
   mCommandDesc.Add( CMD_CSTNAME("Copy") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::Paste) );
   mCommandIDs.Add( CMD_CETINT(PasteID) );
   mCommandNames.Add( CMD_CSTNAME("Paste") );
   mCommandDesc.Add( CMD_CSTNAME("Paste") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnDelete) );
   mCommandIDs.Add( CMD_CETINT(DeleteID) );
   mCommandNames.Add( CMD_CSTNAME("Delete") );
   mCommandDesc.Add( CMD_CSTNAME("Delete") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnSilence) );
   mCommandIDs.Add( CMD_CETINT(SilenceID) );
   mCommandNames.Add( CMD_CSTNAME("Silence") );
   mCommandDesc.Add( CMD_CSTNAME("Silence") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnSplit) );
   mCommandIDs.Add( CMD_CETINT(SplitID) );
   mCommandNames.Add( CMD_CSTNAME("Split") );
   mCommandDesc.Add( CMD_CSTNAME("Split") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnDuplicate) );
   mCommandIDs.Add( CMD_CETINT(DuplicateID) );
   mCommandNames.Add( CMD_CSTNAME("Duplicate") );
   mCommandDesc.Add( CMD_CSTNAME("Duplicate") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnInsertSilence) );
   mCommandIDs.Add( CMD_CETINT(InsertSilenceID) );
   mCommandNames.Add( CMD_CSTNAME("Insert Silence") );
   mCommandDesc.Add( CMD_CSTNAME("Insert Silence") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnSelectAll) );
   mCommandIDs.Add( CMD_CETINT(SelectAllID) );
   mCommandNames.Add( CMD_CSTNAME("Select All") );
   mCommandDesc.Add( CMD_CSTNAME("Select All") );
   // View menu
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnZoomIn) );
   mCommandIDs.Add( CMD_CETINT(ZoomInID) );
   mCommandNames.Add( CMD_CSTNAME("Zoom In") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom In") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnZoomOut) );
   mCommandIDs.Add( CMD_CETINT(ZoomOutID) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Out") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Out") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnZoomNormal) );
   mCommandIDs.Add( CMD_CETINT(ZoomNormalID) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Normal") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Normal") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnZoomFit) );
   mCommandIDs.Add( CMD_CETINT(ZoomFitID) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Fit") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Fit") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnZoomSel) );
   mCommandIDs.Add( CMD_CETINT(ZoomSelID) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Sel") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Sel") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnPlotSpectrum) );
   mCommandIDs.Add( CMD_CETINT(PlotSpectrumID) );
   mCommandNames.Add( CMD_CSTNAME("Plot Spectrum") );
   mCommandDesc.Add( CMD_CSTNAME("Plot Spectrum") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnFloatPalette) );
   mCommandIDs.Add( CMD_CETINT(FloatPaletteID) );
   mCommandNames.Add( CMD_CSTNAME("Float Palette") );
   mCommandDesc.Add( CMD_CSTNAME("Float Palette") );
   // Project menu
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnImport) );
   mCommandIDs.Add( CMD_CETINT(ImportID) );
   mCommandNames.Add( CMD_CSTNAME("Import") );
   mCommandDesc.Add( CMD_CSTNAME("Import") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnImportLabels) );
   mCommandIDs.Add( CMD_CETINT(ImportLabelsID) );
   mCommandNames.Add( CMD_CSTNAME("Import Labels") );
   mCommandDesc.Add( CMD_CSTNAME("Import Labels") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnImportMIDI) );
   mCommandIDs.Add( CMD_CETINT(ImportMIDIID) );
   mCommandNames.Add( CMD_CSTNAME("Import MIDI") );
   mCommandDesc.Add( CMD_CSTNAME("Import MIDI") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnImportRaw) );
   mCommandIDs.Add( CMD_CETINT(ImportRawID) );
   mCommandNames.Add( CMD_CSTNAME("Import Raw") );
   mCommandDesc.Add( CMD_CSTNAME("Import Raw") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnEditID3) );
   mCommandIDs.Add( CMD_CETINT(EditID3ID) );
   mCommandNames.Add( CMD_CSTNAME("Edit ID3") );
   mCommandDesc.Add( CMD_CSTNAME("Edit ID3") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnAlign) );
   mCommandIDs.Add( CMD_CETINT(AlignID) );
   mCommandNames.Add( CMD_CSTNAME("Align") );
   mCommandDesc.Add( CMD_CSTNAME("Align") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnAlignZero) );
   mCommandIDs.Add( CMD_CETINT(AlignZeroID) );
   mCommandNames.Add( CMD_CSTNAME("Align Zero") );
   mCommandDesc.Add( CMD_CSTNAME("Align Zero") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnQuickMix) );
   mCommandIDs.Add( CMD_CETINT(QuickMixID) );
   mCommandNames.Add( CMD_CSTNAME("Quick Mix") );
   mCommandDesc.Add( CMD_CSTNAME("Quick Mix") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnNewWaveTrack) );
   mCommandIDs.Add( CMD_CETINT(NewWaveTrackID) );
   mCommandNames.Add( CMD_CSTNAME("New Wave Track") );
   mCommandDesc.Add( CMD_CSTNAME("New Wave Track") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnNewLabelTrack) );
   mCommandIDs.Add( CMD_CETINT(NewLabelTrackID) );
   mCommandNames.Add( CMD_CSTNAME("New Label Track") );
   mCommandDesc.Add( CMD_CSTNAME("New Label Track") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnRemoveTracks) );
   mCommandIDs.Add( CMD_CETINT(RemoveTracksID) );
   mCommandNames.Add( CMD_CSTNAME("Remove Tracks") );
   mCommandDesc.Add( CMD_CSTNAME("Remove Tracks") );
   // Help menu
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnAbout) );
   mCommandIDs.Add( CMD_CETINT(AboutID) );
   mCommandNames.Add( CMD_CSTNAME("About") );
   mCommandDesc.Add( CMD_CSTNAME("About") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnHelp) );
   mCommandIDs.Add( CMD_CETINT(HelpID) );
   mCommandNames.Add( CMD_CSTNAME("Help") );
   mCommandDesc.Add( CMD_CSTNAME("Help") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnHelpIndex) );
   mCommandIDs.Add( CMD_CETINT(HelpIndexID) );
   mCommandNames.Add( CMD_CSTNAME("Help Index") );
   mCommandDesc.Add( CMD_CSTNAME("Help Index") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnHelpSearch) );
   mCommandIDs.Add( CMD_CETINT(HelpSearchID) );
   mCommandNames.Add( CMD_CSTNAME("Help Search") );
   mCommandDesc.Add( CMD_CSTNAME("Help Search") );

   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnBenchmark) );
   mCommandIDs.Add( CMD_CETINT(BenchmarkID) );
   mCommandNames.Add( CMD_CSTNAME("Benchmark") );
   mCommandDesc.Add( CMD_CSTNAME("Benchmark") );
}

#endif
