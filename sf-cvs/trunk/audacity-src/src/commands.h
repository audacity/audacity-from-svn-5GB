// BG: Generate an array of command names, and their corresponding functions
// BG: Included inside of Menu.cpp

// BG: This is inside a function. It is included to make it easier to edit

#ifdef AUDACITY_MENUS_COMMANDS_METHODS

   WX_DEFINE_ARRAY(wxObjectEventFunction *, CommandFuncArray);
   CommandFuncArray mCommandFunctions;

   WX_DEFINE_ARRAY(wxString *, CommandNameArray);
   CommandNameArray mCommandNames;

   WX_DEFINE_ARRAY(wxString *, CommandDescArray);
   CommandDescArray mCommandDesc;

#else

{
   #define CMD_CACFUNC(a) ( (wxObjectEventFunction *)memcpy(malloc(sizeof(wxObjectEventFunction)), &(&a), sizeof(wxObjectEventFunction)) )
   #define CMD_CSTNAME(a) ( new wxString( a ) )

   // File menu
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnNew) );
   mCommandNames.Add( CMD_CSTNAME("New") );
   mCommandDesc.Add( CMD_CSTNAME("New") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnOpen) );
   mCommandNames.Add( CMD_CSTNAME("Open") );
   mCommandDesc.Add( CMD_CSTNAME("Open") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnClose) );
   mCommandNames.Add( CMD_CSTNAME("Close") );
   mCommandDesc.Add( CMD_CSTNAME("Close") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnSave) );
   mCommandNames.Add( CMD_CSTNAME("Save") );
   mCommandDesc.Add( CMD_CSTNAME("Save") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnSaveAs) );
   mCommandNames.Add( CMD_CSTNAME("Save As") );
   mCommandDesc.Add( CMD_CSTNAME("Save As") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExportMix) );
   mCommandNames.Add( CMD_CSTNAME("Export Mix") );
   mCommandDesc.Add( CMD_CSTNAME("Export Mix") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExportSelection) );
   mCommandNames.Add( CMD_CSTNAME("Export Selection") );
   mCommandDesc.Add( CMD_CSTNAME("Export Selection") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExportLossyMix) );
   mCommandNames.Add( CMD_CSTNAME("Export Lossy Mix") );
   mCommandDesc.Add( CMD_CSTNAME("Export Lossy Mix") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExportLossySelection) );
   mCommandNames.Add( CMD_CSTNAME("Export Lossy Selection") );
   mCommandDesc.Add( CMD_CSTNAME("Export Lossy Selection") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExportLabels) );
   mCommandNames.Add( CMD_CSTNAME("Export Labels") );
   mCommandDesc.Add( CMD_CSTNAME("Export Labels") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnPreferences) );
   mCommandNames.Add( CMD_CSTNAME("Preferences") );
   mCommandDesc.Add( CMD_CSTNAME("Preferences") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnExit) );
   mCommandNames.Add( CMD_CSTNAME("Exit") );
   mCommandDesc.Add( CMD_CSTNAME("Exit") );

   // Edit menu
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::Undo) );
   mCommandNames.Add( CMD_CSTNAME("Undo") );
   mCommandDesc.Add( CMD_CSTNAME("Undo") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::Redo) );
   mCommandNames.Add( CMD_CSTNAME("Redo") );
   mCommandDesc.Add( CMD_CSTNAME("Redo") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::UndoHistory) );
   mCommandNames.Add( CMD_CSTNAME("Undo History") );
   mCommandDesc.Add( CMD_CSTNAME("Undo History") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::Cut) );
   mCommandNames.Add( CMD_CSTNAME("Cut") );
   mCommandDesc.Add( CMD_CSTNAME("Cut") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::Copy) );
   mCommandNames.Add( CMD_CSTNAME("Copy") );
   mCommandDesc.Add( CMD_CSTNAME("Copy") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::Paste) );
   mCommandNames.Add( CMD_CSTNAME("Paste") );
   mCommandDesc.Add( CMD_CSTNAME("Paste") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnDelete) );
   mCommandNames.Add( CMD_CSTNAME("Delete") );
   mCommandDesc.Add( CMD_CSTNAME("Delete") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnSilence) );
   mCommandNames.Add( CMD_CSTNAME("Silence") );
   mCommandDesc.Add( CMD_CSTNAME("Silence") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnSplit) );
   mCommandNames.Add( CMD_CSTNAME("Split") );
   mCommandDesc.Add( CMD_CSTNAME("Split") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnDuplicate) );
   mCommandNames.Add( CMD_CSTNAME("Duplicate") );
   mCommandDesc.Add( CMD_CSTNAME("Duplicate") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnInsertSilence) );
   mCommandNames.Add( CMD_CSTNAME("Insert Silence") );
   mCommandDesc.Add( CMD_CSTNAME("Insert Silence") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnSelectAll) );
   mCommandNames.Add( CMD_CSTNAME("Select All") );
   mCommandDesc.Add( CMD_CSTNAME("Select All") );

   // View menu
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnZoomIn) );
   mCommandNames.Add( CMD_CSTNAME("Zoom In") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom In") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnZoomOut) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Out") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Out") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnZoomNormal) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Normal") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Normal") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnZoomFit) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Fit") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Fit") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnZoomSel) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Sel") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Sel") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnPlotSpectrum) );
   mCommandNames.Add( CMD_CSTNAME("Plot Spectrum") );
   mCommandDesc.Add( CMD_CSTNAME("Plot Spectrum") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnFloatPalette) );
   mCommandNames.Add( CMD_CSTNAME("Float Palette") );
   mCommandDesc.Add( CMD_CSTNAME("Float Palette") );

   // Project menu
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnImport) );
   mCommandNames.Add( CMD_CSTNAME("Import") );
   mCommandDesc.Add( CMD_CSTNAME("Import") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnImportLabels) );
   mCommandNames.Add( CMD_CSTNAME("Import Labels") );
   mCommandDesc.Add( CMD_CSTNAME("Import Labels") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnImportMIDI) );
   mCommandNames.Add( CMD_CSTNAME("Import MIDI") );
   mCommandDesc.Add( CMD_CSTNAME("Import MIDI") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnImportRaw) );
   mCommandNames.Add( CMD_CSTNAME("Import Raw") );
   mCommandDesc.Add( CMD_CSTNAME("Import Raw") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnEditID3) );
   mCommandNames.Add( CMD_CSTNAME("Edit ID3") );
   mCommandDesc.Add( CMD_CSTNAME("Edit ID3") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnAlign) );
   mCommandNames.Add( CMD_CSTNAME("Align") );
   mCommandDesc.Add( CMD_CSTNAME("Align") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnAlignZero) );
   mCommandNames.Add( CMD_CSTNAME("Align Zero") );
   mCommandDesc.Add( CMD_CSTNAME("Align Zero") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnQuickMix) );
   mCommandNames.Add( CMD_CSTNAME("Quick Mix") );
   mCommandDesc.Add( CMD_CSTNAME("Quick Mix") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnNewWaveTrack) );
   mCommandNames.Add( CMD_CSTNAME("New Wave Track") );
   mCommandDesc.Add( CMD_CSTNAME("New Wave Track") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnNewLabelTrack) );
   mCommandNames.Add( CMD_CSTNAME("New Label Track") );
   mCommandDesc.Add( CMD_CSTNAME("New Label Track") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnRemoveTracks) );
   mCommandNames.Add( CMD_CSTNAME("Remove Tracks") );
   mCommandDesc.Add( CMD_CSTNAME("Remove Tracks") );

   // Help menu
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnAbout) );
   mCommandNames.Add( CMD_CSTNAME("About") );
   mCommandDesc.Add( CMD_CSTNAME("About") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnHelp) );
   mCommandNames.Add( CMD_CSTNAME("Help") );
   mCommandDesc.Add( CMD_CSTNAME("Help") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnHelpIndex) );
   mCommandNames.Add( CMD_CSTNAME("Help Index") );
   mCommandDesc.Add( CMD_CSTNAME("Help Index") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnHelpSearch) );
   mCommandNames.Add( CMD_CSTNAME("Help Search") );
   mCommandDesc.Add( CMD_CSTNAME("Help Search") );
   mCommandFunctions.Add( CMD_CACFUNC(AudacityProject::OnBenchmark) );
   mCommandNames.Add( CMD_CSTNAME("Benchmark") );
   mCommandDesc.Add( CMD_CSTNAME("Benchmark") );
}

#endif
