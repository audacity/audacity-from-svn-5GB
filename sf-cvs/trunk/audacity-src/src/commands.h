// BG: Generate an array of command names, and their corresponding functions
// BG: Included inside of Menu.cpp

// BG: This is inside a function. It is included to make it easier to edit

#ifdef AUDACITY_MENUS_COMMANDS_METHODS

   WX_DEFINE_ARRAY(wxObjectEventFunction *, CommandFuncArray);
   CommandFuncArray mCommandFunctions;

   WX_DEFINE_ARRAY(wxString *, CommandNameArray);
   CommandNameArray mCommandNames;

#else

{
   #define CMD_CACFUNC(a) { cmdfunctemp = (wxObjectEventFunction *)malloc(sizeof(wxObjectEventFunction)); memcpy(cmdfunctemp, &(&a), sizeof(wxObjectEventFunction)); }
   #define CMD_CSTNAME(a) ( new wxString( a ) )

   wxObjectEventFunction *cmdfunctemp;

   // File menu
   CMD_CACFUNC(AudacityProject::OnNew)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("New") );
   CMD_CACFUNC(AudacityProject::OnOpen)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Open") );
   CMD_CACFUNC(AudacityProject::OnClose)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Close") );
   CMD_CACFUNC(AudacityProject::OnSave)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Save") );
   CMD_CACFUNC(AudacityProject::OnSaveAs)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Save As") );
   CMD_CACFUNC(AudacityProject::OnExportMix)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Export Mix") );
   CMD_CACFUNC(AudacityProject::OnExportSelection)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Export Selection") );
   CMD_CACFUNC(AudacityProject::OnExportLossyMix)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Export Lossy Mix") );
   CMD_CACFUNC(AudacityProject::OnExportLossySelection)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Export Lossy Selection") );
   CMD_CACFUNC(AudacityProject::OnExportLabels)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Export Labels") );
   CMD_CACFUNC(AudacityProject::OnPreferences)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Preferences") );
   CMD_CACFUNC(AudacityProject::OnExit)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Exit") );

   // Edit menu
   CMD_CACFUNC(AudacityProject::Undo)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Undo") );
   CMD_CACFUNC(AudacityProject::Redo)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Redo") );
   CMD_CACFUNC(AudacityProject::UndoHistory)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Undo History") );
   CMD_CACFUNC(AudacityProject::Cut)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Cut") );
   CMD_CACFUNC(AudacityProject::Copy)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Copy") );
   CMD_CACFUNC(AudacityProject::Paste)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Paste") );
   CMD_CACFUNC(AudacityProject::OnDelete)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Delete") );
   CMD_CACFUNC(AudacityProject::OnSilence)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Silence") );
   CMD_CACFUNC(AudacityProject::OnSplit)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Split") );
   CMD_CACFUNC(AudacityProject::OnDuplicate)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Duplicate") );
   CMD_CACFUNC(AudacityProject::OnInsertSilence)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Insert Silence") );
   CMD_CACFUNC(AudacityProject::OnSelectAll)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Select All") );

   // View menu
   CMD_CACFUNC(AudacityProject::OnZoomIn)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Zoom In") );
   CMD_CACFUNC(AudacityProject::OnZoomOut)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Zoom Out") );
   CMD_CACFUNC(AudacityProject::OnZoomNormal)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Zoom Normal") );
   CMD_CACFUNC(AudacityProject::OnZoomFit)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Zoom Fit") );
   CMD_CACFUNC(AudacityProject::OnZoomSel)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Zoom Sel") );
   CMD_CACFUNC(AudacityProject::OnPlotSpectrum)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Plot Spectrum") );
   CMD_CACFUNC(AudacityProject::OnFloatPalette)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Float Palette") );

   // Project menu
   CMD_CACFUNC(AudacityProject::OnImport)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Import") );
   CMD_CACFUNC(AudacityProject::OnImportLabels)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Import Labels") );
   CMD_CACFUNC(AudacityProject::OnImportMIDI)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Import MIDI") );
   CMD_CACFUNC(AudacityProject::OnImportRaw)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Import Raw") );
   CMD_CACFUNC(AudacityProject::OnEditID3)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Edit ID3") );
   CMD_CACFUNC(AudacityProject::OnAlign)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Align") );
   CMD_CACFUNC(AudacityProject::OnAlignZero)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Align Zero") );
   CMD_CACFUNC(AudacityProject::OnQuickMix)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Quick Mix") );
   CMD_CACFUNC(AudacityProject::OnNewWaveTrack)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("New Wave Track") );
   CMD_CACFUNC(AudacityProject::OnNewLabelTrack)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("New Label Track") );
   CMD_CACFUNC(AudacityProject::OnRemoveTracks)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Remove Tracks") );

   // Help menu
   CMD_CACFUNC(AudacityProject::OnAbout)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("About") );
   CMD_CACFUNC(AudacityProject::OnHelp)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Help") );
   CMD_CACFUNC(AudacityProject::OnHelpIndex)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Help Index") );
   CMD_CACFUNC(AudacityProject::OnHelpSearch)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Help Search") );
   CMD_CACFUNC(AudacityProject::OnBenchmark)
   mCommandFunctions.Add( cmdfunctemp );
   mCommandNames.Add( CMD_CSTNAME("Benchmark") );
}

#endif
