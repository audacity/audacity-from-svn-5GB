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

#ifdef AUDACITY_MENUS_COMMANDS_GLOBALS


//This defines the  function pointer
// BG: Yes. For now, this seems like the best place to put it.

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
   TrimID,

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

   FloatControlToolBarID, 
   LoadEditToolBarID,
   FloatEditToolBarID,

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
   #define CMD_CACFUNC(a) ( (wxObjectEventFunction *)memcpy(malloc(sizeof(audEventFunction)), a, sizeof(audEventFunction)) )
   #define CMD_CSTNAME(a) ( new wxString( a ) )
   #define CMD_ASSIGNKEY(keyCode, controlDown, shiftDown, altDown) { wxKeyEvent tmpKeyEvent; tmpKeyEvent.m_x = 0; tmpKeyEvent.m_y = 0; tmpKeyEvent.m_keyCode = (wxKeyCode)keyCode; tmpKeyEvent.m_controlDown = controlDown; tmpKeyEvent.m_shiftDown = shiftDown; tmpKeyEvent.m_altDown = altDown; tmpKeyEvent.m_metaDown = FALSE; tmpKeyEvent.m_scanCode = 0; mCommandAssignedKey.Add( ( (wxKeyEvent *)memcpy(malloc(sizeof(wxKeyEvent)), &tmpKeyEvent, sizeof(wxKeyEvent)) ) ); }
   #define CMD_ADDFUNCTION(a) {fp = &AudacityProject::a; mCommandFunctions.Add(CMD_CACFUNC(&fp));}

   audEventFunction fp;  //Set up temporary function pointer to use for assigning keybindings
  
    // File menu
   CMD_ADDFUNCTION(OnNew);
   CMD_ASSIGNKEY('n', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(NewID) );
   mCommandNames.Add( CMD_CSTNAME("New") );
   mCommandDesc.Add( CMD_CSTNAME("New") );

   CMD_ADDFUNCTION(OnOpen);
   CMD_ASSIGNKEY('o', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(OpenID) );
   mCommandNames.Add( CMD_CSTNAME("Open") );
   mCommandDesc.Add( CMD_CSTNAME("Open") );

   CMD_ADDFUNCTION(OnClose);
   CMD_ASSIGNKEY('w', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(CloseID) );
   mCommandNames.Add( CMD_CSTNAME("Close") );
   mCommandDesc.Add( CMD_CSTNAME("Close") );

   CMD_ADDFUNCTION(OnSave);
   CMD_ASSIGNKEY('s', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(SaveID) );
   mCommandNames.Add( CMD_CSTNAME("Save") );
   mCommandDesc.Add( CMD_CSTNAME("Save") );

   CMD_ADDFUNCTION(OnSaveAs);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(SaveAsID) );
   mCommandNames.Add( CMD_CSTNAME("Save As") );
   mCommandDesc.Add( CMD_CSTNAME("Save As") );
   
   CMD_ADDFUNCTION(OnExportMix);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ExportMixID) );
   mCommandNames.Add( CMD_CSTNAME("Export Mix") );
   mCommandDesc.Add( CMD_CSTNAME("Export Mix") );

   CMD_ADDFUNCTION(OnExportSelection);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ExportSelectionID) );
   mCommandNames.Add( CMD_CSTNAME("Export Selection") );
   mCommandDesc.Add( CMD_CSTNAME("Export Selection") );

   CMD_ADDFUNCTION(OnExportLossyMix);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ExportLossyMixID) );
   mCommandNames.Add( CMD_CSTNAME("Export Lossy Mix") );
   mCommandDesc.Add( CMD_CSTNAME("Export Lossy Mix") );


   CMD_ADDFUNCTION(OnExportLossySelection);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ExportLossySelectionID) );
   mCommandNames.Add( CMD_CSTNAME("Export Lossy Selection") );
   mCommandDesc.Add( CMD_CSTNAME("Export Lossy Selection") );

   CMD_ADDFUNCTION(OnExportLabels);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ExportLabelsID) );
   mCommandNames.Add( CMD_CSTNAME("Export Labels") );
   mCommandDesc.Add( CMD_CSTNAME("Export Labels") );

   CMD_ADDFUNCTION(OnPreferences);
   CMD_ASSIGNKEY('p', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(PreferencesID) );
   mCommandNames.Add( CMD_CSTNAME("Preferences") );
   mCommandDesc.Add( CMD_CSTNAME("Preferences") );

   CMD_ADDFUNCTION(OnExit);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ExitID) );
   mCommandNames.Add( CMD_CSTNAME("Exit") );
   mCommandDesc.Add( CMD_CSTNAME("Exit") );

   // Edit menu
   CMD_ADDFUNCTION(Undo);
   CMD_ASSIGNKEY('z', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(UndoID) );
   mCommandNames.Add( CMD_CSTNAME("Undo") );
   mCommandDesc.Add( CMD_CSTNAME("Undo") );

   CMD_ADDFUNCTION(Redo);
   CMD_ASSIGNKEY('r', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(RedoID) );
   mCommandNames.Add( CMD_CSTNAME("Redo") );
   mCommandDesc.Add( CMD_CSTNAME("Redo") );

   CMD_ADDFUNCTION(UndoHistory);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(UndoHistoryID) );
   mCommandNames.Add( CMD_CSTNAME("Undo History") );
   mCommandDesc.Add( CMD_CSTNAME("Undo History") );

   CMD_ADDFUNCTION(Cut);
   CMD_ASSIGNKEY('x', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(CutID) );
   mCommandNames.Add( CMD_CSTNAME("Cut") );
   mCommandDesc.Add( CMD_CSTNAME("Cut") );

   CMD_ADDFUNCTION(Copy);
   CMD_ASSIGNKEY('c', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(CopyID) );
   mCommandNames.Add( CMD_CSTNAME("Copy") );
   mCommandDesc.Add( CMD_CSTNAME("Copy") );

   CMD_ADDFUNCTION(Paste);
   CMD_ASSIGNKEY('v', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(PasteID) );
   mCommandNames.Add( CMD_CSTNAME("Paste") );
   mCommandDesc.Add( CMD_CSTNAME("Paste") );

   CMD_ADDFUNCTION(Trim);
   CMD_ASSIGNKEY('t', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(TrimID) );
   mCommandNames.Add( CMD_CSTNAME("Trim") );
   mCommandDesc.Add( CMD_CSTNAME("Trim") );

   CMD_ADDFUNCTION(OnDelete);
   CMD_ASSIGNKEY('k', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(DeleteID) );
   mCommandNames.Add( CMD_CSTNAME("Delete") );
   mCommandDesc.Add( CMD_CSTNAME("Delete") );

   CMD_ADDFUNCTION(OnSilence);
   CMD_ASSIGNKEY('l', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(SilenceID) );
   mCommandNames.Add( CMD_CSTNAME("Silence") );
   mCommandDesc.Add( CMD_CSTNAME("Silence") );

   CMD_ADDFUNCTION(OnSplit);
   CMD_ASSIGNKEY('y', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(SplitID) );
   mCommandNames.Add( CMD_CSTNAME("Split") );
   mCommandDesc.Add( CMD_CSTNAME("Split") );

   CMD_ADDFUNCTION(OnDuplicate);
   CMD_ASSIGNKEY('d', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(DuplicateID) );
   mCommandNames.Add( CMD_CSTNAME("Duplicate") );
   mCommandDesc.Add( CMD_CSTNAME("Duplicate") );

   CMD_ADDFUNCTION(OnInsertSilence);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(InsertSilenceID) );
   mCommandNames.Add( CMD_CSTNAME("Insert Silence") );
   mCommandDesc.Add( CMD_CSTNAME("Insert Silence") );

   CMD_ADDFUNCTION(OnSelectAll);
   CMD_ASSIGNKEY('a', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(SelectAllID) );
   mCommandNames.Add( CMD_CSTNAME("Select All") );
   mCommandDesc.Add( CMD_CSTNAME("Select All") );

   // View menu
   CMD_ADDFUNCTION(OnZoomIn);
   CMD_ASSIGNKEY('1', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ZoomInID) );
   mCommandNames.Add( CMD_CSTNAME("Zoom In") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom In") );

   CMD_ADDFUNCTION(OnZoomOut);
   CMD_ASSIGNKEY('3', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ZoomOutID) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Out") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Out") );

   CMD_ADDFUNCTION(OnZoomNormal);
   CMD_ASSIGNKEY('2', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ZoomNormalID) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Normal") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Normal") );

   CMD_ADDFUNCTION(OnZoomFit);
   CMD_ASSIGNKEY('f', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ZoomFitID) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Fit") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Fit") );

   CMD_ADDFUNCTION(OnZoomSel);
   CMD_ASSIGNKEY('e', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ZoomSelID) );
   mCommandNames.Add( CMD_CSTNAME("Zoom Sel") );
   mCommandDesc.Add( CMD_CSTNAME("Zoom Sel") );

   CMD_ADDFUNCTION(OnPlotSpectrum);
   CMD_ASSIGNKEY('u', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(PlotSpectrumID) );
   mCommandNames.Add( CMD_CSTNAME("Plot Spectrum") );
   mCommandDesc.Add( CMD_CSTNAME("Plot Spectrum") );


   CMD_ADDFUNCTION(OnFloatControlToolBar);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(FloatControlToolBarID) );
   mCommandNames.Add( CMD_CSTNAME("Float Control Toolbar") );
   mCommandDesc.Add( CMD_CSTNAME("Float Control Toolbar") );


   CMD_ADDFUNCTION(OnLoadEditToolBar);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(LoadEditToolBarID) );
   mCommandNames.Add( CMD_CSTNAME("Load Edit Toolbar") );
   mCommandDesc.Add( CMD_CSTNAME("Load Edit Toolbar") );

   CMD_ADDFUNCTION(OnFloatEditToolBar);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(FloatEditToolBarID) );
   mCommandNames.Add( CMD_CSTNAME("Float Edit Toolbar") );
   mCommandDesc.Add( CMD_CSTNAME("Float Edit Toolbar") );

   // Project menu
   CMD_ADDFUNCTION(OnImport);
   CMD_ASSIGNKEY('i', TRUE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ImportID) );
   mCommandNames.Add( CMD_CSTNAME("Import") );
   mCommandDesc.Add( CMD_CSTNAME("Import") );

   CMD_ADDFUNCTION(OnImportLabels);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ImportLabelsID) );
   mCommandNames.Add( CMD_CSTNAME("Import Labels") );
   mCommandDesc.Add( CMD_CSTNAME("Import Labels") );

   CMD_ADDFUNCTION(OnImportMIDI);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ImportMIDIID) );
   mCommandNames.Add( CMD_CSTNAME("Import MIDI") );
   mCommandDesc.Add( CMD_CSTNAME("Import MIDI") );

   CMD_ADDFUNCTION(OnImportRaw);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(ImportRawID) );
   mCommandNames.Add( CMD_CSTNAME("Import Raw") );
   mCommandDesc.Add( CMD_CSTNAME("Import Raw") );

   CMD_ADDFUNCTION(OnEditID3);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(EditID3ID) );
   mCommandNames.Add( CMD_CSTNAME("Edit ID3") );
   mCommandDesc.Add( CMD_CSTNAME("Edit ID3") );

   CMD_ADDFUNCTION(OnAlign);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(AlignID) );
   mCommandNames.Add( CMD_CSTNAME("Align") );
   mCommandDesc.Add( CMD_CSTNAME("Align") );

   CMD_ADDFUNCTION(OnAlignZero);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(AlignZeroID) );
   mCommandNames.Add( CMD_CSTNAME("Align Zero") );
   mCommandDesc.Add( CMD_CSTNAME("Align Zero") );

   CMD_ADDFUNCTION(OnQuickMix);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(QuickMixID) );
   mCommandNames.Add( CMD_CSTNAME("Quick Mix") );
   mCommandDesc.Add( CMD_CSTNAME("Quick Mix") );

   CMD_ADDFUNCTION(OnNewWaveTrack);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(NewWaveTrackID) );
   mCommandNames.Add( CMD_CSTNAME("New Wave Track") );
   mCommandDesc.Add( CMD_CSTNAME("New Wave Track") );

   CMD_ADDFUNCTION(OnNewLabelTrack);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(NewLabelTrackID) );
   mCommandNames.Add( CMD_CSTNAME("New Label Track") );
   mCommandDesc.Add( CMD_CSTNAME("New Label Track") );

   CMD_ADDFUNCTION(OnRemoveTracks);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(RemoveTracksID) );
   mCommandNames.Add( CMD_CSTNAME("Remove Tracks") );
   mCommandDesc.Add( CMD_CSTNAME("Remove Tracks") );

   // Help menu
   CMD_ADDFUNCTION(OnAbout);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(AboutID) );
   mCommandNames.Add( CMD_CSTNAME("About") );
   mCommandDesc.Add( CMD_CSTNAME("About") );

   CMD_ADDFUNCTION(OnHelp);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(HelpID) );
   mCommandNames.Add( CMD_CSTNAME("Help") );
   mCommandDesc.Add( CMD_CSTNAME("Help") );

   CMD_ADDFUNCTION(OnHelpIndex);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(HelpIndexID) );
   mCommandNames.Add( CMD_CSTNAME("Help Index") );
   mCommandDesc.Add( CMD_CSTNAME("Help Index") );

   CMD_ADDFUNCTION(OnHelpSearch);
   CMD_ASSIGNKEY(0, FALSE, FALSE, FALSE);
   mCommandIDs.Add( CMD_CETINT(HelpSearchID) );
   mCommandNames.Add( CMD_CSTNAME("Help Search") );
   mCommandDesc.Add( CMD_CSTNAME("Help Search") );

   CMD_ADDFUNCTION(OnBenchmark);
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
   MNU_UPDATE_ACCELL(mEditMenu, TrimID)
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
   MNU_UPDATE_ACCELL(mViewMenu, FloatControlToolBarID)
   MNU_UPDATE_ACCELL(mViewMenu, LoadEditToolBarID)
   MNU_UPDATE_ACCELL(mViewMenu, FloatEditToolBarID)
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
