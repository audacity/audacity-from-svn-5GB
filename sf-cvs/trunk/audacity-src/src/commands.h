// BG: Generate an array of command names, and their corresponding functions
// BG: Included inside of Menu.cpp

// BG: This is inside a function. It is included to make it easier to edit

#ifdef AUDACITY_MENUS_COMMANDS_METHODS

   WX_DEFINE_ARRAY(CommandMenuItem *, CommandMenuItemArray);
   CommandMenuItemArray mCommandMenuItem;

#endif

#ifdef AUDACITY_MENUS_COMMANDS_GLOBALS

#ifndef AMGC_DEFINED_H
#define AMGC_DEFINED_H

//This defines the  function pointer
// BG: Yes. For now, this seems like the best place to put it.

typedef void (AudacityProject::*audEventFunction)(wxEvent&);

enum menuState {disabledMenu = 0, enabledMenu};

enum menuCategory {fileMenu = 0, editMenu, viewMenu, projectMenu, effectMenu, pluginsMenu, helpMenu};

// BG: This is the structure that holds information about individual command items.

struct CommandMenuItem
{
   wxString         commandString;
   wxString         descriptionString;
   audEventFunction callbackFunction;
   menuCategory     category;
   menuState        state;
};

#endif

#endif

#ifdef AUDACITY_MENUS_COMMANDS_ENUM

#define MenuBaseID 1101

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
   ZoomNormalID,
   ZoomOutID,
   ZoomFitID,
   ZoomSelID,

   UndoHistoryID,
   PlotSpectrumID,

   FloatControlToolBarID, 
   LoadEditToolBarID,
   FloatEditToolBarID,

   // Project Menu

   ImportID,
   ImportLabelsID,
   ImportMIDIID,
   ImportRawID,
   
   QuickMixID,

   AlignID,
   AlignZeroID,

   NewWaveTrackID,
   NewLabelTrackID,
   RemoveTracksID,

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
   #define CMD_ADDFUNCTION(a) {fp = &AudacityProject::a; tmpCmd.callbackFunction = fp;}
   #define CMD_ADDMENU(commandName, commandDesc, callback, nCategory, nState) { tmpCmd.commandString = commandName; tmpCmd.descriptionString = commandDesc; tmpCmd.category = nCategory; tmpCmd.state = nState; CMD_ADDFUNCTION(callback); mCommandMenuItem.Add( (CommandMenuItem *)memcpy((new CommandMenuItem), &tmpCmd, sizeof(CommandMenuItem)) ); }

   CommandMenuItem tmpCmd; // BG: Temporary command structure
   audEventFunction fp;  //Set up temporary function pointer to use for assigning keybindings

   // File menu
   CMD_ADDMENU("New\tCtrl+N", "New", OnNew, fileMenu, enabledMenu);
   CMD_ADDMENU("Open...\tCtrl+O", "Open", OnOpen, fileMenu, enabledMenu);
   CMD_ADDMENU("Close\tCtrl+W", "Close", OnClose, fileMenu, enabledMenu);
   CMD_ADDMENU("Save Project\tCtrl+S", "Save Project", OnSave, fileMenu, enabledMenu);
   CMD_ADDMENU("Save Project &As...", "Save Project As", OnSaveAs, fileMenu, enabledMenu);
   CMD_ADDMENU("Export as %s...", "Export As", OnExportMix, fileMenu, enabledMenu);
   CMD_ADDMENU("Export &Selection as %s...", "Export Selection As", OnExportSelection, fileMenu, enabledMenu);
   CMD_ADDMENU("Export as %s...", "Export As (lossy)", OnExportLossyMix, fileMenu, enabledMenu);
   CMD_ADDMENU("Export Selection as %s...", "Export Selection As (lossy)", OnExportLossySelection, fileMenu, enabledMenu);
   CMD_ADDMENU("Export &Labels...", "Export Labels", OnExportLabels, fileMenu, enabledMenu);
   CMD_ADDMENU("Prefrences...\tCtrl+P", "Prefrences", OnPreferences, fileMenu, enabledMenu);
   CMD_ADDMENU("E&xit", "Exit", OnExit, fileMenu, enabledMenu);

   // Edit menu
   CMD_ADDMENU("Undo\tCtrl+Z", "Undo", Undo, editMenu, enabledMenu);
   CMD_ADDMENU("Redo\tCtrl+R", "Redo", Redo, editMenu, enabledMenu);
   CMD_ADDMENU("Cut\tCtrl+X", "Cut", Cut, editMenu, enabledMenu);
   CMD_ADDMENU("Copy\tCtrl+C", "Copy", Copy, editMenu, enabledMenu);
   CMD_ADDMENU("Pase\tCtrl+V", "Paste", Paste, editMenu, enabledMenu);
   CMD_ADDMENU("Trim\tCtrl+T", "Trim", Trim, editMenu, enabledMenu);
   CMD_ADDMENU("Delete\tCtrl+K", "Delete", OnDelete, editMenu, enabledMenu);
   CMD_ADDMENU("Silence\tCtrl+L", "Silence", OnSilence, editMenu, enabledMenu);
   CMD_ADDMENU("Insert Silence...", "Insert Silence", OnInsertSilence, editMenu, enabledMenu);
   CMD_ADDMENU("Split\tCtrl+Y", "New", OnSplit, editMenu, enabledMenu);
   CMD_ADDMENU("Duplicate\tCtrl+D", "New", OnDuplicate, editMenu, enabledMenu);
   CMD_ADDMENU("Select All\tCtrl+A", "Select All", OnSelectAll, editMenu, enabledMenu);

   // View menu
   CMD_ADDMENU("Zoom In\tCtrl+1", "Zoom In", OnZoomIn, viewMenu, enabledMenu);
   CMD_ADDMENU("Zoom Normal\tCtrl+2", "Zoom Normal", OnZoomNormal, viewMenu, enabledMenu);
   CMD_ADDMENU("Zoom Out\tCtrl+3", "Zoom Out", OnZoomOut, viewMenu, enabledMenu);
   CMD_ADDMENU("Fit in Window\tCtrl+F", "Fit in Window", OnZoomFit, viewMenu, enabledMenu);
   CMD_ADDMENU("Zoom to Selection\tCtrl+E", "Zoom to Selection", OnZoomSel, viewMenu, enabledMenu);
   CMD_ADDMENU("History", "Undo History", UndoHistory, viewMenu, enabledMenu);
   CMD_ADDMENU("Plot Spectrum\tCtrl+U", "Plot Spectrum", OnPlotSpectrum, viewMenu, enabledMenu);
   CMD_ADDMENU("%s Control Toolbar", "Float Control Toolbar", OnFloatControlToolBar, viewMenu, enabledMenu);
   CMD_ADDMENU("%s Edit Toolbar", "Load Edit Toolbar", OnLoadEditToolBar, viewMenu, enabledMenu);
   CMD_ADDMENU("%s Edit Toolbar", "Float Edit Toolbar", OnFloatEditToolBar, viewMenu, enabledMenu);

   // Project menu
   CMD_ADDMENU("Import Audio...\tCtrl+I", "Import Audio", OnImport, projectMenu, enabledMenu);
   CMD_ADDMENU("Import Labels...", "Import Labels...", OnImportLabels, projectMenu, enabledMenu);
   CMD_ADDMENU("Import &MIDI...", "Import MIDI", OnImportMIDI, projectMenu, enabledMenu);
   CMD_ADDMENU("Import Raw Data...", "Import Raw Data", OnImportRaw, projectMenu, enabledMenu);
   CMD_ADDMENU("&Quick Mix", "Quick Mix", OnQuickMix, projectMenu, enabledMenu);
   CMD_ADDMENU("Align Tracks Together", "Align Tracks Together", OnAlign, projectMenu, enabledMenu);
   CMD_ADDMENU("Align with Zero", "Align with Zero", OnAlignZero, projectMenu, enabledMenu);
   CMD_ADDMENU("New &Audio Track", "New Audio Track", OnNewWaveTrack, projectMenu, enabledMenu);
   CMD_ADDMENU("New &Label Track", "New Label Track", OnNewLabelTrack, projectMenu, enabledMenu);
   CMD_ADDMENU("&Remove Track(s)", "Remove Track(s)", OnRemoveTracks, projectMenu, enabledMenu);

   // Help menu
   CMD_ADDMENU("About Audacity...", "About Audacity", OnAbout, helpMenu, enabledMenu);
   CMD_ADDMENU("Online Help...", "Online Help", OnHelp, helpMenu, enabledMenu);
   CMD_ADDMENU("Online Help Index...", "Online Help Index", OnHelpIndex, helpMenu, enabledMenu);
   CMD_ADDMENU("Search Online Help...", "Search Online Help", OnHelpSearch, helpMenu, enabledMenu);
   CMD_ADDMENU("Run Benchmark...", "Run Benchmark", OnBenchmark, helpMenu, enabledMenu);
}

#endif

#ifdef AUDACITY_MENUS_COMMANDS_ACCELL_TABLE

   #define MNU_UPDATE_ACCELL(menuClass, ID) {  if(GetCommandKeyText(ID, &mnuText)) { (menuClass->FindItem(ID))->SetText(mnuText); } }

   for(int amcat_i = 0; amcat_i < GetNumCommands(); amcat_i++)
   {
      wxMenu tempMenu;
      wxString mnuText;
      mnuText = (menuClass->FindItem(ID))->GetLabel();
      tempMenu.Append(56183, )
   }

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
