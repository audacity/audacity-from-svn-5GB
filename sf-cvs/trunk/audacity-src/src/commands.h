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
   bool             separatorPrev;
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
#ifdef USE_LIBID3TAG
   EditID3ID,
#endif
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

   FirstPluginID = 3000
};

#endif

#ifdef AUDACITY_MENUS_COMMANDS_EVENT_TABLE

{
   #define CMD_ADDFUNCTION(a) {fp = &AudacityProject::a; tmpCmd->callbackFunction = fp;}
   #define CMD_ADDMENU(commandName, commandDesc, callback, nCategory, nState) { CommandMenuItem *tmpCmd = new CommandMenuItem; tmpCmd->commandString = commandName; tmpCmd->descriptionString = commandDesc; tmpCmd->category = nCategory; tmpCmd->state = nState; tmpCmd->separatorPrev = false; CMD_ADDFUNCTION(callback); mCommandMenuItem.Add( tmpCmd ); }
   #define CMD_ADDMENUSEP(commandName, commandDesc, callback, nCategory, nState) { CommandMenuItem *tmpCmd = new CommandMenuItem; tmpCmd->commandString = commandName; tmpCmd->descriptionString = commandDesc; tmpCmd->category = nCategory; tmpCmd->state = nState; tmpCmd->separatorPrev = true; CMD_ADDFUNCTION(callback); mCommandMenuItem.Add( tmpCmd ); }

   audEventFunction fp;  //Set up temporary function pointer to use for assigning keybindings

   int format = ReadExportFormatPref();
   wxString pcmFormat = sf_header_name(format & SF_FORMAT_TYPEMASK);

   mExportString.Printf(_("&Export as %s..."), pcmFormat.c_str());
   mExportSelectionString.Printf(_("Export Selection as %s..."),
                                 pcmFormat.c_str());
   wxString lossyFormat = gPrefs->Read(_("/FileFormats/LossyExportFormat"), "MP3");
   mExportLossyString.Printf(_("Export as %s..."), lossyFormat.c_str());
   mExportSelectionLossyString.Printf(_("Export Selection as %s..."),
                                      lossyFormat.c_str());

   // File menu
   CMD_ADDMENU(_("&New\tCtrl+N"), _("New"), OnNew, fileMenu, enabledMenu);
   CMD_ADDMENU(_("&Open...\tCtrl+O"), _("Open"), OnOpen, fileMenu, enabledMenu);
   CMD_ADDMENU(_("&Close\tCtrl+W"), _("Close"), OnClose, fileMenu, enabledMenu);
   CMD_ADDMENU(_("&Save Project\tCtrl+S"), _("Save Project"), OnSave, fileMenu, enabledMenu);
   CMD_ADDMENU(_("Save Project &As..."), _("Save Project As"), OnSaveAs, fileMenu, enabledMenu);

   CMD_ADDMENUSEP(mExportString, _("Export As"), OnExportMix, fileMenu, enabledMenu);
   CMD_ADDMENU(mExportSelectionString, _("Export Selection As"), OnExportSelection, fileMenu, enabledMenu);

   CMD_ADDMENUSEP(mExportLossyString, _("Export As (lossy)"), OnExportLossyMix, fileMenu, enabledMenu);
   CMD_ADDMENU(mExportSelectionLossyString, _("Export Selection As (lossy)"), OnExportLossySelection, fileMenu, enabledMenu);

   CMD_ADDMENUSEP(_("Export &Labels..."), _("Export Labels"), OnExportLabels, fileMenu, enabledMenu);

   CMD_ADDMENUSEP(_("&Preferences...\tCtrl+P"), _("Preferences"), OnPreferences, fileMenu, enabledMenu);

   CMD_ADDMENUSEP(_("E&xit"), _("Exit"), OnExit, fileMenu, enabledMenu);

   // Edit menu
   CMD_ADDMENU(_("Undo\tCtrl+Z"), _("Undo"), Undo, editMenu, enabledMenu);
   CMD_ADDMENU(_("&Redo\tCtrl+R"), _("Redo"), Redo, editMenu, enabledMenu);

   CMD_ADDMENUSEP(_("Cut\tCtrl+X"), _("Cut"), Cut, editMenu, enabledMenu);
   CMD_ADDMENU(_("Copy\tCtrl+C"), _("Copy"), Copy, editMenu, enabledMenu);
   CMD_ADDMENU(_("Paste\tCtrl+V"), _("Paste"), Paste, editMenu, enabledMenu);
   CMD_ADDMENU(_("&Trim\tCtrl+T"), _("Trim"), Trim, editMenu, enabledMenu);

   CMD_ADDMENUSEP(_("&Delete\tCtrl+K"), _("Delete"), OnDelete, editMenu, enabledMenu);
   CMD_ADDMENU(_("&Silence\tCtrl+L"), _("Silence"), OnSilence, editMenu, enabledMenu);

   CMD_ADDMENUSEP(_("&Insert Silence..."), _("Insert Silence"), OnInsertSilence, editMenu, enabledMenu);
   CMD_ADDMENU(_("Split\tCtrl+Y"), _("New"), OnSplit, editMenu, enabledMenu);
   CMD_ADDMENU(_("D&uplicate\tCtrl+D"), _("New"), OnDuplicate, editMenu, enabledMenu);

   CMD_ADDMENUSEP(_("Select All\tCtrl+A"), _("Select All"), OnSelectAll, editMenu, enabledMenu);

   // View menu
   CMD_ADDMENU(_("Zoom &In\tCtrl+1"), _("Zoom In"), OnZoomIn, viewMenu, enabledMenu);
   CMD_ADDMENU(_("Zoom &Normal\tCtrl+2"), _("Zoom Normal"), OnZoomNormal, viewMenu, enabledMenu);
   CMD_ADDMENU(_("Zoom &Out\tCtrl+3"), _("Zoom Out"), OnZoomOut, viewMenu, enabledMenu);
   CMD_ADDMENU(_("&Fit in Window\tCtrl+F"), _("Fit in Window"), OnZoomFit, viewMenu, enabledMenu);
   CMD_ADDMENU(_("Zoom to &Selection\tCtrl+E"), _("Zoom to Selection"), OnZoomSel, viewMenu, enabledMenu);

   CMD_ADDMENUSEP(_("History"), _("Undo History"), UndoHistory, viewMenu, enabledMenu);
   CMD_ADDMENU(_("Plot Spectrum\tCtrl+U"), _("Plot Spectrum"), OnPlotSpectrum, viewMenu, enabledMenu);

#ifndef __WXMAC__
   CMD_ADDMENUSEP(_("Float Control Toolbar"), _("Float Control Toolbar"), OnFloatControlToolBar, viewMenu, enabledMenu);
   CMD_ADDMENU(_("Unload Edit Toolbar"), _("Load Edit Toolbar"), OnLoadEditToolBar, viewMenu, enabledMenu);
   CMD_ADDMENU(_("Float Edit Toolbar"), _("Float Edit Toolbar"), OnFloatEditToolBar, viewMenu, enabledMenu);
#endif

   // Project menu
   CMD_ADDMENU(_("&Import Audio...\tCtrl+I"), _("Import Audio"), OnImport, projectMenu, enabledMenu);
   CMD_ADDMENU(_("Import Labels..."), _("Import Labels..."), OnImportLabels, projectMenu, enabledMenu);
   CMD_ADDMENU(_("Import &MIDI..."), _("Import MIDI"), OnImportMIDI, projectMenu, enabledMenu);
   CMD_ADDMENU(_("Import Raw &Data..."), _("Import Raw Data"), OnImportRaw, projectMenu, enabledMenu);

#ifdef USE_LIBID3TAG
   CMD_ADDMENUSEP(_("Edit ID3 Tags..."), _("Edit ID3 Tags"), OnEditID3, projectMenu, enabledMenu);
#endif

   CMD_ADDMENUSEP(_("&Quick Mix"), _("Quick Mix"), OnQuickMix, projectMenu, enabledMenu);

   CMD_ADDMENUSEP(_("Align Tracks &Together"), _("Align Tracks Together"), OnAlign, projectMenu, enabledMenu);
   CMD_ADDMENU(_("Align with &Zero"), _("Align with Zero"), OnAlignZero, projectMenu, enabledMenu);

   CMD_ADDMENUSEP(_("New &Audio Track"), _("New Audio Track"), OnNewWaveTrack, projectMenu, enabledMenu);
   CMD_ADDMENU(_("New &Label Track"), _("New Label Track"), OnNewLabelTrack, projectMenu, enabledMenu);

   CMD_ADDMENUSEP(_("&Remove Track(s)"), _("Remove Track(s)"), OnRemoveTracks, projectMenu, enabledMenu);

   // Help menu
   CMD_ADDMENU(_("About Audacity..."), _("About Audacity"), OnAbout, helpMenu, enabledMenu);

#ifndef __WXMAC__
   CMD_ADDMENUSEP(_("Online Help..."), _("Online Help"), OnHelp, helpMenu, enabledMenu);
#else
   CMD_ADDMENU(_("Online Help..."), _("Online Help"), OnHelp, helpMenu, enabledMenu);
#endif
   CMD_ADDMENU(_("Online Help Index..."), _("Online Help Index"), OnHelpIndex, helpMenu, enabledMenu);
   CMD_ADDMENU(_("Search Online Help..."), _("Search Online Help"), OnHelpSearch, helpMenu, enabledMenu);

   CMD_ADDMENUSEP(_("Run Benchmark..."), _("Run Benchmark"), OnBenchmark, helpMenu, enabledMenu);
}

#endif

#ifdef AUDACITY_MENUS_COMMANDS_REBU

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
