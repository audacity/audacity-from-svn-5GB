// BG: Generate an array of command names, and their corresponding functions
// BG: Included inside of Menu.cpp

// BG: This is inside a function. It is included to make it easier to edit

#ifdef AUDACITY_MENUS_COMMANDS_METHODS

   CommandMenuItemArray mCommandMenuItem;

#endif

#ifdef AUDACITY_MENUS_COMMANDS_GLOBALS

#ifndef AMGC_DEFINED_H
#define AMGC_DEFINED_H

   //This defines the  function pointer
   // BG: Yes. For now, this seems like the best place to put it.

   typedef void (AudacityProject::*audEventFunction)(wxEvent&);

   enum menuState {disabledMenu = 0, enabledMenu};

   enum menuCategory {fileMenu = 0, editMenu, viewMenu, projectMenu,
                      insertMenu, effectMenu, analyzeMenu, helpMenu,
                      numMenus};

   enum menuType { typeSeparator=0, typeNormal, typeCheckItem, typeRadioItem};
   // BG: This is the structure that holds information about individual command items.

   struct CommandMenuItem
   {
      wxString         commandString;
      wxString         descriptionString;
      audEventFunction callbackFunction;
      menuCategory     category;
      menuState        state;
      menuType         type;
      wxString         comboStrings;
   };

   WX_DEFINE_ARRAY(CommandMenuItem *, CommandMenuItemArray);


// STM: Separators are numbered elements of menus, creating a slight complication.
// So, for each menu item that follows a separator, skip and index.
//
   enum {
      MenuBaseID = 1100,

      // File Menu

      NewID = MenuBaseID,
      OpenID,
      CloseID,
      SaveID,
      SaveAsID,
      /*DUMMY SEPARATOR*/
      ExportMixID=SaveAsID + 2,
      ExportSelectionID,
      /*DUMMY SEPARATOR*/
      ExportLossyMixID=ExportSelectionID + 2,
      ExportLossySelectionID,
      /*DUMMY SEPARATOR*/
      ExportLabelsID = ExportLossySelectionID + 2,
      /*DUMMY SEPARATOR*/
      PreferencesID = ExportLabelsID + 2,
      /*DUMMY SEPARATOR*/
      ExitID = PreferencesID + 2,

      // Edit Menu

      UndoID,
      RedoID,
      /*DUMMY SEPARATOR*/
      CutID = RedoID + 2,
      CopyID,
      PasteID,
      TrimID,
      /*DUMMY SEPARATOR*/
      DeleteID = TrimID + 2,
      SilenceID,
      /*DUMMY SEPARATOR*/
      InsertSilenceID = SilenceID + 2,
      SplitID,
      SplitLabelsID,
      DuplicateID,
      /*DUMMY SEPARATOR*/
      SelectAllID = DuplicateID + 2,

      // View Menu

      ZoomInID,
      ZoomNormalID,
      ZoomOutID,
      ZoomFitID,
      ZoomSelID,

      /*DUMMY SEPARATOR*/
      UndoHistoryID = ZoomSelID + 2,
      PlotSpectrumID,

#ifndef __WXMAC__
      /*DUMMY SEPARATOR*/
      FloatControlToolBarID = PlotSpectrumID + 2, 
      FloatEditToolBarID,
#endif

      // Project Menu

      ImportID,
      ImportLabelsID,
      ImportMIDIID,
      ImportRawID,

#ifdef USE_LIBID3TAG
       /*DUMMY SEPARATOR*/
      EditID3ID = ImportRawID + 2,
#endif

      SeparatorDummy1,  ///*DUMMY SEPARATOR: necessary because of above conditional compilation*/
      QuickMixID,
      /*DUMMY SEPARATOR*/
      AlignID = QuickMixID + 2,
      AlignZeroID,
      //*DUMMY SEPARATOR*/
      NewWaveTrackID = AlignZeroID + 2,
      NewLabelTrackID,
      /*DUMMY SEPARATOR*/
      RemoveTracksID = NewLabelTrackID + 2,

      // Help Menu
      AboutID ,
      /*DUMMY SEPARATOR*/
      HelpID = AboutID + 2,
      HelpIndexID,
      HelpSearchID,
      //*DUMMY SEPARATOR*/
      BenchmarkID = HelpSearchID + 2,

      // Effect Menu

      FirstEffectID = 2000,

      FirstPluginSubMenuID = 4000

   };

#endif

#endif

#ifdef AUDACITY_MENUS_COMMANDS_EVENT_TABLE

{
   #define CMD_ADDFUNCTION(a) {fp = &AudacityProject::a; tmpCmd->callbackFunction = fp;}
   #define CMD_ADDMENU(commandName, commandDesc, callback, nCategory, nState)         { CommandMenuItem *tmpCmd = new CommandMenuItem; tmpCmd->commandString = commandName; tmpCmd->descriptionString = commandDesc; tmpCmd->category = nCategory; tmpCmd->state = nState; tmpCmd->type=typeNormal;    CMD_ADDFUNCTION(callback); mCommandMenuItem.Add( tmpCmd ); }
   #define CMD_ADDMENU_CHECKED(commandName, commandDesc, callback, nCategory, nState) { CommandMenuItem *tmpCmd = new CommandMenuItem; tmpCmd->commandString = commandName; tmpCmd->descriptionString = commandDesc; tmpCmd->category = nCategory; tmpCmd->state = nState; tmpCmd->type=typeCheckItem; CMD_ADDFUNCTION(callback); mCommandMenuItem.Add( tmpCmd ); }
   #define CMD_ADDMENU_SEP(nCategory){ CommandMenuItem *tmpCmd = new CommandMenuItem;  tmpCmd->type=typeSeparator;  tmpCmd->category = nCategory;  mCommandMenuItem.Add( tmpCmd ); }

   audEventFunction fp;  //Set up temporary function pointer to use for assigning keybindings

   int format = ReadExportFormatPref();
   wxString pcmFormat = sf_header_name(format & SF_FORMAT_TYPEMASK);

   mExportString.Printf(_("&Export as %s..."), pcmFormat.c_str());
   mExportSelectionString.Printf(_("Export Selection as %s..."),
                                 pcmFormat.c_str());
   wxString lossyFormat = gPrefs->Read("/FileFormats/LossyExportFormat", "MP3");
   mExportLossyString.Printf(_("Export as %s..."), lossyFormat.c_str());
   mExportSelectionLossyString.Printf(_("Export Selection as %s..."),
                                      lossyFormat.c_str());

   // File menu
   CMD_ADDMENU(_("&New"), _("New"), OnNew, fileMenu, enabledMenu);
   CMD_ADDMENU(_("&Open..."), _("Open"), OnOpen, fileMenu, enabledMenu);
   CMD_ADDMENU(_("&Close"), _("Close"), OnClose, fileMenu, enabledMenu);
   CMD_ADDMENU(_("&Save Project"), _("Save Project"), OnSave, fileMenu, enabledMenu);
   CMD_ADDMENU(_("Save Project &As..."), _("Save Project As"), OnSaveAs, fileMenu, enabledMenu);

   CMD_ADDMENU_SEP(fileMenu);
   CMD_ADDMENU(mExportString, _("Export As"), OnExportMix, fileMenu, enabledMenu);
   CMD_ADDMENU(mExportSelectionString, _("Export Selection As"), OnExportSelection, fileMenu, enabledMenu);

   CMD_ADDMENU_SEP(fileMenu);
   CMD_ADDMENU(mExportLossyString, _("Export As (lossy)"), OnExportLossyMix, fileMenu, enabledMenu);
   CMD_ADDMENU(mExportSelectionLossyString, _("Export Selection As (lossy)"), OnExportLossySelection, fileMenu, enabledMenu);

   CMD_ADDMENU_SEP(fileMenu);
   CMD_ADDMENU(_("Export &Labels..."), _("Export Labels"), OnExportLabels, fileMenu, enabledMenu);

   CMD_ADDMENU_SEP(fileMenu);
   CMD_ADDMENU(_("&Preferences..."), _("Preferences"), OnPreferences, fileMenu, enabledMenu);

   CMD_ADDMENU_SEP(fileMenu);
   CMD_ADDMENU(_("E&xit"), _("Exit"), OnExit, fileMenu, enabledMenu);

   // Edit menu
   CMD_ADDMENU(_("Undo"), _("Undo"), Undo, editMenu, enabledMenu);
   CMD_ADDMENU(_("&Redo"), _("Redo"), Redo, editMenu, enabledMenu);

   CMD_ADDMENU_SEP(editMenu);
   CMD_ADDMENU(_("Cut"), _("Cut"), Cut, editMenu, enabledMenu);
   CMD_ADDMENU(_("Copy"), _("Copy"), Copy, editMenu, enabledMenu);
   CMD_ADDMENU(_("Paste"), _("Paste"), Paste, editMenu, enabledMenu);
   CMD_ADDMENU(_("&Trim"), _("Trim"), Trim, editMenu, enabledMenu);

   CMD_ADDMENU_SEP(editMenu);
   CMD_ADDMENU(_("&Delete"), _("Delete"), OnDelete, editMenu, enabledMenu);
   CMD_ADDMENU(_("&Silence"), _("Silence"), OnSilence, editMenu, enabledMenu);

   CMD_ADDMENU_SEP(editMenu);
   CMD_ADDMENU(_("&Insert Silence..."), _("Insert Silence"), OnInsertSilence, editMenu, enabledMenu);
   CMD_ADDMENU(_("Split"), _("Split"), OnSplit, editMenu, enabledMenu);
   CMD_ADDMENU(_("Split At Labels"), _("Split At Labels"), OnSplitLabels, editMenu, enabledMenu);
   CMD_ADDMENU(_("D&uplicate"), _("Duplicate"), OnDuplicate, editMenu, enabledMenu);

   CMD_ADDMENU_SEP(editMenu);
   CMD_ADDMENU(_("Select All"), _("Select All"), OnSelectAll, editMenu, enabledMenu);

   // View menu
   CMD_ADDMENU(_("Zoom &In"), _("Zoom In"), OnZoomIn, viewMenu, enabledMenu);
   CMD_ADDMENU(_("Zoom &Normal"), _("Zoom Normal"), OnZoomNormal, viewMenu, enabledMenu);
   CMD_ADDMENU(_("Zoom &Out"), _("Zoom Out"), OnZoomOut, viewMenu, enabledMenu);
   CMD_ADDMENU(_("&Fit in Window"), _("Fit in Window"), OnZoomFit, viewMenu, enabledMenu);
   CMD_ADDMENU(_("Zoom to &Selection"), _("Zoom to Selection"), OnZoomSel, viewMenu, enabledMenu);

   CMD_ADDMENU_SEP(viewMenu);
   CMD_ADDMENU(_("History"), _("Undo History"), UndoHistory, viewMenu, enabledMenu);
   CMD_ADDMENU(_("Plot Spectrum"), _("Plot Spectrum"), OnPlotSpectrum, viewMenu, enabledMenu);

#ifndef __WXMAC__
   CMD_ADDMENU_SEP(viewMenu);
   CMD_ADDMENU(_("Float Control Toolbar"), _("Float Control Toolbar"), OnFloatControlToolBar, viewMenu, enabledMenu);
   CMD_ADDMENU(_("Float Edit Toolbar"), _("Float Edit Toolbar"), OnFloatEditToolBar, viewMenu, enabledMenu);
#endif

   // Project menu
   CMD_ADDMENU(_("&Import Audio..."), _("Import Audio"), OnImport, projectMenu, enabledMenu);
   CMD_ADDMENU(_("Import Labels..."), _("Import Labels..."), OnImportLabels, projectMenu, enabledMenu);
   CMD_ADDMENU(_("Import &MIDI..."), _("Import MIDI"), OnImportMIDI, projectMenu, enabledMenu);
   CMD_ADDMENU(_("Import Raw &Data..."), _("Import Raw Data"), OnImportRaw, projectMenu, enabledMenu);

#ifdef USE_LIBID3TAG
   CMD_ADDMENU_SEP(projectMenu);
   CMD_ADDMENU(_("Edit ID3 Tags..."), _("Edit ID3 Tags"), OnEditID3, projectMenu, enabledMenu);
#endif
   CMD_ADDMENU_SEP(projectMenu);
   CMD_ADDMENU(_("&Quick Mix"), _("Quick Mix"), OnQuickMix, projectMenu, enabledMenu);

   CMD_ADDMENU_SEP(projectMenu);
   CMD_ADDMENU(_("Align Tracks &Together"), _("Align Tracks Together"), OnAlign, projectMenu, enabledMenu);
   CMD_ADDMENU(_("Align with &Zero"), _("Align with Zero"), OnAlignZero, projectMenu, enabledMenu);

   CMD_ADDMENU_SEP(projectMenu);
   CMD_ADDMENU(_("New &Audio Track"), _("New Audio Track"), OnNewWaveTrack, projectMenu, enabledMenu);
   CMD_ADDMENU(_("New &Label Track"), _("New Label Track"), OnNewLabelTrack, projectMenu, enabledMenu);

   CMD_ADDMENU_SEP(projectMenu);
   CMD_ADDMENU(_("&Remove Track(s)"), _("Remove Track(s)"), OnRemoveTracks, projectMenu, enabledMenu);

   // Help menu
   CMD_ADDMENU(_("About Audacity..."), _("About Audacity"), OnAbout, helpMenu, enabledMenu);
   CMD_ADDMENU_SEP(helpMenu);

   //STM: There may be an error here: This platform distinction may not be needed.
#ifndef __WXMAC__
   CMD_ADDMENU(_("Online Help..."), _("Online Help"), OnHelp, helpMenu, enabledMenu);
#else
   CMD_ADDMENU(_("Online Help..."), _("Online Help"), OnHelp, helpMenu, enabledMenu);
#endif

   CMD_ADDMENU(_("Online Help Index..."), _("Online Help Index"), OnHelpIndex, helpMenu, enabledMenu);
   CMD_ADDMENU(_("Search Online Help..."), _("Search Online Help"), OnHelpSearch, helpMenu, enabledMenu);

   CMD_ADDMENU_SEP(helpMenu);
   CMD_ADDMENU(_("Run Benchmark..."), _("Run Benchmark"), OnBenchmark, helpMenu, enabledMenu);
}

#endif

#ifdef AUDACITY_MENUS_COMMANDS_DEFAULT_SHORTCUTS

//BG: Preprogrammed defaults

gPrefs->DeleteGroup("/Keyboard");

gPrefs->Write("/Keyboard/0/Ctrl+N", (long)0);
gPrefs->Write("/Keyboard/1/Ctrl+O", (long)0);
gPrefs->Write("/Keyboard/2/Ctrl+W", (long)0);
gPrefs->Write("/Keyboard/3/Ctrl+S", (long)0);
gPrefs->Write("/Keyboard/14/Ctrl+P", (long)0);
gPrefs->Write("/Keyboard/17/Ctrl+Z", (long)0);
gPrefs->Write("/Keyboard/18/Ctrl+R", (long)0);
gPrefs->Write("/Keyboard/20/Ctrl+X", (long)0);
gPrefs->Write("/Keyboard/21/Ctrl+C", (long)0);
gPrefs->Write("/Keyboard/22/Ctrl+V", (long)0);
gPrefs->Write("/Keyboard/23/Ctrl+T", (long)0);
gPrefs->Write("/Keyboard/26/Ctrl+L", (long)0);
gPrefs->Write("/Keyboard/25/Ctrl+K", (long)0);
gPrefs->Write("/Keyboard/29/Ctrl+Y", (long)0);
gPrefs->Write("/Keyboard/31/Ctrl+D", (long)0);
gPrefs->Write("/Keyboard/33/Ctrl+A", (long)0);
gPrefs->Write("/Keyboard/34/Ctrl+1", (long)0);
gPrefs->Write("/Keyboard/35/Ctrl+2", (long)0);
gPrefs->Write("/Keyboard/36/Ctrl+3", (long)0);
gPrefs->Write("/Keyboard/37/Ctrl+F", (long)0);
gPrefs->Write("/Keyboard/38/Ctrl+E", (long)0);
gPrefs->Write("/Keyboard/41/Ctrl+U", (long)0);
gPrefs->Write("/Keyboard/45/Ctrl+I", (long)0);

#endif
