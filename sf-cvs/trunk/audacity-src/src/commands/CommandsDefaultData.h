/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandsDefaultData.h

  Brian Gunlogson

  This file contains the default commands.cfg file. It also contains
  the translation strings.

**********************************************************************/

#ifdef AUDACITY_COMMANDS_DEFAULT_XML

static const char *CommandsDefaultXML[] = {
"<?xml version=\"1.0\"?>",
"<commands>",
"\t<menubar name=\"appmenu\">",
"\t\t<!-- File Menu -->",
"\t\t<menu translation=\"File\">",
"\t\t\t<item translation=\"New\" keys=\"Ctrl+N\" functions=\"OnNew\"/>",
"\t\t\t<item translation=\"Open\" keys=\"Ctrl+O\" functions=\"OnOpen\"/>",
"\t\t\t<item translation=\"Close\" keys=\"Ctrl+W\" functions=\"OnClose\"/>",
"\t\t\t<item translation=\"Save Project\" keys=\"Ctrl+S\" functions=\"OnSave\"/>",
"\t\t\t<item translation=\"Save Project As\" functions=\"OnSaveAs\"/>",
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Export As\" functions=\"OnExportMix\"/>",
"\t\t\t<item translation=\"Export Selection As\" functions=\"OnExportSelection\"/>",
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Export As lossy\" functions=\"OnExportLossyMix\"/>",
"\t\t\t<item translation=\"Export Selection As lossy\" functions=\"OnExportLossySelection\"/>",
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Export Labels\" functions=\"OnExportLabels\"/>",
#ifndef __MACOSX__
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Preferences\" keys=\"Ctrl+P\" functions=\"OnPreferences\"/>",
#endif
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Exit\" functions=\"OnExit\"/>",
"\t\t</menu>",
"\t\t<!-- Edit Menu -->",
"\t\t<menu translation=\"Edit\">",
"\t\t\t<item translation=\"Undo\" keys=\"Ctrl+Z\" functions=\"Undo\"/>",
"\t\t\t<item translation=\"Redo\" keys=\"Ctrl+R\" functions=\"Redo\"/>",
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Cut\" keys=\"Ctrl+X\" functions=\"Cut\"/>",
"\t\t\t<item translation=\"Copy\" keys=\"Ctrl+C\" functions=\"Copy\"/>",
"\t\t\t<item translation=\"Paste\" keys=\"Ctrl+V\" functions=\"Paste\"/>",
"\t\t\t<item translation=\"Trim\" keys=\"Ctrl+T\" functions=\"Trim\"/>",
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Delete\" keys=\"Ctrl+K\" functions=\"OnDelete\"/>",
"\t\t\t<item translation=\"Silence\" keys=\"Ctrl+L\" functions=\"OnSilence\"/>",
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Split\" functions=\"OnSplit\"/>",
"\t\t\t<item translation=\"Split At Labels\" keys=\"Ctrl+Y\" functions=\"OnSplitLabels\"/>",
"\t\t\t<item translation=\"Duplicate\" functions=\"OnDuplicate\"/>",
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Select All\" keys=\"Ctrl+A\" functions=\"OnSelectAll\"/>",
"\t\t\t<item translation=\"Select Start to Cursor\" functions=\"OnSelectStartCursor\"/>",
"\t\t\t<item translation=\"Select Cursor to End\" functions=\"OnSelectCursorEnd\"/>",
"\t\t</menu>",
"\t\t<!-- View Menu -->",
"\t\t<menu translation=\"View\">",
"\t\t\t<item translation=\"Zoom In\" keys=\"Ctrl+1\" functions=\"OnZoomIn\"/>",
"\t\t\t<item translation=\"Zoom Normal\" keys=\"Ctrl+2\" functions=\"OnZoomNormal\"/>",
"\t\t\t<item translation=\"Zoom Out\" keys=\"Ctrl+3\" functions=\"OnZoomOut\"/>",
"\t\t\t<item translation=\"Fit in Window\" keys=\"Ctrl+F\" functions=\"OnZoomFit\"/>",
"\t\t\t<item translation=\"Zoom to Selection\" keys=\"Ctrl+E\" functions=\"OnZoomSel\"/>",
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Undo History\" keys=\"Ctrl+U\" functions=\"UndoHistory\"/>",
"\t\t\t<item translation=\"Plot Spectrum\" functions=\"OnPlotSpectrum\"/>",
#ifndef __WXMAC__
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Float Control Toolbar\" functions=\"OnFloatControlToolBar\"/>",
"\t\t\t<item translation=\"Float Edit Toolbar\" functions=\"OnFloatEditToolBar\"/>",
#endif
"\t\t</menu>",
"\t\t<!-- Project Menu -->",
"\t\t<menu translation=\"Project\">",
"\t\t\t<item translation=\"Import Audio\" keys=\"Ctrl+I\" functions=\"OnImport\"/>",
"\t\t\t<item translation=\"Import Labels\" functions=\"OnImportLabels\"/>",
"\t\t\t<item translation=\"Import MIDI\" functions=\"OnImportMIDI\"/>",
"\t\t\t<item translation=\"Import Raw Data\" functions=\"OnImportRaw\"/>",
#ifdef USE_LIBID3TAG
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Edit ID3 Tags\" functions=\"OnEditID3\"/>",
#endif
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Quick Mix\" functions=\"OnQuickMix\"/>",
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"New Audio Track\" functions=\"OnNewWaveTrack\"/>",
"\t\t\t<item translation=\"New Label Track\" functions=\"OnNewLabelTrack\"/>",
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Remove Tracks\" functions=\"OnRemoveTracks\"/>",
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Cursor or Selection Save\" functions=\"OnSelectionSave\"/>",
"\t\t\t<item translation=\"Cursor or Selection Restore\" functions=\"OnSelectionRestore\"/>",
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Cursor to Track Start\" functions=\"OnCursorTrackStart\"/>",
"\t\t\t<item translation=\"Cursor to Track End\" functions=\"OnCursorTrackEnd\"/>",
"\t\t\t<item translation=\"Cursor to Selection Start\" functions=\"OnCursorSelStart\"/>",
"\t\t\t<item translation=\"Cursor to Selection End\" functions=\"OnCursorSelEnd\"/>",
"\t\t\t<itemseparator/>",
"\t\t\t<!-- Align Submenu -->",
"\t\t\t<submenu translation=\"Align\">",
"\t\t\t\t<item translation=\"Align Tracks Together\" functions=\"OnAlign\"/>",
"\t\t\t\t<item translation=\"Align with Zero\" functions=\"OnAlignZero\"/>",
"\t\t\t\t<item translation=\"Align with Cursor\" functions=\"OnAlignCursor\"/>",
"\t\t\t\t<item translation=\"Align with Selection Start\" functions=\"OnAlignSelStart\"/>",
"\t\t\t\t<item translation=\"Align with Selection End\" functions=\"OnAlignSelEnd\"/>",
"\t\t\t\t<item translation=\"Align End with Cursor\" functions=\"OnAlignEndCursor\"/>",
"\t\t\t\t<item translation=\"Align End with Selection Start\" functions=\"OnAlignEndSelStart\"/>",
"\t\t\t\t<item translation=\"Align End with Selection End\" functions=\"OnAlignEndSelEnd\"/>",
"\t\t\t\t<item translation=\"Align Group with Cursor\" functions=\"OnAlignGroupCursor\"/>",
"\t\t\t\t<item translation=\"Align Group with Selection Start\" functions=\"OnAlignGroupSelStart\"/>",
"\t\t\t\t<item translation=\"Align Group with Selection End\" functions=\"OnAlignGroupSelEnd\"/>",
"\t\t\t\t<item translation=\"Align Group End with Cursor\" functions=\"OnAlignGroupEndCursor\"/>",
"\t\t\t\t<item translation=\"Align Group End with Selection Start\" functions=\"OnAlignGroupEndSelStart\"/>",
"\t\t\t\t<item translation=\"Align Group End with Selection End\" functions=\"OnAlignGroupEndSelEnd\"/>",
"\t\t\t</submenu>",
"\t\t<itemseparator/>",
"\t\t<item translation=\"Add Label At Selection\" keys=\"Ctrl+B\" functions=\"OnAddLabel\"/>",
"\t\t</menu>",
"\t\t<!-- Generate Menu -->",
"\t\t<menu translation=\"Generate\">",
"\t\t\t<dynamicitems name=\"GeneratePlugins\"/>",
"\t\t</menu>",
"\t\t<!-- Effect Menu -->",
"\t\t<menu translation=\"Effect\">",
"\t\t\t<dynamicitems name=\"EffectPlugins\"/>",
"\t\t</menu>",
"\t\t<!-- Analyze Menu -->",
"\t\t<menu translation=\"Analyze\">",
"\t\t\t<dynamicitems name=\"AnalyzePlugins\"/>",
"\t\t</menu>",
"\t\t<!-- Help Menu -->",
"\t\t<menu translation=\"Help\">",
"\t\t\t<item translation=\"About Audacity\" functions=\"OnAbout\"/>",
"\t\t\t<itemseparator/>",
   //STM: There may be an error here: This platform distinction may not be needed.
#ifndef __WXMAC__
"\t\t\t<item translation=\"Online Help\" functions=\"OnHelp\"/>",
#else
"\t\t\t<item translation=\"Online Help\" functions=\"OnHelp\"/>",
#endif
"\t\t\t<item translation=\"Online Help Index\" functions=\"OnHelpIndex\"/>",
"\t\t\t<item translation=\"Search Online Help\" functions=\"OnHelpSearch\"/>",
"\t\t\t<itemseparator/>",
"\t\t\t<item translation=\"Run Benchmark\" functions=\"OnBenchmark\"/>",
"\t\t</menu>",
"\t</menubar>",
"</commands>",
"//EOF//"
};

#endif

#ifdef AUDACITY_COMMANDS_TRANSLATION_STRINGS

   #define COMMANDS_ADDTRANSLATIONSTRING(tName, utName) { CommandsTranslationItem *tmpTranslation = new CommandsTranslationItem; tmpTranslation->translatedName = tName; tmpTranslation->untranslatedName = utName; mCommandsTranslationArray.Add(tmpTranslation); };

   //BG: DO NOT mark the last item in each macro for translation

   int format = ReadExportFormatPref();
   wxString pcmFormat = sf_header_shortname(format & SF_FORMAT_TYPEMASK);
   wxString lossyFormat = gPrefs->Read("/FileFormats/LossyExportFormat", "MP3");

   // File menu
   COMMANDS_ADDTRANSLATIONSTRING(_("&File"), "File");

   COMMANDS_ADDTRANSLATIONSTRING(_("&New"), "New");
   COMMANDS_ADDTRANSLATIONSTRING(_("&Open..."), "Open");
   COMMANDS_ADDTRANSLATIONSTRING(_("&Close"), "Close");
   COMMANDS_ADDTRANSLATIONSTRING(_("&Save Project"), "Save Project");
   COMMANDS_ADDTRANSLATIONSTRING(_("Save Project &As..."), "Save Project As");

   COMMANDS_ADDTRANSLATIONSTRING(wxString::Format(_("&Export as %s..."), pcmFormat.c_str()), "Export As");
   COMMANDS_ADDTRANSLATIONSTRING(wxString::Format(_("Export Selection as %s..."), pcmFormat.c_str()), "Export Selection As");

   COMMANDS_ADDTRANSLATIONSTRING(wxString::Format(_("Export as %s..."), lossyFormat.c_str()), "Export As lossy");
   COMMANDS_ADDTRANSLATIONSTRING(wxString::Format(_("Export Selection as %s..."), lossyFormat.c_str()), "Export Selection As lossy");

   COMMANDS_ADDTRANSLATIONSTRING(_("Export &Labels..."), "Export Labels");

   COMMANDS_ADDTRANSLATIONSTRING(_("&Preferences..."), "Preferences");

   COMMANDS_ADDTRANSLATIONSTRING(_("E&xit"), "Exit");

   // Edit menu
   COMMANDS_ADDTRANSLATIONSTRING(_("&Edit"), "Edit");

   COMMANDS_ADDTRANSLATIONSTRING(_("Undo"), "Undo");
   COMMANDS_ADDTRANSLATIONSTRING(_("&Redo"), "Redo");

   COMMANDS_ADDTRANSLATIONSTRING(_("Cut"), "Cut");
   COMMANDS_ADDTRANSLATIONSTRING(_("Copy"), "Copy");
   COMMANDS_ADDTRANSLATIONSTRING(_("Paste"), "Paste");
   COMMANDS_ADDTRANSLATIONSTRING(_("&Trim"), "Trim");

   COMMANDS_ADDTRANSLATIONSTRING(_("&Delete"), "Delete");
   COMMANDS_ADDTRANSLATIONSTRING(_("&Silence"), "Silence");

   COMMANDS_ADDTRANSLATIONSTRING(_("Split"), "Split");
   COMMANDS_ADDTRANSLATIONSTRING(_("Split At Labels"), "Split At Labels");
   COMMANDS_ADDTRANSLATIONSTRING(_("D&uplicate"), "Duplicate");

   COMMANDS_ADDTRANSLATIONSTRING(_("Select All"), "Select All");
   COMMANDS_ADDTRANSLATIONSTRING(_("Select Start to Cursor"), "Select Start to Cursor");
   COMMANDS_ADDTRANSLATIONSTRING(_("Select Cursor to End"), "Select Cursor to End");

   // View menu
   COMMANDS_ADDTRANSLATIONSTRING(_("&View"), "View");

   COMMANDS_ADDTRANSLATIONSTRING(_("Zoom &In"), "Zoom In");
   COMMANDS_ADDTRANSLATIONSTRING(_("Zoom &Normal"), "Zoom Normal");
   COMMANDS_ADDTRANSLATIONSTRING(_("Zoom &Out"), "Zoom Out");
   COMMANDS_ADDTRANSLATIONSTRING(_("&Fit in Window"), "Fit in Window");
   COMMANDS_ADDTRANSLATIONSTRING(_("Zoom to &Selection"), "Zoom to Selection");

   COMMANDS_ADDTRANSLATIONSTRING(_("History"), "Undo History");
   COMMANDS_ADDTRANSLATIONSTRING(_("Plot Spectrum"), "Plot Spectrum");

   COMMANDS_ADDTRANSLATIONSTRING(_("Float Control Toolbar"), "Float Control Toolbar");
   COMMANDS_ADDTRANSLATIONSTRING(_("Float Edit Toolbar"), "Float Edit Toolbar");

   // Project menu
   COMMANDS_ADDTRANSLATIONSTRING(_("&Project"), "Project");

   COMMANDS_ADDTRANSLATIONSTRING(_("&Import Audio..."), "Import Audio");
   COMMANDS_ADDTRANSLATIONSTRING(_("Import Labels..."), "Import Labels");
   COMMANDS_ADDTRANSLATIONSTRING(_("Import &MIDI..."), "Import MIDI");
   COMMANDS_ADDTRANSLATIONSTRING(_("Import Raw &Data..."), "Import Raw Data");

   COMMANDS_ADDTRANSLATIONSTRING(_("Edit ID3 Tags..."), "Edit ID3 Tags");

   COMMANDS_ADDTRANSLATIONSTRING(_("&Quick Mix"), "Quick Mix");

   COMMANDS_ADDTRANSLATIONSTRING(_("New &Audio Track"), "New Audio Track");
   COMMANDS_ADDTRANSLATIONSTRING(_("New &Label Track"), "New Label Track");

   COMMANDS_ADDTRANSLATIONSTRING(_("&Remove Track(s)"), "Remove Tracks");

   COMMANDS_ADDTRANSLATIONSTRING(_("Cursor or Selection Save"), "Cursor or Selection Save");
   COMMANDS_ADDTRANSLATIONSTRING(_("Cursor or Selection Restore"), "Cursor or Selection Restore");

   COMMANDS_ADDTRANSLATIONSTRING(_("Cursor to Track Start"), "Cursor to Track Start");
   COMMANDS_ADDTRANSLATIONSTRING(_("Cursor to Track End"), "Cursor to Track End");
   COMMANDS_ADDTRANSLATIONSTRING(_("Cursor to Selection Start"), "Cursor to Selection Start");
   COMMANDS_ADDTRANSLATIONSTRING(_("Cursor to Selection End"), "Cursor to Selection End");

   COMMANDS_ADDTRANSLATIONSTRING(_("Align..."), "Align");

   COMMANDS_ADDTRANSLATIONSTRING(_("Align Tracks &Together"), "Align Tracks Together");
   COMMANDS_ADDTRANSLATIONSTRING(_("Align with &Zero"), "Align with Zero");
   COMMANDS_ADDTRANSLATIONSTRING(_("Align with &Cursor"), "Align with Cursor");
   COMMANDS_ADDTRANSLATIONSTRING(_("Align with Selection &Start"), "Align with Selection Start");
   COMMANDS_ADDTRANSLATIONSTRING(_("Align with Selection &End"), "Align with Selection End");
   COMMANDS_ADDTRANSLATIONSTRING(_("Align End with Cursor"), "Align End with Cursor");
   COMMANDS_ADDTRANSLATIONSTRING(_("Align End with Selection Start"), "Align End with Selection Start");
   COMMANDS_ADDTRANSLATIONSTRING(_("Align End with Selection End"), "Align End with Selection End");
   COMMANDS_ADDTRANSLATIONSTRING(_("Align Group with Cursor"), "Align Group with Cursor");
   COMMANDS_ADDTRANSLATIONSTRING(_("Align Group with Selection Start"), "Align Group with Selection Start");
   COMMANDS_ADDTRANSLATIONSTRING(_("Align Group with Selection End"), "Align Group with Selection End");
   COMMANDS_ADDTRANSLATIONSTRING(_("Align Group End with Cursor"), "Align Group End with Cursor");
   COMMANDS_ADDTRANSLATIONSTRING(_("Align Group End with Selection Start"), "Align Group End with Selection Start");
   COMMANDS_ADDTRANSLATIONSTRING(_("Align Group End with Selection End"), "Align Group End with Selection End");

   COMMANDS_ADDTRANSLATIONSTRING(_("Add Label At Selection"), "Add Label At Selection");

   // Generate menu
   COMMANDS_ADDTRANSLATIONSTRING(_("&Generate"), "Generate");

   // Effect menu
   COMMANDS_ADDTRANSLATIONSTRING(_("Effec&t"), "Effect");

   // Analyze menu
   COMMANDS_ADDTRANSLATIONSTRING(_("&Analyze"), "Analyze");

   // Help menu
   COMMANDS_ADDTRANSLATIONSTRING(_("&Help"), "Help");

   COMMANDS_ADDTRANSLATIONSTRING(_("About Audacity..."), "About Audacity");
   COMMANDS_ADDTRANSLATIONSTRING(_("Online Help..."), "Online Help");
   COMMANDS_ADDTRANSLATIONSTRING(_("Online Help Index..."), "Online Help Index");
   COMMANDS_ADDTRANSLATIONSTRING(_("Search Online Help..."), "Search Online Help");

   COMMANDS_ADDTRANSLATIONSTRING(_("Run Benchmark..."), "Run Benchmark");

#endif
