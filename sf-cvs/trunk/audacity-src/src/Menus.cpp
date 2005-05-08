/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.cpp

  Dominic Mazzoni
  Brian Gunlogson
  et. al.

  This file implements the method that creates the menu bar, plus
  all of the methods that get called when you select an item
  from a menu.

**********************************************************************/

#include "Audacity.h"

#include <math.h>

#include <wx/defs.h>
#include <wx/docview.h>
#include <wx/msgdlg.h>
#include <wx/filedlg.h>
#include <wx/textfile.h>
#include <wx/textdlg.h>
#include <wx/progdlg.h>
#include <wx/scrolbar.h>

#include "Project.h"

#include "AudacityApp.h"
#include "AudioIO.h"
#include "LabelTrack.h"
#include "import/ImportMIDI.h"
#include "import/ImportRaw.h"
#include "export/Export.h"
#include "export/ExportMultiple.h"
#include "prefs/PrefsDialog.h"
#include "HistoryWindow.h"
#include "Internat.h"
#include "FileFormats.h"
#include "FreqWindow.h"
#include "Prefs.h"
#include "Printing.h"
#include "NoteTrack.h"
#include "Tags.h"
#include "Mix.h"
#include "AboutDialog.h"
#include "Help.h"
#include "Benchmark.h"

#include "Resample.h"

enum {
   kAlignZero=0,
   kAlignCursor,
   kAlignSelStart,
   kAlignSelEnd,
   kAlignEndCursor,
   kAlignEndSelStart,
   kAlignEndSelEnd,
   kAlign
};

typedef void (AudacityProject::*audCommandFunction)();
typedef void (AudacityProject::*audCommandListFunction)(int);

class AudacityProjectCommandFunctor:public CommandFunctor
{
public:
   AudacityProjectCommandFunctor(AudacityProject *project,
                                 audCommandFunction commandFunction)
   {
      mProject = project;
      mCommandFunction = commandFunction;
      mCommandListFunction = NULL;
   }

   AudacityProjectCommandFunctor(AudacityProject *project,
                                 audCommandListFunction commandFunction)
   {
      mProject = project;
      mCommandFunction = NULL;
      mCommandListFunction = commandFunction;
   }

   virtual void operator()(int index = 0)
   {
      if (mCommandListFunction)
         (mProject->*(mCommandListFunction)) (index);
      else
         (mProject->*(mCommandFunction)) ();
   }

private:
   AudacityProject *mProject;
   audCommandFunction mCommandFunction;
   audCommandListFunction mCommandListFunction;
};

// These flags represent the majority of the states that affect
// whether or not items in menus are enabled or disabled.
enum {
   AudioIONotBusyFlag     = 0x00000001,
   TimeSelectedFlag       = 0x00000002,
   TracksSelectedFlag     = 0x00000004,
   TracksExistFlag        = 0x00000008,
   LabelTracksExistFlag   = 0x00000010,
   WaveTracksSelectedFlag = 0x00000020,
   ClipboardFlag          = 0x00000040,
   TextClipFlag           = 0x00000040, // Same as Clipboard flag for now.
   UnsavedChangesFlag     = 0x00000080,
   HasLastEffectFlag      = 0x00000100,
   UndoAvailableFlag      = 0x00000200,
   RedoAvailableFlag      = 0x00000400,
   ZoomInAvailableFlag    = 0x00000800,
   ZoomOutAvailableFlag   = 0x00001000
};

#define FN(X) new AudacityProjectCommandFunctor(this, &AudacityProject:: X )

void AudacityProject::CreateMenusAndCommands()
{
   CommandManager *c = &mCommandManager;
   EffectArray *effects;
   wxArrayString names;
   unsigned int i;

   wxMenuBar *menubar = c->AddMenuBar(wxT("appmenu"));

   //
   // File menu
   //

   c->BeginMenu(_("&File"));
   c->SetDefaultFlags(AudioIONotBusyFlag, AudioIONotBusyFlag);
   c->AddItem(wxT("New"),            _("&New\tCtrl+N"),                   FN(OnNew));
   c->SetCommandFlags(wxT("New"), 0, 0);
   c->AddItem(wxT("Open"),           _("&Open...\tCtrl+O"),               FN(OnOpen));
   c->SetCommandFlags(wxT("Open"), 0, 0);
   c->AddItem(wxT("Close"),          _("&Close\tCtrl+W"),                 FN(OnClose));
   c->AddItem(wxT("Save"),           _("&Save Project\tCtrl+S"),          FN(OnSave));
   c->SetCommandFlags(wxT("Save"),
                      AudioIONotBusyFlag | UnsavedChangesFlag,
                      AudioIONotBusyFlag | UnsavedChangesFlag);
   c->AddItem(wxT("SaveAs"),         _("Save Project &As..."),            FN(OnSaveAs));
   c->AddSeparator();

   // These 'export' strings are modified later in ModifyExportMenus(), so don't put them
   // up for i18n as it just makes more work for the translators.
   c->AddItem(wxT("Export"),         _("Export As..."),                   FN(OnExportMix));
   c->AddItem(wxT("ExportSel"),      _("Export Selection As..."),         FN(OnExportSelection));
   c->AddSeparator();
   c->AddItem(wxT("ExportMP3"),      _("Export As MP3..."),               FN(OnExportMP3Mix));
   c->AddItem(wxT("ExportMP3Sel"),   _("Export Selection As MP3..."),     FN(OnExportMP3Selection));
   c->AddSeparator();
   c->AddItem(wxT("ExportOgg"),      _("Export As Ogg Vorbis..."),        FN(OnExportOggMix));
   c->AddItem(wxT("ExportOggSel"),   _("Export Selection As Ogg Vorbis..."), FN(OnExportOggSelection));
   c->AddSeparator();
   c->AddItem(wxT("ExportLabels"),   _("Export &Labels..."),              FN(OnExportLabels));
   c->AddItem(wxT("ExportMultiple"),   _("Export &Multiple..."),              FN(OnExportMultiple));
   // Enable Export commands only when there are tracks
   c->SetCommandFlags(AudioIONotBusyFlag | TracksExistFlag,
                      AudioIONotBusyFlag | TracksExistFlag,
                      wxT("Export"), wxT("ExportMP3"), wxT("ExportOgg"), NULL);
   // Enable Export Selection commands only when there's a selection
   c->SetCommandFlags(AudioIONotBusyFlag | TimeSelectedFlag | TracksSelectedFlag,
                      AudioIONotBusyFlag | TimeSelectedFlag | TracksSelectedFlag,
                      wxT("ExportSel"), wxT("ExportMP3Sel"), wxT("ExportOggSel"), NULL);

   c->SetCommandFlags(wxT("ExportLabels"),
                      AudioIONotBusyFlag | LabelTracksExistFlag,
                      AudioIONotBusyFlag | LabelTracksExistFlag);
   c->SetCommandFlags(wxT("ExportMultiple"),
                      AudioIONotBusyFlag | TracksExistFlag,
                      AudioIONotBusyFlag | TracksExistFlag);                      
   c->AddSeparator();
   c->AddItem(wxT("PageSetup"),   _("Page Setup..."),              FN(OnPageSetup));
   c->AddItem(wxT("Print"),       _("Print..."),                   FN(OnPrint));
   c->SetCommandFlags(wxT("PageSetup"),
                      AudioIONotBusyFlag | TracksExistFlag,
                      AudioIONotBusyFlag | TracksExistFlag);
   c->SetCommandFlags(wxT("Print"),
                      AudioIONotBusyFlag | TracksExistFlag,
                      AudioIONotBusyFlag | TracksExistFlag);   
   c->AddSeparator();

   // On the Mac, the Preferences item doesn't actually go here...wxMac will pull it out
   // and put it in the Audacity menu for us based on its ID.
  #ifdef __WXMAC__
   /* i18n-hint: Mac OS X Preferences shortcut should be Ctrl+, */
   c->AddItem(wxT("Preferences"),    _("&Preferences...\tCtrl+,"),        FN(OnPreferences));
  #else
   /* i18n-hint: On Windows and Linux, the Preferences shortcut is usually Ctrl+P */
   c->AddItem(wxT("Preferences"),    _("&Preferences...\tCtrl+P"),        FN(OnPreferences));
   c->AddSeparator();
  #endif

   // Recent Files and Recent Projects menus
   wxMenu* pm = c->BeginSubMenu(_("Recent &Files..."));
   c->EndSubMenu();
   // TODO - read the number of files to store in history from preferences
   mRecentFiles = new wxFileHistory();
   mRecentFiles->UseMenu(pm);
   gPrefs->SetPath(wxT("/RecentFiles"));
   mRecentFiles->Load(*gPrefs);
   gPrefs->SetPath(wxT(".."));
   c->AddSeparator();

   // On the Mac, the Exit item doesn't actually go here...wxMac will pull it out
   // and put it in the Audacity menu for us based on its ID.
  #ifdef __WXMAC__
   c->AddItem(wxT("Exit"),           _("E&xit"),                          FN(OnExit));
   c->SetCommandFlags(wxT("Exit"), 0, 0);
  #else
   c->AddItem(wxT("Exit"),           _("E&xit"),                          FN(OnExit));
   c->SetCommandFlags(wxT("Exit"), 0, 0);
  #endif

   c->EndMenu();

   //
   // Edit Menu
   //

   c->BeginMenu(_("&Edit"));
   c->SetDefaultFlags(AudioIONotBusyFlag | TimeSelectedFlag | TracksSelectedFlag,
                      AudioIONotBusyFlag | TimeSelectedFlag | TracksSelectedFlag);

   c->AddItem(wxT("Undo"),           _("&Undo\tCtrl+Z"),                  FN(OnUndo));
   c->SetCommandFlags(wxT("Undo"),
                      AudioIONotBusyFlag | UndoAvailableFlag,
                      AudioIONotBusyFlag | UndoAvailableFlag);

   // The default shortcut key for Redo is different
   // on different platforms.

   wxString redoLabel = _("&Redo");
   #ifdef __WXMSW__
   redoLabel += wxT("\tCtrl+Y");
   #else
   redoLabel += wxT("\tCtrl+Shift+Z");
   #endif

   c->AddItem(wxT("Redo"),           redoLabel,                           FN(OnRedo));
   c->SetCommandFlags(wxT("Redo"),
                      AudioIONotBusyFlag | RedoAvailableFlag,
                      AudioIONotBusyFlag | RedoAvailableFlag);

   c->AddSeparator();
   c->AddItem(wxT("Cut"),            _("Cu&t\tCtrl+X"),                   FN(OnCut));
   c->AddItem(wxT("Copy"),           _("&Copy\tCtrl+C"),                  FN(OnCopy)); 
   c->AddItem(wxT("Paste"),          _("&Paste\tCtrl+V"),                 FN(OnPaste));
   c->SetCommandFlags(wxT("Paste"),
                      AudioIONotBusyFlag | ClipboardFlag,
                      AudioIONotBusyFlag | ClipboardFlag);
   c->AddItem(wxT("Trim"),           _("&Trim\tCtrl+T"),                  FN(OnTrim));
   c->AddSeparator();
   c->AddItem(wxT("Delete"),         _("&Delete\tCtrl+K"),                FN(OnDelete));
   c->AddItem(wxT("Silence"),        _("&Silence\tCtrl+L"),               FN(OnSilence));
   c->AddSeparator();
   c->AddItem(wxT("Split"),          _("Spl&it"),                         FN(OnSplit));
   c->SetCommandFlags(wxT("Split"),
      AudioIONotBusyFlag | WaveTracksSelectedFlag,
      AudioIONotBusyFlag | WaveTracksSelectedFlag);
   
   c->AddItem(wxT("Duplicate"),      _("D&uplicate\tCtrl+D"),             FN(OnDuplicate));

   c->AddSeparator();

   c->BeginSubMenu(_("Select..."));
   c->AddItem(wxT("SelectAll"),      _("&All\tCtrl+A"),                   FN(OnSelectAll));
   c->SetCommandFlags(wxT("SelectAll"),
                      TracksExistFlag, TracksExistFlag);
   c->AddItem(wxT("SelStartCursor"), _("Start to Cursor"),                FN(OnSelectStartCursor));
   c->AddItem(wxT("SelCursorEnd"),   _("Cursor to End"),                  FN(OnSelectCursorEnd));
   c->SetCommandFlags(TracksSelectedFlag, TracksSelectedFlag,
                      wxT("SelStartCursor"), wxT("SelCursorEnd"), NULL);

   c->EndSubMenu();
   c->AddItem(wxT("ZeroCross"),      _("Find Zero Crossings\tZ"),         FN(OnZeroCrossing));
   c->AddSeparator();
   c->AddItem(wxT("SelSave"),        _("Selection Save"),                 FN(OnSelectionSave));
   c->AddItem(wxT("SelRestore"),     _("Selection Restore"),              FN(OnSelectionRestore));
   c->SetCommandFlags(TracksExistFlag, TracksExistFlag,
                      wxT("SelSave"), wxT("SelRestore"), NULL);
   c->AddSeparator();

   c->BeginSubMenu(_("Move Cursor..."));

   c->AddItem(wxT("CursTrackStart"), _("to Track Start"),                 FN(OnCursorTrackStart));
   c->AddItem(wxT("CursTrackEnd"),   _("to Track End"),                   FN(OnCursorTrackEnd));
   c->SetCommandFlags(TracksSelectedFlag, TracksSelectedFlag,
                      wxT("CursTrackStart"), wxT("CursTrackEnd"), NULL);
   c->AddItem(wxT("CursSelStart"),   _("to Selection Start"),             FN(OnCursorSelStart));
   c->AddItem(wxT("CursSelEnd"),     _("to Selection End"),               FN(OnCursorSelEnd));
   c->SetCommandFlags(TimeSelectedFlag, TimeSelectedFlag,
                      wxT("CursSelStart"), wxT("CursSelEnd"), NULL);
                      
   c->EndSubMenu();

   c->BeginSubMenu(_("Snap-To..."));

   /* i18n-hint: Set snap-to mode on or off */
   c->AddItem(wxT("SnapOn"),         _("Snap On"),                        FN(OnSnapOn));
   /* i18n-hint: Set snap-to mode on or off */
   c->AddItem(wxT("SnapOff"),        _("Snap Off"),                       FN(OnSnapOff));

   c->SetCommandFlags(0, 0, wxT("SnapOn"), wxT("SnapOff"), NULL);

   c->EndSubMenu();

   // Alternate strings
   wxString dummy1 = _("Turn Snap-To On");
   wxString dummy2 = _("Turn Snap-To Off");
   wxString dummy3 = _("Turn Grid Snap On");
   wxString dummy4 = _("Turn Grid Snap Off");

   c->EndMenu();

   //
   // View Menu
   //

   c->BeginMenu(_("&View"));
   c->SetDefaultFlags(0, 0);
   c->AddItem(wxT("ZoomIn"),         _("Zoom &In\tCtrl+1"),               FN(OnZoomIn));
   c->SetCommandFlags(wxT("ZoomIn"), ZoomInAvailableFlag, ZoomInAvailableFlag);

   c->AddItem(wxT("ZoomNormal"),     _("Zoom &Normal\tCtrl+2"),           FN(OnZoomNormal));
   c->AddItem(wxT("ZoomOut"),        _("Zoom &Out\tCtrl+3"),              FN(OnZoomOut));
   c->SetCommandFlags(wxT("ZoomOut"), ZoomOutAvailableFlag, ZoomOutAvailableFlag);

   c->AddItem(wxT("FitInWindow"),    _("&Fit in Window\tCtrl+F"),         FN(OnZoomFit));
   c->AddItem(wxT("FitV"),           _("Fit &Vertically\tCtrl+Shift+F"),  FN(OnZoomFitV));
   c->AddItem(wxT("ZoomSel"),        _("&Zoom to Selection\tCtrl+E"),     FN(OnZoomSel));

   c->AddSeparator();
   c->AddItem(wxT("CollapseAllTracks"), _("&Collapse All Tracks\tCtrl+Shift+C"), FN(OnCollapseAllTracks));
   c->AddItem(wxT("ExpandAllTracks"), _("E&xpand All Tracks\tCtrl+Shift+X"), FN(OnExpandAllTracks));

   c->AddSeparator();
   c->AddItem(wxT("UndoHistory"),    _("&History..."),               FN(OnHistory));
   c->AddItem(wxT("PlotSpectrum"),   _("&Plot Spectrum"),                 FN(OnPlotSpectrum));
   c->SetCommandFlags(wxT("PlotSpectrum"),
                      AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag,
                      AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag);
   c->AddSeparator();
   c->AddItem(wxT("FloatMeterTB"),   _("Float Meter Toolbar"),            FN(OnFloatMeterToolBar));
   c->SetCommandFlags(wxT("FloatMeterTB"), AudioIONotBusyFlag, AudioIONotBusyFlag);

   c->EndMenu();

   //
   // Project Menu
   //

   c->BeginMenu(_("&Project"));
   c->SetDefaultFlags(AudioIONotBusyFlag, AudioIONotBusyFlag);
   c->AddItem(wxT("ImportAudio"),    _("&Import Audio...\tCtrl+I"),       FN(OnImport));
   c->AddItem(wxT("ImportLabels"),   _("Import &Labels..."),              FN(OnImportLabels));
   c->AddItem(wxT("ImportMIDI"),     _("Import &MIDI..."),                FN(OnImportMIDI));
   c->AddItem(wxT("ImportRaw"),      _("Import &Raw Data..."),            FN(OnImportRaw));
   c->AddSeparator();
   c->AddItem(wxT("EditID3"),        _("&Edit ID3 Tags..."),              FN(OnEditID3));
   c->AddSeparator();
   c->AddItem(wxT("QuickMix"),       _("&Quick Mix"),                     FN(OnQuickMix));
   c->SetCommandFlags(wxT("QuickMix"),
                      AudioIONotBusyFlag | WaveTracksSelectedFlag,
                      AudioIONotBusyFlag | WaveTracksSelectedFlag);
   c->AddSeparator();
   c->AddItem(wxT("NewAudioTrack"),  _("New &Audio Track"),               FN(OnNewWaveTrack));
   c->AddItem(wxT("NewStereoTrack"), _("New &Stereo Track"),              FN(OnNewStereoTrack));
   c->AddItem(wxT("NewLabelTrack"),  _("New La&bel Track"),               FN(OnNewLabelTrack));
   c->AddItem(wxT("NewTimeTrack"),   _("New &Time Track"),                FN(OnNewTimeTrack));
   c->AddSeparator();
   c->AddItem(wxT("RemoveTracks"),   _("Remo&ve Tracks"),                 FN(OnRemoveTracks));
   c->SetCommandFlags(wxT("RemoveTracks"),
                      AudioIONotBusyFlag | TracksSelectedFlag,
                      AudioIONotBusyFlag | TracksSelectedFlag);
   c->AddSeparator();

   wxArrayString alignLabels;
   alignLabels.Add(_("Align with &Zero"));
   alignLabels.Add(_("Align with &Cursor"));
   alignLabels.Add(_("Align with Selection &Start"));
   alignLabels.Add(_("Align with Selection &End"));
   alignLabels.Add(_("Align End with Cursor"));
   alignLabels.Add(_("Align End with Selection Start"));
   alignLabels.Add(_("Align End with Selection End"));
   alignLabels.Add(_("Align Tracks Together"));

   c->BeginSubMenu(_("Align Tracks..."));
   c->AddItemList(wxT("Align"), alignLabels, FN(OnAlign));
   c->SetCommandFlags(wxT("Align"),
                      AudioIONotBusyFlag | TracksSelectedFlag,
                      AudioIONotBusyFlag | TracksSelectedFlag);
   c->EndSubMenu();

   alignLabels.RemoveAt(7); // Can't align together and move cursor

   c->BeginSubMenu(_("Align and move cursor..."));
   c->AddItemList(wxT("AlignMove"), alignLabels, FN(OnAlignMoveSel));
   c->SetCommandFlags(wxT("AlignMove"),
                      AudioIONotBusyFlag | TracksSelectedFlag,
                      AudioIONotBusyFlag | TracksSelectedFlag);
   c->EndSubMenu();

   c->AddSeparator();   
   c->AddItem(wxT("AddLabel"),       _("Add Label At Selection\tCtrl+B"), FN(OnAddLabel));
   c->AddItem(wxT("AddLabelPlaying"),       _("Add Label At Playback Position\tCtrl+M"), FN(OnAddLabelPlaying));
   c->SetCommandFlags(wxT("AddLabel"), 0, 0);
   c->SetCommandFlags(wxT("AddLabelPlaying"), 0, AudioIONotBusyFlag);
   c->EndMenu();

   //
   // Generate, Effect & Analyze menus
   //

   c->BeginMenu(_("&Generate"));
   c->SetDefaultFlags(AudioIONotBusyFlag,
                      AudioIONotBusyFlag);

   effects = Effect::GetEffects(INSERT_EFFECT | BUILTIN_EFFECT);
   if(effects->GetCount()){
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList(wxT("Generate"), names, FN(OnGenerateEffect));
   }
   delete effects;

   effects = Effect::GetEffects(INSERT_EFFECT | PLUGIN_EFFECT);
   if (effects->GetCount()) {
      c->AddSeparator();
      names.Clear();
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList(wxT("GeneratePlugin"), names, FN(OnGeneratePlugin), true);
   }
   delete effects;
   c->EndMenu();

   c->BeginMenu(_("Effe&ct"));
   c->SetDefaultFlags(AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
                      AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag);

   c->AddItem(wxT("RepeatLastEffect"),     _("Repeat Last Effect\tCtrl+R"),    FN(OnRepeatLastEffect));
   c->SetCommandFlags(wxT("RepeatLastEffect"),
                      AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag | HasLastEffectFlag,
                      AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag | HasLastEffectFlag);
   c->AddSeparator();

   effects = Effect::GetEffects(PROCESS_EFFECT | BUILTIN_EFFECT);
   if(effects->GetCount()){
      names.Clear();
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList(wxT("Effect"), names, FN(OnProcessEffect));
   }
   delete effects;

   effects = Effect::GetEffects(PROCESS_EFFECT | PLUGIN_EFFECT);
   if (effects->GetCount()) {
      c->AddSeparator();
      names.Clear();
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList(wxT("EffectPlugin"), names, FN(OnProcessPlugin), true);
   }
   delete effects;
   c->EndMenu();

   c->BeginMenu(_("&Analyze"));

   effects = Effect::GetEffects(ANALYZE_EFFECT | BUILTIN_EFFECT);
   if(effects->GetCount()){
      names.Clear();
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList(wxT("Analyze"), names, FN(OnAnalyzeEffect));
   }
   delete effects;

   effects = Effect::GetEffects(ANALYZE_EFFECT | PLUGIN_EFFECT);
   if (effects->GetCount()) {
      c->AddSeparator();
      names.Clear();
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList(wxT("AnalyzePlugin"), names, FN(OnAnalyzePlugin), true);
   }
   delete effects;
   c->EndMenu();

   #ifdef __WXMAC__
   wxGetApp().s_macHelpMenuTitleName = _("&Help");
   #endif

   c->BeginMenu(_("&Help"));
   c->SetDefaultFlags(0, 0);
   c->AddItem(wxT("Help"),           _("&Online Help..."),             FN(OnHelp));
   c->AddSeparator();   
   c->AddItem(wxT("About"),          _("&About Audacity..."),          FN(OnAbout));

#if 1 // Benchmark is enabled in unstable builds
   c->AddSeparator();   
   c->AddItem(wxT("Benchmark"),      _("&Run Benchmark..."),           FN(OnBenchmark));
#endif 

   c->EndMenu();

   ModifyExportMenus();

   SetMenuBar(menubar);

   c->SetDefaultFlags(0, 0);
   c->AddCommand(wxT("SelectTool"),  _("Selection Tool\tF1"),          FN(OnSelectTool));
   c->AddCommand(wxT("EnvelopeTool"),_("Envelope Tool\tF2"),           FN(OnEnvelopeTool));
   c->AddCommand(wxT("DrawTool"),    _("Draw Tool\tF3"),               FN(OnDrawTool));
   c->AddCommand(wxT("ZoomTool"),    _("Zoom Tool\tF4"),               FN(OnZoomTool));
   c->AddCommand(wxT("TimeShiftTool"),_("Time Shift Tool\tF5"),        FN(OnTimeShiftTool));
   c->AddCommand(wxT("MultiTool"),   _("Multi Tool\tF6"),              FN(OnMultiTool));

   c->AddCommand(wxT("NextTool"),   _("Next Tool\tD"),                 FN(OnNextTool));
   c->AddCommand(wxT("PrevTool"),   _("Previous Tool\tA"),             FN(OnPrevTool));

   c->AddCommand(wxT("Play/Stop"),   _("Play/Stop\tSpacebar"),         FN(OnPlayStop));
   c->AddCommand(wxT("Stop"),        _("Stop\tS"),                     FN(OnStop));
   c->AddCommand(wxT("Pause"),       _("Pause\tP"),                    FN(OnPause));
   c->AddCommand(wxT("Record"),      _("Record\tR"),                   FN(OnRecord));
   
   c->AddCommand(wxT("PlayOneSec"),     _("Play One Second\t1"),       FN(OnPlayOneSecond));
   c->AddCommand(wxT("PlayToSelection"),_("Play To Selection\tB"),       FN(OnPlayToSelection));
   c->AddCommand(wxT("PlayLooped"),     _("Play Looped\tL"),           FN(OnPlayLooped));
   c->AddCommand(wxT("PlayLoopAlt"),    _("Play Looped\tShift+Spacebar"), FN(OnPlayLooped));

   c->AddCommand(wxT("SkipStart"),   _("Skip to Start\tHome"),         FN(OnSkipStart));
   c->AddCommand(wxT("SkipEnd"),     _("Skip to End\tEnd"),            FN(OnSkipEnd));

   c->AddCommand(wxT("SelStart"),    _("Selection to Start\tShift+Home"), FN(OnSelToStart));
   c->AddCommand(wxT("SelEnd"),      _("Selection to End\tShift+End"),    FN(OnSelToEnd));

   c->AddCommand(wxT("DeleteKey"),      _("DeleteKey\tBackspace"),           FN(OnDelete));
   c->SetCommandFlags(wxT("DeleteKey"),
                      AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag,
                      AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag);

   c->AddCommand(wxT("DeleteKey2"),      _("DeleteKey2\tDelete"),           FN(OnDelete));
   c->SetCommandFlags(wxT("DeleteKey2"),
                      AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag,
                      AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag);


   c->AddCommand(wxT("CycleTracks"), _("Cycle Between Tracks\tTab"), FN(OnCycleTracks));
   c->AddCommand(wxT("CursorLeft"),  _("Cursor Left\tLeft"),           FN(OnCursorLeft));
   c->AddCommand(wxT("CursorRight"), _("Cursor Right\tRight"),         FN(OnCursorRight));
   c->AddCommand(wxT("SelExtLeft"),  _("Selection Extend Left\tShift+Left"),     FN(OnSelExtendLeft));
   c->AddCommand(wxT("SelExtRight"), _("Selection Extend Right\tShift+Right"),   FN(OnSelExtendRight));
   c->AddCommand(wxT("SelCntrLeft"), _("Selection Contract Left\tCtrl+Shift+Right"),   FN(OnSelContractLeft));
   c->AddCommand(wxT("SelCntrRight"),_("Selection Contract Right\tCtrl+Shift+Left"), FN(OnSelContractRight));

   c->AddCommand(wxT("SetLeftSelection"),_("Set Left Selection at playback position\t["), FN(OnSetLeftSelection));
   c->AddCommand(wxT("SetRightSelection"),_("Set Left Selection at playback position\t]"), FN(OnSetRightSelection));

   c->AddCommand(wxT("TrackPan"), _("Change pan on first selected track\tShift+P"),FN(OnTrackPan));
   c->AddCommand(wxT("TrackGain"), _("Change gain on first selected track\tShift+G"),FN(OnTrackGain));
   c->AddCommand(wxT("TrackMenu"),_("Open menu on first selected track\tShift+M"),FN(OnTrackMenu));
   c->AddCommand(wxT("TrackMute"),_("Mute/Unmute first selected track\tShift+U"),FN(OnTrackMute));
   c->AddCommand(wxT("TrackSolo"),_("Solo/Unsolo first selected track\tShift+S"),FN(OnTrackSolo));
   c->AddCommand(wxT("TrackClose"),_("Close first selected track\tShift+C"),FN(OnTrackClose));

   mLastFlags = 0;

   mLastToolBarCheckSum = 0;

   mSel0save = 0;
   mSel1save = 0;
}

void AudacityProject::ModifyExportMenus()
{
   int format = ReadExportFormatPref();
   wxString pcmFormat = sf_header_shortname(format & SF_FORMAT_TYPEMASK);

   mCommandManager.Modify(wxT("Export"),
                          wxString::Format(_("&Export As %s..."),
                                           pcmFormat.c_str()));
   mCommandManager.Modify(wxT("ExportSel"),
                          wxString::Format(_("&Export Selection As %s..."),
                                           pcmFormat.c_str()));
}

void AudacityProject::ModifyUndoMenus()
{
   wxString desc;
   int cur = mUndoManager.GetCurrentState();

   if (mUndoManager.UndoAvailable()) {
      mUndoManager.GetShortDescription(cur, &desc);
      mCommandManager.Modify(wxT("Undo"),
                             wxString::Format(_("&Undo %s"),
                                              desc.c_str()));
      mCommandManager.Enable(wxT("Undo"), true);
   }
   else {
      mCommandManager.Modify(wxT("Undo"), 
                             wxString::Format(_("Can't Undo")));
      mCommandManager.Enable(wxT("Undo"), false);
   }

   if (mUndoManager.RedoAvailable()) {
      mUndoManager.GetShortDescription(cur+1, &desc);
      mCommandManager.Modify(wxT("Redo"),
                             wxString::Format(_("&Redo %s"),
                                              desc.c_str()));
      mCommandManager.Enable(wxT("Redo"), true);
   }
   else {
      mCommandManager.Modify(wxT("Redo"),
                             wxString::Format(_("Can't Redo")));
      mCommandManager.Enable(wxT("Redo"), false);
   }
}

void AudacityProject::RebuildMenuBar()
{
   wxMenuBar *menuBar = GetMenuBar();
   DetachMenuBar();
   delete menuBar;
   mCommandManager.PurgeData();
   delete mRecentFiles;
   mRecentFiles = NULL;
   CreateMenusAndCommands();
}

wxUint32 AudacityProject::GetUpdateFlags()
{
   // This method determines all of the flags that determine whether
   // certain menu items and commands should be enabled or disabled,
   // and returns them in a bitfield.  Note that if none of the flags
   // have changed, it's not necessary to even check for updates.
   wxUint32 flags = 0;

   if (GetAudioIOToken() == 0 ||
       !gAudioIO->IsAudioTokenActive(GetAudioIOToken()))
      flags |= AudioIONotBusyFlag;

   if (mViewInfo.sel1 > mViewInfo.sel0)
      flags |= TimeSelectedFlag;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   while (t) {
      flags |= TracksExistFlag;
      if (t->GetKind() == Track::Label)
         flags |= LabelTracksExistFlag;
      if (t->GetSelected()) {
         flags |= TracksSelectedFlag;
         if (t->GetKind() == Track::Wave && t->GetLinked() == false)
            flags |= WaveTracksSelectedFlag;
         if (t->GetKind() == Track::Label && ((LabelTrack *)t)->IsTextClipSupported())
             flags |= TextClipFlag;
      }
      t = iter.Next();
   }

   if(msClipLen > 0.0)
      flags |= ClipboardFlag;

   if (mUndoManager.UnsavedChanges())
      flags |= UnsavedChangesFlag;

   if (Effect::GetLastEffect() != NULL)
      flags |= HasLastEffectFlag;

   if (mUndoManager.UndoAvailable())
      flags |= UndoAvailableFlag;

   if (mUndoManager.RedoAvailable())
      flags |= RedoAvailableFlag;

   if (GetZoom() < gMaxZoom && (flags & TracksExistFlag))
      flags |= ZoomInAvailableFlag;

   if (GetZoom() > gMinZoom && (flags & TracksExistFlag))
      flags |= ZoomOutAvailableFlag;

   return flags;
}

#if 0
int AudacityProject::GetToolBarChecksum()
{
   //Calculate the ToolBarCheckSum (uniquely specifies state of all toolbars):
   int toolBarCheckSum = 0;
   ToolBarStub * pStub;
   for(int i=0;i<nToolBars;i++)
   {
      pStub=*gToolBarStubArray[i];
      if( pStub ){
         if ( 
            (pStub == gControlToolBarStub) || //JKC Drop this special-case test?
            pStub->GetLoadedStatus()) 
         {
            if(pStub->GetWindowedStatus())
               toolBarCheckSum += 1<<i;
         }
      }
   }
   return toolBarCheckSum;
}


const struct
{
   const wxChar * Name;
   const wxChar * DockText;
   const wxChar * FloatText;
   ToolBarStub ** ppStub;
} 
ModifyMenuData[] =
{{ 
   wxT("FloatControlTB"),
   _("Dock Control Toolbar"),
   _("Float Control Toolbar"),
   &gControlToolBarStub},
{ 
   wxT("FloatEditTB"),
   _("Dock Edit Toolbar"),
   _("Float Edit Toolbar"),
   &gEditToolBarStub},
{
   wxT("FloatMixerTB"),
   _("Dock Mixer Toolbar"),
   _("Float Mixer Toolbar"),
   &gMixerToolBarStub},
{
   wxT("FloatMeterTB"),
   _("Dock Meter Toolbar"),
   _("Float Meter Toolbar"),
   &gMeterToolBarStub},
{
   wxT("FloatTranscriptionTB"),
   _("Dock Transcription Toolbar"),
   _("Float Transcription Toolbar"),
   &gTranscriptionToolBarStub}
};



void AudacityProject::ModifyToolbarMenus()
{
   const int nItems = sizeof(ModifyMenuData)/sizeof(ModifyMenuData[0]);
   ToolBarStub * pStub;
   const wxChar * pName;
   for(int i=0;i<nItems;i++)
   {
      pStub = *ModifyMenuData[i].ppStub;
      pName = ModifyMenuData[i].Name;
      if( pStub )
      {
        // Loaded or unloaded?
        mCommandManager.Enable(pName, pStub->GetLoadedStatus());

        // Floating or docked?
        if (pStub->GetWindowedStatus())
           mCommandManager.Modify(pName, ModifyMenuData[i].DockText);
        else
           mCommandManager.Modify(pName, ModifyMenuData[i].FloatText);
      }
      else {
         mCommandManager.Enable(pName, false);
      }   
   }

}
#endif

void AudacityProject::UpdateMenus()
{
   if (this != GetActiveProject())
      return;

   if (!IsActive())
      return;

#if 0
   if (gControlToolBarStub) {
      int toolBarCheckSum = GetToolBarChecksum();

      if (mLastToolBarCheckSum != toolBarCheckSum) {
         mLastToolBarCheckSum = toolBarCheckSum;
         ModifyToolbarMenus();
      }
   }

#endif
   // Return from this function if nothing's changed since
   // the last time we were here.
   wxUint32 flags = GetUpdateFlags();
   if (flags == mLastFlags)
      return;

   mLastFlags = flags;
   mCommandManager.EnableUsingFlags(flags, 0xFFFFFFFF);

   unsigned int i;

   ToolBarStub * pStub;
   ToolBar * pToolBar;
   //Now, go through each toolbar, and and call EnableDisableButtons()
   for (i = 0; i < mToolBarArray.GetCount(); i++) {
      mToolBarArray[i]->EnableDisableButtons();
   }

   //Now, do the same thing for the (possibly invisible) floating toolbars
   for(i=0;i<nToolBars;i++)
   {
      pStub=*gToolBarStubArray[i];
      if( pStub )
      {
         pToolBar = pStub->GetToolBar();
         if( pToolBar )
            pToolBar->EnableDisableButtons();
      }
   }
}

//
// Tool selection commands
//

void AudacityProject::OnSelectTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool(selectTool, true);
}

void AudacityProject::OnZoomTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool(zoomTool, true);
}

void AudacityProject::OnEnvelopeTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool(envelopeTool, true);
}

void AudacityProject::OnTimeShiftTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool(slideTool, true);
}

void AudacityProject::OnDrawTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool(drawTool, true);
}

void AudacityProject::OnMultiTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool(multiTool, true);
}


void AudacityProject::OnNextTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool((toolbar->GetCurrentTool()+1)%numTools, true);
}

void AudacityProject::OnPrevTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool((toolbar->GetCurrentTool()+(numTools-1))%numTools, true);
}


//
// Audio I/O Commands
//

// TODO: Should all these functions which involve
// the toolbar actually move into ControlToolBar?

/// MakeReadyToPlay stops whatever is currently playing 
/// and pops the play button up.  Then, if nothing is now
/// playing, it pushes the play button down and enables
/// the stop button.
bool AudacityProject::MakeReadyToPlay()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   // If this project is playing, stop playing
   if (gAudioIO->IsStreamActive(GetAudioIOToken())) {
      toolbar->SetPlay(false);        //Pops
      toolbar->SetStop(true);         //Pushes stop down
      toolbar->OnStop(evt);

      ::wxMilliSleep(100);
   }

   // If it didn't stop playing quickly, or if some other
   // project is playing, return
   if (gAudioIO->IsBusy())
      return false;

   toolbar->SetPlay(true);
   toolbar->SetStop(false);

   return true;
}

void AudacityProject::OnPlayOneSecond()
{
   if( !MakeReadyToPlay() )
      return;

   ControlToolBar *toolbar = GetControlToolBar();
   double pos = mTrackPanel->GetMostRecentXPos();
   mLastPlayMode = oneSecondPlay;
   toolbar->PlayPlayRegion(pos - 0.5, pos + 0.5);
}


/// The idea for this function (and first implementation)
/// was from Juhana Sadeharju.  The function plays the 
/// sound between the current mouse position and the 
/// nearest selection boundary.  This gives four possible 
/// play regions depending on where the current mouse 
/// position is relative to the left and right boundaries 
/// of the selection region.
void AudacityProject::OnPlayToSelection()
{
   if( !MakeReadyToPlay() )
      return;

   ControlToolBar *toolbar = GetControlToolBar();
   double pos = mTrackPanel->GetMostRecentXPos();

   double t0,t1;
   // check region between pointer and the nearest selection edge
   if (fabs(pos - mViewInfo.sel0) < fabs(pos - mViewInfo.sel1)) {
      t0 = t1 = mViewInfo.sel0;
   } else {
      t0 = t1 = mViewInfo.sel1;
   }
   if( pos < t1) 
      t0=pos;
   else
      t1=pos;

   // JKC: oneSecondPlay mode disables auto scrolling
   // On balance I think we should always do this in this function
   // since you are typically interested in the sound EXACTLY 
   // where the cursor is.
   // TODO: have 'playing attributes' such as 'with_autoscroll'
   // rather than modes, since that's how we're now using the modes.
   mLastPlayMode = oneSecondPlay;

   // An alternative, commented out below, is to disable autoscroll
   // only when playing a short region, less than or equal to a second.
//   mLastPlayMode = ((t1-t0) > 1.0) ? normalPlay : oneSecondPlay;

   toolbar->PlayPlayRegion(t0, t1);
}

void AudacityProject::OnPlayLooped()
{
   if( !MakeReadyToPlay() )
      return;

   // Now play in a loop
   // Will automatically set mLastPlayMode
   ControlToolBar *toolbar = GetControlToolBar();
   toolbar->PlayCurrentRegion(true);
}

void AudacityProject::OnPlayStop()
{
   ControlToolBar *toolbar = GetControlToolBar();

   //If busy, stop playing, make sure everything is unpaused.
   if (gAudioIO->IsStreamActive(GetAudioIOToken())) {
      toolbar->SetPlay(false);        //Pops
      toolbar->SetStop(true);         //Pushes stop down
      toolbar->StopPlaying();
   }
   else if (!gAudioIO->IsBusy()) {
      //Otherwise, start playing (assuming audio I/O isn't busy)
      toolbar->SetPlay(true);
      toolbar->SetStop(false);

      // Will automatically set mLastPlayMode
      toolbar->PlayCurrentRegion(false);
   }
}

void AudacityProject::OnStop()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   if (gAudioIO->IsStreamActive())
      toolbar->OnStop(evt);
}

void AudacityProject::OnPause()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   toolbar->OnPause(evt);
}

void AudacityProject::OnRecord()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   toolbar->OnRecord(evt);
}

void AudacityProject::OnSkipStart()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   toolbar->OnRewind(evt);
}

void AudacityProject::OnSkipEnd()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   toolbar->OnFF(evt);
}

void AudacityProject::OnSelToStart()
{
   mViewInfo.sel0 = 0;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelToEnd()
{
   SkipEnd(true);
}


void AudacityProject::OnCycleTracks()
{
   mTrackPanel->OnCycleTracks();
   
}


void AudacityProject::OnCursorLeft()
{

   //if the last adjustment was very recent, we are
   //holding the key down and should move faster.
   wxLongLong curtime = ::wxGetLocalTimeMillis();
   int multiplier =1;
   if(curtime - mLastSelectionAdjustment < 50)
      multiplier=4;
   mLastSelectionAdjustment = curtime;

   if (mViewInfo.sel0 == mViewInfo.sel1) {
      mViewInfo.sel0 -= multiplier/mViewInfo.zoom;
      mViewInfo.sel1 -= multiplier/mViewInfo.zoom;
      if (mViewInfo.sel0 < 0.0)
         mViewInfo.sel0 = 0.0;
      if (mViewInfo.sel1 < 0.0)
         mViewInfo.sel1 = 0.0;
   }
   else
      mViewInfo.sel1 = mViewInfo.sel0;

   mTrackPanel->Refresh(false);   
}

void AudacityProject::OnCursorRight()
{

   //if the last adjustment was very recent, we are
   //holding the key down and should move faster.
   wxLongLong curtime = ::wxGetLocalTimeMillis();
   int multiplier =1;
   if(curtime - mLastSelectionAdjustment < 50)
      multiplier=4;
   mLastSelectionAdjustment = curtime;


   if (mViewInfo.sel0 == mViewInfo.sel1) {
      mViewInfo.sel0 += multiplier/mViewInfo.zoom;
      mViewInfo.sel1 += multiplier/mViewInfo.zoom;
   }
   else
      mViewInfo.sel0 = mViewInfo.sel1;

   mTrackPanel->Refresh(false);   
}

void AudacityProject::OnSelExtendLeft()
{
   //if the last adjustment was very recent, we are
   //holding the key down and should move faster.
   wxLongLong curtime = ::wxGetLocalTimeMillis();
   int multiplier =1;
   if(curtime - mLastSelectionAdjustment < 50)
      multiplier=4;
   mLastSelectionAdjustment = curtime;


   mViewInfo.sel0 -= multiplier/mViewInfo.zoom;
   if (mViewInfo.sel0 < 0.0)
      mViewInfo.sel0 = 0.0;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelExtendRight()
{
   //if the last adjustment was very recent, we are
   //holding the key down and should move faster.
   wxLongLong curtime = ::wxGetLocalTimeMillis();
   int multiplier =1;
   if(curtime - mLastSelectionAdjustment < 50)
      multiplier=4;
   mLastSelectionAdjustment = curtime;

   mViewInfo.sel1 += multiplier/mViewInfo.zoom;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelContractLeft()
{
   //if the last adjustment was very recent, we are
   //holding the key down and should move faster.
   wxLongLong curtime = ::wxGetLocalTimeMillis();
   int multiplier =1;
   if(curtime - mLastSelectionAdjustment < 50)
      multiplier=4;
   mLastSelectionAdjustment = curtime;


   mViewInfo.sel0 += multiplier/mViewInfo.zoom;
   if (mViewInfo.sel0 > mViewInfo.sel1)
      mViewInfo.sel0 = mViewInfo.sel1;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelContractRight()
{
   //if the last adjustment was very recent, we are
   //holding the key down and should move faster.
   wxLongLong curtime = ::wxGetLocalTimeMillis();
   int multiplier =1;
   if(curtime - mLastSelectionAdjustment < 50)
      multiplier=4;
   mLastSelectionAdjustment = curtime;


   mViewInfo.sel1 -= multiplier/mViewInfo.zoom;
   if (mViewInfo.sel1 < mViewInfo.sel0)
      mViewInfo.sel1 = mViewInfo.sel0;
   mTrackPanel->Refresh(false);
}


//this pops up a dialog which allows the left selection to be set.
//If playing/recording is happening, it sets the left selection at
//the current play position.
void AudacityProject::OnSetLeftSelection()
{
   if (GetAudioIOToken()>0 &&
       gAudioIO->IsStreamActive(GetAudioIOToken()))
      {
         double indicator = gAudioIO->GetStreamTime();
         mViewInfo.sel0 = indicator;
      }
   else
      {
         wxString curval = wxString::Format(wxT("%2.2f"),mViewInfo.sel0);
         wxTextEntryDialog * dialog = new wxTextEntryDialog(this,
                                                            _("Set Left Selection Bound"),
                                                            _("Please Enter time"),
                                                            curval,
                                                            wxCANCEL|wxOK|wxCENTRE);
         if(wxID_OK==dialog->ShowModal() )
            {
               //Get the value from the dialog
               wxString val = dialog->GetValue();
               double tmp;
               val.ToDouble(&tmp);
               mViewInfo.sel0 = tmp;
               
               //Make sure it is 'legal'
               if(mViewInfo.sel0 < 0)
                  mViewInfo.sel0 = 0;
            }
         delete dialog;
      }
   
   if(mViewInfo.sel1 < mViewInfo.sel0)
      mViewInfo.sel1 = mViewInfo.sel0;
   mTrackPanel->Refresh(false);
   
}


void AudacityProject::OnSetRightSelection()
{
   if (GetAudioIOToken()>0 &&
       gAudioIO->IsStreamActive(GetAudioIOToken())) 
      {
         double indicator = gAudioIO->GetStreamTime();
         mViewInfo.sel1 = indicator;
      }

   else
      {
         wxString curval = wxString::Format(wxT("%2.2f"),mViewInfo.sel1);
         wxTextEntryDialog * dialog =  new wxTextEntryDialog(this,_("Set Right Selection Bound"),_("Please Enter time"),
                                                      curval,wxCANCEL|wxOK|wxCENTRE);
         if(wxID_OK==dialog->ShowModal() )
            {
               //Get the value from the dialog
               wxString val = dialog->GetValue();
               double tmp;
               val.ToDouble(&tmp);
               mViewInfo.sel1=tmp; 
             
               //Make sure it is 'legal'
               if(mViewInfo.sel1 < 0)
                  mViewInfo.sel1 = 0;
            }
         delete dialog;
      }

   if(mViewInfo.sel0 >  mViewInfo.sel0)
      mViewInfo.sel0 = mViewInfo.sel1;
   mTrackPanel->Refresh(false);
   
}

void AudacityProject::OnTrackPan()
{
   mTrackPanel->OnTrackPan(mTrackPanel->GetFirstSelectedTrack());
}

void AudacityProject::OnTrackGain()
{
   mTrackPanel->OnTrackGain(mTrackPanel->GetFirstSelectedTrack());
}

void AudacityProject::OnTrackMenu()
{
   mTrackPanel->OnTrackMenu(mTrackPanel->GetFirstSelectedTrack());
}

void AudacityProject::OnTrackMute()
{
   mTrackPanel->OnTrackMute(mTrackPanel->GetFirstSelectedTrack(),false);
}
void AudacityProject::OnTrackSolo()
{
   mTrackPanel->OnTrackSolo(mTrackPanel->GetFirstSelectedTrack(),false);
}
void AudacityProject::OnTrackClose()
{
   mTrackPanel->OnTrackClose(mTrackPanel->GetFirstSelectedTrack());
}



double AudacityProject::NearestZeroCrossing(double t0)
{
   int windowSize = (int)(GetRate() / 100);
   float *dist = new float[windowSize];
   int i, j;

   for(i=0; i<windowSize; i++)
      dist[i] = 0.0;

   TrackListIterator iter(mTracks);
   Track *track = iter.First();
   while (track) {
      if (!track->GetSelected() || track->GetKind() != (Track::Wave)) {
         track = iter.Next();
         continue;
      }
      WaveTrack *one = (WaveTrack *)track;
      int oneWindowSize = (int)(one->GetRate() / 100);
      float *oneDist = new float[oneWindowSize];
      longSampleCount s = one->TimeToLongSamples(t0);
      one->Get((samplePtr)oneDist, floatSample,
               s - oneWindowSize/2, oneWindowSize);

      // Start by penalizing downward motion.  We prefer upward
      // zero crossings.
      if (oneDist[1] - oneDist[0] < 0)
         oneDist[0] = oneDist[0]*6 + 0.3;
      for(i=1; i<oneWindowSize; i++)
         if (oneDist[i] - oneDist[i-1] < 0)
            oneDist[i] = oneDist[i]*6 + 0.3;

      for(i=0; i<oneWindowSize; i++)
         oneDist[i] = fabs(oneDist[i]);

      for(i=0; i<windowSize; i++) {
         if (windowSize != oneWindowSize)
            j = i * (oneWindowSize-1) / (windowSize-1);
         else
            j = i;

         dist[i] += oneDist[j];
      }
         
      track = iter.Next();
   }

   int argmin = windowSize/2; // Start at default pos in center
   float min = dist[argmin];
   for(i=0; i<windowSize; i++) {
      if (dist[i] < min) {
         argmin = i;
         min = dist[i];
      }
   }

   return t0 + (argmin - windowSize/2)/GetRate();
}

void AudacityProject::OnZeroCrossing()
{
   if (mViewInfo.sel0 == mViewInfo.sel1)
      mViewInfo.sel0 = mViewInfo.sel1 =
         NearestZeroCrossing(mViewInfo.sel0);
   else {
      mViewInfo.sel0 = NearestZeroCrossing(mViewInfo.sel0);
      mViewInfo.sel1 = NearestZeroCrossing(mViewInfo.sel1);

      if (mViewInfo.sel1 < mViewInfo.sel0)
         mViewInfo.sel1 = mViewInfo.sel0;
   }

   mTrackPanel->Refresh(false);
}

//
// Effect Menus
//

void AudacityProject::OnEffect(int type, int index)
{
   EffectArray *effects;
   Effect *f = NULL;

   effects = Effect::GetEffects(type);
   f = (*effects)[index];
   delete effects;

   if (!f)
      return;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   //double prevEndTime = mTracks->GetEndTime();
   int count = 0;
   
   while (t) {
      if (t->GetSelected() && t->GetKind() == (Track::Wave))
         count++;
      t = iter.Next();
   }
   
   if (count == 0) {
      // No tracks were selected...
      if (f->GetEffectFlags() & INSERT_EFFECT) {
         // Create a new track for the generated audio...
         WaveTrack *newTrack = mTrackFactory->NewWaveTrack();
         mTracks->Add(newTrack);
         newTrack->SetSelected(true);
      }
      else {
         wxMessageBox(_("You must select a track first."));
         return;
      }
   }
   
   if (f->DoEffect(this, type, mRate, mTracks, mTrackFactory,
                   &mViewInfo.sel0, &mViewInfo.sel1)) {
      wxString longDesc = f->GetEffectDescription();
      wxString shortDesc = f->GetEffectName();

      if (shortDesc.Length() > 3 && shortDesc.Right(3)==wxT("..."))
         shortDesc = shortDesc.Left(shortDesc.Length()-3);

      PushState(longDesc, shortDesc);
      
      //STM:
      //The following automatically re-zooms after sound was generated.
      // IMO, it was disorienting, removing to try out without re-fitting
      //if (mTracks->GetEndTime() > prevEndTime)
      //      OnZoomFit();


      FixScrollbars();

      // Only remember a successful effect, don't rmemeber insert,
      // or analyze effects.
      if ((f->GetEffectFlags() & (INSERT_EFFECT | ANALYZE_EFFECT))==0) {
         Effect::SetLastEffect( type, index, f );
         mCommandManager.Modify(wxT("RepeatLastEffect"),
            wxString::Format(_("Repeat %s\tCtrl+R"),
            shortDesc.c_str()));
      }
      
      mTrackPanel->Refresh(false);
   } else {
      // TODO: undo the effect if necessary?
   }

   
   return;
}

void AudacityProject::OnGenerateEffect(int index)
{
   OnEffect(BUILTIN_EFFECT | INSERT_EFFECT, index);
}

void AudacityProject::OnGeneratePlugin(int index)
{
   OnEffect(PLUGIN_EFFECT | INSERT_EFFECT, index);
}

void AudacityProject::OnRepeatLastEffect(int index)
{
   if( Effect::GetLastEffect()  != NULL )
   {
      // Setting the CONFIGURED_EFFECT bit prevents
      // prompting for parameters.
      OnEffect( 
         Effect::GetLastEffectType() | CONFIGURED_EFFECT,
         Effect::GetLastEffectIndex());
   }
}

void AudacityProject::OnProcessEffect(int index)
{
   OnEffect(BUILTIN_EFFECT | PROCESS_EFFECT, index);
}

void AudacityProject::OnProcessPlugin(int index)
{
   OnEffect(PLUGIN_EFFECT | PROCESS_EFFECT, index);
}

void AudacityProject::OnAnalyzeEffect(int index)
{
   OnEffect(BUILTIN_EFFECT | ANALYZE_EFFECT, index);
}

void AudacityProject::OnAnalyzePlugin(int index)
{
   OnEffect(PLUGIN_EFFECT | ANALYZE_EFFECT, index);
}

//
// File Menu
//

void AudacityProject::OnNew()
{
   CreateNewAudacityProject(gParentWindow);
}

void AudacityProject::OnOpen()
{
   ShowOpenDialog(this);
}

void AudacityProject::OnClose()
{
   Close();
}

void AudacityProject::OnSave()
{
   Save();
}

void AudacityProject::OnSaveAs()
{
   SaveAs();
}

void AudacityProject::OnExit()
{
   QuitAudacity();
}

void AudacityProject::OnExportLabels()
{
   Track *t;
   int numLabelTracks = 0;

   TrackListIterator iter(mTracks);

   t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Label)
         numLabelTracks++;
      t = iter.Next();
   }

   if (numLabelTracks == 0) {
      wxMessageBox(_("There are no label tracks to export."));
      return;
   }

   wxString fName = _("labels.txt");

   fName = wxFileSelector(_("Export Labels As:"),
                          NULL,
                          fName,
                          wxT("txt"),
                          wxT("*.txt"), wxSAVE | wxOVERWRITE_PROMPT, this);

   if (fName == wxT(""))
      return;

   // Move existing files out of the way.  Otherwise wxTextFile will
   // append to (rather than replace) the current file.

   if (wxFileExists(FILENAME(fName))) {
#ifdef __WXGTK__
      wxString safetyFileName = fName + wxT("~");
#else
      wxString safetyFileName = fName + wxT(".bak");
#endif

      if (wxFileExists(FILENAME(safetyFileName)))
         wxRemoveFile(FILENAME(safetyFileName));

      wxRename(fName, safetyFileName);
   }

   wxTextFile f(FILENAME(fName).c_str());
#ifdef __WXMAC__
   wxFile *temp = new wxFile();
   temp->Create(FILENAME(fName));
   delete temp;
#else
   f.Create();
#endif
   f.Open();
   if (!f.IsOpened()) {
      wxMessageBox(_("Couldn't write to file: ") + fName);
      return;
   }

   t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Label)
         ((LabelTrack *) t)->Export(f);

      t = iter.Next();
   }

#ifdef __WXMAC__
   f.Write(wxTextFileType_Mac);
#else
   f.Write();
#endif
   f.Close();
}

void AudacityProject::OnExportMix()
{
   ::Export(this, false, 0.0, mTracks->GetEndTime());
}

void AudacityProject::OnExportSelection()
{
   ::Export(this, true, mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnExportMP3Mix()
{
   gPrefs->Write(wxT("/FileFormats/LossyExportFormat"), wxT("MP3"));
   ::ExportLossy(this, false, 0.0, mTracks->GetEndTime());
}

void AudacityProject::OnExportMP3Selection()
{
   gPrefs->Write(wxT("/FileFormats/LossyExportFormat"), wxT("MP3"));
   ::ExportLossy(this, true, mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnExportOggMix()
{
   gPrefs->Write(wxT("/FileFormats/LossyExportFormat"), wxT("OGG"));
   ::ExportLossy(this, false, 0.0, mTracks->GetEndTime());
}

void AudacityProject::OnExportOggSelection()
{
   gPrefs->Write(wxT("/FileFormats/LossyExportFormat"), wxT("OGG"));
   ::ExportLossy(this, true, mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnExportMultiple()
{
   ::ExportMultiple(this);
}

void AudacityProject::OnPreferences()
{
   PrefsDialog dialog(this /* parent */ );
   dialog.ShowModal();
}

void AudacityProject::OnPageSetup()
{
   HandlePageSetup(this);
}

void AudacityProject::OnPrint()
{
   HandlePrint(this, GetName(), mTracks);
}

//
// Edit Menu
//

void AudacityProject::OnUndo()
{
   if (!mUndoManager.UndoAvailable()) {
      wxMessageBox(_("Nothing to undo"));
      return;
   }

   TrackList *l = mUndoManager.Undo(&mViewInfo.sel0, &mViewInfo.sel1);
   PopState(l);

   FixScrollbars();
   mTrackPanel->Refresh(false);

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();

   ModifyUndoMenus();   
}

void AudacityProject::OnRedo()
{
   if (!mUndoManager.RedoAvailable()) {
      wxMessageBox(_("Nothing to redo"));
      return;
   }

   TrackList *l = mUndoManager.Redo(&mViewInfo.sel0, &mViewInfo.sel1);
   PopState(l);

   FixScrollbars();
   mTrackPanel->Refresh(false);

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();

   ModifyUndoMenus();
}

void AudacityProject::OnHistory()
{
   if (mHistoryWindow)
      mHistoryWindow->Show(true);
   else {
      mHistoryWindow = new HistoryWindow(this, &mUndoManager);
      mHistoryWindow->Show(true);
   }
}


void AudacityProject::OnCut()
{
   TrackListIterator iter(mTracks);
   Track *n = iter.First();
   Track *dest;

   while (n) {
      if (n->GetSelected()) {
         if (n->GetKind() == Track::Label) {
            if (((LabelTrack *)n)->CutSelectedText()) {
               mTrackPanel->Refresh(false);
               return;
            }
         }
      }
      n = iter.Next();
   }
   

   ClearClipboard();
   n = iter.First();
   while (n) {
      if (n->GetSelected()) {
         dest = NULL;
         if (n->GetKind() == Track::Wave && 
             gPrefs->Read(wxT("/GUI/EnableCutLines"), (long)0))
         {
            ((WaveTrack*)n)->CutAndAddCutLine(mViewInfo.sel0, mViewInfo.sel1, &dest);
         } else
         {
            n->Cut(mViewInfo.sel0, mViewInfo.sel1, &dest);
         }
         if (dest) {
            dest->SetChannel(n->GetChannel());
            dest->SetLinked(n->GetLinked());
            dest->SetName(n->GetName());
            if (n->GetKind() == Track::Wave) {
               ((WaveTrack *)dest)->SetRate(((WaveTrack *)n)->GetRate());
            }
            msClipboard->Add(dest);
         }
      }
      n = iter.Next();
   }

   msClipLen = (mViewInfo.sel1 - mViewInfo.sel0);
   msClipProject = this;

   mViewInfo.sel1 = mViewInfo.sel0;

   PushState(_("Cut to the clipboard"), _("Cut"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}



void AudacityProject::OnCopy()
{
  
   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *dest;

   while (n) {
      if (n->GetSelected()) {
         if (n->GetKind() == Track::Label) {
            if (((LabelTrack *)n)->CopySelectedText()) {
               //mTrackPanel->Refresh(false);
               return;
            }
         }
      }
      n = iter.Next();
   }

   ClearClipboard();
   n = iter.First();
   while (n) {
      if (n->GetSelected()) {
         dest = NULL;
         n->Copy(mViewInfo.sel0, mViewInfo.sel1, &dest);
         if (dest) {
            dest->SetChannel(n->GetChannel());
            dest->SetLinked(n->GetLinked());
            dest->SetName(n->GetName());
            if (n->GetKind() == Track::Wave) {
               ((WaveTrack *)dest)->SetRate(((WaveTrack *)n)->GetRate());
            }
            msClipboard->Add(dest);
         }
      }
      n = iter.Next();
   }

   msClipLen = (mViewInfo.sel1 - mViewInfo.sel0);
   msClipProject = this;
   
   //Make sure the menus/toolbar states get updated
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnPaste()
{
   // If nothing's selected, we just insert new tracks...so first
   // check to see if anything's selected
   
   TrackListIterator iter2(mTracks);
   Track *countTrack = iter2.First();

   while (countTrack) {
      if (countTrack->GetSelected()) {
         if (countTrack->GetKind() == Track::Label) {
            if (((LabelTrack *)countTrack)->PasteSelectedText()) {
               mTrackPanel->Refresh(false);
               return;
            }
         }
      }
      countTrack = iter2.Next();
   }

   int numSelected =0;

   countTrack = iter2.First();
   while (countTrack) {
      if (countTrack->GetSelected())
         numSelected++;
      countTrack = iter2.Next();
   }
   
   if (numSelected == 0) {
      TrackListIterator clipIter(msClipboard);
      Track *c = clipIter.First();
      Track *n;

      while (c) {
         if (msClipProject != this && c->GetKind() == Track::Wave)
            ((WaveTrack *) c)->Lock();
         
         switch(c->GetKind()) {
         case Track::Wave: {
            WaveTrack *w = (WaveTrack *)c;
            n = mTrackFactory->NewWaveTrack(w->GetSampleFormat());
            ((WaveTrack *)n)->SetRate(w->GetRate());
            } break;

         case Track::Note:
            n = mTrackFactory->NewNoteTrack();
            break;

         case Track::Label:
            n = mTrackFactory->NewLabelTrack();
            break;
            
         case Track::Time:
            n = mTrackFactory->NewTimeTrack();
            break;

         default:
            c = clipIter.Next();
            continue;
         }

         n->SetLinked(c->GetLinked());
         n->SetChannel(c->GetChannel());
         n->SetName(c->GetName());

         n->Paste(0.0, c);
         mTracks->Add(n);
         n->SetSelected(true);         
         
         if (msClipProject != this && c->GetKind() == Track::Wave)
            ((WaveTrack *) c)->Unlock();
         
         c = clipIter.Next();
      }

      mViewInfo.sel0 = 0.0;
      mViewInfo.sel1 = msClipLen;
      
      PushState(_("Pasted from the clipboard"), _("Paste"));
      
      FixScrollbars();
      mTrackPanel->Refresh(false);

      return;
   }

   // Otherwise, paste into the selected tracks.

   if (mViewInfo.sel0 != mViewInfo.sel1)
      Clear();

   wxASSERT(mViewInfo.sel0 == mViewInfo.sel1);

   double tsel = mViewInfo.sel0;

   TrackListIterator iter(mTracks);
   TrackListIterator clipIter(msClipboard);

   Track *n = iter.First();
   Track *c = clipIter.First();

   while (n && c) {
      if (n->GetSelected()) {
         if (msClipProject != this && c->GetKind() == Track::Wave)
            ((WaveTrack *) c)->Lock();

         n->Paste(tsel, c);

         if (msClipProject != this && c->GetKind() == Track::Wave)
            ((WaveTrack *) c)->Unlock();

         c = clipIter.Next();
      }

      n = iter.Next();
   }

   // TODO: What if we clicked past the end of the track?

   mViewInfo.sel0 = tsel;
   mViewInfo.sel1 = tsel + msClipLen;

   PushState(_("Pasted from the clipboard"), _("Paste"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnPasteOver()
{
   if(msClipLen>0.0)
   {
      mViewInfo.sel1=mViewInfo.sel0+msClipLen;
   }
   OnPaste();

   return;
}

void AudacityProject::OnTrim()
{
   if (mViewInfo.sel0 >= mViewInfo.sel1)
      return;

   TrackListIterator iter(mTracks);
   Track *n = iter.First();

   while (n) {
      if (n->GetSelected()) {
         //Delete the section before the left selector
         n->Clear(n->GetOffset(), mViewInfo.sel0);
         if (mViewInfo.sel0 > n->GetOffset())
            n->SetOffset(mViewInfo.sel0);

         //Delete the section after the right selector
         n->Clear(mViewInfo.sel1, n->GetEndTime());
      }
      n = iter.Next();
   }

   FixScrollbars();
   mTrackPanel->Refresh(false);
   PushState(_("Trim file to selection"), _("Trim"));
}



void AudacityProject::OnDelete()
{
   Clear();
}

void AudacityProject::OnSilence()
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();

   while (n) {
      if (n->GetSelected())
         n->Silence(mViewInfo.sel0, mViewInfo.sel1);

      n = iter.Next();
   }

   PushState(wxString::
             Format(_("Silenced selected tracks for %.2f seconds at %.2f"),
                    mViewInfo.sel1 - mViewInfo.sel0, mViewInfo.sel0),
             _("Silence"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnDuplicate()
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *dest;

   TrackList newTracks;

   while (n) {
      if (n->GetSelected()) {
         dest = NULL;
         n->Copy(mViewInfo.sel0, mViewInfo.sel1, &dest);
         if (dest) {
            dest->Init(*n);
            dest->SetOffset(wxMax(mViewInfo.sel0, n->GetOffset()));
            newTracks.Add(dest);
         }
      }
      n = iter.Next();
   }

   TrackListIterator nIter(&newTracks);
   n = nIter.First();
   while (n) {
      mTracks->Add(n);
      n = nIter.Next();
   }

   PushState(_("Duplicated"), _("Duplicate"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSplit()
{
   TrackListIterator iter(mTracks);

   double sel0 = mViewInfo.sel0;
   double sel1 = mViewInfo.sel1;

   for (Track* n=iter.First(); n; n = iter.Next())
   {
      if (n->GetKind() == Track::Wave)
      {
         WaveTrack* wt = (WaveTrack*)n;
         if (wt->GetSelected()) {
            wt->SplitAt(sel0);
            if (sel0 != sel1)
               wt->SplitAt(sel1);
         }
      }
   }

   PushState(_("Split"), _("Split"));
   mTrackPanel->Refresh(false);

   /*
    * Previous (pre-multiclip) implementation of "Split" command
    * This does work only when a range is selected!
    *
   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *dest;

   TrackList newTracks;

   while (n) {
      if (n->GetSelected()) {
         double sel0 = mViewInfo.sel0;
         double sel1 = mViewInfo.sel1;

         dest = NULL;
         n->Copy(sel0, sel1, &dest);
         if (dest) {
            dest->Init(*n);
            dest->SetOffset(wxMax(sel0, n->GetOffset()));

            if (sel1 >= n->GetEndTime())
               n->Clear(sel0, sel1);
            else if (sel0 <= n->GetOffset()) {
               n->Clear(sel0, sel1);
               n->SetOffset(sel1);
            } else
               n->Silence(sel0, sel1);

            newTracks.Add(dest);
         }
      }
      n = iter.Next();
   }

   TrackListIterator nIter(&newTracks);
   n = nIter.First();
   while (n) {
      mTracks->Add(n);
      n = nIter.Next();
   }

   PushState(_("Split"), _("Split"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
   */
}

void AudacityProject::OnSplitLabels()
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *srcRight = 0;
   Track *srcLeft = 0;
   bool stereo = false;
   LabelTrack *label = 0;

   while(n) {
      if(n->GetSelected()) {
         if(n->GetKind() == Track::Wave) {
            if(n->GetLinked() == true) {
               stereo = true;
               srcLeft = n;
               srcRight  = iter.Next();
            }
            else {
               srcRight = n;
               stereo = false;
            }
         }
         else if(n->GetKind() == Track::Label)
            label = (LabelTrack*)n;  // cast necessary to call LabelTrack specific methods
      }
      n = iter.Next();
   }

   // one new track for every label, from that label to the next
   
   TrackList newTracks;

   for(int i = 0; i < label->GetNumLabels(); i++) {
      wxString name = label->GetLabel(i)->title;
      double begin = label->GetLabel(i)->t;
      double end;

      // if on the last label, extend to the end of the wavetrack
      if(i == label->GetNumLabels() - 1) {
         if(stereo)
            end = wxMax(srcLeft->GetEndTime(), srcRight->GetEndTime());
         else
            end = srcLeft->GetEndTime();
      }
      else
         end = label->GetLabel(i+1)->t;

      Track *destLeft = 0;
      Track *destRight = 0;

      srcLeft->Copy(begin, end, &destLeft);
      if (destLeft) {
         destLeft->Init(*srcLeft);
         destLeft->SetOffset(wxMax(begin, srcLeft->GetOffset()));
         destLeft->SetName(name);
         
         mTracks->Add(destLeft);
      }

      if(stereo) {
         srcRight->Copy(begin, end, &destRight);
         if (destRight) {
            destRight->Init(*srcRight);
            destRight->SetOffset(wxMax(begin, srcRight->GetOffset()));
            destRight->SetName(name);
            
            mTracks->Add(destRight);
         }
         else if(destLeft)
            // account for possibility of a non-aligned linked track, which could
            // cause the left channel to be eligible for creating a new track,
            // but not the right.
            destLeft->SetLinked(false);
      }
   }

   PushState(_("Split at labels"), _("Split at labels"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelectAll()
{
   TrackListIterator iter(mTracks);

   Track *t = iter.First();
   while (t) {
      t->SetSelected(true);
      t = iter.Next();
   }
   mViewInfo.sel0 = mTracks->GetMinOffset();
   mViewInfo.sel1 = mTracks->GetEndTime();

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelectCursorEnd()
{
   double maxEndOffset = -1000000.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetEndTime() > maxEndOffset)
            maxEndOffset = t->GetEndTime();
      }

      t = iter.Next();
   }

   mViewInfo.sel1 = maxEndOffset;

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelectStartCursor()
{
   double minOffset = 1000000.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetOffset() < minOffset)
            minOffset = t->GetOffset();
      }

      t = iter.Next();
   }

   mViewInfo.sel0 = minOffset;

   mTrackPanel->Refresh(false);
}

//
// View Menu
//

void AudacityProject::OnZoomIn()
{
   // DMM: Here's my attempt to get logical zooming behavior
   // when there's a selection that's currently at least
   // partially on-screen

   bool selectionIsOnscreen =
      (mViewInfo.sel0 < mViewInfo.h + mViewInfo.screen) &&
      (mViewInfo.sel1 > mViewInfo.h);

   bool selectionFillsScreen =
      (mViewInfo.sel0 < mViewInfo.h) &&
      (mViewInfo.sel1 > mViewInfo.h + mViewInfo.screen);
   
   if (selectionIsOnscreen && !selectionFillsScreen) {
      // Start with the center of the selection
      double selCenter = (mViewInfo.sel0 + mViewInfo.sel1) / 2;

      // If the selection center is off-screen, pick the
      // center of the part that is on-screen.
      if (selCenter < mViewInfo.h)
         selCenter = mViewInfo.h + (mViewInfo.sel1 - mViewInfo.h) / 2;
      if (selCenter > mViewInfo.h + mViewInfo.screen)
         selCenter = mViewInfo.h + mViewInfo.screen -
            (mViewInfo.h + mViewInfo.screen - mViewInfo.sel0) / 2;
         
      // Zoom in
      Zoom(mViewInfo.zoom *= 2.0);

      // Recenter on selCenter
      TP_ScrollWindow(selCenter - mViewInfo.screen / 2);
      return;
   }


   double origLeft = mViewInfo.h;
   double origWidth = mViewInfo.screen;
   Zoom(mViewInfo.zoom *= 2.0);
   
   double newh = origLeft + (origWidth - mViewInfo.screen) / 2;
   
   // MM: Commented this out because it was confusing users
   /*
   // make sure that the *right-hand* end of the selection is
   // no further *left* than 1/3 of the way across the screen
   if (mViewInfo.sel1 < newh + mViewInfo.screen / 3)
      newh = mViewInfo.sel1 - mViewInfo.screen / 3;

   // make sure that the *left-hand* end of the selection is
   // no further *right* than 2/3 of the way across the screen
   if (mViewInfo.sel0 > newh + mViewInfo.screen * 2 / 3)
      newh = mViewInfo.sel0 - mViewInfo.screen * 2 / 3;
   */

   TP_ScrollWindow(newh);
}

void AudacityProject::OnZoomOut()
{  
   //Zoom() may change these, so record original values:
   double origLeft = mViewInfo.h;
   double origWidth = mViewInfo.screen;

   Zoom(mViewInfo.zoom /= 2.0);

   double newh = origLeft + (origWidth - mViewInfo.screen) / 2;
   // newh = (newh > 0) ? newh : 0;
   TP_ScrollWindow(newh);

}

static double OldZooms[2]={ 44100.0/512.0, 4410.0/512.0 };
void AudacityProject::OnZoomToggle()
{
   double origLeft = mViewInfo.h;
   double origWidth = mViewInfo.screen;

   float f;
   // look at percentage difference.  We add a small fudge factor
   // to avoid testing for zero divisor.
   f = mViewInfo.zoom / (OldZooms[0] + 0.0001f);
   // If old zoom is more than 10 percent different, use it.
   if( (0.90f > f) || (f >1.10) ){
      OldZooms[1]=OldZooms[0];
      OldZooms[0]=mViewInfo.zoom;
   }
   Zoom( OldZooms[1] );
   double newh = origLeft + (origWidth - mViewInfo.screen) / 2;
   TP_ScrollWindow(newh);
}


void AudacityProject::OnZoomNormal()
{
   Zoom(44100.0 / 512.0);
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnZoomFit()
{
   double len = mTracks->GetEndTime();

   if (len <= 0.0)
      return;

   int w, h;
   mTrackPanel->GetTracksUsableArea(&w, &h);
   w -= 10;

   Zoom(w / len);
   TP_ScrollWindow(0.0);
}

void AudacityProject::OnZoomFitV()
{
   int width, height, count;

   mTrackPanel->GetTracksUsableArea(&width, &height);

   height -= 28;

   count = 0;
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Wave)
         count++;
      else
         height -= t->GetHeight();

      t = iter.Next();
   }

   if (count == 0)
      return;

   height = height / count;

   if (height < 40)
      height = 40;

   TrackListIterator iter2(mTracks);
   t = iter2.First();
   while (t) {
      if (t->GetKind() == Track::Wave)
         t->SetHeight(height);
      t = iter2.Next();
   }

   mVsbar->SetThumbPosition(0);
   FixScrollbars();
   Refresh(false);
   ModifyState();
}

void AudacityProject::OnZoomSel()
{
   if (mViewInfo.sel1 <= mViewInfo.sel0)
      return;

   Zoom(mViewInfo.zoom * mViewInfo.screen / (mViewInfo.sel1 - mViewInfo.sel0)),
   TP_ScrollWindow(mViewInfo.sel0);
}

void AudacityProject::OnSnapOn()
{
   mSnapTo = 1;
   TP_DisplaySelection();
}

void AudacityProject::OnSnapOff()
{
   mSnapTo = 0;
   TP_DisplaySelection();
}

void AudacityProject::OnPlotSpectrum()
{
   int selcount = 0;
   int i;
   double rate = 0;
   sampleCount len = 0;
   float *buffer = NULL;
   bool warning = false;
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   while (t) {
      if (t->GetSelected() && t->GetKind() == Track::Wave) {
         WaveTrack *track = (WaveTrack *)t;
         if (selcount==0) {
            rate = track->GetRate();
            longSampleCount start, end;
            start = track->TimeToLongSamples(mViewInfo.sel0);
            end = track->TimeToLongSamples(mViewInfo.sel1);
            len = (sampleCount)(end - start);
            if (len > 1048576) {
               warning = true;
               len = 1048576;
            }
            buffer = new float[len];
            track->Get((samplePtr)buffer, floatSample, start, len);
         }
         else {
            if (track->GetRate() != rate) {
               wxMessageBox(_("To plot the spectrum, all selected tracks must be the same sample rate."));
               delete[] buffer;
               return;
            }
            longSampleCount start;
            start = track->TimeToLongSamples(mViewInfo.sel0);
            float *buffer2 = new float[len];
            track->Get((samplePtr)buffer2, floatSample, start, len);
            for(i=0; i<len; i++)
               buffer[i] += buffer2[i];
            delete[] buffer2;
         }
         selcount++;
      }
      t = iter.Next();
   }
   
   if (selcount == 0)
      return;
   
   if (selcount > 1)
      for(i=0; i<len; i++)
         buffer[i] /= selcount;
   
   if (warning) {
      wxString msg;
      msg.Printf(_("Too much audio was selected.  Only the first %.1f seconds of audio will be analyzed."),
                          (len / rate));
      wxMessageBox(msg);
   }

   InitFreqWindow(gParentWindow);
   gFreqWindow->Plot(len, buffer, rate);
   gFreqWindow->Show(true);
   gFreqWindow->Raise();

   delete[] buffer;
}


void AudacityProject::FloatToolBar(ToolBarStub * pStub )
{
   if (pStub->GetWindowedStatus()) {
      pStub->HideWindowedToolBar();
      pStub->LoadAll();
   } else {
      pStub->ShowWindowedToolBar();
      pStub->UnloadAll();
   }
}

void AudacityProject::LoadToolBar(ToolBarStub ** ppStub, enum ToolBarType t)
{
   if (*ppStub) {
      ToolBarStub *pStub = *ppStub;
      if (pStub->GetLoadedStatus()) {

         // Logic to prevent the control tool bar being unloaded.
         // TODO: Move into caller(s) of this function so
         // that this function can remain generic.
         if( pStub == gControlToolBarStub )
            return;
         //the toolbar is "loaded", meaning its visible either in the window
         //or floating
         pStub->SetLoadedStatus(false);
         pStub->HideWindowedToolBar();
         pStub->UnloadAll();

      } else {

         //the toolbar is not "loaded", meaning that although the stub exists, 
         //the toolbar is not visible either in a window or floating around
         pStub->SetLoadedStatus(true);

         if (pStub->GetWindowedStatus()) {
            //Make the floating toolbar appear
            pStub->ShowWindowedToolBar();
            pStub->LoadAll();
         } else {
            //Make it appear in all the windows
            pStub->LoadAll();
         }

      }
   } else {
      *ppStub = new ToolBarStub(gParentWindow, t);
      (*ppStub)->LoadAll();
   }
}


void AudacityProject::OnFloatControlToolBar()
{
   FloatToolBar( gControlToolBarStub );
}


void AudacityProject::OnLoadEditToolBar()
{
   LoadToolBar(&gEditToolBarStub, EditToolBarID);
}


void AudacityProject::OnFloatEditToolBar()
{
   FloatToolBar( gEditToolBarStub );
}


void AudacityProject::OnLoadMixerToolBar()
{
   LoadToolBar(&gMixerToolBarStub, MixerToolBarID);
}


void AudacityProject::OnFloatMixerToolBar()
{
   FloatToolBar( gMixerToolBarStub );
}

void AudacityProject::OnLoadMeterToolBar()
{
   LoadToolBar(&gMeterToolBarStub, MeterToolBarID);
}


void AudacityProject::OnFloatMeterToolBar()
{
   // Can't drag the Meter toolbar while Audio I/O is busy at all
   if (gAudioIO->IsStreamActive())
      return;
   FloatToolBar( gMeterToolBarStub );
}

void AudacityProject::OnFloatTranscriptionToolBar()
{
   FloatToolBar( gTranscriptionToolBarStub );
}


//
// Project Menu
//

void AudacityProject::OnImport()
{
   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"),FROMFILENAME(::wxGetCwd()));

   // TODO: Build the list of file types dynamically
   
   wxFileDialog dlog(this, _("Select one or more audio files..."),
                     path, wxT(""),
                     _("All files (*.*)|*.*|"
                       "WAV files (*.wav)|*.wav|"
                       "AIFF files (*.aif)|*.aif|"
                       "AU files (*.au)|*.au|"
                       "MP3 files (*.mp3)|*.mp3|"
                       "Ogg Vorbis files (*.ogg)|*.ogg|"
		       "FLAC files (*.flac)|*.flac|"
                       "List of Files (*.lof)|*.lof"),
                     wxOPEN | wxMULTIPLE);

   int result = dlog.ShowModal();

   if (result != wxID_OK)
      return;

   wxArrayString selectedFiles;
   unsigned int ff;

   dlog.GetPaths(selectedFiles);

   for(ff=0; ff<selectedFiles.GetCount(); ff++) {
      wxString fileName = selectedFiles[ff];

      path =::wxPathOnly(fileName);
      gPrefs->Write(wxT("/DefaultOpenPath"), path);
      
      Import(fileName);
   }
}

void AudacityProject::OnImportLabels()
{
   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"),FROMFILENAME(::wxGetCwd()));

   wxString fileName =
       wxFileSelector(_("Select a text file containing labels..."),
                      path,     // Path
                      wxT(""),       // Name
                      wxT(".txt"),   // Extension
                      _("Text files (*.txt)|*.txt|" "All files (*.*)|*.*"),
                      0,        // Flags
                      this);    // Parent

   if (fileName != wxT("")) {
      path =::wxPathOnly(fileName);
      gPrefs->Write(wxT("/DefaultOpenPath"), path);

      wxTextFile f;

      f.Open(fileName);
      if (!f.IsOpened()) {
         wxMessageBox(_("Could not open file: ") + fileName);
         return;
      }

      LabelTrack *newTrack = new LabelTrack(mDirManager);

      newTrack->Import(f);

      SelectNone();
      mTracks->Add(newTrack);
      newTrack->SetSelected(true);

      PushState(wxString::
                Format(_("Imported labels from '%s'"), fileName.c_str()),
                _("Import Labels"));

      FixScrollbars();
      mTrackPanel->Refresh(false);
   }
}

void AudacityProject::OnImportMIDI()
{
   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"),FROMFILENAME(::wxGetCwd()));

   wxString fileName = wxFileSelector(_("Select a MIDI file..."),
                                      path,     // Path
                                      wxT(""),       // Name
                                      wxT(""),       // Extension
                                      _("All files (*.*)|*.*|"
                                        "MIDI files (*.mid)|*.mid|"
                                        "Allegro files (*.gro)|*.gro"),
                                      0,        // Flags
                                      this);    // Parent

   if (fileName != wxT("")) {
      path =::wxPathOnly(fileName);
      gPrefs->Write(wxT("/DefaultOpenPath"), path);

      NoteTrack *newTrack = new NoteTrack(mDirManager);

      if (::ImportMIDI(fileName, newTrack)) {

         SelectNone();
         mTracks->Add(newTrack);
         newTrack->SetSelected(true);

         PushState(wxString::Format(_("Imported MIDI from '%s'"),
                                    fileName.c_str()),
                   _("Import MIDI"));

         FixScrollbars();
         mTrackPanel->Refresh(false);
      }
   }
}

void AudacityProject::OnImportRaw()
{
   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"),FROMFILENAME(::wxGetCwd()));

   wxString fileName =
       wxFileSelector(_("Select any uncompressed audio file..."),
                      path,     // Path
                      wxT(""),       // Name
                      wxT(""),       // Extension
                      _("All files (*)|*"),
                      0,        // Flags
                      this);    // Parent

   if (fileName == wxT(""))
      return;

   path =::wxPathOnly(fileName);
   gPrefs->Write(wxT("/DefaultOpenPath"), path);
   
   Track **newTracks;
   int numTracks;
   
   wxStartTimer();

   wxASSERT(!mImportProgressDialog);

   mImportingRaw = true;

   numTracks = ::ImportRaw(this, fileName, mTrackFactory, &newTracks,
                           AudacityProject::ImportProgressCallback,
                           this);

   if(mImportProgressDialog) {
      delete mImportProgressDialog;
      mImportProgressDialog = NULL;
   }

   mImportingRaw = false;
   
   if (numTracks <= 0)
      return;

   AddImportedTracks(fileName, newTracks, numTracks);
}

void AudacityProject::OnEditID3()
{
   if (mTags->ShowEditDialog(this, _("Edit ID3 Tags (for MP3 exporting)")))
      PushState(_("Edit ID3 tags"), _("Edit ID3 Tags"));
}

void AudacityProject::OnQuickMix()
{
   WaveTrack *newLeft = NULL;
   WaveTrack *newRight = NULL;

   if (::QuickMix(mTracks, mTrackFactory, mRate, mDefaultFormat, 0.0, 0.0,
                  &newLeft, &newRight)) {

      // Remove originals

      TrackListIterator iter(mTracks);
      Track *t = iter.First();

      while (t) {
         if (t->GetSelected()) {
            delete t;
            t = iter.RemoveCurrent();
         }
         else
            t = iter.Next();
      }

      // Add new tracks

      mTracks->Add(newLeft);
      if (newRight)
         mTracks->Add(newRight);

      PushState(_("Quick mix"), _("Quick mix"));

      FixScrollbars();
      mTrackPanel->Refresh(false);
   }
}

void AudacityProject::OnSelectionSave()
{
   mSel0save = mViewInfo.sel0;
   mSel1save = mViewInfo.sel1;
}

void AudacityProject::OnSelectionRestore()
{
   mViewInfo.sel0 = mSel0save;
   mViewInfo.sel1 = mSel1save;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorTrackStart()
{
   double minOffset = 1000000.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetOffset() < minOffset)
            minOffset = t->GetOffset();
      }

      t = iter.Next();
   }

   if (minOffset < 0.0) minOffset = 0.0;
   mViewInfo.sel0 = minOffset;
   mViewInfo.sel1 = minOffset;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorTrackEnd()
{
   double maxEndOffset = -1000000.0;
   double thisEndOffset = 0.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         thisEndOffset = t->GetEndTime();
         if (thisEndOffset > maxEndOffset)
            maxEndOffset = thisEndOffset;
      }

      t = iter.Next();
   }

   mViewInfo.sel0 = maxEndOffset;
   mViewInfo.sel1 = maxEndOffset;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorSelStart()
{
   mViewInfo.sel1 = mViewInfo.sel0;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorSelEnd()
{
   mViewInfo.sel0 = mViewInfo.sel1;
   mTrackPanel->Refresh(false);
}

void AudacityProject::HandleAlign(int index, bool moveSel)
{
   TrackListIterator iter(mTracks);
   wxString action;
   double offset;
   double minOffset = 1000000000.0;
   double maxEndOffset = 0.0;
   double avgOffset = 0.0;
   int numSelected = 0;
   Track *t = iter.First();
   double delta = 0.0;
   double newPos = -1.0;

   while (t) {
      if (t->GetSelected()) {
         numSelected++;

         offset = t->GetOffset();
         avgOffset += offset;
         if (offset < minOffset)
            minOffset = offset;
         if (t->GetEndTime() > maxEndOffset)
            maxEndOffset = t->GetEndTime();
      }
      t = iter.Next();
   }

   avgOffset /= numSelected;

   switch(index) {
   case kAlignZero:
      delta = -minOffset;
      action = _("Aligned with zero");
      break;
   case kAlignCursor:
      delta = mViewInfo.sel0 - minOffset;
      action = _("Aligned cursor");
      break;
   case kAlignSelStart:
      delta = mViewInfo.sel0 - minOffset;
      action = _("Aligned with selection start");
      break;
   case kAlignSelEnd:
      delta = mViewInfo.sel1 - minOffset;
      action = _("Aligned with selection end");
      break;
   case kAlignEndCursor:
      delta = mViewInfo.sel0 - maxEndOffset;
      action = _("Aligned end with cursor");
      break;
   case kAlignEndSelStart:
      delta = mViewInfo.sel0 - maxEndOffset;
      action = _("Aligned end with selection start");
      break;
   case kAlignEndSelEnd:
      delta = mViewInfo.sel1 - maxEndOffset;
      action = _("Aligned end with selection end");
      break;
   case kAlign:
      newPos = avgOffset;
      action = _("Aligned");
      break;
   }

   if (newPos >= 0.0) {
      TrackListIterator iter(mTracks);
      Track *t = iter.First();
      
      while (t) {
         if (t->GetSelected()) {
            t->SetOffset(newPos);
         }
         t = iter.Next();
      }
   }

   if (delta != 0.0) {
      TrackListIterator iter(mTracks);
      Track *t = iter.First();
      
      while (t) {
         if (t->GetSelected()) {
            t->SetOffset(t->GetOffset() + delta);
         }
         t = iter.Next();
      }
   }

   if (moveSel) {
      mViewInfo.sel0 += delta;
      mViewInfo.sel1 += delta;
   }

   PushState(action, _("Align"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlign(int index)
{
   HandleAlign(index, false);
}

void AudacityProject::OnAlignMoveSel(int index)
{
   HandleAlign(index, true);
}

void AudacityProject::OnNewWaveTrack()
{
   WaveTrack *t = mTrackFactory->NewWaveTrack(mDefaultFormat);
   t->SetRate((int) mRate);
   SelectNone();

   mTracks->Add(t);
   t->SetSelected(true);

   PushState(_("Created new audio track"), _("New Track"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnNewStereoTrack()
{
   WaveTrack *t = mTrackFactory->NewWaveTrack(mDefaultFormat);
   t->SetChannel(Track::LeftChannel);
   t->SetRate((int) mRate);
   SelectNone();
   
   mTracks->Add(t);
   t->SetSelected(true);
   t->SetLinked (true);
   
   t = mTrackFactory->NewWaveTrack(mDefaultFormat);
   t->SetChannel(Track::RightChannel);
   t->SetRate((int) mRate);
   
   mTracks->Add(t);
   t->SetSelected (true);
   
   PushState(_("Created new stereo audio track"), _("New Track"));
   
   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnNewLabelTrack()
{
   LabelTrack *t = new LabelTrack(mDirManager);

   SelectNone();

   mTracks->Add(t);
   t->SetSelected(true);
   t->SetKeyOn(true);

   PushState(_("Created new label track"), _("New Track"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnNewTimeTrack()
{
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   bool alreadyHaveTimeTrack = false;
   
   while (t)
      {
         if (t->GetKind() == Track::Time)
            {
               alreadyHaveTimeTrack = true;
               break;
            }
         t = iter.Next();
      }
   
   if( alreadyHaveTimeTrack )
      {
         wxString msg;
         msg.Printf(_("The version of Audacity you are using does not support multiple time tracks."));
         wxMessageBox(msg);
      }
   else
      {
         TimeTrack *t = new TimeTrack(mDirManager);

         SelectNone();
         mTracks->AddToHead(t);
         t->SetSelected(true);
         
         PushState(_("Created new time track"), _("New Track"));

         /*
         TrackListIterator iter(mTracks);
         for( Track *tr = iter.First(); (tr); tr = iter.Next() )
            tr->SetTimeTrack( t );
         */
         
         FixScrollbars();
         mTrackPanel->Refresh(false);
      }
}

int AudacityProject::DoAddLabel(double left, double right)
{
   TrackListIterator iter(mTracks);
   LabelTrack *lt = NULL;

   Track *t = iter.First();
   while (t && !lt) {
      if (t->GetKind() == Track::Label)
         lt = (LabelTrack *)t;
      else
         t = iter.Next();
   }

   if (!lt) {
      lt = new LabelTrack(mDirManager);
      mTracks->Add(lt);

   }
   SelectNone();
   lt->SetSelected(true);

   int index = lt->AddLabel(left, right);

   PushState(_("Added label"), _("Label"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
   return index;
}

void AudacityProject::OnAddLabel()
{
   DoAddLabel(mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnAddLabelPlaying()
{
   if (GetAudioIOToken()>0 &&
       gAudioIO->IsStreamActive(GetAudioIOToken())) {
      double indicator = gAudioIO->GetStreamTime();
      DoAddLabel(indicator, indicator);
   }
}

void AudacityProject::OnRemoveTracks()
{
   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         delete t;
         t = iter.RemoveCurrent();
      }
      else
         t = iter.Next();
   }

   PushState(_("Removed audio track(s)"), _("Remove Track"));

   mTrackPanel->Refresh(false);
}

//
// Help Menu
//

void AudacityProject::OnAbout()
{
   AboutDialog dlog(this);
   dlog.ShowModal();
}

void AudacityProject::OnHelp()
{
   ::ShowHelp(this);
}

void AudacityProject::OnHelpIndex()
{
   ::ShowHelpIndex(this);
}

void AudacityProject::OnHelpSearch()
{
   ::SearchHelp(this);
}

void AudacityProject::OnBenchmark()
{
   ::RunBenchmark(this);
}

//

void AudacityProject::OnSeparator()
{

}

void AudacityProject::OnCollapseAllTracks()
{
   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t)
   {
      t->SetMinimized(true);
      t = iter.Next();
   }

   ModifyState();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnExpandAllTracks()
{
   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t)
   {
      t->SetMinimized(false);
      t = iter.Next();
   }

   ModifyState();
   mTrackPanel->Refresh(false);
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: e8ab21c6-d9b9-4d35-b4c2-ff90c1781b85

