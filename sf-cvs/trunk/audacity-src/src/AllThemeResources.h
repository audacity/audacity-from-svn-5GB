/**********************************************************************

  Audacity: A Digital Audio Editor

  AllThemeResources.h

  James Crook

  Audacity is free software.
  This file is licensed under the GPL license, see License.txt

********************************************************************//**

\file AllThemeResources.h

This file contains definitions of all images, cursors, colours, fonts
and grids used by Audacity.

This will be split up into separate include files to reduce the amount
of recompilation on a change.

*//*******************************************************************/

// Note: No '#ifndef/#define' pair on this header file.
// we want to include it multiple times in Theme.cpp.

#include "MacroMagic.h"

   SET_THEME_FLAGS( resFlagPaired );
   // The control buttons...
   DEFINE_IMAGE(  bmpPause, MaskedImage(Pause, PauseAlpha ),wxT("Pause"));
   DEFINE_IMAGE(  bmpPauseDisabled, MaskedImage(PauseDisabled,PauseAlpha),wxT("PauseDisabled"));
                  
   DEFINE_IMAGE(  bmpPlay, MaskedImage(Play,PlayAlpha),wxT("Play"));
   DEFINE_IMAGE(  bmpPlayDisabled, MaskedImage(PlayDisabled,PlayAlpha),wxT("PlayDisabled"));
                  
   DEFINE_IMAGE(  bmpLoop, MaskedImage((const char **)Loop,(const char **)LoopAlpha),wxT("Loop"));
   DEFINE_IMAGE(  bmpLoopDisabled, MaskedImage((const char **)LoopDisabled,(const char **)LoopAlpha),wxT("LoopDisabled"));
                  
   DEFINE_IMAGE(  bmpStop, MaskedImage(Stop,StopAlpha),wxT("Stop"));
   DEFINE_IMAGE(  bmpStopDisabled, MaskedImage(StopDisabled,StopAlpha),wxT("StopDisabled"));
                  
//   DEFINE_IMAGE(  bmpRewind, MaskedImage(Rewind,RewindAlpha),wxT("Rewind"));
   DEFINE_IMAGE(  bmpRewind, wxImage( 16, 16 ),wxT("Rewind"));
   DEFINE_IMAGE(  bmpRewindDisabled, wxImage( 16, 16 ),wxT("RewindDisabled"));
                  
   DEFINE_IMAGE(  bmpFFwd, MaskedImage(FFwd,FFwdAlpha),wxT("FFwd"));
   DEFINE_IMAGE(  bmpFFwdDisabled, MaskedImage(FFwdDisabled,FFwdAlpha),wxT("FFwdDisabled"));
                  
   DEFINE_IMAGE(  bmpRecord, MaskedImage(Record,RecordAlpha),wxT("Record"));
   DEFINE_IMAGE(  bmpRecordDisabled, MaskedImage(RecordDisabled,RecordAlpha),wxT("RecordDisabled"));
                  
   DEFINE_IMAGE(  bmpCleanSpeech, MaskedImage(CleanSpeech,CleanSpeechAlpha),wxT("CleanSpeech"));
   DEFINE_IMAGE(  bmpCleanSpeechDisabled, MaskedImage(CleanSpeechDisabled,CleanSpeechAlpha),wxT("CleanSpeechDisabled"));
   
//------- START of Platform specific images -----------------------// 
   SET_THEME_FLAGS( resFlagNewLine );

   // PC/Linux: The backgrounds used for the large control buttons.
   DEFINE_IMAGE(  bmpUpButtonLarge, UpButton, wxT("UpButtonLarge"));
   DEFINE_IMAGE(  bmpDownButtonLarge, DownButton, wxT("DownButtonLarge"));
   DEFINE_IMAGE(  bmpHiliteButtonLarge, HiliteButton, wxT("HiliteButtonLarge"));

   // PC/Linux: The backgrounds used for the small control buttons.
   DEFINE_IMAGE(  bmpUpButtonSmall, Up, wxT("UpButtonSmall"));
   DEFINE_IMAGE(  bmpDownButtonSmall, Down, wxT("DownButtonSmall"));
   DEFINE_IMAGE(  bmpHiliteButtonSmall, Hilite, wxT("HiliteButtonSmall"));
   // Other sliders...
   DEFINE_IMAGE(  bmpVolumeSlider,      VolumeSlider,      wxT("VolumeSlider"));
   DEFINE_IMAGE(  bmpVolumeSliderThumb, VolumeSliderThumb, wxT("VolumeSliderThumb"));

   // PC/Linux: Sliders and their thumbs.
   DEFINE_IMAGE(  bmpSliderThumb, MaskedImage( SliderThumb, (const char**)SliderThumbAlpha ), wxT("SliderThumb"));
   // The Basic slider is hand-drawn for now.
//   DEFINE_IMAGE(  bmpSlider,      Slider,      wxT("Slider"));

// Hack: force new line in imageCache
   SET_THEME_FLAGS( resFlagNone );
   // Mac: The backgrounds used for the large control buttons.
   DEFINE_IMAGE(  bmpMacUpButtonLarge, MacUpButton, wxT("MacUpButton"));
   DEFINE_IMAGE(  bmpMacDownButtonLarge, MacDownButton, wxT("MacDownButton"));
   DEFINE_IMAGE(  bmpMacHiliteButtonLarge, MacHiliteButton, wxT("MacHiliteButton"));

   // Mac: The backgrounds used for the small control buttons.
   DEFINE_IMAGE(  bmpMacUpButtonSmall, MacUp, wxT("MacUpButtonSmall"));
   DEFINE_IMAGE(  bmpMacDownButtonSmall, MacDown, wxT("MacDownButtonSmall"));
   DEFINE_IMAGE(  bmpMacHiliteButtonSmall, MacHilite, wxT("MacHiliteButtonSmall"));

   // Mac: Sliders and their thumbs.
   DEFINE_IMAGE(  bmpMacSlider,      MacSlider,      wxT("MacSlider"));
   DEFINE_IMAGE(  bmpMacSliderThumb, MacSliderThumb, wxT("MacSliderThumb"));

//------- END of Platform specific images -----------------------// 

   SET_THEME_FLAGS( resFlagNewLine );
   // These three images are just 'workspace'.  Use the internal flag so they
   // aren't loaded or saved.
   SET_THEME_FLAGS( resFlagInternal );
   DEFINE_IMAGE(  bmpRecoloredUpLarge, UpButton, wxT("RecolouredUpLarge"));
   DEFINE_IMAGE(  bmpRecoloredDownLarge, DownButton, wxT("RecolouredDownLarge"));
   DEFINE_IMAGE(  bmpRecoloredHiliteLarge,HiliteButton, wxT("RecolouredHiliteLarge"));

   DEFINE_IMAGE(  bmpRecoloredUpSmall, UpButton, wxT("RecolouredUpSmall"));
   DEFINE_IMAGE(  bmpRecoloredDownSmall, DownButton, wxT("RecolouredDownSmall"));
   DEFINE_IMAGE(  bmpRecoloredHiliteSmall,HiliteButton, wxT("RecolouredHiliteSmall"));
   SET_THEME_FLAGS( resFlagNone );


   SET_THEME_FLAGS( resFlagCursor );
   DEFINE_IMAGE(  bmpIBeamCursor, IBeamCursorXpm, wxT("IBeamCursor"));
   DEFINE_IMAGE(  bmpDrawCursor,DrawCursorXpm, wxT("DrawCursor"));
   DEFINE_IMAGE(  bmpEnvCursor,EnvCursorXpm, wxT("EnvCursor"));
   DEFINE_IMAGE(  bmpTimeCursor,MaskedImage(TimeCursorXpm,TimeCursorXpm), wxT("TimeCursor"));
   DEFINE_IMAGE(  bmpZoomInCursor,ZoomInCursorXpm, wxT("ZoomInCursor"));
   DEFINE_IMAGE(  bmpZoomOutCursor,ZoomOutCursorXpm, wxT("ZoomOutCursor"));
   DEFINE_IMAGE(  bmpLabelCursorLeft,LabelCursorLeftXpm, wxT("LabelCursorLeft"));
   DEFINE_IMAGE(  bmpLabelCursorRight, LabelCursorRightXpm, wxT("LabelCursorRight"));
   DEFINE_IMAGE(  bmpDisabledCursor,DisabledCursorXpm, wxT("DisabledCursor"));

   SET_THEME_FLAGS( resFlagNone );
   DEFINE_IMAGE(  bmpIBeam,MaskedImage(IBeam,IBeamAlpha), wxT("IBeam"));
   DEFINE_IMAGE(  bmpZoom,MaskedImage(Zoom,ZoomAlpha), wxT("Zoom"));
   DEFINE_IMAGE(  bmpEnvelope,MaskedImage(Envelope,EnvelopeAlpha), wxT("Envelope"));
   DEFINE_IMAGE(  bmpTimeShift,MaskedImage(TimeShift,TimeShiftAlpha), wxT("TimeShift"));
   DEFINE_IMAGE(  bmpDraw,MaskedImage(Draw,DrawAlpha), wxT("Draw"));
   DEFINE_IMAGE(  bmpMulti,MaskedImage(Multi,MultiAlpha), wxT("Multi"));

   DEFINE_IMAGE(  bmpMic,MaskedImage(Mic,MicAlpha), wxT("Mic"));
   DEFINE_IMAGE(  bmpSpeaker,MaskedImage(Speaker,SpeakerAlpha), wxT("Speaker"));

   SET_THEME_FLAGS( resFlagPaired );
   DEFINE_IMAGE(  bmpZoomFit,MaskedImage(ZoomFit,ZoomFitAlpha), wxT("ZoomFit"));
   DEFINE_IMAGE(  bmpZoomFitDisabled,MaskedImage(ZoomFitDisabled,ZoomFitAlpha), wxT("ZoomFitDisabled"));
                  
   DEFINE_IMAGE(  bmpZoomIn,MaskedImage(ZoomIn,ZoomInAlpha), wxT("ZoomIn"));
   DEFINE_IMAGE(  bmpZoomInDisabled,MaskedImage(ZoomInDisabled,ZoomInAlpha), wxT("ZoomInDisabled"));
                  
   DEFINE_IMAGE(  bmpZoomOut,MaskedImage(ZoomOut,ZoomOutAlpha), wxT("ZoomOut"));
   DEFINE_IMAGE(  bmpZoomOutDisabled,MaskedImage(ZoomOutDisabled,ZoomOutAlpha), wxT("ZoomOutDisabled"));
                  
   DEFINE_IMAGE(  bmpZoomSel,MaskedImage(ZoomSel,ZoomSelAlpha), wxT("ZoomSel"));
   DEFINE_IMAGE(  bmpZoomSelDisabled,MaskedImage(ZoomSelDisabled,ZoomSelAlpha), wxT("ZoomSelDisabled"));
                  
   DEFINE_IMAGE(  bmpZoomToggle,MaskedImage(ZoomToggle,ZoomToggleAlpha), wxT("ZoomToggle"));
   DEFINE_IMAGE(  bmpZoomToggleDisabled,MaskedImage(ZoomToggleDisabled,ZoomToggleAlpha), wxT("ZoomToggleDisabled"));
   DEFINE_IMAGE(  bmpCut,MaskedImage(Cut,CutAlpha), wxT("Cut"));
   DEFINE_IMAGE(  bmpCutDisabled,MaskedImage(CutDisabled,CutAlpha), wxT("CutDisabled"));
                  
   DEFINE_IMAGE(  bmpCopy,MaskedImage(Copy,CopyAlpha), wxT("Copy"));
   DEFINE_IMAGE(  bmpCopyDisabled,MaskedImage(CopyDisabled,CopyAlpha), wxT("CopyDisabled"));
                  
   DEFINE_IMAGE(  bmpPaste,MaskedImage(Paste,PasteAlpha), wxT("Paste"));
   DEFINE_IMAGE(  bmpPasteDisabled,MaskedImage(PasteDisabled,PasteAlpha), wxT("PasteDisabled"));
                  
   DEFINE_IMAGE(  bmpTrim,MaskedImage(Trim,TrimAlpha), wxT("Trim"));
   DEFINE_IMAGE(  bmpTrimDisabled,MaskedImage(TrimDisabled,TrimAlpha), wxT("TrimDisabled"));
                  
   DEFINE_IMAGE(  bmpSilence,MaskedImage(Silence,SilenceAlpha), wxT("Silence"));
   DEFINE_IMAGE(  bmpSilenceDisabled,MaskedImage(SilenceDisabled,SilenceAlpha), wxT("SilenceDisabled"));
                  
   DEFINE_IMAGE(  bmpUndo,MaskedImage(Undo,UndoAlpha), wxT("Undo"));
   DEFINE_IMAGE(  bmpUndoDisabled,MaskedImage(UndoDisabled,UndoAlpha), wxT("UndoDisabled"));
                  
   DEFINE_IMAGE(  bmpRedo,MaskedImage(Redo,RedoAlpha), wxT("Redo"));
   DEFINE_IMAGE(  bmpRedoDisabled,MaskedImage(RedoDisabled,RedoAlpha), wxT("RedoDisabled"));

   SET_THEME_FLAGS( resFlagPaired | resFlagNewLine );
   DEFINE_IMAGE(  bmpTnStartOn,MaskedImage(StartOn, StartOnAlpha), wxT("TnStartOn"));
   DEFINE_IMAGE(  bmpTnStartOnDisabled,MaskedImage(StartOnDisabled, StartOnAlpha), wxT("TnStartOnDisabled"));

   DEFINE_IMAGE(  bmpTnStartOff,MaskedImage(StartOff, StartOffAlpha), wxT("TnStartOff"));
   DEFINE_IMAGE(  bmpTnStartOffDisabled,MaskedImage(StartOffDisabled, StartOffAlpha), wxT("TnStartOffDisabled"));

   DEFINE_IMAGE(  bmpTnEndOn,MaskedImage(EndOn, EndOnAlpha), wxT("TnEndOn"));
   DEFINE_IMAGE(  bmpTnEndOnDisabled,MaskedImage(EndOnDisabled, EndOnAlpha), wxT("TnEndOnDisabled"));

   DEFINE_IMAGE(  bmpTnEndOff,MaskedImage(EndOff, EndOffAlpha), wxT("TnEndOff"));
   DEFINE_IMAGE(  bmpTnEndOffDisabled,MaskedImage(EndOffDisabled, EndOffAlpha), wxT("TnEndOffDisabled"));

   DEFINE_IMAGE(  bmpTnCalibrate,MaskedImage(CalibrateUp, CalibrateAlpha), wxT("TnCalibrate"));
   DEFINE_IMAGE(  bmpTnCalibrateDisabled,MaskedImage(CalibrateDisabled, CalibrateAlpha), wxT("TnCalibrateDisabled"));

   DEFINE_IMAGE(  bmpTnAutomateSelection,MaskedImage(AutomateSelection, AutomateSelectionAlpha), wxT("TnAutomateSelection"));
   DEFINE_IMAGE(  bmpTnAutomateSelectionDisabled,MaskedImage(AutomateSelectionDisabled, AutomateSelectionAlpha), wxT("TnAutomateSelectionDisabled"));

   DEFINE_IMAGE(  bmpTnMakeTag,MaskedImage(MakeTag, MakeTagAlpha), wxT("TnMakeTag"));
   DEFINE_IMAGE(  bmpTnMakeTagDisabled,MaskedImage(MakeTagDisabled, MakeTagAlpha), wxT("TnMakeTagDisabled"));

   DEFINE_IMAGE(  bmpTnSelectSound,MaskedImage(SelectSound, SelectSoundAlpha), wxT("TnSelectSound"));
   DEFINE_IMAGE(  bmpTnSelectSoundDisabled,MaskedImage(SelectSoundDisabled, SelectSoundAlpha), wxT("TnSelectSoundDisabled"));

   DEFINE_IMAGE(  bmpTnSelectSilence,MaskedImage(SelectSilence, SelectSilenceAlpha), wxT("TnSelectSilence"));
   DEFINE_IMAGE(  bmpTnSelectSilenceDisabled,MaskedImage(SelectSilenceDisabled, SelectSilenceAlpha), wxT("TnSelectSilenceDisabled"));


   SET_THEME_FLAGS( resFlagNone );
   DEFINE_IMAGE(  bmpLabelGlyph0,        Glyph0, wxT("LabelGlyph0" ));
   DEFINE_IMAGE(  bmpLabelGlyph1,        Glyph1, wxT("LabelGlyph1" ));
   DEFINE_IMAGE(  bmpLabelGlyph2,        Glyph2, wxT("LabelGlyph2" ));
   DEFINE_IMAGE(  bmpLabelGlyph3,        Glyph3, wxT("LabelGlyph3" ));
   DEFINE_IMAGE(  bmpLabelGlyph4,        Glyph4, wxT("LabelGlyph4" ));
   DEFINE_IMAGE(  bmpLabelGlyph5,        Glyph5, wxT("LabelGlyph5" ));
   DEFINE_IMAGE(  bmpLabelGlyph6,        Glyph6, wxT("LabelGlyph6" ));
   DEFINE_IMAGE(  bmpLabelGlyph7,        Glyph7, wxT("LabelGlyph7" ));
   DEFINE_IMAGE(  bmpLabelGlyph8,        Glyph8, wxT("LabelGlyph8" ));
   DEFINE_IMAGE(  bmpLabelGlyph9,        Glyph9, wxT("LabelGlyph9" ));
   DEFINE_IMAGE(  bmpLabelGlyph10,        Glyph10, wxT("LabelGlyph10" ));
   DEFINE_IMAGE(  bmpLabelGlyph11,        Glyph11, wxT("LabelGlyph11" ));


   SET_THEME_FLAGS( resFlagNewLine );
   DEFINE_IMAGE(  bmpPostfishHome,        bar_home_xpm, wxT("PostfishHome" ));
   DEFINE_IMAGE(  bmpPostfishFastRewind,  bar_bb_xpm,   wxT("PostfishFastRewind" ));
   DEFINE_IMAGE(  bmpPostfishRewind,      bar_b_xpm,    wxT("PostfishRewind" ));
   DEFINE_IMAGE(  bmpPostfishPlay,        bar_p_xpm,    wxT("PostfishPlay" ));
   DEFINE_IMAGE(  bmpPostfishForward,     bar_f_xpm,    wxT("PostfishForward" ));
   DEFINE_IMAGE(  bmpPostfishFastForward, bar_ff_xpm,   wxT("PostfishFastForward" ));
   DEFINE_IMAGE(  bmpPostfishEnd,         bar_end_xpm,  wxT("PostfishEnd" ));
   DEFINE_IMAGE(  bmpPostfishLoop,        bar_l_xpm,    wxT("PostfishLoop" ));

   SET_THEME_FLAGS( resFlagNone );

   DEFINE_IMAGE( bmpDockDown, DockDown, wxT("DockDown"));
   DEFINE_IMAGE( bmpDockDownShort, DockDownShort, wxT("DockDownShort"));
   DEFINE_IMAGE( bmpDockOver, DockOver, wxT("DockOver"));
   DEFINE_IMAGE( bmpDockOverShort, DockOverShort, wxT("DockOverShort"));
   DEFINE_IMAGE( bmpDockUp, DockUp, wxT("DockUp"));
   DEFINE_IMAGE( bmpDockUpShort, DockUpShort, wxT("DockUpShort"));

   DEFINE_IMAGE(  bmpToolBarToggle, ToolBarToggle,wxT("ToolBarToggle"));
   DEFINE_IMAGE(  bmpToolBarTarget, ToolBarTarget,wxT("ToolBarTarget"));
   DEFINE_IMAGE(  bmpToolBarGrabber, ToolBarGrabber,wxT("ToolBarGrabber"));
   
   //   DEFINE_IMAGE( bmpAqua, Aqua, wxT("Aqua"));
   DEFINE_IMAGE( bmpArrow, arrow_xpm, wxT("Arrow"));

   DEFINE_IMAGE( bmpUploadFile, file_xpm,wxT("UploadFile"));
   DEFINE_IMAGE( bmpUploadFolder, folder_xpm, wxT("UploadFolder"));
   DEFINE_IMAGE( bmpUploadMp3, mp3_xpm, wxT("UploadMp3"));
   DEFINE_IMAGE( bmpUploadUp, up_xpm, wxT("UploadUp"));

   SET_THEME_FLAGS( resFlagNewLine );
   DEFINE_IMAGE( bmpAudacityLogo, AudacityLogo_xpm, wxT("AudacityLogo"));
   DEFINE_IMAGE( bmpAudacityLogo48x48, AudacityLogo48x48_xpm, wxT("AudacityLogo48x48"));

#if 0
enum teColours
{
   clrBlank=0,
   clrUnselected,
   clrSelected,
   clrSample,
   clrSelSample,
   clrDragSample,
                  
   clrMuteSample,
   clrRms,
   clrMuteRms,
   clrShadow
};
#endif

