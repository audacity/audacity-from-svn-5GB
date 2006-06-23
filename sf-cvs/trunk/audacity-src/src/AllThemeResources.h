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

To add an image, you give its size and name like so:

\code
   DEFINE_IMAGE( bmpPause, wxImage( 16, 16 ), wxT("Pause"));
\endcode 

If you do this and run the program the image will be black to start 
with, but you can go into ThemePrefs and load it (load components) 
from there.  Audacity will look for a file called "Pause.png".  

 - Now save into ImageCache.
 - From here on you can get the image by loading ImageCache.
 - To burn it into the program defaults, use the 
 'Output Sourcery' button.

\see \ref Themability in DOxygen documentation for more details.

*//*******************************************************************/

// Note: No '#ifndef/#define' pair on this header file.
// we want to include it multiple times in Theme.cpp.

#include "MacroMagic.h"

#define XPMS_RETIRED
#ifdef XPMS_RETIRED

   SET_THEME_FLAGS(  resFlagPaired  );
   DEFINE_IMAGE( bmpPause, wxImage( 16, 16 ), wxT("Pause"));
   DEFINE_IMAGE( bmpPauseDisabled, wxImage( 16, 16 ), wxT("PauseDisabled"));
   DEFINE_IMAGE( bmpPlay, wxImage( 16, 16 ), wxT("Play"));
   DEFINE_IMAGE( bmpPlayDisabled, wxImage( 16, 16 ), wxT("PlayDisabled"));
   DEFINE_IMAGE( bmpLoop, wxImage( 16, 16 ), wxT("Loop"));
   DEFINE_IMAGE( bmpLoopDisabled, wxImage( 16, 16 ), wxT("LoopDisabled"));
   DEFINE_IMAGE( bmpStop, wxImage( 16, 16 ), wxT("Stop"));
   DEFINE_IMAGE( bmpStopDisabled, wxImage( 16, 16 ), wxT("StopDisabled"));
   DEFINE_IMAGE( bmpRewind, wxImage( 16, 16 ), wxT("Rewind"));
   DEFINE_IMAGE( bmpRewindDisabled, wxImage( 16, 16 ), wxT("RewindDisabled"));
   DEFINE_IMAGE( bmpFFwd, wxImage( 16, 16 ), wxT("FFwd"));
   DEFINE_IMAGE( bmpFFwdDisabled, wxImage( 16, 16 ), wxT("FFwdDisabled"));
   DEFINE_IMAGE( bmpRecord, wxImage( 16, 16 ), wxT("Record"));
   DEFINE_IMAGE( bmpRecordDisabled, wxImage( 16, 16 ), wxT("RecordDisabled"));
   DEFINE_IMAGE( bmpCleanSpeech, wxImage( 16, 16 ), wxT("CleanSpeech"));
   DEFINE_IMAGE( bmpCleanSpeechDisabled, wxImage( 16, 16 ), wxT("CleanSpeechDisabled"));

   SET_THEME_FLAGS(  resFlagNewLine  );
   DEFINE_IMAGE( bmpUpButtonLarge, wxImage( 48, 48 ), wxT("UpButtonLarge"));
   DEFINE_IMAGE( bmpDownButtonLarge, wxImage( 48, 48 ), wxT("DownButtonLarge"));
   DEFINE_IMAGE( bmpHiliteButtonLarge, wxImage( 48, 48 ), wxT("HiliteButtonLarge"));
   DEFINE_IMAGE( bmpUpButtonSmall, wxImage( 27, 27 ), wxT("UpButtonSmall"));
   DEFINE_IMAGE( bmpDownButtonSmall, wxImage( 27, 27 ), wxT("DownButtonSmall"));
   DEFINE_IMAGE( bmpHiliteButtonSmall, wxImage( 27, 27 ), wxT("HiliteButtonSmall"));
   DEFINE_IMAGE( bmpVolumeSlider, wxImage( 100, 28 ), wxT("VolumeSlider"));
   DEFINE_IMAGE( bmpVolumeSliderThumb, wxImage( 10, 28 ), wxT("VolumeSliderThumb"));
   DEFINE_IMAGE( bmpSliderThumb, wxImage( 11, 14 ), wxT("SliderThumb"));

   SET_THEME_FLAGS(  resFlagNone  );
   DEFINE_IMAGE( bmpMacUpButton, wxImage( 36, 36 ), wxT("MacUpButton"));
   DEFINE_IMAGE( bmpMacDownButton, wxImage( 36, 36 ), wxT("MacDownButton"));
   DEFINE_IMAGE( bmpMacHiliteButton, wxImage( 36, 36 ), wxT("MacHiliteButton"));
   DEFINE_IMAGE( bmpMacUpButtonSmall, wxImage( 27, 27 ), wxT("MacUpButtonSmall"));
   DEFINE_IMAGE( bmpMacDownButtonSmall, wxImage( 27, 27 ), wxT("MacDownButtonSmall"));
   DEFINE_IMAGE( bmpMacHiliteButtonSmall, wxImage( 27, 27 ), wxT("MacHiliteButtonSmall"));
   DEFINE_IMAGE( bmpMacSlider, wxImage( 100, 28 ), wxT("MacSlider"));
   DEFINE_IMAGE( bmpMacSliderThumb, wxImage( 17, 28 ), wxT("MacSliderThumb"));

   SET_THEME_FLAGS(  resFlagInternal  );
   DEFINE_IMAGE( bmpRecoloredUpLarge, wxImage( 48, 48 ), wxT("RecoloredUpLarge"));
   DEFINE_IMAGE( bmpRecoloredDownLarge, wxImage( 48, 48 ), wxT("RecoloredDownLarge"));
   DEFINE_IMAGE( bmpRecoloredHiliteLarge, wxImage( 48, 48 ), wxT("RecoloredHiliteLarge"));
   DEFINE_IMAGE( bmpRecoloredUpSmall, wxImage( 27, 27 ), wxT("RecoloredUpSmall"));
   DEFINE_IMAGE( bmpRecoloredDownSmall, wxImage( 27, 27 ), wxT("RecoloredDownSmall"));
   DEFINE_IMAGE( bmpRecoloredHiliteSmall, wxImage( 27, 27 ), wxT("RecoloredHiliteSmall"));

   SET_THEME_FLAGS(  resFlagCursor  );
   DEFINE_IMAGE( bmpIBeamCursor, wxImage( 32, 32 ), wxT("IBeamCursor"));
   DEFINE_IMAGE( bmpDrawCursor, wxImage( 32, 32 ), wxT("DrawCursor"));
   DEFINE_IMAGE( bmpEnvCursor, wxImage( 32, 32 ), wxT("EnvCursor"));
   DEFINE_IMAGE( bmpTimeCursor, wxImage( 32, 32 ), wxT("TimeCursor"));
   DEFINE_IMAGE( bmpZoomInCursor, wxImage( 32, 32 ), wxT("ZoomInCursor"));
   DEFINE_IMAGE( bmpZoomOutCursor, wxImage( 32, 32 ), wxT("ZoomOutCursor"));
   DEFINE_IMAGE( bmpLabelCursorLeft, wxImage( 32, 32 ), wxT("LabelCursorLeft"));
   DEFINE_IMAGE( bmpLabelCursorRight, wxImage( 32, 32 ), wxT("LabelCursorRight"));
   DEFINE_IMAGE( bmpDisabledCursor, wxImage( 32, 32 ), wxT("DisabledCursor"));

   SET_THEME_FLAGS(  resFlagNone  );
   DEFINE_IMAGE( bmpIBeam, wxImage( 27, 27 ), wxT("IBeam"));
   DEFINE_IMAGE( bmpZoom, wxImage( 27, 27 ), wxT("Zoom"));
   DEFINE_IMAGE( bmpEnvelope, wxImage( 27, 27 ), wxT("Envelope"));
   DEFINE_IMAGE( bmpTimeShift, wxImage( 27, 27 ), wxT("TimeShift"));
   DEFINE_IMAGE( bmpDraw, wxImage( 27, 27 ), wxT("Draw"));
   DEFINE_IMAGE( bmpMulti, wxImage( 27, 27 ), wxT("Multi"));
   DEFINE_IMAGE( bmpMic, wxImage( 25, 25 ), wxT("Mic"));
   DEFINE_IMAGE( bmpSpeaker, wxImage( 25, 25 ), wxT("Speaker"));

   SET_THEME_FLAGS(  resFlagPaired  );
   DEFINE_IMAGE( bmpZoomFit, wxImage( 27, 27 ), wxT("ZoomFit"));
   DEFINE_IMAGE( bmpZoomFitDisabled, wxImage( 27, 27 ), wxT("ZoomFitDisabled"));
   DEFINE_IMAGE( bmpZoomIn, wxImage( 27, 27 ), wxT("ZoomIn"));
   DEFINE_IMAGE( bmpZoomInDisabled, wxImage( 27, 27 ), wxT("ZoomInDisabled"));
   DEFINE_IMAGE( bmpZoomOut, wxImage( 27, 27 ), wxT("ZoomOut"));
   DEFINE_IMAGE( bmpZoomOutDisabled, wxImage( 27, 27 ), wxT("ZoomOutDisabled"));
   DEFINE_IMAGE( bmpZoomSel, wxImage( 27, 27 ), wxT("ZoomSel"));
   DEFINE_IMAGE( bmpZoomSelDisabled, wxImage( 27, 27 ), wxT("ZoomSelDisabled"));
   DEFINE_IMAGE( bmpZoomToggle, wxImage( 27, 27 ), wxT("ZoomToggle"));
   DEFINE_IMAGE( bmpZoomToggleDisabled, wxImage( 27, 27 ), wxT("ZoomToggleDisabled"));
   DEFINE_IMAGE( bmpCut, wxImage( 26, 24 ), wxT("Cut"));
   DEFINE_IMAGE( bmpCutDisabled, wxImage( 26, 24 ), wxT("CutDisabled"));
   DEFINE_IMAGE( bmpCopy, wxImage( 26, 24 ), wxT("Copy"));
   DEFINE_IMAGE( bmpCopyDisabled, wxImage( 26, 24 ), wxT("CopyDisabled"));
   DEFINE_IMAGE( bmpPaste, wxImage( 26, 24 ), wxT("Paste"));
   DEFINE_IMAGE( bmpPasteDisabled, wxImage( 26, 24 ), wxT("PasteDisabled"));
   DEFINE_IMAGE( bmpTrim, wxImage( 26, 24 ), wxT("Trim"));
   DEFINE_IMAGE( bmpTrimDisabled, wxImage( 26, 24 ), wxT("TrimDisabled"));
   DEFINE_IMAGE( bmpSilence, wxImage( 26, 24 ), wxT("Silence"));
   DEFINE_IMAGE( bmpSilenceDisabled, wxImage( 26, 24 ), wxT("SilenceDisabled"));
   DEFINE_IMAGE( bmpUndo, wxImage( 26, 24 ), wxT("Undo"));
   DEFINE_IMAGE( bmpUndoDisabled, wxImage( 26, 24 ), wxT("UndoDisabled"));
   DEFINE_IMAGE( bmpRedo, wxImage( 26, 24 ), wxT("Redo"));
   DEFINE_IMAGE( bmpRedoDisabled, wxImage( 26, 24 ), wxT("RedoDisabled"));

   SET_THEME_FLAGS(  resFlagPaired | resFlagNewLine  );
   DEFINE_IMAGE( bmpTnStartOn, wxImage( 27, 27 ), wxT("TnStartOn"));
   DEFINE_IMAGE( bmpTnStartOnDisabled, wxImage( 27, 27 ), wxT("TnStartOnDisabled"));
   DEFINE_IMAGE( bmpTnStartOff, wxImage( 27, 27 ), wxT("TnStartOff"));
   DEFINE_IMAGE( bmpTnStartOffDisabled, wxImage( 27, 27 ), wxT("TnStartOffDisabled"));
   DEFINE_IMAGE( bmpTnEndOn, wxImage( 27, 27 ), wxT("TnEndOn"));
   DEFINE_IMAGE( bmpTnEndOnDisabled, wxImage( 27, 27 ), wxT("TnEndOnDisabled"));
   DEFINE_IMAGE( bmpTnEndOff, wxImage( 27, 27 ), wxT("TnEndOff"));
   DEFINE_IMAGE( bmpTnEndOffDisabled, wxImage( 27, 27 ), wxT("TnEndOffDisabled"));
   DEFINE_IMAGE( bmpTnCalibrate, wxImage( 27, 27 ), wxT("TnCalibrate"));
   DEFINE_IMAGE( bmpTnCalibrateDisabled, wxImage( 27, 27 ), wxT("TnCalibrateDisabled"));
   DEFINE_IMAGE( bmpTnAutomateSelection, wxImage( 27, 27 ), wxT("TnAutomateSelection"));
   DEFINE_IMAGE( bmpTnAutomateSelectionDisabled, wxImage( 27, 27 ), wxT("TnAutomateSelectionDisabled"));
   DEFINE_IMAGE( bmpTnMakeTag, wxImage( 27, 27 ), wxT("TnMakeTag"));
   DEFINE_IMAGE( bmpTnMakeTagDisabled, wxImage( 27, 27 ), wxT("TnMakeTagDisabled"));
   DEFINE_IMAGE( bmpTnSelectSound, wxImage( 24, 24 ), wxT("TnSelectSound"));
   DEFINE_IMAGE( bmpTnSelectSoundDisabled, wxImage( 24, 24 ), wxT("TnSelectSoundDisabled"));
   DEFINE_IMAGE( bmpTnSelectSilence, wxImage( 24, 24 ), wxT("TnSelectSilence"));
   DEFINE_IMAGE( bmpTnSelectSilenceDisabled, wxImage( 24, 24 ), wxT("TnSelectSilenceDisabled"));

   SET_THEME_FLAGS(  resFlagNone  );
   DEFINE_IMAGE( bmpLabelGlyph0, wxImage( 15, 23 ), wxT("LabelGlyph0"));
   DEFINE_IMAGE( bmpLabelGlyph1, wxImage( 15, 23 ), wxT("LabelGlyph1"));
   DEFINE_IMAGE( bmpLabelGlyph2, wxImage( 15, 23 ), wxT("LabelGlyph2"));
   DEFINE_IMAGE( bmpLabelGlyph3, wxImage( 15, 23 ), wxT("LabelGlyph3"));
   DEFINE_IMAGE( bmpLabelGlyph4, wxImage( 15, 23 ), wxT("LabelGlyph4"));
   DEFINE_IMAGE( bmpLabelGlyph5, wxImage( 15, 23 ), wxT("LabelGlyph5"));
   DEFINE_IMAGE( bmpLabelGlyph6, wxImage( 15, 23 ), wxT("LabelGlyph6"));
   DEFINE_IMAGE( bmpLabelGlyph7, wxImage( 15, 23 ), wxT("LabelGlyph7"));
   DEFINE_IMAGE( bmpLabelGlyph8, wxImage( 15, 23 ), wxT("LabelGlyph8"));
   DEFINE_IMAGE( bmpLabelGlyph9, wxImage( 15, 23 ), wxT("LabelGlyph9"));
   DEFINE_IMAGE( bmpLabelGlyph10, wxImage( 15, 23 ), wxT("LabelGlyph10"));
   DEFINE_IMAGE( bmpLabelGlyph11, wxImage( 15, 23 ), wxT("LabelGlyph11"));

   SET_THEME_FLAGS(  resFlagNewLine  );
   DEFINE_IMAGE( bmpPostfishHome, wxImage( 19, 17 ), wxT("PostfishHome"));
   DEFINE_IMAGE( bmpPostfishFastRewind, wxImage( 17, 17 ), wxT("PostfishFastRewind"));
   DEFINE_IMAGE( bmpPostfishRewind, wxImage( 18, 17 ), wxT("PostfishRewind"));
   DEFINE_IMAGE( bmpPostfishPlay, wxImage( 29, 17 ), wxT("PostfishPlay"));
   DEFINE_IMAGE( bmpPostfishForward, wxImage( 18, 17 ), wxT("PostfishForward"));
   DEFINE_IMAGE( bmpPostfishFastForward, wxImage( 17, 17 ), wxT("PostfishFastForward"));
   DEFINE_IMAGE( bmpPostfishEnd, wxImage( 19, 17 ), wxT("PostfishEnd"));
   DEFINE_IMAGE( bmpPostfishLoop, wxImage( 29, 17 ), wxT("PostfishLoop"));

   SET_THEME_FLAGS(  resFlagNone  );
   DEFINE_IMAGE( bmpDockDown, wxImage( 15, 55 ), wxT("DockDown"));
   DEFINE_IMAGE( bmpDockDownShort, wxImage( 15, 27 ), wxT("DockDownShort"));
   DEFINE_IMAGE( bmpDockOver, wxImage( 15, 55 ), wxT("DockOver"));
   DEFINE_IMAGE( bmpDockOverShort, wxImage( 15, 27 ), wxT("DockOverShort"));
   DEFINE_IMAGE( bmpDockUp, wxImage( 15, 55 ), wxT("DockUp"));
   DEFINE_IMAGE( bmpDockUpShort, wxImage( 15, 27 ), wxT("DockUpShort"));
   DEFINE_IMAGE( bmpToolBarToggle, wxImage( 43, 35 ), wxT("ToolBarToggle"));
   DEFINE_IMAGE( bmpToolBarTarget, wxImage( 17, 26 ), wxT("ToolBarTarget"));
   DEFINE_IMAGE( bmpToolBarGrabber, wxImage( 17, 8 ), wxT("ToolBarGrabber"));
   DEFINE_IMAGE( bmpArrow, wxImage( 9, 16 ), wxT("Arrow"));
   DEFINE_IMAGE( bmpUploadFile, wxImage( 16, 16 ), wxT("UploadFile"));
   DEFINE_IMAGE( bmpUploadFolder, wxImage( 16, 16 ), wxT("UploadFolder"));
   DEFINE_IMAGE( bmpUploadMp3, wxImage( 16, 16 ), wxT("UploadMp3"));
   DEFINE_IMAGE( bmpUploadUp, wxImage( 16, 16 ), wxT("UploadUp"));

   SET_THEME_FLAGS(  resFlagNewLine  );
   DEFINE_IMAGE( bmpAudacityLogo, wxImage( 215, 190 ), wxT("AudacityLogo"));
   DEFINE_IMAGE( bmpAudacityLogo48x48, wxImage( 48, 48 ), wxT("AudacityLogo48x48"));

#else

   // This is older 'bootstrap' code that defined the images using XPMs.
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
   DEFINE_IMAGE(  bmpRecoloredUpLarge, UpButton, wxT("RecoloredUpLarge"));
   DEFINE_IMAGE(  bmpRecoloredDownLarge, DownButton, wxT("RecoloredDownLarge"));
   DEFINE_IMAGE(  bmpRecoloredHiliteLarge,HiliteButton, wxT("RecoloredHiliteLarge"));

   DEFINE_IMAGE(  bmpRecoloredUpSmall, UpButton, wxT("RecoloredUpSmall"));
   DEFINE_IMAGE(  bmpRecoloredDownSmall, DownButton, wxT("RecoloredDownSmall"));
   DEFINE_IMAGE(  bmpRecoloredHiliteSmall,HiliteButton, wxT("RecoloredHiliteSmall"));
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
#endif

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

