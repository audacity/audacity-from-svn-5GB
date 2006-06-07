/**********************************************************************

  Audacity: A Digital Audio Editor

  AllThemeResources.h

  James Crook

  Audacity is free software.
  This file is licensed under the GPL license, see License.txt

********************************************************************//*@

\file AllThemeResources.h

This file contains definitions of all images, cursors, colours, fonts
and grids used by Audacity.

This will be split up into separate include files to reduce the amount
of recompilation on a change.

*//*******************************************************************/

// Note: No '#ifndef/#define' pair on this header file.
// we want to include it multiple times in Theme.cpp.

#include "MacroMagic.h"

   // The control buttons...
   DEFINE_IMAGE(  bmpPause, MaskedBmp(Pause, PauseAlpha ),wxT("Pause"));
   DEFINE_IMAGE(  bmpPauseDisabled, MaskedBmp(PauseDisabled,PauseAlpha),wxT("PauseDisabled"));
                  
   DEFINE_IMAGE(  bmpPlay, MaskedBmp(Play,PlayAlpha),wxT("Play"));
   DEFINE_IMAGE(  bmpPlayDisabled, MaskedBmp(PlayDisabled,PlayAlpha),wxT("PlayDisabled"));
                  
   DEFINE_IMAGE(  bmpLoop, MaskedBmp((const char **)Loop,(const char **)LoopAlpha),wxT("Loop"));
   DEFINE_IMAGE(  bmpLoopDisabled, MaskedBmp((const char **)LoopDisabled,(const char **)LoopAlpha),wxT("LoopDisabled"));
                  
   DEFINE_IMAGE(  bmpStop, MaskedBmp(Stop,StopAlpha),wxT("Stop"));
   DEFINE_IMAGE(  bmpStopDisabled, MaskedBmp(StopDisabled,StopAlpha),wxT("StopDisabled"));
                  
   DEFINE_IMAGE(  bmpRewind, MaskedBmp(Rewind,RewindAlpha),wxT("Rewind"));
   DEFINE_IMAGE(  bmpRewindDisabled, MaskedBmp(RewindDisabled,RewindAlpha),wxT("RewindDisabled"));
                  
   DEFINE_IMAGE(  bmpFFwd, MaskedBmp(FFwd,FFwdAlpha),wxT("FFwd"));
   DEFINE_IMAGE(  bmpFFwdDisabled, MaskedBmp(FFwdDisabled,FFwdAlpha),wxT("FFwdDisabled"));
                  
   DEFINE_IMAGE(  bmpRecord, MaskedBmp(Record,RecordAlpha),wxT("Record"));
   DEFINE_IMAGE(  bmpRecordDisabled, MaskedBmp(RecordDisabled,RecordAlpha),wxT("RecordDisabled"));
                  
   DEFINE_IMAGE(  bmpCleanSpeech, MaskedBmp(CleanSpeech,CleanSpeechAlpha),wxT("CleanSpeech"));
   DEFINE_IMAGE(  bmpCleanSpeechDisabled, MaskedBmp(CleanSpeechDisabled,CleanSpeechAlpha),wxT("CleanSpeechDisabled"));
   
   DEFINE_IMAGE(  bmpToolBarToggle, ToolBarToggle,wxT("ToolBarToggle"));
   DEFINE_IMAGE(  bmpToolBarTarget, ToolBarTarget,wxT("ToolBarTarget"));
   DEFINE_IMAGE(  bmpToolBarGrabber, ToolBarGrabber,wxT("ToolBarGrabber"));

   // The backgrounds used for the control buttons.
   SET_THEME_FLAGS( resFlagNone );
   DEFINE_IMAGE(  bmpUpButton, UpButton, wxT("UpButton"));
   DEFINE_IMAGE(  bmpDownButton, DownButton, wxT("DownButton"));
   DEFINE_IMAGE(  bmpHiliteButton, HiliteButton, wxT("HiliteButton"));
                   
   DEFINE_IMAGE(  bmpRecoloredUpButton, UpButton, wxT("RecolouredUpButton"));
   DEFINE_IMAGE(  bmpRecoloredDownButton, DownButton, wxT("RecolouredDownButton"));
   DEFINE_IMAGE(  bmpRecoloredHiliteButton,HiliteButton, wxT("RecolouredHiliteButton"));

   SET_THEME_FLAGS( resFlagPaired | resFlagCursor );
   DEFINE_IMAGE(  bmpIBeamCursor, IBeamCursorXpm, wxT("IBeamCursor"));
   DEFINE_IMAGE(  bmpDrawCursor,DrawCursorXpm, wxT("DrawCursor"));
   DEFINE_IMAGE(  bmpEnvCursor,EnvCursorXpm, wxT("EnvCursor"));
   DEFINE_IMAGE(  bmpTimeCursor,MaskedBmp(TimeCursorXpm,TimeCursorXpm), wxT("TimeCursor"));
   DEFINE_IMAGE(  bmpZoomInCursor,ZoomInCursorXpm, wxT("ZoomInCursor"));
   DEFINE_IMAGE(  bmpZoomOutCursor,ZoomOutCursorXpm, wxT("ZoomOutCursor"));
   DEFINE_IMAGE(  bmpLabelCursorLeft,LabelCursorLeftXpm, wxT("LabelCursorLeft"));
   DEFINE_IMAGE(  bmpLabelCursorRight, LabelCursorRightXpm, wxT("LabelCursorRight"));
   DEFINE_IMAGE(  bmpDisabledCursor,DisabledCursorXpm, wxT("DisabledCursor"));

   SET_THEME_FLAGS( resFlagPaired );
   DEFINE_IMAGE(  bmpCut,MaskedBmp(Cut,CutAlpha), wxT("Cut"));
   DEFINE_IMAGE(  bmpCutDisabled,MaskedBmp(CutDisabled,CutAlpha), wxT("CutDisabled"));
                  
   DEFINE_IMAGE(  bmpCopy,MaskedBmp(Copy,CopyAlpha), wxT("Copy"));
   DEFINE_IMAGE(  bmpCopyDisabled,MaskedBmp(CopyDisabled,CopyAlpha), wxT("CopyDisabled"));
                  
   DEFINE_IMAGE(  bmpPaste,MaskedBmp(Paste,PasteAlpha), wxT("Paste"));
   DEFINE_IMAGE(  bmpPasteDisabled,MaskedBmp(PasteDisabled,PasteAlpha), wxT("PasteDisabled"));
                  
   DEFINE_IMAGE(  bmpTrim,MaskedBmp(Trim,TrimAlpha), wxT("Trim"));
   DEFINE_IMAGE(  bmpTrimDisabled,MaskedBmp(TrimDisabled,TrimAlpha), wxT("TrimDisabled"));
                  
   DEFINE_IMAGE(  bmpSilence,MaskedBmp(Silence,SilenceAlpha), wxT("Silence"));
   DEFINE_IMAGE(  bmpSilenceDisabled,MaskedBmp(SilenceDisabled,SilenceAlpha), wxT("SilenceDisabled"));
                  
   DEFINE_IMAGE(  bmpUndo,MaskedBmp(Undo,UndoAlpha), wxT("Undo"));
   DEFINE_IMAGE(  bmpUndoDisabled,MaskedBmp(UndoDisabled,UndoAlpha), wxT("UndoDisabled"));
                  
   DEFINE_IMAGE(  bmpRedo,MaskedBmp(Redo,RedoAlpha), wxT("Redo"));
   DEFINE_IMAGE(  bmpRedoDisabled,MaskedBmp(RedoDisabled,RedoAlpha), wxT("RedoDisabled"));
                  
   DEFINE_IMAGE(  bmpZoomFit,MaskedBmp(ZoomFit,ZoomFitAlpha), wxT("ZoomFit"));
   DEFINE_IMAGE(  bmpZoomFitDisabled,MaskedBmp(ZoomFitDisabled,ZoomFitAlpha), wxT("ZoomFitDisabled"));
                  
   DEFINE_IMAGE(  bmpZoomIn,MaskedBmp(ZoomIn,ZoomInAlpha), wxT("ZoomIn"));
   DEFINE_IMAGE(  bmpZoomInDisabled,MaskedBmp(ZoomInDisabled,ZoomInAlpha), wxT("ZoomInDisabled"));
                  
   DEFINE_IMAGE(  bmpZoomOut,MaskedBmp(ZoomOut,ZoomOutAlpha), wxT("ZoomOut"));
   DEFINE_IMAGE(  bmpZoomOutDisabled,MaskedBmp(ZoomOutDisabled,ZoomOutAlpha), wxT("ZoomOutDisabled"));
                  
   DEFINE_IMAGE(  bmpZoomSel,MaskedBmp(ZoomSel,ZoomSelAlpha), wxT("ZoomSel"));
   DEFINE_IMAGE(  bmpZoomSelDisabled,MaskedBmp(ZoomSelDisabled,ZoomSelAlpha), wxT("ZoomSelDisabled"));
                  
// DEFINE_IMAGE(  bmpZoomToggle,ZoomT,MaskedBmp(ZoomToggle,ZoomToggleAlpha), wxT(""));
// DEFINE_IMAGE(  bmpZoomToggleDisabl,MaskedBmp(ZoomToggleDisabled,ZoomToggleAlpha), wxT(""));
                  
   DEFINE_IMAGE(  bmpIBeam,MaskedBmp(IBeam,IBeamAlpha), wxT("IBeam"));
   DEFINE_IMAGE(  bmpZoom,MaskedBmp(Zoom,ZoomAlpha), wxT("Zoom"));
   DEFINE_IMAGE(  bmpEnvelope,MaskedBmp(Envelope,EnvelopeAlpha), wxT("Envelope"));
   DEFINE_IMAGE(  bmpTimeShift,MaskedBmp(TimeShift,TimeShiftAlpha), wxT("TimeShift"));
   DEFINE_IMAGE(  bmpDraw,MaskedBmp(Draw,DrawAlpha), wxT("Draw"));
   DEFINE_IMAGE(  bmpMulti,MaskedBmp(Multi,MultiAlpha), wxT("Multi"));

   SET_THEME_FLAGS( resFlagNone );
   DEFINE_IMAGE(  bmpMic,MaskedBmp(Mic,MicAlpha), wxT("Mic"));
   DEFINE_IMAGE(  bmpSpeaker,MaskedBmp(Speaker,SpeakerAlpha), wxT("Speaker"));

   DEFINE_IMAGE(  bmpUp, Up, wxT("Up"));
   DEFINE_IMAGE(  bmpDown, Down, wxT("Down"));
   DEFINE_IMAGE(  bmpHilite, Hilite, wxT("Hilite"));




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

