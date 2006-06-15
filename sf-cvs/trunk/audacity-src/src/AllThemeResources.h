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
                  
   DEFINE_IMAGE(  bmpRewind, MaskedImage(Rewind,RewindAlpha),wxT("Rewind"));
   DEFINE_IMAGE(  bmpRewindDisabled, MaskedImage(RewindDisabled,RewindAlpha),wxT("RewindDisabled"));
                  
   DEFINE_IMAGE(  bmpFFwd, MaskedImage(FFwd,FFwdAlpha),wxT("FFwd"));
   DEFINE_IMAGE(  bmpFFwdDisabled, MaskedImage(FFwdDisabled,FFwdAlpha),wxT("FFwdDisabled"));
                  
   DEFINE_IMAGE(  bmpRecord, MaskedImage(Record,RecordAlpha),wxT("Record"));
   DEFINE_IMAGE(  bmpRecordDisabled, MaskedImage(RecordDisabled,RecordAlpha),wxT("RecordDisabled"));
                  
   DEFINE_IMAGE(  bmpCleanSpeech, MaskedImage(CleanSpeech,CleanSpeechAlpha),wxT("CleanSpeech"));
   DEFINE_IMAGE(  bmpCleanSpeechDisabled, MaskedImage(CleanSpeechDisabled,CleanSpeechAlpha),wxT("CleanSpeechDisabled"));
   
   SET_THEME_FLAGS( resFlagNone );
   DEFINE_IMAGE(  bmpToolBarToggle, ToolBarToggle,wxT("ToolBarToggle"));
   DEFINE_IMAGE(  bmpToolBarTarget, ToolBarTarget,wxT("ToolBarTarget"));
   DEFINE_IMAGE(  bmpToolBarGrabber, ToolBarGrabber,wxT("ToolBarGrabber"));

   // The backgrounds used for the control buttons.
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
   DEFINE_IMAGE(  bmpTimeCursor,MaskedImage(TimeCursorXpm,TimeCursorXpm), wxT("TimeCursor"));
   DEFINE_IMAGE(  bmpZoomInCursor,ZoomInCursorXpm, wxT("ZoomInCursor"));
   DEFINE_IMAGE(  bmpZoomOutCursor,ZoomOutCursorXpm, wxT("ZoomOutCursor"));
   DEFINE_IMAGE(  bmpLabelCursorLeft,LabelCursorLeftXpm, wxT("LabelCursorLeft"));
   DEFINE_IMAGE(  bmpLabelCursorRight, LabelCursorRightXpm, wxT("LabelCursorRight"));
   DEFINE_IMAGE(  bmpDisabledCursor,DisabledCursorXpm, wxT("DisabledCursor"));

   SET_THEME_FLAGS( resFlagPaired );
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
                  
   DEFINE_IMAGE(  bmpZoomFit,MaskedImage(ZoomFit,ZoomFitAlpha), wxT("ZoomFit"));
   DEFINE_IMAGE(  bmpZoomFitDisabled,MaskedImage(ZoomFitDisabled,ZoomFitAlpha), wxT("ZoomFitDisabled"));
                  
   DEFINE_IMAGE(  bmpZoomIn,MaskedImage(ZoomIn,ZoomInAlpha), wxT("ZoomIn"));
   DEFINE_IMAGE(  bmpZoomInDisabled,MaskedImage(ZoomInDisabled,ZoomInAlpha), wxT("ZoomInDisabled"));
                  
   DEFINE_IMAGE(  bmpZoomOut,MaskedImage(ZoomOut,ZoomOutAlpha), wxT("ZoomOut"));
   DEFINE_IMAGE(  bmpZoomOutDisabled,MaskedImage(ZoomOutDisabled,ZoomOutAlpha), wxT("ZoomOutDisabled"));
                  
   DEFINE_IMAGE(  bmpZoomSel,MaskedImage(ZoomSel,ZoomSelAlpha), wxT("ZoomSel"));
   DEFINE_IMAGE(  bmpZoomSelDisabled,MaskedImage(ZoomSelDisabled,ZoomSelAlpha), wxT("ZoomSelDisabled"));
                  
// DEFINE_IMAGE(  bmpZoomToggle,ZoomT,MaskedImage(ZoomToggle,ZoomToggleAlpha), wxT(""));
// DEFINE_IMAGE(  bmpZoomToggleDisabl,MaskedImage(ZoomToggleDisabled,ZoomToggleAlpha), wxT(""));
                  
   DEFINE_IMAGE(  bmpIBeam,MaskedImage(IBeam,IBeamAlpha), wxT("IBeam"));
   DEFINE_IMAGE(  bmpZoom,MaskedImage(Zoom,ZoomAlpha), wxT("Zoom"));
   DEFINE_IMAGE(  bmpEnvelope,MaskedImage(Envelope,EnvelopeAlpha), wxT("Envelope"));
   DEFINE_IMAGE(  bmpTimeShift,MaskedImage(TimeShift,TimeShiftAlpha), wxT("TimeShift"));
   DEFINE_IMAGE(  bmpDraw,MaskedImage(Draw,DrawAlpha), wxT("Draw"));
   DEFINE_IMAGE(  bmpMulti,MaskedImage(Multi,MultiAlpha), wxT("Multi"));

   SET_THEME_FLAGS( resFlagNone );
   DEFINE_IMAGE(  bmpMic,MaskedImage(Mic,MicAlpha), wxT("Mic"));
   DEFINE_IMAGE(  bmpSpeaker,MaskedImage(Speaker,SpeakerAlpha), wxT("Speaker"));

   DEFINE_IMAGE(  bmpUp, Up, wxT("Up"));
   DEFINE_IMAGE(  bmpDown, Down, wxT("Down"));
   DEFINE_IMAGE(  bmpHilite, Hilite, wxT("Hilite"));

   DEFINE_IMAGE(  bmpPostfishHome,        bar_home_xpm, wxT("PostfishHome" ));
   DEFINE_IMAGE(  bmpPostfishFastRewind,  bar_bb_xpm,   wxT("PostfishFastRewind" ));
   DEFINE_IMAGE(  bmpPostfishRewind,      bar_b_xpm,    wxT("PostfishRewind" ));
   DEFINE_IMAGE(  bmpPostfishPlay,        bar_p_xpm,    wxT("PostfishPlay" ));
   DEFINE_IMAGE(  bmpPostfishForward,     bar_f_xpm,    wxT("PostfishForward" ));
   DEFINE_IMAGE(  bmpPostfishFastForward, bar_ff_xpm,   wxT("PostfishFastForward" ));
   DEFINE_IMAGE(  bmpPostfishEnd,         bar_end_xpm,  wxT("PostfishEnd" ));
   DEFINE_IMAGE(  bmpPostfishLoop,        bar_l_xpm,    wxT("PostfishLoop" ));

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

