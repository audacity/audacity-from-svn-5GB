// Microsoft Windows specific include file

#define MP3SUPPORT 1
#define USE_LADSPA 1
#define USE_LIBFLAC 1
#define USE_LIBID3TAG 1
#define USE_LIBMAD 1
#define USE_LIBRESAMPLE 1
#undef USE_LIBSAMPLERATE
#define USE_LIBVORBIS 1
#define USE_NYQUIST 1
#define USE_VAMP 1
#define USE_PORTMIXER 1
#define USE_SOUNDTOUCH 1
#define USE_LIBTWOLAME 1
#undef USE_VST

#define INSTALL_PREFIX "."

#define rint(x)   (floor((x)+0.5f)) 

// Override the default wxWidgets control names.
//
// When wxUSE_ACCESSIBILITY is defined, wxWidgets supplies a wxAccessible
// class for each window it creates.  Unfortunately, as part of that process
// it also provides a default name for the window and Windows (or MSAA) passes
// that back to the accessibility aid.
//
// So, for example, all edit boxes are identified to the user as "text" without
// any indication of what the field really is.  And the same thing will happen
// for any wxWidget control that doesn't display its own label.
//
// Controls like the radio button or checkbox do display their own label, so
// these aren't a problem and do not need to be overridden.
//
// To workaround the issue, we override the name with a null string.  When
// Windows (or MSAA) gets the null string, it realizes that it needs to
// perform a search for an appropriate label to present to the user.
//
// In most cases, the search identifies an appropriate label.  However, every
// so often, it will get it wrong or not be able to find one at all.  In these
// cases, you may need to specifically precede a control with a label.
//
// (A good example is the selection bar.  I still haven't figured out what to
// do about that one.)
//
// If you need to override any others controls:
//
// 1)  Add a define to override the class name like:
//     #define wxTextCtrlNameStr overrideTextCtrlNameStr
//
// 2)  Add an extern for the new name like:
//     extern const char *overrideTextCtrlNameStr;
//
// 3)  In "AudacityApp.h", add a null wxString for the new name like:
//     const char *overrideTextCtrlNameStr = wxT("");
//
// 4)  Include "Audacity.h" in the source file creating the control
//
#include <wx/defs.h>

#if wxUSE_ACCESSIBILITY

#define _PTR_

#if wxCHECK_VERSION(2, 8, 0)
extern const wxChar overrideTextCtrlNameStr[];
extern const wxChar overrideChoiceNameStr[];
extern const wxChar overrideComboBoxNameStr[];
extern const wxChar overrideSliderNameStr[];
#else
extern const wxChar *overrideTextCtrlNameStr;
extern const wxChar *overrideChoiceNameStr;
extern const wxChar *overrideComboBoxNameStr;
extern const wxChar *overrideSliderNameStr;
#endif

#define wxTextCtrlNameStr overrideTextCtrlNameStr
#define wxChoiceNameStr overrideChoiceNameStr
#define wxComboBoxNameStr overrideComboBoxNameStr
#define wxSliderNameStr overrideSliderNameStr

#undef _PTR_
#endif

#ifdef _DEBUG
    #ifdef _MSC_VER
        #include <crtdbg.h>
    #endif
#endif

// arch-tag: dcb2defc-1c07-4bae-a9ca-c5377cb470e4

