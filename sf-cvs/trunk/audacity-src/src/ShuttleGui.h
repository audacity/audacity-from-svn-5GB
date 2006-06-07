/**********************************************************************

  Audacity: A Digital Audio Editor

  ShuttleGui.h

  James Crook

  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

**********************************************************************/

#ifndef SHUTTLE_GUI
#define SHUTTLE_GUI

const int nMaxNestedSizers = 20;

enum teShuttleMode
{
	eIsCreating,
   eIsGettingFromDialog,
   eIsSettingToDialog,
   eIsSavingViaShuttle,
   eIsGettingViaShuttle,

   // Next two are only ever seen in constructor.
   // After that they revert to one of the modes above.
   // They are used to achieve 'two step' operation,
   // where we transfer between two shuttles in one go.
   eIsCreatingFromPrefs,
   eIsSavingToPrefs
};

class wxListCtrl;
class wxCheckBox;
class wxChoice;
class wxComboBox;
class wxScrolledWindow;
class wxStaticText;
class wxTreeCtrl;
class wxTextCtrl;
class wxSlider;
class wxTreeListCtrl;
class wxNotebook;
class wxButton;
class wxRadioButton;
class wxBitmap;
class wxPanel;
class wxSizer;
class Shuttle;


/// \brief Base class for shuttling data to and from GUI.
/// 
/// ShuttleGuiBase contains the generic functionality.
///   - wxWidgets controls are in ShuttleGuiBase.
///   - Audacity specific widgets are in ShuttleGui
/// 
/// Use the:
///   - Start/End methods for containers like two-column-layout.
///   - Add methods if you are only interested in creating the controls.
///   - Tie methods if you also want to exchange data using ShuttleGui.
///
class ShuttleGuiBase
{
public:
   ShuttleGuiBase(wxWindow * pParent,teShuttleMode ShuttleMode);
   ~ShuttleGuiBase(void);
   void Init();

//-- Add functions.  These only add a widget.
   void AddPrompt(const wxString &Prompt);
   void AddTitle(const wxString &Prompt);
   wxWindow * AddWindow(wxWindow * pWindow );
   wxSlider * AddSlider(const wxString &Prompt, int pos, int Max);
	wxTreeCtrl * AddTree();
	wxRadioButton * AddRadioButton( const wxString & Prompt );
	wxRadioButton * AddRadioButtonToGroup( const wxString & Prompt);
	wxButton * AddButton( const wxString & Text, int PositionFlags = wxALIGN_CENTRE );
   wxStaticText * AddVariableText(const wxString &Str, bool bCenter = false);
   wxTextCtrl * AddTextBox(const wxString &Caption, const wxString &Value, const int nChars);
   wxTextCtrl * AddTextWindow(const wxString &Value);
   wxListCtrl * AddListControl();
   wxListCtrl * AddListControlReportMode();
   wxCheckBox * AddCheckBox( const wxString &Prompt, const wxString &Selected);
   wxComboBox * AddCombo( const wxString &Prompt, const wxString &Selected,const wxArrayString * pChoices );
   wxChoice * AddChoice( const wxString &Prompt, const wxString &Selected, const wxArrayString * pChoices );
	void AddIcon( wxBitmap * pBmp);
	void AddIconButton( const wxString & Command, const wxString & Params,wxBitmap * pBmp );
	void AddFixedText( const wxString & Str, bool bCenter = false );
   void AddConstTextBox( const wxString &Caption, const wxString & Value );
   void AddTickBox( const wxString &Prompt, const wxString &Selected);
   void AddTickBoxOnRight( const wxString &Prompt, const wxString &Selected);

//-- Start and end functions.  These are used for sizer, or other window containers
//   and create the appropriate widget.
   void StartHorizontalLay(int PositionFlags=wxALIGN_CENTRE, int iProp=1);
   void EndHorizontalLay();
   void StartVerticalLay(int iProp=1);
   void EndVerticalLay();
   wxScrolledWindow * StartScroller(int iStyle=0);
   void EndScroller();
   wxPanel * StartPanel(int iStyle=0);
   void EndPanel();
   void StartMultiColumn(int nCols, int PositionFlags=wxALIGN_LEFT);
   void EndMultiColumn();

   void StartTwoColumn() {StartMultiColumn(2);};
   void EndTwoColumn() {EndMultiColumn();};
   void StartThreeColumn(){StartMultiColumn(3);};
   void EndThreeColumn(){EndMultiColumn();};
   void StartTwoColumnStretchy();

   void StartStatic( const wxString & Str, int iProp=0 );
   void EndStatic();

   wxNotebook * StartNotebook();
   void EndNotebook();
   void StartNotebookPage( const wxString Name );
   void EndNotebookPage();
   wxPanel * StartInvisiblePanel();
   void EndInvisiblePanel();

   void StartRadioButtonGroup( const wxString & SettingName, const int iDefaultValue );
   void StartRadioButtonGroup( const wxString & SettingName, const wxString &DefaultValue );
   void EndRadioButtonGroup();

//-- Tie functions both add controls and also read/write to them.
   wxTextCtrl * TieTextBox( const wxString &Caption, wxString & Value, const int nChars=0);
   wxTextCtrl * TieTextBox( const wxString &Prompt, int &Selected, const int nChars=0);
   wxChoice * TieChoice( const wxString &Prompt, wxString &Selected, const wxArrayString * pChoices );
   wxSlider * TieSlider( const wxString &Prompt, const float min, const float max, float &f );
   wxSlider * TieSlider( const wxString &Prompt, int &pos, const int max );
   void TieRadioButton( const wxString & Prompt, int iIndex, wxString &Selected);
	void TieCheckBox(    const wxString & Prompt, bool & Var );
	void TieCheckBoxOnRight( const wxString & Prompt, bool & Var );

//-- Variants of the standard Tie functions which do two step exchange in one go
// Note that unlike the other Tie functions, ALL the arguments are const.
// That's because the data is being exchanged between the dialog and mpShuttle
// so it doesn't need an argument that is writeable.
   void TieCheckBox( const wxString &Prompt, const wxString &SettingName, const bool bDefault);
   void TieRadioButton( const wxString &Prompt, const int iValue);
   void TieRadioButton( const wxString &Prompt, const wxString &Value);
   wxChoice * TieChoice( 
      const wxString &Prompt, 
      const wxString &SettingName, 
      const wxString &Default, 
      const wxArrayString &Choices,
      const wxArrayString & TranslatedChoices );
   wxChoice * ShuttleGuiBase::TieChoice( 
      const wxString &Prompt, 
      const wxString &SettingName, 
      const int Default, 
      const wxArrayString & Choices,
      const wxArrayInt & TranslatedChoices);
   void ShuttleGuiBase::TieTextBox(
      const wxString &Prompt, 
      const wxString &SettingName, 
      const wxString &Default,
      const int nChars);
   void ShuttleGuiBase::TieTextBox(
      const wxString & Prompt, 
      const wxString & SettingName, 
      const double & Default,
      const int nChars);
//-- End of variants.
   void EnableCtrl( bool bEnable );
   void SetBorder( int Border ) {miBorder = Border;};
   void SetNoMatchSelector( int iSelector ) {miNoMatchSelector = iSelector;};
   void SetSizerProportion( int iProp ) {miSizerProp = iProp;};
   void SetStretchyCol( int i );

protected:
   void UseUpId();
   void PushSizer();
	void PopSizer();

	void UpdateSizersCore( bool bPrepend, int Flags );
   void UpdateSizers();
   void UpdateSizersC();
   void UpdateSizersAtStart();
  
   wxWindow * mpLastWind;
	wxWindow * mpDlg;
	wxSizer * pSizerStack[ nMaxNestedSizers ];
   wxString mBoxName;

   Shuttle * mpShuttle; /*! Controls source/destination of shuttled data.  You can 
   leave this NULL if you are shuttling to variables */
   int miNoMatchSelector; //! Used in choices to determine which item to use on no match.

   teShuttleMode mShuttleMode;

   // These four are needed to handle radio button groups.
   wxString mSettingName; /// The setting controlled by a group.
   int mRadioCount;       /// The index of this radio item.  -1 for none.
   int mRadioValue;       /// The value associated with the active radio button.
   wxString mStrRadioValue; /// mRadioValue, for when values map to strings.

   int miSizerProp;
   int mSizerDepth;
   int miBorder;
   int miProp;

   // See UseUpId() for explanation of these three.
   int miId;
   int miIdNext;
   int miIdSetByUser;

   wxSizer * mpSubSizer;
   wxSizer * mpSizer;
   wxWindow * mpParent;
   wxWindow * mpWind;
};

// A rarely used helper function that sets a pointer
// ONLY if the value it is to be set to is non NULL.
extern void SetIfCreated( wxChoice *&Var, wxChoice * Val );
extern void SetIfCreated( wxTextCtrl *&Var, wxTextCtrl * Val );
extern void SetIfCreated( wxStaticText *&Var, wxStaticText * Val );

class GuiWaveTrack;
class AdornedRulerPanel;
class RulerPanel;
class AttachableScrollBar;
struct ViewInfo;
#include <wx/scrolbar.h>  // to get wxSB_HORIZONTAL

// ShuttleGui extends ShuttleGuiBase with Audacity specific extensions.
class ShuttleGui : public ShuttleGuiBase
{
public:
   ShuttleGui(wxWindow * pParent,teShuttleMode ShuttleMode);
   ~ShuttleGui(void);
public:
   ShuttleGui & Id(int id );
   GuiWaveTrack * AddGuiWaveTrack( const wxString & Name);
   AdornedRulerPanel * AddAdornedRuler( ViewInfo *pViewInfo );
   RulerPanel * AddRulerVertical( float low, float hi, const wxString & Units );
   AttachableScrollBar * AddAttachableScrollBar( long style = wxSB_HORIZONTAL );
};

#endif
