/**********************************************************************

  Audacity: A Digital Audio Editor

  ShuttleGui.h

  James Crook

  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

**********************************************************************/

#ifndef __AUDACITY_SHUTTLE_GUI__
#define __AUDACITY_SHUTTLE_GUI__

const int nMaxNestedSizers = 20;

enum teShuttleMode
{
	eIsCreating,
   eIsGettingFromDialog,
   eIsSettingToDialog,
   eIsSavingViaShuttle,
   eIsGettingViaShuttle
};

class wxListCtrl;
class wxCheckBox;
class wxChoice;
class wxComboBox;
class wxScrolledWindow;
class wxTreeCtrl;
class wxTextCtrl;
class wxSlider;
class wxTreeListCtrl;


// JKC: Operations are split between ShuttleGuiBase and ShuttleGui
//    wxWidgets controls are in ShuttleGuiBase.
//    Audacity specific widgets are in ShuttleGui
class ShuttleGuiBase
{
public:
   ShuttleGuiBase(wxWindow * pParent);
   ~ShuttleGuiBase(void);
   void Init();

//-- Add functions.  These only add a widget.
   wxSlider * AddSlider(const wxString &Prompt, int pos);
	wxTreeCtrl * AddTree();
	wxRadioButton * AddRadioButton( const wxString & Prompt );
	wxRadioButton * AddRadioButtonToGroup( const wxString & Prompt);
	wxButton * AddButton( const wxString & Text );
   wxStaticText * AddVariableText(const wxString &Str, bool bCenter = false);
   wxTextCtrl * AddTextBox(const wxString &Caption, const wxString &Value, const int nChars);
   wxListCtrl * AddListControl();
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
   void StartHorizontalLay(int PositionFlags=wxCENTRE, int iProp=1);
   void EndHorizontalLay();
   void StartVerticalLay(int iProp=1);
   void EndVerticalLay();
   wxScrolledWindow * StartScroller(int iStyle=0);
   void EndScroller();
   wxPanel * StartPanel(int iStyle=0);
   void EndPanel();
   void StartMultiColumn(int nCols);
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

//-- Tie functions both add controls and also read/write to them.
   wxTextCtrl * TieTextBox( const wxString &Caption, wxString & Value, const int nChars=0);
   wxChoice * TieCombo( const wxString &Prompt, wxString &Selected, const wxArrayString * pChoices );
   wxSlider * TieSlider( const wxString &Prompt, const float min, const float max, float &f );
   void TieRadioButton( const wxString & Prompt, int iIndex, wxString &Selected);
	void TieTickbox(        const wxString & Prompt, bool & Var );
	void TieTickboxOnRight( const wxString & Prompt, bool & Var );

   void EnableCtrl( bool bEnable );
   void SetBorder( int Border ) {miBorder = Border;};
   

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

   teShuttleMode mShuttleMode;
   wxString mRadioChoice;

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
   ShuttleGui(wxWindow * pParent) : ShuttleGuiBase( pParent)
   {;};
   ~ShuttleGui(void){;};
public:
   ShuttleGui & Id(int id );
   GuiWaveTrack * AddGuiWaveTrack( const wxString & Name);
   AdornedRulerPanel * AddAdornedRuler( ViewInfo *pViewInfo );
   RulerPanel * AddRulerVertical( float low, float hi, const wxString & Units );
   AttachableScrollBar * AddAttachableScrollBar( long style = wxSB_HORIZONTAL );
};

#endif // __AUDACITY_SHUTTLE_GUI__

// Indentation settings for Vim and Emacs
// Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:


