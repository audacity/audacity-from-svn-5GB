/**********************************************************************

  Audacity: A Digital Audio Editor

  ShuttleGui.cpp

  James Crook

  Implements ShuttleGui, ShuttleGuiBase and InvisiblePanel.

  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

**********************************************************************//*!

\file ShuttleGui.cpp
\brief
  ShuttleGui extends the idea of the data shuttle class to include creation 
  of dialog controls.  As part of this it provides an interface to sizers 
  that leads to shorter more readable code.  

  It allows the code that is used to create dialogs to be reused
  to shuttle information in and out.  

  Most of the ShuttleGui functions are defined in ShuttleGuiBase
  which handles wxWidgets.  Audacity widgets are in the derived class.

  The code in this file is fairly repetitive.  We are dealing with
    - Many different types of Widget.
    - Creation / Reading / Writing / Exporting / Importing
    - int, float, string variants (for example of TextCtrl contents).

  Possibly something cleverer with template classes 
  or macros could be done to reduce this code.  

*//******************************************************************/

#include <wx/wx.h>
#include <wx/listctrl.h>
#include <wx/treectrl.h>
#include "Internat.h"
#include "Experimental.h"
#include "ShuttleGui.h"
#include "Shuttle.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

ShuttleGuiBase::ShuttleGuiBase(wxWindow * pParent, teShuttleMode ShuttleMode )
{
   wxASSERT( (pParent != NULL ) || ( ShuttleMode != eIsCreating));

   mpParent = pParent;
   mShuttleMode = ShuttleMode;
   mpDlg = pParent;
   Init();
}

ShuttleGuiBase::~ShuttleGuiBase()
{
}

void ShuttleGuiBase::Init()
{
   mpShuttle = NULL;
   mpSizer = NULL;
   mpWind = NULL;
   mpSubSizer = NULL;

   mSettingName = wxT("");
   mRadioCount = -1;

   miBorder = 5;
   miProp=0;
   miSizerProp=0;
   mSizerDepth=-1;

   miIdSetByUser = -1;
   miId = -1;
   miIdNext = 1000;

   miNoMatchSelector = 0;

   if( mShuttleMode != eIsCreating )
      return;

   mpSizer = mpParent->GetSizer();

#if 0
   if( mpSizer == NULL )
   {
      wxWindow * pGrandParent = mpParent->GetParent();
      if( pGrandParent )
      {
         mpSizer = pGrandParent->GetSizer();
      }
   }
#endif

   if( !mpSizer )
   {
      mpSizer = new wxBoxSizer( wxVERTICAL );
      mpParent->SetSizer( mpSizer );
   }
   PushSizer();
   mpSizer->SetMinSize(250,100);
}

void ShuttleGuiBase::EnableCtrl( bool bEnable )
{
   if( mShuttleMode != eIsCreating )
      return;
   mpLastWind->Enable( bEnable );
}

/// Used to modify an already placed FlexGridSizer to make a column stretchy.
void ShuttleGuiBase::SetStretchyCol( int i )
{
   if( mShuttleMode != eIsCreating )
      return;
   wxFlexGridSizer *pSizer = wxDynamicCast(mpSizer, wxFlexGridSizer);
   wxASSERT( pSizer );
   pSizer->AddGrowableCol( i, 1 );
}

/// Used to modify an already placed FlexGridSizer to make a row stretchy.
void ShuttleGuiBase::SetStretchyRow( int i )
{
   if( mShuttleMode != eIsCreating )
      return;
   wxFlexGridSizer *pSizer = wxDynamicCast(mpSizer, wxFlexGridSizer);
   wxASSERT( pSizer );
   pSizer->AddGrowableRow( i, 1 );
}


//---- Add Functions.

void ShuttleGuiBase::AddPrompt(const wxString &Prompt)
{
   if( Prompt.IsEmpty() )
      return;
   if( mShuttleMode != eIsCreating )
      return;
   miProp=1;
   mpWind = new wxStaticText(mpParent, -1, Prompt, wxDefaultPosition, wxDefaultSize, 
      wxALIGN_RIGHT );
   UpdateSizersCore( false, wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL );
}

void ShuttleGuiBase::AddUnits(const wxString &Prompt)
{
   if( Prompt.IsEmpty() )
      return;
   if( mShuttleMode != eIsCreating )
      return;
   miProp=1;
   mpWind = new wxStaticText(mpParent, -1, Prompt, wxDefaultPosition, wxDefaultSize, 
      wxALIGN_LEFT );
   UpdateSizersCore( false, wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL );
}

void ShuttleGuiBase::AddTitle(const wxString &Prompt)
{
   if( Prompt.IsEmpty() )
      return;
   if( mShuttleMode != eIsCreating )
      return;
   mpWind = new wxStaticText(mpParent, -1, Prompt, wxDefaultPosition, wxDefaultSize, 
      wxALIGN_CENTRE );
   UpdateSizers();
}

wxWindow * ShuttleGuiBase::AddWindow(wxWindow * pWindow )
{
   if( mShuttleMode != eIsCreating )
      return pWindow;
   mpWind = pWindow;
   UpdateSizersC();
   return pWindow;
}


wxCheckBox * ShuttleGuiBase::AddCheckBox( const wxString &Prompt, const wxString &Selected)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxCheckBox);
   wxCheckBox * pCheckBox;
   miProp=0;
   mpWind = pCheckBox = new wxCheckBox(mpParent, miId, Prompt);
   pCheckBox->SetValue(Selected == wxT("true"));
   UpdateSizers();
   return pCheckBox;
}

wxButton * ShuttleGuiBase::AddButton(const wxString &Text, int PositionFlags)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxButton);
   wxButton * pBtn;
   mpWind = pBtn = new wxButton( mpParent, miId, Text );
   miProp=0;
   UpdateSizersCore(false, PositionFlags | wxALL);
   return pBtn;
}

wxBitmapButton * ShuttleGuiBase::AddBitmapButton(const wxBitmap &Bitmap, int PositionFlags)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxBitmapButton);
   wxBitmapButton * pBtn;
   mpWind = pBtn = new wxBitmapButton( mpParent, miId, Bitmap, 
      wxDefaultPosition, wxDefaultSize, wxNO_BORDER );
   pBtn->SetBackgroundColour( 
      wxColour( 246,246,243));
//      wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE));
   miProp=0;
   UpdateSizersCore(false, PositionFlags | wxALL);
   return pBtn;
}

void ShuttleGuiBase::AddTickBox( const wxString &Prompt, const wxString &Selected)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return;
   wxCheckBox * pCheckBox;
   miProp=0;
   wxString Prompt2=Prompt;
   // Prompt2.Replace( wxT("&"), wxT("&&") );
   mpWind = pCheckBox = new wxCheckBox(mpParent, miId, Prompt2);
   pCheckBox->SetValue(Selected==wxT("true"));
   UpdateSizers();
}

/// For a consistant two-column layout we want labels on the left and
/// controls on the right.  TickBoxes break that rule, so we fake it by
/// placing a static text label and then a tick box with an empty label.
void ShuttleGuiBase::AddTickBoxOnRight( const wxString &Prompt, const wxString &Selected)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
//      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wx);
      return;
   wxCheckBox * pCheckBox;
   miProp=0;
   wxString Prompt2=Prompt;
   Prompt2.Replace( wxT("&"), wxT("&&") );
   mpWind = new wxStaticText(mpParent, -1, Prompt2, wxDefaultPosition, wxDefaultSize, 
      wxALIGN_RIGHT );
   UpdateSizers();
   mpWind = pCheckBox = new wxCheckBox(mpParent, miId, wxT(""));
   pCheckBox->SetValue(Selected==wxT("true"));
   UpdateSizers();
}


wxChoice * ShuttleGuiBase::AddChoice( const wxString &Prompt, const wxString &Selected, const wxArrayString * pChoices )
{
// wxCheckBox * pCheckBox;
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxChoice);
   wxChoice * pChoice;
   miProp=0;

   int n = pChoices->GetCount();
   if( n>50 ) n=50;
   int i;
   wxString Choices[50];
   for(i=0;i<n;i++)
   {
      Choices[i] = (*pChoices)[i];
   }

   AddPrompt( Prompt );
   mpWind = pChoice = new wxChoice(
      mpParent,
      miId,
      wxDefaultPosition, 
      wxDefaultSize,
      *pChoices);
//      n, 
//      Choices);
   pChoice->SetSizeHints( 180,-1);// Use -1 for 'default size' - Platform specific.
   pChoice->SetStringSelection( Selected );

   UpdateSizers();
   return pChoice;
}

void ShuttleGuiBase::AddFixedText(const wxString &Str, bool bCenter)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return;
   mpWind = new wxStaticText(mpParent, miId, Str, wxDefaultPosition, wxDefaultSize, 
      wxALIGN_LEFT );
   if( bCenter )
   {
      miProp=1;
      UpdateSizersC();
   }
   else
      UpdateSizers();
}

wxStaticText * ShuttleGuiBase::AddVariableText(const wxString &Str, bool bCenter)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxStaticText);

   wxStaticText *pStatic;
   mpWind = pStatic = new wxStaticText(mpParent, miId, Str, wxDefaultPosition, wxDefaultSize, 
      wxALIGN_LEFT );
   if( bCenter )
   {
      miProp=1;
      UpdateSizersC();
   }
   else
      UpdateSizers();
   return pStatic;
}

wxComboBox * ShuttleGuiBase::AddCombo( const wxString &Prompt, const wxString &Selected,const wxArrayString * pChoices )
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxComboBox);
   wxComboBox * pCombo;
   miProp=0;

   int n = pChoices->GetCount();
   if( n>50 ) n=50;
   int i;
   wxString Choices[50];
   for(i=0;i<n;i++)
   {
      Choices[i] = (*pChoices)[i];
   }

   AddPrompt( Prompt );

   mpWind = pCombo = new wxComboBox(mpParent, miId, Selected, wxDefaultPosition, wxDefaultSize, 
      n, Choices);

   UpdateSizers();
   return pCombo;
}


wxRadioButton * ShuttleGuiBase::AddRadioButton(const wxString &Prompt)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxRadioButton);
   wxRadioButton * pRad;
   mpWind = pRad = new wxRadioButton( mpParent, miId, Prompt,
      wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
   pRad->SetValue(true );
   UpdateSizers();
   return pRad;
}

wxRadioButton * ShuttleGuiBase::AddRadioButtonToGroup(const wxString &Prompt)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxRadioButton);
   wxRadioButton * pRad;
   mpWind = pRad = new wxRadioButton( mpParent, miId, Prompt );
   UpdateSizers();
   return pRad;
}

wxSlider * ShuttleGuiBase::AddSlider(const wxString &Prompt, int pos, int Max)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxSlider);
   AddPrompt( Prompt );
   wxSlider * pSlider;
   mpWind = pSlider = new wxSlider( mpParent, miId, 
      pos, 0, Max, 
      wxDefaultPosition, wxDefaultSize,
      wxSL_HORIZONTAL | wxSL_LABELS | wxSL_AUTOTICKS
      );
   miProp=1;
   UpdateSizers();
   return pSlider;
}


wxTextCtrl * ShuttleGuiBase::AddTextBox(const wxString &Caption, const wxString &Value, const int nChars)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxTextCtrl);
   wxTextCtrl * pTextCtrl;
   wxSize Size(wxDefaultSize);
   if( nChars > 0 )
   {
      Size.SetWidth( nChars *5 );
   }
   AddPrompt( Caption );
   miProp=0;

#ifdef RIGHT_ALIGNED_TEXTBOXES
   long flags = wxTE_RIGHT;
#else
   long flags = wxTE_LEFT;
#endif

   mpWind = pTextCtrl = new wxTextCtrl(mpParent, miId, Value,
      wxDefaultPosition, Size, flags);
   UpdateSizers();
   return pTextCtrl;
}

wxTextCtrl * ShuttleGuiBase::AddTextWindow(const wxString &Value)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxTextCtrl);
   wxTextCtrl * pTextCtrl;
   miProp=1;
   mpWind = pTextCtrl = new wxTextCtrl(mpParent, miId, Value, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE);
   UpdateSizers();
   return pTextCtrl;
}


void ShuttleGuiBase::AddConstTextBox(const wxString &Caption, const wxString &Value)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return;
//      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wx);
   miProp=0;
   AddPrompt( Caption );
   UpdateSizers();
   miProp=0;
   mpWind = new wxStaticText(mpParent, miId, Value);
   UpdateSizers();
}


wxListCtrl * ShuttleGuiBase::AddListControl()
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxListCtrl);
   wxListCtrl * pListCtrl;
   miProp=1;
   mpWind = pListCtrl = new wxListCtrl(mpParent, miId);
   pListCtrl->SetMinSize( wxSize( 120,150 ));
   UpdateSizers();
   return pListCtrl;
}

wxListCtrl * ShuttleGuiBase::AddListControlReportMode()
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxListCtrl);
   wxListCtrl * pListCtrl;
   miProp=1;
   mpWind = pListCtrl = new wxListCtrl(mpParent, miId,
      wxDefaultPosition, wxSize(230,120),//wxDefaultSize,
      wxLC_REPORT | wxLC_HRULES | wxLC_VRULES | wxSUNKEN_BORDER );
//   pListCtrl->SetMinSize( wxSize( 120,150 ));
   UpdateSizers();
   return pListCtrl;
}

wxTreeCtrl * ShuttleGuiBase::AddTree()
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxTreeCtrl);
   wxTreeCtrl * pTreeCtrl;
   miProp=1;
   mpWind = pTreeCtrl = new wxTreeCtrl(mpParent, miId);
   pTreeCtrl->SetMinSize( wxSize( 120,650 ));
   UpdateSizers();
   return pTreeCtrl;
}

void ShuttleGuiBase::AddIcon(wxBitmap *pBmp)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
//      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wx);
      return;
   wxBitmapButton * pBtn;
   mpWind = pBtn = new wxBitmapButton( mpParent, miId, *pBmp  );
   pBtn->SetWindowStyle( 0 );
   UpdateSizersC();
}

/// Starts a static box around a number of controls.
///  @param Str   The text of the title for the box.
///  @param iProp The resizing proportion value.  
/// Use iProp == 0 for a minimum sized static box.
/// Use iProp == 1 for a box that grows if there is space to spare.
void ShuttleGuiBase::StartStatic(const wxString &Str, int iProp)
{
   UseUpId();
   mBoxName = Str;
   if( mShuttleMode != eIsCreating )
      return;
   mpSubSizer = new wxStaticBoxSizer( 
      new wxStaticBox(mpParent, miId, 
      Str ),
      wxVERTICAL );
   miSizerProp = iProp;
   UpdateSizers();
}

void ShuttleGuiBase::EndStatic()
{
   if( mShuttleMode != eIsCreating )
      return;
   PopSizer();
}

/// This allows subsequent controls and static boxes to be in 
/// a scrolled panel.  Very handy if you are running out of space
/// on a dialog.
///
/// The iStyle parameter is used in some very hacky code that
/// dynamically repopulates a dialog.  It also controls the 
/// background colour.  Look at the code for details.
///  @param istyle deprecated parameter, but has been used for hacking. 
wxScrolledWindow * ShuttleGuiBase::StartScroller(int iStyle)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxScrolledWindow);
      
   wxScrolledWindow * pScroller;
   mpWind = pScroller = new wxScrolledWindow( mpParent, miId, wxDefaultPosition, wxDefaultSize,
      wxSUNKEN_BORDER);
   pScroller->SetScrollRate( 20,20 );

   mpWind->SetBackgroundColour( 
      iStyle==0 
      ? wxColour( 190,200,230) :
      wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOW) 
      );
   miProp=1;
   if( iStyle==2 )
   {
      UpdateSizersAtStart();
   }
   else
   {
      UpdateSizers();  // adds window in to current sizer.
   }

   // create a sizer within the window...
   mpParent = pScroller;
   mpSizer = new wxBoxSizer( wxVERTICAL );
   pScroller->SetSizer( mpSizer );
   PushSizer();
   return pScroller;
}

void ShuttleGuiBase::EndScroller()
{
   if( mShuttleMode != eIsCreating )
      return;
   wxSize ScrollSize = mpSizer->GetMinSize();
   int yMin = ScrollSize.y+4;
   int xMin = ScrollSize.x+4;
   if( yMin > 400)
   {
      yMin = 400;
      xMin+=50;// extra space for vertical scrollbar.
   }

   mpParent->SetMinSize( wxSize(xMin, yMin) );

   PopSizer();
   mpParent = mpParent->GetParent(); 
}

wxPanel * ShuttleGuiBase::StartPanel(int iStyle)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxPanel);
   wxPanel * pPanel;
   mpWind = pPanel = new wxPanel( mpParent, miId, wxDefaultPosition, wxDefaultSize,
      wxNO_BORDER);

   mpWind->SetBackgroundColour( 
      iStyle==0 
      ? wxColour( 190,200,230) :
      wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOW) 
      );
   miProp=0;
   miBorder=2;
   UpdateSizers();  // adds window in to current sizer.

   // create a sizer within the window...
   mpParent = pPanel;
   mpSizer = new wxBoxSizer( wxVERTICAL );
   pPanel->SetSizer( mpSizer );
   PushSizer();
   return pPanel;
}

void ShuttleGuiBase::EndPanel()
{
   if( mShuttleMode != eIsCreating )
      return;
   PopSizer();
   mpParent = mpParent->GetParent(); 
}

wxNotebook * ShuttleGuiBase::StartNotebook()
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxNotebook);
   wxNotebook * pNotebook;
   mpWind = pNotebook = new wxNotebook(mpParent, 
      miId, wxDefaultPosition, wxDefaultSize);
   miProp=1;
   UpdateSizers();
   mpParent = pNotebook;
   return pNotebook;
}

void ShuttleGuiBase::EndNotebook()
{
   //PopSizer();
   mpParent = mpParent->GetParent(); 
}


void ShuttleGuiBase::StartNotebookPage( const wxString Name )
{
   if( mShuttleMode != eIsCreating )
      return;
//      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wx);
   wxNotebook * pNotebook = (wxNotebook*)mpParent;
   wxNotebookPage * pPage = new wxPanel(mpParent ); 
   pNotebook->AddPage( 
      pPage, 
      Name);
   PushSizer();

   miProp=1;
   mpParent = pPage;
   mpSizer = new wxBoxSizer( wxVERTICAL );
   mpSizer->SetMinSize(250,500);
   pPage->SetSizer( mpSizer );
//   UpdateSizers();
}

void ShuttleGuiBase::EndNotebookPage()
{
   if( mShuttleMode != eIsCreating )
      return;
   PopSizer();
   mpParent = mpParent->GetParent(); 
}


/// An InvisiblePanel is a panel which does not repaint its own background.

/// It is used (a) To group together widgets which need to be refreshed 
/// together.  A single refresh of the panel causes all the subwindows to
/// refresh.  (b) as a base class for some flicker-free classes for which
/// the backgorund is never repainted.
class InvisiblePanel : public wxPanel
{
public:
   InvisiblePanel( 
      wxWindow* parent, 
      wxWindowID id = -1, 
      const wxPoint& pos = wxDefaultPosition, 
      const wxSize& size = wxDefaultSize, 
      long style = wxTAB_TRAVERSAL ) :
      wxPanel( parent, id, pos, size, style )
   {
   };
   ~InvisiblePanel(){;};
   void OnPaint( wxPaintEvent &event );
   void OnErase(wxEraseEvent &evt){;};
   DECLARE_EVENT_TABLE()  
};


BEGIN_EVENT_TABLE(InvisiblePanel, wxPanel)
//   EVT_PAINT(InvisiblePanel::OnPaint)
     EVT_ERASE_BACKGROUND( InvisiblePanel::OnErase)
END_EVENT_TABLE()

void InvisiblePanel::OnPaint( wxPaintEvent &event )
{
   // Don't repaint my background...
   wxPaintDC dc(this);
// event.Skip();
   ;// swallow the paint event.
}


wxPanel * ShuttleGuiBase::StartInvisiblePanel()
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), wxPanel);
   wxPanel * pPanel;
   mpWind = pPanel = new wxPanel( mpParent, miId, wxDefaultPosition, wxDefaultSize,
      wxNO_BORDER);

   mpWind->SetBackgroundColour( 
      wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE) 
      );
   miProp=1;
   miBorder=0;
   UpdateSizers();  // adds window in to current sizer.

   // create a sizer within the window...
   mpParent = pPanel;
   mpSizer = new wxBoxSizer( wxVERTICAL );
   pPanel->SetSizer( mpSizer );
   PushSizer();
   return pPanel;
}

void ShuttleGuiBase::EndInvisiblePanel()
{
   EndPanel();
}


/// Starts a Horizontal Layout.
///  - Use wxEXPAND and 0 to expand horizontally but not vertically.
///  - Use wxEXPAND and 1 to expand horizontally and vertically.
///  - Use wxCENTRE and 1 for no expansion.
/// @param PositionFlag  Typically wxEXPAND or wxCENTRE.
/// @param iProp         Proportionality for resizing.  
void ShuttleGuiBase::StartHorizontalLay( int PositionFlags, int iProp)
{
   if( mShuttleMode != eIsCreating )
      return;
   miSizerProp=iProp;
   mpSubSizer = new wxBoxSizer( wxHORIZONTAL );
   UpdateSizersCore( false, PositionFlags | wxALL );
}

void ShuttleGuiBase::EndHorizontalLay()
{
   if( mShuttleMode != eIsCreating )
      return;
   PopSizer();
}

void ShuttleGuiBase::StartVerticalLay(int iProp)
{
   if( mShuttleMode != eIsCreating )
      return;
   miSizerProp=iProp;
   mpSubSizer = new wxBoxSizer( wxVERTICAL );
   UpdateSizers();
}

void ShuttleGuiBase::EndVerticalLay()
{
   if( mShuttleMode != eIsCreating )
      return;
   PopSizer();
}

void ShuttleGuiBase::StartMultiColumn(int nCols, int PositionFlags)
{
   if( mShuttleMode != eIsCreating )
      return;
   mpSubSizer = new wxFlexGridSizer( nCols );
   UpdateSizersCore( false, PositionFlags | wxALL );
}

void ShuttleGuiBase::EndMultiColumn()
{
   if( mShuttleMode != eIsCreating )
      return;
   PopSizer();
}

void ShuttleGuiBase::TieCheckBox(const wxString &Prompt, bool &Var)
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode != eIsCreating )
      UseUpId();

   switch( mShuttleMode )
   {
   // IF Creating the dialog controls and setting them from internal storage.
   case eIsCreating:
      {
         AddTickBox( Prompt, Var ? wxT("true") : wxT("false"));
      }
      break;
   // IF setting internal storage from the controls.
   case eIsGettingFromDialog:
      {
         wxWindow * pWnd      = wxWindow::FindWindowById( miId, mpDlg);
         wxCheckBox * pCheckBox = wxDynamicCast(pWnd, wxCheckBox);
         wxASSERT( pCheckBox );
         Var = pCheckBox->GetValue();
      }
      break;
   case eIsSettingToDialog:
      {
         wxWindow * pWnd      = wxWindow::FindWindowById( miId, mpDlg);
         wxCheckBox * pCheckBox = wxDynamicCast(pWnd, wxCheckBox);
         wxASSERT( pCheckBox );
         pCheckBox->SetValue( Var );
      }
      break;
   // IF Saving settings to external storage...
   // or IF Getting settings from external storage.
   case eIsSavingViaShuttle:
   case eIsGettingViaShuttle:
      {
         wxASSERT( mpShuttle );
         mpShuttle->TransferBool( Prompt, Var, Var );
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
}
// See comment in AddTickBoxOnRight() for why we have this variant.
void ShuttleGuiBase::TieCheckBoxOnRight(const wxString &Prompt, bool &Var)
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode != eIsCreating )
      UseUpId();

   switch( mShuttleMode )
   {
   // IF Creating the dialog controls and setting them from internal storage.
   case eIsCreating:
      {
         AddTickBoxOnRight( Prompt, Var ? wxT("true") : wxT("false"));
      }
      break;
   // IF setting internal storage from the controls.
   case eIsGettingFromDialog:
      {
         wxWindow * pWnd      = wxWindow::FindWindowById( miId, mpDlg);
         wxCheckBox * pCheckBox = wxDynamicCast(pWnd, wxCheckBox);
         wxASSERT( pCheckBox );
         Var = pCheckBox->GetValue();
      }
      break;
   case eIsSettingToDialog:
      {
         wxWindow * pWnd      = wxWindow::FindWindowById( miId, mpDlg);
         wxCheckBox * pCheckBox = wxDynamicCast(pWnd, wxCheckBox);
         wxASSERT( pCheckBox );
         pCheckBox->SetValue( Var );
      }
      break;
   // IF Saving settings to external storage...
   case eIsSavingViaShuttle:
      {
//         wxASSERT(false);
      }
      break;
   // IF Getting settings from external storage.
   case eIsGettingViaShuttle:
      {
//         wxASSERT(false);
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
}

wxSlider * ShuttleGuiBase::TieSlider( const wxString &Prompt, const float min, const float max, float &f )
{
   wxASSERT( false );// This version needs fixing.
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode != eIsCreating )
      UseUpId();
   wxSlider * pSlider=NULL;
   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         pSlider = AddSlider( Prompt, 100*(f-min)/(max-min), 100 );
      }
      break;
   // IF setting internal storage from the controls.
   case eIsGettingFromDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         pSlider = wxDynamicCast(pWnd, wxSlider);
         wxASSERT( pSlider );
         f = (pSlider->GetValue() / 100.0f) * (max-min) + min;
      }
      break;
   case eIsSettingToDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         pSlider = wxDynamicCast(pWnd, wxSlider);
         wxASSERT( pSlider );
         pSlider->SetValue( 100*(f-min)/(max-min) );
      }
      break;
   // IF Saving settings to external storage...
   // or IF Getting settings from external storage.
   case eIsSavingViaShuttle:
   case eIsGettingViaShuttle:
   default:
      wxASSERT( false );
      break;
   }
   return pSlider;
}

wxSlider * ShuttleGuiBase::TieSlider( const wxString &Prompt, int &pos, const int max )
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode != eIsCreating )
      UseUpId();
   wxSlider * pSlider=NULL;
   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         pSlider = AddSlider( Prompt, pos, max );
      }
      break;
   // IF setting internal storage from the controls.
   case eIsGettingFromDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         pSlider = wxDynamicCast(pWnd, wxSlider);
         wxASSERT( pSlider );
         pos = pSlider->GetValue();
      }
      break;
   case eIsSettingToDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         pSlider = wxDynamicCast(pWnd, wxSlider);
         wxASSERT( pSlider );
         pSlider->SetValue( pos );
      }
      break;
   // IF Saving settings to external storage...
   // or IF Getting settings from external storage.
   case eIsSavingViaShuttle:
   case eIsGettingViaShuttle:
   default:
      wxASSERT( false );
      break;
   }
   return pSlider;
}

wxTextCtrl * ShuttleGuiBase::TieTextBox( const wxString &Prompt, wxString &Selected, const int nChars)
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode != eIsCreating )
      UseUpId();
   wxTextCtrl * pTextBox=NULL;
   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         pTextBox = AddTextBox( Prompt, Selected, nChars );
      }
      break;
   // IF setting internal storage from the controls.
   case eIsGettingFromDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         pTextBox = wxDynamicCast(pWnd, wxTextCtrl);
         wxASSERT( pTextBox );
         Selected = pTextBox->GetValue();
      }
      break;
   case eIsSettingToDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         pTextBox = wxDynamicCast(pWnd, wxTextCtrl);
         wxASSERT( pTextBox );
         pTextBox->SetValue( Selected );
      }
      break;
   // IF Saving settings to external storage...
   // or IF Getting settings from external storage.
   case eIsGettingViaShuttle:
   case eIsSavingViaShuttle:
      {
         wxASSERT( mpShuttle );
         mpShuttle->TransferString( Prompt, Selected, Selected );
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
   return pTextBox;
}


wxTextCtrl * ShuttleGuiBase::TieTextBox( const wxString &Prompt, int &Selected, const int nChars)
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode != eIsCreating )
      UseUpId();
   wxTextCtrl * pTextBox=NULL;
   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         pTextBox = AddTextBox( Prompt, wxString::Format( wxT("%i"), Selected), nChars );
      }
      break;
   // IF setting internal storage from the controls.
   case eIsGettingFromDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         pTextBox = wxDynamicCast(pWnd, wxTextCtrl);
         wxASSERT( pTextBox );
         Selected = wxAtoi(pTextBox->GetValue());
      }
      break;
   case eIsSettingToDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         pTextBox = wxDynamicCast(pWnd, wxTextCtrl);
         wxASSERT( pTextBox );
         pTextBox->SetValue( wxString::Format( wxT("%i"), Selected ));
      }
      break;
   // IF Saving settings to external storage...
   // or IF Getting settings from external storage.
   case eIsGettingViaShuttle:
   case eIsSavingViaShuttle:
      {
         wxASSERT( mpShuttle );
         mpShuttle->TransferInt( Prompt, Selected, Selected );
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
   return pTextBox;
}

wxChoice * ShuttleGuiBase::TieChoice( 
   const wxString &Prompt, 
   wxString &Selected, 
   const wxArrayString * pChoices )
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode != eIsCreating )
      UseUpId();
   wxChoice * pChoice=NULL;
   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         pChoice = AddChoice( Prompt, Selected, pChoices );
      }
      break;
   // IF setting internal storage from the controls.
   case eIsGettingFromDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         pChoice = wxDynamicCast(pWnd, wxChoice);
         wxASSERT( pChoice );
         Selected = pChoice->GetStringSelection();
      }
      break;
   case eIsSettingToDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         pChoice = wxDynamicCast(pWnd, wxChoice);
         wxASSERT( pChoice );
         pChoice->SetStringSelection( Selected );
      }
      break;
   // IF Saving settings to external storage...
   // or IF Getting settings from external storage.
   case eIsSavingViaShuttle:
   case eIsGettingViaShuttle:
      {
         wxASSERT( mpShuttle );
         // HACK: Determine whether a string or integer, based on
         // choices.
         // Perhaps we should have a different TieChoice
         // to cover such cases?
         wxString Temp;
         Temp = wxString::Format( wxT("%i"), wxAtoi( (*pChoices)[0] ));
         if( Temp.IsSameAs( (*pChoices)[0] ))
         {
            // Another HACK: make up for Selected being a string..
            int Value=wxAtoi( Selected );
            mpShuttle->TransferInt( Prompt, Value, Value );
            Selected = wxString::Format( wxT("%i"), Value );
         }
         else
         {
            mpShuttle->TransferString( Prompt, Selected, Selected );
         }
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
   return pChoice;
}

void ShuttleGuiBase::TieRadioButton(const wxString &Prompt, int iIndex, wxString &Selected)
{
   UseUpId();
   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         wxRadioButton * pRad;
         mpWind = pRad = new wxRadioButton( mpParent, miId, Prompt,
            wxDefaultPosition, wxDefaultSize, 
            (iIndex==0)?wxRB_GROUP:0);
//         pRad->SetValue(iIndex == 0);
         pRad->SetValue(Selected==Prompt);
         UpdateSizers();
      }
      break;
   // IF setting internal storage from the controls.
   case eIsGettingFromDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         wxRadioButton * pRadioButton = wxDynamicCast(pWnd, wxRadioButton);
         wxASSERT( pRadioButton );
         if( pRadioButton->GetValue() )
         {
            Selected = pRadioButton->GetLabel();
         }
      }
      break;
   case eIsSettingToDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         wxRadioButton * pRadioButton = wxDynamicCast(pWnd, wxRadioButton);
         wxASSERT( pRadioButton );
         if( pRadioButton->GetLabel() == Selected )
         {
            pRadioButton->SetValue( true );
         }
      }
      break;
   // IF Saving settings to external storage...
   // or IF Getting settings from external storage.
   case eIsSavingViaShuttle:
   case eIsGettingViaShuttle:
      {
         wxASSERT(mpShuttle);
         // Nothing to do here since exchange with the shuttle is done by the 
         // group...
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
}

//-----------------------------------------------------------------------//


// ShuttleGui code uses the model that you read into program variables
// and write out from program variables.

// In programs like Audacity which don't use internal program variables
// you have to do both steps in one go, using variants of the standard
// 'Tie' functions which call the underlying Tie functions twice.

//----------------------------------------------------------------------//

/// Variant of the standard TieCheckBox which does the two step exchange 
/// between gui and stack variable and stack variable and shuttle.
void ShuttleGuiBase::TieCheckBox(
   const wxString &Prompt, 
   const wxString &SettingName, 
   const bool bDefault)
{
   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         bool bValue = bDefault;
         mShuttleMode = eIsGettingViaShuttle;
         TieCheckBox( SettingName, bValue );
         miIdSetByUser = miId; // Reset the id, we're about to reuse it.
         mShuttleMode = eIsCreating;
         TieCheckBox( Prompt, bValue );
      }
      break;
   case eIsGettingFromDialog:
      {
         bool bValue = bDefault;
         TieCheckBox( Prompt, bValue );
         miIdSetByUser = miId; // Reset the id, we're about to reuse it.
         mShuttleMode = eIsSavingViaShuttle;
         TieCheckBox( SettingName, bValue );
         mShuttleMode = eIsGettingFromDialog;
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
}


void ShuttleGuiBase::StartRadioButtonGroup( const wxString & SettingName, const int iDefaultValue )
{
   mSettingName = SettingName;
   mRadioCount = 0;
   mRadioValue = iDefaultValue;
   mStrRadioValue = wxT("USE_INTEGERS"); // a b
   if( mShuttleMode == eIsCreating )
   {
      if( mpShuttle )
      {
         mpShuttle->TransferInt( SettingName, mRadioValue, iDefaultValue );
      }
   }
}

void ShuttleGuiBase::StartRadioButtonGroup( const wxString & SettingName, const wxString & DefaultValue )
{
   mSettingName = SettingName;
   mRadioCount = 0;
   mStrRadioValue = DefaultValue;
   mRadioValue = -1;
   if( mShuttleMode == eIsCreating )
   {
      if( mpShuttle )
      {
         mpShuttle->TransferString( SettingName, mStrRadioValue, DefaultValue );
      }
   }
}


void ShuttleGuiBase::EndRadioButtonGroup()
{
   if( mShuttleMode == eIsGettingFromDialog )
   {
      if( mpShuttle )
      {
         if( mStrRadioValue == wxT("USE_INTEGERS") )
         {
            mpShuttle->TransferInt( mSettingName, mRadioValue, mRadioValue );
         }
         else
         {
            mpShuttle->TransferString( mSettingName, mStrRadioValue, mStrRadioValue );
         }
      }
   }
   mSettingName = wxT("");
   mRadioCount = -1; // So we detect a problem.
}

/// Variant of the standard TieRadioButton which does the two step exchange 
/// between gui and stack variable and stack variable and shuttle.
/// This one must be within a StartRadioButtonGroup - EndRadioButtonGroup pair.
void ShuttleGuiBase::TieRadioButton( 
   const wxString &Prompt, 
   int iValue)
{
   /// \todo Current implementation is stand alone and does not use the basic
   /// TieRadioButton.  Should change that.
   wxASSERT( mRadioCount >= 0); // Did you remember to use StartRadioButtonGroup() ?
   mRadioCount++;
   UseUpId();

   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         wxRadioButton * pRad;
         mpWind = pRad = new wxRadioButton( mpParent, miId, Prompt,
            wxDefaultPosition, wxDefaultSize, 
            (mRadioCount==1)?wxRB_GROUP:0);
         pRad->SetValue(iValue==mRadioValue);
         UpdateSizers();
      }
      break;
   case eIsGettingFromDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         wxRadioButton * pRadioButton = wxDynamicCast(pWnd, wxRadioButton);
         wxASSERT( pRadioButton );
         if( pRadioButton->GetValue() )
         {
            mRadioValue = iValue;
         }
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
}

/// Variant of the standard TieRadioButton which does the two step exchange 
/// between gui and stack variable and stack variable and shuttle.
/// This one must be within a StartRadioButtonGroup - EndRadioButtonGroup pair.
void ShuttleGuiBase::TieRadioButton( 
   const wxString &Prompt, 
   const wxString &Value)
{
   /// \todo Current implementation is stand alone and does not use the basic
   /// TieRadioButton.  Should change that.
   wxASSERT( mRadioCount >= 0); // Did you remember to use StartRadioButtonGroup() ?
   mRadioCount++;
   UseUpId();

   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         wxRadioButton * pRad;
         mpWind = pRad = new wxRadioButton( mpParent, miId, Prompt,
            wxDefaultPosition, wxDefaultSize, 
            (mRadioCount==1)?wxRB_GROUP:0);
         pRad->SetValue(Value==mStrRadioValue);
         UpdateSizers();
      }
      break;
   case eIsGettingFromDialog:
      {
         wxWindow * pWnd  = wxWindow::FindWindowById( miId, mpDlg);
         wxRadioButton * pRadioButton = wxDynamicCast(pWnd, wxRadioButton);
         wxASSERT( pRadioButton );
         if( pRadioButton->GetValue() )
         {
            mStrRadioValue = Value;
         }
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
}

/// Variant of the standard TieChoice which does the two step exchange 
/// between gui and stack variable and stack variable and shuttle.
///   @param Prompt             The prompt shown beside the control.
///   @param SettingName        The setting name as stored in gPrefs
///   @parsm Default            The default value for this control (translated)
///   @param Choices            An array of choices that appear on screen.
///   @param TranslatedChoices  The correcponding values (as a string array)
wxChoice * ShuttleGuiBase::TieChoice( 
   const wxString &Prompt, 
   const wxString &SettingName, 
   const wxString &Default, 
   const wxArrayString & Choices,
   const wxArrayString & TranslatedChoices)
{
   wxChoice * pChoice=(wxChoice*)NULL;
   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         int n;
         wxString strValue = Default;
         mShuttleMode = eIsGettingViaShuttle;
         pChoice = TieChoice( SettingName, strValue, &TranslatedChoices );
         // Translate from translated to display version
         n = TranslatedChoices.Index( strValue );
         if( n== wxNOT_FOUND )
            n=miNoMatchSelector;
         miNoMatchSelector=0;
         strValue = Choices[n];
         miIdSetByUser = miId; // Reset the id, we're about to reuse it.
         mShuttleMode = eIsCreating;
         TieChoice( Prompt, strValue, &Choices );
      }
      break;
   case eIsGettingFromDialog:
      {
         int n;
         wxString strValue = Default;
         pChoice = TieChoice( Prompt, strValue, &Choices );
         // Translate from display version to translated version
         n = Choices.Index( strValue );
         if( n== wxNOT_FOUND )
            n=miNoMatchSelector;
         miNoMatchSelector=0;
         strValue = TranslatedChoices[n];
         miIdSetByUser = miId; // Reset the id, we're about to reuse it.
         mShuttleMode = eIsSavingViaShuttle;
         TieChoice( SettingName, strValue, &TranslatedChoices );
         mShuttleMode = eIsGettingFromDialog;
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
   return pChoice;
}

/// Variant of the standard TieChoice which does the two step exchange 
/// between gui and stack variable and stack variable and shuttle.
/// Difference to previous one is that the Translated choices and default
/// are integers, not Strings.
///   @param Prompt             The prompt shown beside the control.
///   @param SettingName        The setting name as stored in gPrefs
///   @parsm Default            The default value for this control (translated)
///   @param Choices            An array of choices that appear on screen.
///   @param TranslatedChoices  The correcponding values (as a string array)
wxChoice * ShuttleGuiBase::TieChoice( 
   const wxString &Prompt, 
   const wxString &SettingName, 
   const int Default, 
   const wxArrayString & Choices,
   const wxArrayInt & TranslatedChoices)
{
   wxASSERT( mpShuttle );
   wxChoice * pChoice=(wxChoice*)NULL;
   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         int n;
         int Value;
         mpShuttle->TransferInt( SettingName, Value, Default );
         // Translate from translated to display version
         n = TranslatedChoices.Index( Value );
         if( n== wxNOT_FOUND )
            n=miNoMatchSelector;
         miNoMatchSelector=0;
         wxString strValue = Choices[n];
         pChoice = TieChoice( Prompt, strValue, &Choices );
      }
      break;
   case eIsGettingFromDialog:
      {
         int n;
         wxString strValue;
         pChoice = TieChoice( Prompt, strValue, &Choices );
         // Translate from display version to translated version
         n = Choices.Index( strValue );
         if( n== wxNOT_FOUND )
            n=miNoMatchSelector;
         miNoMatchSelector=0;
         int Value = TranslatedChoices[n];
         mpShuttle->TransferInt( SettingName, Value, Default );
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
   return pChoice;
}

/// Variant of the standard TieTextBox which does the two step exchange 
/// between gui and stack variable and stack variable and shuttle.
void ShuttleGuiBase::TieTextBox(
   const wxString & Prompt, 
   const wxString & SettingName, 
   const wxString & Default,
   const int nChars)
{
   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         wxString strValue = Default;
         mShuttleMode = eIsGettingViaShuttle;
         TieTextBox( SettingName, strValue, nChars );
         miIdSetByUser = miId; // Reset the id, we're about to reuse it.
         mShuttleMode = eIsCreating;
         TieTextBox( Prompt, strValue, nChars );
      }
      break;
   case eIsGettingFromDialog:
      {
         wxString strValue = Default;
         TieTextBox( Prompt, strValue, nChars );
         miIdSetByUser = miId; // Reset the id, we're about to reuse it.
         mShuttleMode = eIsSavingViaShuttle;
         TieTextBox( SettingName, strValue, nChars );
         mShuttleMode = eIsGettingFromDialog;
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
}


/// Variant of the standard TieTextBox which does the two step exchange 
/// between gui and stack variable and stack variable and shuttle.
/// This one does it for double values...
void ShuttleGuiBase::TieTextBox(
   const wxString & Prompt, 
   const wxString & SettingName, 
   const double & Default,
   const int nChars)
{
   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         double Value = Default;
         wxASSERT( mpShuttle );
         mpShuttle->TransferDouble( SettingName, Value, Value );
         wxString strValue = wxString::Format(wxT("%g"),Value ); 
         TieTextBox( Prompt, strValue, nChars );
      }
      break;
   case eIsGettingFromDialog:
      {
         wxString strValue;
         TieTextBox( Prompt, strValue, nChars );
         wxASSERT( mpShuttle );
         double Value= Internat::CompatibleToDouble( strValue );
         mpShuttle->TransferDouble( SettingName, Value, Value );
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
}

// The Ids increment as we add new controls.
// However, the user can force the id manually, for example
// if they need a specific Id for a button, and then let it
// resume normal numbering later.
// UseUpId() sets miId to the next Id, either using the 
// user specicfied one, or resuming the sequence.
void ShuttleGuiBase::UseUpId()
{
   if( miIdSetByUser > 0)
   {
      miId = miIdSetByUser;
      miIdSetByUser = -1;
      return;
   }
   miId = miIdNext++;
}

void ShuttleGuiBase::UpdateSizersCore(bool bPrepend, int Flags)
{
   if( mpWind && mpParent )
   {
      if( mpSizer){
         if( bPrepend )
         {
            mpSizer->Prepend(mpWind, miProp, Flags,miBorder);
         }
         else
         {
            mpSizer->Add(mpWind, miProp, Flags,miBorder);
         }
      }
   }

   if( mpSubSizer && mpSizer )
   {
      mpSizer->Add( mpSubSizer,miSizerProp, Flags ,miBorder);
      mpSizer = mpSubSizer;
      mpSubSizer = NULL;
      PushSizer();
   }
   mpLastWind = mpWind;
   mpWind = NULL;
   miProp = 0;
   miSizerProp =0;
}

// Sizer is added into parent sizer, and will expand/shrink.
void ShuttleGuiBase::UpdateSizers()
{  UpdateSizersCore( false, wxEXPAND | wxALL );}

// Sizer is added into parent sizer, centred
void ShuttleGuiBase::UpdateSizersC()
{  UpdateSizersCore( false, wxALIGN_CENTRE | wxALL );}

// Sizer is added into parent sizer, and will expand/shrink.
// added to start of sizer list.
void ShuttleGuiBase::UpdateSizersAtStart()
{  UpdateSizersCore( true, wxEXPAND | wxALL );}

void ShuttleGuiBase::PopSizer()
{
   mSizerDepth--;
   wxASSERT( mSizerDepth >=0 );
   mpSizer = pSizerStack[ mSizerDepth ];
}

void ShuttleGuiBase::PushSizer()
{
   mSizerDepth++;
   wxASSERT( mSizerDepth < nMaxNestedSizers );
   pSizerStack[ mSizerDepth ] = mpSizer;
}

// A rarely used helper function that sets a pointer
// ONLY if the value it is to be set to is non NULL.
void SetIfCreated( wxChoice * &Var, wxChoice * Val )
{
   if( Val != NULL )
      Var = Val;
};
void SetIfCreated( wxTextCtrl * &Var, wxTextCtrl * Val )
{
   if( Val != NULL )
      Var = Val;
};
void SetIfCreated( wxStaticText *&Var, wxStaticText * Val )
{
   if( Val != NULL )
      Var = Val;
};

#ifdef EXPERIMENTAL_TRACK_PANEL
// Additional includes down here, to make it easier to split this into
// two files at some later date.
#include "../extnpanel-src/GuiWaveTrack.h"
#endif
#include "./widgets/Ruler.h"
#include "./widgets/AttachableScrollBar.h"
#include "ShuttlePrefs.h"

ShuttleGui::ShuttleGui(wxWindow * pParent, teShuttleMode ShuttleMode) :
   ShuttleGuiBase( pParent, ShuttleMode )
{
   if( ShuttleMode == eIsCreatingFromPrefs )
   {
      mShuttleMode = eIsCreating;
      Init(); // Wasn't fully done in base constructor because it is only done when eIsCreating is set.
   }
   else if( ShuttleMode == eIsSavingToPrefs )
   {
      mShuttleMode = eIsGettingFromDialog;
   }
   else
   {
      return;
   }

   mpShuttle = new ShuttlePrefs;
   // In this case the client is the GUI, so if creating we do want to
   // store in the client.
   mpShuttle->mbStoreInClient = (mShuttleMode == eIsCreating );
};

ShuttleGui::~ShuttleGui()
{
   if( mpShuttle )
      delete mpShuttle;
}

// Now we have Audacity specific shuttle functions.
ShuttleGui & ShuttleGui::Id(int id )
{
   miIdSetByUser = id;
   return *this;
}

GuiWaveTrack * ShuttleGui::AddGuiWaveTrack( const wxString & Name)
{
#ifdef EXPERIMENTAL_TRACK_PANEL
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return (GuiWaveTrack*)NULL;
//      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), GuiWaveTrack);
   GuiWaveTrack * pGuiWaveTrack;
   miProp=1;
   mpWind = pGuiWaveTrack = new GuiWaveTrack(mpParent, miId, Name);
   mpWind->SetMinSize(wxSize(100,50));
   UpdateSizers();
   return pGuiWaveTrack;
#else
   return NULL;
#endif
}

AdornedRulerPanel * ShuttleGui::AddAdornedRuler( ViewInfo *pViewInfo )
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return (AdornedRulerPanel*)NULL;
//      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), AdornedRulerPanel);
   AdornedRulerPanel * pAdornedRuler;
   miProp=0;
   mpWind = pAdornedRuler = new AdornedRulerPanel(
      mpParent, 
      miId, 
      wxDefaultPosition,
      wxDefaultSize,
      pViewInfo
      );

   mpWind->SetMinSize(wxSize(100,28));
   UpdateSizers();
   return pAdornedRuler;
}


RulerPanel * ShuttleGui::AddRulerVertical(float low, float hi, const wxString & Units )
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return (RulerPanel*)NULL;
//    return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), RulerPanel);
   RulerPanel * pRulerPanel;
   miProp=0;
   mpWind = pRulerPanel = new RulerPanel(
      mpParent, 
      miId,
      wxDefaultPosition,
      wxDefaultSize
      );
   Ruler & Ruler = pRulerPanel->ruler;
   Ruler.SetOrientation(wxVERTICAL);
   Ruler.SetBounds(0, 0, 100, 100); // Ruler can't handle small sizes
   Ruler.SetRange(low, hi);
   Ruler.SetFormat(Ruler::RealFormat);
   Ruler.SetUnits(Units);
   Ruler.SetLabelEdges(true);
   
   mpWind->SetMinSize(wxSize(38,50));
   UpdateSizers();
   return pRulerPanel;
}

AttachableScrollBar * ShuttleGui::AddAttachableScrollBar( long style )
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return (AttachableScrollBar*)NULL;
//      return wxDynamicCast(wxWindow::FindWindowById( miId, mpDlg), AttachableScrollBar);
   AttachableScrollBar * pAttachableScrollBar;
   miProp=0;
   mpWind = pAttachableScrollBar = new AttachableScrollBar(
      mpParent, 
      miId,
      wxDefaultPosition,
      wxDefaultSize,
	  style
      );
   mpWind->SetMinSize(wxSize(10,20));
   UpdateSizers();
   return pAttachableScrollBar;
}
