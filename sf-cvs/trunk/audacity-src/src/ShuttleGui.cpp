/**********************************************************************

  Audacity: A Digital Audio Editor

  ShuttleGui.cpp

  James Crook

  ShuttleGui extends the idea of the data shuttle class to include creation 
  of dialog controls.  As part of this it provides an interface to sizers 
  that leads to shorter more readable code.  

  It allows the code that is used to create dialogs to be reused
  to shuttle information in and out.  

  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

**********************************************************************/

#include <wx/wx.h>
#include <wx/listctrl.h>
#include <wx/treectrl.h>
#include "Experimental.h"
#include "ShuttleGui.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

// Most of the ShuttleGui functions are defined in ShuttleGuiBase
// which handles wxWidgets.  Audacity widgets are in the derived class.
ShuttleGuiBase::ShuttleGuiBase(wxWindow * pParent)
{
   mpSizer = NULL;
   miBorder = 5;
   miProp=0;
   miSizerProp=0;
   mpParent = pParent;
   mpWind = NULL;
   mpSubSizer = NULL;
   mSizerDepth=-1;

   miIdSetByUser = -1;
   miId = -1;
   miIdNext = 1000;

   mShuttleMode = eIsCreating;
}

ShuttleGuiBase::~ShuttleGuiBase()
{

}

void ShuttleGuiBase::Init()
{
   miIdSetByUser = -1;
   miId = -1;
   miIdNext = 1000;
   if( mShuttleMode != eIsCreating )
      return;

   mpSizer = mpParent->GetSizer();
   if( mpSizer == NULL )
   {
      wxWindow * pGrandParent = mpParent->GetParent();
      if( pGrandParent )
      {
         mpSizer = pGrandParent->GetSizer();
      }
   }
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


//---- Add Functions.

wxCheckBox * ShuttleGuiBase::AddCheckBox( const wxString &Prompt, const wxString &Selected)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return NULL;
   wxCheckBox * pCheckBox;
   miProp=0;
   mpWind = pCheckBox = new wxCheckBox(mpParent, miId, Prompt);
   pCheckBox->SetValue(Selected == wxT("true"));
   UpdateSizers();
   return pCheckBox;
}

wxButton * ShuttleGuiBase::AddButton(const wxString &Text)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return NULL;
   wxButton * pBtn;
   mpWind = pBtn = new wxButton( mpParent, miId, Text );
   UpdateSizersC();
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
   Prompt2.Replace( wxT("&"), wxT("&&") );
   mpWind = pCheckBox = new wxCheckBox(mpParent, miId, Prompt2);
   pCheckBox->SetValue(Selected==wxT("true"));
   UpdateSizers();
}

// For a consistant two-column layout we want labels on the left and
// controls on the right.  TickBoxes break that rule, so we fake it by
// placing a static text label and then a tick box with an empty label.
void ShuttleGuiBase::AddTickBoxOnRight( const wxString &Prompt, const wxString &Selected)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
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
      return NULL;
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

   mpWind = new wxStaticText(mpParent, -1, Prompt, wxDefaultPosition, wxDefaultSize, 
      wxALIGN_RIGHT );
   UpdateSizers();
   mpWind = pChoice = new wxChoice(
      mpParent,
      miId,
      wxDefaultPosition, 
      wxDefaultSize, 
      n, 
      Choices);
   pChoice->SetSizeHints( 180,20);
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
      return NULL;
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
      return NULL;
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

   mpWind = new wxStaticText(mpParent, -1, Prompt, wxDefaultPosition, wxDefaultSize, 
      wxALIGN_RIGHT );
   UpdateSizers();

   mpWind = pCombo = new wxComboBox(mpParent, miId, Selected, wxDefaultPosition, wxDefaultSize, 
      n, Choices);

   UpdateSizers();
   return pCombo;
}


wxRadioButton * ShuttleGuiBase::AddRadioButton(const wxString &Prompt)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return NULL;
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
      return NULL;
   wxRadioButton * pRad;
   mpWind = pRad = new wxRadioButton( mpParent, miId, Prompt );
   UpdateSizers();
   return pRad;
}

wxSlider * ShuttleGuiBase::AddSlider(const wxString &Prompt, int pos)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return NULL;
   wxSlider * pSlider;
   mpWind = pSlider = new wxSlider( mpParent, miId, 
      pos, 0, 100, 
      wxDefaultPosition, wxDefaultSize,
      wxSL_HORIZONTAL | wxSL_LABELS | wxSL_AUTOTICKS
      );
   UpdateSizers();
   return pSlider;
}


wxTextCtrl * ShuttleGuiBase::AddTextBox(const wxString &Caption, const wxString &Value, const int nChars)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return NULL;
   wxTextCtrl * pTextCtrl;
   miProp=0;
   wxSize Size(wxDefaultSize);
   if( nChars > 0 )
   {
      Size.SetWidth( nChars *5 );
   }
   mpWind = new wxStaticText(mpParent, -1, Caption, wxDefaultPosition, wxDefaultSize, 
      wxALIGN_RIGHT );
   UpdateSizers();
   mpWind = pTextCtrl = new wxTextCtrl(mpParent, miId, Value,
      wxDefaultPosition, Size);
   UpdateSizers();
   return pTextCtrl;
}

void ShuttleGuiBase::AddConstTextBox(const wxString &Caption, const wxString &Value)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return;
   miProp=0;
   mpWind = new wxStaticText(mpParent, miId, Caption, wxDefaultPosition, wxDefaultSize, 
      wxALIGN_RIGHT );
   UpdateSizers();
   mpWind = new wxStaticText(mpParent, miId, Value);
   UpdateSizers();
}


wxListCtrl * ShuttleGuiBase::AddListControl()
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return NULL;
   wxListCtrl * pListCtrl;
   miProp=1;
   mpWind = pListCtrl = new wxListCtrl(mpParent, miId);
   pListCtrl->SetMinSize( wxSize( 120,150 ));
   UpdateSizers();
   return pListCtrl;
}

wxTreeCtrl * ShuttleGuiBase::AddTree()
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return NULL;
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
      return;
   wxBitmapButton * pBtn;
   mpWind = pBtn = new wxBitmapButton( mpParent, miId, *pBmp  );
   pBtn->SetWindowStyle( 0 );
   UpdateSizersC();
}

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

wxScrolledWindow * ShuttleGuiBase::StartScroller(int iStyle)
{
   UseUpId();
   if( mShuttleMode != eIsCreating )
      return NULL;

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
      return NULL;
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
      return NULL;
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
      return NULL;
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

void ShuttleGuiBase::StartMultiColumn(int nCols)
{
   if( mShuttleMode != eIsCreating )
      return;
   mpSubSizer = new wxFlexGridSizer( nCols );
   UpdateSizersC();
}

void ShuttleGuiBase::EndMultiColumn()
{
   if( mShuttleMode != eIsCreating )
      return;
   PopSizer();
}

void ShuttleGuiBase::StartTwoColumnStretchy()
{
   if( mShuttleMode != eIsCreating )
      return;
   wxFlexGridSizer * pSizer = new wxFlexGridSizer( 2 );
   mpSubSizer = pSizer;
   pSizer->AddGrowableCol( 1,1 );
   pSizer->AddGrowableRow( 1,1 );
   miSizerProp=1;
   UpdateSizers();
}

void ShuttleGuiBase::TieTickbox(const wxString &Prompt, bool &Var)
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
   case eIsSavingViaShuttle:
      {
         wxASSERT(false);
      }
      break;
   // IF Getting settings from external storage.
   case eIsGettingViaShuttle:
      {
         wxASSERT(false);
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
}
// See comment in AddTickBoxOnRight() for why we have this variant.
void ShuttleGuiBase::TieTickboxOnRight(const wxString &Prompt, bool &Var)
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
         wxASSERT(false);
      }
      break;
   // IF Getting settings from external storage.
   case eIsGettingViaShuttle:
      {
         wxASSERT(false);
      }
      break;
   default:
      wxASSERT( false );
      break;
   }
}



wxSlider * ShuttleGuiBase::TieSlider( const wxString &Prompt, const float min, const float max, float &f )
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode != eIsCreating )
      UseUpId();
   wxSlider * pSlider=NULL;
   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         AddSlider( Prompt, 100*(f-min)/(max-min) );
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
   case eIsSavingViaShuttle:
   // IF Getting settings from external storage.
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
         AddTextBox( Prompt, Selected, nChars );
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
   case eIsSavingViaShuttle:
   // IF Getting settings from external storage.
   case eIsGettingViaShuttle:
   default:
      wxASSERT( false );
      break;
   }
   return pTextBox;
}

wxChoice * ShuttleGuiBase::TieCombo( const wxString &Prompt, wxString &Selected, const wxArrayString * pChoices )
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode != eIsCreating )
      UseUpId();
   wxChoice * pChoice=NULL;
   switch( mShuttleMode )
   {
   case eIsCreating:
      {
         AddCombo( Prompt, Selected, pChoices );
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
   case eIsSavingViaShuttle:
   // IF Getting settings from external storage.
   case eIsGettingViaShuttle:
      break;
   default:
      wxASSERT( false );
      break;
   }
   return pChoice;
}

void ShuttleGuiBase::TieRadioButton(const wxString &Prompt, int iIndex, wxString &Selected)
{
   // The Add function does a UseUpId(), so don't do it here in that case.
   if( mShuttleMode != eIsCreating )
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
   case eIsSavingViaShuttle:
      {
         wxASSERT(false);
      }
      break;
   // IF Getting settings from external storage.
   case eIsGettingViaShuttle:
      {
         wxASSERT( false );
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



#ifdef EXPERIMENTAL_TRACK_PANEL
// Additional includes down here, to make it easier to split this into
// two files at some later date.
#include "GuiWaveTrack.h"
#endif
#include "./widgets/Ruler.h"
#include "./widgets/AttachableScrollbar.h"


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
