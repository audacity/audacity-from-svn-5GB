/**********************************************************************

  Audacity: A Digital Audio Editor

  Grid.cpp

  Leland Lucius

*******************************************************************//**

\class Grid
\brief Supplies an accessible grid based on wxGrid.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/choice.h>
#include <wx/dc.h>
#include <wx/grid.h>
#include <wx/intl.h>
#include <wx/settings.h>
#include <wx/toplevel.h>

#include "Grid.h"
#include "TimeTextCtrl.h"

TimeEditor::TimeEditor()
{
   TimeEditor(wxT("seconds"), 44100);
}

TimeEditor::TimeEditor(const wxString &format, double rate)
{
   mFormat = format;
   mRate = rate;
   mOld = 0.0;
}

TimeEditor::~TimeEditor()
{
}

void TimeEditor::Create(wxWindow *parent, wxWindowID id, wxEvtHandler *handler)
{
   m_control = new TimeTextCtrl(parent,
                                wxID_ANY,
                                wxT(""),
                                mOld,
                                mRate,
                                wxDefaultPosition,
                                wxDefaultSize,
                                true);
   /* look up provided format string name to a format string, then set that as
    * the format string for the control. Unfortunately m_control is a base
    * class pointer not a TimeTextCtrl pointer, so we have to cast it. It can't
    * fail to cast, however unless the preceeding new operation failed, so it's
    * reasonably safe. */
   ((TimeTextCtrl *)m_control)->SetFormatString(((TimeTextCtrl *)m_control)->GetBuiltinFormat(mFormat));

   wxGridCellEditor::Create(parent, id, handler);
}

void TimeEditor::SetSize(const wxRect &rect)
{
   wxSize size = m_control->GetSize();

   // Always center...looks bad otherwise
   int x = rect.x + ((rect.width / 2) - (size.x / 2)) + 1;
   int y = rect.y + ((rect.height / 2) - (size.y / 2)) + 1;

   m_control->Move(x, y);
}

void TimeEditor::BeginEdit(int row, int col, wxGrid *grid)
{
   wxGridTableBase *table = grid->GetTable();

   table->GetValue(row, col).ToDouble(&mOld);

   GetTimeCtrl()->SetTimeValue(mOld);
   GetTimeCtrl()->EnableMenu();

   GetTimeCtrl()->SetFocus();
}

bool TimeEditor::EndEdit(int row, int col, wxGrid *grid)
{
   double newtime = GetTimeCtrl()->GetTimeValue();
   bool changed = newtime != mOld;

   if (changed) {
      grid->GetTable()->SetValue(row, col, wxString::Format(wxT("%g"), newtime));
   }

   return changed;
}

void TimeEditor::Reset()
{
   GetTimeCtrl()->SetTimeValue(mOld);
}

bool TimeEditor::IsAcceptedKey(wxKeyEvent &event)
{
   if (wxGridCellEditor::IsAcceptedKey(event)) {
      if (event.GetKeyCode() == WXK_RETURN) {
         return true;
      }
   }

   return false;
}

wxGridCellEditor *TimeEditor::Clone() const
{
   return new TimeEditor(mFormat, mRate);
}

wxString TimeEditor::GetValue() const
{
   return wxString::Format(wxT("%g"), GetTimeCtrl()->GetTimeValue());
}

wxString TimeEditor::GetFormat()
{
   return mFormat;
}

double TimeEditor::GetRate()
{
   return mRate;
}

void TimeEditor::SetFormat(const wxString &format)
{
   mFormat = format;
}

void TimeEditor::SetRate(double rate)
{
   mRate = rate;
}

void TimeRenderer::Draw(wxGrid &grid,
                        wxGridCellAttr &attr,
                        wxDC &dc,
                        const wxRect &rect,
                        int row,
                        int col,
                        bool isSelected)
{
   wxGridCellRenderer::Draw(grid, attr, dc, rect, row, col, isSelected);

   wxGridTableBase *table = grid.GetTable();
   TimeEditor *te = (TimeEditor *) grid.GetCellEditor(row, col);
   wxString tstr;

   if (te) {
      double value;

      table->GetValue(row, col).ToDouble(&value);

      TimeTextCtrl tt(&grid,
                      wxID_ANY,
                      wxT(""),
                      value,
                      te->GetRate(),
                      wxPoint(10000, 10000),  // create offscreen
                      wxDefaultSize,
                      true);
      tt.SetFormatString(tt.GetBuiltinFormat(te->GetFormat()));
      tstr = tt.GetTimeString();

      te->DecRef();
   }

   dc.SetBackgroundMode(wxTRANSPARENT);
   
   if (grid.IsEnabled())
   {
      if (isSelected)
      {
         dc.SetTextBackground(grid.GetSelectionBackground());
         dc.SetTextForeground(grid.GetSelectionForeground());
      }
      else
      {
         dc.SetTextBackground(attr.GetBackgroundColour());
         dc.SetTextForeground(attr.GetTextColour());
      }
   }
   else
   {
      dc.SetTextBackground(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE));
      dc.SetTextForeground(wxSystemSettings::GetColour(wxSYS_COLOUR_GRAYTEXT));
   }
   
   dc.SetFont(attr.GetFont());

   int hAlign, vAlign;

   attr.GetAlignment(&hAlign, &vAlign);

   grid.DrawTextRectangle(dc, tstr, rect, hAlign, vAlign);
}

wxSize TimeRenderer::GetBestSize(wxGrid &grid,
                                 wxGridCellAttr &attr,
                                 wxDC &dc,
                                 int row,
                                 int col)
{
   wxGridTableBase *table = grid.GetTable();
   TimeEditor *te = (TimeEditor *) grid.GetCellEditor(row, col);
   wxSize sz;

   if (te) {
      double value;
      table->GetValue(row, col).ToDouble(&value);
      TimeTextCtrl tt(&grid,
                      wxID_ANY,
                      wxT(""),
                      value,
                      te->GetRate(),
                      wxPoint(10000, 10000),  // create offscreen
                      wxDefaultSize,
                      true);
      tt.SetFormatString(tt.GetBuiltinFormat(te->GetFormat()));
      sz = tt.GetSize();

      te->DecRef();
   }

   return sz;
}

wxGridCellRenderer *TimeRenderer::Clone() const
{
   return new TimeRenderer();
}

ChoiceEditor::ChoiceEditor(size_t count, const wxString choices[])
{
   if (count) {
      mChoices.Alloc(count);
      for (size_t n = 0; n < count; n++) {
         mChoices.Add(choices[n]);
      }
   }
}

ChoiceEditor::ChoiceEditor(const wxArrayString &choices)
{
   mChoices = choices;
}

ChoiceEditor::~ChoiceEditor()
{
   if (m_control)
      m_control->GetEventHandler()->Disconnect(wxEVT_KILL_FOCUS, wxFocusEventHandler(ChoiceEditor::OnKillFocus));
}

wxGridCellEditor *ChoiceEditor::Clone() const
{
   return new ChoiceEditor(mChoices);
}

void ChoiceEditor::Create(wxWindow* parent, wxWindowID id, wxEvtHandler* evtHandler)
{
   m_control = new wxChoice(parent,
                            id,
                            wxDefaultPosition,
                            wxDefaultSize,
                            mChoices);

   wxGridCellEditor::Create(parent, id, evtHandler);
   m_control->GetEventHandler()->Connect(wxEVT_KILL_FOCUS, wxFocusEventHandler(ChoiceEditor::OnKillFocus));
}

void ChoiceEditor::SetSize(const wxRect &rect)
{
   wxSize size = m_control->GetSize();

   // Always center...looks bad otherwise
   int x = rect.x + ((rect.width / 2) - (size.x / 2)) + 1;
   int y = rect.y + ((rect.height / 2) - (size.y / 2)) + 1;

   m_control->Move(x, y);
}

void ChoiceEditor::BeginEdit(int row, int col, wxGrid* grid)
{
   if (!m_control)
      return;

   mOld = grid->GetTable()->GetValue(row, col);

   Choice()->Clear();
   Choice()->Append(mChoices);
   Choice()->SetSelection(mChoices.Index(mOld));
   Choice()->SetFocus();
}

bool ChoiceEditor::EndEdit(int row, int col,
                           wxGrid* grid)
{
   wxString val = mChoices[Choice()->GetSelection()];

   if (val == mOld)
      return false;

   grid->GetTable()->SetValue(row, col, val);

   return true;
}

void ChoiceEditor::Reset()
{
   Choice()->SetSelection(mChoices.Index(mOld));
}

void ChoiceEditor::SetChoices(const wxArrayString &choices)
{
   mChoices = choices;
}

wxString ChoiceEditor::GetValue() const
{
   return mChoices[Choice()->GetSelection()];
}

void ChoiceEditor::OnKillFocus(wxFocusEvent &event)
{
   return;
}

///
///
///

BEGIN_EVENT_TABLE(Grid, wxGrid)
   EVT_KEY_DOWN(Grid::OnKeyDown)
   EVT_GRID_SELECT_CELL(Grid::OnSelectCell)
END_EVENT_TABLE()

Grid::Grid(wxWindow *parent,
           wxWindowID id,
           const wxPoint& pos,
           const wxSize& size,
           long style,
           const wxString& name)
: wxGrid(parent, id, pos, size, style | wxWANTS_CHARS, name)
{
#if wxUSE_ACCESSIBILITY
   mAx = new GridAx(this, NULL, wxROLE_SYSTEM_TABLE, 0, 0);
   SetAccessible(mAx);

   mObjNdx = -1;
   mChildren.Clear();
#endif

   RegisterDataType(GRID_VALUE_TIME,
                    new TimeRenderer,
                    new TimeEditor);

   RegisterDataType(GRID_VALUE_CHOICE,
                    new wxGridCellStringRenderer,
                    new ChoiceEditor);
}

Grid::~Grid()
{
#if wxUSE_ACCESSIBILITY
   int cnt = mChildren.GetCount();

   while (cnt) {
      GridAx *ax = (GridAx *) mChildren[--cnt];
      delete ax;
   }
#endif
}

void Grid::OnSelectCell(wxGridEvent &event)
{
   event.Skip();

#if wxUSE_ACCESSIBILITY
   mAx->SetCurrentCell(event.GetRow(), event.GetCol());
#endif
}

void Grid::OnKeyDown(wxKeyEvent &event)
{
   switch (event.GetKeyCode())
   {
      case WXK_TAB:
      {
         int flags = wxNavigationKeyEvent::FromTab |
                     ( event.ShiftDown() ?
                       wxNavigationKeyEvent::IsBackward :
                       wxNavigationKeyEvent::IsForward );
         Navigate(flags);
      }
      break;

      case WXK_RETURN:
      case WXK_NUMPAD_ENTER:
      {
         if (!IsCellEditControlShown()) {
            wxTopLevelWindow *tlw = wxDynamicCast(wxGetTopLevelParent(this), wxTopLevelWindow);
            wxWindow *def = tlw->GetDefaultItem();
            if (def && def->IsEnabled()) {
               wxCommandEvent cevent(wxEVT_COMMAND_BUTTON_CLICKED,
                                     def->GetId());
               GetParent()->ProcessEvent(cevent);
            }
         }
         else {
            wxGrid::OnKeyDown(event);
         }
         break;
      }

      default:
         wxGrid::OnKeyDown(event);
      break;
   }
}

#if wxUSE_ACCESSIBILITY
void Grid::ClearGrid()
{
   wxGrid::ClearGrid();

   mAx->TableUpdated();

   return;
}

bool Grid::InsertRows(int pos, int numRows, bool updateLabels)
{
   bool res = wxGrid::InsertRows(pos, numRows, updateLabels);

   mAx->TableUpdated();

   return res;
}

bool Grid::AppendRows(int numRows, bool updateLabels)
{
   bool res = wxGrid::AppendRows(numRows, updateLabels);
   
   mAx->TableUpdated();

   return res;
}

bool Grid::DeleteRows(int pos, int numRows, bool updateLabels)
{
   bool res = wxGrid::DeleteRows(pos, numRows, updateLabels);
   
   mAx->TableUpdated();

   return res;
}

bool Grid::InsertCols(int pos, int numCols, bool updateLabels)
{
   bool res = wxGrid::InsertCols(pos, numCols, updateLabels);

   mAx->TableUpdated();

   return res;
}

bool Grid::AppendCols(int numCols, bool updateLabels)
{
   bool res = wxGrid::AppendCols(numCols, updateLabels);

   mAx->TableUpdated();

   return res;
}

bool Grid::DeleteCols(int pos, int numCols, bool updateLabels)
{
   bool res = wxGrid::DeleteCols(pos, numCols, updateLabels);

   mAx->TableUpdated();

   return res;
}

GridAx *Grid::GetNextAx(GridAx *parent, wxAccRole role, int row, int col)
{
   mObjNdx = (mObjNdx + 1) & 0x3f;

   if (mObjNdx < (int)mChildren.GetCount()) {
      GridAx *ax = (GridAx *) mChildren[mObjNdx];
      delete ax;

      mChildren[mObjNdx] = new GridAx(this, parent, role, row, col);
   }
   else {
      mChildren.Add(new GridAx(this, parent, role, row, col));
   }

   return (GridAx *)mChildren[mObjNdx];
}

GridAx::GridAx(Grid *grid, GridAx *parent, wxAccRole role, int row, int col)
: wxWindowAccessible(NULL)
{
   mGrid = grid;
   mParent = parent;
   mRole = role;
   mRow = row;
   mCol = col;
}

void GridAx::TableUpdated()
{
   NotifyEvent(wxACC_EVENT_OBJECT_REORDER,
               mGrid,
               wxOBJID_CLIENT,
               0);
}

void GridAx::SetCurrentCell(int row, int col)
{
   int id = -(((row * mGrid->GetNumberCols()) + col) + 1);

   NotifyEvent(wxACC_EVENT_OBJECT_SELECTION,
               mGrid,
               wxOBJID_CLIENT,
               id);

   NotifyEvent(wxACC_EVENT_OBJECT_FOCUS,
               mGrid,
               wxOBJID_CLIENT,
               id);
}

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus GridAx::GetChild(int childId, wxAccessible** child)
{
   if (childId == wxACC_SELF) {
      *child = this;
      return wxACC_OK;
   }

   if (childId < 0) {
      int cols = mGrid->GetNumberCols();
      int row;
      int col;

      childId *= -1;
      childId -= 1;
      row = childId / cols;
      col = childId % cols;

      *child = mGrid->GetNextAx(this, wxROLE_SYSTEM_CELL, row + 1, col + 1);

      return wxACC_OK;
   }

   childId -= 1;

   switch (mRole)
   {
      case wxROLE_SYSTEM_TABLE:
         *child = mGrid->GetNextAx(this, wxROLE_SYSTEM_ROW, childId, 0);
      break;

      case wxROLE_SYSTEM_ROW:
         wxAccRole role;

         if (childId == 0) {
            role = wxROLE_SYSTEM_ROWHEADER;
         }
         else if (mRow == 0) {
            role = wxROLE_SYSTEM_COLUMNHEADER;
         }
         else {
            role = wxROLE_SYSTEM_CELL;
         }

         *child = mGrid->GetNextAx(this, role, mRow, childId);
      break;

      default:
         *child = NULL;
      break;
   }

   return wxACC_OK;
}

// Gets the number of children.
wxAccStatus GridAx::GetChildCount(int* childCount)
{
   switch (mRole)
   {
      case wxROLE_SYSTEM_TABLE:
         *childCount = mGrid->GetNumberRows() + 1;
      break;

      case wxROLE_SYSTEM_ROW:
         *childCount = mGrid->GetNumberCols() + 1;
      break;

      default:
         *childCount = 0;
      break;
   }

   return wxACC_OK;
}

// Gets the default action for this object (0) or > 0 (the action for a child).
// Return wxACC_OK even if there is no action. actionName is the action, or the empty
// string if there is no action.
// The retrieved string describes the action that is performed on an object,
// not what the object does as a result. For example, a toolbar button that prints
// a document has a default action of "Press" rather than "Prints the current document."
wxAccStatus GridAx::GetDefaultAction( int childId, wxString *actionName )
{
   actionName->Clear();

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus GridAx::GetDescription(int childId, wxString *description)
{
   if (childId == wxACC_SELF) {
      switch (mRole)
      {
         case wxROLE_SYSTEM_TABLE:
            description->Printf(_("%d rows, %d columns"),
                                mGrid->GetNumberRows(),
                                mGrid->GetNumberCols());
         break;

         case wxROLE_SYSTEM_ROW:
            description->Printf(_("Row %d"),
                                mRow);
         break;

         case wxROLE_SYSTEM_CELL:
            description->Printf(wxT("%d"),
                                mCol);
         break;

         default:
            description->Printf(wxT("oh-oh...buggie"));
         break;
      }

      return wxACC_OK;
   }

   return wxACC_NOT_IMPLEMENTED;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus GridAx::GetHelpText(int childId, wxString *helpText)
{
   helpText->Clear();

   return wxACC_OK;
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus GridAx::GetKeyboardShortcut(int childId, wxString *shortcut)
{
   shortcut->Clear();

   return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus GridAx::GetLocation(wxRect& rect, int elementId)
{
   wxRect r;

   wxASSERT_MSG((elementId == wxACC_SELF),
      wxString::Format(wxT("GridAx::GetLocation Unexpected elementId: %d"), elementId));

   switch (mRole)
   {
      case wxROLE_SYSTEM_TABLE:
         rect = mGrid->GetRect();
         r = mGrid->GetGridCornerLabelWindow()->GetRect();
         rect.x -= r.width;
         rect.y -= r.height;
      break;

      case wxROLE_SYSTEM_ROW:
         if (mRow == 0) {
            rect = mGrid->CellToRect(0, 0);

            r = mGrid->GetGridRowLabelWindow()->GetRect();
            rect.x -= r.width;
            rect.width = r.width;

            r = mGrid->GetGridColLabelWindow()->GetRect();
            rect.y -= r.height;
            rect.width += r.width;
         }
         else {
            rect = mGrid->CellToRect(mRow - 1, 0);

            r = mGrid->GetGridRowLabelWindow()->GetRect();
            rect.x -= r.width;
            rect.width = r.width;

            r = mGrid->GetGridColLabelWindow()->GetRect();
            rect.width += r.width;
         }
      break;

      case wxROLE_SYSTEM_ROWHEADER:
         r = mGrid->GetGridCornerLabelWindow()->GetRect();
         if (mRow == 0) {
            rect = mGrid->CellToRect(0, 0);
            rect.y -= r.height;
         }
         else {
            rect = mGrid->CellToRect(mRow - 1, 0);
         }
         rect.x -= r.width;
         rect.width = r.width;
      break;

      case wxROLE_SYSTEM_COLUMNHEADER:
         rect = mGrid->CellToRect(0, mCol - 1);
         r = mGrid->GetGridColLabelWindow()->GetRect();
         rect.y -= r.height;
      break;

      case wxROLE_SYSTEM_CELL:
         rect = mGrid->CellToRect(mRow - 1, mCol - 1);
      break;

      default:
         wxASSERT_MSG(0,
            wxString::Format(wxT("GridAx::GetLocation Unrecognized role: %d"),
                             mRole));
      break;
   }

   rect.SetPosition(mGrid->GetGridWindow()->ClientToScreen(rect.GetPosition()));

   return wxACC_OK;
}

// Gets the name of the specified object.
wxAccStatus GridAx::GetName(int childId, wxString* name)
{
   wxASSERT_MSG((childId == wxACC_SELF),
      wxString::Format(wxT("GridAx::GetName Unexpected childId: %d"), childId));

   switch (mRole)
   {
      case wxROLE_SYSTEM_TABLE:
         *name = mGrid->GetName();
      break;

      case wxROLE_SYSTEM_ROW:
         name->Printf(_("Row %d"), mRow);
      break;

      case wxROLE_SYSTEM_ROWHEADER:
         *name = mGrid->GetRowLabelValue(mRow);
      break;

      case wxROLE_SYSTEM_COLUMNHEADER:
         *name = mGrid->GetColLabelValue(mCol - 1);
      break;

      case wxROLE_SYSTEM_CELL:
      {
         *name = mGrid->GetCellValue(mRow - 1, mCol - 1);
         if (name->IsEmpty()) {
            *name = _("Empty");
         }

         // Hack to provide a more intelligible response
         TimeEditor *d =
            (TimeEditor *)mGrid->GetDefaultEditorForType(GRID_VALUE_TIME);
         TimeEditor *c =
            (TimeEditor *)mGrid->GetCellEditor(mRow - 1, mCol - 1);

         if (c && d && c == d) {
            double value;
            name->ToDouble(&value);

            TimeTextCtrl tt(mGrid,
                            wxID_ANY,
                            wxT(""),
                            value,
                            c->GetRate(),
                            wxPoint(10000, 10000),  // create offscreen
                            wxDefaultSize,
                            true);
            tt.SetFormatString(tt.GetBuiltinFormat(c->GetFormat()));
            *name = tt.GetTimeString();
         }

         if (c)
            c->DecRef();
         if (d)
            d->DecRef();
      }
      break;

      default:
         wxASSERT_MSG(0,
            wxString::Format(wxT("GridAx::GetName Unrecognized role: %d"),
                             mRole));
      break;
   }

   return wxACC_OK;
}

wxAccStatus GridAx::GetParent(wxAccessible **parent)
{
   *parent = mParent;

   return wxACC_OK;
}

// Returns a role constant.
wxAccStatus GridAx::GetRole(int childId, wxAccRole* role)
{
   wxASSERT_MSG((childId == wxACC_SELF),
      wxString::Format(wxT("GridAx::GetRole Unexpected childId: %d"), childId));

   *role = mRole;

   return wxACC_OK;
}

// Gets a variant representing the selected children
// of this object.
// Acceptable values:
// - a null variant (IsNull() returns TRUE)
// - a list variant (GetType() == wxT("list"))
// - an integer representing the selected child element,
//   or 0 if this object is selected (GetType() == wxT("long"))
// - a "void*" pointer to a wxAccessible child object
wxAccStatus GridAx::GetSelections( wxVariant *selections )
{
   return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus GridAx::GetState( int childId, long* state )
{
   int row = mGrid->GetGridCursorRow();
   int col = mGrid->GetGridCursorCol();
   int cols = mGrid->GetNumberCols();
   int flag;

   wxASSERT_MSG((childId == wxACC_SELF),
      wxString::Format(wxT("GridAx::GetState Unexpected childId: %d"), childId));

   *state = 0;

   switch (mRole)
   {
      case wxROLE_SYSTEM_TABLE:
         *state = wxACC_STATE_SYSTEM_FOCUSABLE | wxACC_STATE_SYSTEM_SELECTABLE;
      break;

      case wxROLE_SYSTEM_ROW:
         *state = wxACC_STATE_SYSTEM_FOCUSABLE | wxACC_STATE_SYSTEM_SELECTABLE;

         flag = wxACC_STATE_SYSTEM_INVISIBLE;
         for (col = 0; col < cols; col++) {
            if (mGrid->IsVisible(mRow, col)) {
               flag = 0;
               break;
            }
         }
         *state |= flag;

      break;

      case wxROLE_SYSTEM_ROWHEADER:
      case wxROLE_SYSTEM_COLUMNHEADER:
         *state = wxACC_STATE_SYSTEM_UNAVAILABLE;
      break;

      case wxROLE_SYSTEM_CELL:
      {
         int row = mGrid->GetGridCursorRow();
         int col = mGrid->GetGridCursorCol();
         int cols = mGrid->GetNumberCols();

         *state = wxACC_STATE_SYSTEM_FOCUSABLE | wxACC_STATE_SYSTEM_SELECTABLE;

         if (mRow - 1 == row) {
            *state |= wxACC_STATE_SYSTEM_FOCUSED;
         }

         flag = wxACC_STATE_SYSTEM_SELECTED;
         for (col = 0; col < cols; col++) {
            if (!mGrid->IsInSelection(row, col)) {
               flag = 0;
               break;
            }
         }
         *state |= flag;
      }
      break;

      default:
         wxASSERT_MSG(0,
            wxString::Format(wxT("GridAx::GetState Unrecognized role: %d"),
                             mRole));
      break;
   }

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus GridAx::GetValue( int childId, wxString* strValue )
{
   return wxACC_NOT_SUPPORTED;
}

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus GridAx::GetFocus( int *childId, wxAccessible **child )
{
   if (mGrid->FindFocus() == mGrid->GetGridWindow()) {
      *childId = mGrid->GetGridCursorRow() + 1;
   }

   return wxACC_OK;
}

#endif // wxUSE_ACCESSIBILITY

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 94f72c32-970b-4f4e-bbf3-3880fce7b965
