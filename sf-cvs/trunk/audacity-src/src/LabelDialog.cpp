/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelDialog.cpp

  Dominic Mazzoni

*******************************************************************//**

\class LabelDialog
\brief Dialog for editing labels.

*//*******************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/choice.h>
#include <wx/dc.h>
#include <wx/dialog.h>
#include <wx/grid.h>
#include <wx/intl.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/textdlg.h>
#include <wx/msgdlg.h>
#include <wx/filedlg.h>

#include "LabelDialog.h"
#include "LabelTrack.h"
#include "Track.h"
#include "widgets/TimeTextCtrl.h"
#include "Prefs.h"
#include "Internat.h"

enum Column
{
   Col_Track,
   Col_Label,
   Col_Stime,
   Col_Etime,
   Col_Max
};

static const wxChar *headers[Col_Max] =
{
   _("Track"),
   _("Label"),
   _("Start Time"),
   _("End Time")
};

class RowData
{
 public:
   RowData() {};

   int index;

   wxString title;
   double stime;
   double etime;
};

enum {
   ID_INSERTA = 11000,
   ID_INSERTB,
   ID_REMOVE,
   ID_IMPORT,
   ID_EXPORT
};

class EmptyLabelRenderer:public wxGridCellStringRenderer
{
 public:
   void Draw(wxGrid &grid,
             wxGridCellAttr& attr,
             wxDC& dc,
             const wxRect& rect,
             int row, int col,
             bool isSelected)
   {
      wxString val = grid.GetCellValue(row, col);
      wxColour clr;
      int halign;
      int valign;

      if (val.IsEmpty()) {
         attr.GetAlignment(&halign, &valign);
         clr = attr.GetTextColour();

         if (col == 0) {
            grid.SetCellValue(row, col, _("<add new label here>"));
         }
         else {
            grid.SetCellValue(row, col, _("<enter label name>"));
         }

         attr.SetTextColour(wxColour(128, 128, 128));
         attr.SetAlignment(wxALIGN_CENTRE, wxALIGN_CENTRE);
      }

      wxGridCellStringRenderer::Draw(grid, attr, dc, rect, row, col, isSelected);

      if (val.IsEmpty()) {
         attr.SetAlignment(halign, valign);
         attr.SetTextColour(clr);
         grid.SetCellValue(row, col, wxT(""));
      }
   }
};

BEGIN_EVENT_TABLE(LabelDialog, wxDialog)
   EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, LabelDialog::OnUpdate)
   EVT_BUTTON(ID_INSERTA, LabelDialog::OnInsert)
   EVT_BUTTON(ID_INSERTB, LabelDialog::OnInsert)
   EVT_BUTTON(ID_REMOVE,  LabelDialog::OnRemove)
   EVT_BUTTON(ID_IMPORT,  LabelDialog::OnImport)
   EVT_BUTTON(ID_EXPORT,  LabelDialog::OnExport)
//   EVT_KEY_DOWN(LabelDialog::OnKeyDown)
   EVT_GRID_CELL_CHANGE(LabelDialog::OnCellChange)
   EVT_GRID_EDITOR_SHOWN(LabelDialog::OnEditorShown)
END_EVENT_TABLE()

LabelDialog::LabelDialog(wxWindow *parent,
                         DirManager *dirmanager,
                         TrackList *tracks,
                         double rate)
: wxDialog(parent,
           wxID_ANY,
           _("Edit Labels"),
           wxDefaultPosition,
           wxSize(800, 600),
           wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
  mDirManager(dirmanager),
  mTracks(tracks),
  mRate(rate)
{
   wxBoxSizer *vs = new wxBoxSizer(wxVERTICAL);
   mGrid = new Grid(this, wxID_ANY);
   vs->Add(mGrid, 1, wxEXPAND | wxALL, 5);

   // Build the works
   wxBoxSizer *hs = new wxBoxSizer(wxHORIZONTAL);
   hs->Add(new wxButton(this, ID_INSERTA, _("Insert &After")), 1, wxCENTER | wxALL, 5);
   hs->Add(new wxButton(this, ID_INSERTB, _("Insert &Before")), 1, wxCENTER | wxALL, 5);
   hs->Add(new wxButton(this, ID_REMOVE,  _("&Remove")), 1, wxCENTER | wxALL, 5);
   hs->Add(new wxButton(this, ID_IMPORT,  _("I&mport...")), 1, wxCENTER | wxALL, 5);
   hs->Add(new wxButton(this, ID_EXPORT,  _("E&xport...")), 1, wxCENTER | wxALL, 5);
   vs->Add(hs, 0, wxEXPAND | wxCENTER | wxALL, 5);

   vs->Add(CreateButtonSizer(wxOK | wxCANCEL),
           0,
           wxCENTER | wxBOTTOM | wxALL,
           10);
   SetSizer(vs);

   // Build the initial (empty) grid
   mGrid->CreateGrid(0, Col_Max);
   mGrid->SetDefaultCellAlignment(wxALIGN_LEFT, wxALIGN_CENTER);

   int i;
   for (i = 0; i < Col_Max; i++) {
      mGrid->SetColLabelValue(i, headers[i]);
   }

   // Create and remember editors.  No need to delete these as the wxGrid will
   // do it for us.
   mChoiceEditor = (ChoiceEditor *) mGrid->GetDefaultEditorForType(GRID_VALUE_CHOICE);
   mTimeEditor = (TimeEditor *) mGrid->GetDefaultEditorForType(GRID_VALUE_TIME);

   // Initialize and set the track name column attributes
   wxGridCellAttr *attr = new wxGridCellAttr();
   attr->SetEditor(mChoiceEditor);
   mGrid->SetColAttr(Col_Track, attr);
   mTrackNames.Add(_("New..."));

   // Initialize and set the label name column attributes
//   attr = new wxGridCellAttr();
//   attr->SetRenderer(new EmptyLabelRenderer());
//   mGrid->SetColAttr(Col_Label, attr);

   // Initialize and set the time column attributes
   attr = new wxGridCellAttr();
   attr->SetRenderer(mGrid->GetDefaultRendererForType(GRID_VALUE_TIME));
   attr->SetEditor(mTimeEditor);
   attr->SetAlignment(wxALIGN_CENTER, wxALIGN_CENTER);
   mGrid->SetColAttr(Col_Stime, attr);
   mGrid->SetColAttr(Col_Etime, attr->Clone());

   // Seems there's a bug in wxGrid.  Adding only 1 row does not
   // allow SetCellSize() to work properly and you will not get
   // the expected 1 row by 4 column cell.
   //
   // So, we set the minimum row height to 0 and basically hide
   // the extra row by setting its height to 0.  And not allowing the
   // rows to be manually resized prevents the user from ever seeing
   // the extra row.
   mGrid->SetRowMinimalAcceptableHeight(0);
   mGrid->EnableDragRowSize(false);

   // Add 3 rows...1 for determining size and 2 for "new label" rows
   mGrid->AppendRows(3);

   // Call Autosize() to set the height of the first row.  Use that
   // height for the first "new label" row and hide the second row.
   mGrid->AutoSize();
   mGrid->SetRowSize(1, mGrid->GetRowSize(0));
   mGrid->SetRowSize(2, 0);

   // No longer need the "height" row.
   mGrid->DeleteRows(0, 1);

   // Allow the "new label" row to span the full width of the grid
   // and 2 rows due to critter (see above).
   mGrid->SetCellSize(0, 0, 2, Col_Max);

   // Set various properties for "new label" row
   mGrid->SetCellRenderer(0, 0, new wxGridCellStringRenderer);
   mGrid->SetCellEditor(0, 0, new wxGridCellTextEditor);
   mGrid->SetCellAlignment(0, 0, wxALIGN_CENTRE, wxALIGN_CENTRE);
   mGrid->SetCellTextColour(0, 0, wxColour(128, 128, 128));
   mGrid->SetCellValue(0, 0, _("<add new label here>"));

   // Locate all labels in current track list
   FindAllLabels();

   // Populate the grid
   TransferDataToWindow();

   // Resize the label name column and ensure it doesn't go below an
   // arbitrary width.  (Should be as wide as the "Enter Label Name"
   // prompt...see EmptyLabelRenderer.)
   //
   // This should not be in TransferDataToWindow() since a user might
   // resize the column and we'd resize it back the the minimum.
   mGrid->AutoSizeColumn(Col_Label, false );
   mGrid->SetColSize(Col_Label, wxMax(150, mGrid->GetColSize(Col_Label)));
   mGrid->SetColMinimalWidth(Col_Label, mGrid->GetColSize(Col_Label));

   // Layout the works
   Layout();

   // Resize width base on width of columns and the vertical scrollbar
   wxRect r = mGrid->GetGridColLabelWindow()->GetRect();
   wxScrollBar sb(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSB_VERTICAL);
   r.width += sb.GetSize().GetWidth() + 6;
   SetClientSize(r.width, 300);

   // Make sure it doesn't go below this size
   r = GetRect();
   SetSizeHints(r.GetWidth(), r.GetHeight());

   // Center on display
   Center();
}

LabelDialog::~LabelDialog()
{
   int cnt = mData.GetCount();

   // Delete any RowData we've allocated
   while (cnt) {
      RowData *rd = mData[--cnt];
      delete rd;
   }
}

bool LabelDialog::TransferDataToWindow()
{
   int cnt = mData.GetCount();
   int i;

   // Disable redrawing until we're done
   mGrid->BeginBatch();

   // Delete all rows except the "new label" rows
   if (mGrid->GetNumberRows() - 2) {
      mGrid->DeleteRows(0, mGrid->GetNumberRows() - 2);
   }

   // Add the exact number that we'll need
   mGrid->InsertRows(0, cnt);

   // Populate the rows
   for (i = 0; i < cnt; i++) {
      RowData *rd = mData[i];

      // Autosize each row...do not use AutoSizeRows() since that would autosize
      // the "new label" rows and we have specific requirements for those.
      mGrid->AutoSizeRow(i, false);

      // Set the cell contents
      mGrid->SetCellValue(i, Col_Track, TrackName(rd->index));
      mGrid->SetCellValue(i, Col_Label, rd->title);
      mGrid->SetCellValue(i, Col_Stime, wxString::Format(wxT("%g"), rd->stime));
      mGrid->SetCellValue(i, Col_Etime, wxString::Format(wxT("%g"), rd->etime));
   }

   // Set the editor parameters.  Do this each time since they may change
   // due to new tracks and change in TimeTextCtrl format.  Rate won't
   // change but might as well leave it here.
   mChoiceEditor->SetChoices(mTrackNames);
   mTimeEditor->SetFormat(mFormat);
   mTimeEditor->SetRate(mRate);

   // Resize the track name column.  Use a wxChoice to determine the maximum
   // width needed.
   wxChoice tc(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, mTrackNames);
   mGrid->SetColSize(Col_Track, tc.GetSize().x);
   mGrid->SetColMinimalWidth(Col_Track, tc.GetSize().x);

   // Autosize the time columns and set their minimal widths
   mGrid->AutoSizeColumn(Col_Stime);
   mGrid->AutoSizeColumn(Col_Etime);

   // We're done, so allow the grid to redraw
   mGrid->EndBatch();

   return true;
}

bool LabelDialog::TransferDataFromWindow()
{
   int cnt = mData.GetCount();
   int i, j;
   TrackListIterator iter(mTracks);
   Track *t;
   int tndx = 0;

   // Clear all label tracks of labels
   for (t = iter.First(); t; t = iter.Next()) {
      if (t->GetKind() == Track::Label) {
         LabelTrack *lt = (LabelTrack *)t;
         tndx++;

         for (i = lt->GetNumLabels() - 1; i >= 0 ; i--) {
            lt->DeleteLabel(i);
         }
      }
   }

   // Create any added tracks
   while (tndx < mTrackNames.GetCount() - 1) {

      // Extract the name
      wxString name = mTrackNames[tndx + 1].AfterFirst(wxT('-')).Mid(1);

      // Create the new track and add to track list
      LabelTrack *newTrack = new LabelTrack(mDirManager);
      newTrack->SetName(name);
      mTracks->Add(newTrack);
      tndx++;
   }

   // Repopulate with updated labels
   for (i = 0; i < cnt; i++) {
      RowData *rd = mData[i];

      // Look for track with matching index
      tndx = 1;
      for (t = iter.First(); t; t = iter.Next()) {
         if (t->GetKind() == Track::Label && rd->index == tndx++) {
            break;
         }
      }

      // Add the label to it
      ((LabelTrack *) t)->AddLabel(rd->stime, rd->etime, rd->title);
      ((LabelTrack *) t)->Unselect();
   }

   return true;
}

wxString LabelDialog::TrackName(int index, wxString dflt)
{
   // Generate a new track name if the passed index is out of range
   if (index < 1 || index >= mTrackNames.GetCount()) {
      index = mTrackNames.GetCount();
      mTrackNames.Add(wxString::Format(wxT("%d - %s"), index, dflt.c_str()));
   }

   // Return the track name
   return mTrackNames[index];
}

void LabelDialog::FindAllLabels()
{
   TrackListIterator iter(mTracks);
   Track *t;

   // Add labels from all label tracks
   for (t = iter.First(); t; t = iter.Next()) {
      if (t->GetKind() == Track::Label) {
         AddLabels((LabelTrack *) t);
      }
   }
}

void LabelDialog::AddLabels(LabelTrack *t)
{
   wxString lab;
   int tndx = mTrackNames.GetCount();
   int i;

   // Add each label in the track
   for (i = 0; i < t->GetNumLabels(); i++) {
      const LabelStruct *ls = t->GetLabel(i);
      RowData *rd = new RowData();

      rd->index = tndx;
      rd->stime = ls->t;
      rd->etime = ls->t1;
      rd->title = ls->title;

      mData.Add(rd);
   }

   // Add a new track name
   TrackName(tndx, t->GetName());
}

void LabelDialog::OnKeyDown(wxKeyEvent &event)
{
   switch (event.GetKeyCode())
   {
      case WXK_RETURN:
      {
         if (!mGrid->IsCellEditControlEnabled()) {
            wxCommandEvent e(wxEVT_COMMAND_BUTTON_CLICKED, wxID_OK);
            GetEventHandler()->AddPendingEvent(e);
            event.Skip(false);
         }
         else {
            event.Skip(true);
         }
      }
      break;

      case WXK_ESCAPE:
      {
         if (!mGrid->IsCellEditControlEnabled()) {
            wxCommandEvent e(wxEVT_COMMAND_BUTTON_CLICKED, wxID_CANCEL);
            GetEventHandler()->AddPendingEvent(e);
            event.Skip(false);
         }
         else {
            event.Skip(true);
         }
      }
      break;
   }
}

void LabelDialog::OnUpdate(wxCommandEvent &event)
{
   // Remember the new format and repopulate grid
   mFormat = event.GetString();
   TransferDataToWindow();

   event.Skip(false);
}

void LabelDialog::OnInsert(wxCommandEvent &event)
{
   RowData *rd = new RowData();
   int cnt = mData.GetCount();
   int row = 0;
   int index = 0;

   // Make sure the edit control isn't active before inserting any rows
   if (mGrid->IsCellEditControlShown()) {
      mGrid->HideCellEditControl();
   }

   // Attempt to guess which track the label to reside on
   if (cnt > 0) {
      row = mGrid->GetCursorRow();
      if (row > 0 && row >= cnt) {
         index = mTrackNames.Index(mGrid->GetCellValue(row - 1, Col_Track));
      }
      else {
         index = mTrackNames.Index(mGrid->GetCellValue(row, Col_Track));
      }
   }

   // Initialize the new label
   rd->index = index;
   rd->stime = 0.0;
   rd->etime = 0.0;
   rd->title = wxT("");

   // Insert it before or after the current row
   if (event.GetId() == ID_INSERTA && row < cnt) {
      row++;
   }
   mData.Insert(rd, row);

   // Repopulate the grid
   TransferDataToWindow();

   // Reposition cursor to new row/col and put user into edit mode to
   // set the label name
   mGrid->SetGridCursor(row, Col_Label);
   mGrid->EnableCellEditControl(true);
   mGrid->ShowCellEditControl();
}

void LabelDialog::OnRemove(wxCommandEvent &event)
{
   int row = mGrid->GetCursorRow();
   int col = mGrid->GetCursorColumn();
   int cnt = mData.GetCount();

   // Don't try to remove if no labels exist and don't remove the
   // "new label" rows
   if (cnt == 0 || row >= cnt) {
      return;
   }

   // Remove the row
   RowData *rd = mData[row];
   mData.RemoveAt(row);
   delete rd;

   // Repopulate the grid
   TransferDataToWindow();

   // Reposition the cursor
   if (row > 0 && row >= --cnt) {
      row--;
   }
   mGrid->SetGridCursor(row, col);
}

void LabelDialog::OnImport(wxCommandEvent &event)
{
   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"),FROMFILENAME(::wxGetCwd()));

   // Ask user for a filename
   wxString fileName =
       wxFileSelector(_("Select a text file containing labels..."),
                      path,     // Path
                      wxT(""),       // Name
                      wxT(".txt"),   // Extension
                      _("Text files (*.txt)|*.txt|All files (*.*)|*.*"),
                      0,        // Flags
                      this);    // Parent

   // They gave us one...
   if (fileName != wxT("")) {
      path =::wxPathOnly(fileName);
      gPrefs->Write(wxT("/DefaultOpenPath"), path);

      wxTextFile f;

      // Get at the data
      f.Open(fileName);
      if (!f.IsOpened()) {
         wxMessageBox(_("Could not open file: ") + fileName);
      }
      else {
         // Create a temporary label track and load the labels
         // into it
         LabelTrack *lt = new LabelTrack(mDirManager);
         lt->Import(f);
         delete lt;

         // Add the labesls to our collection
         AddLabels(lt);
      }

      // Repopulate the grid
      TransferDataToWindow();
   }
}

void LabelDialog::OnExport(wxCommandEvent &event)
{
   int cnt = mData.GetCount();

   // Silly user (could just disable the button, but that's a hassle ;-))
   if (cnt == 0) {
      wxMessageBox(_("There are no label to export."));
      return;
   }

   wxString fName;

   fName = wxFileSelector(_("Export Labels As:"),
                          NULL,
                          _("labels.txt"),
                          wxT("txt"),
                          wxT("*.txt"), wxSAVE | wxOVERWRITE_PROMPT, this);

   if (fName == wxT(""))
      return;

   // Move existing files out of the way.  Otherwise wxTextFile will
   // append to (rather than replace) the current file.

   if (wxFileExists(FILENAME(fName))) {
#ifdef __WXGTK__
      wxString safetyFileName = fName + wxT("~");
#else
      wxString safetyFileName = fName + wxT(".bak");
#endif

      if (wxFileExists(FILENAME(safetyFileName)))
         wxRemoveFile(FILENAME(safetyFileName));

      wxRename(fName, safetyFileName);
   }

   wxTextFile f(FILENAME(fName).c_str());
#ifdef __WXMAC__
   wxFile *temp = new wxFile();
   temp->Create(FILENAME(fName));
   delete temp;
#else
   f.Create();
#endif
   f.Open();
   if (!f.IsOpened()) {
      wxMessageBox(_("Couldn't write to file: ") + fName);
      return;
   }

   // Transfer our collection to a temporary label track
   LabelTrack *lt = new LabelTrack(mDirManager);
   int i;

   for (i = 0; i < cnt; i++) {
      RowData *rd = mData[i];

      lt->AddLabel(rd->stime, rd->etime, rd->title);
   }

   // Export them and clean
   lt->Export(f);
   delete lt;

#ifdef __WXMAC__
   f.Write(wxTextFileType_Mac);
#else
   f.Write();
#endif
   f.Close();
}

void LabelDialog::OnEditorShown(wxGridEvent &event)
{
   int row = event.GetRow();
   int col = event.GetCol();

   // User typed something into the "new label" row
   if (row == mData.GetCount()) {
      mGrid->SetCellTextColour(row, col, wxColour(0, 0, 0));
      mGrid->SetCellValue(row, col, wxT(""));
      event.Skip();

      return;
   }
}

void LabelDialog::OnCellChange(wxGridEvent &event)
{
   static bool guard = false;
   int row = event.GetRow();
   RowData *rd;

   // Guard against recursion which can happen when a change to the "new label" row
   // is made.  When InsertRow() is done in TransferDataToWindow(), checks are made
   // within wxGrid to see if the edit control is active and since it hasn't yet
   // been marked inactive on the first time through here, we get entered again.
   // Sort of a double change.  I think this is probably a bug in wxGrid.
   if (guard) {
      return;
   }
   guard = true;

   // User typed something into the "new label" row
   if (row == mData.GetCount()) {
      rd = new RowData();

      // Initialize and add the new label...use 1 for index to either create
      // a new track or use the first one available.
      rd->index = 1;
      rd->stime = 0.0;
      rd->etime = 0.0;
      rd->title = mGrid->GetCellValue(row, 0);
      mData.Add(rd);

      // Repopulate the grid
      TransferDataToWindow();

      // Clear the "new label" row
      mGrid->SetCellValue(row + 1, 0, wxT("<add new label here>"));
      mGrid->SetCellTextColour(row + 1, 0, wxColour(128, 128, 128));

      // Make sure the new row is completely visible
      mGrid->MakeCellVisible(mData.GetCount(), Col_Track);

      // The refresh is necessary cause MakeCellVisible() leaves border
      // artifacts.
      mGrid->Refresh(false);

      // Hmmm...is this needed?  Check me later...
      event.Skip(true);

      // Done...no need for protection anymore
      guard = false;

      return;
   }

   // The change was to an existing label, so go process it based
   // on which column was changed.
   rd = mData[row];
   switch (event.GetCol())
   {
      case Col_Track:
         OnChangeTrack(event, row, rd);
      break;

      case Col_Label:
         OnChangeLabel(event, row, rd);
      break;

      case Col_Stime:
         OnChangeStime(event, row, rd);
      break;

      case Col_Etime:
         OnChangeEtime(event, row, rd);
      break;
   }

   // Done...no need for protection anymore
   guard = false;

   return;
}

void LabelDialog::OnChangeTrack(wxGridEvent &event, int row, RowData *rd)
{
   wxString val = mGrid->GetCellValue(row, Col_Track);

   // User selected the "New..." choice so ask for a new name
   if (mTrackNames.Index(val) == 0) {
      wxTextEntryDialog d(this,
                          _("New Label Track"),
                          _("Enter track name"),
                          _("Label Track"));

      // User canceled so repopulating the grid will set the track
      // name to the orignal value
      if (d.ShowModal() == wxID_CANCEL) {
         TransferDataToWindow();
         return;
      }

      // Force generation of a new track name
      val = TrackName(-1, d.GetValue());
   }

   // Remember the tracks index
   rd->index = mTrackNames.Index(val);

   // Repopulate the grid
   TransferDataToWindow();

   return;
}

void LabelDialog::OnChangeLabel(wxGridEvent &event, int row, RowData *rd)
{
   // Remember the value...no need to repopulate
   rd->title = mGrid->GetCellValue(row, Col_Label);

   return;
}

void LabelDialog::OnChangeStime(wxGridEvent &event, int row, RowData *rd)
{
   // Remember the value...no need to repopulate
   mGrid->GetCellValue(row, Col_Stime).ToDouble(&rd->stime);

   return;
}

void LabelDialog::OnChangeEtime(wxGridEvent &event, int row, RowData *rd)
{
   // Remember the value...no need to repopulate
   mGrid->GetCellValue(row, Col_Etime).ToDouble(&rd->etime);

   return;
}
