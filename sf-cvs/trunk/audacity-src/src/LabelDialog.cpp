/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelDialog.cpp

  Dominic Mazzoni

*******************************************************************//**

\class LabelDialog
\brief Dialog used to request a time value.

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

#include "LabelDialog.h"
#include "LabelTrack.h"
#include "Track.h"
#include "widgets/TimeTextCtrl.h"

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

   LabelTrack *track;
   int nndx;
   int tndx;
   int lndx;

   wxString title;
   double stime;
   double etime;
};

BEGIN_EVENT_TABLE(LabelDialog, wxDialog)
   EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, LabelDialog::OnUpdate)
//   EVT_KEY_DOWN(LabelDialog::OnKeyDown)
   EVT_GRID_CELL_CHANGE(LabelDialog::OnCellChange)
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
   vs->Add(mGrid, 1, wxEXPAND | wxALIGN_TOP | wxALIGN_LEFT | wxLEFT | wxRIGHT, 5);
   vs->Add(CreateButtonSizer(wxOK | wxCANCEL),
           0,
           wxCENTER | wxBOTTOM | wxALL,
           10);
   SetSizer(vs);

   int i;

   mGrid->CreateGrid(0, Col_Max);
   mGrid->SetDefaultCellAlignment(wxALIGN_LEFT, wxALIGN_CENTER);

   for (i = 0; i < Col_Max; i++) {
      mGrid->SetColLabelValue(i, headers[i]);
   }

   mChoiceEditor = (ChoiceEditor *) mGrid->GetDefaultEditorForType(GRID_VALUE_CHOICE);
   mTimeEditor = (TimeEditor *) mGrid->GetDefaultEditorForType(GRID_VALUE_TIME);
   mTimeRenderer = (TimeRenderer *) mGrid->GetDefaultRendererForType(GRID_VALUE_TIME);

   wxGridCellAttr *attr = new wxGridCellAttr();
   attr->SetRenderer(mTimeRenderer);
   attr->SetEditor(mTimeEditor);
   attr->SetAlignment(wxALIGN_CENTER, wxALIGN_CENTER);

   mGrid->SetColAttr(Col_Stime, attr);
   mGrid->SetColAttr(Col_Etime, attr->Clone());

   attr = new wxGridCellAttr();
   attr->SetEditor(mChoiceEditor);
   mGrid->SetColAttr(Col_Track, attr);

   FindAllLabels();

   TransferDataToWindow();

   Center();
}

LabelDialog::~LabelDialog()
{
   int cnt = mData.GetCount();

   while (cnt) {
      RowData *rd = mData[--cnt];
      delete rd;
   }
}

bool LabelDialog::TransferDataToWindow()
{
   int cnt = mData.GetCount();
   int i;

   mGrid->BeginBatch();

   if (mGrid->GetNumberRows()) {
      mGrid->DeleteRows(0, mGrid->GetNumberRows());
   }

   mChoiceEditor->SetChoices(mTrackNames);
   mTimeEditor->SetFormat(mFormat);
   mTimeEditor->SetRate(mRate);

   mGrid->AppendRows(cnt);

   int w = 0;
   for (i = 0; i < cnt; i++) {
      RowData *rd = mData[i];

      mGrid->SetCellValue(i, Col_Track, mTrackNames[rd->nndx]);
      mGrid->SetCellValue(i, Col_Label, rd->title);
      w = wxMax(w, mGrid->CellToRect(i, Col_Label).GetWidth());
      mGrid->SetCellValue(i, Col_Stime, wxString::Format(wxT("%g"), rd->stime));
      mGrid->SetCellValue(i, Col_Etime, wxString::Format(wxT("%g"), rd->etime));
   }

   mGrid->AutoSizeRows();

   wxChoice tc(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, mTrackNames);
   mGrid->SetColSize(Col_Track, tc.GetSize().x);
   mGrid->SetColSize(Col_Label, wxMax(150, w));

   mGrid->AutoSizeColumn(Col_Stime);
   mGrid->AutoSizeColumn(Col_Etime);

   mGrid->EndBatch();

   Layout();

   wxRect r = mGrid->BlockToDeviceRect(wxGridCellCoords(0, 0),
                                       wxGridCellCoords(cnt - 1, Col_Max - 1));
   wxScrollBar sb(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSB_VERTICAL);

   // 20 = (left sizer border = 5) + (right sizer border = 5) + (heck if I know = 15)
   r.width += mGrid->GetRowLabelSize() + sb.GetSize().GetWidth() + 25;
   SetClientSize(r.width, 200);

   r = GetRect();
   SetSizeHints(r.GetWidth(), r.GetHeight(), r.GetWidth());

   return true;
}

bool LabelDialog::TransferDataFromWindow()
{
   int cnt = mData.GetCount();
   int i;
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

      wxString lab = mTrackNames[tndx].AfterFirst(wxT('-')).Mid(1);

      LabelTrack *newTrack = new LabelTrack(mDirManager);
      newTrack->SetName(lab);
      mTracks->Add(newTrack);
      tndx++;
   }

   // Repopulate with updated labels
   for (i = 0; i < cnt; i++) {
      RowData *rd = mData[i];
      tndx = 0;

      for (t = iter.First(); t; t = iter.Next()) {
         if (t->GetKind() == Track::Label && rd->nndx == tndx++) {
            break;
         }
      }

      rd->track = (LabelTrack *)t;
      rd->track->AddLabel(rd->stime, rd->etime, rd->title);
   }

   return true;
}

void LabelDialog::FindAllLabels()
{
   TrackListIterator iter(mTracks);
   Track *t;

   for (t = iter.First(); t; t = iter.Next()) {
      if (t->GetKind() == Track::Label) {
         LabelTrack *lt = (LabelTrack *)t;
         wxString lab;
         int tndx = mTrackNames.GetCount();
         int i;

         for (i = 0; i < lt->GetNumLabels(); i++) {
            const LabelStruct *ls = lt->GetLabel(i);
            RowData *rd = new RowData();

            rd->track = lt;
            rd->nndx = tndx;
            rd->tndx = tndx;
            rd->lndx = i;
            rd->stime = ls->t;
            rd->etime = ls->t1;
            rd->title = ls->title;

            mData.Add(rd);
         }

         lab.Printf(wxT("%d - %s"), tndx + 1, t->GetName().c_str());
         mTrackNames.Add(lab);
      }
   }

   mTrackNames.Add(_("New..."));
}

void LabelDialog::OnUpdate(wxCommandEvent &event)
{
   mFormat = event.GetString();

   TransferDataToWindow();

   event.Skip(false);
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

void LabelDialog::OnCellChange(wxGridEvent &event)
{
   int row = event.GetRow();
   RowData *rd = mData[row];

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

   event.Skip(false);

   return;
}

void LabelDialog::OnChangeTrack(wxGridEvent &event, int row, RowData *rd)
{
   wxString val = mGrid->GetCellValue(row, Col_Track);
   if (val == _("New...")) {
      wxTextEntryDialog d(this,
                          _("New Label Track"),
                          _("Enter track name"),
                          _("Label Track"));
      if (d.ShowModal() == wxID_CANCEL) {
         TransferDataToWindow();
         return;
      }
      int nndx = mTrackNames.GetCount();

      val.Printf(wxT("%d - %s"), nndx, d.GetValue().c_str());

      mTrackNames.Insert(val, nndx - 1);
      mGrid->SetCellValue(row, Col_Track, val);
      rd->track = NULL;
   }

   rd->nndx = mTrackNames.Index(val);

   wxGridCellAttr *attr = new wxGridCellAttr();
   attr->SetEditor(new ChoiceEditor(mTrackNames));
   mGrid->SetColAttr(Col_Track, attr);

   return;
}

void LabelDialog::OnChangeLabel(wxGridEvent &event, int row, RowData *rd)
{
   rd->title = mGrid->GetCellValue(row, Col_Label);

   return;
}

void LabelDialog::OnChangeStime(wxGridEvent &event, int row, RowData *rd)
{
   mGrid->GetCellValue(row, Col_Stime).ToDouble(&rd->stime);

   return;
}

void LabelDialog::OnChangeEtime(wxGridEvent &event, int row, RowData *rd)
{
   mGrid->GetCellValue(row, Col_Etime).ToDouble(&rd->etime);

   return;
}
