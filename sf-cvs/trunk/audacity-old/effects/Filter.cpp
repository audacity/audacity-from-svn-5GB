/**********************************************************************

  Audacity: A Digital Audio Editor

  Filter.cpp

  Dominic Mazzoni

**********************************************************************/

#include <math.h>

#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/brush.h>
#include <wx/image.h>
#include <wx/dcmemory.h>

#include "Filter.h"
#include "../Envelope.h"
#include "../FFT.h"
#include "../WaveTrack.h"

EffectFilter::EffectFilter()
{
   mEnvelope = new Envelope();
   mEnvelope->Flatten(0.5);
   mEnvelope->SetTrackLen(1.0);
   mEnvelope->Mirror(false);

   windowSize = 256;
   filterFunc = new float[windowSize];
}

bool EffectFilter::Begin(wxWindow *parent)
{
   FilterDialog dlog(parent, -1, "FFT Filter");
   dlog.SetEnvelope(mEnvelope);

   dlog.CentreOnParent();
   dlog.ShowModal();
   
   if (!dlog.GetReturnCode())
      return false;

   for(int i=0; i<=windowSize/2; i++) {
      double envVal = mEnvelope->GetValue(((double)i)/(windowSize/2));
      filterFunc[i] = (float)(pow(10.0, envVal*2.0 - 1.0));
   }
   
   return true;
}

bool EffectFilter::DoIt(WaveTrack *t,
                        sampleCount start,
                        sampleCount len)
{
   sampleCount s = start;
   sampleCount idealBlockLen = 65536;
   
   sampleType *buffer = new sampleType[idealBlockLen];
   
   sampleType *window1 = new sampleType[windowSize];
   sampleType *window2 = new sampleType[windowSize];
   sampleType *thisWindow = window1;
   sampleType *lastWindow = window2;
   
   int i;
   
   for(i=0; i<windowSize; i++)
      lastWindow[i] = 0;
   
   while(len) {
      int block = idealBlockLen;
      if (block > len)
         block = len;
      
      t->Get(buffer, s, block);
      
      for(i=0; i<block; i+=windowSize/2) {
         int wlen = i + windowSize;
         int wcopy = windowSize;
         if (i + wcopy > block)
            wcopy = block - i;
         
         int j;
         for(j=0; j<wcopy; j++)
            thisWindow[j] = buffer[i+j];
         for(j=wcopy; j<windowSize; j++)
            thisWindow[j] = 0;
         
         Filter(windowSize, thisWindow);
         
         for(j=0; j<windowSize/2; j++)
            buffer[i+j] = thisWindow[j] + lastWindow[windowSize/2 + j];
         
         sampleType *tempP = thisWindow;
         thisWindow = lastWindow;
         lastWindow = tempP;
      }
      
      if (len > block && len > windowSize/2)
         block -= windowSize/2;
      
      t->Set(buffer, s, block);
      
      len -= block;
      s += block;
   }
   
   delete[] buffer;
   delete[] window1;
   delete[] window2;
   
   return true;
}

void EffectFilter::Filter(sampleCount len,
                          sampleType *buffer)
{
   float *inr = new float[len];
   float *ini = new float[len];
   float *outr = new float[len];
   float *outi = new float[len];
   
   int i;
   
   for(i=0; i<len; i++)
      inr[i] = buffer[i]/32767.;
   
   WindowFunc(2, len, inr);
   
   FFT(len, false, inr, NULL, outr, outi);
   
   //  RealFFT(len, inr, outr, outi);
   
   // Apply filter
   
   int half = len/2;
   for(i=0; i<=half; i++) {
      int j = len - i;
      
      outr[i] = outr[i]*filterFunc[i];
      outi[i] = outi[i]*filterFunc[i];
      
      if (i!=0 && i!=len/2) {
         outr[j] = outr[j]*filterFunc[i];
         outi[j] = outi[j]*filterFunc[i];
      }
   }
   
   FFT(len, true, outr, outi, inr, ini);
   
   for(i=0; i<len; i++)
      buffer[i] = sampleType(inr[i]*32767);
}

//----------------------------------------------------------------------------
// FilterPanel
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(FilterPanel, wxPanel)
    EVT_PAINT(FilterPanel::OnPaint)
    EVT_MOUSE_EVENTS(FilterPanel::OnMouseEvent)
END_EVENT_TABLE()

FilterPanel::FilterPanel( wxWindow *parent, wxWindowID id,
                          const wxPoint& pos,
                          const wxSize& size):
   wxPanel(parent, id, pos, size)
{
   mBitmap = NULL;
   mWidth = 0;
   mHeight = 0;
}

void FilterPanel::OnPaint(wxPaintEvent & evt)
{
   wxPaintDC dc(this);

   int width, height;
   GetSize(&width, &height);
 
   if (!mBitmap || mWidth!=width || mHeight!=height) {
      if (mBitmap)
         delete mBitmap;

      mWidth = width;
      mHeight = height;
      mBitmap = new wxBitmap(mWidth, mHeight);
   }
  
   wxMemoryDC memDC;
   memDC.SelectObject(*mBitmap);

   wxRect border;
   border.x = 0;
   border.y = 0;
   border.width = mWidth;
   border.height = mHeight;

   memDC.SetBrush(*wxWHITE_BRUSH);
   memDC.SetPen(*wxBLACK_PEN);
   memDC.DrawRectangle(border);

   wxRect r = border;
   r.x += 2;
   r.width -= 8;
   r.y += 3;
   r.height -= 6;

   // Pure blue x-axis line
   memDC.SetPen(wxPen(wxColour(0, 0, 255), 1, wxSOLID));
   int center = r.height/2;
   memDC.DrawLine(0, center, r.width, center);

   // Med-blue envelope line
   memDC.SetPen(wxPen(wxColour(128, 128, 255), 1, wxSOLID));

   // Draw envelope
   double *values = new double[r.width];
   mEnvelope->GetValues(values, r.width, 0.0, 1.0/r.width);
   for(int x=0; x<r.width; x++) {
      memDC.DrawLine(x, r.height-r.height*values[x]-2,
                     x, r.height-r.height*values[x]+2);
   }
   delete[] values;
   r.y -= 3;
   mEnvelope->Draw(memDC, r, 0.0, r.width, false);
   r.y += 3;

   // Paint border again
   memDC.SetBrush(*wxTRANSPARENT_BRUSH);
   memDC.SetPen(*wxBLACK_PEN);
   memDC.DrawRectangle(border);

   dc.Blit(0, 0, mWidth, mHeight,
           &memDC, 0, 0, wxCOPY, FALSE);
}

void FilterPanel::OnMouseEvent(wxMouseEvent & event)
{
   wxRect r;
   r.x = 0;
   r.y = 0;
   r.width = mWidth;
   r.height = mHeight;

   if (mEnvelope->MouseEvent(event, r, 0.0, mWidth, false))
      Refresh(false);
}

// WDR: class implementations

//----------------------------------------------------------------------------
// FilterDialog
//----------------------------------------------------------------------------

// WDR: event table for FilterDialog

BEGIN_EVENT_TABLE(FilterDialog,wxDialog)
   EVT_BUTTON( wxID_OK, FilterDialog::OnOk )
   EVT_BUTTON( wxID_CANCEL, FilterDialog::OnCancel )
   EVT_SIZE( FilterDialog::OnSize )
   EVT_BUTTON( ID_CLEAR, FilterDialog::OnClear )
END_EVENT_TABLE()

FilterDialog::FilterDialog(wxWindow *parent, wxWindowID id,
                           const wxString &title,
                           const wxPoint &position, const wxSize& size,
                           long style ) :
   wxDialog( parent, id, title, position, size, style )
{
   MakeFilterDialog( this, TRUE ); 
   
   SetSizeHints(300, 200, 20000, 20000);
}

void FilterDialog::SetEnvelope(Envelope *env)
{
   ((FilterPanel *) FindWindow(ID_FILTERPANEL))->mEnvelope = env;
}

bool FilterDialog::Validate()
{
   return TRUE;
}

bool FilterDialog::TransferDataToWindow()
{
   return TRUE;
}

bool FilterDialog::TransferDataFromWindow()
{
   return TRUE;
}

// WDR: handler implementations for FilterDialog

void FilterDialog::OnClear( wxCommandEvent &event )
{
    
}

void FilterDialog::OnSize(wxSizeEvent &event)
{
   event.Skip();
}

void FilterDialog::OnOk(wxCommandEvent &event)
{
   TransferDataFromWindow();

   if (Validate())
      EndModal(true);
   else {
      event.Skip();
   }
}

void FilterDialog::OnCancel(wxCommandEvent &event)
{
   EndModal(false);
}

wxSizer *MakeFilterDialog( wxPanel *parent, bool call_fit, bool set_sizer )
{
   wxBoxSizer *item0 = new wxBoxSizer( wxVERTICAL );

   wxStaticText *item1 = new wxStaticText( parent, ID_TEXT, "FFT Filter by Dominic Mazzoni", wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE );
   item0->Add( item1, 0, wxALIGN_CENTRE|wxALL, 5 );

   wxWindow *item2 = new FilterPanel( parent, ID_FILTERPANEL, wxDefaultPosition, wxDefaultSize );
   wxASSERT( item2 );
   item0->Add( item2, 1, wxGROW|wxALIGN_CENTRE|wxALL, 5 );

   wxBoxSizer *item3 = new wxBoxSizer( wxHORIZONTAL );

   wxButton *item4 = new wxButton( parent, ID_CLEAR, "Clear", wxDefaultPosition, wxDefaultSize, 0 );
   item3->Add( item4, 0, wxALIGN_CENTRE|wxALL, 5 );

   item3->Add( 20, 20, 1, wxALIGN_CENTRE|wxALL, 5 );

   wxButton *item5 = new wxButton( parent, wxID_OK, "OK", wxDefaultPosition, wxDefaultSize, 0 );
   item5->SetDefault();
   item3->Add( item5, 0, wxALIGN_CENTRE|wxALL, 5 );

   wxButton *item6 = new wxButton( parent, wxID_CANCEL, "Cancel", wxDefaultPosition, wxDefaultSize, 0 );
   item3->Add( item6, 0, wxALIGN_CENTRE|wxALL, 5 );

   item0->Add( item3, 0, wxGROW|wxALIGN_CENTER_VERTICAL|wxALL, 5 );

   if (set_sizer)
      {
         parent->SetAutoLayout( TRUE );
         parent->SetSizer( item0 );
         if (call_fit)
            {
               item0->Fit( parent );
               item0->SetSizeHints( parent );
            }
      }
    
   return item0;
}

// Implement menu bar functions

// Implement bitmap functions


// End of generated file



