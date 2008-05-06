/**********************************************************************

  Audacity: A Digital Audio Editor

  LinkingHtmlWindow.cpp

  Vaughan Johnson
  Dominic Mazzoni

  utility fn and 
  descendant of wxHtmlWindow that opens links in the user's 
  default browser

**********************************************************************/

#include "../Audacity.h"

#include <wx/mimetype.h>
#include <wx/filename.h>

#include "LinkingHtmlWindow.h"
#include "../HelpText.h"
#include "../FileNames.h"
#include "ErrorDialog.h"

BEGIN_EVENT_TABLE(BrowserFrame, wxFrame)
   EVT_BUTTON( wxID_FORWARD,  BrowserFrame::OnForward)
   EVT_BUTTON( wxID_BACKWARD, BrowserFrame::OnBackward)
   EVT_BUTTON( wxID_CLOSE,    BrowserFrame::OnClose)
   EVT_CHAR(BrowserFrame::OnChar)
END_EVENT_TABLE()


void BrowserFrame::OnForward(wxCommandEvent & event)
{
   mpHtml->HistoryForward();
   UpdateButtons();
}

void BrowserFrame::OnBackward(wxCommandEvent & event)
{
   mpHtml->HistoryBack();
   UpdateButtons();
}

void BrowserFrame::OnClose(wxCommandEvent & event)
{
   Close();
}

void BrowserFrame::OnChar(wxKeyEvent & event)
{
   bool bSkip = true;
   if (event.GetKeyCode() == WXK_ESCAPE)
   {
      bSkip = false; 
      this->Show(FALSE);
   }
   event.Skip(bSkip);
}


void BrowserFrame::UpdateButtons()
{
   wxWindow * pWnd;
   if( (pWnd = FindWindowById( wxID_BACKWARD, this )) != NULL )
   {
      pWnd->Enable(mpHtml->HistoryCanBack());
   }
   if( (pWnd = FindWindowById( wxID_FORWARD, this )) != NULL )
   {
      pWnd->Enable(mpHtml->HistoryCanForward());
   }
}

void OpenInDefaultBrowser(const wxHtmlLinkInfo& link)
{
   #ifdef __WXMAC__
      wxString openCmd = wxT("open ") + link.GetHref();
      ::wxExecute(openCmd);
   #else
      #ifdef __WXMSW__
         wxFileType* pFileType = wxTheMimeTypesManager->GetFileTypeFromExtension(wxT(".htm"));
         if (pFileType == NULL) 
            return;
         wxString openCmd = pFileType->GetOpenCommand(link.GetHref());
         if (openCmd.Lower().Contains(wxT("iexplore.exe")))
            // GetOpenCommand is not quite right for Internet Explorer.
            openCmd.Replace(wxT("WWW_OpenURL#\"file://"), wxT("WWW_OpenURL#\""));
         ::wxExecute(openCmd);
         delete pFileType;
      #else
         wxLaunchDefaultBrowser(link.GetHref());
      #endif
   #endif
};

LinkingHtmlWindow::LinkingHtmlWindow(wxWindow *parent, wxWindowID id /*= -1*/, 
                                       const wxPoint& pos /*= wxDefaultPosition*/, 
                                       const wxSize& size /*= wxDefaultSize*/, 
                                       long style /*= wxHW_SCROLLBAR_AUTO*/) :
   wxHtmlWindow(parent, id, pos, size, style)
{

}

void LinkingHtmlWindow::OnLinkClicked(const wxHtmlLinkInfo& link)
{
   wxString href = link.GetHref();
   if( href.StartsWith(wxT("innerlink:")) )
   {
      wxString FileName = 
         wxFileName( FileNames::HtmlHelpDir(), href.Mid( 10 ) + wxT(".htm") ).GetFullPath();
      if( wxFileExists( FileName ) )
      {
         ShowHelpDialog(NULL, FileName, wxT(""));
         return;
      }
      else
      {
         SetPage( HelpText( href.Mid( 10 )));
         GetParent()->SetLabel( TitleText( href.Mid( 10 )));
      }
   }
   else if( !href.StartsWith( wxT("http:")))
   {
      wxHtmlWindow::OnLinkClicked( link );
   }
   else
   {
      OpenInDefaultBrowser(link);
      return;
   }
   BrowserFrame * pDlg = wxDynamicCast( GetParent(), BrowserFrame );
   if( pDlg )
   {
      pDlg->UpdateButtons();
   };
}


