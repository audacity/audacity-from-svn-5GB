/**********************************************************************

  Audacity: A Digital Audio Editor

  LinkingHtmlWindow.cpp

  Vaughan Johnson
  Dominic Mazzoni

  utility fn and 
  descendant of wxHtmlWindow that opens links in the user's 
  default browser
  
**********************************************************************/

#include "LinkingHtmlWindow.h"
#include <wx/mimetype.h>
#include "../HelpText.h"

BEGIN_EVENT_TABLE(BrowserFrame, wxFrame)
   EVT_BUTTON( wxID_FORWARD,  BrowserFrame::OnForward)
   EVT_BUTTON( wxID_BACKWARD, BrowserFrame::OnBackward)
   EVT_BUTTON( wxID_CLOSE,    BrowserFrame::OnClose)
END_EVENT_TABLE()


void BrowserFrame::OnForward(wxCommandEvent & event)
{
   mpHtml->HistoryForward();
}

void BrowserFrame::OnBackward(wxCommandEvent & event)
{
   mpHtml->HistoryBack();
}

void BrowserFrame::OnClose(wxCommandEvent & event)
{
   Close();
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
      SetPage( HelpText( href.Mid( 10 )));
      GetParent()->SetLabel( TitleText( href.Mid( 10 )));
      return;
   }
   else if( !href.StartsWith( wxT("http:")))
   {
      wxHtmlWindow::OnLinkClicked( link );
      return;
   }
   OpenInDefaultBrowser(link);
}

#if 0
void LinkingHtmlWindow::OnSetTitle(const wxString& title)
{
   wxLogDebug( wxT("Title: %s"), title );
   BrowserDialog * pDlg = wxDynamicCast( GetParent(), BrowserDialog );
   if( pDlg )
   {
      pDlg->SetTitle( title );
   };
   wxHtmlWindow::OnSetTitle( title );
}
#endif

