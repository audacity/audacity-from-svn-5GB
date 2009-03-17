/**********************************************************************

  Audacity: A Digital Audio Editor

  LinkingHtmlWindow.cpp

  Vaughan Johnson
  Dominic Mazzoni

  utility fn and 
  descendant of HtmlWindow that opens links in the user's 
  default browser

**********************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include "../Audacity.h"

#include <wx/mimetype.h>
#include <wx/filename.h>

#include "LinkingHtmlWindow.h"
#include "../HelpText.h"
#include "../FileNames.h"
#include "ErrorDialog.h"

BEGIN_EVENT_TABLE(BrowserFrame, wxFrame)
   EVT_BUTTON(wxID_FORWARD,  BrowserFrame::OnForward)
   EVT_BUTTON(wxID_BACKWARD, BrowserFrame::OnBackward)
   EVT_BUTTON(wxID_CANCEL,   BrowserFrame::OnClose)
   EVT_KEY_DOWN(BrowserFrame::OnKeyDown)
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

void BrowserFrame::OnKeyDown(wxKeyEvent & event)
{
   bool bSkip = true;
   if (event.GetKeyCode() == WXK_ESCAPE)
   {
      bSkip = false; 
      Close(false);
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

#if defined(__WXMSW__)
// Enumeration function to scan windows for a matching process ID
// and, if found, bring the window to the fore.
BOOL CALLBACK ShowWindowFunc(HWND wnd, LPARAM pid)
{
   DWORD id;

   GetWindowThreadProcessId(wnd, &id);
   if (id == pid) {
      SetForegroundWindow(wnd);

      if (IsIconic(wnd)) {
         ShowWindow(wnd, SW_RESTORE);
      }

      return FALSE;
   }

   return TRUE ;
}
#endif

void OpenInDefaultBrowser(const wxHtmlLinkInfo& link)
{
   #ifdef __WXMAC__
      wxString openCmd = wxT("open ") + link.GetHref();
      ::wxExecute(openCmd);
   #else
      #ifdef __WXMSW__
         wxFileType* pFileType = wxTheMimeTypesManager->GetFileTypeFromExtension(wxT(".htm"));
         if (pFileType == NULL)  {
            return;
         }

         wxString openCmd = pFileType->GetOpenCommand(link.GetHref());

         if (openCmd.Lower().Contains(wxT("iexplore.exe"))) {
            // GetOpenCommand is not quite right for Internet Explorer.
            openCmd.Replace(wxT("WWW_OpenURL#\"file://"), wxT("WWW_OpenURL#\""));
         }

         if (openCmd.Lower().Contains(wxT("firefox.exe"))) {
            // For Firefox, we do not want to use DDE so that we can get back 
            // a real process ID.  When using DDE, the window doesn't always
            // come up the top and the WWW_Activate DDE command simply opens
            // an empty browser window.
            //
            // If this doesn't work, then another other option may be to
            // get the process ID via a call to DdeQueryConvInfo() followed
            // by a call to GetWindowThreadProcessId() using the hwndPartner
            // field of the CONVINFO structure.
            openCmd = openCmd.AfterFirst(wxT('#')).BeforeFirst(wxT('#'));
         }

         long pid = ::wxExecute(openCmd);

         // If the returned process ID isn't the fake DDE process ID, then
         // enumerate the windows until a matching process is located and
         // bring the related window to the fore.
         if (pid != -1) {
            EnumWindows((WNDENUMPROC)ShowWindowFunc, pid);
         }

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
   HtmlWindow(parent, id, pos, size, style)
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
         wxGetTopLevelParent(this)->SetLabel( TitleText( href.Mid( 10 )));
      }
   }
   else if( !href.StartsWith( wxT("http:")))
   {
      HtmlWindow::OnLinkClicked( link );
   }
   else
   {
      OpenInDefaultBrowser(link);
      return;
   }
   BrowserFrame * pDlg = wxDynamicCast( GetRelatedFrame(), BrowserFrame );
   if( pDlg )
   {
      pDlg->UpdateButtons();
   };
}
