/////////////////////////////////////////////////////////////////////////////
// Name:        src/msw/filedlg.cpp
// Purpose:     wxFileDialog
// Author:      Julian Smart
// Modified by: Leland Lucius
// Created:     01/02/97
// RCS-ID:      $Id: FileDialogPrivate.cpp,v 1.8 2008-05-22 16:12:17 llucius Exp $
// Copyright:   (c) Julian Smart
// Licence:     wxWindows licence
//
// Modified for Audacity to support an additional button on Save dialogs
//
/////////////////////////////////////////////////////////////////////////////

// ============================================================================
// declarations
// ============================================================================

// ----------------------------------------------------------------------------
// headers
// ----------------------------------------------------------------------------

// For compilers that support precompilation, includes "wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
    #pragma hdrstop
#endif

#if wxUSE_FILEDLG && !(defined(__SMARTPHONE__) && defined(__WXWINCE__))

#ifndef WX_PRECOMP
    #include "wx/utils.h"
    #include "wx/msgdlg.h"
    #include "wx/filedlg.h"
    #include "wx/filefn.h"
    #include "wx/intl.h"
    #include "wx/log.h"
    #include "wx/app.h"
#endif

#include "wx/msw/wrapcdlg.h"

#include <stdlib.h>
#include <string.h>

#include "wx/filename.h"
#include "wx/tokenzr.h"
#include "wx/math.h"

#include "wx/msw/missing.h"

#include "../FileDialog.h"

#include <shlobj.h>

// ----------------------------------------------------------------------------
// constants
// ----------------------------------------------------------------------------

#ifdef __WIN32__
# define wxMAXPATH   65534
#else
# define wxMAXPATH   1024
#endif

# define wxMAXFILE   1024

# define wxMAXEXT    5

// ----------------------------------------------------------------------------
// globals
// ----------------------------------------------------------------------------

// standard dialog size
static wxRect gs_rectDialog(0, 0, 428, 266);

// ============================================================================
// implementation
// ============================================================================

IMPLEMENT_CLASS(FileDialog, wxFileDialogBase)

// ----------------------------------------------------------------------------
// hook function for moving the dialog
// ----------------------------------------------------------------------------

UINT_PTR APIENTRY
FileDialogHookFunction(HWND      hDlg,
                       UINT      iMsg,
                       WPARAM    WXUNUSED(wParam),
                       LPARAM    lParam)
{
    HWND   hwndDialog;
    hwndDialog = ::GetParent( hDlg );
    switch (iMsg)
    {
        case WM_INITDIALOG:
            {
#ifdef _WIN64
                SetWindowLongPtr(hDlg, GWLP_USERDATA, lParam);
#else
                SetWindowLong(hDlg, GWL_USERDATA, lParam);
#endif
            }
        case WM_DESTROY:
            {
                RECT dlgRect;
                GetWindowRect( hwndDialog, & dlgRect );
                gs_rectDialog.x = dlgRect.left;
                gs_rectDialog.y = dlgRect.top;
                gs_rectDialog.width = dlgRect.right - dlgRect.left;
                gs_rectDialog.height = dlgRect.bottom - dlgRect.top;
            }
            break;

        case WM_NOTIFY:
            {
                OFNOTIFY *   pNotifyCode;
                pNotifyCode = (LPOFNOTIFY) lParam;
                if (CDN_INITDONE == (pNotifyCode->hdr).code)
                {
                    SetWindowPos( hwndDialog, HWND_TOP,
                                  gs_rectDialog.x,
                                  gs_rectDialog.y,
                                  gs_rectDialog.width,
                                  gs_rectDialog.height,
                                  SWP_NOZORDER|SWP_NOSIZE);

                    OPENFILENAME *ofn = (OPENFILENAME *)
                        GetWindowLongPtr(hDlg, GWLP_USERDATA);
                    FileDialog *me = (FileDialog *)
                        ofn->lCustData;

                    if (!me->m_buttonlabel.IsEmpty())
                    {
                       CommDlg_OpenSave_SetControlText( hwndDialog,
                                                        pshHelp,
                                                        (LPTSTR)me->m_buttonlabel.c_str());
                    }
                 }
                 else if (CDN_HELP == (pNotifyCode->hdr).code)
                 {
                    OPENFILENAME *ofn = (OPENFILENAME *)
                        GetWindowLongPtr(hDlg, GWLP_USERDATA);
                    FileDialog *me = (FileDialog *)
                        ofn->lCustData;
                    HWND w = GetFocus();
                    int index = SendDlgItemMessage(hwndDialog,
                                                   cmb1,
                                                   CB_GETCURSEL,
                                                   0,
                                                   0);
                    me->ClickButton(index);
                    SetFocus(w);
                 }
//                 else if ((CDN_SELCHANGE == (pNotifyCode->hdr).code) ||
//                          (CDN_FOLDERCHANGE == (pNotifyCode->hdr).code))
                 else if (CDN_SELCHANGE == (pNotifyCode->hdr).code)
                 {
                    OPENFILENAME *ofn = (OPENFILENAME *)
                        GetWindowLongPtr(hDlg, GWLP_USERDATA);
                    FileDialog *me = (FileDialog *)
                        ofn->lCustData;
                     me->FilterFiles(hDlg);
                 }
                 else if (CDN_TYPECHANGE == (pNotifyCode->hdr).code)
                 {
                    OPENFILENAME *ofn = (OPENFILENAME *)
                        GetWindowLongPtr(hDlg, GWLP_USERDATA);
                    FileDialog *me = (FileDialog *)
                        ofn->lCustData;
                     me->ParseFilter(ofn->nFilterIndex);
                     me->FilterFiles(hDlg);
                 }
            }
            break;
    }

    // do the default processing
    return 0;
}

void FileDialog::FilterFiles(HWND hDlg)
{
   HWND parent = ::GetParent(hDlg);
   IShellFolder *ishell = NULL;
   LPMALLOC imalloc = NULL;
   HRESULT hr;

   // Get pointer to the ListView control
   HWND lv = ::GetDlgItem(::GetDlgItem(parent, lst2), 1);
   if (lv == NULL)
   {
      wxASSERT(lv != NULL);
      return;
   }

   // Get shell's memory allocation interface (must be Release()'d)
   hr = SHGetMalloc(&imalloc);
   if ((hr != NOERROR) || (imalloc == NULL))
   {
      wxASSERT((hr == NOERROR) && (imalloc != NULL));
      return;
   }

   // Init
   LVITEM lvi;
   wxZeroMemory(lvi);

   // Process all items
   int fltcnt = m_Filters.GetCount();
   int itmcnt = ::SendMessage(lv, LVM_GETITEMCOUNT, 0, 0);
   for (int itm = 0; itm < itmcnt; itm++)
   {
      // Retrieve the file IDL
      lvi.iItem = itm;
      lvi.mask = LVIF_PARAM;
      if (ListView_GetItem(lv, &lvi) != TRUE)
      {
         wxASSERT(FALSE);
         break;
      }
      LPCITEMIDLIST fidl = (LPCITEMIDLIST) lvi.lParam;

      // Retrieve the IShellFolder interface of the parent (must be Release()'d)
      if (ishell == NULL)
      {
         hr = SHBindToParent(fidl, IID_IShellFolder, (void **)&ishell, NULL);
         if (!SUCCEEDED(hr))
         {
            wxASSERT(SUCCEEDED(hr));
            break;
         }
      }

      // Get the attributes of the object
      DWORD attr = SFGAO_FOLDER | SFGAO_STREAM;
      hr = ishell->GetAttributesOf(1, &fidl, &attr);
      if (!SUCCEEDED(hr))
      {
         wxASSERT(SUCCEEDED(hr));
         break;
      }

      // Allow all folders (things like zip files get filtered below)
      if (attr == SFGAO_FOLDER)
      {
         continue;
      }

      // Retrieve the parsable name of the object (includes extension)
      STRRET str;
      hr = ishell->GetDisplayNameOf(fidl, SHGDN_INFOLDER | SHGDN_FORPARSING, &str);
      if (hr != NOERROR)
      {
         // For some objects, we get back an error of 80070057.  I'm assuming this
         // means there is no way to represent the name (like some sort of virtual name)
         // or I've not used the correct PIDL.  But, in either case, it "probably"
         // represents some sort of folder (at least in all cases I've seen), so we
         // simply allow it to display.
         continue;
      }

      // Convert result to wxString
      wxString filename;
      switch (str.uType)
      {
         case STRRET_WSTR:
            filename = str.pOleStr;
            imalloc->Free(str.pOleStr);
         break;

         case STRRET_OFFSET:
            filename = wxString(((char *)fidl) + str.uOffset, wxConvISO8859_1);
         break;

         case STRRET_CSTR:
            filename = wxString(str.cStr, wxConvISO8859_1);
         break;
      }

      // Attempt to match it to all of our filters
      bool match = false;
      for (int flt = 0; flt < fltcnt; flt++)
      {
         if (wxMatchWild(m_Filters[flt], filename, false))
         {
            match = true;
            break;
         }
      }

      // Remove it from the display if it didn't match any of the filters.
      if (!match)
      {
         ListView_DeleteItem(lv, itm);
         itm--;
         itmcnt--;
      }
   }

done:

   // Release the interface
   if (ishell)
   {
      ishell->Release();
   }

   // Release the interface
   if (imalloc)
   {
      imalloc->Release();
   }
}

void FileDialog::ParseFilter(int index)
{
   m_Filters.Empty();

   wxStringTokenizer tokenWild(m_FilterGroups[index - 1], wxT(";"));

   while (tokenWild.HasMoreTokens())
   {
      wxString token = tokenWild.GetNextToken();
      if (m_Filters.Index(token, false) == wxNOT_FOUND)
      {
         m_Filters.Add(token);
      }
   }
}

// ----------------------------------------------------------------------------
// FileDialog
// ----------------------------------------------------------------------------

FileDialog::FileDialog(wxWindow *parent,
                       const wxString& message,
                       const wxString& defaultDir,
                       const wxString& defaultFileName,
                       const wxString& wildCard,
                       long style,
                       const wxPoint& pos)
            : wxFileDialogBase(parent, message, defaultDir, defaultFileName,
                               wildCard, style, pos)

{
    m_dialogStyle = style;

    if ( ( m_dialogStyle & wxMULTIPLE ) && ( m_dialogStyle & wxSAVE ) )
        m_dialogStyle &= ~wxMULTIPLE;

    m_bMovedWindow = false;

    // Must set to zero, otherwise the wx routines won't size the window
    // the second time you call the file dialog, because it thinks it is
    // already at the requested size.. (when centering)
    gs_rectDialog.x =
    gs_rectDialog.y = 0;

    m_callback = NULL;
    m_cbdata = NULL;
}

void FileDialog::GetPaths(wxArrayString& paths) const
{
    paths.Empty();

    wxString dir(m_dir);
    if ( m_dir.Last() != _T('\\') )
        dir += _T('\\');

    size_t count = m_fileNames.GetCount();
    for ( size_t n = 0; n < count; n++ )
    {
        if (wxFileName(m_fileNames[n]).IsAbsolute())
            paths.Add(m_fileNames[n]);
        else
            paths.Add(dir + m_fileNames[n]);
    }
}

void FileDialog::GetFilenames(wxArrayString& files) const
{
    files = m_fileNames;
}

void FileDialog::SetPath(const wxString& path)
{
    wxString ext;
    wxSplitPath(path, &m_dir, &m_fileName, &ext);
    if ( !ext.empty() )
        m_fileName << _T('.') << ext;
}

void FileDialog::DoGetPosition( int *x, int *y ) const
{
    *x = gs_rectDialog.x;
    *y = gs_rectDialog.y;
}


void FileDialog::DoGetSize(int *width, int *height) const
{
    *width  = gs_rectDialog.width;
    *height = gs_rectDialog.height;
}

void FileDialog::DoMoveWindow(int x, int y, int WXUNUSED(width), int WXUNUSED(height))
{
    m_bMovedWindow = true;

    gs_rectDialog.x = x;
    gs_rectDialog.y = y;

    /*
        The width and height can not be set by the programmer
        its just not possible.  But the program can get the
        size of the Dlg after it has been shown, in case they need
        that data.
    */
}

int FileDialog::ShowModal()
{
    HWND hWnd = 0;
    if (m_parent) hWnd = (HWND) m_parent->GetHWND();
    if (!hWnd && wxTheApp->GetTopWindow())
        hWnd = (HWND) wxTheApp->GetTopWindow()->GetHWND();

    static wxChar fileNameBuffer [ wxMAXPATH ];           // the file-name
    wxChar        titleBuffer    [ wxMAXFILE+1+wxMAXEXT ];  // the file-name, without path

    *fileNameBuffer = wxT('\0');
    *titleBuffer    = wxT('\0');

#if WXWIN_COMPATIBILITY_2_4
    long msw_flags = 0;
    if ( (m_dialogStyle & wxHIDE_READONLY) || (m_dialogStyle & wxSAVE) )
        msw_flags |= OFN_HIDEREADONLY;
#else
    long msw_flags = OFN_HIDEREADONLY;
#endif

    if ( m_dialogStyle & wxFILE_MUST_EXIST )
        msw_flags |= OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST;
    /*
        If the window has been moved the programmer is probably
        trying to center or position it.  Thus we set the callback
        or hook function so that we can actually adjust the position.
        Without moving or centering the dlg, it will just stay
        in the upper left of the frame, it does not center
        automatically..  One additional note, when the hook is
        enabled, the PLACES BAR in the dlg (shown on later versions
        of windows (2000 and XP) will automatically be turned off
        according to the MSDN docs.  This is normal.  If the
        programmer needs the PLACES BAR (left side of dlg) they
        just shouldn't move or center the dlg.
    */
    if (m_bMovedWindow) // we need these flags.
    {
        msw_flags |= OFN_EXPLORER|OFN_ENABLEHOOK;
#ifndef __WXWINCE__
        msw_flags |= OFN_ENABLESIZING;
#endif
    }

    if (m_dialogStyle & wxMULTIPLE )
    {
        // OFN_EXPLORER must always be specified with OFN_ALLOWMULTISELECT
        msw_flags |= OFN_EXPLORER | OFN_ALLOWMULTISELECT;
    }

    // if wxCHANGE_DIR flag is not given we shouldn't change the CWD which the
    // standard dialog does by default (notice that under NT it does it anyhow, 
    // OFN_NOCHANGEDIR or not, see below)
    if ( !(m_dialogStyle & wxCHANGE_DIR) )
    {
        msw_flags |= OFN_NOCHANGEDIR;
    }

    if ( m_dialogStyle & wxOVERWRITE_PROMPT )
    {
        msw_flags |= OFN_OVERWRITEPROMPT;
    }

    if ( m_dialogStyle & wxRESIZE_BORDER )
    {
       msw_flags |= OFN_ENABLESIZING;
    }

    if ( m_callback != NULL )
    {
        msw_flags |= OFN_SHOWHELP | OFN_EXPLORER | OFN_ENABLEHOOK;
    }

    // We always need EXPLORER and ENABLEHOOK to use our filtering code
    msw_flags |= OFN_EXPLORER | OFN_ENABLEHOOK;

    OPENFILENAME of;
    wxZeroMemory(of);

    // Allow Places bar to show on supported platforms
    if ( wxGetOsVersion() == wxWINDOWS_NT )
    {
        of.lStructSize       = sizeof(OPENFILENAME);
    }
    else
    {
        of.lStructSize       = OPENFILENAME_SIZE_VERSION_400;
    }

    of.hwndOwner         = hWnd;
    of.lpstrTitle        = WXSTRINGCAST m_message;
    of.lpstrFileTitle    = titleBuffer;
    of.nMaxFileTitle     = wxMAXFILE + 1 + wxMAXEXT;    // Windows 3.0 and 3.1
    of.lCustData         = (LPARAM) this;

    // Convert forward slashes to backslashes (file selector doesn't like
    // forward slashes) and also squeeze multiple consecutive slashes into one
    // as it doesn't like two backslashes in a row neither

    wxString  dir;
    size_t    i, len = m_dir.length();
    dir.reserve(len);
    for ( i = 0; i < len; i++ )
    {
        wxChar ch = m_dir[i];
        switch ( ch )
        {
            case _T('/'):
                // convert to backslash
                ch = _T('\\');

                // fall through

            case _T('\\'):
                while ( i < len - 1 )
                {
                    wxChar chNext = m_dir[i + 1];
                    if ( chNext != _T('\\') && chNext != _T('/') )
                        break;

                    // ignore the next one, unless it is at the start of a UNC path
                    if (i > 0)
                        i++;
                    else
                        break;
                }
                // fall through

            default:
                // normal char
                dir += ch;
        }
    }

    of.lpstrInitialDir   = dir.c_str();

    of.Flags             = msw_flags;
    of.lpfnHook          = FileDialogHookFunction;

    wxArrayString wildDescriptions;

    size_t items = wxParseCommonDialogsFilter(m_wildCard, wildDescriptions, m_FilterGroups);

    wxASSERT_MSG( items > 0 , _T("empty wildcard list") );

    wxString filterBuffer;

    for (i = 0; i < items ; i++)
    {
        filterBuffer += wildDescriptions[i];
        filterBuffer += wxT("|");
        filterBuffer += wxT("*.*");
        filterBuffer += wxT("|");
    }

    // Replace | with \0
    for (i = 0; i < filterBuffer.Len(); i++ ) {
        if ( filterBuffer.GetChar(i) == wxT('|') ) {
            filterBuffer[i] = wxT('\0');
        }
    }

    of.lpstrFilter  = (LPTSTR)filterBuffer.c_str();
    of.nFilterIndex = m_filterIndex + 1;

    ParseFilter(of.nFilterIndex);

    //=== Setting defaultFileName >>=========================================

    wxStrncpy( fileNameBuffer, (const wxChar *)m_fileName, wxMAXPATH-1 );
    fileNameBuffer[ wxMAXPATH-1 ] = wxT('\0');

    of.lpstrFile = fileNameBuffer;  // holds returned filename
    of.nMaxFile  = wxMAXPATH;

    // we must set the default extension because otherwise Windows would check
    // for the existing of a wrong file with wxOVERWRITE_PROMPT (i.e. if the
    // user types "foo" and the default extension is ".bar" we should force it
    // to check for "foo.bar" existence and not "foo")
    wxString defextBuffer; // we need it to be alive until GetSaveFileName()!
    if (m_dialogStyle & wxSAVE && m_dialogStyle & wxOVERWRITE_PROMPT)
    {
        const wxChar* extension = filterBuffer;
        int maxFilter = (int)(of.nFilterIndex*2L) - 1;

        for( int i = 0; i < maxFilter; i++ )           // get extension
            extension = extension + wxStrlen( extension ) + 1;

        // use dummy name a to avoid assert in AppendExtension
        defextBuffer = AppendExtension(wxT("a"), extension);
        if (defextBuffer.StartsWith(wxT("a.")))
        {
            defextBuffer.Mid(2);
            of.lpstrDefExt = defextBuffer.c_str();
        }
    }

    // store off before the standard windows dialog can possibly change it 
    const wxString cwdOrig = wxGetCwd(); 

    //== Execute FileDialog >>=================================================

    bool success = (m_dialogStyle & wxSAVE ? GetSaveFileName(&of)
                                           : GetOpenFileName(&of)) != 0;

#ifdef __WXWINCE__
    DWORD errCode = GetLastError();
#else
    DWORD errCode = CommDlgExtendedError();

    // GetOpenFileName will always change the current working directory on 
    // (according to MSDN) "Windows NT 4.0/2000/XP" because the flag 
    // OFN_NOCHANGEDIR has no effect.  If the user did not specify wxCHANGE_DIR 
    // let's restore the current working directory to what it was before the 
    // dialog was shown (assuming this behavior extends to Windows Server 2003 
    // seems safe). 
    if ( success && 
            (msw_flags & OFN_NOCHANGEDIR) && 
                wxGetOsVersion() == wxWINDOWS_NT ) 
    { 
        wxSetWorkingDirectory(cwdOrig); 
    } 

#ifdef __WIN32__
    if (!success && (errCode == CDERR_STRUCTSIZE))
    {
        // The struct size has changed so try a smaller or bigger size

        int oldStructSize = of.lStructSize;
        of.lStructSize       = oldStructSize - (sizeof(void *) + 2*sizeof(DWORD));
        success = (m_dialogStyle & wxSAVE) ? (GetSaveFileName(&of) != 0)
                                            : (GetOpenFileName(&of) != 0);
        errCode = CommDlgExtendedError();

        if (!success && (errCode == CDERR_STRUCTSIZE))
        {
            of.lStructSize       = oldStructSize + (sizeof(void *) + 2*sizeof(DWORD));
            success = (m_dialogStyle & wxSAVE) ? (GetSaveFileName(&of) != 0)
                                            : (GetOpenFileName(&of) != 0);
        }
    }
#endif // __WIN32__
#endif // __WXWINCE__

    if ( success )
    {
        m_fileNames.Empty();

        if ( ( m_dialogStyle & wxMULTIPLE ) &&
#if defined(OFN_EXPLORER)
             ( fileNameBuffer[of.nFileOffset-1] == wxT('\0') )
#else
             ( fileNameBuffer[of.nFileOffset-1] == wxT(' ') )
#endif // OFN_EXPLORER
           )
        {
#if defined(OFN_EXPLORER)
            m_dir = fileNameBuffer;
            i = of.nFileOffset;
            m_fileName = &fileNameBuffer[i];
            m_fileNames.Add(m_fileName);
            i += m_fileName.Len() + 1;

            while (fileNameBuffer[i] != wxT('\0'))
            {
                m_fileNames.Add(&fileNameBuffer[i]);
                i += wxStrlen(&fileNameBuffer[i]) + 1;
            }
#else
            wxStringTokenizer toke(fileNameBuffer, _T(" \t\r\n"));
            m_dir = toke.GetNextToken();
            m_fileName = toke.GetNextToken();
            m_fileNames.Add(m_fileName);

            while (toke.HasMoreTokens())
                m_fileNames.Add(toke.GetNextToken());
#endif // OFN_EXPLORER

            wxString dir(m_dir);
            if ( m_dir.Last() != _T('\\') )
                dir += _T('\\');

            m_path = dir + m_fileName;
            m_filterIndex = (int)of.nFilterIndex - 1;
        }
        else
        {
            //=== Adding the correct extension >>=================================
            m_filterIndex = (int)of.nFilterIndex - 1;

#if 0
            // LLL:  Removed to prevent adding extension during Export
            //       processing.

            if ( !of.nFileExtension ||
                 (of.nFileExtension && fileNameBuffer[of.nFileExtension] == wxT('\0')) )
            {
                // User has typed a filename without an extension:
                const wxChar* extension = filterBuffer;
                int   maxFilter = (int)(of.nFilterIndex*2L) - 1;

                for( int i = 0; i < maxFilter; i++ )           // get extension
                    extension = extension + wxStrlen( extension ) + 1;

                m_fileName = AppendExtension(fileNameBuffer, extension);
                wxStrncpy(fileNameBuffer, m_fileName.c_str(), wxMin(m_fileName.Len(), wxMAXPATH-1));
                fileNameBuffer[wxMin(m_fileName.Len(), wxMAXPATH-1)] = wxT('\0');
            }
#endif
            m_path = fileNameBuffer;
            m_fileName = wxFileNameFromPath(fileNameBuffer);
            m_fileNames.Add(m_fileName);
            m_dir = wxPathOnly(fileNameBuffer);
        }
    }
    else
    {
        // common dialog failed - why?
#ifdef __WXDEBUG__
#ifdef __WXWINCE__
        if (errCode == 0)
        {
            // OK, user cancelled the dialog
        }
        else if (errCode == ERROR_INVALID_PARAMETER)
        {
            wxLogError(wxT("Invalid parameter passed to file dialog function."));
        }
        else if (errCode == ERROR_OUTOFMEMORY)
        {
            wxLogError(wxT("Out of memory when calling file dialog function."));
        }
        else if (errCode == ERROR_CALL_NOT_IMPLEMENTED)
        {
            wxLogError(wxT("Call not implemented when calling file dialog function."));
        }
        else
        {
            wxLogError(wxT("Unknown error %d when calling file dialog function."), errCode);
        }
#else
        DWORD dwErr = CommDlgExtendedError();
        if ( dwErr != 0 )
        {
            // this msg is only for developers
            wxLogError(wxT("Common dialog failed with error code %0lx."),
                       dwErr);
        }
        //else: it was just cancelled
#endif
#endif
    }

    return success ? wxID_OK : wxID_CANCEL;

}

#endif // wxUSE_FILEDLG && !(__SMARTPHONE__ && __WXWINCE__)

