// This is the wxFileHistory class from wxWindows-2.4.2 with the modifications to allow
// multiple file histories to co-exist from wxWidgets-2.5.2 back-ported to it and
// renamed 'audFileHistory'.
//
// Once Audacity is officially dependent on wxWidgets-2.6.x this class and its files can
// be removed and all instances of 'audFileHistory' in the Audacity code base changed
// to 'wxFileHistory'.
//
// The multiple file history support is required for 'Recent Files' and 'Recent Projects'
// to be able to co-exist in the menu system.

#include "wx/wx.h"
#include "wx/confbase.h"
#include "AudacityFileHistory.h"

// ----------------------------------------------------------------------------
// local constants
// ----------------------------------------------------------------------------

static const wxChar *s_MRUEntryFormat = wxT("&%d %s");

// ----------------------------------------------------------------------------
// File history processor
// ----------------------------------------------------------------------------

audFileHistory::audFileHistory(size_t maxFiles, wxWindowID idBase)
{
    m_fileMaxFiles = maxFiles;
    m_idBase = idBase;
    m_fileHistoryN = 0;
    m_fileHistory = new wxChar *[m_fileMaxFiles];
}

audFileHistory::~audFileHistory()
{
    size_t i;
    for (i = 0; i < m_fileHistoryN; i++)
        delete[] m_fileHistory[i];
    delete[] m_fileHistory;
}

// File history management
void audFileHistory::AddFileToHistory(const wxString& file)
{
    size_t i;

    // Check we don't already have this file
    for (i = 0; i < m_fileHistoryN; i++)
    {
        if ( m_fileHistory[i] && (file == m_fileHistory[i]) )
        {
            // we do have it, move it to the top of the history
            RemoveFileFromHistory (i);
            AddFileToHistory (file);
            return;
        }
    }

    // if we already have a full history, delete the one at the end
    if ( m_fileMaxFiles == m_fileHistoryN )
    {
        RemoveFileFromHistory (m_fileHistoryN - 1);
        AddFileToHistory (file);
        return;
    }

    // Add to the project file history:
    // Move existing files (if any) down so we can insert file at beginning.
    if (m_fileHistoryN < m_fileMaxFiles)
    {
        wxNode* node = m_fileMenus.First();
        while (node)
        {
            wxMenu* menu = (wxMenu*) node->Data();
            if ( m_fileHistoryN == 0 && menu->GetMenuItemCount() )
            {
                menu->AppendSeparator();
            }
            menu->Append(m_idBase+m_fileHistoryN, _("[EMPTY]"));
            node = node->Next();
        }
        m_fileHistoryN ++;
    }
    // Shuffle filenames down
    for (i = (m_fileHistoryN-1); i > 0; i--)
    {
        m_fileHistory[i] = m_fileHistory[i-1];
    }
    m_fileHistory[0] = copystring(file);

    // this is the directory of the last opened file
    wxString pathCurrent;
    wxSplitPath( m_fileHistory[0], &pathCurrent, NULL, NULL );
    for (i = 0; i < m_fileHistoryN; i++)
    {
        if ( m_fileHistory[i] )
        {
            // if in same directory just show the filename; otherwise the full
            // path
            wxString pathInMenu, path, filename, ext;
            wxSplitPath( m_fileHistory[i], &path, &filename, &ext );
            if ( path == pathCurrent )
            {
                pathInMenu = filename;
                if ( !ext.empty() )
                    pathInMenu = pathInMenu + wxFILE_SEP_EXT + ext;
            }
            else
            {
                // absolute path; could also set relative path
                pathInMenu = m_fileHistory[i];
            }

            wxString buf;
            buf.Printf(s_MRUEntryFormat, i + 1, pathInMenu.c_str());
            wxNode* node = m_fileMenus.First();
            while (node)
            {
                wxMenu* menu = (wxMenu*) node->Data();
                menu->SetLabel(m_idBase + i, buf);
                node = node->Next();
            }
        }
    }
}

void audFileHistory::RemoveFileFromHistory(size_t i)
{
    wxCHECK_RET( i < m_fileHistoryN,
                 wxT("invalid index in audFileHistory::RemoveFileFromHistory") );

        // delete the element from the array (could use memmove() too...)
        delete [] m_fileHistory[i];

        size_t j;
        for ( j = i; j < m_fileHistoryN - 1; j++ )
        {
            m_fileHistory[j] = m_fileHistory[j + 1];
        }

    wxNode* node = m_fileMenus.First();
    while ( node )
    {
        wxMenu* menu = (wxMenu*) node->Data();


        // shuffle filenames up
        wxString buf;
        for ( j = i; j < m_fileHistoryN - 1; j++ )
        {
            buf.Printf(s_MRUEntryFormat, j + 1, m_fileHistory[j]);
            menu->SetLabel(m_idBase + j, buf);
        }

        node = node->Next();

        // delete the last menu item which is unused now
        wxWindowID lastItemId = m_idBase + m_fileHistoryN - 1;
        if (menu->FindItem(lastItemId))
  	    {
  	        menu->Delete(lastItemId);
  	    }

        // delete the last separator too if no more files are left
        if ( m_fileHistoryN == 1 )
        {
            wxMenuItemList::Node *node = menu->GetMenuItems().GetLast();
            if ( node )
            {
                wxMenuItem *menuItem = node->GetData();
                if ( menuItem->IsSeparator() )
                {
                    menu->Delete(menuItem);
                }
                //else: should we search backwards for the last separator?
            }
            //else: menu is empty somehow
        }
    }

    m_fileHistoryN--;
}

wxString audFileHistory::GetHistoryFile(size_t i) const
{
    wxString s;
    if ( i < m_fileHistoryN )
    {
        s = m_fileHistory[i];
    }
    else
    {
        wxFAIL_MSG( wxT("bad index in audFileHistory::GetHistoryFile") );
    }

    return s;
}

void audFileHistory::UseMenu(wxMenu *menu)
{
    if (!m_fileMenus.Member(menu))
        m_fileMenus.Append(menu);
}

void audFileHistory::RemoveMenu(wxMenu *menu)
{
    m_fileMenus.DeleteObject(menu);
}

#if wxUSE_CONFIG
void audFileHistory::Load(wxConfigBase& config)
{
    m_fileHistoryN = 0;
    wxString buf;
    buf.Printf(wxT("file%d"), (int)m_fileHistoryN + 1);
    wxString historyFile;
    while ((m_fileHistoryN < m_fileMaxFiles) && config.Read(buf, &historyFile) && (historyFile != wxT("")))
    {
        m_fileHistory[m_fileHistoryN] = copystring((const wxChar*) historyFile);
        m_fileHistoryN ++;
        buf.Printf(wxT("file%d"), m_fileHistoryN + 1);
        historyFile = wxT("");
    }
    AddFilesToMenu();
}

void audFileHistory::Save(wxConfigBase& config)
{
    size_t i;
    for (i = 0; i < m_fileHistoryN; i++)
    {
        wxString buf;
        buf.Printf(wxT("file%d"), (int)i + 1);
        config.Write(buf, wxString(m_fileHistory[i]));
    }
}
#endif // wxUSE_CONFIG

void audFileHistory::AddFilesToMenu()
{
    if (m_fileHistoryN > 0)
    {
        wxNode* node = m_fileMenus.First();
        while (node)
        {
            wxMenu* menu = (wxMenu*) node->Data();
            if (menu->GetMenuItemCount())
            {
                menu->AppendSeparator();
            }
            size_t i;
            for (i = 0; i < m_fileHistoryN; i++)
            {
                if (m_fileHistory[i])
                {
                    wxString buf;
                    buf.Printf(s_MRUEntryFormat, i+1, m_fileHistory[i]);
                    menu->Append(m_idBase + i, buf);
                }
            }
            node = node->Next();
        }
    }
}

void audFileHistory::AddFilesToMenu(wxMenu* menu)
{
    if (m_fileHistoryN > 0)
    {
        if (menu->GetMenuItemCount())
        {
            menu->AppendSeparator();
        }
        size_t i;
        for (i = 0; i < m_fileHistoryN; i++)
        {
            if (m_fileHistory[i])
            {
                wxString buf;
                buf.Printf(s_MRUEntryFormat, i+1, m_fileHistory[i]);
                menu->Append(m_idBase + i, buf);
            }
        }
    }
}
