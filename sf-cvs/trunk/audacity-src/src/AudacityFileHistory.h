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

#ifndef __AUDACITY_FILE_HISTORY__
#define __AUDACITY_FILE_HISTORY__

#include "wx/docview.h"

// ----------------------------------------------------------------------------
// File history management
// ----------------------------------------------------------------------------

class WXDLLEXPORT audFileHistory : public wxObject
{
public:
    audFileHistory(size_t maxFiles = 9, wxWindowID idBase = wxID_FILE1);
    ~audFileHistory();

    // Operations
    virtual void AddFileToHistory(const wxString& file);
    virtual void RemoveFileFromHistory(size_t i);
    virtual size_t GetMaxFiles() const { return m_fileMaxFiles; }
    virtual void UseMenu(wxMenu *menu);

    // Remove menu from the list (MDI child may be closing)
    virtual void RemoveMenu(wxMenu *menu);

#if wxUSE_CONFIG
    virtual void Load(wxConfigBase& config);
    virtual void Save(wxConfigBase& config);
#endif // wxUSE_CONFIG

    virtual void AddFilesToMenu();
    virtual void AddFilesToMenu(wxMenu* menu); // Single menu

    // Accessors
    virtual wxString GetHistoryFile(size_t i) const;

    // A synonym for GetNoHistoryFiles
    virtual size_t GetCount() const { return m_fileHistoryN; }
    size_t GetNoHistoryFiles() const { return m_fileHistoryN; }

    wxList& GetMenus() const { return (wxList&) m_fileMenus; }

protected:
    // Last n files
    wxChar**          m_fileHistory;
    // Number of files saved
    size_t            m_fileHistoryN;
    // Menus to maintain (may need several for an MDI app)
    wxList            m_fileMenus;
    // Max files to maintain
    size_t            m_fileMaxFiles;
    
private:
    // The ID of the first history menu item (Doesn't have to be wxID_FILE1)
    wxWindowID m_idBase;

    //DECLARE_DYNAMIC_CLASS(audFileHistory)
};

#endif   // __AUDACITY_FILE_HISTORY__
