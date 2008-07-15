///////////////////////////////////////////////////////////////////////////// 
// Name:        treebook.h 
// Purpose:      
// Author:      Hartmut Seichter, Troels K 
// Modified by: 
// Created:     2005/06/21 
// License:     wxWindows license 
///////////////////////////////////////////////////////////////////////////// 

#include <wx/version.h>

#if wxCHECK_VERSION(2,7,0)

// wxWidgets 2.7.x and greater does have wxTreebook
#include <wx/treebook.h>

#else

#ifndef __TREEBOOK_H__ 
#define __TREEBOOK_H__ 

#include "wx/treectrl.h" 
#include "wx/bookctrl.h"
#include "wx/containr.h"

// ---------------------------------------------------------------------------- 
// wxTreebook 
// ---------------------------------------------------------------------------- 

class wxTreebook : public wxBookCtrlBase 
{ 
public: 
    wxTreebook() : wxBookCtrlBase() { Init(); } 
    wxTreebook(wxWindow *parent, 
               wxWindowID id, 
               const wxPoint& pos = wxDefaultPosition, 
               const wxSize& size = wxDefaultSize, 
               long style = wxLB_LEFT | wxTAB_TRAVERSAL, 
               const wxString& name = wxEmptyString) : wxBookCtrlBase() 
    { 
        Init(); 

        (void)Create(parent, id, pos, size, style, name); 
    } 

    // quasi ctor 
    bool Create(wxWindow *parent, 
                wxWindowID id, 
                const wxPoint& pos = wxDefaultPosition, 
                const wxSize& size = wxDefaultSize, 
                long style = wxLB_LEFT | wxTAB_TRAVERSAL, 
                const wxString& name = wxEmptyString); 

    wxTreeItemId InsertPage(const wxTreeItemId& parent, 
                            size_t n, 
                            wxWindow *page, 
                            const wxString& text, 
                            bool bSelect = false, 
                            int imageId = -1); 

    wxTreeItemId AddPage(const wxTreeItemId& parent, wxWindow *page, 
                         const wxString& text, 
                         bool bSelect = false, 
                         int imageId = -1) 
    { 
        InvalidateBestSize(); 
        return InsertPage(parent, GetPageCount(), page, text, bSelect, imageId); 
    } 
    virtual bool AddPage(wxWindow *page, 
                         const wxString& text, 
                         bool bSelect = false, 
                         int imageId = -1) 
    { 
        InvalidateBestSize(); 
        return InsertPage(GetRootItem(), GetPageCount(), page, text, bSelect, imageId).IsOk(); 
    } 

    virtual int GetSelection() const; 
    virtual bool SetPageText(size_t n, const wxString& strText); 
    virtual wxString GetPageText(size_t n) const; 
    virtual int GetPageImage(size_t n) const; 
    virtual bool SetPageImage(size_t n, int imageId); 
    virtual wxSize CalcSizeFromPage(const wxSize& sizePage) const; 
    virtual bool InsertPage(size_t n, 
                            wxWindow *page, 
                            const wxString& text, 
                            bool bSelect = false, 
                            int imageId = -1) 
    { 
       return InsertPage(GetRootItem(), n, page, text, bSelect, imageId).IsOk(); 
    } 
    virtual int SetSelection(size_t n);
    int ChangeSelection(size_t n) { return SetSelection(n); };

    // returns true if we have wxLB_TOP or wxLB_BOTTOM style 
    bool IsVertical() const { return HasFlag(wxLB_BOTTOM | wxLB_TOP); } 

    virtual bool DeleteAllPages(); 

          wxTreeCtrl* GetTreeCtrl()       { return m_list; } 
    const wxTreeCtrl* GetTreeCtrl() const { return m_list; } 

protected: 
    virtual wxWindow *DoRemovePage(size_t page); 

    // get the size which the list control should have 
    wxSize GetListSize() const; 

    // get the page area 
    wxRect GetPageRect() const; 

    // event handlers 
    void OnSize(wxSizeEvent& event); 
    void OnListSelected(wxTreeEvent& event); 

    // the list control we use for showing the pages index 
    wxTreeCtrl *m_list; 

#if wxUSE_LINE_IN_LISTBOOK 
    // the line separating it from the page area 
    wxStaticLine *m_line; 
#endif // wxUSE_LINE_IN_LISTBOOK 

    // the currently selected page or wxNOT_FOUND if none 
    int m_selection; 
public: 
    wxTreeItemId GetRootItem() const { return GetTreeCtrl()->GetRootItem(); } 

    bool FindItemRecursively(const wxTreeItemId& idParent, 
                                     wxTreeItemIdValue* cookie, 
                                     int nPage, 
                                     wxTreeItemId* foundid) const; 
    wxTreeItemId FindItemByPageNum(const wxTreeItemId& parent,int nPage) const; 
    wxTreeItemId FindItemByPageNum(int nPage) const { return FindItemByPageNum(GetRootItem(), nPage); } 
    void SelectByPageNum(int nPage); 

protected: 
    // common part of all constructors 
    void Init(); 

    DECLARE_EVENT_TABLE() 
    DECLARE_DYNAMIC_CLASS_NO_COPY(wxTreebook) 

    WX_DECLARE_CONTROL_CONTAINER();
}; 

#endif // __TREEBOOK_H__ 

#endif // wxCHECK_VERSION(2,7,0)
