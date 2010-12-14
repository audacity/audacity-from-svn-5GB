///////////////////////////////////////////////////////////////////////////// 
// Name:        treebook.cpp 
// Purpose:      
// Author:      Hartmut Seichter, Troels K 
// Modified by: James Crook
// Created:     2005/06/21 
// License:     wxWindows license 
///////////////////////////////////////////////////////////////////////////// 

#include <wx/version.h>

#if !wxCHECK_VERSION(2,7,0)

//
// For wxWidgets 2.6.x, use our backported version of wxTreebook
//

#include "wx/listctrl.h" 
#include "wx/statline.h" 
#include "wx/listbook.h"  // wxListbookEvent 
#include "treebook.h" 

class wxTreebookItemData : public wxTreeItemData 
{ 
protected: 
    int m_page; 
public:    
    wxTreebookItemData(int nPage) : wxTreeItemData(), m_page(nPage) { } 
    int GetPage(void) const { return m_page; } 
}; 

// ---------------------------------------------------------------------------- 
// constants 
// ---------------------------------------------------------------------------- 

// margin between the list and the page, should be bigger than wxStaticLine 
// size 
const wxCoord MARGIN = 5; 

// ---------------------------------------------------------------------------- 
// various wxWidgets macros 
// ---------------------------------------------------------------------------- 

// check that the page index is valid 
#define IS_VALID_PAGE(nPage) ((nPage) < GetPageCount()) 

// ---------------------------------------------------------------------------- 
// event table 
// ---------------------------------------------------------------------------- 

IMPLEMENT_DYNAMIC_CLASS(wxTreebook, wxControl) 

const int wxID_TREEBOOKCTRL = wxNewId(); 

BEGIN_EVENT_TABLE(wxTreebook, wxBookCtrlBase) 
    EVT_SIZE(wxTreebook::OnSize) 
    EVT_TREE_SEL_CHANGED(wxID_TREEBOOKCTRL, wxTreebook::OnListSelected) 

    WX_EVENT_TABLE_CONTROL_CONTAINER(wxTreebook)
END_EVENT_TABLE() 

// ============================================================================ 
// wxTreebook implementation 
// ============================================================================ 
#if wxCHECK_VERSION(2,7,0)
WX_DELEGATE_TO_CONTROL_CONTAINER(wxTreebook, wxWindow)
#else
WX_DELEGATE_TO_CONTROL_CONTAINER(wxTreebook)
#endif

// ---------------------------------------------------------------------------- 
// wxTreebook creation 
// ---------------------------------------------------------------------------- 

void wxTreebook::Init() 
{ 
    m_list = NULL; 
#if wxUSE_LINE_IN_LISTBOOK 
    m_line = NULL; 
#endif // wxUSE_LINE_IN_LISTBOOK 
    m_selection = wxNOT_FOUND; 
    m_container.SetContainerWindow(this);
} 

bool 
wxTreebook::Create(wxWindow *parent, 
                   wxWindowID id, 
                   const wxPoint& pos, 
                   const wxSize& size, 
                   long style, 
                   const wxString& name) 
{ 
    if ( (style & wxLB_ALIGN_MASK) == wxLB_DEFAULT ) 
    { 
#ifdef __WXMAC__ 
        style |= wxLB_TOP; 
#else // !__WXMAC__ 
        style |= wxLB_LEFT; 
#endif // __WXMAC__/!__WXMAC__ 
    } 

    // no border for this control, it doesn't look nice together with 
    // wxListCtrl border 
    style &= ~wxBORDER_MASK; 
    style |= wxBORDER_NONE; 

    if ( !wxControl::Create(parent, id, pos, size, style, 
                            wxDefaultValidator, name) ) 
        return false; 

    m_list = new wxTreeCtrl(this,wxID_TREEBOOKCTRL, 
        wxDefaultPosition, 
        wxDefaultSize, 
//        wxTR_HIDE_ROOT | wxTR_SINGLE /*| wxTR_HIDE_ROOT |  wxTR_LINES_AT_ROOT */| wxTR_HAS_BUTTONS); 
        wxTR_HIDE_ROOT | wxTR_SINGLE | wxTR_NO_LINES | wxTR_NO_BUTTONS );

    GetTreeCtrl()->SetIndent(0);
    GetTreeCtrl()->SetSpacing(2);
    GetTreeCtrl()->AddRoot(name); 

#if wxUSE_LINE_IN_LISTBOOK 
    m_line = new wxStaticLine 
                 ( 
                    this, 
                    wxID_ANY, 
                    wxDefaultPosition, 
                    wxDefaultSize, 
                    IsVertical() ? wxLI_HORIZONTAL : wxLI_VERTICAL 
                 ); 
#endif // wxUSE_LINE_IN_LISTBOOK 

#ifdef __WXMSW__ 
    // On XP with themes enabled the GetViewRect used in GetListSize to 
    // determine the space needed for the list view will incorrectly return 
    // (0,0,0,0) the first time.  So send a pending event so OnSize will be 
    // called again after the window is ready to go.  Technically we don't 
    // need to do this on non-XP windows, but if things are already sized 
    // correctly then nothing changes and so there is no harm. 
    wxSizeEvent evt; 
    GetEventHandler()->AddPendingEvent(evt); 
#endif 
    return true; 
} 

// ---------------------------------------------------------------------------- 
// wxTreebook geometry management 
// ---------------------------------------------------------------------------- 
wxSize wxTreebook::GetListSize() const 
{ 
    const wxSize sizeClient = GetClientSize(), 
                 sizeList = m_list->/*GetViewRect().*/GetSize(); 
    wxSize size; 
    if ( IsVertical() ) 
    { 
        size.x = sizeClient.x; 
        size.y = sizeList.y; 
    } 
    else // left/right aligned 
    { 
        size.x = sizeList.x; 
        size.y = sizeClient.y; 
    } 
    return size; 
} 

wxRect wxTreebook::GetPageRect() const 
{ 
    const wxSize sizeList = m_list->GetSize(); 

    wxPoint pt; 
    wxRect rectPage(pt, GetClientSize()); 
    switch ( GetWindowStyle() & wxLB_ALIGN_MASK ) 
    { 
        default: 
            wxFAIL_MSG( _T("unexpected wxTreebook alignment") ); 
            // fall through 

        case wxLB_TOP: 
            rectPage.y = sizeList.y + MARGIN; 
            // fall through 

        case wxLB_BOTTOM: 
            rectPage.height -= sizeList.y + MARGIN; 
            break; 

        case wxLB_LEFT: 
            rectPage.x = sizeList.x + MARGIN; 
            // fall through 

        case wxLB_RIGHT: 
            rectPage.width -= sizeList.x + MARGIN; 
            break; 
    } 
    return rectPage; 
} 

void wxTreebook::OnSize(wxSizeEvent& event) 
{ 
    event.Skip(); 
    if ( !m_list ) 
    { 
        // we're not fully created yet 
        return; 
    } 
    // resize the list control and the page area to fit inside our new size 
    const wxSize sizeClient = GetClientSize(), 
                 sizeList = GetListSize(); 
    wxPoint posList; 
    switch ( GetWindowStyle() & wxLB_ALIGN_MASK ) 
    { 
        default: 
            wxFAIL_MSG( _T("unexpected wxTreebook alignment") ); 
            // fall through 

        case wxLB_TOP: 
        case wxLB_LEFT: 
            // posList is already ok 
            break; 

        case wxLB_BOTTOM: 
            posList.y = sizeClient.y - sizeList.y; 
            break; 

        case wxLB_RIGHT: 
            posList.x = sizeClient.x - sizeList.x; 
            break; 
    } 

    m_list->Move(posList.x, posList.y);
   // JKC Reduce x-size slightly to prevent drift when resizing.
   // The constant of 4 is needed on WindowsXP, 
   //    wxWidgets 2.6.1 (reported JKC)
   // A constant of 0 should be used on Windows XP and on Mac with 
   //    wxWidgets 2.6.3 (reportaed LL)
   // 
   // This is a temporary fix.  We plan to use the real wxTreebook when
   // wxWidgets 2.7.x is ready, so it is not worth investing huge time
   // in patching this fully.

   // If you see the tree shrinking in width, you need a smaller iBorder.
   // If you see the tree increasing in width, you need a larger iBorder.

   // #if wxCHECK_VERSION(2, 6, 2)
   //    const int iBorder = 0;
   // #else 
   //    const int iBorder = 4;
   // #endif

   // m_list->SetClientSize(sizeList.x-iBorder, sizeList.y); 
    m_list->SetSize(sizeList.x, sizeList.y); 

#if wxUSE_LINE_IN_LISTBOOK 
    if ( m_line ) 
    { 
        wxRect rectLine(sizeClient); 
        switch ( GetWindowStyle() & wxLB_ALIGN_MASK ) 
        { 
            case wxLB_TOP: 
                rectLine.y = sizeList.y + 1; 
                rectLine.height = MARGIN - 2; 
                break; 

            case wxLB_BOTTOM: 
                rectLine.height = MARGIN - 2; 
                rectLine.y = sizeClient.y - sizeList.y - rectLine.height; 
                break; 

            case wxLB_LEFT: 
                rectLine.x = sizeList.x + 1; 
                rectLine.width = MARGIN - 2; 
                break; 

            case wxLB_RIGHT: 
                rectLine.width = MARGIN - 2; 
                rectLine.x = sizeClient.x - sizeList.x - rectLine.width; 
                break; 
        } 

        m_line->SetSize(rectLine); 
    } 
#endif // wxUSE_LINE_IN_LISTBOOK 
    // resize the currently shown page 
    if (m_selection != wxNOT_FOUND) 
    { 
        wxWindow *page = m_pages[m_selection]; 
        wxCHECK_RET( page, _T("NULL page in wxTreebook?") ); 
        page->SetSize(GetPageRect()); 
    } 
} 

wxSize wxTreebook::CalcSizeFromPage(const wxSize& sizePage) const 
{ 
    // we need to add the size of the list control and the margin 
    const wxSize sizeList = GetListSize(); 

    wxSize size = sizePage; 
    if ( IsVertical() ) 
    { 
        size.y += sizeList.y + MARGIN; 
    } 
    else // left/right aligned 
    { 
        size.x += sizeList.x + MARGIN; 
    } 

    return size; 
} 

// ---------------------------------------------------------------------------- 
// accessing the pages 
// ---------------------------------------------------------------------------- 
bool wxTreebook::SetPageText(size_t n, const wxString& strText) 
{ 
    m_list->SetItemText(FindItemByPageNum(n), strText); 

    return true; 
} 

wxString wxTreebook::GetPageText(size_t n) const 
{ 
    return m_list->GetItemText(FindItemByPageNum(n)); 
} 

// ---------------------------------------------------------------------------- 
// selection 
// ---------------------------------------------------------------------------- 
int wxTreebook::GetSelection() const 
{ 
    return m_selection; 
} 

int wxTreebook::SetSelection(size_t n) 
{ 
    wxCHECK_MSG( IS_VALID_PAGE(n), wxNOT_FOUND, 
                 wxT("invalid page index in wxTreebook::SetSelection()") ); 

    const int oldSel = m_selection; 

    if ( int(n) != m_selection ) 
    { 
        wxListbookEvent event(wxEVT_COMMAND_LISTBOOK_PAGE_CHANGING, m_windowId); 
        event.SetSelection(n); 
        event.SetOldSelection(m_selection); 
        event.SetEventObject(this); 
        if ( !GetEventHandler()->ProcessEvent(event) || event.IsAllowed() ) 
        { 
            if ( m_selection != wxNOT_FOUND ) 
                m_pages[m_selection]->Hide(); 

            wxWindow *page = m_pages[n]; 
            page->SetSize(GetPageRect()); 
            page->Show(); 

            // change m_selection now to ignore the selection change event 
            m_selection = n; 
            SelectByPageNum(n); 

            // program allows the page change 
            event.SetEventType(wxEVT_COMMAND_LISTBOOK_PAGE_CHANGED); 
            (void)GetEventHandler()->ProcessEvent(event); 
        } 
    } 
    return oldSel; 
} 

// ---------------------------------------------------------------------------- 
// adding/removing the pages 
// ---------------------------------------------------------------------------- 
wxTreeItemId 
wxTreebook::InsertPage(const wxTreeItemId& parent, 
                       size_t n, 
                       wxWindow *page, 
                       const wxString& text, 
                       bool bSelect, 
                       int imageId) 
{ 
    wxTreeItemId _id; 
    if (wxBookCtrlBase::InsertPage(n, page, text, bSelect, imageId) ) 
    { 
       _id = GetTreeCtrl()->AppendItem(parent, text, imageId, imageId, new wxTreebookItemData(n)); 
       if (_id.IsOk()) 
       { 
          if (parent != GetRootItem()) GetTreeCtrl()->Expand(parent); 
          // if the inserted page is before the selected one, we must update the 
          // index of the selected page 
          if ( int(n) <= m_selection ) 
          { 
              // one extra page added 
              m_selection++; 
              SelectByPageNum(m_selection); 
          } 
          // some page should be selected: either this one or the first one if there 
          // is still no selection 
          int selNew = -1; 
          if ( bSelect ) 
              selNew = n; 
          else if ( m_selection == -1 ) selNew = 0; 

          if ( selNew != m_selection ) 
              page->Hide(); 

          //if ( selNew != -1 ) SetSelection(selNew); // this activates TransferFromWindow! it's too early! 

          InvalidateBestSize(); 
       } 
    } 
    return _id; 
} 

wxWindow *wxTreebook::DoRemovePage(size_t page) 
{ 
    const int page_count = GetPageCount(); 
    wxWindow *win = wxBookCtrlBase::DoRemovePage(page); 
    if ( win ) 
    { 
        m_list->Delete(FindItemByPageNum(page)); 

        if (m_selection >= (int)page) 
        { 
            // force new sel valid if possible 
            int sel = m_selection - 1; 
            if (page_count == 1) 
                sel = wxNOT_FOUND; 
            else if ((page_count == 2) || (sel == -1)) 
                sel = 0; 

            // force sel invalid if deleting current page - don't try to hide it 
            m_selection = (m_selection == (int)page) ? wxNOT_FOUND : m_selection - 1; 

            if ((sel != wxNOT_FOUND) && (sel != m_selection)) 
                SetSelection(sel); 
        } 
    } 
    return win; 
} 

bool wxTreebook::DeleteAllPages() 
{ 
    m_list->DeleteAllItems(); 
    return wxBookCtrlBase::DeleteAllPages(); 
} 

// ---------------------------------------------------------------------------- 
// wxTreebook events 
// ---------------------------------------------------------------------------- 

void wxTreebook::OnListSelected(wxTreeEvent& event) 
{ 
   int selNew = wxNOT_FOUND; 

    wxTreeItemId _itemId = event.GetItem(); 
    if (_itemId) 
    { 
       wxTreebookItemData *_item = (wxTreebookItemData *)GetTreeCtrl()->GetItemData(_itemId); 
       if (_item)  selNew = _item->GetPage(); 
    } 

    if ( (selNew == wxNOT_FOUND) || (selNew == m_selection) ) 
    { 
        // this event can only come from our own Select(m_selection) below 
        // which we call when the page change is vetoed, so we should simply 
        // ignore it 
        return; 
    } 

    SetSelection(selNew); 

    // change wasn't allowed, return to previous state 
    if (m_selection != selNew) 
    { 
       SelectByPageNum(m_selection); 
    } 
} 

bool wxTreebook::FindItemRecursively(const wxTreeItemId& idParent, 
                                     wxTreeItemIdValue* cookie, 
                                     int nPage, 
                                     wxTreeItemId* foundid) const
{ 
   wxTreeItemIdValue temp; 
   wxTreeItemId id = cookie ? GetTreeCtrl()->GetNextChild(idParent, *cookie) : GetTreeCtrl()->GetFirstChild(idParent, temp); 
   if (cookie == NULL) cookie = &temp; 

    if(id <= 0) return FALSE; 

    // do something with the thingiee here 
    wxTreebookItemData *_item = (wxTreebookItemData *)GetTreeCtrl()->GetItemData(id); 

    if (nPage == _item->GetPage()) 
    { 
        *foundid = id; 
        return TRUE; 
    } 
    if (GetTreeCtrl()->ItemHasChildren(id)) 
    {        
        if (FindItemRecursively(id,NULL,nPage,foundid)) return TRUE;    
    } 
    if (FindItemRecursively(idParent, cookie, nPage, foundid)) return TRUE; 
    return FALSE; 
} 

wxTreeItemId wxTreebook::FindItemByPageNum(const wxTreeItemId& parent,int nPage) const 
{    
    wxTreeItemId id; 
    FindItemRecursively(parent,NULL,nPage,&id); 
    return id;    
} 

void wxTreebook::SelectByPageNum(int nPage) 
{ 
    GetTreeCtrl()->SelectItem(FindItemByPageNum(nPage)); 
} 

int wxTreebook::GetPageImage(size_t WXUNUSED(n)) const 
{ 
    wxFAIL_MSG( _T("wxListbook::GetPageImage() not implemented") ); 
    return -1; 
} 

bool wxTreebook::SetPageImage(size_t WXUNUSED(n), int WXUNUSED(imageId)) 
{ 
    //return m_list->SetItemImage(n, imageId); 
   return false; 
}

#endif // wxCHECK_VERSION(2,7,0)
