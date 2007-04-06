/**********************************************************************

  Audacity: A Digital Audio Editor

  FileDialog.cpp

  Leland Lucius

*******************************************************************//**

\class FileDialog
\brief Dialog used to present platform specific "Save As" dialog with
custom controls.

*//*******************************************************************/

#include "FileDialog.h"

#if defined(__WXMAC__)
#include "mac/FileDialog.hpp"
#elif defined(__WXMSW__)
#include "win/FileDialog.hpp"
#elif defined(__WXGTK__)
#include "gtk/FileDialog.hpp"
#else
#include "generic/FileDialog.hpp"
#endif

void FileDialog::EnableButton(wxString label, fdCallback cb, void *data)
{
   m_buttonlabel = label;
   m_callback = cb;
   m_cbdata = data;
}

void FileDialog::ClickButton(int index)
{
   if (m_callback)
   {
      m_callback(m_cbdata, index);
   }
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 94f72c32-970b-4f4e-bbf3-3880fce7b965
