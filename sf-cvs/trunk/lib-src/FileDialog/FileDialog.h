/////////////////////////////////////////////////////////////////////////////
// Name:        filedlg.h
// Purpose:     wxFileDialog base header
// Author:      Robert Roebling
// Modified by:
// Created:     8/17/99
// Copyright:   (c) Robert Roebling
// RCS-ID:      $Id: FileDialog.h,v 1.1 2007-04-06 10:16:52 llucius Exp $
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#ifndef _FILE_DIALOG_H_
#define _FILE_DIALOG_H_

#include "wx/defs.h"
#include "wx/filedlg.h"

typedef void (*fdCallback)(void *, int);

#if defined(__WXMAC__)
#include "mac/FileDialog.h"
#elif defined(__WXMSW__)
#include "win/FileDialog.h"
#elif defined(__WXGTK__)
#include "gtk/FileDialog.h"
#else
#include "generic/FileDialog.h"
#endif

#endif
