/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportQT.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_IMPORT_QT__
#define __AUDACITY_IMPORT_QT__

class ImportPluginList;
class UnusableImportPluginList;

void GetQTImportPlugin(ImportPluginList *importPluginList,
                       UnusableImportPluginList *unusableImportPluginList);

#ifdef USE_QUICKTIME

void InitQuicktime();
void ExitQuicktime();

#endif

#endif

// Indentation settings for Vim and Emacs.
// Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3


