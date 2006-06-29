/**********************************************************************

  Audacity: A Digital Audio Editor

  Legacy.cpp

  Dominic Mazzoni

*******************************************************************//*!

\file Legacy.cpp
\brief Converts old Audacity file types.  Implements 
AutoRollbackRenamer.

  These routines convert Audacity project files from the
  0.98...1.0 format into an XML format that's compatible with
  Audacity 1.2.0 and newer.

*//****************************************************************//**

\class AutoRollbackRenamer
\brief AutoRollbackRenamer handles the renaming of files
which is needed when producing a new version of a file which may fail.
On failure the old version is put back in place.

*//*******************************************************************/


#include "Audacity.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <wx/defs.h>
#include <wx/ffile.h>
#include <wx/filefn.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/string.h>
#include <wx/textfile.h>

#include "Internat.h"
#include "Legacy.h"

class AutoRollbackRenamer {
public:
   AutoRollbackRenamer(wxString oldName, wxString newName) {
      mOldName = oldName;
      mNewName = newName;
      mRenameSucceeded = ::wxRenameFile(FILENAME(mOldName), FILENAME(mNewName));
      mFinished = false;
      mNewFile = NULL;
   }
   ~AutoRollbackRenamer()
   {
      if (mNewFile)
         fclose(mNewFile);

      if (mRenameSucceeded && !mFinished) {
         ::wxRemoveFile(FILENAME(mOldName));
         ::wxRenameFile(FILENAME(mNewName), FILENAME(mOldName));
      }
   }
   bool RenameSucceeded()
   {
      return mRenameSucceeded;
   }
   void Finished()
   {
      mFinished = true;
   }
   void SetNewFile(FILE *f)
   {
      mNewFile = f;
   }

   wxString mOldName, mNewName;
   bool mRenameSucceeded;
   bool mFinished;
   FILE *mNewFile;
};

bool ConvertLegacyTrack(wxTextFile *f, FILE *outf)
{
   wxString line;
   wxString kind;

   kind = (*f)[f->GetCurrentLine()];

   if (kind == wxT("WaveTrack")) {
      fprintf(outf, "\t<wavetrack name='%s'",
              (const char *)f->GetNextLine().mb_str());

      wxString channel = f->GetNextLine();
      if (channel == wxT("left")) {
         fprintf(outf, " channel='0'");
         line = f->GetNextLine();
      }
      else if (channel == wxT("right")) {
         fprintf(outf, " channel='1'");
         line = f->GetNextLine();
      }
      else if (channel == wxT("mono")) {
         fprintf(outf, " channel='2'");
         line = f->GetNextLine();
      }
      else {
         fprintf(outf, " channel='2'");
         line = channel;
      }

      if (line == wxT("linked")) {
         fprintf(outf, " linked='1'");
         line = f->GetNextLine();
      }

      if (line != wxT("offset"))
         return false;
      fprintf(outf, " offset='%s'", (const char *)f->GetNextLine().mb_str());

      long envLen;

      if (f->GetNextLine() != wxT("EnvNumPoints"))
         return false;
      line = f->GetNextLine();
      line.ToLong(&envLen);
      if (envLen < 0 || envLen > 10000)
         return false;

      size_t envStart = f->GetCurrentLine();
      if (f->GetLineCount() < envStart+(2*envLen)+1)
         return false;

      f->GoToLine(envStart+(2*envLen));
      if (f->GetNextLine() != wxT("EnvEnd"))
         return false;
      if (f->GetNextLine() != wxT("numSamples"))
         return false;

      wxString numSamples = f->GetNextLine();

      if (f->GetNextLine() != wxT("rate"))
         return false;
      fprintf(outf, " rate='%s'", (const char *)f->GetNextLine().mb_str());
      fprintf(outf, ">\n");

      if (envLen > 0) {
         fprintf(outf, "\t\t<envelope numpoints='%d'>\n", (int)envLen);

         long i;
         for(i=0; i<envLen; i++) {
            fprintf(outf, "\t\t\t<controlpoint t='%s' val='%s'/>\n",
                    (const char *)f->GetLine(envStart + 2*i + 1).mb_str(),
                    (const char *)f->GetLine(envStart + 2*i + 2).mb_str());
         }

         fprintf(outf, "\t\t</envelope>\n");
      }

      if (f->GetNextLine() != wxT("numBlocks"))
         return false;
      long numBlocks;
      line = f->GetNextLine();
      line.ToLong(&numBlocks);

      if (numBlocks < 0 || numBlocks > 131072)
         return false;

      fprintf(outf, "\t\t<sequence maxsamples='524288'");
      fprintf(outf, " sampleformat='131073' ");
      fprintf(outf, " numsamples='%s'>\n", (const char *)numSamples.mb_str());

      long b;
      for(b=0; b<numBlocks; b++) {
         wxString start;
         wxString len;
         wxString name;

         if (f->GetNextLine() != wxT("Block start"))
            return false;
         start = f->GetNextLine();
         if (f->GetNextLine() != wxT("Block len"))
            return false;
         len = f->GetNextLine(); 
         if (f->GetNextLine() != wxT("Block info"))
            return false;
         name = f->GetNextLine();

         fprintf(outf, "\t\t\t<waveblock start='%s'>\n",
                 (const char *)start.mb_str());

         if (name == wxT("Alias")) {
            wxString aliasPath = f->GetNextLine();
            wxString localLen = f->GetNextLine();
            wxString aliasStart = f->GetNextLine();
            wxString aliasLen = f->GetNextLine();
            wxString aliasChannel = f->GetNextLine();
            wxString localName = f->GetNextLine();

            fprintf(outf, "\t\t\t\t<legacyblockfile");
            fprintf(outf, " name='%s'", (const char *)localName.mb_str());
            fprintf(outf, " alias='1'");
            fprintf(outf, " aliaspath='%s'", (const char *)aliasPath.mb_str());
            fprintf(outf, " aliasstart='%s'", (const char *)aliasStart.mb_str());
            fprintf(outf, " aliaslen='%s'", (const char *)aliasLen.mb_str());
            fprintf(outf, " aliaschannel='%s'", (const char *)aliasChannel.mb_str());
            fprintf(outf, " summarylen='%s'", (const char *)localLen.mb_str());
            fprintf(outf, " norms='1'");
            fprintf(outf, " />\n");
         }
         else {
            fprintf(outf, "\t\t\t\t<legacyblockfile");
            fprintf(outf, " name='%s'", (const char *)name.mb_str());
            fprintf(outf, " len='%s'", (const char *)len.mb_str());
            fprintf(outf, " summarylen='8244'");
            fprintf(outf, " norms='1'");
            fprintf(outf, " />\n");
         }

         fprintf(outf, "\t\t\t</waveblock>\n");
      }

      fprintf(outf, "\t\t</sequence>\n");
      fprintf(outf, "\t</wavetrack>\n");
      
      return true;
   }
   else if (kind == wxT("LabelTrack")) {
      line = f->GetNextLine();
      if (line != wxT("NumMLabels"))
         return false;

      long numLabels, l;

      line = f->GetNextLine();
      line.ToLong(&numLabels);
      if (numLabels < 0 || numLabels > 1000000)
         return false;

      fprintf(outf, "\t<labeltrack name='Labels' numlabels='%ld'>\n",
              numLabels);

      for(l=0; l<numLabels; l++) {
         wxString t, title;

         t = f->GetNextLine();
         title = f->GetNextLine();

         fprintf(outf, "\t\t<label t='%s' title='%s' />\n",
                 (const char *)t.mb_str(), (const char *)title.mb_str());
      }

      fprintf(outf, "\t</labeltrack>\n");

      line = f->GetNextLine();
      if (line != wxT("MLabelsEnd"))
         return false;

      return true;
   }
   else if (kind == wxT("NoteTrack")) {
      // Just skip over it - they didn't even work in version 1.0!

      do {
         line = f->GetNextLine();
         if (line == wxT("WaveTrack") ||
             line == wxT("NoteTrack") ||
             line == wxT("LabelTrack") |\
             line == wxT("EndTracks")) {
            f->GoToLine(f->GetCurrentLine()-1);
            return true;
         }
      } while (f->GetCurrentLine() < f->GetLineCount());

      return false;
   }
   else
      return false;
}

bool ConvertLegacyProjectFile(wxFileName filename)
{
   wxTextFile f;
   FILE *outf;
   int index = 0;
   wxString backupName;

   do {
      index++;
      fflush(stdout);
      backupName = filename.GetPath() + wxFILE_SEP_PATH + filename.GetName() +
         wxT("_bak") + wxString::Format(wxT("%d"), index) + wxT(".") + filename.GetExt();
   } while(::wxFileExists(FILENAME(backupName)));

   // This will move the original file out of the way, but 
   // move it back if we exit from this function early.
   AutoRollbackRenamer renamer(filename.GetFullPath(), backupName);
   if (!renamer.RenameSucceeded())
      return false;

   f.Open(FILENAME(backupName));
   if (!f.IsOpened())
      return false;

   wxString name = filename.GetFullPath();

   wxFFile out_wxFFile(FILENAME(name).c_str(), wxT("wb"));
   if (!out_wxFFile.IsOpened())
      return false;
   outf = out_wxFFile.fp();
   fprintf(outf, "<?xml version='1.0'?>\n");

   renamer.SetNewFile(outf);

   wxString label;
   wxString value;

   if (f.GetFirstLine() != wxT("AudacityProject"))
      return false;
   if (f.GetNextLine() != wxT("Version"))
      return false;
   if (f.GetNextLine() != wxT("0.95"))
      return false;
   if (f.GetNextLine() != wxT("projName"))
      return false;

   fprintf(outf, "<audacityproject projname='%s'",
           (const char *)f.GetNextLine().mb_str());
   fprintf(outf, " version='1.1.0' audacityversion='%s'",
           AUDACITY_VERSION_STRING);
   label = f.GetNextLine();
   while (label != wxT("BeginTracks")) {
      value = f.GetNextLine();
      fprintf(outf, " %s='%s'", (const char *)label.mb_str(), (const char *)value.mb_str());
      label = f.GetNextLine();
   }
   fprintf(outf, ">\n");

   label = f.GetNextLine();
   while (label != wxT("EndTracks")) {
      bool success = ConvertLegacyTrack(&f, outf);
      if (!success)
         return false;
      label = f.GetNextLine();
   }

   fprintf(outf, "</audacityproject>\n");

   renamer.Finished();

   ::wxMessageBox(wxString::Format(_("Converted a 1.0 project file to the new format.\nThe old file has been saved as '%s'"), backupName.c_str()),
                  _("Opening Audacity Project"));

   return true;
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
// arch-tag: b15960fe-5b13-4cda-b2b1-da4dd59d1cd4

