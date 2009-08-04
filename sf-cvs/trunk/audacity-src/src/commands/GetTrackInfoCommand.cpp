/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file GetTrackInfoCommand.cpp
\brief Definitions for GetTrackInfoCommand and GetTrackInfoCommandType classes

\class GetTrackInfoCommand
\brief Command that returns requested track information

*//*******************************************************************/

#include "GetTrackInfoCommand.h"
#include "../Project.h"
#include "../Track.h"

wxString GetTrackInfoCommandType::BuildName()
{
   return wxT("GetTrackInfo");
}

void GetTrackInfoCommandType::BuildSignature(CommandSignature &signature)
{
   IntValidator *trackIndexValidator = new IntValidator();
   signature.AddParameter(wxT("TrackIndex"), 0, trackIndexValidator);

   OptionValidator *infoTypeValidator = new OptionValidator();
   infoTypeValidator->AddOption(wxT("Name"));
   infoTypeValidator->AddOption(wxT("StartTime"));
   infoTypeValidator->AddOption(wxT("EndTime"));
   signature.AddParameter(wxT("Type"), wxT("Name"), infoTypeValidator);
}

Command *GetTrackInfoCommandType::Create(CommandOutputTarget *target)
{
   return new GetTrackInfoCommand(*this, target);
}

bool GetTrackInfoCommand::Apply(CommandExecutionContext context)
{
   wxString mode = GetString(wxT("Type"));

   long trackIndex = GetLong(wxT("TrackIndex"));

   // (Note: this ought to be somewhere else)
   long i = 0;
   TrackListIterator iter(context.proj->GetTracks());
   Track *t = iter.First();
   while (t && i != trackIndex)
   {
      t = iter.Next();
      ++i;
   }
   if (i != trackIndex || !t)
   {
      Error(wxT("TrackIndex was invalid."));
      return false;
   }

   if (mode.IsSameAs(wxT("Name")))
   {
      Status(t->GetName());
   }
   else if (mode.IsSameAs(wxT("StartTime")))
   {
      Status(wxString::Format(wxT("%f"), t->GetStartTime()));
   }
   else if (mode.IsSameAs(wxT("EndTime")))
   {
      Status(wxString::Format(wxT("%f"), t->GetEndTime()));
   }
   else
   {
      Error(wxT("Invalid info type!"));
      return false;
   }
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
// arch-tag: TBD
