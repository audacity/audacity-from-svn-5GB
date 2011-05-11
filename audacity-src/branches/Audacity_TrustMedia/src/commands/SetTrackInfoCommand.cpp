/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file SetTrackInfoCommand.cpp
\brief Definitions for SetTrackInfoCommand and SetTrackInfoCommandType classes

\class SetTrackInfoCommand
\brief Command that sets track information (currently name only)

*//*******************************************************************/

#include "SetTrackInfoCommand.h"
#include "../Project.h"
#include "../Track.h"

// The following parameters have a boolean value, indicated by the kSettingStr
#define kSettingStr "Setting"


wxString SetTrackInfoCommandType::BuildName()
{
   return wxT("SetTrackInfo");
}

void SetTrackInfoCommandType::BuildSignature(CommandSignature &signature)
{
   IntValidator *trackIndexValidator = new IntValidator();
   signature.AddParameter(wxT("TrackIndex"), 0, trackIndexValidator);

   OptionValidator *infoTypeValidator = new OptionValidator();
   infoTypeValidator->AddOption(wxT("Name"));
   infoTypeValidator->AddOption(wxT("Selected"));
   infoTypeValidator->AddOption(wxT("Solo"));
   infoTypeValidator->AddOption(wxT("Mute"));
   signature.AddParameter(wxT("Type"), wxT("Name"), infoTypeValidator);
   Validator *nameValidator = new Validator();
   signature.AddParameter(wxT("Name"), wxT("Unnamed"), nameValidator);
   BoolValidator *selectValidator = new BoolValidator();
   signature.AddParameter(wxT(kSettingStr), wxT("false"), selectValidator );  // gets a single bool track setting
}

Command *SetTrackInfoCommandType::Create(CommandOutputTarget *target)
{
   return new SetTrackInfoCommand(*this, target);
}

bool SetTrackInfoCommand::Apply(CommandExecutionContext context)
{
   wxString mode = GetString(wxT("Type"));

   long trackIndex = GetLong(wxT("TrackIndex"));

   // (Note: track selection ought to be somewhere else)
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
      wxString name = GetString(wxT("Name"));
      t->SetName(name);
   }
   else if (mode.IsSameAs(wxT("Select")))
   {
      t->SetSelected(GetBool(wxT(kSettingStr)));
   }
   else if (mode.IsSameAs(wxT("Solo")))     //  See also the TrackSolo/TrackMute commands
   {
      t->SetSolo(GetBool(wxT(kSettingStr)));
   }
   else if (mode.IsSameAs(wxT("Mute")))
   {
      t->SetMute(GetBool(wxT(kSettingStr)));
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
