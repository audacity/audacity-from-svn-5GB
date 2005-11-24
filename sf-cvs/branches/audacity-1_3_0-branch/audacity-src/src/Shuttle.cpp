/**********************************************************************

  Audacity: A Digital Audio Editor

  Shuttle.cpp

  Dominic Mazzoni
  James Crook

  Shuttle provides a base class for transfering parameter data into and
  out of clasess into some other structure.  This is a common 
  requirement and is needed for:
    - Prefs data
    - Command line parameter data
    - Project data in XML

  The 'Master' is the string side of the shuttle transfer, the 'Client'
  is the binary data side of the transfer.

**********************************************************************/
#include <wx/defs.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/listctrl.h>
#include <wx/radiobut.h>
#include <wx/button.h>


#include "Audacity.h"
#include "Project.h"
#include "Shuttle.h"
#include "commands/CommandManager.h"
#include "effects/Effect.h"

const int Enums::NumDbChoices = 14;

const wxString Enums::DbChoices[] = 
   {wxT("-20db"), wxT("-25db"), wxT("-30db"), 
    wxT("-35db"), wxT("-40db"), wxT("-45db"), 
    wxT("-50db"), wxT("-55db"), wxT("-60db"),
    wxT("-65db"), wxT("-70db"), wxT("-75db"), 
    wxT("-80db"), wxT("Off-Skip")};

const double Enums::Db2Signal[] = 
//     -20db    -25db    -30db    -35db    -40db    -45db    -50db    -55db    -60db    -65db     -70db     -75db     -80db    Off
   { 0.10000, 0.05620, 0.03160, 0.01780, 0.01000, 0.00562, 0.00316, 0.00178, 0.00100, 0.000562, 0.000316, 0.000178, 0.0001000, 0.0 };


const wxString * Enums::GetDbChoices()
{
   return DbChoices;
}


Shuttle::Shuttle()
{
}

bool Shuttle::TransferBool( const wxString & Name, bool & bValue, const bool & bDefault )
{
   if( mbStoreInClient )
   {
      bValue = bDefault;
      if( ExchangeWithMaster( Name ))
      {
         if( !mValueString.IsEmpty() )
            bValue = mValueString.GetChar(0) == wxT('y');
      }
   }  
   else
   {
      mValueString = (bValue==0) ? wxT("no"):wxT("yes");
      return ExchangeWithMaster( Name );
   }
   return true;
}

bool Shuttle::TransferFloat( const wxString & Name, float & fValue, const float &fDefault )
{
   if( mbStoreInClient )
   {
      fValue = fDefault;
      if( ExchangeWithMaster( Name ))
      {
         if( !mValueString.IsEmpty() )
            fValue = wxAtof( mValueString );
      }
   }  
   else
   {
      mValueString = wxString::Format(wxT("%f"),fValue);
      return ExchangeWithMaster( Name );
   }
   return true;
}

bool Shuttle::TransferDouble( const wxString & Name, double & dValue, const double &dDefault )
{
   if( mbStoreInClient )
   {
      dValue = dDefault;
      if( ExchangeWithMaster( Name ))
      {
         if( !mValueString.IsEmpty() )
            dValue = wxAtof( mValueString );
      }
   }  
   else
   {
      //%f is format string for double 
      mValueString = wxString::Format(wxT("%f"),dValue);
      return ExchangeWithMaster( Name );
   }
   return true;
}

bool Shuttle::TransferInt( const wxString & Name, int & iValue, const int & iDefault )
{
   if( mbStoreInClient )
   {
      iValue = iDefault;
      if( ExchangeWithMaster( Name ))
      {
         iValue = wxAtoi( mValueString );
      }
   }  
   else
   {
      mValueString = wxString::Format(wxT("%i"),iValue);
      return ExchangeWithMaster( Name );
   }
   return true;
}

bool Shuttle::TransferLongLong( const wxString & Name, wxLongLong_t & iValue, const wxLongLong_t & iDefault )
{
   if( mbStoreInClient )
   {
      iValue = iDefault;
      if( ExchangeWithMaster( Name ))
      {
         iValue = wxAtoi( mValueString );
      }
   }  
   else
   {
      //TODO: fixup for long long values.
      mValueString = wxString::Format(wxT("%d"),iValue);
      return ExchangeWithMaster( Name );
   }
   return true;
}


bool Shuttle::TransferEnum( const wxString & Name, int & iValue, 
      const int nChoices, const wxString * pFirstStr)
{
   if( mbStoreInClient )
   {
      iValue = 0;// default index if none other selected.
      if( ExchangeWithMaster( Name ))
      {
         int i;
         for(i=0;i<nChoices;i++)
         {
            if( mValueString.IsSameAs( pFirstStr[i] ))
            {
               iValue = i;
            }
         }
      }
   }  
   else
   {
      //TIDY-ME: Out of range configuration values are silently discarded...
      if( iValue > nChoices )
         iValue = 0;
      if( iValue < 0 )
         iValue = 0;
      mValueString = pFirstStr[iValue];
      return ExchangeWithMaster( Name );
   }
   return true;
}

bool Shuttle::ExchangeWithMaster(const wxString & Name)
{
   // ExchangeWithMaster() will usually be over-ridden
   // in derived classes.  We could have made it an
   // abstract function.
   wxASSERT( false );
   return true;
}

// This variant uses valuse of the form
// param1=value1 param2=value2 
bool ShuttleCli::ExchangeWithMaster(const wxString & Name)
{
   if( !mbStoreInClient )
   {
      mParams += wxT(" ");
      mParams +=Name;
      mParams += wxT("=");
      mParams +=mValueString;
   }
   else
   {
      int i;
      i=mParams.Find( wxT(" ")+Name+wxT("=") );
      if( i>=0 )
      {
         int j=i+2+Name.Length();
         i=j;
         while( j<(int)mParams.Length() && mParams.GetChar(j) != wxT(' ') )
            j++;
         mValueString = mParams.Mid(i,j-i);
         return true;
      }
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
