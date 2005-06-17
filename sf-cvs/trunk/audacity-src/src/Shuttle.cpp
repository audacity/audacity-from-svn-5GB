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
            bValue = (mValueString[0]=='y');
      }
   }  
   else
   {
      mValueString = (bValue==0) ? "no":"yes";
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
            fValue = atof( mValueString );
      }
   }  
   else
   {
      mValueString = wxString::Format("%f",fValue);
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
            dValue = atof( mValueString );
      }
   }  
   else
   {
      //%f is format string for double 
      mValueString = wxString::Format("%f",dValue);
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
         iValue = atoi( mValueString );
      }
   }  
   else
   {
      mValueString = wxString::Format("%i",iValue);
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
         iValue = atoi( mValueString );
      }
   }  
   else
   {
      //TODO: fixup for long long values.
      mValueString = wxString::Format("%d",iValue);
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
      mParams +=" ";
      mParams +=Name;
      mParams +="=";
      mParams +=mValueString;
   }
   else
   {
      int i;
      i=mParams.Find( " "+Name+"=" );
      if( i>=0 )
      {
         int j=i+2+Name.Length();
         i=j;
         while( j<(int)mParams.Length() && mParams[j] != ' ')
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
