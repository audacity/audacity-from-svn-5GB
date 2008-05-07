
//#define UNICODE
//#define _UNICODE

#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers
#include <windows.h>
#include <stdio.h>
#include <tchar.h>


const int nBuff = 1024;

extern "C" int DoSrv( char * pIn );
extern "C" int DoSrvMore( char * pOut, int nMax );

void PipeServer()
{
   HANDLE hPipeToSrv;
   HANDLE hPipeFromSrv;

   LPTSTR pipeNameToSrv= "\\\\.\\pipe\\ToSrvPipe";
   hPipeToSrv = CreateNamedPipe( 
      pipeNameToSrv ,
      PIPE_ACCESS_DUPLEX,
      PIPE_TYPE_MESSAGE | PIPE_READMODE_MESSAGE | PIPE_WAIT ,//| PIPE_REJECT_REMOTE_CLIENTS,
      PIPE_UNLIMITED_INSTANCES,
      nBuff,
      nBuff,
      50,//Timeout - always send straight away.
      NULL);
   if( hPipeToSrv == INVALID_HANDLE_VALUE)
      return;

   LPTSTR pipeNameFromSrv= "\\\\.\\pipe\\FromSrvPipe";
   hPipeFromSrv = CreateNamedPipe( 
      pipeNameFromSrv ,
      PIPE_ACCESS_DUPLEX,
      PIPE_TYPE_MESSAGE | PIPE_READMODE_MESSAGE | PIPE_WAIT ,//| PIPE_REJECT_REMOTE_CLIENTS,
      PIPE_UNLIMITED_INSTANCES,
      nBuff,
      nBuff,
      50,//Timeout - always send straight away.
      NULL);
   if( hPipeFromSrv == INVALID_HANDLE_VALUE)
      return;


   BOOL bConnected;
   BOOL bSuccess;
   DWORD cbBytesRead;
   DWORD cbBytesWritten;
   CHAR chRequest[ nBuff ];
   CHAR chResponse[ nBuff ];

   int jj=0;

   for(;;)
   {
      printf( "Obtaining pipe\n" );
      bConnected = ConnectNamedPipe(hPipeToSrv, NULL) ? 
         TRUE : (GetLastError()==ERROR_PIPE_CONNECTED );
      printf( "Obtained to-srv %i\n", bConnected );
      bConnected = ConnectNamedPipe(hPipeFromSrv, NULL) ? 
         TRUE : (GetLastError()==ERROR_PIPE_CONNECTED );
      printf( "Obtained from-srv %i\n", bConnected );
      if( bConnected )
      {
         for(;;)
         {
            printf( "About to read\n" );
            bSuccess = ReadFile( hPipeToSrv, chRequest, nBuff, &cbBytesRead, NULL);

            chRequest[ cbBytesRead] = '\0'; 

            if( !bSuccess || cbBytesRead==0 )
               break;

            printf( "Rxd %s\n", chRequest );

            DoSrv( chRequest );
            jj++;
            while( true )
            {
               int nWritten = DoSrvMore( chResponse, nBuff );
               if( nWritten <= 1 )
                  break;
               WriteFile( hPipeFromSrv, chResponse, nWritten-1, &cbBytesWritten, NULL);
            }
            //FlushFileBuffers( hPipeFromSrv );
         }
         FlushFileBuffers( hPipeToSrv );
         DisconnectNamedPipe( hPipeToSrv );
         FlushFileBuffers( hPipeFromSrv );
         DisconnectNamedPipe( hPipeFromSrv );
         break;
      }
      else
      {
         CloseHandle( hPipeToSrv );
         CloseHandle( hPipeFromSrv );
      }
   }
   CloseHandle( hPipeToSrv );
   CloseHandle( hPipeFromSrv );
}
