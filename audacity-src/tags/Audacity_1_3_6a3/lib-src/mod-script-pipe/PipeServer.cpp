
#if defined(WIN32)

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

   LPTSTR pipeNameToSrv= _T("\\\\.\\pipe\\ToSrvPipe");

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

   LPTSTR pipeNameFromSrv= __T("\\\\.\\pipe\\FromSrvPipe");

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

#else

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

const char fifotmpl[] = "/tmp/audacity_script_pipe.%d";

const int nBuff = 1024;

extern "C" int DoSrv( char * pIn );
extern "C" int DoSrvMore( char * pOut, int nMax );

void PipeServer()
{
   FILE *fifo = NULL;
   int rc;
   char buf[nBuff];
   char fifoname[nBuff];
   
   sprintf(fifoname, fifotmpl, getuid());

   rc = mkfifo(fifoname, S_IRWXU);
   if (rc < 0)
   {
      perror("Unable to create fifo");
      printf("Ignoring...");
//      return;
   }

   fifo = fopen(fifoname, "rw");
   if (fifo == NULL)
   {
      perror("Unable to open fifo");
      return;
   }

   while (true)
   {
      int len;

      if (fgets(buf, sizeof(buf), fifo) == NULL)
      {
         perror("Read failed on fifo");
         break;
      }

      len = strlen(buf);
      if (len <= 1)
      {
         continue;
      }

      buf[len - 1] = '\0';

      DoSrv(buf);

      while (true)
      {
         len = DoSrvMore(buf, nBuff);
         if (len <= 1)
         {
            break;
         }
         printf(buf);

         fwrite(buf, 1, len, fifo);
      }

      // Here until I figure out why the second read fails.
      break;
   }

   if (fifo != NULL)
   {
      fclose(fifo);
   }

   unlink(fifoname);
}
#endif
