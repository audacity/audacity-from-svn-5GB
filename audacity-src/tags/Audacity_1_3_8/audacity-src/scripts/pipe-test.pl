#!/usr/bin/env perl

# Test script for communicating with audacity via mod-script-pipe
# Audacity should be running first, with the scripting plugin loaded.

sub startUp{
   # TODO: Maybe get the pipe names from audacity?
   if ($^O eq 'MSWin32') {
      $Name = 'Srv';
      $ToSrvName = '\\\\.\\pipe\\To'.$Name.'Pipe';
      $FromSrvName = '\\\\.\\pipe\\From'.$Name.'Pipe';
   } elsif ($^O eq 'linux') {
      $UID = $<;
      $ToSrvName = '/tmp/audacity_script_pipe.to.'.$UID;
      $FromSrvName = '/tmp/audacity_script_pipe.from.'.$UID;
   } elsif ($^O eq 'darwin') {
      $UID = $<;
      $ToSrvName = '/tmp/audacity_script_pipe.to.'.$UID;
      $FromSrvName = '/tmp/audacity_script_pipe.from.'.$UID;      
   }

   open( TO_SRV, "+<$ToSrvName" )
      or die "Could not open $ToSrvName";
   open( FROM_SRV, "+<$FromSrvName" )
      or die "Could not open $FromSrvName";

   # The next 'magic incantation' causes TO_SRV to be flushed every time we
   # write something to it.
   select((select(TO_SRV),$|=1)[0]);
}

sub finish{
   close TO_SRV;
   close FROM_SRV;
   print "Done: press return to end.";
   <>
}

sub doCommand{
   my $command = shift;
   if ($^O eq 'MSWin32') {
      print TO_SRV "$command\r\n\0";
   } else {
      # Don't explicitly send \0 on Linux or reads after the first one fail...
      print TO_SRV "$command\n";
   }
   print "Sent: $command\n";

   while(my $ttt = <FROM_SRV>) {
      chomp($ttt);
      last if ($ttt eq '');
      print "Recd:'$ttt'\n";
   }

   return $ttt;
}

# Send some test commands
# (Delay between sends so it's clearer what's going on)
$delay = 1.0;

# Fill in the path where you want the screenshots to appear
$filePath = "/home/dan/Temp";

startUp();
sleep($delay);
doCommand( "screenshot: FilePath=$filePath CaptureMode=window Background=None" );
sleep($delay);
doCommand( "screenshot: FilePath=$filePath CaptureMode=fullwindow Background=Blue" );
sleep($delay);
doCommand( "screenshot: FilePath=$filePath CaptureMode=windowplus Background=White" );
sleep($delay);
doCommand( "screenshot: FilePath=$filePath CaptureMode=fullscreen Background=None" );
sleep($delay);
doCommand( "screenshot: FilePath=$filePath CaptureMode=mixer Background=None" );
sleep($delay);
doCommand( "screenshot: FilePath=$filePath CaptureMode=device Background=None" );
sleep($delay);
doCommand( "screenshot: FilePath=$filePath CaptureMode=ruler Background=None" );
sleep($delay);
doCommand( "screenshot: FilePath=$filePath CaptureMode=firsttrack Background=None" );
#sleep($delay);
#doCommand( "Amplify: Ratio=0.1" );
#sleep($delay);
#doCommand( "Echo: Delay=1.0 Decay=0.5" );
#sleep($delay);
#doCommand( "Amplify: Ratio=2.0" );
#sleep($delay);
#doCommand( "Echo: Delay=1.0 Decay=0.5" );
#sleep($delay);
#doCommand( "ExportMp3" );
#sleep($delay);
#doCommand( "<Not a valid command>");
finish();
