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

# Send some test commands - commands currently allowed are precisely those
# supported by the Batch system, and with the same syntax.
# (Delay between sends so it's clearer what's going on)
startUp();
sleep(1.0);
doCommand( 'Amplify: Ratio=0.1' );
sleep(1.0);
doCommand( 'Echo: Delay=1.0 Decay=0.5' );
sleep(1.0);
doCommand( 'Amplify: Ratio=2.0' );
sleep(1.0);
doCommand( 'Echo: Delay=1.0 Decay=0.5' );
sleep(1.0);
doCommand( 'ExportMp3' );
sleep(1.0);
doCommand( '<Not a valid command>');
finish();
