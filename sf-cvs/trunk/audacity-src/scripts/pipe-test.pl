#!/usr/bin/env perl

# Test script for communicating with audacity via mod-script-pipe
# Audacity should be running first, with the scripting plugin loaded.

use Time::HiRes qw( gettimeofday tv_interval );

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
   print "Done: press return to end.";
   <>;
   close TO_SRV;
   close FROM_SRV;
}

sub startTiming{
   $t0 = [gettimeofday];
}

sub stopTiming{
   my $elapsed = tv_interval ( $t0, [gettimeofday] );
   print "Total time for command: $elapsed seconds.\n";
}

sub doCommand{
   startTiming();
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

   stopTiming();
   return $ttt;
}

# Send a screenshot command
sub screenshot{
   my $filePath    = shift;
   my $captureMode = shift;
   my $background  = shift;
   doCommand("Screenshot: FilePath=$filePath CaptureMode=$captureMode Background=$background");
}

# Send a menu command
sub menuCommand{
   my $commandName = shift;
   doCommand("MenuCommand: CommandName=$commandName");
}

# Send a command which requests a list of all available menu commands
sub getMenuCommands{
   doCommand("GetAllMenuCommands:");
}

# Send a string that should be a syntax error
sub syntaxError{
   doCommand("CommandWithNoColon foo bar");
}

# Send a command that doesn't exist
sub noSuchCommand{
   doCommand("NoSuchCommand: myParam=3");
}

# Send some test commands
# (Delay between sends so it's clearer what's going on)
$delay = 1.0;

startUp();

$screenshotDir = "/home/dan/Temp";

getMenuCommands();
menuCommand("Play");

#syntaxError();
#noSuchCommand();

#screenshot($screenshotDir, "window", "None");
#sleep($delay);
#doCommand( "BatchCommand: CommandName=Amplify ParamString=Ratio=0.1" );
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

#print "> ";
#while($input = <>) {
#   chomp($input);
#   print "> ";
#}

finish();
