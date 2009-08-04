#!/usr/bin/env perl

# Test script for communicating with audacity via mod-script-pipe
# Audacity should be running first, with the scripting plugin loaded.

$screenshotDir = "/home/dan/Temp";

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
   print "Done. Press return to end.";
   <>;
   close TO_SRV;
   close FROM_SRV;
}

sub startTiming{
   $t0 = [gettimeofday];
}

sub stopTiming{
   my $elapsed = tv_interval ( $t0, [gettimeofday] );
   print "[Total time for command: $elapsed seconds.]\n";
}

sub sendCommand{
   my $command = shift;
   if ($^O eq 'MSWin32') {
      print TO_SRV "$command\r\n\0";
   } else {
      # Don't explicitly send \0 on Linux or reads after the first one fail...
      print TO_SRV "$command\n";
   }
   print "[$command]\n";
}

sub doCommand{
   startTiming();
   sendCommand(shift);

   while(my $ttt = <FROM_SRV>) {
      chomp($ttt);
      last if ($ttt eq '');
      print "$ttt\n";
   }

   stopTiming();
   print "\n";
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

sub parameterTest{
   # Set a non-existent parameter
   doCommand("GetAllMenuCommands: blah=2");
   # Parameter with no '='
   doCommand("MenuCommand: CommandName");
}

# See what happens when commands have extra spaces in various places
sub extraSpaces{
   doCommand("Help: CommandName=Help");
   doCommand("Help : CommandName=Help");
   doCommand("Help: CommandName =Help");
   doCommand("Help: CommandName= Help");
   doCommand("Help: CommandName=Help ");
}

# Test whether we can fall back to batch mode
sub batchFallback{
   doCommand( "Echo: Delay=1.0 Decay=0.5" );
}

# Send lots of commands quickly
sub stressTest{
   my $n = 0;
   while($n < 600){
      getMenuCommands();
      ++$n;
   }
}

# Get help on a command
sub help{
   my $cmdName = shift;
   doCommand("Help: CommandName=$cmdName");
}

# From script, this works like an 'echo'
sub message{
   my $msg = shift;
   doCommand("Message: MessageString=$msg");
}

sub compareAudio{
   my $threshold = shift;
   doCommand("CompareAudio: Threshold=$threshold");
}
sub deleteAll{
   doCommand("Select: Mode=All");
   menuCommand("RemoveTracks");
}

sub compareTest{
   deleteAll();

   menuCommand("NewAudioTrack");
   doCommand("Chirp:");
   menuCommand("NewAudioTrack");
   doCommand("Chirp:");

   $j = 0;
   while($j < 3)
   {
      $i = 0;
      while($i < 6){
         doCommand("Select: Mode=Range StartTime=5.0 EndTime=8.0 FirstTrack=0 LastTrack=0");

         doCommand("Amplify: Ratio=0.95");
         doCommand("Select: Mode=All");
         compareAudio(0.4 - 0.1*$j);
         ++$i;
      }
      ++$j;
   }
}

sub getTrackInfo{
   $trackID = shift;
   sendCommand("GetTrackInfo: Type=Name TrackIndex=0");
   my $name = <FROM_SRV>;
   chomp($name);
   <FROM_SRV>;
   <FROM_SRV>;
   sendCommand("GetTrackInfo: Type=StartTime TrackIndex=0");
   my $startTime = <FROM_SRV>;
   chomp($startTime);
   <FROM_SRV>;
   <FROM_SRV>;
   sendCommand("GetTrackInfo: Type=EndTime TrackIndex=0");
   my $endTime = <FROM_SRV>;
   chomp($endTime);
   <FROM_SRV>;
   <FROM_SRV>;

   print "     Name: $name\n";
   print "StartTime: $startTime\n";
   print "  EndTime: $endTime\n";
}

# Assortment of different tests
sub fullTest{
   syntaxError();
   extraSpaces();
   menuCommand("NewStereoTrack");
   #screenshot($screenshotDir, "window", "None"); # (Slow)
   doCommand("Select: Mode=All");
   getMenuCommands();
   menuCommand("NewAudioTrack");
   batchFallback();
   help("Screenshot");
   message("Hello!");
   getTrackInfo(0);
   deleteAll();
}

# Send some test commands
# (Delay between sends so it's clearer what's going on)

startUp();
fullTest();

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
