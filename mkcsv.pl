opendir(my $dir, "drivers") || die "can't opendir drivers $!";
@drivers = readdir($dir);
closedir $dir;
$| = 1;

$ichunk = 1;
open ($out, ">chunk_".$ichunk.".csv") || die "can't open output $!";
print $out "driver,tripnum,x,y\n";

$dbg = 0;
$idrv = 1;

foreach $d (@drivers) {
#print STDOUT $d,"\n";
   next if $d =~ /^\./;
   print STDOUT $d,"... ";
   for ($i=1; $i<=200; $i++) {
      #print STDOUT ".";
      $fname = sprintf( "drivers/%s/%d.csv" , $d, $i);
	  open( $fh, $fname) || die "can't open fh $!";	 
	  
	  $skip = <$fh>; #header
	  while( $row = <$fh> ) { print $out "$d,$i,$row"; }
	  close( $fh);
   }	  
   #last if (++$dbg >5);
   if (++$idrv%200 == 0) {
      close $out;
	  $ichunk++;
	  open($out, ">chunk_".$ichunk.".csv") || die "can't open next output $!";
      print $out "driver,tripnum,x,y\n";
	}
}
close $out;