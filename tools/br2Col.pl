#!/usr/bin/perl -w
use strict;
$|=1;

################################################################
# br2Col.pl
#  09/27/10, Michael Connor
#
# br2HMMCol.pl
#  09/07/09, Michael Connor
#
# Given ohmm model and br POS file, create appropriate column file
# with columns for word | POS | hmm predicted state.
#

die "Usage: br2Col.pl <br file> [lc? 0]\n"
    if (@ARGV < 1);

my $brFile = shift;
open POS, $brFile or die "Can't open $brFile: $!\n";

my $useLC = 0;
$useLC = shift if (@ARGV);

while (<POS>) {
    chomp;
    my @line = split /[\(\s\)]+/, $_;
    #print join(",",@line),"\n";
    #In POS bracketed file, every other token should be the word
    for (my $i = 2; $i<@line; $i+=2) {
	my $t = $line[$i-1];
	my $w = $line[$i];
	
	$w = lc $w if ($useLC);
    
	#$w =~ s/^[+\-]?\d+[\d.,\-+\\\/]*$/NUMBERSYMBOL/;

	print "$t\t$w\n";
    }
    print "\n";
}
close POS;
