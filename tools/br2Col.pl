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
# Given br POS file, create appropriate column file
# with columns for POS | word | features
#
# And for features we'll do:
#  contains digit
#  contains punctuation
#  contains any capitalization
#  suffixes of length 3,2,1


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
	
	my $lcw = lc $w if ($useLC);
    
	#$w =~ s/^[+\-]?\d+[\d.,\-+\\\/]*$/NUMBERSYMBOL/;

	print "$t\t",($useLC ? $lcw : $w);
	print "\t",($w =~ /[[:digit:]]/ ? 'DIGIT' : 'NODIGIT');
	print "\t",($w =~ /[[:punct:]]/ ? 'PUNCT' : 'NOPUNCT');
#	print "\t",($w =~ /[[:upper:]]/ ? 'CAP' : 'NOCAP');
	print "\t",($w =~ /^[[:upper:]]/ ? 'CAP' : 'NOCAP');
#	print "\t",suffixes($w);
	print "\t",suffix($w);
	print "\n";
    }
    print "\n";
}
close POS;

sub suffixes {
    my $w = shift;
    my @a = ();
    push @a, substr($w,-3) if (length($w) > 4);
    push @a, substr($w,-2) if (length($w) > 3);
    push @a, substr($w,-1) if (length($w) > 2);

    return join(',', @a);
}

sub suffix {
    my $w = shift;
    my $n = (length($w) > 3 ? 3 : length($w));
    return substr($w, -$n);
}
