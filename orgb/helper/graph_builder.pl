#!/usr/bin/perl

use warnings;
use strict;

my $V = 1000;

sub my_rand {
    my ($val) = @_;
    return int (rand $val);
}

foreach my $s (1..$V) {
    foreach my $t (1..(rand int ($V / 4))) {
        print "addEdge(graph, $s, ", &my_rand($V), ", ", &my_rand(100), ");\n";
    }

}


