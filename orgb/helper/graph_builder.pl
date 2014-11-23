#!/usr/bin/perl

use warnings;
use strict;

my $V = 1000;

sub rrand {
    my ($val) = @_;
    return int (rand $val);
}

foreach my $s (1..$V - 1) {
    for (my $t = 0; $t < rrand($V / 4); $t++) {
        print "addEdge(graph, $s, ", rrand($V), ", ", rrand(100), ");\n";
    }
}


