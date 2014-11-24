#!/usr/bin/perl

use warnings;
use strict;

my $prog_dir = "../../build";
my $sample_dir = "../samples";
my $out_dir = "benchs";
my $m2s_cmd = "m2s --x86-config ../base_proc.conf --net-config " .
              "../base_net.conf --x86-sim detailed --mem-config ../base_mem.conf ";
`mkdir -p $out_dir`;

foreach my $sample_file (`ls $sample_dir`) {
    chomp $sample_file;
    foreach my $prog (`ls $prog_dir`) {
        chomp $prog;
        my $out_filename = $out_dir . "/" . $prog . "-base" .
                           "-" . $sample_file . ".sim";
        print "processando $out_filename\n";
        my $full_cmd = $m2s_cmd .
                       " ${prog_dir}/$prog ${sample_dir}/$sample_file " .
                       "1>/dev/null 2>" . $out_filename;
#            print $full_cmd . "\n";
        `$full_cmd`;
    }
}


