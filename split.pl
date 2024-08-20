#!/usr/bin/perl
use strict;
use warnings;

while (my $line = <>) {
    chomp $line;

    if ($line =~ /\[\];$/) {
        next;
    }
    if ($line =~ /\{\};$/) {
        next;
    }

    if ($line =~ /(loc_ghost|loc_end|loc_start|loc_stack|_loc|loc2)/) {
        next;
    }
    my @fields = split(/\./, $line);
    my $last_four = join(".", @fields[-2..-1]);
    print "$last_four\n";
}


