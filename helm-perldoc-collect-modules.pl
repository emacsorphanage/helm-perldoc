#!/usr/bin/env perl
use strict;
use warnings;

use File::Find;
use File::Spec;

my %modules = ();
my %inc = map { $_ => $_ } map { File::Spec->canonpath($_) } grep { -d $_ && $_ ne '.' } @INC;

for my $path (keys %inc) {
    find(+{
        wanted => sub {
            my $name = $File::Find::fullname;

            if (-d $name) {
                if (exists $inc{$name} && $inc{$name} ne $path) {
                    $File::Find::prune = 1;
                }
                return;
            }
            return unless $name =~ s/\.p(?:m|od)\z//;

	    $name = File::Spec->abs2rel($name, $path);
            my $module = join('::', File::Spec->splitdir($name));
            $modules{$module}++;
        },
        follow => 1,
        follow_skip => 2,
    }, $path);
}
print "$_\n" for sort keys %modules;
