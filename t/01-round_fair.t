#!perl
use warnings;
use strict;
use Test::More tests => 6;
use List::Util qw/sum min max/;

BEGIN { use_ok 'Math::Round::Fair', qw/round_fair/ }

ok $Math::Round::Fair::debug, 'assertions enabled for tests';

my $to_allocate = 7;
my @weights = (1) x 10;
my @alloc   = round_fair($to_allocate, @weights);

ok  @alloc == @weights,          'return count';
ok  sum(@alloc) == $to_allocate, 'full allocation';
ok  max(@alloc) == 1,            'max allocation';
ok  min(@alloc) == 0,            'min_allocation';
