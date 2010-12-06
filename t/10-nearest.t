#!perl -w

use strict;
$^W=1;
BEGIN { chdir "t" if -d "t"; }
use lib '../blib/arch', '../blib/lib';

use Test;
BEGIN { plan tests => 6, todo => [] }

use Math::Round::Fair qw(fair_round_adjacent);
$Math::Round::Fair::debug = 1;

srand(0);
my @result = fair_round_adjacent();
ok(@result==0);
@result = fair_round_adjacent(1.23, 4.56, 7.89);
ok(@result==3);
ok($result[0]==1 || $result[0]==2);
ok($result[1]==4 || $result[1]==5);
ok($result[2]==7 || $result[2]==8);
my $sum=0.0;
$sum += $_ for(@result);
ok($sum==13 || $sum==14);
