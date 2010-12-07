package Math::Round::Fair;
use warnings;
use strict;
use 5.005000;
use Carp;
use base qw/Exporter/;
use List::Util qw/shuffle sum min/;
use POSIX qw/floor ceil DBL_EPSILON/;

our $VERSION = '0.01-aj1';

our $debug = 0;

our @EXPORT_OK = qw/round_fair fair_round_adjacent/;

=head1 NAME

Math::Round::Fair - distribute rounding errors fairly

=head1 SYNOPSIS

  use Math::Round::Fair 'round_fair';

  my $cents = 7;
  my @weights = (1, 2, 3, 2, 1);
  my @allocation = round_fair($cents, @weights);

  print "@allocation\n";

  # output will be one of the following:
  # 0 1 3 2 1
  # 0 2 2 2 1
  # 0 2 3 1 1
  # 0 2 3 2 0
  # 1 1 2 2 1
  # 1 1 3 1 1
  # 1 1 3 2 0
  # 1 2 2 1 1
  # 1 2 2 2 0

  my @total;
  for ( 1..900 ) {
      @allocation = round_fair($cents, @weights);
      @total[$_] += @allocation[$_] for 0..$#allocation;
  }
  print "@total\n";

  # output will be *near* 700 1400 2100 1400 700, e.g.:
  # 698 1411 2096 1418 677


=head1 DESCRIPTION

This module provides a single, exportable function, C<round_fair>, which
allocates an integer value, fairly distributing rounding errors.

C<round_fair> rounds up, or down, randomly, where the probability of rounding
up is equal to the fraction to round.  For example, C<round_fair> will round
0.5 to 1.0 with a probability of 0.5.  It will round 0.3 to 1.0 3 out of 10
times and to zero 7 out of 10 times, on average.

Consider the problem of distributing one indivisible item, for example a penny,
across three evenly weighted accounts, A, B, and C.

Using a naive approach, none of the accounts will receive an allocation since
the allocated portion to each is 1/3 and 1/3 rounds to zero.  We are left with
1 unallocated item.

Another approach is to adjust the basis at each step.  We start with 1 item to
allocate to 3 accounts.  1/3 rounds to 0, so account A receives no allocation,
and we drop it from consideration.  Now, we have 2 accounts and one item to
allocate.  1/2 rounds to 1, so we allocate 1 item to account B.  Account C
gets no allocation since there is nothing left to allocate.

But what happens if we allocate one item to the same three accounts 10,000
times? Ideally, two accounts should end up with 3,333 items and one should end
up with 3,334 items.

Using the naive approach, all three accounts receive no allocation since at
each round the allocation is 1/3 which rounds to zero. Using the second method,
account A and account C will receive no allocation, and account B will receive
a total allocation of 10,000 items.  Account B always receives the benefit of
the rounding errors using the second method.

C<round_fair> uses an algorithm with randomness to ensure a fair distribution of
rounding errors.  In our example problem, we start with 1 item to allocate.  We
calculate account A's share, 1/3.  Since it is less than one item, we give it a
1/3 chance of rounding up (and, therefore, a 2/3 chance of rounding down).  It
wins the allocation 1/3 of the time.  2/3 of the time we continue to B. We
calculate B's allocation as 1/2 (since there are only 2 accounts remaining and
one item to allocate).  B rounds up 1/2 of 2/3 (or 1/3) of the time and down
1/2 of 2/3 (or 1/3) of the time.  If neither A nor B rounds up (which occurs
2/3 * 1/2, or 1/3 of the time), C's allocation is calculated as 1/1 since we
have one item to allocate and only one account to allocate it to.  So, 1/3 of
the time C receives the benefit of the rounding error.  We never end up with
any unallocated items.

This algorithm works for any number of weighted allocations.

=over 4

=item round_fair($value, @weights)

Returns a list of integer values that sum to C<$value> where each return value
is a portion of C<$value> allocated by the respective weights in C<@weights>.
The number of return values is equal to the number of elements in C<@weights>

C<$value> must be an integer.

=cut

sub round_fair {
    my $value = shift;

    croak "Value to be allocated must be an integer >= 0" unless int($value) == $value && $value >= 0;
    
    my $basis = 0;
    for my $w ( @_ ) {
        croak "Weights must be > 0" unless $w > 0;
        $basis += $w;
    }

    return ($value) if @_ == 1;
    return (0) x @_ if $value == 0;

    fair_round_adjacent(map { $value * $_ / $basis } @_)
}

=item fair_round_adjacent(@input_values)

Returns a list of integer values, each of which is one of which is numerically
adjacent to the corresponding element of @input_values, and whose total is
numerically adjacent to the total of @input_values.

The expected value of each output value is equal to the corresponding element
of @input_values (within a small error margin due to the limited machine
precision).

=cut

sub fair_round_adjacent {
    my @in = @_;

    # First, create the extra entry for the total, so that the sum of
    # the new array is zero.
    my $sum = sum 0, @in;
    push @in, -$sum;

    # Next, shuffle the order, so that the input order has no effect
    # on the randomness characteristics.
    my @order = shuffle($[..$#in);
    @in = map $in[$_], @order;

    my @out = fair_round_adjacent_1(@in);

    sum(0, @out) == 0 or die "internal error" if $debug;

    # put the output back into original order
    my @r;
    $r[$order[$_]] = $out[$_] for $[ .. $#order;

    pop @r; # Discard the entry for the total

    return @r;
}

# Like fair_round_adjacent, except that the inputs must sum to zero, and the
# input order may affect the variance and correlations, etc.
sub fair_round_adjacent_1 {
    my $eps1 = 4.0 * DBL_EPSILON() * (1 + @_);
    my $eps = $eps1;
    my @fp = map { my $ip = floor($_); $_ - $ip } @_;

    do { $_ < 0.0 and die "internal error" for(@fp)} if $debug;

    # TBD: Maybe accuracy or fairness can be improved by
    # re-adjusting after every iteration.  This would slow it
    # down significantly, though.
    _adjust_input(\@fp);

    my @out;
    INPUT: while(@fp) {
        $eps += $eps1;

        _check_invariants($eps, \@_, \@fp) if $debug;

        # Calculate the next output.  Discard the next input in the
        # process.
        my $p0 = shift @fp; # Probability of having to overpay
        my $r0 = rand()<$p0 ? 1 : 0; # 1 if selected to overpay; else 0
        push @out, floor(shift @_) + $r0;

        # Now adjust the remaining fractional parts.

        # $slack[i] = min( $p0 * $fp[i], (1-$p0) * (1-$fp[i]) ).
        my ($tslack, @slack) = _slack($p0, \@fp);

        # See bottom of file for proof of this property:
        die "internal error: $tslack $eps" unless $tslack + $eps >= $p0 * (1.0 - $p0);
        warn "TSLACK = $tslack\n" if $debug > 1;

        if ( $tslack > $eps1 ) {
            $eps += 128.0 * $eps1 * $eps / $tslack;
            # NOTE: The expected value of gain is
            #	$p0 * ($p0 - 1.0) /$tslack +
            #	(1.0 - $p0) * $p0 / $tslack = 0
            my $gain = do {
                if ( $r0 ) {
                    # Last guy overpaid, so the probabilities for
                    # subsequent payers drop.
                    ($p0 - 1.0) / $tslack;
                }
                else {
                    # Last guy underpaid, so the probabilities for
                    # subsequent payers rise.
                    $p0 / $tslack;
                }
            };

            # NOTE: The change in the sum of @fp due to this step
            # is $tslack * $gain, which is either $p0 or ($p0 - 1).
            # Either way, the sum remains an integer, because it
            # was reduced by $p0 when we shifted off the first
            # element early in the INPUT loop iteration.
            # Also note that each element of @fp stays in the range
            # [0,1] because if $r0, then slack($_, $p0) * -$gain <=
            # $p0 * $_ * (1.0 - $p0) / ($p0 * (1.0 - $p0)) ==
            # $_, and otherwise slack($_, $p0) * $gain <=
            # (1 - $p0) * (1 - $_) * $p0 / ($p0 * (1.0 - $p0)) ==
            # 1 - $_.
            # We modify in place here, for performance.
            $_ += shift(@slack) * $gain for(@fp);
        }
    }
    die if @_;
    return @out;
}

sub _adjust_input {
    my $p = shift;

    # Adjust @$p to account for numerical errors due to small
    # difference of large numbers when the integer parts are big.
    my $sum = sum @$p;
    if ( $sum != floor($sum) ) {
        my $target = floor($sum + 0.5);

        die "Total loss of precision"
            unless abs($sum - $target) < 0.1 && $sum + 0.05 != $sum;

        my $adj = $target / $sum;
        if ( $adj <= 1.0 ) {
            $_ *= $adj for @$p;
        } else {
            $adj = (@$p - $target) / (@$p - $sum);
            $_ = 1.0 - (1.0-$_) * $adj for @$p;
        }
    }
}

sub _check_invariants {
    my ( $eps, $v, $fp ) = @_;

    if ( $debug > 1 ) {
        warn sprintf "%d %f\n", floor($_), $_ for @$fp;
    }

    die unless @$v && @$v == @$fp;

    for ( @$fp ) {
        die "internal error: $_ < \$eps"       if $_ < -$eps;
        die "internal error: $_ > 1.0 + \$eps" if $_ > 1.0 + $eps;
    }

    my $sum = sum @$fp;
    die "internal error: sum=$sum"
        unless abs($sum - floor($sum + 0.5)) < $eps * (1 + $sum);
}

sub _slack {
    my ( $p0, $fp ) = @_;

    my $sum = 0.0;
    my @slack = map {
        if ( 1 ) {
            my $slack = min $p0 * $_, (1 - $_) * (1.0 - $p0);
            $sum += $slack;
            $slack;
        }
        else {
            # This is fewer FLOPS, but the perf benefit
            # is only 1% on a modern system, and it leads
            # to greater numerical errors for some reason.
            my $add = $p0 + $_;
            my $mult = $p0 * $_;
            $add > 1.0 ? 1.0 - $add + $mult : $mult
        }
    } @$fp;

    return ($sum, @slack);
}

1;

__END__

=back

=head1 CAVEATS

=over 2

=item *

A number of in-situ integrity checks are enabled by default.
The execution time can be reduced by approximately 25% if these checks
are disabled by setting $Math::Round::Fair::debug to 0.
This might become the default someday, so set it to 1 if you really want
the checks.

=item *

The algorithm that satisfies these constraints is not necessarily unique,
and then implementation may change over time.

=item *

Randomness is obtained via calls to rand().
You might want to call srand() first.
The number of invocations to rand() per call may change in subsequent versions.

=item *

The rounding of each element in the list in I<not> independent of the rounding
of the other elements.
This is the price that you pay for guaranteeing that the total is also fair
and accurate.

=back

=head1 AUTHORS

Marc Mims <marc@questright.com>, Anders Johnson <anders@ieee.org>

=head1 LICENSE

Copyright (c) 2009-2010 Marc Mims

This is free software.  You may use it, distributed it, and modify it under the
same terms as Perl itself.

=cut

PROOF THAT $tslack >= $p0 * (1 - $p0)

Imagine a clock, where 0.0 is noon, 0.5 is 6 o'clock, and 1.0 is noon again,
etc.  If we start at 0.0, and advance clockwise first for $p0, and then for
every remaining element of @fp, then we have to wind up at noon (according
to the invariant).

If $p0 is 0 or 1, then it is possible that every element of @fp is either 0
or 1, in which case $tslack is 0 and the theorem holds trivially.  Otherwise,
the clock has to somehow travel the absolute distance back to noon.

slack($_, $p0) == (1-$p0)*(1-$_) if and only if (1-$p0)*(1-$_) <= $p0*$_, or
equivalently 1-$p0-$_+$p0*$_ <= $p0*$_, or equivalently $_ >= 1-$p0.  For
$_ <= 1-$p0, we can imagine the clock advancing clockwise by $_.  For
$_ >= 1-$p0, we can imagine the clock retreating counter-clockwise by 1-$_.
Starting at $p0, the total absolute distance traveled clockwise must be at least
1-$p0, or the total abolute distance traveled counter-clockwise must be
at least $p0 (or both).

Mathematically:

$tslack == sum { $_ <= 1-$p0 ? $p0*$_ : (1-$p0)*(1-$_) } @fp ==
  $p0 * sum { $_ <= 1-$p0 ? $_ : 0 } @fp +
  (1-$p0) * sum { $_ >= 1-$p0 ? 1-$_ : 0 } @fp.

sum { $_ <= 1-$p0 ? $_ : 0 } @fp >= 1-$p0. [clockwise]
sum { $_ >= 1-$p0 ? 1-$_ : 0 } @fp >= $p0. [counter-clockwise]

And if $p0 * (1 - $p0) > 0, then either
  sum { $_ <= 1-$p0 ? $_ : 0 } @fp >= 1 - $p0, which implies
  $tslack >= $p0 * sum { $_ <= 1-$p0 ? $_ : 0 } @fp >= $p0 * (1 - $p0);
or
  sum { $_ >= 1-$p0 ? 1 - $_ : 0 } @fp >= $p0, which implies
  $tslack >= (1-$p0) * sum { $_ >= 1-$p0 ? 1-$_ : 0 } @fp >= (1 - $p0) * $p0.

q.e.d.

