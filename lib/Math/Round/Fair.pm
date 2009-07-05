package Math::Round::Fair;
use warnings;
use strict;
use 5.005000;
use Carp;
use base qw/Exporter/;

our $VERSION = '0.01';

our @EXPORT_OK = qw/round_fair/;

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

    croak "Value to be allocated must be an integer" unless int($value) == $value;
    
    my $basis = 0;
    for my $w ( @_ ) {
        croak "Weights must be > 0" unless $w > 0;
        $basis += $w;
    }

    return ($value) if @_ == 1;

    map {
        my $allocation = $value * $_ / $basis;
        my $allocated  = int $allocation;
        my $remainder  = $allocation - $allocated;
        ++$allocated if rand() < $remainder;
        $basis -= $_;
        $value -= $allocated;
        $allocated
    } @_;
}

1;

__END__

=back

=head1 AUTHOR

Marc Mims <marc@questright.com>

=head1 LICENSE

Copyright (c) 2009 Marc Mims

This is free software.  You may use it, distributed it, and modify it under the
same terms as Perl itself.
