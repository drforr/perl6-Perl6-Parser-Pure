use v6;

use Test;
use Perl6::Parser::Pure;

plan 6;

# Reuse $pp so that we can make sure state is cleaned up.
#
# Also, just check that we have the keys we're expecting in the hash/list.
#
my $ppp                = Perl6::Parser::Pure.new;
my $*CONSISTENCY-CHECK = True;
my $*FALL-THROUGH      = True;

# Most of these will throw warnings, but will actually compile.
# Warnings aren't the point here.

ok $ppp.to-tree( Q{} ),        Q{'' (the empty file)};
ok $ppp.to-tree( Q{;} ),       Q{';'};
dies-ok { $ppp.to-tree( Q{:} ) },      Q{':' fails};

subtest 'version', {
  ok $ppp.to-tree( Q{use v6} ),  Q{'use v6'};
  ok $ppp.to-tree( Q{use v6;} ), Q{'use v6;'};

  done-testing;
};

ok $ppp.to-tree( Q{loop {}} ), Q{'loop {}'};

subtest 'require', {
  ok $ppp.to-tree( Q{require Module} ),
     Q{'require Module'};
  ok $ppp.to-tree( Q{require My::Module} ),
     Q{'require My::Module'};
  ok $ppp.to-tree( Q{require Module;} ),
     Q{'require Module;'};
  ok $ppp.to-tree( Q{require My::Module} ),
     Q{'require My::Module;'};

  done-testing;
};

# vim: ft=perl6
