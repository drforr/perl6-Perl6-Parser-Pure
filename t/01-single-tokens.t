use v6;

use Test;
use Perl6::Parser::Pure;

plan 7;

# Reuse $pp so that we can make sure state is cleaned up.
#
# Also, just check that we have the keys we're expecting in the hash/list.
#
my $ppp                = Perl6::Parser::Pure.new;
my $*CONSISTENCY-CHECK = True;
my $*FALL-THROUGH      = True;

# Most of these will throw warnings, but will actually compile.
# Warnings aren't the point here.

sub parse( Str $code ) {
  $ppp.to-tree( $code );
}

ok parse( Q{} ),         Q{'' (the empty file)};

subtest 'failing', {
  ok !parse( Q{~} );
  ok !parse( Q{`} );
  ok !parse( Q{!} );
  # '@' is legal
  # '#' is legal
  # '$' is legal
  # '%' is legal
  ok !parse( Q{^} );
  # '&' is legal
  # '*' is legal
  ok !parse( Q{(} );
  ok !parse( Q{)} );
  ok !parse( Q{-} );
  ok !parse( Q{_} );
  ok !parse( Q{=} );
  ok !parse( Q{+} );

  ok !parse( Q{:} );
};

subtest 'BOM', {
  ok !parse( qq{\xFFFE} ), Q{'\xFFFE' fails, not BOM};
  ok parse( qq{\xFEFF} ),  Q{'\xFEFF' (someone set us up the BOM)};
};

# use <version> has nothing to do with use <module>, so it's tested
# separately, at the start of the file.
#
subtest 'use vN', {
  subtest 'no BOM', {
    subtest 'version variants', {
      ok parse( Q{use v6} );
      ok !parse( Q{use v*} ), 'failing test';
      ok parse( Q{use v6.*} );
      ok !parse( Q{use v*.6} ), 'failing test';
      ok parse( Q{use v6.*.2} );
      ok parse( Q{use v6.2.*} );
    };
    ok parse( Q{ use v5} );
    ok parse( qq{\nuse v41} );
  };

  subtest 'with BOM', {
    ok parse( qq{\xFEFFuse v6} );
    ok parse( qq{\xFEFF use v5} );
    ok parse( qq{\nuse v41} );
    ok !parse( qq{ \xFEFFuse v6} ), 'failing test';
  };
};

# Whaddya mean these are empty?
#
# Well, ya see, 'use v6.2.*' isn't a statement, but a <lang-version>, which
# is optional. So technically '' before ';' is the first statement, making ';'
# the terminator of an empty statement.
#
subtest 'empty statement', {
  ok parse( Q{;} );
  ok parse( Q{;;} );
  ok parse( Q{ ;;} );
  ok parse( Q{; ;} );
  ok parse( Q{;; } );
  ok parse( Q{use v6.2.*;} );
  ok parse( Q{use v6.2.* ;} );
  ok parse( qq{use v6.2.*\n;} );
};

subtest 'label', {
  ok !parse( Q{a:;} );
  ok parse( Q{a: ;} );
  ok parse( Q{a: b: ;} );
};

subtest 'if', {
# ok parse( Q{ if} );
# ok parse( Q{ if;} );
# ok parse( Q{ if; } );
  ok parse( Q{ ifÞ} );
  ok parse( Q{ ifÞ;} );
  ok parse( Q{ ifÞ; } );
};

# vim: ft=perl6
