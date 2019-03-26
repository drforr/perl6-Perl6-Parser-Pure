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

#`{

ok $ppp.to-tree( Q{if Þ} ); # XXX Complete this

subtest 'loop', {

  subtest 'failing', {
    dies-ok { $ppp.to-tree( Q[for (] ) };
    dies-ok { $ppp.to-tree( Q[for ( ; ; )] ) };
    dies-ok { $ppp.to-tree( Q[for my] ) };
    dies-ok { $ppp.to-tree( Q{loop ( ) {}} ) };
    dies-ok { $ppp.to-tree( Q{loop ( ; ) {}} ) };
  };

  ok $ppp.to-tree( Q{loop {}} );

  # Just for the hell of it, run through the permutations.
  #
  ok $ppp.to-tree( Q{loop ( ; ; ) {}} ); # 000
  ok $ppp.to-tree( Q{loop ( Þ ; ; ) {}} ); # 100
  ok $ppp.to-tree( Q{loop ( ; Þ ; ) {}} ); # 010
  ok $ppp.to-tree( Q{loop ( Þ ; Þ ; ) {}} ); # 110
  ok $ppp.to-tree( Q{loop ( ; ; Þ ) {}} ); # 001
  ok $ppp.to-tree( Q{loop ( Þ ; ; Þ ) {}} ); # 101
  ok $ppp.to-tree( Q{loop ( ; Þ ; Þ ) {}} ); # 011
  ok $ppp.to-tree( Q{loop ( Þ ; Þ ; Þ ) {}} ); # 111
};

subtest 'no', {
  ok $ppp.to-tree( Q{no Module} );
  ok $ppp.to-tree( Q{no My::Module} );
  ok $ppp.to-tree( Q{no My::( Þ)} ); # XXX Woop woop... ( Þ ) is an error!
  #ok $ppp.to-tree( Q{no My::( Þ )} ); # XXX Woop woop... ( Þ ) is an error!

  subtest 'full statement', {
    ok $ppp.to-tree( Q{no Module;} );
    ok $ppp.to-tree( Q{no My::Module;} );
  };

  done-testing;
};

subtest 'require', {
  ok $ppp.to-tree( Q{require Module} );
  ok $ppp.to-tree( Q{require My::Module} );
  ok $ppp.to-tree( Q{require My::( Þ)} ); # XXX Woop woop... ( Þ ) is an error!
  #ok $ppp.to-tree( Q{require My::( Þ )} ); # XXX Woop woop... ( Þ ) is an error!
  ok $ppp.to-tree( Q{require Þ} );

  subtest 'full statement', {
    ok $ppp.to-tree( Q{require Module;} );
    ok $ppp.to-tree( Q{require My::Module;} );
    ok $ppp.to-tree( Q{require Þ;} );
  };

  done-testing;
};

subtest 'need', {
  subtest 'Module', {
    ok $ppp.to-tree( Q{need Module} );
    ok $ppp.to-tree( Q{need My::Module} );
    ok $ppp.to-tree( Q{need My::( Þ)} ); # XXX Woop woop... ( Þ ) is an error!
    #ok $ppp.to-tree( Q{need My::( Þ )} ); # XXX Woop woop... ( Þ ) is an error!
    ok $ppp.to-tree( Q{need Module, Other::Module} );

    subtest 'full statement', {
      ok $ppp.to-tree( Q{need Module;} );
      ok $ppp.to-tree( Q{need My::Module;} );
    };

    done-testing;
  };
  subtest 'version', {

    subtest 'failing', {
      dies-ok { $ppp.to-tree( Q{need v6} ) };
      dies-ok { $ppp.to-tree( Q{need v6.c} ) };
    };

    ok $ppp.to-tree( Q{need v} ); # This just means "Need module 'v'".

    subtest 'full statement', {
      ok $ppp.to-tree( Q{need v;} );
      dies-ok { $ppp.to-tree( Q{need v6;} ) };
      dies-ok { $ppp.to-tree( Q{need v6.c;} ) };
    };

    note "It might be useful to catch this for deprecation warnings.";

    done-testing;
  };
};

subtest 'import', {
  ok $ppp.to-tree( Q{import Module} );
  ok $ppp.to-tree( Q{import My::Module} );
  ok $ppp.to-tree( Q{import My::( Þ)} ); # XXX Woop woop... ( Þ ) is an error!
  #ok $ppp.to-tree( Q{import My::( Þ )} ); # XXX Woop woop... ( Þ ) is an error!
  ok $ppp.to-tree( Q{import Module Þ} );

  subtest 'full statement', {
    ok $ppp.to-tree( Q{import Module;} );
    ok $ppp.to-tree( Q{import My::Module} );
  };

  done-testing;
};

subtest 'use Module', {
  ok $ppp.to-tree( Q{use Module} );
  ok $ppp.to-tree( Q{use My::Module} );
  ok $ppp.to-tree( Q{use My::( Þ)} ); # XXX Woop woop... ( Þ ) is an error!
  #ok $ppp.to-tree( Q{use My::( Þ )} ); # XXX Woop woop... ( Þ ) is an error!

  subtest 'full statement', {
    ok $ppp.to-tree( Q{use Module;} );
    ok $ppp.to-tree( Q{use My::Module;} );
  };

  done-testing;
};

subtest 'when', {
  ok $ppp.to-tree( Q{when {}} );

  subtest 'failing', {
    dies-ok { $ppp.to-tree( Q{when} ) };

    dies-ok { $ppp.to-tree( Q[when{] ) };
    dies-ok { $ppp.to-tree( Q[when}] ) };

    dies-ok { $ppp.to-tree( Q[when {] ) };
    dies-ok { $ppp.to-tree( Q[when }] ) };
  };
};

subtest 'default', {
  ok $ppp.to-tree( Q{default {}} );

  subtest 'failing', {
    dies-ok { $ppp.to-tree( Q{default} ) };

    dies-ok { $ppp.to-tree( Q[default{] ) };
    dies-ok { $ppp.to-tree( Q[default}] ) };

    dies-ok { $ppp.to-tree( Q[default {] ) };
    dies-ok { $ppp.to-tree( Q[default }] ) };
  };
};

subtest 'CATCH', {
  ok $ppp.to-tree( Q{CATCH {}} );

  subtest 'failing', {
    dies-ok { $ppp.to-tree( Q{CATCH} ) };

    dies-ok { $ppp.to-tree( Q[CATCH{] ) };
    dies-ok { $ppp.to-tree( Q[CATCH}] ) };

    dies-ok { $ppp.to-tree( Q[CATCH {] ) };
    dies-ok { $ppp.to-tree( Q[CATCH }] ) };
  };
};

subtest 'CONTROL', {
  ok $ppp.to-tree( Q{CONTROL {}} );

  subtest 'failing', {
    dies-ok { $ppp.to-tree( Q{CONTROL} ) };

    dies-ok { $ppp.to-tree( Q[CONTROL{] ) };
    dies-ok { $ppp.to-tree( Q[CONTROL}] ) };

    dies-ok { $ppp.to-tree( Q[CONTROL {] ) };
    dies-ok { $ppp.to-tree( Q[CONTROL }] ) };
  };
};

subtest 'QUIT', {
  ok $ppp.to-tree( Q{QUIT {}} );

  subtest 'failing', {
    dies-ok { $ppp.to-tree( Q{QUIT} ) };

    dies-ok { $ppp.to-tree( Q[QUIT{] ) };
    dies-ok { $ppp.to-tree( Q[QUIT}] ) };

    dies-ok { $ppp.to-tree( Q[QUIT {] ) };
    dies-ok { $ppp.to-tree( Q[QUIT }] ) };
  };
};

}

# vim: ft=perl6
