use v6;

use Test;
use Perl6::Parser::Pure;

plan 15;

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
ok $ppp.to-tree( Q{;} );
dies-ok { $ppp.to-tree( Q{:} ) }, Q{failing test};

subtest 'version', {
  ok $ppp.to-tree( Q{use v6} );
  ok $ppp.to-tree( Q{use v6;} );

  done-testing;
};

ok $ppp.to-tree( Q{loop {}} );

subtest 'no', {
  ok $ppp.to-tree( Q{no Module} );
  ok $ppp.to-tree( Q{no My::Module} );

  subtest 'full statement', {
    ok $ppp.to-tree( Q{no Module;} );
    ok $ppp.to-tree( Q{no My::Module;} );
  };

  done-testing;
};

subtest 'require', {
  ok $ppp.to-tree( Q{require Module} );
  ok $ppp.to-tree( Q{require My::Module} );

  subtest 'full statement', {
    ok $ppp.to-tree( Q{require Module;} );
    ok $ppp.to-tree( Q{require My::Module;} );
  };

  done-testing;
};

subtest 'need', {
  subtest 'Module', {
    ok $ppp.to-tree( Q{need Module} );
    ok $ppp.to-tree( Q{need My::Module} );

    subtest 'full statement', {
      ok $ppp.to-tree( Q{need Module;} );
      ok $ppp.to-tree( Q{need My::Module;} );
    };

    done-testing;
  };
  subtest 'version', {
    ok $ppp.to-tree( Q{need v} ); # This just means "Need module 'v'".
    ok $ppp.to-tree( Q{need v6} );
    ok $ppp.to-tree( Q{need v6.c} );

    subtest 'full statement', {
      ok $ppp.to-tree( Q{need v;} );
      ok $ppp.to-tree( Q{need v6;} );
      ok $ppp.to-tree( Q{need v6.c;} );
    };

    note "It might be useful to catch this for deprecation warnings.";

    done-testing;
  };
};

subtest 'import', {
  ok $ppp.to-tree( Q{import Module} );
  ok $ppp.to-tree( Q{import My::Module} );

  subtest 'full statement', {
    ok $ppp.to-tree( Q{import Module;} );
    ok $ppp.to-tree( Q{import My::Module} );
  };

  done-testing;
};

subtest 'use Module', {
  ok $ppp.to-tree( Q{use Module} );
  ok $ppp.to-tree( Q{use My::Module} );

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

# vim: ft=perl6
