use v6;

use Test;

use lib '../perl6-grammarx-debugger/lib/';
use Perl6::Parser::Pure;

my $ppp = Perl6::Parser::Pure.new;

ok $ppp.to-tree( '' );
ok $ppp.to-tree( 'use v6' );
ok $ppp.to-tree( 'use v6;' );
#ok $ppp.to-tree( 'use v6 if 1' );
#ok $ppp.to-tree( 'if 1' );

say $ppp.to-tree( 'use v6;while 1' );
#say $ppp.to-tree( 'use v6;' );
#say $ppp.to-tree( '' );

# vim: ft=perl6
