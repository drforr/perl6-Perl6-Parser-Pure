use v6;

use Test;
use Perl6::Parser::Pure;

use lib 't/lib/';
use Utils;

plan 25;

# Please note for the record that these tests aren't meant to cover the
# admittedly bewildering array (hee) of inputs these operators can take, they're
# just meant to cover the fact that the basic operators format correctly.
#
# The code being tested will never be executed, so I'm not concerned about
# whether a sample produces warnings or not. It'd be nice if it didn't, but
# not at the cost of extraneous terms.
#
# There are probably ways to bum down some of these tests as well, but as long
# as the operators remain operators I don't mind.
#
# For those operators that can generate or exist in list context, we're also not
# concerned about the context they generate in these tests. The text is meant to
# be very simple.
#
# There are also +=, R= and /= variants, those will be in separate files.
# As will the [+] hyperoperator, as it'll probably get a test suite of its own.

my $pp = Perl6::Parser::Pure.new;
my $*CONSISTENCY-CHECK = True;
my $*FALL-THROUGH = True;

subtest {
  plan 4;
  
  subtest {
    plan 2;
    
    subtest {
      plan 2;

      my $source = Q{<a>};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::String ), Q{found string};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{no ws};
    
    subtest {
      plan 2;

      my $source = Q:to[_END_];
      < a >
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::String ), Q{found string};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{ws};
  }, Q{<>};
  
  subtest {
    plan 2;
    
    subtest {
      plan 2;

      my $source = Q{(1)};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{no ws};
    
    subtest {
      plan 2;

      my $source = Q:to[_END_];
      ( 1 )
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{ws};
  }, Q{()};
  
  subtest {
    plan 2;
    
    subtest {
      plan 2;

      my $source = Q[{1}];
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Block ), Q{found block};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{no ws};
    
    subtest {
      plan 2;

      my $source = Q:to[_END_];
      { 1 }
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Block ), Q{found block};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{ws};
  }, Q[{}];
  
  subtest {
    plan 2;
    
    subtest {
      plan 2;

      my $source = Q{[1]};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{no ws};
    
    subtest {
      plan 2;

      my $source = Q:to[_END_];
      [ 1 ]
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{ws};
  }, Q{[]}
}, Q{Term Precedence};

subtest {
  plan 15;
  
  subtest {
    plan 2;
    
    subtest {
      plan 2;

      my $source = Q{my@a;@a[2]};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{no ws};
    
    subtest {
      plan 2;

      my $source = Q:to[_END_];
      my @a; @a[ 2 ]
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{ws};
  }, Q{[]};
  
  subtest {
    plan 2;
    
    subtest {
      plan 2;

      # Whitespace sensitive between 'a' and '{' '}'
      my $source = Q{my%a;%a{"foo"}};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{no ws};
    
    subtest {
      plan 2;

      # Whitespace sensitive between 'a' and '{' '}'
      my $source = Q:to[_END_];
      my %a; %a{ "foo" }
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{ws};
  }, Q{%a{}};
  
  subtest {
    plan 2;
    
    subtest {
      plan 2;

      # Whitespace sensitive between 'a' and '<' '>'
      my $source = Q{my%a;%a<foo>};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{no ws};
    
    subtest {
      plan 2;

      # Whitespace sensitive between 'a' and '<' '>'
      my $source = Q:to[_END_];
      my %a; %a< foo >
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{ws};
  }, Q{%a{}};
  
  subtest {
    plan 2;
    
    subtest {
      plan 2;

      # Whitespace sensitive between 'a' and '«' '»'
      my $source = Q{my%a;%a«foo»};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{no ws};
    
    subtest {
      plan 2;

      # Whitespace sensitive between 'a' and '«' '»'
      my $source = Q:to[_END_];
      my %a; %a« foo »
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{ws};
  }, Q{%a«»};
  
  subtest {
    plan 3;
    
    subtest {
      plan 2;

      # Whitespace sensitive between 'chomp' and '(' ')'
      my $source = Q{chomp()};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{no arguments, no ws};
    
    subtest {
      plan 2;

      # Whitespace sensitive between 'chomp' and '(' ')'
      my $source = Q:to[_END_];
      chomp( )
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{no arguments, ws};
    
    subtest {
      plan 2;

      # Whitespace sensitive between 'chomp' and '(' ')'
      my $source = Q:to[_END_];
      chomp( 1 )
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{with arguments};
  }, Q{func()};
  
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    42.round
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};
    
    done-testing;
  }, Q{.};
  
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    42.&round
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};
    
    done-testing;
  }, Q{.&};
  
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    Int.=round
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};
    
    done-testing;
  }, Q{.=};
  
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    42.^name
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};
    
    done-testing;
  }, Q{.^};
  
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    42.?name
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};
    
    done-testing;
  }, Q{.?};
  
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    42.+name
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};
    
    done-testing;
  }, Q{.+};
  
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    42.*name
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};
    
    done-testing;
  }, Q{.*};
  
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    42>>.say
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};
    
    done-testing;
  }, Q{>>.};
  
  subtest {
    plan 2;
    
    subtest {
      plan 2;

      my $source = Q{my$a;$a.:<++>};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{no ws};
    
    subtest {
      plan 2;

      my $source = Q:to[_END_];
      my $a; $a.:< ++ >
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{ws};
  }, Q{.:};
  
  subtest {
    plan 2;
    
    subtest {
      plan 2;

      my $source = Q{my $a; $a.Foo::Bar};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{no ws};
    
    subtest {
      plan 2;

      my $source = Q:to[_END_];
      my $a; $a.Foo::Bar
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{ws};
  }, Q{.::};
}, Q{Method Postfix Precedence};

subtest {
  plan 4;
  
  subtest {
    plan 2;
    
    subtest {
      plan 2;

      my $source = Q{my$a;++$a};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{no ws};
    
    subtest {
      plan 2;

      my $source = Q:to[_END_];
      my $a; ++$a
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{ws};
  }, Q{++$a};
  
  subtest {
    plan 2;
    
    subtest {
      plan 2;

      my $source = Q{my$a;--$a};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{no ws};
    
    subtest {
      plan 2;

      my $source = Q:to[_END_];
      my $a; --$a
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{ws};
  }, Q{--$a};
  
  subtest {
    plan 2;
    
    subtest {
      plan 2;

      my $source = Q{my$a;$a++};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{no ws};
    
    subtest {
      plan 2;

      my $source = Q:to[_END_];
      my $a; $a++
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
      
      done-testing;
    }, Q{ws};
  }, Q{$a++};
  
  subtest {
    plan 2;
  
    subtest {
      plan 2;

      my $source = Q{my$a;$a--};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
  
      done-testing;
    }, Q{no ws};
  
    subtest {
      plan 2;

      my $source = Q:to[_END_];
      my $a; $a--
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};
  
      done-testing;
    }, Q{ws};
  }, Q{$a--};
}, Q{Autoincrement Precedence};

subtest {
  plan 2;

  subtest {
    plan 2;

    my $source = Q{1**2};
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{no ws};

  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 ** 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{ws};
}, Q{Exponentiation Precedence};

subtest {
  plan 9;

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{?2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      ? 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{?};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{!2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      ! 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{!};

  subtest {
    plan 2;

    my $source = Q:to[_END_];
    + 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{+};

  subtest {
    plan 2;

    my $source = Q:to[_END_];
    - 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{-};

  subtest {
    plan 2;

    my $source = Q:to[_END_];
    ~ 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{~};

  subtest {
    plan 2;

    my $source = Q:to[_END_];
    | 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{|};

  subtest {
    plan 2;

    my $source = Q:to[_END_];
    +^ 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{+^};

  subtest {
    plan 2;

    my $source = Q:to[_END_];
    ?^ 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{?^};

  subtest {
    plan 2;

    my $source = Q:to[_END_];
    ^ 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{^};
}, Q{Symbolic Unary Precedence};

subtest {
  plan 11;

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1*2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 * 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{*};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1/2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 / 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{/};

  # XXX '1div2' is illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 div 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{div};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1%2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 % 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{%};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1%%2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 %% 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{%%};

  # XXX '1mod2' is illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 mod 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{mod};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1+&2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 +& 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{+&};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1+<2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 +< 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{+<};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1+>2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 +> 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{+>};

  # XXX '1gcd2' is illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 gcd 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{gcd};

  # XXX '1lcm2' is illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 lcm 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{lcm};
}, Q{Multipicative Precedence};

subtest {
  plan 5;

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1+2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 + 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{+};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1=2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 - 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{-};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1+|2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 +| 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{+|};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1+^2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 +^ 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{+^};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1?|2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 ?| 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{?|};
}, Q{Additive Precedence};

subtest {
  plan 2;

  # XXX '1x2' is illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 x 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{x};

  # XXX '1xx2' is illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 xx 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{xx};
}, Q{Replication Precedence};

subtest {
  plan 2;

  subtest {
    plan 2;

    my $source = Q{1~2};
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{no ws};

  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 ~ 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{ws};
}, Q{Concatenation Precedence};

subtest {
  plan 2;

  subtest {
    plan 2;

    my $source = Q{1&2};
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{no ws};

  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 & 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{ws};
}, Q{Junctive AND Precedence};

subtest {
  plan 2;

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1|2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 | 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{|};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1^2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 ^ 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{^};
}, Q{Junctive OR Precedence};

# XXX 'temp$a' is illegal (weird.)
subtest {
  plan 2;

  subtest {
    plan 2;

    my $source = Q:to[_END_];
    my $a; temp $a
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{temp};

  # XXX 'let$a' is illegal (weird.)
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    my $a; let $a
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{ws};
}, Q{Named Unary Precedence};

subtest {
  plan 9;

  # XXX '1does2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 does 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{does};

  # XXX '1but2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 but 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{but};

  # XXX '1cmp2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 cmp 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{cmp};

  # XXX '1leg2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 leg 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{leg};

  # XXX '1<=>2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 <=> 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{<=>};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1..2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 .. 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{..};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1^..2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 ^.. 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{^..};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1..^2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 ..^ 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{..^};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1^..^2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 ^..^ 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{^..^};
}, Q{Nonchaining Binary Precedence};

subtest {
  plan 20;

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1==2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 == 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{==};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1!=2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 != 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{!=};

  # XXX '1<2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 < 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{<};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1>2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 > 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{>};

  # XXX '1<=2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 <= 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{<=};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1>2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 > 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{>};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1>=2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 >= 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{>=};

  # XXX '1eq2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 eq 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{eq};

  # XXX '1ne2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 ne 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{ne};

  # XXX '1gt2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 gt 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{gt};

  # XXX '1ge2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 ge 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{ge};

  # XXX '1lt2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 lt 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{lt};

  # XXX '1le2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 le 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{le};

  # XXX '1before2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 before 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{before};

  # XXX '1after2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 after 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{after};

  # XXX '1eqv2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 eqv 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{eqv};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1===2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1===2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{===};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1=:=2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 =:= 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{=:=};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1~~2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 ~~ 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{~~};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1=~=2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 =~= 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{=~=};
}, Q{Chaining Binary Precedence};

subtest {
  plan 2;

  subtest {
    plan 2;

    my $source = Q{1&&2};
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{no ws};

  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 && 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{ws};
}, Q{Tight AND Precedence};

subtest {
  plan 5;

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1||2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 || 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{||};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1^^2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 ^^ 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{^^};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1//2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 // 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{//};

  # XXX '1min2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 min 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{min};

  # XXX '1max2' is illegal.
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 max 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{ws};
}, Q{Tight OR Precedence};

subtest {
  plan 9;

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1??2!!3};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 ?? 2 !! 3
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{?? !!};

  # XXX '1ff2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 ff 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{ff};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1...2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 ^ff 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{^ff};

  # XXX '1ff^2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 ff^ 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{ff^};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1^ff^2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 ^ff^ 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{^ff^};

  # XXX '1fff2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 fff 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{fff};

  # XXX '1^fff2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 ^fff 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{^fff};

  # XXX '1fff^2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 fff^ 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{fff^};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1^fff^2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 ^fff^ 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{^fff^};
}, Q{Conditional Operator Precedence};

subtest {
  plan 2;

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{my$a=1};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      my $a = 1
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{=};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{a=>1};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      a => 1
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{=>};
}, Q{Item Assignment Precedence};

subtest {
  plan 2;

  # XXX 'not2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    not 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{not};

  # XXX 'so1' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    so 1
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{so};
}, Q{Loose Unary Precedence};

subtest {
  plan 2;

  subtest {
    plan 2;

    my $source = Q{1,2};
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{no ws};

  subtest {
    plan 2;

    my $source = Q:to[_END_];
    1 , 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{ws};
}, Q{Comma Operator Precedence};

# XXX 'X' and 'Z' have a lot of variants, test separately?
#
subtest {
  plan 3;

  # XXX '3Z2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    3 Z 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{Z};

  # XXX '3X2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    3 X 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{X};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{1...2};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      1 ... 2
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{...};
}, Q{List Infix Precedence};

subtest {
  plan 5;

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{my$a=1};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      my $a = 1
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{=};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{my$a:=1};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      my $a := 1
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{:=};

  # XXX "::= NIY";

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{...};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      ...
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{...};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{!!!};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      !!!
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{!!!};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{???};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      ???
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{???};

  # XXX Undecided on [+] implementation
}, Q{List Prefix Precedence};

subtest {
  plan 2;

  # XXX '3and2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    3 and 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{and};

  # XXX '3andthen2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    3 andthen 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{andthen};
}, Q{Loose AND Precedence};

subtest {
  plan 2;

  # XXX '3or2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    3 or 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{or};

  # XXX '3orelse2' illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    3 orelse 2
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{orelse};
}, Q{Loose OR Precedence};

subtest {
  plan 2;

  # XXX "@a<=='a'" illegal
  subtest {
    plan 2;

    my $source = Q:to[_END_];
    my @a; @a <== 'a'
    _END_
    my $tree = $pp.to-tree( $source );
    ok has-a( $tree, Perl6::Operator ), Q{found operator};
    is $pp.to-string( $tree ), $source, Q{formatted};

    done-testing;
  }, Q{<==};

  subtest {
    plan 2;

    subtest {
      plan 2;

      my $source = Q{my@a;'a'==>@a};
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{no ws};

    subtest {
      plan 2;

      my $source = Q:to[_END_];
      my @a; 'a' ==> @a
      _END_
      my $tree = $pp.to-tree( $source );
      ok has-a( $tree, Perl6::Operator ), Q{found operator};
      is $pp.to-string( $tree ), $source, Q{formatted};

      done-testing;
    }, Q{ws};
  }, Q{==>};
}, Q{Sequencer Precedence};

# vim: ft=perl6
