use Perl6::Parser::Pure::Classes;

# XXX Note - always put the new tokens/rules at the top... at least that way
# XXX it's consistently jumbled.

grammar Perl6::Parser::Pure::Grammar
  {
  token comp_unit
    {
    # XXX There was code here

    <.bom>?
    <lang-version>
    <.finishpad>
    <statementlist=.FOREIGN_LANG($*MAIN, 'statementlist', 1)>

    <.install_doc_phaser>

    [ $ || <.typed_panic: 'X::Syntax::Confused'> ]

    <.explain_mystery>
    <.cry_sorrows>
    # THere was code here
    }

  rule TOP
    {
    <comp_unit>
    }
  }

#role startstops[$start, $stop1, $stop2] {
#    token starter { $start }
#    token stopper { $stop1 | $stop2 }
#}

#role startstop[$start, $stop] {
#    token starter { $start }
#    token stopper { $stop }
#}

#role stop[$stop] {
#    token starter { <!> }
#    token stopper { $stop }
#}

#role STD {
#  token opener
#    {
#      <[
#        \x0028 \x003C \x005B \x007B \x00AB \x0F3A \x0F3C \x169B \x2018 \x201A
#        \x201B \x201C \x201E \x201F \x2039 \x2045 \x207D \x208D \x2208 \x2209
#        \x220A \x2215 \x223C \x2243 \x2252 \x2254 \x2264 \x2266 \x2268 \x226A
#        \x226E \x2270 \x2272 \x2274 \x2276 \x2278 \x227A \x227C \x227E \x2280
#        \x2282 \x2284 \x2286 \x2288 \x228A \x228F \x2291 \x2298 \x22A2 \x22A6
#        \x22A8 \x22A9 \x22AB \x22B0 \x22B2 \x22B4 \x22B6 \x22C9 \x22CB \x22D0
#        \x22D6 \x22D8 \x22DA \x22DC \x22DE \x22E0 \x22E2 \x22E4 \x22E6 \x22E8
#        \x22EA \x22EC \x22F0 \x22F2 \x22F3 \x22F4 \x22F6 \x22F7 \x2308 \x230A
#        \x2329 \x23B4 \x2768 \x276A \x276C \x276E \x2770 \x2772 \x2774 \x27C3
#        \x27C5 \x27D5 \x27DD \x27E2 \x27E4 \x27E6 \x27E8 \x27EA \x2983 \x2985
#        \x2987 \x2989 \x298B \x298D \x298F \x2991 \x2993 \x2995 \x2997 \x29C0
#        \x29C4 \x29CF \x29D1 \x29D4 \x29D8 \x29DA \x29F8 \x29FC \x2A2B \x2A2D
#        \x2A34 \x2A3C \x2A64 \x2A79 \x2A7D \x2A7F \x2A81 \x2A83 \x2A8B \x2A91
#        \x2A93 \x2A95 \x2A97 \x2A99 \x2A9B \x2AA1 \x2AA6 \x2AA8 \x2AAA \x2AAC
#        \x2AAF \x2AB3 \x2ABB \x2ABD \x2ABF \x2AC1 \x2AC3 \x2AC5 \x2ACD \x2ACF
#        \x2AD1 \x2AD3 \x2AD5 \x2AEC \x2AF7 \x2AF9 \x2E02 \x2E04 \x2E09 \x2E0C
#        \x2E1C \x2E20 \x2E28 \x3008 \x300A \x300C \x300E \x3010 \x3014 \x3016
#        \x3018 \x301A \x301D \xFD3E \xFE17 \xFE35 \xFE37 \xFE39 \xFE3B \xFE3D
#        \xFE3F \xFE41 \xFE43 \xFE47 \xFE59 \xFE5B \xFE5D \xFF08 \xFF1C \xFF3B
#        \xFF5B \xFF5F \xFF62
#      ]>
#    }
#
#  token starter
#    {
#    <!>
#    }
#
#  token stopper
#    {
#    <!>
#    }
#
#  token babble($l, @base_tweaks?)
#    {
#    [ <quotepair> <.ws> ]*
#
#    $<B>=[<?before .>]
#    }
#
#  role herestop
#    {
#    token starter { <!> }
#    token stopper { ^^ {} $<ws>=(\h*) $*DELIM \h* $$ [\r\n | \v]? }
#    }
#
#  token cheat_heredoc
#    {
#    \h* <[ ; } ]> \h* <?before \n | '#'> <.ws> <?MARKER('endstmt')>
#    }
#
#  token quibble($l, *@base_tweaks)
#    {
#    <babble($l, @base_tweaks)>
#
#    $start <nibble($lang)> [ $stop || #`{ XXX There was code here } ]
#    }
#
#  token obsbrace
#    {
#    <.obs('curlies around escape argument','square brackets')>
#    }
#
#  token experimental($feature)
#    {
#    # XXX There was code here
#    || <.typed_panic('X::Experimental', :$feature)>
#    }
#
#  token RESTRICTED
#    {
#    [ <?{ $*RESTRICTED }> [ $ || <.security($*RESTRICTED)> ] ]?
#    <!>
#    }
#}

#grammar Perl6::Grammar is HLL::Grammar does STD {
#  #================================================================
#  # AMBIENT AND POD-COMMON CODE HANDLERS
#  #================================================================
#  method TOP()
#    {
#    # Language braid.
#    my $*LANG := self;
#    my $*LEAF := self;  # the leaf cursor, workaround for when we can't pass via $/ into world
#    self.define_slang('MAIN',    self.WHAT,             self.actions);
#    self.define_slang('Quote',   Perl6::QGrammar,       Perl6::QActions);
#    self.define_slang('Regex',   Perl6::RegexGrammar,   Perl6::RegexActions);
#    self.define_slang('P5Regex', Perl6::P5RegexGrammar, Perl6::P5RegexActions);
#    self.define_slang('Pod',     Perl6::PodGrammar,     Perl6::PodActions);
#
#    # Old language braid, going away eventually
#    # XXX TODO: if these are going out, be sure to make similar change
#    # to src/perl6-debug.nqp and ensure it still works.
#    my %*LANG;
#    %*LANG<Regex>           := Perl6::RegexGrammar;
#    %*LANG<Regex-actions>   := Perl6::RegexActions;
#    %*LANG<P5Regex>         := Perl6::P5RegexGrammar;
#    %*LANG<P5Regex-actions> := Perl6::P5RegexActions;
#    %*LANG<Quote>           := Perl6::QGrammar;
#    %*LANG<Quote-actions>   := Perl6::QActions;
#    %*LANG<MAIN>            := self.WHAT;
#    %*LANG<MAIN-actions>    := self.actions;
#
#    # We could start out TOP with a fatalizing language in self, conceivably...
#    my $*FATAL := self.pragma('fatal');  # also set if somebody calls 'use fatal' in mainline
#    self.set_pragma('worries', 1);
#
#    # A cacheable false dynvar value.
#    my $*WANTEDOUTERBLOCK := 0;
#
#    # Package declarator to meta-package mapping. Starts pretty much empty;
#    # we get the mappings either imported or supplied by the setting. One
#    # issue is that we may have no setting to provide them, e.g. when we
#    # compile the setting, but it still wants some kinda package. We just
#    # fudge in knowhow for that.
#    self.set_how('knowhow', nqp::knowhow());
#    self.set_how('package', nqp::knowhow());
#
#    # Will we use the result of this? (Yes for EVAL and REPL).
#    my $*NEED_RESULT := nqp::existskey(%*COMPILING<%?OPTIONS>, 'outer_ctx');
#
#    # Symbol table and serialization context builder - keeps track of
#    # objects that cross the compile-time/run-time boundary that are
#    # associated with this compilation unit.
#    my $file := nqp::getlexdyn('$?FILES');
#    my $source_id := nqp::sha1($file ~ (
#        nqp::defined(%*COMPILING<%?OPTIONS><outer_ctx>)
#            ?? self.target() ~ SerializationContextId.next-id()
#            !! self.target()));
#    my $outer_world := nqp::getlexdyn('$*W');
#    my $is_nested := (
#        $outer_world
#        && nqp::defined(%*COMPILING<%?OPTIONS><outer_ctx>)
#        && $outer_world.is_precompilation_mode()
#    );
#
#    my $*W := $is_nested
#        ?? $outer_world.create_nested()
#        !! nqp::isnull($file)
#            ?? Perl6::World.new(:handle($source_id))
#            !! Perl6::World.new(:handle($source_id), :description($file));
#
#    unless $is_nested {
#        $*W.add_initializations();
#    }
#
#    my $cursor := self.comp_unit;
#    $*W.pop_lexpad(); # UNIT
#    $*W.pop_lexpad(); # UNIT_OUTER
#    $cursor;
#    }
#
#    ## Lexer stuff
#
#  token apostrophe
#    {
#    <[ ' \- ]>
#    }
#
#  token identifier
#    {
#    <.ident> [ <.apostrophe> <.ident> ]*
#    }
#
#  token name
#    {
#      [
#      | <identifier> <morename>*
#      | <morename>+
#      ]
#    }
#
#  token morename
#    {
#    '::'
#      [
#      ||  <?before '(' | <.alpha> >
#          [
#          | <identifier>
#          | '(' ~ ')' [ <.ws> <EXPR> ]
#          ]
#      || <?before '::'> <.typed_panic: "X::Syntax::Name::Null">
#      || $<bad>=[<.sigil><.identifier>] # XXX There was code here
#      ]?
#    }
#
#  token longname
#    {
#    <name> {}
#    [ <?before ':' <.+alpha+[\< \[ \« ]>> <!RESTRICTED> <colonpair> ]*
#    }
#
#  token deflongname
#    {
#    <name> <colonpair>*
#    }
#
#  token subshortname
#    {
#    <desigilname>
#    }
#
#  token sublongname
#    {
#    <subshortname> <sigterm>?
#    }
#
#  token deftermnow
#    {
#    <defterm>
#    }
#
#  token defterm
#    {     # XXX this is probably too general
#    <identifier>
#      [
#      | <colonpair>+
#      | <?>
#      ]
#    }
#
#  token module_name
#    {
#    <longname>
#    [ <?[[]> '[' ~ ']' <arglist> ]?
#    }
#
#  token end_keyword
#    {
#    » <!before <.[ \( \\ ' \- ]> || \h* '=>'>
#    }
#  token end_prefix
#    {
#    <.end_keyword> \s*
#    }
#
#  token spacey
#    {
#    <?[\s#]>
#    }
#
#  token kok
#    {
#    <.end_keyword>
#      [
#      || <?before <.[ \s \# ]> > <.ws>
#      || # XXX Code was here
#      ]
#    }
#
#  token tok
#    {
#    <.end_keyword>
#    }
#
#  token ENDSTMT
#    {
#      [
#      | \h* $$ <.ws> <?MARKER('endstmt')>
#      | <.unv>? $$ <.ws> <?MARKER('endstmt')>
#      ]?
#    }
#
#    # ws is highly performance sensitive. So, we check if we already marked it
#    # at this point with a simple method, and only if that is not the case do
#    # we bother doing any pattern matching.
#  token _ws
#    {
#    <!ww>
#      [
#      | [\r\n || \v] <.heredoc>
#      | <.unv>
#      | <.unsp>
#      ]*
#    <?MARKER('ws')>
#    }
#
#  token unsp
#    {
#    \\ <?before \s | '#'>
#      [
#      | <.vws>
#      | <.unv>
#      | <.unsp>
#      ]*
#    }
#
#    token vws {
#        [
#            [
#            | \v
#            | '<<<<<<<' {} <?before [.*? \v '=======']: .*? \v '>>>>>>>' > <.sorry: 'Found a version control conflict marker'> \V* \v
#            | '=======' {} .*? \v '>>>>>>>' \V* \v   # ignore second half
#            ]
#        ]+
#    }
#
#    token unv {
#        [
#        | \h+
#        | \h* <.comment>
#        | <?before \h* '=' [ \w | '\\'] > ^^ <.pod_content_toplevel>
#        ]
#    }
#
#    token install_doc_phaser { <?> }
#
#    token vnum {
#        \w+ | '*'
#    }
#
#    token version {
#        <?before v\d+\w*> 'v' $<vstr>=[<vnum>+ % '.' '+'?]
#        <!before '-'|\'> # cheat because of LTM fail
#    }
#
#    ## Top-level rules
#
#    token comp_unit {
#        # From STD.pm.
#        # XXX There was code here
#
#        <.bom>?
#        <lang-version>
#        <.finishpad>
#        <statementlist=.FOREIGN_LANG($*MAIN, 'statementlist', 1)>
#
#        <.install_doc_phaser>
#
#        [ $ || <.typed_panic: 'X::Syntax::Confused'> ]
#
#        <.explain_mystery>
#        <.cry_sorrows>
#        # THere was code here
#    }
#
#    rule lang-version {
#        [
#          <.ws>? 'use' <version> {} # <-- update $/ so we can grab $<version>
#          # we parse out the numeral, since we could have "6d"
#          [
#          ||  <?{ $version == 6 }> { $*W.load-lang-ver: $<version>, $comp }
#          ||  { $/.typed_panic: 'X::Language::Unsupported',
#                  version => ~$<version> }
#          ]
#        || # XXX There was code here
#        ]
#    }
#
#    rule statementlist($*statement_level = 0) {
#        <.ws>
#        # Define this scope to be a new language.
#        <!!{ $*LANG := $*LEAF := $/.clone_braid_from(self); 1 }>
#        [
#        | $
#        | <?before <.[\)\]\}]>>
#        | [ <statement> <.eat_terminator> ]*
#        ]
#        <.set_braid_from(self)>   # any language tweaks must not escape
#        <!!{ nqp::rebless($/, self.WHAT); 1 }>
#    }
#
#    rule semilist {
#        ''
#        [
#        | <?before <.[)\]}]> >
#        | [<statement><.eat_terminator> ]*
#        ]
#    }
#
#    rule sequence {
#        ''
#        [
#        | <?before <.[)\]}]> >
#        | [<statement><.eat_terminator> ]*
#        ]
#    }
#
#    token label {
#        <identifier> ':' <?[\s]> <.ws>
#        # XXX There was code here
#    }
#
#    token statement($*LABEL = '') {
#
#        # NOTE: annotations that use STATEMENT_ID often also need IN_STMT_MOD annotation, in order
#        # to correctly migrate QAST::Blocks in constructs inside topics of statement modifiers
#
#        <!!{ $/.set_actions($actions); 1 }>
#        <!before <.[\])}]> | $ >
#        <!stopper>
#        <!!{ nqp::rebless($/, self.slang_grammar('MAIN')); 1 }>
#        [
#        | <label> <statement($*LABEL)> { $*LABEL := '' if $*LABEL }
#        | <statement_control>
#        | <EXPR>
#            [
#            || <?MARKED('endstmt')>
#            || <.ws> <statement_mod_cond> <statement_mod_loop>?
#            || <.ws> <statement_mod_loop>
#               # XXX There was code here
#            ]?
#        | <?[;]>
#        | <?stopper>
#        | {} <.panic: "Bogus statement">
#        ]
#    }
#
#    token eat_terminator {
#        || ';'
#        || <?MARKED('endstmt')> <.ws>
#        || <?before ')' | ']' | '}' >
#        || $
#        || <?stopper>
#        || <?before [if|while|for|loop|repeat|given|when] » > { $/.'!clear_highwater'(); self.typed_panic( 'X::Syntax::Confused', reason => "Missing semicolon" ) }
#        || { $/.typed_panic( 'X::Syntax::Confused', reason => "Confused" ) }
#    }
#
#    # Options for xblock/block implicit topic.
#    my $PBLOCK_NO_TOPIC := 0;
#    my $PBLOCK_OPTIONAL_TOPIC := 1;
#    my $PBLOCK_REQUIRED_TOPIC := 2;
#
#    token xblock($*IMPLICIT = $PBLOCK_NO_TOPIC) {
#        <EXPR> <.ws> <pblock($*IMPLICIT)>
#    }
#
#    token pblock($*IMPLICIT = $PBLOCK_NO_TOPIC) {
#        # XXX There was code here
#        <.attach_leading_docs>
#        # XXX There was code here
#        [
#        | <lambda>
#            <.newpad>
#            <blockoid> # XXX There was code here
#        | <?[{]>
#            <.newpad>
#            <blockoid>
#        || <.missing_block($borg, $has_mystery)>
#        ]
#    }
#
#    token lambda { '->' | '<->' }
#
#    token block($*IMPLICIT = 0) {
#        # XXX There was code here
#        [ <?[{]> || <.missing_block($borg, $has_mystery)>]
#        <.newpad>
#        <blockoid>
#    }
#
#    token blockoid {
#        <.finishpad>
#        # XXX There was code here
#        [
#        | '{YOU_ARE_HERE}' <you_are_here>
#        |   '{'
#            <!!{ $*VARIABLE := '' if $*VARIABLE; 1 }>
#            <statementlist(1)>
#            [<.cheat_heredoc> || '}']
#            <?ENDSTMT>
#        || <.missing_block($borg, $has_mystery)>
#        ]
#        # XXX There was code here
#    }
#
#    token unitstart { <?> }
#    token you_are_here {
#        <?{ nqp::getlexdyn('$?FILES') ~~ /\.setting$/ }> ||
#        # XXX There was code here
#    }
#    token newpad { <?> { $*W.push_lexpad($/) } }
#    token newthunk { <?> { $*W.push_thunk($/) } }
#    token finishpad { <?> }
#
#    token bom { \xFEFF }
#
#    proto token terminator { <...> }
#
#    token terminator:sym<;> { <?[;]> }
#    token terminator:sym<)> { <?[)]> }
#    token terminator:sym<]> { <?[\]]> }
#    token terminator:sym<}> { <?[}]> }
#    token terminator:sym<ang> { <?[>]> <?{ $*IN_REGEX_ASSERTION }> }
#    token terminator:sym<if>     { 'if'     <.kok> }
#    token terminator:sym<unless> { 'unless' <.kok> }
#    token terminator:sym<while>  { 'while'  <.kok> }
#    token terminator:sym<until>  { 'until'  <.kok> }
#    token terminator:sym<for>    { 'for'    <.kok> }
#    token terminator:sym<given>  { 'given'  <.kok> }
#    token terminator:sym<when>   { 'when'   <.kok> }
#    token terminator:sym<with>   { 'with'   <.kok> }
#    token terminator:sym<without> { 'without' <.kok> }
#    token terminator:sym<arrow>  { '-->' }
#
#    token stdstopper {
#        [
#        || <?MARKED('endstmt')> <?>
#        || [
#           | <?terminator>
#           | $
#           ]
#       ]
#    }
#
#    ## Statement control
#
#    proto rule statement_control { <...> }
#
#    rule statement_control:sym<if> {
#        $<sym>=[if|with]<.kok> {}
#        <xblock(~$<sym>[0] ~~ /with/ ?? $PBLOCK_REQUIRED_TOPIC !! $PBLOCK_NO_TOPIC)>
#        [
#            [
#            | 'else'\h*'if' <.typed_panic: 'X::Syntax::Malformed::Elsif'>
#            | 'elif' { $/.typed_panic('X::Syntax::Malformed::Elsif', what => "elif") }
#            | $<sym>='elsif' <xblock>
#            | $<sym>='orwith' <xblock($PBLOCK_REQUIRED_TOPIC)>
#            ]
#        ]*
#        {}
#        [
#            'else'
#            <else=.pblock(~$<sym>[-1] ~~ /with/ ?? $PBLOCK_REQUIRED_TOPIC !! $PBLOCK_NO_TOPIC)>
#        ]?
#    }
#
#    rule statement_control:sym<unless> {
#        $<sym>='unless'<.kok>
#        <xblock($PBLOCK_NO_TOPIC)> # 0 means we're not parsing `without`
#        [ <!before [els[e|if]|orwith]» >
#            || $<wrong-keyword>=[els[e|if]|orwith]» {}
#                <.typed_panic: 'X::Syntax::UnlessElse',
#                    keyword => ~$<wrong-keyword>,
#                >
#        ]
#    }
#
#    rule statement_control:sym<without> {
#        $<sym>='without'<.kok>
#        <xblock($PBLOCK_REQUIRED_TOPIC)> # 1 means we're not parsing `unless`
#        [ <!before [els[e|if]|orwith]» >
#            || $<wrong-keyword>=[els[e|if]|orwith]» {}
#                <.typed_panic: 'X::Syntax::WithoutElse',
#                    keyword => ~$<wrong-keyword>,
#                >
#        ]
#    }
#
#    rule statement_control:sym<while> {
#        $<sym>=[while|until]<.kok> {}
#        <xblock>
#    }
#
#    rule statement_control:sym<repeat> {
#        <sym><.kok> {}
#        [
#        | $<wu>=[while|until]<.kok> <xblock>
#        | <pblock>
#          [$<wu>=['while'|'until']<.kok> || <.missing('"while" or "until"')>]
#          <EXPR>
#        ]
#    }
#
#    rule statement_control:sym<for> {
#        <sym><.kok> {}
#        [ <?before 'my'? '$'\w+\s+'(' >
#            <.typed_panic: 'X::Syntax::P5'> ]?
#        [ <?before '(' <.EXPR>? ';' <.EXPR>? ';' <.EXPR>? ')' >
#            <.obs('C-style "for (;;)" loop', '"loop (;;)"')> ]?
#        <xblock($PBLOCK_REQUIRED_TOPIC)>
#    }
#
#    rule statement_control:sym<whenever> {
#        <sym><.kok>
#        [
#        || # XXX There was code here
#        || <.typed_panic('X::Comp::WheneverOutOfScope')>
#        ]
#        # XXX There was code here
#        <xblock($PBLOCK_REQUIRED_TOPIC)>
#    }
#
#    rule statement_control:sym<foreach> {
#        <sym><.end_keyword> <.obs("'foreach'", "'for'")>
#    }
#
#    token statement_control:sym<loop> {
#        <sym><.kok>
#        :s''
#        [ '('
#            [
#            <e1=.EXPR>? ';' <e2=.EXPR>? ';' <e3=.EXPR>?
#            || <.malformed('loop spec')>
#            ]
#        ')' ]?
#        <block>
#    }
#
#    rule statement_control:sym<need> {
#        <sym>
#        [
#        | <version> <.sorry('In case of using pragma, use "use" instead (e.g., "use v6;", "use v6.c;").')>
#        | <module_name>
#        ]+ % ','
#        # XXX There was code here        
#    }
#
#    token statement_control:sym<import> {
#        <sym> <.ws>
#        <module_name> [ <.spacey> <arglist> ]? <.ws>
#        # XXX There was code here
#    }
#
#    token statement_control:sym<no> {
#        <sym> <.ws>
#        [
#        | <module_name> [ <.spacey> <arglist> ]? <.explain_mystery> <.cry_sorrows>
#            { $*W.do_pragma_or_load_module($/,0) }
#        ]
#        <.ws>
#    }
#
#    token statement_control:sym<use> {
#        $<doc>=[ 'DOC' \h+ ]**0..1
#        <sym> <.ws>
#        [
#        | <version>
#            { $/.typed_panic: 'X::Language::TooLate', version => ~$<version> }
#        | <module_name>
#            [
#            || <.spacey> <arglist> <.cheat_heredoc>? <?{ $<arglist><EXPR> }> <.explain_mystery> <.cry_sorrows>
#                # XXX There was code here
#            || # XXX There was code here
#            ]
#        ]
#        [ # XXX There was code here
#          <.eat_terminator>
#          <statementlist=.FOREIGN_LANG($*MAIN, 'statementlist', 1)>
#        || <?> ]
#        <.ws>
#    }
#
#    rule statement_control:sym<require> {
#        <sym>
#        [
#        | <module_name>
#        | <file=.variable>
#        | <!sigil> <file=.term>
#        ]
#        <EXPR>?
#    }
#
#    rule statement_control:sym<given> {
#        <sym><.kok> <xblock($PBLOCK_REQUIRED_TOPIC)>
#    }
#    rule statement_control:sym<when> {
#        <sym><.kok> <xblock>
#    }
#    rule statement_control:sym<default> {
#        <sym><.kok> <block>
#    }
#
#    rule statement_control:sym<CATCH> {<sym> <block(1)> }
#    rule statement_control:sym<CONTROL> {<sym> <block(1)> }
#    rule statement_control:sym<QUIT> {<sym> <block(1)> }
#
#    proto token statement_prefix { <...> }
#    token statement_prefix:sym<BEGIN>   { <sym><.kok> <blorst> <.explain_mystery> <.cry_sorrows> }
#    token statement_prefix:sym<COMPOSE> { <sym><.kok> <blorst> }
#    token statement_prefix:sym<TEMP>    { <sym><.kok> <blorst> }
#    token statement_prefix:sym<CHECK>   { <sym><.kok> <blorst> }
#    token statement_prefix:sym<INIT>    { <sym><.kok> <blorst> }
#    token statement_prefix:sym<ENTER>   { <sym><.kok> <blorst> }
#    token statement_prefix:sym<FIRST>   { <sym><.kok> <blorst> }
#
#    token statement_prefix:sym<END>   { <sym><.kok> <blorst> }
#    token statement_prefix:sym<LEAVE> { <sym><.kok> <blorst> }
#    token statement_prefix:sym<KEEP>  { <sym><.kok> <blorst> }
#    token statement_prefix:sym<UNDO>  { <sym><.kok> <blorst> }
#    token statement_prefix:sym<NEXT>  { <sym><.kok> <blorst> }
#    token statement_prefix:sym<LAST>  { <sym><.kok> <blorst> }
#    token statement_prefix:sym<PRE>   { <sym><.kok> <blorst> }
#    token statement_prefix:sym<POST>  { <sym><.kok> <blorst> }
#    token statement_prefix:sym<CLOSE> { <sym><.kok> <blorst> }
#
#    token statement_prefix:sym<race> {
#        <sym><.kok>
#        [
#        | <?before 'for' <.kok>> <for=.statement_control>
#        | <blorst>
#        ]
#    }
#    token statement_prefix:sym<hyper> {
#        <sym><.kok>
#        [
#        | <?before 'for' <.kok>> <for=.statement_control>
#        | <blorst>
#        ]
#    }
#    token statement_prefix:sym<lazy> {
#        <sym><.kok>
#        [
#        | <?before 'for' <.kok>> <for=.statement_control>
#        | <blorst>
#        ]
#    }
#    token statement_prefix:sym<eager>   { <sym><.kok> <blorst> }
#    token statement_prefix:sym<sink>    { <sym><.kok> <blorst> }
#    token statement_prefix:sym<try>     {
#        <!!{ $/.clone_braid_from(self).set_pragma('fatal',1); }>
#        <sym><.kok> <blorst>
#        <.set_braid_from(self)>
#
#    }
#    token statement_prefix:sym<quietly> { <sym><.kok> <blorst> }
#    token statement_prefix:sym<gather>  { <sym><.kok> <blorst> }
#    token statement_prefix:sym<once>    { <sym><.kok> <blorst> }
#    token statement_prefix:sym<start>   { <sym><.kok> <blorst> }
#    token statement_prefix:sym<supply>  {
#        <sym><.kok> <blorst>
#    }
#    token statement_prefix:sym<react>   {
#        <sym><.kok> <blorst>
#    }
#    token statement_prefix:sym<do>      { <sym><.kok> <blorst> }
#    token statement_prefix:sym<DOC>     {
#        <sym><.kok> $<phase>=['BEGIN' || 'CHECK' || 'INIT']<.end_keyword><.ws>
#        <blorst>
#    }
#
#    token blorst {
#        [ <?[{]> <block> | <![;]> <statement> <.cheat_heredoc>? || <.missing: 'block or statement'> ]
#    }
#
#    ## Statement modifiers
#
#    proto rule statement_mod_cond { <...> }
#
#    token modifier_expr($k) { <EXPR> || <.nomodexpr($k)> }
#    token smexpr($k)        { <EXPR> || <.nomodexpr($k)> }
#
#    rule statement_mod_cond:sym<if>     { <sym><.kok> <modifier_expr('if')> }
#    rule statement_mod_cond:sym<unless> { <sym><.kok> <modifier_expr('unless')> }
#    rule statement_mod_cond:sym<when>   { <sym><.kok> <modifier_expr('when')> }
#    rule statement_mod_cond:sym<with>   { <sym><.kok> <modifier_expr('with')> }
#    rule statement_mod_cond:sym<without>{ <sym><.kok> <modifier_expr('without')> }
#
#    proto rule statement_mod_loop { <...> }
#
#    rule statement_mod_loop:sym<while> { <sym><.kok> <smexpr('while')> }
#    rule statement_mod_loop:sym<until> { <sym><.kok> <smexpr('until')> }
#    rule statement_mod_loop:sym<for>   { <sym><.kok> <smexpr('for')> }
#    rule statement_mod_loop:sym<given> { <sym><.kok> <smexpr('given')> }
#
#    ## Terms
#
#    token term:sym<fatarrow>           { <fatarrow> }
#    token term:sym<colonpair>          { <colonpair> }
#    token term:sym<variable>           { <variable> { $*VAR := $<variable> unless $*VAR; } }  # maybe desigilname already set it
#    token term:sym<package_declarator> { <package_declarator> }
#    token term:sym<scope_declarator>   { <scope_declarator> }
#    token term:sym<routine_declarator> { <routine_declarator> }
#    token term:sym<multi_declarator>   { <?before 'multi'|'proto'|'only'> <multi_declarator> }
#    token term:sym<regex_declarator>   { <regex_declarator> }
#    token term:sym<circumfix>          { <circumfix> }
#    token term:sym<statement_prefix>   { <statement_prefix> }
#    token term:sym<**>                 { <sym> }
#    token term:sym<*>                  { <sym> }
#    token term:sym<lambda>             { <?lambda> <pblock> {$*BORG<block> := $<pblock> } }
#    token term:sym<type_declarator>    { <type_declarator> }
#    token term:sym<value>              { <value> }
#    token term:sym<unquote>            { '{{{' <?{ $*IN_QUASI }> <statementlist> '}}}' }
#    token term:sym<!!>                 { '!!' <?before \s> }  # actual error produced inside infix:<?? !!>
#
#    token term:sym<::?IDENT> {
#        $<sym> = [ '::?' <identifier> ] »
#    }
#    token term:sym<p5end> {
#        << __END__ >>
#        <.obs('__END__ as end of code',
#          'the =finish pod marker and $=finish to read')>
#    }
#    token term:sym<p5data> {
#        << __DATA__ >>
#        <.obs('__DATA__ as start of data',
#          'the =finish pod marker and $=finish to read')>
#    }
#
#    token infix:sym<lambda> {
#        <?before '{' | <.lambda> > <!{ $*IN_META }> # XXX There was code here
#        [
#        || <!{ $*IN_REDUCE }> # XXX There was code here
#        || <!>
#        ]
#    }
#
#    token term:sym<undef> {
#        <!{ $*LANG.pragma('p5isms') }>
#        <sym> >> {}
#        [ <?before \h*'$/' >
#            <.obs('$/ variable as input record separator',
#                 "the filehandle's .slurp method")>
#        ]?
#        [ <?before [ '(' || \h*<.sigil><.twigil>?\w ] >
#            <.obs('undef as a verb', 'undefine() or assignment of Nil')>
#        ]?
#        <.obs('undef as a value', "something more specific:\n\tan undefined type object such as Any or Int,\n\t:!defined as a matcher,\n\tAny:U as a type constraint,\n\tNil as the absence of an expected value\n\tor fail() as a failure return\n\t   ")>
#    }
#
#    token term:sym<new> {
#        <!{ $*LANG.pragma('c++isms') }>
#        'new' \h+ <longname> \h* <![:]> <.obs("C++ constructor syntax", "method call syntax", :ism<c++isms>)>
#    }
#
#    token fatarrow {
#        <key=.identifier> \h* '=>' <.ws> <val=.EXPR('i<=')>
#    }
#
#    token coloncircumfix($front) {
#        # reset $*IN_DECL in case this colonpair is part of var we're
#        # declaring, since colonpair might have other vars. Don't make those
#        # think we're declaring them
#        [
#        | '<>' <.worry("Pair with <> really means an empty list, not null string; use :$front" ~ "('') to represent the null string,\n  or :$front" ~ "() to represent the empty list more accurately")>
#        | {} <circumfix>
#        ]
#    }
#
#    token colonpair {
#        ':'
#        [
#        | '!' [ <identifier> || <.panic: "Malformed False pair; expected identifier"> ]
#            [ <[ \[ \( \< \{ ]> # XXX There was code here
#            ]?
#            # XXX There was code here
#        | $<num> = [\d+] <identifier> [ <?before <.[ \[ \( \< \{ ]>> {} <.sorry("Extra argument not allowed; pair already has argument of " ~ $<num>.Str)> <.circumfix> ]?
#            # XXX There was code here
#            # XXX There was code here
#        | <identifier>
#            # XXX There was code here
#            [
#            || <.unsp>? <coloncircumfix($*key)> # XXX There was code here
#            || # XXX There was code here
#            ]
#        | '(' ~ ')' <fakesignature>
#        | <coloncircumfix('')>
#            # XXX There was code here
#        | <var=.colonpair_variable>
#            # XXX There was code here
#        ]
#    }
#
#    token colonpair_variable {
#        <sigil> {}
#        [
#        | <twigil>? <desigilname>
#        | $<capvar>='<' <desigilname> '>'
#        ]
#    }
#
#    proto token special_variable { <...> }
#
#    token special_variable:sym<$!{ }> {
#        [ '$!{' .*? '}' | '%!' ]
#        <.obsvar('%!')>
#    }
#
#    token special_variable:sym<$`> {
#        <sym>  <?before \s | ',' | <.terminator> >
#        <.obsvar('$`')>
#    }
#
#    token special_variable:sym<$@> {
#        <sym> <[ \s ; , ) ]> .
#        <.obsvar('$@')>
#    }
#
#    token special_variable:sym<$#> {
#        <sym> <identifier>
#        {}
#        <.obsvar('$#', ~$<identifier>)>
#    }
#
#    token special_variable:sym<$$> {
#        <sym> \W
#        <.obsvar('$$')>
#    }
#
#    token special_variable:sym<$&> {
#        <sym> <?before \s | ',' | <.terminator> >
#        <.obsvar('$&')>
#    }
#
#    token special_variable:sym<@+> {
#        <sym> <?before \s | ',' | <.terminator> >
#        <.obsvar('@+')>
#    }
#
#    token special_variable:sym<%+> {
#        <sym> <?before \s | ',' | <.terminator> >
#        <.obsvar('%+')>
#    }
#
#    token special_variable:sym<$+[ ]> {
#        '$+['
#        <.obsvar('@+')>
#    }
#
#    token special_variable:sym<@+[ ]> {
#        '@+['
#        <.obsvar('@+')>
#    }
#
#    token special_variable:sym<@+{ }> {
#        '@+{'
#        <.obsvar('%+')>
#    }
#
#    token special_variable:sym<@-> {
#        <sym> <?before \s | ',' | <.terminator> >
#        <.obsvar('@-')>
#    }
#
#    token special_variable:sym<%-> {
#        <sym> <?before \s | ',' | <.terminator> >
#        <.obsvar('%-')>
#    }
#
#    token special_variable:sym<$-[ ]> {
#        '$-['
#        <.obsvar('@-')>
#    }
#
#    token special_variable:sym<@-[ ]> {
#        '@-['
#        <.obsvar('@-')>
#    }
#
#    token special_variable:sym<%-{ }> {
#        '@-{'
#        <.obsvar('%-')>
#    }
#
#    token special_variable:sym<$/> {
#        <sym> <?before \h* '=' \h* <.[ ' " ]> >
#        <.obsvar('$/')>
#    }
#
#    token special_variable:sym<$\\> {
#        '$\\' <?before \s | ',' | '=' | <.terminator> >
#        <.obsvar('$\\')>
#    }
#
#    token special_variable:sym<$|> {
#        <sym> <?before \h* '='>
#        <.obsvar('$|')>
#    }
#
#    token special_variable:sym<$;> {
#        <sym> <?before \h* '='>
#        <.obsvar('$;')>
#    }
#
#    token special_variable:sym<$'> { #'
#        <sym> <?before \s | ',' | <.terminator> >
#        <.obsvar('$' ~ "'")>
#    }
#
#    token special_variable:sym<$"> {
#        <sym> <?before \h* '='>
#        <.obsvar('$"')>
#    }
#
#    token special_variable:sym<$,> {
#        <sym> <?before \h* '='>
#        <.obsvar('$,')>
#    }
#
#    token special_variable:sym<$.> {
#        <sym> {} <!before \w | '(' | '^' >
#        <.obsvar('$.')>
#    }
#
#    token special_variable:sym<$?> {
#        <sym> {} <!before \w | '('>
#        <.obsvar('$?')>
#    }
#
#    token special_variable:sym<$]> {
#        <sym> {} <!before \w | '('>
#        <.obsvar('$]')>
#    }
#
#    regex special_variable:sym<${ }> {
#        <sigil> '{' {} $<text>=[.*?] '}'
#        <!{ $*IN_DECL }>
#        <!{ $<text> ~~ / '=>' || ':'<:alpha> || '|%' / }>
#        <!{ $<text> ~~ / ^ \s* $ / }>
#        # XXX There was code here
#    }
#
#    token desigilname {
#        [
#        | <?before <.sigil> <.sigil> > <variable>
#        | <?sigil>
#            [ <?{ $*IN_DECL }> <.typed_panic: 'X::Syntax::Variable::IndirectDeclaration'> ]?
#            <variable> # XXX There was code here
#        | <longname>
#        ]
#    }
#
#    token desigilmetaname {
#        $<longname>=( $<name>=( <identifier>  ) )
#    }
#
#    token variable {
#        [
#        | '&[' ~ ']' <infixish('[]')>
#        | <sigil> [ $<twigil>=['.^'] <desigilname=desigilmetaname> | <twigil>? <desigilname> ]
#          [ # XXX There was code here
#            { self.typed_panic: 'X::Syntax::Variable::Initializer', name => $*VARIABLE } ]?
#        | <special_variable>
#        | <sigil> $<index>=[\d+]                              [<?{ $*IN_DECL }> <.typed_panic: "X::Syntax::Variable::Numeric">]?
#        | <sigil> <?[<]> <postcircumfix>                      [<?{ $*IN_DECL }> <.typed_panic('X::Syntax::Variable::Match')>]?
#        | <?before <.sigil> <.?[ ( [ { ]>> <!RESTRICTED> <?{ !$*IN_DECL }> <contextualizer>
#        | $<sigil>=['$'] $<desigilname>=[<[/_!¢]>]
#        | {} <sigil> <!{ $*QSIGIL }> <?MARKER('baresigil')>   # try last, to allow sublanguages to redefine sigils (like & in regex)
#        ]
#        [ # XXX There was code here
#            [ <.unsp> | '\\' | <?> ] <?[(]> <!RESTRICTED> <arglist=.postcircumfix>
#        ]?
#        # XXX There was code here
#    }
#
#    token contextualizer {
#        [ <?{ $*IN_DECL }> <.panic: "Cannot declare a contextualizer"> ]?
#        [
#        | <sigil> '(' ~ ')'    <coercee=sequence>
#        | <sigil> <?[ \[ \{ ]> <coercee=circumfix>
#        ]
#    }
#
#    token sigil { <[$@%&]> }
#
#    proto token twigil { <...> }
#    token twigil:sym<.> { <sym> <?before \w> }
#    token twigil:sym<!> { <sym> <?before \w> }
#    token twigil:sym<^> { <sym> <?before \w> }
#    token twigil:sym<:> { <sym> <?before \w> }
#    token twigil:sym<*> { <sym> <?before \w> }
#    token twigil:sym<?> { <sym> <?before \w> }
#    token twigil:sym<=> { <sym> <?before \w> }
#    token twigil:sym<~> { <sym> <?before \w> }
#
#    proto token package_declarator { <...> }
#    token package_declarator:sym<package> {
#        <sym><.kok> <package_def>
#        <.set_braid_from(self)>
#    }
#    token package_declarator:sym<module> {
#        <sym><.kok> <package_def>
#        <.set_braid_from(self)>
#    }
#    token package_declarator:sym<class> {
#        <sym><.kok> <package_def>
#        <.set_braid_from(self)>
#    }
#    token package_declarator:sym<grammar> {
#        <sym><.kok> <package_def>
#        <.set_braid_from(self)>
#    }
#    token package_declarator:sym<role> {
#        <sym><.kok> <package_def>
#        <.set_braid_from(self)>
#    }
#    token package_declarator:sym<knowhow> {
#        <sym><.kok> <package_def>
#        <.set_braid_from(self)>
#    }
#    token package_declarator:sym<native> {
#        <sym><.kok> <package_def>
#        <.set_braid_from(self)>
#    }
#    token package_declarator:sym<slang> {
#        <sym><.kok> <package_def>
#        <.set_braid_from(self)>
#    }
#    token package_declarator:sym<trusts> {
#        <sym><.kok> [ <typename> || <.typo_typename(1)> ]
#    }
#    rule package_declarator:sym<also> {
#        <sym><.kok>
#        [ <trait>+ || <.panic: "No valid trait found after also"> ]
#    }
#
#    rule package_def {
#        # XXX There was code here
#        <.attach_leading_docs>
#
#        # Type-object will live in here; also set default REPR (a trait
#        # may override this, e.g. is repr('...')).
#
#        # Default to our scoped.
#        # XXX There was code here
#
#        <!!{ $/.clone_braid_from(self) }>
#        [
#            [ <longname> { $longname := $*W.dissect_longname($<longname>); } ]?
#            <.newpad>
#
#            [   # XXX There was code here
#                '[' ~ ']' <signature>
#                { $*IN_DECL := ''; }
#            ]?
#
#            <trait>*
#            # XXX There was code here
#            :!s
#            # XXX There was code here
#            [
#            || <?[{]>
#                [
#                # XXX There was code here
#                <blockoid>
#                ]
#
#            || ';'
#                [
#                || # XXX There was code here
#                    <.finishpad>
#                    <statementlist(1)>     # whole rest of file, presumably
#                    # XXX There was code here
#                || # XXX There was code here
#                ]
#            || <.panic("Unable to parse $*PKGDECL definition")>
#            ]
#            # XXX There was code here
#        ]:!s || # XXX There was code here
#    }
#
#    token declarator {
#        [
#        # STD.pm6 uses <defterm> here, but we need different
#        # action methods
#        | '\\' <deftermnow>
#            [ <.ws> <term_init=initializer> || <.typed_panic: "X::Syntax::Term::MissingInitializer"> ]
#        | <variable_declarator>
#          [
#          || <?{ $*SCOPE eq 'has' }> <.newpad> [<.ws> <initializer>]? { $*ATTR_INIT_BLOCK := $*W.pop_lexpad() }
#          || [<.ws> <initializer>]?
#          ]
#        | '(' ~ ')' <signature('variable')> [ <.ws> <trait>+ ]? [ <.ws> <initializer> ]?
#        | <routine_declarator>
#        | <regex_declarator>
#        | <type_declarator>
#        ]
#    }
#
#    proto token multi_declarator { <...> }
#    token multi_declarator:sym<multi> {
#        <sym><.kok> 
#        [ <?before '('> { $*W.throw($/, 'X::Anon::Multi', multiness => $*MULTINESS) } ]?
#        [ <declarator> || <routine_def('sub')> || <.malformed('multi')> ]
#    }
#    token multi_declarator:sym<proto> {
#        <sym><.kok> 
#        [ <?before '('> { $*W.throw($/, 'X::Anon::Multi', multiness => $*MULTINESS) } ]?
#        [ <declarator> || <routine_def('sub')> || <.malformed('proto')> ]
#    }
#    token multi_declarator:sym<only> {
#        <sym><.kok> 
#        [ <declarator> || <routine_def('sub')> || <.malformed('only')>]
#    }
#    token multi_declarator:sym<null> {
#        <declarator>
#    }
#
#    proto token scope_declarator { <...> }
#    token scope_declarator:sym<my>        { <sym> <scoped('my')> }
#    token scope_declarator:sym<our>       { <sym> <scoped('our')> }
#    token scope_declarator:sym<has>       {
#        <sym>
#        <scoped('has')>
#    }
#    token scope_declarator:sym<HAS>       {
#        <sym>
#        <scoped('has')>
#    }
#    token scope_declarator:sym<augment>   { <sym> <scoped('augment')> }
#    token scope_declarator:sym<anon>      { <sym> <scoped('anon')> }
#    token scope_declarator:sym<state>     { <sym> <scoped('state')> }
#    token scope_declarator:sym<supersede> {
#        <sym> <scoped('supersede')> <.NYI('"supersede"')>
#    }
#    token scope_declarator:sym<unit>      { <sym> <scoped('unit')> }
#
#    token scoped($*SCOPE) {
#        <.end_keyword>
#        [
#        # XXX There was code here
#        <.ws>
#        [
#        | <DECL=declarator>
#        | <DECL=regex_declarator>
#        | <DECL=package_declarator>
#        | [<typename><.ws>]+
#          # XXX There was code here
#          <DECL=multi_declarator>
#        | <DECL=multi_declarator>
#        ]
#        || <.ws>[<typename><.ws>]* <ident>
#           <?before <.ws>
#           [
#           | ':'?':'?'='
#           | <.terminator>
#           | <trait>
#           | "where" <.ws> <EXPR>
#           | $
#           ]
#           > {} <.malformed("$*SCOPE (did you mean to declare a sigilless \\{~$<ident>} or \${~$<ident>}?)")>
#        || <.ws><typename><.ws> <?before "where" <.ws> <EXPR>> {}
#            <.malformed("$*SCOPE (found type followed by constraint; did you forget a variable in between?)")>
#        || <.ws><typename><.ws> <?before <trait>> {}
#            <.malformed("$*SCOPE (found type followed by trait; did you forget a variable in between?)")>
#        || <.ws><typename><.ws> <?before [ <.terminator> | $ ]> {}
#            <.malformed("$*SCOPE (did you forget a variable after type?)")>
#        || <.ws><!typename> <typo_typename> <!>
#        || <.malformed($*SCOPE)>
#        ]
#    }
#
#    token variable_declarator {
#        <variable>
#        # XXX There was code here
#        [
#            <.unsp>?
#            $<shape>=[
#            | '(' ~ ')' <signature> # XXX There was code here
#            | '[' ~ ']' <semilist> # XXX There was code here
#            | '{' ~ '}' <semilist> # XXX There was code here
#            | <?[<]> <postcircumfix> <.NYI: "Shaped variable declarations">
#            ]+
#        ]?
#
#        [ <.ws> <trait>+ ]?
#        [ <.ws> <post_constraint('var')>+ ]?
#    }
#
#    proto token routine_declarator { <...> }
#    token routine_declarator:sym<sub> {
#        <sym> <.end_keyword> <routine_def('sub')>
#    }
#    token routine_declarator:sym<method> {
#        <sym> <.end_keyword> <method_def('method')>
#    }
#    token routine_declarator:sym<submethod> {
#        <sym> <.end_keyword> <method_def('submethod')>
#    }
#    token routine_declarator:sym<macro> {
#        <sym> <.end_keyword> <macro_def()>
#    }
#
#    rule routine_def($d) {
#        # XXX There was code here
#        # XXX There was code here
#        <.attach_leading_docs>
#        <deflongname>?
#        # XXX There was code here
#        <.newpad>
#        [
#            '(' <multisig> ')' {
#                %*SIG_INFO := $<multisig>.ast;
#                $*SIG_OBJ := $*W.create_signature_and_params($<multisig>,
#                    %*SIG_INFO, $*W.cur_lexpad(), 'Any');
#            }
#        ]?
#        <trait>* :!s
#        # XXX There was code here
#        [
#        || ';'
#            # XXX There was code here
#            <.finishpad>
#            <statementlist(1)>
#            # XXX There was code here
#        || <onlystar>
#        || <!before '{'> <possibly_subname=.deflongname> { if self.parse($<deflongname>.Str, :rule('typename')) { $/.panic("Did you mean to write \"my $<deflongname> sub $<possibly_subname>\" or put \"returns $<deflongname>\" before the block?"); } } <!>
#        || <blockoid>
#        ]
#    }
#
#    rule method_def($d) {
#        # XXX There was code here
#        # XXX There was code here
#        <.attach_leading_docs>
#        [
#            <.newpad>
#            [
#            | $<specials>=[<[ ! ^ ]>?]<longname> [ '(' <multisig(1)> ')' ]? <trait>*
#            | '(' <multisig(1)> ')' <trait>*
#            | <sigil>'.':!s
#                [
#                | '(' ~ ')' <multisig(1)>
#                | '[' ~ ']' <multisig(1)>
#                | '{' ~ '}' <multisig(1)>
#                ]:s
#                <trait>*
#            | <?>
#            ]
#            # XXX There was code here
#            [
#            || <onlystar>
#            || <blockoid>
#            ]
#        ] || <.malformed('method')>
#    }
#
#    rule macro_def() {
#        <.experimental('macros')>
#        # XXX There was code here
#        # XXX There was code here
#        <.attach_leading_docs>
#        <deflongname>?
#        # XXX There was code here
#        <.newpad>
#        [ '(' <multisig> ')' ]?
#        <trait>*
#        # XXX There was code here
#        [
#        || <onlystar>
#        || <blockoid>
#        ]
#    }
#
#    token onlystar {
#        # XXX There was code here
#        '{' <.ws> '*' <.ws> '}'
#        <?ENDSTMT>
#        <.finishpad>
#        # XXX There was code here
#    }
#
#    ###########################
#    # Captures and Signatures #
#    ###########################
#
#    token capterm {
#        '\\'
#        [
#        | '(' <semiarglist> ')'
#        | <?before '$' | '@' | '%' | '&'> <.typed_worry('X::Worry::P5::Reference')> <termish>
#        | <?before \d> <.typed_worry('X::Worry::P5::BackReference')> <termish>
#        | <?before \S> <termish>
#        | {} <.panic: "You can't backslash that">
#        ]
#    }
#
#    rule param_sep {
#        '' $<sep>=[','|':'|';;'|';'] {
#            if $<sep> eq ';;' {
#                $/.panic("Can only specify ';;' once in a signature")
#                  if $*multi_invocant == 0;
#                $*multi_invocant := 0;
#            }
#            @*seps.push($<sep>);
#        }
#    }
#
#    # XXX Not really implemented yet.
#    token multisig($allow_invocant = 0) {
#        <signature('sig', $allow_invocant)>
#    }
#
#    token sigterm {
#        ':(' ~ ')' <fakesignature>
#    }
#
#    token fakesignature {
#        <.newpad>
#        <signature('sig', 1)>
#    }
#
#    token signature($*IN_DECL = 'sig', $*ALLOW_INVOCANT = 0) {
#        :my @*seps := nqp::list();
#        <.ws>
#        [
#        | <?before '-->' | ')' | ']' | '{' | ':'\s | ';;' >
#        | <parameter>
#        ]+ % <param_sep>
#        <.ws>
#        [ <?before '-->' | ')' | ']' | '{' | ':'\s | ';;' > || <.malformed('parameter')> ]
#        { $*IN_DECL := ''; }
#        [ '-->' <.ws> [ || [<typename>|<value>||<typo_typename(1)>] <.ws>
#                           [ || <?[ { ) ]>
#                             || <?before <.param_sep>? <.parameter>>
#                                <.malformed('return value (return constraints only allowed at the end of the signature)')>
#                           ]
#                        || <.malformed('return value')>
#                      ] ]?
#        { $*LEFTSIGIL := '@'; }
#    }
#
#    token parameter {
#        # We'll collect parameter information into a hash, then use it to
#        # build up the parameter object in the action method
#        [
#        | <type_constraint>+
#            [
#            | $<quant>=['**'|'*'|'+'] <param_var>
#            | $<quant>=['\\'|'|'] <param_var> {
#                $/.panic('Obsolete use of | or \\ with sigil on param ' ~ $<param_var>);
#            }
#            | $<quant>=['\\'|'|'|'+'] <param_term>
#            | [ <param_var> | <named_param> ] $<quant>=['?'|'!'|<?>]
#            | <?>
#            ]
#        | $<quant>=['**'|'*'|'+'] <param_var>
#        | $<quant>=['\\'|'|'] <param_var> {
#            $/.panic('Obsolete use of | or \\ with sigil on param ' ~ $<param_var>);
#        }
#        | $<quant>=['\\'|'|'|'+'] <param_term>
#        | [ <param_var> | <named_param> ] $<quant>=['?'|'!'|<?>]
#        | <longname>
#            {
#                my $name := $*W.dissect_longname($<longname>);
#                $*W.throw($/, ['X', 'Parameter', 'InvalidType'],
#                    :typename($name.name),
#                    :suggestions($*W.suggest_typename($name.name)));
#            }
#        ]
#        <.ws>
#        <trait>*
#        <post_constraint('param')>*
#        <.newthunk>
#        [
#            <default_value>
#            [ <modifier=.trait> {
#                self.typed_panic: "X::Parameter::AfterDefault", type => "trait", modifier => $<modifier>, default => $<default_value>
#            }]?
#            [ <modifier=.post_constraint('param')> {
#                self.typed_panic: "X::Parameter::AfterDefault", type => "post constraint", modifier => $<modifier>, default => $<default_value>
#            }]?
#        ]**0..1
#        { $*CURTHUNK := $*W.pop_thunk() }
#
#        # enforce zone constraints
#        {
#            my $kind :=
#                $<named_param>                      ?? '*' !!
#                $<quant> eq '?' || $<default_value> ?? '?' !!
#                $<quant> eq '!'                     ?? '!' !!
#                $<quant> ne '' && $<quant> ne '\\'  ?? '*' !!
#                                                       '!';
#            my $name := %*PARAM_INFO<variable_name> // '';
#            if $kind eq '!' {
#                if $*zone eq 'posopt' {
#                    $/.typed_panic('X::Parameter::WrongOrder', misplaced => 'required', after => 'optional', parameter => $name);
#                }
#                elsif $*zone eq 'var' {
#                    $/.typed_panic('X::Parameter::WrongOrder', misplaced => 'required', after => 'variadic', parameter => $name);
#                }
#            }
#            elsif $kind eq '?' {
#                if $*zone  eq 'posreq' {
#                        $*zone := 'posopt';
#                }
#                elsif $*zone eq  'var' {
#                    $/.typed_panic('X::Parameter::WrongOrder', misplaced => 'optional positional', after => 'variadic', parameter => $name);
#                }
#            }
#            elsif $kind eq '*' {
#                $*zone := 'var';
#            }
#
#            %*PARAM_INFO<is_multi_invocant> := $*multi_invocant;
#            %*PARAM_INFO<node> := $/;
#        }
#    }
#
#    token param_var {
#        <.attach_leading_docs>
#        {
#            my $line_no := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
#            if $*PRECEDING_DECL_LINE < $line_no {
#                $*PRECEDING_DECL_LINE := $line_no;
#                my $par_type := $*W.find_symbol(['Parameter'], :setting-only);
#                $*PRECEDING_DECL := nqp::create($par_type); # actual declarand comes later, in World::create_parameter
#            }
#        }
#        [
#        | '[' ~ ']' <signature>
#        | '(' ~ ')' <signature>
#        | <sigil> <twigil>?
#          [
#          || # XXX There was code here
#              [<?identifier> {} <name=.sublongname> | <sigterm>]
#          || <name=.identifier>
#          || <name=.decint> { $*W.throw($/, 'X::Syntax::Variable::Numeric', what => 'parameter') }
#          || $<name>=[<[/!]>]
#          ]?
#
#          [
#          | <?before ':('>  ':'  # XXX allow fakesig parsed as subsig for the moment
#          | <?before '('>         <.sorry: "Shape declaration with () is reserved;\n  please use whitespace if you meant a subsignature for unpacking,\n  or use the :() form if you meant to add signature info to the function's type">
#          | <?before '['> <arrayshape=.postcircumfix>
#          | <?before <.[ { < « ]>> <.sorry: 'Shape declaration is not yet implemented; please use whitespace if you meant something else'>
#               <postcircumfix>
#          ]?
#        ]
#    }
#
#    token param_term {
#        <defterm>?
#    }
#
#    token named_param {
#        ':'
#        [
#        | <name=.identifier> '('
#            <.ws> [ <named_param> | <param_var> ] <.ws>
#            [ ')' || <.panic: 'Unable to parse named parameter; couldn\'t find right parenthesis'> ]
#        | <param_var>
#        ]
#    }
#
#    rule default_value {
#        '=' <EXPR('i=')>
#    }
#
#    token type_constraint {
#        [
#        | <value>
#        | [ <[-−]> | '+' ] $<value>=<numish>
#        | <typename>
#        | where <.ws> <EXPR('i=')>
#        ]
#        <.ws>
#    }
#
#    rule post_constraint($*CONSTRAINT_USAGE) {
#        [
#        | '[' ~ ']' <signature>
#        | '(' ~ ')' <signature>
#        | where <EXPR('i=')>
#        ]
#    }
#
#    proto token regex_declarator { <...> }
#    token regex_declarator:sym<rule> {
#        <sym><.kok>
#        # XXX There was code here
#        <regex_def>
#    }
#    token regex_declarator:sym<token> {
#        <sym><.kok>
#        # XXX There was code here
#        <regex_def>
#    }
#    token regex_declarator:sym<regex> {
#        <sym><.kok>
#        <regex_def>
#    }
#
#    rule regex_def {
#        # XXX There was code here
#        # XXX There was code here
#        <.attach_leading_docs>
#        [
#          <deflongname>?
#          { if $<deflongname> { %*RX<name> := ~$<deflongname>.ast } }
#          { $*IN_DECL := '' }
#           <.newpad>
#          [ [ ':'?'(' <signature('sig', 1)> ')' ] | <trait> ]*
#          '{'
#          [
#          | ['*'|'<...>'|'<*>'] <?{ $*MULTINESS eq 'proto' }> $<onlystar>={1}
#          | <nibble(self.quote_lang(%*RX<P5> ?? self.slang_grammar('P5Regex') !! self.slang_grammar('Regex'), '{', '}'))>
#          ]
#          '}'<!RESTRICTED><?ENDSTMT>
#          # XXX There was code here
#        ] || <.malformed('regex')>
#    }
#
#    proto token type_declarator { <...> }
#
#    token type_declarator:sym<enum> {
#        <sym><.kok>
#        # XXX There was code here
#        # XXX There was code here
#        <.attach_leading_docs>
#        [
#        | <longname> # XXX There was code here
#        | <variable>
#        | <?>
#        ]
#        # XXX There was code here
#        <.ws>
#        <trait>*
#        [ <?[<(«]> <term> <.ws> || <.panic: 'An enum must supply an expression using <>, «», or ()'> ]
#        <.explain_mystery> <.cry_sorrows>
#    }
#
#    rule type_declarator:sym<subset> {
#        <sym><.kok>
#        # XXX There was code here
#        # XXX There was code here
#        <.attach_leading_docs>
#        [
#            [
#                [
#                    <longname>
#                    # XXX There was code here
#                ]?
#                # XXX There was code here
#                <trait>*
#                [ where <EXPR('e=')> ]?
#            ]
#            || <.malformed('subset')>
#        ]
#    }
#
#    token type_declarator:sym<constant> {
#        <sym><.kok>
#
#        [
#        | '\\'? <defterm>
#        | <variable> { $¢.add_variable(~$<variable>) } # for new &infix:<foo> synonyms
#        | <?>
#        ]
#        # XXX There was code here
#        <.ws>
#
#        <trait>*
#
#        # XXX There was code here
#        [
#        || <initializer> <.explain_mystery> <.cry_sorrows>
#        || <.missing: "initializer on constant declaration">
#        ]
#
#        <.cheat_heredoc>?
#    }
#
#    proto token initializer { <...> }
#    token initializer:sym<=> {
#        <sym>
#        [
#            <.ws>
#            [
#            || <?{ $*LEFTSIGIL eq '$' }> <EXPR('i<=')>
#            || <EXPR('e=')>
#            ]
#            || <.malformed: 'initializer'>
#        ]
#    }
#    token initializer:sym<:=> {
#        <sym> [ <.ws> <EXPR('e=')> || <.malformed: 'binding'> ]
#    }
#    token initializer:sym<::=> {
#        <sym> [ <.ws> <EXPR('e=')> <.NYI('"::="')> || <.malformed: 'binding'> ]
#    }
#    token initializer:sym<.=> {
#        <sym> [ <.ws> <dottyopish> || <.malformed: 'mutator method call'> ]
#    }
#
#    rule trait {
#        <trait_mod>
#    }
#
#    proto rule trait_mod { <...> }
#    rule trait_mod:sym<is> {
#        <sym> [ [<longname><circumfix>**0..1] || <.panic: 'Invalid name'> ]
#        <.explain_mystery> <.cry_sorrows>
#        # XXX There was code here
#    }
#    rule trait_mod:sym<hides>   { <sym> [ <typename> || <.bad_trait_typename>] }
#    rule trait_mod:sym<does>    { <sym> [ <typename> || <.bad_trait_typename>] }
#    rule trait_mod:sym<will>    { <sym> [ <identifier> || <.panic: 'Invalid name'>] <pblock($PBLOCK_OPTIONAL_TOPIC)> }
#    rule trait_mod:sym<of>      { <sym> [ <typename> || <.bad_trait_typename>] }
#    rule trait_mod:sym<returns> { <sym> [ <typename> || <.bad_trait_typename>]
#                                  || 'return' <.panic: 'Invalid trait modifier (did you mean \'returns\'?)'> }
#    rule trait_mod:sym<handles> { <sym> [ <term> || <.panic: 'Invalid term'>] }
#
#    token bad_trait_typename {
#        || <longname> # XXX There was code here
#        || <.malformed: 'trait'>
#    }
#
#    ## Terms
#
#    proto token term { <...> }
#
#    token term:sym<self> {
#        <sym> <.end_keyword>
#        # XXX There was code here
#    }
#
#    token term:sym<now> { <sym> <.tok> }
#
#    token term:sym<time> { <sym> <.tok> }
#
#    token term:sym<empty_set> { "∅" <!before <.[ \( \\ ' \- ]> || \h* '=>'> }
#
#    token term:sym<rand> {
#        <!{ $*LANG.pragma('p5isms') }>
#        <sym> »
#        [ <?before '('? \h* [\d|'$']> <.obs('rand(N)', 'N.rand for Num or (^N).pick for Int result')> ]?
#        [ <?before '()'> <.obs('rand()', 'rand')> ]?
#        <.end_keyword>
#    }
#
#    token term:sym<...> {
#        [<sym>|'…']
#        [ <?after ','\h*<.[ . … ]>+> { self.worry("Comma found before apparent sequence operator; please remove comma (or put parens around the ... call, or use 'fail' instead of ...)") } ]?
#        [ <?{ $*GOAL eq 'endargs' && !$*COMPILING_CORE_SETTING }> <?after <.:L + [\]]>\h*<[ . … ]>+> { self.worry("Apparent sequence operator parsed as stubbed function argument; please supply any missing argument to the function or the sequence (or parenthesize the ... call, or use 'fail' instead of ...)") } ]?
#        <args>
#    }
#    token term:sym<???> { <sym> <args> }
#    token term:sym<!!!> { <sym> <args> }
#
#    token term:sym<identifier> {
#        <identifier> <!{ $*W.is_type([~$<identifier>]) }> [ <?before <.unsp>? '('> | \\ <?before '('> ]
#        # XXX There was code here
#        <args(1)>
#        # XXX There was code here
#    }
#
#    token term:sym<nqp::op> {
#        'nqp::' $<op>=[\w+] <args>?
#    }
#
#    token term:sym<nqp::const> {
#        'nqp::const::' $<const>=[\w+]
#    }
#
#    my %deftrap := nqp::hash(
#        'say', 1, 'print', 1, 'abs', 1, 'chomp', 1, 'chop', 1, 'chr', 1, 'cos', 1,
#        'defined', 1, 'exp', 1, 'lc', 1, 'log', 1, 'mkdir', 1, 'ord', 1, 'reverse', 1,
#        'rmdir', 1, 'sin', 1, 'split', 1, 'sqrt', 1, 'uc', 1, 'unlink', 1, 'fc', 1,
#
#        'WHAT', 2, 'WHICH', 2, 'WHERE', 2, 'HOW', 2, 'WHENCE', 2, 'WHO', 2, 'VAR', 2, 'any', 2,
#        'all', 2, 'none', 2, 'one', 2, 'set', 2, 'bag', 2, 'tclc', 2, 'wordcase', 2, 'put', 2,
#    );
#
#    token term:sym<name> {
#        <longname>
#        # XXX There was code here
#        [
#        ||  # XXX There was code here
#            <.unsp>?
#            [
#                <?[[]> # XXX There was code here
#                '[' ~ ']' <arglist>
#            ]?
#            <.unsp>?
#            [
#                <?[{]> # XXX There was code here
#                <whence=.postcircumfix> <.NYI('Autovivifying object closures')>
#            ]?
#            <.unsp>?
#            [
#                <?[(]> # XXX There was code here
#                '(' <.ws> [
#                    || <accept=.maybe_typename> <!{ nqp::isconcrete($<accept>.ast) }>
#                    || $<accept_any>=<?>
#                ] <.ws> ')'
#            ]?
#            [
#                # XXX There was code here
#                <colonpairs=.O(|%colonpairs)>
#            ]?
#        || [ \\ <?before '('> ]? <args(1)>
#        ]
#    }
#
#    token term:sym<dotty> { <dotty> { self.mark_variable_used('$_') } }
#
#    token term:sym<capterm> { <capterm> }
#
#    token term:sym<onlystar> {
#        '{*}' <?ENDSTMT>
#        [ <?{ $*IN_PROTO }> || <.panic: '{*} may only appear in proto'> ]
#    }
#
#    token args($*INVOCANT_OK = 0) {
#        [
#        | '(' ~ ')' <semiarglist>
#        | <.unsp> '(' ~ ')' <semiarglist>
#        | [ \s <arglist> ]
#        | <?>
#        ]
#    }
#
#    token semiarglist {
#        <arglist>+ % ';'
#        <.ws>
#    }
#
#    token arglist {
#        <.ws>
#        [
#        | <?stdstopper>
#        | <EXPR('e=')>
#        | <?>
#        ]
#    }
#
#    proto token value { <...> }
#    token value:sym<quote>  { <quote> }
#    token value:sym<number> { <number> }
#    token value:sym<version> { <version> }
#
#    proto token number { <...> }
#    token number:sym<numish>   { <numish> }
#
#    token signed-number { <sign> <number> }
#
#    token numish {
#        [
#        | 'NaN' >>
#        | <integer>
#        | <dec_number>
#        | <rad_number>
#        | <rat_number>
#        | <complex_number>
#        | 'Inf' >>
#        | $<uinf>='∞'
#        | <unum=:No+:Nl>
#        ]
#    }
#
#    token dec_number {
#        [
#        | $<coeff> = [               '.' <frac=.decint> ] <escale>?
#        | $<coeff> = [ <int=.decint> '.' <frac=.decint> ] <escale>?
#        | $<coeff> = [ <int=.decint>                    ] <escale>
#        ]
#    }
#
#    token signed-integer { <sign> <integer> }
#
#    token integer {
#        [
#        | 0 [ b '_'? <VALUE=binint>
#            | o '_'? <VALUE=octint>
#            | x '_'? <VALUE=hexint>
#            | d '_'? <VALUE=decint>
#            | <VALUE=decint>
#                <!!{ $/.typed_worry('X::Worry::P5::LeadingZero', value => ~$<VALUE>) }>
#            ]
#        | <VALUE=decint>
#        ]
#        <!!before ['.' <?before \s | ',' | '=' | ':' <!before  <coloncircumfix <OPER=prefix> > > | <.terminator> | $ > <.typed_sorry: 'X::Syntax::Number::IllegalDecimal'>]? >
#        [ <?before '_' '_'+\d> <.sorry: "Only isolated underscores are allowed inside numbers"> ]?
#    }
#
#    token rad_number {
#        ':' $<radix> = [\d+] <.unsp>?
#        {}           # don't recurse in lexer
#        [
#        || '<'
#                $<ohradix>  = [ '0x' <?{ $r < 34 }> | '0o' <?{ $r < 25 }> | '0d' <?{ $r < 14 }> | '0b' <?{ $r < 12 }> ]**0..1
#                $<intpart>  = <rad_digits>
#                $<fracpart> = [ '.' <rad_digits> ]**0..1
#                [ '*' <base=.radint> '**' <exp=.radint> ]**0..1
#           '>'
#        || <?[[]> <bracket=circumfix>
#        || <?[(]> <circumfix>
#        || <.malformed: 'radix number'>
#        ]
#    }
#
#    token radint {
#        [
#        | <integer>
#        ]
#    }
#
#    token escale { <[Ee]> <sign> <decint> }
#
#    token sign { '+' | '-' | '−' | '' }
#
#    token rat_number { '<' <bare_rat_number> '>' }
#    token bare_rat_number { <?before <.[-−+0..9<>:boxd]>+? '/'> <nu=.signed-integer> '/' <de=integer> }
#
#    token complex_number { '<' <bare_complex_number> '>' }
#    token bare_complex_number { <?before <.[-−+0..9<>:.eEboxdInfNa\\]>+? 'i'> <re=.signed-number> <?[-−+]> <im=.signed-number> \\? 'i' }
#
#    token typename {
#        [
#        | '::?'<identifier> <colonpair>*    # parse ::?CLASS as special case
#        | <longname>
#          # XXX There was code here
#        ]
#        # parametric/coercion type?
#        <.unsp>? [
#            <?[[]>
#            '[' ~ ']' <arglist>
#            <.explain_mystery> <.cry_sorrows>
#        ]?
#        <.unsp>? [ <?before '{'> <whence=.postcircumfix> <.NYI('Autovivifying object closures')> ]?
#        <.unsp>? [ <?[(]> '(' ~ ')' [<.ws> [<accept=.typename> || $<accept_any>=<?>] <.ws>] ]?
#        [<.ws> 'of' <.ws> <typename> ]?
#        [
#            # XXX There was code here
#            <colonpairs=.O(|%colonpairs)>
#        ]?
#    }
#
#    token typo_typename($panic = 0) {
#        <longname>
#        # XXX There was code here
#    }
#
#    token quotepair($*purpose = 'quoteadverb') {
#        ':'
#        [
#        | '!' <identifier> [ <?[(]> <.sorry('Argument not allowed on negated pair')> ]?
#            { $*key := ~$<identifier>; $*value := 0; }
#        | <identifier>
#            { $*key := ~$<identifier> }
#            [
#            || <?[(]> <circumfix> { $*value := $<circumfix>.ast; }
#            || { $*value := 1; }
#            ]
#        | (\d+) <identifier>
#            [ <?[(]> <.circumfix> <.sorry('2nd argument not allowed on pair')> ]?
#            { $*key := ~$<identifier>; $*value := +~$/[0] }
#        ]
#    }
#
#    token rx_adverbs($quotepair-kind = 'rxadverb') {
#        [
#            <quotepair($quotepair-kind)> <.ws>
#            # XXX There was code here
#            <.setup_quotepair>
#        ]*
#    }
#
#    token qok($x) {
#        » <![(]>
#        [ <?[:]> || <!{ my str $n := ~$x; $*W.is_name([$n]) || $*W.is_name(['&' ~ $n]) }> ]
#        [ \s* '#' <.panic: "# not allowed as delimiter"> ]?
#        <.ws>
#    }
#
#    proto token quote_mod   {*}
#    token quote_mod:sym<w>  { <sym> }
#    token quote_mod:sym<ww> { <sym> }
#    token quote_mod:sym<x>  { <sym> }
#    token quote_mod:sym<to> { <sym> }
#    token quote_mod:sym<s>  { <sym> }
#    token quote_mod:sym<a>  { <sym> }
#    token quote_mod:sym<h>  { <sym> }
#    token quote_mod:sym<f>  { <sym> }
#    token quote_mod:sym<c>  { <sym> }
#    token quote_mod:sym<b>  { <sym> }
#
#    proto token quote { <...> }
#    token quote:sym<apos>  { "'" ~ "'" <nibble(self.quote_lang(self.slang_grammar('Quote'), "'", "'", ['q']))> }
#    token quote:sym<sapos> { "‘" ~ "’" <nibble(self.quote_lang(self.slang_grammar('Quote'), "‘", "’", ['q']))> }
#    token quote:sym<lapos> { "‚" ~ <[’‘]> <nibble(self.quote_lang(self.slang_grammar('Quote'), "‚", ["’","‘"], ['q']))> }
#    token quote:sym<hapos> { "’" ~ <[’‘]> <nibble(self.quote_lang(self.slang_grammar('Quote'), "’", ["’","‘"], ['q']))> }
#    token quote:sym<dblq>  { '"' ~ '"' <nibble(self.quote_lang(self.slang_grammar('Quote'), '"', '"', ['qq']))> }
#    token quote:sym<sdblq> { '“' ~ '”' <nibble(self.quote_lang(self.slang_grammar('Quote'), '“', '”', ['qq']))> }
#    token quote:sym<ldblq> { '„' ~ <[”“]> <nibble(self.quote_lang(self.slang_grammar('Quote'), '„', ['”','“'], ['qq']))> }
#    token quote:sym<hdblq> { '”' ~ <[”“]> <nibble(self.quote_lang(self.slang_grammar('Quote'), '”', ['”','“'], ['qq']))> }
#    token quote:sym<crnr>  { '｢' ~ '｣' <nibble(self.quote_lang(self.slang_grammar('Quote'), '｢', '｣'))> }
#    token quote:sym<q> {
#        'q'
#        [
#        | <quote_mod> {} <.qok($/)> { $qm := $<quote_mod>.Str } <quibble(self.slang_grammar('Quote'), 'q', $qm)>
#        | {} <.qok($/)> <quibble(self.slang_grammar('Quote'), 'q')>
#        ]
#    }
#    token quote:sym<qq> {
#        'qq'
#        [
#        | <quote_mod> { $qm := $<quote_mod>.Str } <.qok($/)> <quibble(self.slang_grammar('Quote'), 'qq', $qm)>
#        | {} <.qok($/)> <quibble(self.slang_grammar('Quote'), 'qq')>
#        ]
#    }
#    token quote:sym<Q> {
#        'Q'
#        [
#        | <quote_mod> { $qm := $<quote_mod>.Str } <.qok($/)> <quibble(self.slang_grammar('Quote'), $qm)>
#        | {} <.qok($/)> <quibble(self.slang_grammar('Quote'))>
#        ]
#    }
#
#    token quote:sym</null/> { '/' \s* '/' <.typed_panic: "X::Syntax::Regex::NullRegex"> }
#    token quote:sym</ />  {
#        '/' <nibble(self.quote_lang(self.slang_grammar('Regex'), '/', '/'))> [ '/' || <.panic: "Unable to parse regex; couldn't find final '/'"> ]
#        <.old_rx_mods>?
#    }
#    token quote:sym<rx>   {
#        <sym>
#        {} <.qok($/)>
#        <rx_adverbs>
#        <quibble(%*RX<P5> ?? self.slang_grammar('P5Regex') !! self.slang_grammar('Regex'))>
#        <!old_rx_mods>
#    }
#
#    token quote:sym<m> {
#        <sym> (s)**0..1
#        # XXX There was code here
#        <.qok($/)>
#        <rx_adverbs>
#        <quibble(%*RX<P5> ?? self.slang_grammar('P5Regex') !! self.slang_grammar('Regex'))>
#        <!old_rx_mods>
#    }
#
#    token quote:sym<qr> {
#        <sym> {} <.qok($/)> <.obs('qr for regex quoting', 'rx//')>
#    }
#
#    token setup_quotepair { '' }
#
#    # nibbler for s///
#    token sibble($l, $lang2, @lang2tweaks?) {
#        <babble($l)>
#        # XXX There was code here
#
#        # XXX There was code here
#        $start <left=.nibble($lang)> [ $stop || { self.fail-terminator($/, $start, $stop) } ]
#        # XXX There was code here
#        # XXX There was code here
#        [ # XXX There was code here
#            <.ws>
#            [ <?[ \[ \{ \( \< ]> <.obs('brackets around replacement', 'assignment syntax')> ]?
#            [ <infixish> || <.missing: "assignment operator"> ]
#            [ <?{ $<infixish>.Str eq '=' || $<infixish><infix_postfix_meta_operator> }> || <.malformed: "assignment operator"> ]
#            <.ws>
#            [ <right=.EXPR('i')> || <.panic: "Assignment operator missing its expression"> ]
#        ||
#            { $lang := self.quote_lang($lang2, $stop, $stop, @lang2tweaks); }
#            <right=.nibble($lang)> $stop || <.panic("Malformed replacement part; couldn't find final $stop")>
#        ]
#        # XXX There was code here
#    }
#
#    token quote:sym<s> {
#        <sym=[Ss]> (s)**0..1
#        # XXX There was code here
#        <.qok($/)>
#        <rx_adverbs>
#        <sibble(%*RX<P5> ?? self.slang_grammar('P5Regex') !! self.slang_grammar('Regex'), self.slang_grammar('Quote'), ['qq'])>
#        [ <?{ $<sibble><infixish> }> || <.old_rx_mods>? ]
#    }
#    # nibbler for tr///
#    token tribble ($l, $lang2 = $l, @lang2tweaks?) {
#        <babble($l, @lang2tweaks)>
#        # XXX There was code here
#
#        $start <left=.nibble($lang)> [ $stop || { self.fail-terminator($/, $start, $stop) } ]
#        # XXX There was code here
#
#        [ # XXX There was code here
#            $start <right=.nibble($lang)> [ $stop || { self.fail-terminator($/, $start, $stop) } ]
#        || # XXX There was code here
#            <right=.nibble($lang)> $stop || <.panic("Malformed replacement part; couldn't find final $stop")>
#        ]
#    }
#
#    token quote:sym<tr> {
#        $<sym>=['tr' | 'TR']
#        {} <.qok($/)>
#        <rx_adverbs('tr-adverb')>
#        <tribble(self.slang_grammar('Quote'), self.slang_grammar('Quote'), ['cc'])>
#        <.old_rx_mods>?
#    }
#
#    token quote:sym<y> {
#        <sym>
#        <?before \h*\W>
#        {} <.qok($/)>
#        <.obs('y///','tr///')>
#    }
#
#    token old_rx_mods {
#        (<[ i g s m x c e ]>)
#        # XXX There was code here
#    }
#
#    token quote:sym<quasi> {
#        <sym> <.ws> <![(]>
#        <.experimental('macros')>
#        <block>
#    }
#
#    token circumfix:sym<STATEMENT_LIST( )> { 'STATEMENT_LIST(' ~ ')' <sequence> }
#
#    token circumfix:sym<( )> {
#        '(' ~ ')' <semilist>
#    }
#    token circumfix:sym<[ ]> { '[' ~ ']' <semilist> }
#    token circumfix:sym<ang> {
#        '<' ~ '>'
#        [
#            [ <?before 'STDIN>' > <.obs('<STDIN>', '$*IN.lines (or add whitespace to suppress warning)')> ]?
#            [ <?[>]> <.obs('<>', 'lines() to read input, (\'\') to represent a null string or () to represent an empty list')> ]?
#            <nibble(self.quote_lang(self.slang_grammar('Quote'), "<", ">", ['q', 'w', 'v']))>
#        ]
#    }
#    token circumfix:sym«<< >>» { '<<' ~ '>>' <nibble(self.quote_lang(self.slang_grammar('Quote'), "<<", ">>", ['qq', 'ww', 'v']))> }
#    token circumfix:sym<« »> { '«' ~ '»' <nibble(self.quote_lang(self.slang_grammar('Quote'), "«", "»", ['qq', 'ww', 'v']))> }
#    token circumfix:sym<{ }> {
#        <?[{]> <pblock($PBLOCK_OPTIONAL_TOPIC)>
#        # XXX There was code here
#    }
#
#    ## Operators
#
#    my %methodcall      := nqp::hash('prec', 'y=', 'assoc', 'unary', 'dba', 'methodcall', 'fiddly', 1);
#    my %autoincrement   := nqp::hash('prec', 'x=', 'assoc', 'unary', 'dba', 'autoincrement');
#    my %exponentiation  := nqp::hash('prec', 'w=', 'assoc', 'right', 'dba', 'exponentiation');
#    my %symbolic_unary  := nqp::hash('prec', 'v=', 'assoc', 'unary', 'dba', 'symbolic unary');
#    my %dottyinfix      := nqp::hash('prec', 'v=', 'assoc', 'left', 'dba', 'dotty infix', 'nextterm', 'dottyopish', 'sub', 'z=', 'fiddly', 1);
#    my %multiplicative  := nqp::hash('prec', 'u=', 'assoc', 'left', 'dba', 'multiplicative');
#    my %additive        := nqp::hash('prec', 't=', 'assoc', 'left', 'dba', 'additive');
#    my %replication     := nqp::hash('prec', 's=', 'assoc', 'left', 'dba', 'replication');
#    my %replication_xx  := nqp::hash('prec', 's=', 'assoc', 'left', 'dba', 'replication', 'thunky', 't.');
#    my %concatenation   := nqp::hash('prec', 'r=', 'assoc', 'left', 'dba', 'concatenation');
#    my %junctive_and    := nqp::hash('prec', 'q=', 'assoc', 'list', 'dba', 'junctive and');
#    my %junctive_or     := nqp::hash('prec', 'p=', 'assoc', 'list', 'dba', 'junctive or');
#    my %named_unary     := nqp::hash('prec', 'o=', 'assoc', 'unary', 'dba', 'named unary');
#    my %structural      := nqp::hash('prec', 'n=', 'assoc', 'non', 'dba', 'structural infix', 'diffy', 1);
#    my %chaining        := nqp::hash('prec', 'm=', 'assoc', 'left', 'dba', 'chaining', 'iffy', 1, 'diffy', 1, 'pasttype', 'chain');
#    my %tight_and       := nqp::hash('prec', 'l=', 'assoc', 'left', 'dba', 'tight and', 'thunky', '.t');
#    my %tight_or        := nqp::hash('prec', 'k=', 'assoc', 'list', 'dba', 'tight or', 'thunky', '.t');
#    my %tight_or_minmax := nqp::hash('prec', 'k=', 'assoc', 'list', 'dba', 'tight or');
#    my %conditional     := nqp::hash('prec', 'j=', 'assoc', 'right', 'dba', 'conditional', 'fiddly', 1, 'thunky', '.tt');
#    my %conditional_ff  := nqp::hash('prec', 'j=', 'assoc', 'right', 'dba', 'conditional', 'fiddly', 1, 'thunky', 'tt');
#    my %item_assignment := nqp::hash('prec', 'i=', 'assoc', 'right', 'dba', 'item assignment');
#    my %list_assignment := nqp::hash('prec', 'i=', 'assoc', 'right', 'dba', 'list assignment', 'sub', 'e=', 'fiddly', 1);
#    my %loose_unary     := nqp::hash('prec', 'h=', 'assoc', 'unary', 'dba', 'loose unary');
#    my %comma           := nqp::hash('prec', 'g=', 'assoc', 'list', 'dba', 'comma', 'nextterm', 'nulltermish', 'fiddly', 1);
#    my %list_infix      := nqp::hash('prec', 'f=', 'assoc', 'list', 'dba', 'list infix');
#    my %list_prefix     := nqp::hash('prec', 'e=', 'assoc', 'right', 'dba', 'list prefix');
#    my %loose_and       := nqp::hash('prec', 'd=', 'assoc', 'left', 'dba', 'loose and', 'thunky', '.t');
#    my %loose_andthen   := nqp::hash('prec', 'd=', 'assoc', 'left', 'dba', 'loose and', 'thunky', '.b');
#    my %loose_or        := nqp::hash('prec', 'c=', 'assoc', 'list', 'dba', 'loose or', 'thunky', '.t');
#    my %loose_orelse    := nqp::hash('prec', 'c=', 'assoc', 'list', 'dba', 'loose or', 'thunky', '.b');
#    my %sequencer       := nqp::hash('prec', 'b=', 'assoc', 'list', 'dba', 'sequencer');
#
#    token termish {
#        [
#        ||  [
#            | <prefixish>+ [ <.arg_flat_nok> <term> || {} <.panic("Prefix " ~ $<prefixish>[-1].Str ~ " requires an argument, but no valid term found")> ]
#            | <.arg_flat_nok> <term>
#            ]
#        || <!{ $*QSIGIL }> <?before <infixish> {
#            $/.typed_panic('X::Syntax::InfixInTermPosition', infix => ~$<infixish>); } >
#        || <!>
#        ]
#        [
#        || # XXX There was code here
#            [
#            || <?{ $*QSIGIL eq '$' }> [ <postfixish>+! <?{ bracket_ending($<postfixish>) }> ]**0..1
#            ||                          <postfixish>+! # XXX There was code here
#            ]
#        || <!{ $*QSIGIL }> <postfixish>*
#        ]
#        # XXX There was code here
#    }
#
#    token arg_flat_nok {
#        <!{ $*ARG_FLAT_OK := 0 }>
#    }
#
#    token prefixish {
#        [
#        | <OPER=prefix>
#        | <OPER=prefix_circumfix_meta_operator>
#        ]
#        <prefix_postfix_meta_operator>**0..1
#        <.ws>
#    }
#
#    token infixish($in_meta = nqp::getlexdyn('$*IN_META')) {
#        <!stdstopper>
#        <!infixstopper>
#        [
#        | <!{ $*IN_REDUCE }> <colonpair> <fake_infix> { $*OPER := $<fake_infix> }
#        |   [
#            | '[' ~ ']' <infixish('[]')> { $*OPER := $<infixish><OPER> }
#                # XXX Gets false positives.
#                #[ <!before '='> { self.worry("Useless use of [] around infix op") unless $*IN_META; } ]?
#            | <?before '[&' <twigil>? [<alpha>|'('] > '[' ~ ']' <variable>
#                {
#                    $<variable><O> := self.O(:prec<t=>, :assoc<left>, :dba<additive>).MATCH unless $<variable><O>;
#                    $*OPER := $<variable>;
#                    self.check_variable($<variable>)
#                }
#            | <infix_circumfix_meta_operator> { $*OPER := $<infix_circumfix_meta_operator> }
#            | <infix_prefix_meta_operator> { $*OPER := $<infix_prefix_meta_operator> }
#            | <infix> { $*OPER := $<infix> }
#            | <?{ $*IN_META ~~ /^[ '[]' | 'hyper' | 'HYPER' | 'R' | 'S' ]$/ && !$*IN_REDUCE }> <.missing("infix inside " ~ $*IN_META)>
#            ]
#            [ <?before '='> <infix_postfix_meta_operator> { $*OPER := $<infix_postfix_meta_operator> }
#            ]?
#        ]
#        <OPER=.AS_MATCH($*OPER)>
#        # XXX There was code here
#    }
#
#    token fake_infix {
#        <O(|%item_assignment, :assoc<unary>, :fake<1>, :dba<adverb>)>
#        # XXX There was code here
#    }
#
#    regex infixstopper {
#        [
#        | <?before '!!'> # XXX There was code here
#        | <?before '{' | <.lambda> > <?MARKED('ws')> # XXX There was code here
#        ]
#    }
#
#    token postfixish {
#        <!stdstopper>
#
#        # last whitespace didn't end here
#        # XXX There was code here
#
#        [ <!{ $*QSIGIL }> [ <.unsp> | '\\' ] ]?
#
#        [ ['.' <.unsp>?]? <postfix_prefix_meta_operator> <.unsp>?]**0..1
#        [
#        | <OPER=postfix>
#        | '.' <?before \W> <OPER=postfix>  ## dotted form of postfix operator (non-wordy only)
#        | <OPER=postcircumfix>
#        | '.' <?[ [ { < ]> <OPER=postcircumfix>
#        | <OPER=dotty>
#        | <OPER=privop>
#        | # XXX There was code here
#            [
#            || <?space> <.missing: "postfix">
#            || <?alpha> <.missing: "dot on method call">
#            || <.malformed: "postfix">
#            ]
#        ]
#        { $*LEFTSIGIL := '@'; }
#    }
#
#    token postop {
#        | <postfix>         $<O> = {$<postfix><O>} $<sym> = {$<postfix><sym>}
#        | <postcircumfix>   $<O> = {$<postcircumfix><O>} $<sym> = {$<postcircumfix><sym>}
#    }
#
#    proto token prefix_circumfix_meta_operator { <...> }
#
#    proto token infix_postfix_meta_operator { <...> }
#
#    proto token infix_prefix_meta_operator { <...> }
#
#    proto token infix_circumfix_meta_operator { <...> }
#
#    proto token postfix_prefix_meta_operator { <...> }
#
#    proto token prefix_postfix_meta_operator { <...> }
#
#    regex term:sym<reduce> {
#        <?before '['\S+']'>
#        <!before '[' <.[ - + ? ~ ^ ]> <.[ \w $ @ ]> >  # disallow accidental prefix before termish thing
#
#        '['
#        [
#        || <op=.infixish('red')> <?[\]]>
#        || $<triangle>=[\\]<op=.infixish('tri')> <?[\]]>
#        || <!>
#        ]
#        ']'
#        { $op := $<op>; }
#
#        <.can_meta($op, "reduce with")>
#
#        [
#        || <!{ $op<OPER><O>.made<diffy> }>
#        || # XXX There was code here
#        || # XXX There was code here
#        ]
#
#        { $*IN_REDUCE := 0 }
#        <args>
#    }
#
#    token postfix_prefix_meta_operator:sym<»> {
#        [ <sym> | $<sym> = '>>' ]
#        [ <!{ $*QSIGIL }> || <![(]> ]
#    }
#
#    token prefix_postfix_meta_operator:sym<«> {
#        <sym> | '<<'
#    }
#
#    token infix_circumfix_meta_operator:sym<« »> {
#        $<opening>=[ '«' | '»' ]
#        {} <infixish('hyper')>
#        $<closing>=[ '«' | '»' || <.missing("« or »")> ]
#        <.can_meta($<infixish>, "hyper with")>
#        {} <O=.AS_MATCH($<infixish><OPER><O>)>
#    }
#
#    token infix_circumfix_meta_operator:sym«<< >>» {
#        $<opening>=[ '<<' | '>>' ]
#        {} <infixish('HYPER')>
#        $<closing>=[ '<<' | '>>' || <.missing("<< or >>")> ]
#        {} <O=.AS_MATCH($<infixish><OPER><O>)>
#    }
#
#    token revO($from) {
#        <?>
#    }
#
#    proto token dotty { <...> }
#    token dotty:sym<.> {
#        <sym> <dottyop>
#        <O(|%methodcall)>
#    }
#
#    token dotty:sym<.*> {
#        $<sym>=['.' [ <[+*?=]> | '^' '!'? ]] <dottyop>
#        <O(|%methodcall)>
#    }
#
#    token dottyop {
#        <.unsp>?
#        [
#        | <methodop>
#        | <colonpair>
#        | <!alpha> <postop> $<O> = {$<postop><O>} $<sym> = {$<postop><sym>}
#        ]
#    }
#
#    token privop {
#        '!' <methodop>
#        <O(|%methodcall)>
#    }
#
#    token methodop {
#        [
#        | <longname> { if $<longname> eq '::' { self.malformed("class-qualified postfix call") } }
#        | <?[$@&]> <variable> { self.check_variable($<variable>) }
#        | <?['"]>
#            [ <!{$*QSIGIL}> || <!before '"' <.-["]>*? [\s|$] > ] # dwim on "$foo."
#            <quote>
#            [ <?before '(' | '.(' | '\\'> || <.panic: "Quoted method name requires parenthesized arguments. If you meant to concatenate two strings, use '~'."> ]
#        ] <.unsp>?
#        [
#            [
#            | <?[(]> <args>
#            | ':' <?before \s | '{'> <!{ $*QSIGIL }> <args=.arglist>
#            ]
#            || <!{ $*QSIGIL }> <?>
#            || <?{ $*QSIGIL }> <?[.]> <?>
#        ] <.unsp>?
#    }
#
#    token dottyopish {
#        <term=.dottyop>
#    }
#
#    token postcircumfix:sym<[ ]> {
#        '[' ~ ']' [ <.ws> <semilist> ]
#        <O(|%methodcall)>
#    }
#
#    token postcircumfix:sym<{ }> {
#        '{' ~ '}' [ <.ws> <semilist> ]
#        <O(|%methodcall)>
#    }
#
#    token postcircumfix:sym<ang> {
#        '<'
#        [
#        || <nibble(self.quote_lang(self.slang_grammar('Quote'), "<", ">", ['q', 'w', 'v']))> '>'
#        || '='* <?before \h* [ \d | <.sigil> | ':' ] >
#           # XXX There was code here
#        || # XXX There was code here
#        ]
#        <O(|%methodcall)>
#    }
#
#    token postcircumfix:sym«<< >>» {
#        '<<'
#        [
#        || <nibble(self.quote_lang(self.slang_grammar('Quote'), "<<", ">>", ['qq', 'ww', 'v']))> '>>'
#        || # XXX There was code here
#        ]
#        <O(|%methodcall)>
#    }
#
#    token postcircumfix:sym<« »> {
#        '«'
#        [
#        || <nibble(self.quote_lang(self.slang_grammar('Quote'), "«", "»", ['qq', 'ww', 'v']))> '»'
#        || # XXX There was code here
#        ]
#        <O(|%methodcall)>
#    }
#
#    token postcircumfix:sym<( )> {
#        '(' ~ ')' [ <.ws> <arglist> ]
#        <O(|%methodcall)>
#    }
#
#    # These are here to prevent us generating the candidates when parsing CORE.setting.
#    token postcircumfix:sym<[; ]> { <!> }
#    token postcircumfix:sym<{; }> { <!> }
#
#    token postfix:sym<i>  { <sym> >> <O(|%methodcall)> }
#
#    token prefix:sym<++>  { <sym>  <O(|%autoincrement)> }
#    token prefix:sym<-->  { <sym>  <O(|%autoincrement)> }
#    token prefix:sym<++⚛> { <sym>  <O(|%autoincrement)> }
#    token prefix:sym<--⚛> { <sym>  <O(|%autoincrement)> }
#    token postfix:sym<++> { <sym>  <O(|%autoincrement)> }
#    token postfix:sym<--> { <sym>  <O(|%autoincrement)> }
#    token postfix:sym<⚛++> { <sym>  <O(|%autoincrement)> }
#    token postfix:sym<⚛--> { <sym>  <O(|%autoincrement)> }
#    token postfix:sym<ⁿ> { <sign=[⁻⁺¯]>? <dig=[⁰¹²³⁴⁵⁶⁷⁸⁹]>+ <O(|%autoincrement)> }
#
#    # TODO: report the correct bracket in error message
#    token postfix:sym«->» {
#        <sym>
#        [
#        |  ['[' | '{' | '(' ] <.obs('->(), ->{} or ->[] as postfix dereferencer', '.(), .[] or .{} to deref, or whitespace to delimit a pointy block')>
#        | <.obs('-> as postfix', 'either . to call a method, or whitespace to delimit a pointy block')>
#        ]
#    }
#
#    token infix:sym<**>   { <sym>  <O(|%exponentiation)> }
#
#    token prefix:sym<+>   { <sym>  <O(|%symbolic_unary)> }
#    token prefix:sym<~~>  { <sym> <.dupprefix('~~')> <O(|%symbolic_unary)> }
#    token prefix:sym<~>   { <sym>  <O(|%symbolic_unary)> }
#    token prefix:sym<->   { <sym> <O(|%symbolic_unary)> }
#    token prefix:sym<−>   { <sym> <O(|%symbolic_unary)> }
#    token prefix:sym<??>  { <sym> <.dupprefix('??')> <O(|%symbolic_unary)> }
#    token prefix:sym<?>   { <sym> <!before '??'> <O(|%symbolic_unary)> }
#    token prefix:sym<!>   { <sym> <!before '!!'> <O(|%symbolic_unary)> }
#    token prefix:sym<|>   { <sym>  <O(|%symbolic_unary)> }
#    token prefix:sym<+^>  { <sym>  <O(|%symbolic_unary)> }
#    token prefix:sym<~^>  { <sym>  <O(|%symbolic_unary)> }
#    token prefix:sym<?^>  { <sym>  <O(|%symbolic_unary)> }
#    token prefix:sym<^^>  { <sym> <.dupprefix('^^')> <O(|%symbolic_unary)> }
#    token prefix:sym<^>   {
#        <sym>  <O(|%symbolic_unary)>
#        <?before \d+ <?before \. <.?alpha> > <.worry: "Precedence of ^ is looser than method call; please parenthesize"> >?
#    }
#    token prefix:sym<⚛>   { <sym>  <O(|%symbolic_unary)> }
#
#    token infix:sym<*>    { <sym>  <O(|%multiplicative)> }
#    token infix:sym<×>    { <sym>  <O(|%multiplicative)> }
#    token infix:sym</>    { <sym>  <O(|%multiplicative)> }
#    token infix:sym<÷>    { <sym>  <O(|%multiplicative)> }
#    token infix:sym<div>  { <sym> >> <O(|%multiplicative)> }
#    token infix:sym<gcd>  { <sym> >> <O(|%multiplicative)> }
#    token infix:sym<lcm>  { <sym> >> <O(|%multiplicative)> }
#    token infix:sym<%>    { <sym>  <O(|%multiplicative)> }
#    token infix:sym<mod>  { <sym> >> <O(|%multiplicative)> }
#    token infix:sym<%%>   { <sym>  <O(|%multiplicative, :iffy(1))> }
#    token infix:sym<+&>   { <sym>  <O(|%multiplicative)> }
#    token infix:sym<~&>   { <sym>  <O(|%multiplicative)> }
#    token infix:sym<?&>   { <sym>  <O(|%multiplicative, :iffy(1))> }
#    token infix:sym«+<»   { <sym> [ <!{ $*IN_META }> || <?before '<<'> || <![<]> ] <O(|%multiplicative)> }
#    token infix:sym«+>»   { <sym> [ <!{ $*IN_META }> || <?before '>>'> || <![>]> ] <O(|%multiplicative)> }
#    token infix:sym«~<»   { <sym> [ <!{ $*IN_META }> || <?before '<<'> || <![<]> ] <O(|%multiplicative)> }
#    token infix:sym«~>»   { <sym> [ <!{ $*IN_META }> || <?before '>>'> || <![>]> ] <O(|%multiplicative)> }
#
#    token infix:sym«<<» { <sym> <!{ $*IN_META }> <?[\s]> <.sorryobs('<< to do left shift', '+< or ~<')> <O(|%multiplicative)> }
#
#    token infix:sym«>>» { <sym> <!{ $*IN_META }> <?[\s]> <.sorryobs('>> to do right shift', '+> or ~>')> <O(|%multiplicative)> }
#
#    token infix:sym<+>    { <sym>  <O(|%additive)> }
#    token infix:sym<->    {
#       # We want to match in '$a >>->> $b' but not 'if $a -> { ... }'.
#        <sym> [<?before '>>'> || <![>]>]
#        <O(|%additive)>
#    }
#    token infix:sym<−>    { <sym>  <O(|%additive)> }
#    token infix:sym<+|>   { <sym>  <O(|%additive)> }
#    token infix:sym<+^>   { <sym>  <O(|%additive)> }
#    token infix:sym<~|>   { <sym>  <O(|%additive)> }
#    token infix:sym<~^>   { <sym>  <O(|%additive)> }
#    token infix:sym<?|>   { <sym>  <O(|%additive, :iffy(1))> }
#    token infix:sym<?^>   { <sym>  <O(|%additive, :iffy(1))> }
#
#    token infix:sym<x>    { <sym> >> <O(|%replication)> }
#    token infix:sym<xx>    { <sym> >> <O(|%replication_xx)> }
#
#    token infix:sym<~>    { <sym>  <O(|%concatenation)> }
#    token infix:sym<.>    { <sym> <ws>
#        <!{ $*IN_REDUCE }>
#        [<!alpha>
#            # XXX There was code here
#        ]?
#        <O(|%dottyinfix)>
#    }
#    token infix:sym<∘>   { <sym>  <O(|%concatenation)> }
#    token infix:sym<o>   { <sym>  <O(|%concatenation)> }
#
#    token infix:sym<&>   { <sym> <O(|%junctive_and, :iffy(1))> }
#    token infix:sym<(&)> { <sym> <O(|%junctive_and)> }
#    token infix:sym«∩»   { <sym> <O(|%junctive_and)> }
#    token infix:sym<(.)> { <sym> <O(|%junctive_and)> }
#    token infix:sym«⊍»   { <sym> <O(|%junctive_and)> }
#
#    token infix:sym<|>    { <sym> <O(|%junctive_or, :iffy(1))> }
#    token infix:sym<^>    { <sym> <O(|%junctive_or, :iffy(1))> }
#    token infix:sym<(|)>  { <sym> <O(|%junctive_or)> }
#    token infix:sym«∪»    { <sym> <O(|%junctive_or)> }
#    token infix:sym<(^)>  { <sym> <O(|%junctive_or)> }
#    token infix:sym«⊖»    { <sym> <O(|%junctive_or)> }
#    token infix:sym<(+)>  { <sym> <O(|%junctive_or)> }
#    token infix:sym«⊎»    { <sym> <O(|%junctive_or)> }
#    token infix:sym<(-)>  { <sym> <O(|%junctive_or)> }
#    token infix:sym«∖»    { <sym> <O(|%junctive_or)> }
#
#    token prefix:sym<let>  { <sym><.kok> <O(|%named_unary)> { $*W.give_cur_block_let($/) } }
#    token prefix:sym<temp> { <sym><.kok> <O(|%named_unary)> { $*W.give_cur_block_temp($/) } }
#
#    token infix:sym«=~=»  { <sym>  <O(|%chaining)> }
#    token infix:sym«≅»    { <sym>  <O(|%chaining)> }
#    token infix:sym«==»   { <sym>  <O(|%chaining)> }
#    token infix:sym«!=»   { <sym> <?before \s|']'> <O(|%chaining)> }
#    token infix:sym«≠»    { <sym>  <O(|%chaining)> }
#    token infix:sym«<=»   { <sym>  <O(|%chaining)> }
#    token infix:sym«≤»    { <sym>  <O(|%chaining)> }
#    token infix:sym«>=»   { <sym>  <O(|%chaining)> }
#    token infix:sym«≥»    { <sym>  <O(|%chaining)> }
#    token infix:sym«<»    { <sym>  <O(|%chaining)> }
#    token infix:sym«>»    { <sym>  <O(|%chaining)> }
#    token infix:sym«eq»   { <sym> >> <O(|%chaining)> }
#    token infix:sym«ne»   { <sym> >> <O(|%chaining)> }
#    token infix:sym«le»   { <sym> >> <O(|%chaining)> }
#    token infix:sym«ge»   { <sym> >> <O(|%chaining)> }
#    token infix:sym«lt»   { <sym> >> <O(|%chaining)> }
#    token infix:sym«gt»   { <sym> >> <O(|%chaining)> }
#    token infix:sym«=:=»  { <sym>  <O(|%chaining)> }
#    token infix:sym<===>  { <sym>  <O(|%chaining)> }
#    token infix:sym<eqv>    { <sym> >> <O(|%chaining)> }
#    token infix:sym<before> { <sym> >> <O(|%chaining)> }
#    token infix:sym<after>  { <sym> >> <O(|%chaining)> }
#    token infix:sym<~~>   { <sym> <O(|%chaining)> }
#    token infix:sym<!~~>  { <sym> <O(|%chaining)> }
#    token infix:sym<(elem)> { <sym> <O(|%chaining)> }
#    token infix:sym«∈»      { <sym> <O(|%chaining)> }
#    token infix:sym«∉»      { <sym> <O(|%chaining)> }
#    token infix:sym<(cont)> { <sym> <O(|%chaining)> }
#    token infix:sym«∋»      { <sym> <O(|%chaining)> }
#    token infix:sym«∌»      { <sym> <O(|%chaining)> }
#    token infix:sym«(<)»    { <sym> <O(|%chaining)> }
#    token infix:sym«⊂»      { <sym> <O(|%chaining)> }
#    token infix:sym«⊄»      { <sym> <O(|%chaining)> }
#    token infix:sym«(>)»    { <sym> <O(|%chaining)> }
#    token infix:sym«⊃»      { <sym> <O(|%chaining)> }
#    token infix:sym«⊅»      { <sym> <O(|%chaining)> }
#    token infix:sym«(<=)»   { <sym> <O(|%chaining)> }
#    token infix:sym«⊆»      { <sym> <O(|%chaining)> }
#    token infix:sym«⊈»      { <sym> <O(|%chaining)> }
#    token infix:sym«(>=)»   { <sym> <O(|%chaining)> }
#    token infix:sym«⊇»      { <sym> <O(|%chaining)> }
#    token infix:sym«⊉»      { <sym> <O(|%chaining)> }
#    token infix:sym«(<+)»   { <sym> <O(|%chaining)> }
#    token infix:sym«≼»      { <sym> <O(|%chaining)> }
#    token infix:sym«(>+)»   { <sym> <O(|%chaining)> }
#    token infix:sym«≽»      { <sym> <O(|%chaining)> }
#
#    token infix:sym<&&>   { <sym>  <O(|%tight_and, :iffy(1), :pasttype<if>)> }
#
#    token infix:sym<||>   { <sym>  <O(|%tight_or, :iffy(1), :assoc<left>, :pasttype<unless>)> }
#    token infix:sym<^^>   { <sym>  <O(|%tight_or, :iffy(1), :pasttype<xor>, :thunky<..t>)> }
#    token infix:sym<//>   { <sym>  <O(|%tight_or, :assoc<left>, :pasttype<defor>)> }
#    token infix:sym<min>  { <sym> >> <O(|%tight_or_minmax)> }
#    token infix:sym<max>  { <sym> >> <O(|%tight_or_minmax)> }
#
#    token infix:sym<?? !!> {
#        $<sym>='??'
#        <.ws>
#        <EXPR('i=')>
#        [ '!!'
#        || <?before '::' <.-[=]>> { self.typed_panic: "X::Syntax::ConditionalOperator::SecondPartInvalid", second-part => "::" }
#        || <?before ':' <.-[=\w]>> { self.typed_panic: "X::Syntax::ConditionalOperator::SecondPartInvalid", second-part => ":" }
#        || <infixish> { self.typed_panic: "X::Syntax::ConditionalOperator::PrecedenceTooLoose", operator => ~$<infixish> }
#        || <?{ ~$<EXPR> ~~ / '!!' / }> { self.typed_panic: "X::Syntax::ConditionalOperator::SecondPartGobbled" }
#        || <?before \N*? [\n\N*?]? '!!'> { self.typed_panic: "X::Syntax::Confused", reason => "Confused: Bogus code found before the !! of conditional operator" }
#        || # XXX There was code here
#        ]
#        <O(|%conditional, :reducecheck<ternary>, :pasttype<if>)>
#    }
#
#    token infix_prefix_meta_operator:sym<!> {
#        <sym> <![!]> {} [ <infixish('neg')> || <.panic: "Negation metaoperator not followed by valid infix"> ]
#        [
#        || <?{ $<infixish>.Str eq '=' }> <O(|%chaining)>
#        || <.can_meta($<infixish>, "negate")> <?{ $<infixish><OPER><O>.made<iffy> }> <O=.AS_MATCH($<infixish><OPER><O>)>
#        || # XXX There was code here
#        ]
#    }
#
#    token infix_prefix_meta_operator:sym<R> {
#        <sym> <infixish('R')> {}
#        <.can_meta($<infixish>, "reverse the args of")>
#        <O=.revO($<infixish>)>
#    }
#
#    token infix_prefix_meta_operator:sym<S> {
#        <sym> <infixish('S')> {}
#        <.can_meta($<infixish>, "sequence the args of")>
#        <O=.AS_MATCH($<infixish><OPER><O>)>
#    }
#
#    token infix_prefix_meta_operator:sym<X> {
#        <sym> <infixish('X')> {}
#        <.can_meta($<infixish>, "cross with")>
#        <O(|%list_infix)>
#
#    }
#
#    token infix_prefix_meta_operator:sym<Z> {
#        <sym> <infixish('Z')> {}
#        <.can_meta($<infixish>, "zip with")>
#        <O(|%list_infix)>
#    }
#
#    token infix:sym<minmax> { <sym> >> <O(|%list_infix)> }
#
#    token infix:sym<:=> {
#        <sym>  <O(|%list_assignment)>
#    }
#
#    token infix:sym<::=> {
#        <sym>  <O(|%item_assignment)> <.NYI('"::="')>
#    }
#
#    token infix:sym<.=> { <sym> <O(|%dottyinfix)> }
#
#    # Should probably have <!after '='> to agree w/spec, but after NYI.
#    # Modified infix != below instead to prevent misparse
#    # token infix_postfix_meta_operator:sym<=>($op) {
#    # use $*OPER until NQP/Rakudo supports proto tokens with arguments
#    token infix_postfix_meta_operator:sym<=> {
#        '='
#        { %fudge_oper<OPER> := $*OPER }
#        <.can_meta(%fudge_oper, "make assignment out of")>
#        [ <!{ $*OPER<O>.made<diffy> }> || <.can_meta(%fudge_oper, "make assignment out of", "diffy")> ]
#        # XXX There was code here
#        <O(|%prec, , :iffy(0))> {}
#    }
#
#    token infix:sym«=>» { <sym> <O(|%item_assignment)> }
#
#    token prefix:sym<so> { <sym><.end_prefix> <O(|%loose_unary)> }
#    token prefix:sym<not>  { <sym><.end_prefix> <O(|%loose_unary)> }
#
#    token infix:sym<,>    {
#        <.unsp>? <sym> <O(|%comma, :fiddly(0))>
#        # XXX There was code here
#    }
#    token infix:sym<:>    {
#        # XXX There was code here
#        <.unsp>? <sym> <?before \s | <.terminator> | $ >
#        <O(|%comma, :fiddly(0))>
#        [ <?{ $*INVOCANT_OK }> || <.panic: "Invocant colon not allowed here"> ]
#        # XXX There was code here
#    }
#
#    token infix:sym<Z>    { <!before <.sym> <.infixish> > <sym>  <O(|%list_infix)> }
#    token infix:sym<X>    { <!before <.sym> <.infixish> > <sym>  <O(|%list_infix)> }
#
#    token infix:sym<...>  { <sym> <O(|%list_infix)> }
#    token infix:sym<…>    { <sym> <O(|%list_infix)> }
#    token infix:sym<...^> { <sym>  <O(|%list_infix)> }
#    token infix:sym<…^>   { <sym>  <O(|%list_infix)> }
#    # token term:sym<...>   { <sym> <args>**0..1 <O(|%list_prefix)> }
#
#    token infix:sym<?>    { <sym> {} <![?]> <?before <.-[;]>*?':'> <.obs('? and : for the ternary conditional operator', '?? and !!')> <O(|%conditional)> }
#
#    token infix:sym<ff> { <sym> <O(|%conditional_ff)> }
#    token infix:sym<^ff> { <sym> <O(|%conditional_ff)> }
#    token infix:sym<ff^> { <sym> <O(|%conditional_ff)> }
#    token infix:sym<^ff^> { <sym> <O(|%conditional_ff)> }
#
#    token infix:sym<fff> { <sym> <O(|%conditional_ff)> }
#    token infix:sym<^fff> { <sym> <O(|%conditional_ff)> }
#    token infix:sym<fff^> { <sym> <O(|%conditional_ff)> }
#    token infix:sym<^fff^> { <sym> <O(|%conditional_ff)> }
#
#    token infix:sym<=> {
#        <sym>
#        [
#        || <?{ $*LEFTSIGIL eq '$' || $*IN_META }> <O(|%item_assignment)>
#        || <O(|%list_assignment)>
#        ]
#        # XXX There was code here
#    }
#
#    token infix:sym<⚛=> { <sym> <O(|%item_assignment)> }
#    token infix:sym<⚛+=> { <sym> <O(|%item_assignment)> }
#    token infix:sym<⚛-=> { <sym> <O(|%item_assignment)> }
#    token infix:sym<⚛−=> { <sym> <O(|%item_assignment)> }
#
#    token infix:sym<and>  { <sym> >> <O(|%loose_and, :iffy(1), :pasttype<if>)> }
#    token infix:sym<andthen> { <sym> >> <O(|%loose_andthen, :assoc<list>)> }
#    token infix:sym<notandthen> { <sym> >> <O(|%loose_andthen, :assoc<list>)> }
#
#    token infix:sym<or>   { <sym> >> <O(|%loose_or, :iffy(1), :assoc<left>, :pasttype<unless>)> }
#    token infix:sym<xor>  { <sym> >> <O(|%loose_or, :iffy(1), :pasttype<xor>)> }
#    token infix:sym<orelse> { <sym> >> <O(|%loose_orelse, :assoc<list>, :pasttype<defor>)> }
#
#    token infix:sym«<==»  { <sym> <O(|%sequencer)> }
#    token infix:sym«==>»  { <sym> <O(|%sequencer)> }
#    token infix:sym«<<==» { <sym> <O(|%sequencer)> }
#    token infix:sym«==>>» { <sym> <O(|%sequencer)> }
#
#    token infix:sym<..>   { <sym> [<!{ $*IN_META }> <?[)\]]> <.panic: "Please use ..* for indefinite range">]? <O(|%structural)> }
#    token infix:sym<^..>  { <sym> <O(|%structural)> }
#    token infix:sym<..^>  { <sym> <O(|%structural)> }
#    token infix:sym<^..^> { <sym> <O(|%structural)> }
#
#    token infix:sym<leg>    { <sym> >> <O(|%structural)> }
#    token infix:sym<cmp>    { <sym> >> <O(|%structural)> }
#    token infix:sym<unicmp> { <sym> >> <O(|%structural)> }
#    token infix:sym<coll>   { <sym> >> <O(|%structural)> }
#    token infix:sym«<=>»    { <sym> <O(|%structural)> }
#
#    token infix:sym<but>  { <sym> >> <O(|%structural)> }
#    token infix:sym<does> { <sym> >> <O(|%structural)> }
#
#    token infix:sym<!~> { <sym> \s <.obs('!~ to do negated pattern matching', '!~~')> <O(|%chaining)> }
#    token infix:sym<=~> { <sym> <.obs('=~ to do pattern matching', '~~')> <O(|%chaining)> }
#
#    #================================================================
#    # POD-ONLY CODE HANDLERS
#    #================================================================
#    # move ALL Pod-only grammar objects here
#
#    proto token comment { <...> }
#
#    token comment:sym<#> {
#       '#' {} \N*
#    }
#
#    token comment:sym<#`(...)> {
#        '#`' <?opener> {}
#        [ <.quibble(self.slang_grammar('Quote'))> || <.typed_panic: 'X::Syntax::Comment::Embedded'> ]
#    }
#
#    token comment:sym<#|(...)> {
#        '#|' <?opener> <attachment=.quibble(self.slang_grammar('Quote'))>
#        # XXX There was code here
#    }
#
#    token comment:sym<#|> {
#        '#|' \h+ $<attachment>=[\N*]
#        # XXX There was code here
#    }
#
#    token comment:sym<#=(...)> {
#        '#=' <?opener> <attachment=.quibble(self.slang_grammar('Quote'))>
#        # XXX There was code here
#    }
#
#    token comment:sym<#=> {
#        '#=' \h+ $<attachment>=[\N*] # There was no code
#    }
#
#    token pod_content_toplevel {
#        <pod_block>
#    }
#
#    proto token pod_content { <...> }
#
#    token pod_content:sym<block> {
#        <pod_newline>*
#        <pod_block>
#        <pod_newline>*
#    }
#
#    # any number of paragraphs of text
#    # the paragraphs are separated by one
#    #   or more pod_newlines
#    # each paragraph originally could have
#    #   consisted of more than one line of
#    #   text that were subsequently squeezed
#    #   into one line
#    token pod_content:sym<text> {
#        <pod_newline>*
#
#        # TODO get first line if IN-DEFN-BLOCK
#        <pod_textcontent>+ % <pod_newline>+
#
#        <pod_newline>*
#    }
#
#    # not a block, just a directive
#    token pod_content:sym<config> {
#        <pod_newline>*
#        ^^ \h* '=config' \h+ $<type>=\S+ <pod_configuration>
#        <pod_newline>+
#    }
#
#    proto token pod_textcontent { <...> }
#
#    # for non-code (i.e., regular) text
#    token pod_textcontent:sym<regular> {
#        $<spaces>=[ \h* ]
#         # XXX There was code here
#
#        $<text> = [
#            \h* <!before '=' \w> <pod_string> [ <pod_newline> | $ ]
#        ] +
#    }
#
#    token pod_textcontent:sym<code> {
#        $<spaces>=[ \h* ]
#        # XXX There was code here
#
#        # TODO get first line if IN-DEFN-BLOCK
#        $<text> = [
#            [<!before '=' \w> \N+]+ % [<pod_newline>+ $<spaces>]
#        ]
#    }
#
#    token pod_formatting_code {
#        # XXX There was code here
#
#        <code=[A..Z]>
#        $<begin-tag>=['<'+ <![<]> | '«'] { $*POD_IN_FORMATTINGCODE := 1 }
#        # XXX There was code here
#        [ <!{$<code> eq 'E'}>
#          $<contents>=[
#              <!before $endtag>
#              [ <?{$<code> ne 'L' && $<code> ne 'D' && $<code> ne 'X' }> || <!before \s* \| > ]
#              <pod_string_character>
#          ]*
#        ]?
#        [
#        | <?{$<code> eq 'L'}> \s* \| \s* $<meta>=[<!before $endtag>.]+
#        | <?{$<code> eq 'X'}> \s* \| \s* ( [$<meta>=[<!before $endtag | <[,;]> >.]+] +%% \, ) +%% \;
#        | <?{$<code> eq 'D'}> \s* \| \s* [$<meta>=[<!before $endtag | \; >.]+] +%% \;
#        | <?{$<code> eq 'E'}> ( <integer> | $<uni_name>=<[A..Z\s]>+ <![a..z]> || $<html_ref>=<[A..Za..z]>+ ) +%% \;
#        ]?
#        [ $endtag || <.worry: "Pod formatting code $<code> missing endtag '$endtag'."> ]
#    }
#
#    token pod_balanced_braces {
#        # XXX There was code here
#        [
#            $<braces>=[
#                      || '<'+ <![<]>
#                      || '>'+ <![>]>
#                      ]
#            # XXX There was code here
#          ||
#            # XXX There was code here
#            $<start>=['<'+] <![<]>
#            # XXX There was code here
#            $<contents>=[ <pod_string_character>*?]
#            <!after '>'> $<endtag>=[$endtag]
#        ]
#    }
#
#    token pod_string {
#        <pod_string_character>+
#    }
#
#    token pod_string_character {
#        <pod_balanced_braces> || <pod_formatting_code> || $<char>=[ \N || [
#            <?{ $*POD_IN_FORMATTINGCODE }> \n [
#                <?{ $*POD_DELIMITED_CODE_BLOCK }> <!before \h* '=end' \h+ <pod-delim-code-typ> > ||
#                <!before \h* '=' \w>
#                ]
#            ]
#        ]
#    }
#
#    proto token pod_block { <...> }
#
#    token pod_configuration($spaces = '') {
#        [ [\n $spaces '=']? \h+ <colonpair> ]*
#    }
#
#    token pod_block:sym<delimited_comment> {
#        ^^
#        $<spaces> = [ \h* ]
#        '=begin' \h+ 'comment' {}
#        <pod_configuration($<spaces>)> <pod_newline>+
#        [
#         $<pod_content> = [ .*? ]
#         ^^ $<spaces> '=end' \h+
#         [
#            'comment' [ <pod_newline> | $ ]
#            || $<instead>=<identifier>? # XXX There was code here
#         ]
#        ]
#    }
#
#    regex pod_block:sym<delimited> {
#        ^^
#        $<spaces> = [ \h* ]
#        '=begin'
#        [ <?pod_newline>
#          <.typed_panic('X::Syntax::Pod::BeginWithoutIdentifier')>
#        ]?
#        \h+ <!before 'finish'> # XXX There was code here
#        $<type> = [
#            <pod_code_parent> # There was no code
#            || <identifier>
#        ]
#        <pod_configuration($<spaces>)> <pod_newline>+
#        [
#         # TODO need first line to check for ws-separated '#'
#         <pod_content> *
#         ^^ $<spaces> '=end' \h+
#         [
#             $<type> [ <pod_newline> | $ ]
#             || $<instead>=<identifier>? # XXX There was code here
#         ]
#        ]
#    }
#
#    token pod_block:sym<delimited_table> {
#        ^^
#        $<spaces> = [ \h* ]
#        '=begin' \h+ 'table' {}
#        <pod_configuration($<spaces>)> <pod_newline>+
#        [
#         [ $<table_row>=<.table_row_or_blank> ]*
#         ^^ \h* '=end' \h+
#         [
#            'table' [ <pod_newline> | $ ]
#             || $<instead>=<identifier>? # XXX There was code here
#         ]
#        ]
#    }
#
#    # There are several different identifiers for pod blocks
#    # that are treated essentially the same: 'code', 'input',
#    # and 'output'.
#    token pod-delim-code-typ { code | input | output }
#    token pod_block:sym<delimited_code> {
#        ^^
#        $<spaces> = [ \h* ]
#        '=begin' \h+ $<typ>=<pod-delim-code-typ> {}
#        <pod_configuration($<spaces>)> <pod_newline>+
#        [
#        || <delimited_code_content($<spaces>)> $<spaces> '=end' \h+
#            [ $<end>=<pod-delim-code-typ> [ <pod_newline> | $ ]
#              # There was no code
#              || $<instead>=<identifier>? # XXX There was code here
#            ]
#        ]
#    }
#
#    token delimited_code_content($spaces = '') {
#        ^^
#        (
#        | $spaces
#            <!before '=end' \h+ <pod-delim-code-typ> [ <pod_newline> | $ ]>
#            <pod_string>**0..1 <pod_newline>
#        | <pod_newline>
#        )*
#    }
#
#    token table_row {
#        \h* <!before '=' \w> \N+ [ \n | $ ]
#    }
#
#    token table_row_or_blank {
#        <.table_row> | [\h* <!before '=' \w> \n ]
#    }
#
#    token pod_block:sym<finish> {
#        ^^ \h*
#        [
#            | '=begin' \h+ 'finish' <pod_newline>
#            | '=for'   \h+ 'finish' <pod_newline>
#            | '=finish' <pod_newline>
#        ]
#        $<finish> = .*
#    }
#
#    token pod_block:sym<paragraph> {
#        ^^
#        $<spaces> = [ \h* ]
#        '=for' \h+ <!before 'finish'>
#        # XXX There was code here
#        [ :!ratchet
#            $<type> = [
#                <pod_code_parent> { $*ALLOW_INLINE_CODE := 1 }
#                || <identifier>
#            ]
#            <pod_configuration($<spaces>)>
#            <pod_newline>
#        ]
#        # TODO [defn, term], [first text, line numbered-alias]
#        #   if this is a defn block
#        #     the first line of the first pod_textcontent
#        #     becomes the term
#        #     then combine the rest of the text
#        # TODO exchange **0..1 for modern syntax
#        <pod_content=.pod_textcontent>**0..1
#    }
#
#    token pod_block:sym<paragraph_comment> {
#        ^^
#        $<spaces> = [ \h* ]
#        '=for' \h+ 'comment' {}
#        <pod_configuration($<spaces>)> <pod_newline>
#        $<pod_content> = [ \h* <!before '=' \w> \N+ [ \n | $ ] ]*
#    }
#
#    token pod_block:sym<paragraph_table> {
#        ^^
#        $<spaces> = [ \h* ]
#        '=for' \h+ 'table' {}
#        <pod_configuration($<spaces>)> <pod_newline>
#
#        # TODO add numbered-alias token here
#        [ <!before \h* \n> <table_row>]*
#    }
#
#    token pod_block:sym<paragraph_code> {
#        ^^
#        $<spaces> = [ \h* ]
#        '=for' \h+ <pod-delim-code-typ> {}
#        <pod_configuration($<spaces>)> <pod_newline>
#
#        # TODO get first line if IN-DEFN-BLOCK
#        [ <!before \h* '=' \w> <pod_line> ]*
#    }
#
#    # TODO make sure this token works in all desired
#    #      places: may have to remove the before/afters
#    #      when using with non-abbreviated blocks
#    #      (particulary code blocks)
#    token numbered-alias { <after [^|\s]> '#' <before \s> }
#    token pod_block:sym<abbreviated> {
#        # Note an abbreviated block does not have
#        # %config data, but see the hash mark
#        # handling below.
#        ^^
#        $<spaces> = [ \h* ]
#        '=' <!before begin || end || for || finish || config>
#        # There was no code
#        [ :!ratchet
#            $<type> = [
#                <pod_code_parent> {
#                    $*ALLOW_INLINE_CODE := 1;
#                }
#                || <identifier>
#            ]
#
#            # An optional hash char here is special.
#            [\h+ <numbered-alias>]?
#
#            [\h*\n|\h+]
#        ]
#        # TODO [defn, term], [first text, line numbered-alias]
#        # TODO exchange **0..1 for modern syntax
#        <pod_content=.pod_textcontent>**0..1
#    }
#
#    token pod_block:sym<abbreviated_comment> {
#        ^^
#        $<spaces> = [ \h* ]
#        '=comment' {}
#        [\h*\n|\h+]
#        $<pod_content> = [ \h* <!before '=' \w> \N+ [ \n | $ ] ]*
#    }
#
#    token pod_block:sym<abbreviated_table> {
#        ^^
#        $<spaces> = [ \h* ]
#        '=table' {}
#
#        # An optional hash char here is special.
#        [\h+ <numbered-alias>]?
#
#        <pod_newline>
#        [ <!before \h* \n> <table_row>]*
#    }
#
#    token pod_block:sym<abbreviated_code> {
#        ^^
#        $<spaces> = [ \h* ]
#        '=' <pod-delim-code-typ> {}
#
#        # An optional hash char here is special.
#        # For the code block, we want to eat any ws
#        # between the '#' and the next char
#        #$<numbered-alias>=[\h+ '#' \s]?
#        $<numbered-alias>=[\h+ '#' \s]?
#
#        [\h*\n|\h+]
#
#        [ <!before \h* '=' \w> <pod_line> ]*
#    }
#
#    # TODO exchange **1 for modern syntax
#    token pod_line { <pod_string>**1 [ <pod_newline> | $ ] }
#
#    token pod_newline {
#        \h* \n
#    }
#
#    # These parents can contain implicit code blocks when data
#    # lines begin with whitespace indention from the virtual
#    # margin.
#    token pod_code_parent {
#        [
#        | [ 'pod' | 'item' \d* | 'nested' | 'defn' | 'finish' ]
#        | <upper>+
#        ]
#        <![\w]>
#    }
#}

#grammar Perl6::QGrammar is HLL::Grammar does STD {
#    proto token escape {*}
#    proto token backslash {*}
#
#    role b1 {
#        token escape:sym<\\> { <sym> {} <item=.backslash> }
#        token backslash:sym<qq> { <?[q]> <quote=.LANG('MAIN','quote')> }
#        token backslash:sym<\\> { <text=.sym> }
#        token backslash:delim { <text=.starter> | <text=.stopper> }
#        token backslash:sym<a> { <sym> }
#        token backslash:sym<b> { <sym> }
#        token backslash:sym<c> { <sym> <charspec> }
#        token backslash:sym<e> { <sym> }
#        token backslash:sym<f> { <sym> }
#        token backslash:sym<N> { <?before 'N{'<.[A..Z]>> <.obs('\N{CHARNAME}','\c[CHARNAME]')>  }
#        token backslash:sym<n> { <sym> }
#        token backslash:sym<o> { <sym> [ <octint> | '[' ~ ']' <octints> | '{' <.obsbrace> ] }
#        token backslash:sym<r> { <sym> }
#        token backslash:sym<rn> { 'r\n' }
#        token backslash:sym<t> { <sym> }
#        token backslash:sym<x> { <sym> [ <hexint> | '[' ~ ']' <hexints> | '{' <.obsbrace> ] }
#        token backslash:sym<0> { <sym> }
#        token backslash:sym<1> {
#            <[1..9]>\d* # XXX There was code here
#        }
#        token backslash:sym<unrec> {
#          {} (\w) # XXX There was code here
#        }
#        token backslash:sym<misc> { \W }
#    }
#
#    role b0 {
#        token escape:sym<\\> { <!> }
#    }
#
#    role c1 {
#        token escape:sym<{ }> { <?[{]> <!RESTRICTED> <block=.LANG('MAIN','block')> }
#    }
#
#    role c0 {
#        token escape:sym<{ }> { <!> }
#    }
#
#    role s1 {
#        token escape:sym<$> {
#            <?[$]>
#            <!RESTRICTED>
#            [ <EXPR=.LANG('MAIN', 'EXPR', 'y=')> || { $*W.throw($/, 'X::Backslash::NonVariableDollar') } ]
#        }
#    }
#
#    role s0 {
#        token escape:sym<$> { <!> }
#    }
#
#    role a1 {
#        token escape:sym<@> {
#            <?[@]>
#            <!RESTRICTED>
#            <EXPR=.LANG('MAIN', 'EXPR', 'y=')>
#        }
#    }
#
#    role a0 {
#        token escape:sym<@> { <!> }
#    }
#
#    role h1 {
#        token escape:sym<%> {
#            <?[%]>
#            <!RESTRICTED>
#            <EXPR=.LANG('MAIN', 'EXPR', 'y=')>
#        }
#    }
#
#    role h0 {
#        token escape:sym<%> { <!> }
#    }
#
#    role f1 {
#        token escape:sym<&> {
#            <?[&]>
#            <!RESTRICTED>
#            <EXPR=.LANG('MAIN', 'EXPR', 'y=')>
#        }
#    }
#
#    role f0 {
#        token escape:sym<&> { <!> }
#    }
#
#    role ww {
#        token escape:sym<'> {
#            <?[ ' " ‘ ‚ ’ “ „ ” ｢ ]> <quote=.LANG('MAIN','quote')>
#        }
#        token escape:sym<colonpair> {
#            <?[:]> <!RESTRICTED> <colonpair=.LANG('MAIN','colonpair')>
#        }
#        token escape:sym<#> {
#            <?[#]> <.LANG('MAIN', 'comment')>
#        }
#    }
#
#    role to[$herelang] {
#    }
#
#    role q {
#        token starter { \' }
#        token stopper { \' }
#
#        token escape:sym<\\> { <sym> <item=.backslash> }
#
#        token backslash:sym<qq> { <?[q]> <quote=.LANG('MAIN','quote')> }
#        token backslash:sym<\\> { <text=.sym> }
#        token backslash:delim { <text=.starter> | <text=.stopper> }
#
#        token backslash:sym<miscq> { {} . }
#    }
#
#    role qq does b1 does c1 does s1 does a1 does h1 does f1 {
#        token starter { \" }
#        token stopper { \" }
#    }
#
#    token nibbler {
#        <.do_nibbling>
#    }
#
#    token do_nibbling {
#        [
#            <!stopper>
#            [
#            || <starter> <nibbler> <stopper> # XXX There was code here
#            || <escape> # XXX There was code here
#            || .
#            ]
#        ]*
#        # XXX There was code here
#    }
#
#    role cc {
#        token starter { \' }
#        token stopper { \' }
#
#        # (must not allow anything to match . in nibbler or we'll lose track of state)
#        token escape:ws { \s+ [ <?[#]> <.ws> ]? }
#        token escape:sym<#> { '#' <.panic: "Please backslash # for literal char or put whitespace in front for comment"> }
#        token escape:sym<\\> { <sym> <item=.backslash> <.ccstate('\\' ~ $<item>)> }
#        token escape:sym<..> { <sym>
#            [
#            || <?{ ($*CCSTATE eq '') || ($*CCSTATE eq '..') }> <.sorry("Range missing start character on the left")>
#            || <?before \s* <!stopper> <!before '..'> \S >
#            || <.sorry("Range missing stop character on the right")>
#            ]
#            { $*CCSTATE := '..'; }
#        }
#
#        token escape:sym<-> {
#            '-' <?{ $*CCSTATE ne '' }> \s* <!stopper> \S
#            <.obs('- as character range','.. (or \\- if you mean a literal hyphen)')>
#        }
#        token escape:ch { $<ch> = [\S] { self.ccstate($<ch>) } }
#
#        token backslash:delim { <text=.starter> | <text=.stopper> }
#        token backslash:sym<\\> { <text=.sym> }
#        token backslash:sym<a> { :i <sym> }
#        token backslash:sym<b> { :i <sym> }
#        token backslash:sym<c> { :i <sym> <charspec> }
#        token backslash:sym<d> { :i <sym> { $*CCSTATE := '' } }
#        token backslash:sym<e> { :i <sym> }
#        token backslash:sym<f> { :i <sym> }
#        token backslash:sym<h> { :i <sym> { $*CCSTATE := '' } }
#        token backslash:sym<N> { <?before 'N{'<.[A..Z]>> <.obs('\N{CHARNAME}','\c[CHARNAME]')>  }
#        token backslash:sym<n> { :i <sym> }
#        token backslash:sym<o> { :i <sym> [ <octint> | '[' ~ ']' <octints> | '{' <.obsbrace> ] }
#        token backslash:sym<r> { :i <sym> }
#        token backslash:sym<s> { :i <sym> { $*CCSTATE := '' } }
#        token backslash:sym<t> { :i <sym> }
#        token backslash:sym<v> { :i <sym> { $*CCSTATE := '' } }
#        token backslash:sym<w> { :i <sym> { $*CCSTATE := '' } }
#        token backslash:sym<x> { :i <sym> [ <hexint> | '[' ~ ']' <hexints> | '{' <.obsbrace> ] }
#        token backslash:sym<0> { <sym> }
#
#        # keep random backslashes like qq does
#        token backslash:misc { {}
#            [
#            | $<text>=(\W)
#            | $<x>=(\w) <.typed_panic: 'X::Backslash::UnrecognizedSequence', :sequence(~$<x>)>
#            ]
#        }
#    }
#}

#grammar Perl6::RegexGrammar is QRegex::P6Regex::Grammar does STD does MatchPackageNibbler {
#    token normspace { <?before \s | '#'> <.LANG('MAIN', 'ws')> }
#
#    token rxstopper { <stopper> }
#
#    token metachar:sym<:my> {
#        ':' <?before 'my'|'constant'|'state'|'our'|'temp'|'let'> <statement=.LANG('MAIN', 'statement')>
#        <!RESTRICTED>
#        <.LANG('MAIN', 'eat_terminator')>
#    }
#
#    token metachar:sym<{ }> {
#        <?[{]> <codeblock>
#    }
#
#    token metachar:sym<rakvar> {
#        <?before <.sigil> $<twigil>=[<.alpha> | \W<.alpha> | '(']>
#        <!before <.sigil> <.rxstopper> >
#        <var=.LANG('MAIN', 'variable')>
#        [
#        || $<binding> = ( \s* '=' \s* <quantified_atom> )
#           { self.check_variable($<var>) unless $<twigil> eq '<' }
#        || { self.check_variable($<var>) }
#           [ <?before '.'? <.[ \[ \{ \< ]>> <.worry: "Apparent subscript will be treated as regex"> ]?
#        ]
#        <.SIGOK>
#    }
#
#    token metachar:sym<qw> {
#        <?before '<' \s >  # (note required whitespace)
#        '<' <nibble(self.quote_lang(self.slang_grammar('Quote'), "<", ">", ['q', 'w']))> '>'
#        <.SIGOK>
#    }
#
#    token metachar:sym<'> { <?[ ' " ‘ ‚ ’ “ „ ” ｢ ]> <quote=.LANG('MAIN','quote')> <.SIGOK> }
#
#    token metachar:sym<{}> { \\<[xo]>'{' <.obsbrace> }
#
#    token backslash:sym<1> {
#        <.[\d] - [0]>\d*
#        {}
#        <.typed_panic: 'X::Backslash::UnrecognizedSequence', :sequence(~$/), :suggestion('$' ~ ($/ - 1))>
#    }
#
#    token assertion:sym<{ }> {
#        <?[{]> <codeblock>
#    }
#
#    token assertion:sym<?{ }> {
#        '?' <?before '{'> <codeblock>
#    }
#
#    token assertion:sym<!{ }> {
#        '!' <?before '{'> <codeblock>
#    }
#
#    token assertion:sym<var> {
#        [
#        | <?[&]> <!RESTRICTED> <var=.LANG('MAIN', 'term:sym<variable>')>
#            [
#            | ':' <arglist>
#            | '(' <arglist> ')'
#            ]?
#        | <?sigil> <!RESTRICTED> <var=.LANG('MAIN', 'term:sym<variable>')>
#        ]
#    }
#
#    token assertion:sym<~~> {
#        <sym>
#        <!RESTRICTED>
#        [ <?[>]> | $<num>=[\d+] | <desigilname=.LANG('MAIN','desigilname')> ]
#    }
#
#    token codeblock {
#        <!RESTRICTED>
#        <block=.LANG('MAIN','block')>
#    }
#
#    token arglist {
#        <!RESTRICTED>
#        <arglist=.LANG('MAIN','arglist')>
#    }
#
#    token assertion:sym<name> {
#        <longname=.LANG('MAIN','longname')>
#            [
#            | <?[>]>
#            | '=' <assertion>
#            | ':' <arglist>
#            | '(' <arglist> ')'
#            | <.normspace> <nibbler>
#            ]?
#    }
#}

#grammar Perl6::P5RegexGrammar is QRegex::P5Regex::Grammar does STD does MatchPackageNibbler {
#    token rxstopper { <stopper> }
#
#    token p5metachar:sym<(?{ })> {
#        '(?' <?[{]> <codeblock> ')'
#    }
#
#    token p5metachar:sym<(??{ })> {
#        '(??' <?[{]> <codeblock> ')'
#    }
#
#    token p5metachar:sym<var> {
#        <?[$]> <var=.LANG('MAIN', 'variable')>
#    }
#
#    token codeblock {
#        <block=.LANG('MAIN','block')>
#    }
#}

# vim: ft=perl6 et sw=4
