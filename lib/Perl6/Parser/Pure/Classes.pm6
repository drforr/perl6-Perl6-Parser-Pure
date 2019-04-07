class Perl6::Element
  {
  }

class Perl6::Visible extends Perl6::Element
  {
  }

class Perl6::Operator extends Perl6::Visible
  {
  }

class Perl6::Operator::Hyper extends Perl6::Operator
  {
  }

class Perl6::Operator::Prefix extends Perl6::Operator
  {
  }

class Perl6::Operator::Infix extends Perl6::Operator
  {
  }

class Perl6::Operator::Postfix extends Perl6::Operator
  {
  }

class Perl6::Operator::Circumfix extends Perl6::Operator
  {
  }

class Perl6::Operator::PostCircumfix extends Perl6::Operator
  {
  }

class Perl6::String extends Perl6::Visible
  {
  }

class Perl6::Body extends Perl6::String
  {
  }

class Perl6::String::WordQuoting extends Perl6::String
  {
  }

class Perl6::String::WordQuoting::QuoteProtection extends Perl6::String::WordQuoting
  {
  }

class Perl6::String::Interpolation extends Perl6::String
  {
  }

class Perl6::String::Interpolation::Shell extends Perl6::String::Interpolation
  {
  }

class Perl6::String::Interpolation::WordQuoting extends Perl6::String::Interpolation
  {
  }

class Perl6::String::Interpolation::WordQuoting::QuoteProtection extends Perl6::String::Interpolation::WordQuoting
  {
  }

class Perl6::String::Shell extends Perl6::String
  {
  }

class Perl6::String::Escaping extends Perl6::String
  {
  }

class Perl6::String::Literal extends Perl6::String
  {
  }

class Perl6::String::Literal::WordQuoting extends Perl6::String::Literal
  {
  }

class Perl6::String::Literal::Shell extends Perl6::String::Literal
  {
  }

class Perl6::Documentation extends Perl6::Visible
  {
  }

class Perl6::Pod extends Perl6::Documentation
  {
  }

class Perl6::Comment extends Perl6::Documentation
  {
  }

class Perl6::Balanced extends Perl6::Visible
  {
  }

class Perl6::Balanced::Enter extends Perl6::Balanced
  {
  }

class Perl6::Block::Enter extends Perl6::Balanced::Enter
  {
  }

class Perl6::String::Enter extends Perl6::Balanced::Enter
  {
  }

class Perl6::Balanced::Exit extends Perl6::Balanced
  {
  }

class Perl6::Block::Exit extends Perl6::Balanced::Exit
  {
  }

class Perl6::String::Exit extends Perl6::Balanced::Exit
  {
  }

class Perl6::Catch-All extends Perl6::Visible
  {
  }

class Perl6::Whatever extends Perl6::Visible
  {
  }

class Perl6::Loop-Separator extends Perl6::Visible
  {
  }

class Perl6::Dimension-Separator extends Perl6::Visible
  {
  }

class Perl6::Semicolon extends Perl6::Visible
  {
  }

class Perl6::Backslash extends Perl6::Visible
  {
  }

class Perl6::Sir-Not-Appearing-In-This-Statement extends Perl6::Visible
  {
  }

class Perl6::Number extends Perl6::Visible
  {
  }

class Perl6::Number::Binary extends Perl6::Number
  {
  }

class Perl6::Number::Octal extends Perl6::Number
  {
  }

class Perl6::Number::Decimal extends Perl6::Number
  {
  }

class Perl6::Number::Decimal::Explicit extends Perl6::Number::Decimal
  {
  }

class Perl6::Number::Hexadecimal extends Perl6::Number
  {
  }

class Perl6::Number::Radix extends Perl6::Number
  {
  }

class Perl6::Number::Rational extends Perl6::Number
  {
  }

class Perl6::Number::Real extends Perl6::Number
  {
  }

class Perl6::Number::FloatingPoint extends Perl6::Number
  {
  }

class Perl6::Number::Complex extends Perl6::Number
  {
  }

class Perl6::NotANumber extends Perl6::Visible
  {
  }

class Perl6::Infinity extends Perl6::Visible
  {
  }

class Perl6::Regex extends Perl6::Visible
  {
  }

class Perl6::Bareword extends Perl6::Visible
  {
  }

class Perl6::SubroutineDeclaration extends Perl6::Bareword
  {
  }

class Perl6::ColonBareword extends Perl6::Bareword
  {
  }

class Perl6::Adverb extends Perl6::Visible
  {
  }

class Perl6::PackageName extends Perl6::Visible
  {
  }

class Perl6::Variable extends Perl6::Visible
  {
  }

class Perl6::Variable::Scalar extends Perl6::Variable
  {
  }

class Perl6::Variable::Scalar::Contextualizer extends Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::Dynamic extends Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::Attribute extends Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::Accessor extends Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::CompileTime extends Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::MatchIndex extends Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::Positional extends Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::Named extends Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::Pod extends Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::SubLanguage extends Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Array extends Perl6::Variable
  {
  }

#class Perl6::Variable::Array::Contextualizer extends Perl6::Variable::Array
#  {
#  }

class Perl6::Variable::Array::Dynamic extends Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::Attribute extends Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::Accessor extends Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::CompileTime extends Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::MatchIndex extends Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::Positional extends Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::Named extends Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::Pod extends Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::SubLanguage extends Perl6::Variable::Array
  {
  }

class Perl6::Variable::Hash extends Perl6::Variable
  {
  }

#class Perl6::Variable::Hash::Contextualizer extends Perl6::Variable::Hash
#  {
#  }

class Perl6::Variable::Hash::Dynamic extends Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::Attribute extends Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::Accessor extends Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::CompileTime extends Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::MatchIndex extends Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::Positional extends Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::Named extends Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::Pod extends Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::SubLanguage extends Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Callable extends Perl6::Variable
  {
  }

#class Perl6::Variable::Callable::Contextualizer extends Perl6::Variable::Callable
#  {
#  }

class Perl6::Variable::Callable::Dynamic extends Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::Attribute extends Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::Accessor extends Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::CompileTime extends Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::MatchIndex extends Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::Positional extends Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::Named extends Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::Pod extends Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::SubLanguage extends Perl6::Variable::Callable
  {
  }

class Perl6::Invisible extends Perl6::Element
  {
  }

class Perl6::WS extends Perl6::Invisible
  {
  }

class Perl6::Newline extends Perl6::Invisible
  {
  }

class Perl6::Document extends Perl6::Element
  {
  }

class Perl6::Statement extends Perl6::Element
  {
  }

class Perl6::Block extends Perl6::Element
  {
  }
