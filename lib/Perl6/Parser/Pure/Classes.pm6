class Perl6::Element
  {
  }

class Perl6::Visible is Perl6::Element
  {
  }

class Perl6::Operator is Perl6::Visible
  {
  }

class Perl6::Operator::Hyper is Perl6::Operator
  {
  }

class Perl6::Operator::Prefix is Perl6::Operator
  {
  }

class Perl6::Operator::Infix is Perl6::Operator
  {
  }

class Perl6::Operator::Postfix is Perl6::Operator
  {
  }

class Perl6::Operator::Circumfix is Perl6::Operator
  {
  }

class Perl6::Operator::PostCircumfix is Perl6::Operator
  {
  }

class Perl6::String is Perl6::Visible
  {
  }

class Perl6::Body is Perl6::String
  {
  }

class Perl6::String::WordQuoting is Perl6::String
  {
  }

class Perl6::String::WordQuoting::QuoteProtection is Perl6::String::WordQuoting
  {
  }

class Perl6::String::Interpolation is Perl6::String
  {
  }

class Perl6::String::Interpolation::Shell is Perl6::String::Interpolation
  {
  }

class Perl6::String::Interpolation::WordQuoting is Perl6::String::Interpolation
  {
  }

class Perl6::String::Interpolation::WordQuoting::QuoteProtection is Perl6::String::Interpolation::WordQuoting
  {
  }

class Perl6::String::Shell is Perl6::String
  {
  }

class Perl6::String::Escaping is Perl6::String
  {
  }

class Perl6::String::Literal is Perl6::String
  {
  }

class Perl6::String::Literal::WordQuoting is Perl6::String::Literal
  {
  }

class Perl6::String::Literal::Shell is Perl6::String::Literal
  {
  }

class Perl6::Documentation is Perl6::Visible
  {
  }

class Perl6::Pod is Perl6::Documentation
  {
  }

class Perl6::Comment is Perl6::Documentation
  {
  }

class Perl6::Balanced is Perl6::Visible
  {
  }

class Perl6::Balanced::Enter is Perl6::Balanced
  {
  }

class Perl6::Block::Enter is Perl6::Balanced::Enter
  {
  }

class Perl6::String::Enter is Perl6::Balanced::Enter
  {
  }

class Perl6::Balanced::Exit is Perl6::Balanced
  {
  }

class Perl6::Block::Exit is Perl6::Balanced::Exit
  {
  }

class Perl6::String::Exit is Perl6::Balanced::Exit
  {
  }

class Perl6::Catch-All is Perl6::Visible
  {
  }

class Perl6::Whatever is Perl6::Visible
  {
  }

class Perl6::Loop-Separator is Perl6::Visible
  {
  }

class Perl6::Dimension-Separator is Perl6::Visible
  {
  }

class Perl6::Semicolon is Perl6::Visible
  {
  }

class Perl6::Backslash is Perl6::Visible
  {
  }

class Perl6::Sir-Not-Appearing-In-This-Statement is Perl6::Visible
  {
  }

class Perl6::Number is Perl6::Visible
  {
  }

class Perl6::Number::Binary is Perl6::Number
  {
  }

class Perl6::Number::Octal is Perl6::Number
  {
  }

class Perl6::Number::Decimal is Perl6::Number
  {
  }

class Perl6::Number::Decimal::Explicit is Perl6::Number::Decimal
  {
  }

class Perl6::Number::Hexadecimal is Perl6::Number
  {
  }

class Perl6::Number::Radix is Perl6::Number
  {
  }

class Perl6::Number::Rational is Perl6::Number
  {
  }

class Perl6::Number::Real is Perl6::Number
  {
  }

class Perl6::Number::FloatingPoint is Perl6::Number
  {
  }

class Perl6::Number::Complex is Perl6::Number
  {
  }

class Perl6::NotANumber is Perl6::Visible
  {
  }

class Perl6::Infinity is Perl6::Visible
  {
  }

class Perl6::Regex is Perl6::Visible
  {
  }

class Perl6::Bareword is Perl6::Visible
  {
  }

class Perl6::SubroutineDeclaration is Perl6::Bareword
  {
  }

class Perl6::ColonBareword is Perl6::Bareword
  {
  }

class Perl6::Adverb is Perl6::Visible
  {
  }

class Perl6::PackageName is Perl6::Visible
  {
  }

class Perl6::Variable is Perl6::Visible
  {
  }

class Perl6::Variable::Scalar is Perl6::Variable
  {
  }

class Perl6::Variable::Scalar::Contextualizer is Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::Dynamic is Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::Attribute is Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::Accessor is Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::CompileTime is Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::MatchIndex is Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::Positional is Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::Named is Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::Pod is Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Scalar::SubLanguage is Perl6::Variable::Scalar
  {
  }

class Perl6::Variable::Array is Perl6::Variable
  {
  }

#class Perl6::Variable::Array::Contextualizer is Perl6::Variable::Array
#  {
#  }

class Perl6::Variable::Array::Dynamic is Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::Attribute is Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::Accessor is Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::CompileTime is Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::MatchIndex is Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::Positional is Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::Named is Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::Pod is Perl6::Variable::Array
  {
  }

class Perl6::Variable::Array::SubLanguage is Perl6::Variable::Array
  {
  }

class Perl6::Variable::Hash is Perl6::Variable
  {
  }

#class Perl6::Variable::Hash::Contextualizer is Perl6::Variable::Hash
#  {
#  }

class Perl6::Variable::Hash::Dynamic is Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::Attribute is Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::Accessor is Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::CompileTime is Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::MatchIndex is Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::Positional is Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::Named is Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::Pod is Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Hash::SubLanguage is Perl6::Variable::Hash
  {
  }

class Perl6::Variable::Callable is Perl6::Variable
  {
  }

#class Perl6::Variable::Callable::Contextualizer is Perl6::Variable::Callable
#  {
#  }

class Perl6::Variable::Callable::Dynamic is Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::Attribute is Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::Accessor is Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::CompileTime is Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::MatchIndex is Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::Positional is Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::Named is Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::Pod is Perl6::Variable::Callable
  {
  }

class Perl6::Variable::Callable::SubLanguage is Perl6::Variable::Callable
  {
  }

class Perl6::Invisible is Perl6::Element
  {
  }

class Perl6::WS is Perl6::Invisible
  {
  }

class Perl6::Newline is Perl6::Invisible
  {
  }

class Perl6::Document is Perl6::Element
  {
  }

class Perl6::Statement is Perl6::Element
  {
  }

class Perl6::Block is Perl6::Element
  {
  }
