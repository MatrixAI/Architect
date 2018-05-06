# Compiler Infrastructure

Progress notes on https://github.com/MatrixAI/Architect/issues/14

Created the Core directory. This should represent the language core for now.

Core is sometimes the core libraries.

One could also call it base.

We also have parser, and lexer.

These things will need to be considered as well.

Actually changed to compiler library.

We have brought in the hint library.

---

First we need to learn megaparsec and do some examples of parsing the language.

Then we will check out the hint library and see what we can do.

The megaparsec is used for CSV cassava. 

The error messages are automatic for expected and unexpected. The library can report problems that have to do iwth using methods from the `FromRecord` and `FromNamedRecord` typeclasses.

It transforms ByteStrings into a particular instance of those type classes.

While performing the conversion, things can go wrong.

When parsing strings:

```
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Expr as E
```

The lexer exposes lexing functions, the C exposes parsing functions, and the expression exposes functions to help parse expressions.

So this usually is about operators, and it deals with fixity. Currently it only deals with operators. Not just normal function application. Prefix operators, postfix operators infix operators!

When testing examples, just use `stack ghci ./examples/While.hs`.

The way you use Megaparsec is that you use their generic functions, like `space` and `lexeme` and you specialise it. At that point, your domain of discourse has changed. And you're just using your own language.

Not sure what the difference between symbol and lexeme. But it seems that lexeme wraps a parser and returns a parser a, but specifically makes it a token?

The `Text.Megaparsec` also exports the `Text.Megaparsec.Error`, which allows you to create fancy errors. Megaparsec supports well-typed errors instead of String based ones. The `ErrorItem t` is a data type representing unexpected and expected items in `ParseError`. The constructors are `Token (NonEmpty t)` `Label (NonEmpty Char)` and `EndOfInput`. There's also `ErrorFancy e`.

Then there's `ParseError t e`. Which is a parse error parameterised over the token type `t` and the custom data `e`.

Remember we need to parse inclusions, this reminds me of the include problem of git. How when you include you now want to maintain a tree. So you could make changes to it without any problems.

The `label` or `<?>` takes a string. And `label name p` means that `p` behaves like `p`, but if the parser `p` fails without consuming any input. The names of expected tokens gets replaced with the name `name`. Why would you need this? For error reporting?

The `try` is needed when using on parsers that consume input. If the parser automatically backtracks, then `try` is no longer needed. But if you have complex composite parsers, then `try` is still useful.

The `ShowErrorComponent` is a type class which says that there's an implementation for a given custom error, and it there's pretty printer for that error. The Void type is an instance of the ShowErrorComponent which means that it can be "shown".

Then there's a function `absurd` for the Void type for the `ShowErrorComponent` typeclass.
