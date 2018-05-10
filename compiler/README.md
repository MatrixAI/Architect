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

The `sepEndBy` is actually part of `Control.Monad.Combinators`. Here we go.

We really should be using the quasiquoter to get the usage of the multlines.
Otherwise it's quite annoying.

How do you get megaparsec to work on text? Or do you have to use byte string?

We use the quasiquoters to more easily write test cases for our expressions. Just for small embedded expressions.

This is actually one way to embed architecte expressions is to use quasiquoters.

We should ppsh to really print this stuff properly.

Next we use Indentation Sensitive Parsing.

So we can have proper indentation system.

---

We need to go through parsec to megaparsec.

Text.Megaparsec - Contains common useful tools

Text.Megaparsec.Char - when youa parsing stream of characters

Text.Megaparsec.Byte - bytes

Text.Megaparsec.Perm - Permutation phrases?

Text.Megaparsec.Expr - Exprssions?

Also char lexing and binary lexing.

`count' m n p` allows you to parse from m to n occurences of p.

failure and fancyFailure can have errors with custom data

Unicode parsing:

asciiChar
charCategory
controlChar
latin1Char
...

StateT and WriterT are instances of MonadParsec. So if you want to collect contents of comments. If they are documentation strings, then you may want to have backtracking userstate where you put last encountered comment satisfying some criteria and then when you parse function definition you can check the state and attach a doc-string to your parsed function. It's all easy.

```
import Control.Monad.State.Lazy

-- wait...?

type MyParser = StateT String Parser



skipLineComment' :: MyParser ()
```

The `StateT s m a` means you can have a state and then the inner monad. Remember you wrap monads in monad transformers. Allowing state usage and further stuff.

If you want to do isomorphic translations we need an ability to map from CST to AST. And we need a CST for the language itself managed as Haskell system!

The CST would need to be indexed using tree order index or other structure. So a CST is one way, and then an index is an encoding of a tree data structure.

Monad transformers of the parsect will slow it down. So we should not use too many. How should we index that? Maybe down the line thing.

Inlien generously. Especially for short functions. Parsers that are defined in one module and used in another because INLINE and INLINEABLE prgamas make GHC dump function definitions into an interface file and this facilitates specialising!

Use fast primitives `takeWhileP` and `takeP` and `takeWhile1P`. These are fast for text and bytestring. Efficient operations exist for producing Text from Text and ByteString from ByteString.

https://markkarpov.com/post/megaparsec-more-speed-more-power.html#there-is-hope - this just describes the Stream typeclass and how it is made generic over all possible stream like things that Megaparsec can parse, so that's strings, text and bytestrings for now. The basic primitives of the typeclass enable all sorts of more abstract parsing combinators.
