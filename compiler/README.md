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

---

So we need to have declarations that deal with just the `A = ...` bindings. And we need to deal with that here.

Top level declarations following the style of the write you a haskell. That's one idea.

The write you a haskell focuses on function declarations first. Maybe we should try that first as well. Because the usage of functions is bit strange. And we need to parse that as well. Basic functions.

While we want to be able to write declarations.

The hnix repository starts with a docs directory that has the `expr_function`. Which is a production head that comes from expr.

I looked for something similar in the haskell code since I don't think they are using YACC grammar directly.

In the `Nix/Expr/Types.hs` they have `data NExprF r` which is the main nix erxpression type. It's a polymorphic type so that it can be mdade a functor. Which allows them to traverse expressions and map functioms over them. The actual `NExpr` type is a fixed point of this functor.

```
data NExprF r = ...
type NExpr = Fix NExprF
```

They say that the monomorphic expression type is a fixed point of the polymorphic one. How does that work?

The Fix type allows you to do:

```
Fix f = f (Fix f)
```

The type f should be a Functor if you want to use simple recursion schemes or Traversable. This allows you express recursive functions in non-recursive manner.

We can try to do it based on how Nix was written. In a similar way basically. Since the language is sort of based on Nix but expanded with the properties that we want. And expanded with the concept of constructors!

Grammars are usually defined recursively. So we have expressions like:

```
data Expr = Const Int
          | Add Expr Expr
          | Mul Expr Expr
```

The recursion can be abstracted away to uncover the real primitives behind expressions. The trick is to define a non-recursive function and then find its fixed point.

We use a type function that is non recursive (and thus represents an anonymously recursive function).

```
data ExprF a = Const Int
             | Add a a
             | Mul a a
```

To use this we can now pass `ExprF` into itself, and we can get deeper trees. After infinitely more iterations, we get to a fixed point where further iterations make no difference.

Evaluation is a recipe for extracting a single value from an expression. In order to evaluate expressions which are defined recursively, the evaluation must proceed recursively as well.

However we can abstract away the recursion, instead all we need is an evaluation strategy for each of the top level constructs and a way evaluate its children. This non recursive top level evaluator is `alg` and the recursive one is the `eval`. Both `alg` and `eval` return values of the same type `a`.

So by making `ExprF` a functor, we can now map `eval` over the functor.

Wait so if we didn't do it this way, we wouldn't be able to to use generic functions like fmap to help evaluate the code!

Basically yea it's all about generics being able to use catamorphisms generalised folds on the evaluation tree.. That's basically it.

Haskell strict syntax. You can do `![...]` inside that constructor.

Oh it's a list. A strict list of antiquoted stuff. So we are basically saying that there are multiple lines to it.

We found where the parser is. They are using ParsecT instead of Parsec. I wonder why.

---

Use `parseTest` to test small individual parsers and see where it goes.

The language can have `blah`.


note that nix allows \' and - in their identifier names
i think that's too liberal and easy to confuse
we must not allow identifiers to be in reserved names

the first letterChar must not be capitals
caps are reserved for

Ok we are building from the Nix parser.
