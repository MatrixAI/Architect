# Geometry of Interaction

Let's start with implementing http://semantic-domain.blogspot.com/2012/11/in-this-post-ill-show-how-to-turn.html into Haskell.

Interesting the resumption concept seems related to my coroutine work!?

* https://gist.github.com/CMCDragonkai/9f665940c4aeaa934498155736cda9cc
* https://gist.github.com/CMCDragonkai/9a8b8eefe083df02b0743496b448604d
* https://gist.github.com/CMCDragonkai/01ebcdf7f51afcc94b028215a28fd2ff

---

Ok so I just realised.

In the G category. The morphisms are:

Interface -> Interface.

That is it.

And an Interface is defined as `(A+, A-)`.

The `A+` is what messages you send out. And the `A-` is the messages you receive.

Alternatively in software modules. `A+` is the data types you supply while `A-` is the data types you need.

So We could say:

```
category: G

morphism: Interface -> Interface

Interface: (Send, Receive)
```

So how is this implemented in the G construction?

It's:

```
newtype G a b' a' b = G (R (a :+ b') (a' :+ b))
```

So these are the objects? I'm really confused!

No! Those are the morphism! The `newtype G a b' a' b` is the MORPHISM.

The individual "object" here for this morphism is literally...

`(a :+ b')`.

As the article explains. The morphisms are `(A+, A-) -> (B+, B-)`.

So an object is `(A+, A-)`.

So then they are representing the morphism as a "wrapped" (remember the usage of newtype is like a pattern of repurposing an existing type for a totally different scenario, it just avoids having to define things all over again).

But if we would write it from scratch we could do something like:

```
data Interface a b = Interface a b
morphism :: Interface a b -> Interface c d
morphism = undefined
```

But here we are representing a morphism as a single type here, instead of a literal function. Because a morphism isn't literally a function. At least not yet. It's a more sophisticated map here.

Thus a morphism is represented as a resumption between the `Resumption (Either A+ B-) (Either A- B+)`.

What's weird here is that the resumptions usually work with Either types like taking the Left to the Left. So `A+ => A-`. Which sort of makes sense when you have `(A+, A-)` as a Interface type.

So you can see that such a resumption resembles the idea of `A+ -> A-` and `B- -> B+`.

But I still don't know why the interface is flipped.

Why is the idea of `(A+, A-) -> (B+, B-)` somehow represented as a resumption of `Either A+ B- Either A- B+`?

Where a resumption is ultimately a function from `Either A+ B- -> (Either A- B+, (Either A+ B- -> Either A- B+)`.

"We are transforming A messages out to B messages out, and we need to accept B messages in order to transform them into A messsages in".

```
type l :+ r = Either l r

newtype R i o = R (i -> (o, R i o))
type i :-> o = R i o

newtype G a b' a' b = G ((a :+ b') :-> (a' :+ b))
```

```
-- i still don't understand how this maps to mapping an interface to another interface
r : Either A+ B- -> (Either A- B+, r)
```

Ok so if taking Left A+ means that you can transform them to Right B+.

But Right B- you can transform into Left A-.

Bidirectional communication right? Are we saying that this represents 2 different things. A and B, both having an interface pair.

```
  +--> A+ B- -->+
 /               \
A                 B
 \               /
  +<-- A- B+ <--+
```

Maybe that's what it means. When we are doing some sort of morphism here, we are really talking about bidirectional communication. So really what we are doing is mapping `A+ B-` which is 2 parts of a single communication between A and B things. Then sending it back means `A- B+`. This is a bit of a stretch really.

Composition between morphisms.

Why does the G category need morphism between interfaces? What is the point? Is the whole point of morphism between interfaces intended to actually achieve real composition between the interfaces?

There seems to be a paper that explores this in more detail: http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=EE4060D190A0E0E9FE2E8DF0A8E3A869?doi=10.1.1.46.7024&rep=rep1&type=pdf

```
R = X -> Y * R
```

Defines the mathematical space of "resumptions". The states of tranducers are unfolded into their observable behaviours.

There is a notion of `~` which means behavioural equivalence between tranducers and for any transducer.

The category R of resumptions (we limit to deterministic resumptions) has as objects sets and as morphisms:

```
R(X,Y) = X -> Y * R(X, Y)
```

We implement at the type level in Haskell.

Composition of 2 resumptions is defined coinductively. What does coinductive mean?

```
-- using ; as the composition operator of resumptions
-- which makes sense as a monadic bind operator!
f;g(x) = (z, f';g') where f(x) = (y, f'), g(y) = (z, g')
```

This `;` represents a "sequence" or "series" composition of transducers. Are we saying resumptions is one form of transducers or is the generalisation of transducers?

Ok so the objects are sets. In Haskell these are types. (Where types can be like sets of instances). Then the monoid of the objects is just `X + Y`, the disjoint union of sets. In Haskell, the disjoint union is the Either type. Since it's a tagged union. I see, so it's not that we are actually using Either for some semantic relevance, and tagged union is sufficient.

Thus you can also then apply this operator to the morphisms as well.

```
X * Y = X + Y
f: R(X,Y)
g: R(X',Y')
f * g = R(X * X', Y * Y') 
```

This is (asynchronous) parallel composition of transducers. Right this is just like linear algebra where there is "sequence" operator and parallel composition. Represented as `* + and ;`. In GLA, we have `+` and `;` as the operators. But I can see things like `;` as sequential composition of resumptions and `*` as the parallel composition of resumptions.

At each stage, we can take an input from X wire according to f, and output on Y wire. And at the same time, we can take an input from X' wire according to g and output to Y' wire.

This paper was first published in 1996.

The objects in the G category are pairs of objects in C. The idea is that `A+` is the type of moves by player, while `A-` is the type of moves made by the opponent.

A G morphism is encoded as a parallel composition of 2 R morphisms.

See that:

```
(A+ + B-) -> (A- + B+) = f || g

f : A+ -> A-
g : B- -> B+
```

Why do we have the polarities swapped? Because a G object is meant to describe an interaction. Usually an interaction from the perspective of 1 process. A morphism mapping 1 interaction to another interaction turns out to need the simultaneous passing of data from. `A+` and `B-`.

So basically G morphisms have both serial composition and parallel composition as well. And the parallel composition for a R morphism is basically the idea that both types can be transferred at the same time. "Asynchronous parellel composition of transducers at each stage we respond on the X wire according to f, with output appearing on the Y wire". Basically parallel composition just means both can happen at the same time, but this seems to be interpreted in programming as a function that process both kinds of data at the same time, making that function a bit more generic.

The paper says that "interaction is modelled as composition in the category G". Interaction is then aligned with the computation as cut elimination paradigm. Remember that cut elimination is basically where you can remove transitive paths between the beginning and the end. Ah yes so serial composition is literally interaction between 2 processes! Oh... so then.. are we saying that serial composition between f and g morphisms in G is producing another interaction? But what is f and g then? They are morphisms between input/output types? And so a single morphism that maps from 1 input/output to another output/input is in fact an interaction. Oh I see now... the G morphism is the interaction. And then serial composition allows you to compose interactions! Oh that is cool.

The `assoc` and `assoc2` is actually referring to:

```
assoc: A+ * C- * B- * B+ === A+ * B- * B+ * C-
assoc2: A- * B+ * B- * C+ === A- * C+ * B- * B+
```

---

The object of (X+,X-) being the input/output types of a process. Can also be thought of as a 2 person game. A resumption is then a strategy for a player. `f: X- -> X+`. This means we receive a message and send a message.

A set of plays can be constructed from this: `P(f)`. Because remember a resumption takes input and returns output plus itself. You can then construct a set of plays. Passing an x, k times into the `f`. And you get `y` k times out of it. Thus you end up a set like : `{x1y1,...,xkyk}`. Which is a sequence of messages received and messages sent out. Basically the input and output log of a process.

Composition of interactions in the G category is given by parallel composition plus hiding of the resumptions.

```
P(f;g)
```

Can we use `;` in our operators. Not in haskell, because it is special. We have to use `>>` and `<+>` and `<*>`. Wait we can reuse the monoidal typeclass and use the `<>` operator as well for the product operation. But we

If `-> a` is a monoid. Then... Yea we could definitely use `<>` and define a monoid over it. But which one is the monoidal operator? The parallel composition is the monoidal structure.

For this to work, we would need to define an instance for those morphisms. But both are bifunctors. Sequential composition of operators. Wait.. to be a bifunctor it has to map back to an object internally.

Ah yes there is `Sum` and `Product` newtypes in `Data.Monoid`. So we could just define it for them and make use these wrappers when we want to do a serial composition vs a parallel composition. But this means we use `newtype` wrappers and define them be instances of `Monoid`. And then we unwrap them however we want. Oh we need to create our own newtype wrappers. I see.

If we think of these things as functors, we could instead use the applicative instance. I think that should be possible as well. Instead of always using the same `<>` operators then. https://stackoverflow.com/questions/29499119/why-int-does-not-implement-monoid

Ok anyway, the above models means that we can think of them as dynamic systems in which information is a token or particle that traces around a path in a network.

In the above we just created a model for functions. But a type system based on this could be used for interface types as well.

It is also possible to give an interpretation in which an information wave travels through the network. This is supported by a multplicative carteisan product interpretation of the tensor. Interesting ,the idea is that means we have a Cpo category of cpos and continuous functions. The category G(Cpo) and then a subcategory of this category consists of dataflow networks. Where the objects are domains of streams. This is interesting, I'm not sure where this leads to and what relevant it has to FRP.

In continuous systems, the feedback is interpreted by solving a differential equation. There should be a traced monoidal category C of manifolds and smooth maps for which G(C) would give an infinitesimal model of interaction. This category may be relevant to the study of hybrid systems. I wonder what this has to do with compiling to categories?

This now allows you model functions as processes. Function application is a particular form of process interaction as advocated by Robin Milner. now we have a highly structured syntax free and compositional.

Ok I think I can try to reimplement these things in GoI in Haskell now.

The point is that now higher order expressions (lambda terms) can be "compiled down" or intepreted as a lower order compositions of resumptions. Maybe that's what compiling to categories allows?

Apparently the ability to update behaviour based on previous inputs means that we can have "memoryful" resumptions? And this allows effectful computaitons.

---

```
-- this is the definition of a morphism
-- but it's also a bifunctor
-- because it can refer to a single typed object
-- so you can create a bifunctor here
-- and make a bifunctor applicative
-- wait is this a bifunctor? It's actually possible to be profunctor
-- it's a profunctor
-- because we take an i and return an o
-- it's a profucntor!
-- dimap (c -> a) -> (b -> d) -> f a b -> f c d
-- f a b is a morphism
-- f c d is another morphism
-- so this maps 1 morphism to another morphism

-- category of resumptions allows you to model processes
-- that can change as it sees input coming in
-- this really resembles a state monad as well
-- given that the state updates is itself a resumption
-- in that sense, the state is used an input
-- remember the state action signature is s -> (a, s)
-- where s is the input and s gets updated, and we get output
-- so this seems to generalise it
```

Note that `Resumption` is a profunctor. So we could apply things like dimap to it. But `dimap` does have to be careful. Since what we are doing is transforming the `Resumption` morphism by making changes to it. We gain access to `dimap`.

If you use `infix`.

---

So I've written the curry and uncurry functions.

And they are related to the lollipop operator which is intended to define a linear exponential which turns a traced monoidal category into a closed monoidal category.

Curry and uncurry are functors. There are natural transformations between them. Here we have a curry as a functor that takes a hom-set of H(A*B, C) to a hom-set H(A, C^B).

We are able to translate hom-set cat-theory notation to just into an interaction. That is an interaction is both H(A*B, C) and H(A, C^B). We can see that in `curryInt`. The cat theory notation deals with 3 objects: A, B, C. Whereas the Interaction deals with 6 types. Those 6 types are actually in pairs.

We can see that category theory `*` is encoded as a pairwise either of `a a'` and `b b'`. Subsequently the linear exponential of `C^B` is encoded by using a pairwise either of `b b'` and `c c'`. However we swap the `b'` `b` to the left side and their respective positions.

Doing this sort of makes sense, since an Interaction is already encoded as a resumption of Eithers. You can see that curryInt takes an Interaction and produces another Interaction. The first Interaction encodes a morphism between `A*B` and `C`, but the second Interaction encodes between `A` and `C^B`. And it works because an interaction itself is a Resumption that has that weird ordering.

An interaction is already a resumption between 2 eithers. And by embedding eithers into the interation types, we now have a higher order language as interactions can be embedded into interactions.

Oh... interesting

The lollipop operator is the linear exponential. And that can get defined as just a series of types in a particular structure that gets embedded into an interaction. The fact that it can be embedded in an interaction makes it interesting. The embedding itself produces a higher order language.

A closed monoidal category has the right adjoint functor called curry (it is right adjoint to the right tensoring functor `*`), and IF you are able to construct this functor in your language/category, then this means you have a higher order language or a closed monoidal category. That's how you prove that you have what you need!

---

Time to try to continue with module systems. How does this all work in that context?

http://semantic-domain.blogspot.com/2012/12/total-functional-programming-in-partial.html

Most of us prefer lambda calculus to SKI combinators for good  reason. Programs with variables and binders are much easier to read. All operations in lambda calculus can be encoded via abstraction elimination into SKI calculus as binary trees whose leaves are one of the three symbols S, K, and I.

We want to see how to compile linear lambda calculus into combinator terms. TO do this, we need to introduce the basic ideas in a simpler setting. So we are not compiling linear types yet.

The total language here is System T, which is a first proper total functional programming language. Extending simply typed lambda calculus with a type of natural numbers, and iteration over them. This system is remarkably expressive.

It is simply typed lambda calculus with natural numbers and iteration defined over them.

In OCaml, it models it as a type signatures as well. With the usage of a module signature using `id : ('a, 'a) hom`. Where `hom` is a type. That takes the `('a, 'b)`.

So the idea is that this represents an interface that some sort of module is going to implement. These are the type signatures in a way. To do this in the Haskell way, we can just have individual type signatures for our functions. Or we can define a typeclass some how, but it doesn't make sense.

I don't know why we call it Hom.

The linear lambda calculus where variables are restricted to occur in terms exactly once, has a very weak expressive power. In particular all functions terminate in linear time. In this paper we consider a simple extension with natural numbers and restricted iterator. Only closed linear functions can be iterated. We show properties of this linear version of System T using closed reduction strategy, and study the class of functions that can be represented. Surprisingly this linear calculus offers a huge increase in expressive power over previous linear versions of System T which is closed at construction, rather than closed at reduction.

System T is a theory of arithmetic in all finite types used in Goedel's Dialectica interpretation. It seems that SystemT has some relationship to linear lambda calculus.

Abstraction elimination is the process of turning regular lambda calculus terms into point-free combinators.

Extensions on simply typed lambda calculus:

1. System T
2. PCF
3. System F
4. LF

Higher order abstract syntax.

What is the computational power of linear calculus without the exponentials. A calculus that is syntactically linear: all variables occur exactly once. This is a severely restricted form of the (simply typed) lambda calculus. Computation is deifned by the usual beta-reduction rule.

We introduce pairs and natural numbers with the corresponding iterator to obtain a linear version of Godel's System T. Ok so system T already exists, and then we geta linear version of this. We call this System L. System T is an extension of simply typed lambda calculus with numbers and a recursion operator. Its power comes essentially from primitive recursion combined with linear higher order functions.

In a correctness proof for geometry of interaction, Girard uses a strategy ofor cut elimination where cut elimination steps can only take place when exponential boxes are closed. Not only is htis strategy for cut elimination simpler than the general one, it is also exceptionally efficient in terms of the number of cut elimination steps.

Linearity can be defined in the 3 main ways:

1. Syntactical - linear use of variables in terms and it is the computational counterpart of linearity in linear logic
2. Operational - redexes cannot be deuplicated during evaluation
3. Denotational - linear functions can be defined in the language

Apparently syntactical linearity is best.

Denotational semantics is mathematical and equational. The details of the reduction matter less than the end result which is a timeless value in some mathematical space.

Operational semantics is algorithmic. It unfolds in individual steps in time. The process is part of the meaning, and the end result is just a distinguished step in that process.

> To start with, denotational semantics wants to say something like "the meaning of this notation is that". A real semanticist would want to imagine that the meanings are what exist in our mind and the notations are just a way of expressing those meanings. The requirement that denotational semantics should be compositional follows from this. If the meanings are primary and the notations secondary, then we have no choice but to define the meanings of bigger notations as functions of the meanings of their constituents.
> https://cstheory.stackexchange.com/questions/3577/what-constitutes-denotational-semantics

Ok so that's interesting.

In terms of implementing the module signature, we are not even going to try to "abstract" this implementation, and just straight forward implement it. This means `Hom a b` is actually just `a -> b`. So `type Hom a b = a -> b`.

So the GoI exercise was to show how to derive a higher order language from a first order language. We did this by extending the first order language until we got a higher order language. It is implemented as a bunch of combinators. Assume if we have a lambda calculus of some sort, then we can compile it to the combinators. This means we can use this higher order language on the backend (and it will be easier to manipulate), but we present a more easier to use language on the front end with variable binders and such.

So this article http://semantic-domain.blogspot.com/2012/12/total-functional-programming-in-partial.html basically produces a similar problem. Where we end up with a higher order combinator language. But it's hard to use. So instead we then define an abstract syntax that has let bindings. It uses OCaml metaprogramming to later allow us to write this in OCaml and then compile it straight to the combinator language. We shouldn't really need to use this metaprogramming (but I guess it's a useful exercise to learn how to embed a language into OCaml). But we actually want an external DSL. I'm pretty sure the same internal DSL is achievable just by using ADTs (but I guess it would not be truly embedded with metaprogramming template haskell.) But since we are working on a external DSL this should be possible.

1 is the unit type. pi1 and pi2 are fst and snd, they are pair projections. The e is the syntax of expressions. Whereas the A is the syntax of types (it is common to use capitals to represent types and lowecase to represent expressions). We also have extra syntax in the module language.

Here we are going to follow a Lisp like strategy in embedding the syntax of our language. We will be reusing Ocaml syntax for types and programs. So our parser is just a function that takes OCaml AST and converts it into elements of term and tp data types. In our program we have represented as ADT. But they use `tp` and `term`. Camlp4 lets us match quotations in pattern match expressions so this is easy.

Here `<:ctyp<t>>` just matches an AST with the same shape as term t. Holes inside the pattern can be bound to variables using the notation `$x$`. We can also do the same thing with expressions using the quotation form `<:expr<e>>`. As a result all the issues of precedence is handled automatically. I'm not sure how to use template haskell, so we need to figure out how ignore these sorts of things.

But here they are:

```
  let rec tp = function
    | <:ctyp< nat >> -> Nat
    | <:ctyp< unit >> -> One 
    | <:ctyp< $a$ -> $b$ >> -> Arrow(tp a, tp b)
    | <:ctyp< $a$ * $b$ >> -> Prod(tp a, tp b)
    | _ -> failwith "unhandled type expression"

  let rec term = function
    | <:expr< () >>           -> Unit
    | <:expr< ($e1$, $e2$) >> -> Pair(term e1, term e2)
    | <:expr< (fst $e$) >>    -> Fst (term e)
    | <:expr< (snd $e$) >>    -> Snd (term e)
    | <:expr< Zero >>         -> Zero
    | <:expr< Succ($e$) >>    -> Succ(term e)
    | <:expr< iter (match $en$ with
                     | Zero -> $ez$ 
                     | Succ $lid:x$ -> $es$) >> ->
        Iter(term en, term ez, x, term es)
    | <:expr< let $lid:x$ : $t$ = $e1$ in $e2$ >> ->
        Let(x, Annot(term e1, tp t), term e2)
    | <:expr< let $lid:x$ = $e1$ in $e2$ >> ->
        Let(x, term e1, term e2)
    | <:expr< ($e$ : $t$) >> -> Annot(term e, tp t)
    | <:expr< $lid:x$ >>      -> Var x
    | <:expr< fun $lid:x$ -> $e$ >> -> Lam(x, term e)
    | <:expr< ($e1$ $e2$) >>    -> App(term e1, term e2)
    | _ -> failwith "Unknown expression"
```

At the end of this, this is the end of the `module Term`.

We can see that `tp` basically turns quoted expressions and turns them into the equivalent thing inside their ADT. In the case of `tp` that's a TType constructor. Then with `term` it takes a quoted expression (note the usage of `<:expr< () >>` is used to reprepresent the Unit constructor). and turns it into the TTerm constructor.

So really it's a parser. This is literally a parser.

But we don't want to use this, so we just want to work from the constructors themselvees.

But the problem is that... the `check` system seems to produce quoted expressions!? At least within a Context monad!

That happens in the Quote module. This uses quotations as a meta-language.

Things like:

```
module Quote = 
struct
  let id = <:expr< Goedel.id >>
  let compose f g = <:expr< Goedel.compose $f$ $g$ >>
  let unit = <:expr< Goedel.unit >>

  let fst = <:expr< Goedel.fst >>
  let snd = <:expr< Goedel.snd >>
  let pair f g = <:expr< Goedel.pair $f$ $g$ >>
  let prod f g = <:expr< Goedel.prod $f$ $g$ >>

  let curry f = <:expr< Goedel.curry $f$ >> 
  let eval = <:expr< Goedel.eval >> 

  let zero = <:expr< Goedel.zero >> 
  let succ = <:expr< Goedel.succ >> 
  let iter z s = <:expr< Goedel.iter $z$ $s$ >> 

  let rec find x  = function
    | [] -> raise Not_found
    | (x', t) :: ctx' when x = x' -> <:expr< Goedel.snd >> 
    | (x', t) :: ctx' -> <:expr< Goedel.compose Goedel.fst $find x ctx'$ >>
end
```

Here we have rewritten the quoted expressions... that seems to give us expressions of the `Goedel` module's expressions.

This says that they want to construct terms of ML type `Hom a b`. The Quote module gies a set of functions which construct AST nodes corresponding to each of the combinators in our interface. How does this correspond to the syntax that we we will be using!?

Note the usage of `Term.term`. It can refer to the type or function. Since it is defined as both. So I think when used in a type signature, it refers to the types. When used in expression space it refers to the function.

In the `Elaborate` module, it opens the Term module, and we end up seeing it use `Term.term` only in the type space.

In the `Extension` module, we get an actual usage of `Term.term` which is applied to something...

It says that they can now use metaprogramming which lets them splice into our DSL into ordinary OCaml terms. This extends OCaml's grammar with T(t) construct which constructs of type `Hom () a`.

You can see something like:

```
match Context.run (Elaborate.synth (Term.term tm)) with
| Context.Done (e, _) -> e 
| Context.Error msg -> Loc.raise _loc (Failure msg)_
```

Then in the `Test`  module, you can see that's when they use their DSL inside `e1` and the `e2`. You can also see that they use this `T` prefix wrapping the entire program. Basically that's like their quasiquoter that would be used inside Template Haskell. In haskell it looks liek:

```
v :: String
v = [something| ... some DSL here ... ]
```

It's pretty cool!

Ok so we are not going to do that then...

Then why use `return unit` for the Elaborate.

It says that a elaborator, we don't just type check the term, we also produce a term in the output language as part of the type checking process. Why do we do that?

Ok so interestingly the syntax extension actually performs `synth` not check. Instead the `synth` ends up calling the `check`.

Ok so if we are doing `synth` on the `term`, this means we get literal expressions. Then we use the `Term.term` to first convert the raw expressions into things like `Unit` and `Pair` which are the constructors of TTerm. Weirdly I don't see usage of things like `Term.tp` as a function... 

Oh actually I see. `Term.term` as a function actually calls `Term.tp`. Because the term language becomes annotations. You can see things like:

```
| <:expr< let $lid:x$ : $t$ = $e1$ in $e2$ >> ->
        Let(x, Annot(term e1, tp t), term e2)
```

Here we can see that if we have `$lid:x$ : $t$ = $e1$ in $e2$`, then that is translated into an ADT using `Let`, but it also calls `tp` as a function on the `t` type. Thus giving us what we want. Ok so this parser is the entrypoint parser.

Ok so we can assume that `term` has already been done. So we already have an algebraic structure. So the `synth` takes the algebraic structure, and returns:

```
synth :: Term.term -> Context.t (Ast.expr, Term.tp)
```

So we are taking the algebraic structure and returning within the same Context monad a tuple of the elaborated expression and the type. The check function takes a term and type to check it against, and returns elaborated code. The synth function takes a term and it infers a type for that term AND also in addition emitting code for that term.

Ok I get it, an elaborator both checks the type, and returns the output code. So it's like going from a high-level language to a low-level language.

Many programming languages and proof assistants are defined by elaboration from a high-level language with a great deal of implicit information to a highly explicit core language. Or translating a user-accessible language into a smaller core language. Apparently the smaller language can be type checked more easily.

Many typed languages include both a type checker and a type elaborator. The type elaborator translates source code to an explicitly typed representation for the type checker to validate. Normally, programmers cannot extend the behavior of the type elaborator without modifying the compiler.

Bidirectional type checking? Means consider a judgement `t: A`, it means to split into 2 judgements:

1. `t <= A`: the term t can be checked to have the type `A`
2. `t => A`: the term t we can infer or synthesise the type A it has

In the case of `t <= A`, we consider both `t` and `A` to be inputs to our checking function. Whereas in the case of `t => A`, we only consider the `t` to be the input to our synthesise function.

So the algorithm ends up being `check(, A)`. And `synth(t)`.

Ok so we are doing these 2, but we are also elaborating when we do the `check`. It's sort of interesting that we end up producing our core language when using the ADT.

```
High Level Language -> SystemT ADT -> Core Combinators
```

So that's why we have 3 levels. But I still find it weird, that we end up using the Ocaml syntax extension again to wrap the core combinators.

Oh... `Context.t (Ast.expr, Term.tp)`.

And what is `Context.t`.

It's this:

```
-- Ctx is a list of (variables and types)
-- it's the typing context I think
type Var = String
data Ctx = [(Var, TType)]
type T a = Ctx -> Either String a


-- when we run the program, we are running with an empty context
-- the program has no other environment variables within the system
-- so we expect that the program is a t
run :: T
run program = program []
```

Left indicates error, Right indicates it is done. And we are done with a result expression and a type. And we are still using the macro system to wrap our combinators. Even though we should be able to construct a combinator language. So basically instead of using that `hom`, we should be using another ADT for our Hom language!

There you go...

So we want to do something where we are doing `Context.t (TComb, TType)`.

Because it's a lower level expression language that has the same typing rules. So it uses the same type. But we now have a lower level language. Going from `TTerm` to `TComb`.

```
synth :: TTerm -> Context (TComb, TType)
```

But the TComb language is weird. How do I get a combinator language?

We start with a Hom. And some specific Homs, then we have `curry`, `eval` that work only on Homs. So the language are constructs that work on Homs. I think this makes sense. But we need to define several kinds of Homs.

Our system needs to return TComb, but TComb doesn't seem to make sense as a language. It's constructs applies to a single type, that is the Hom. Which always 2 type variables. How do I do things like composition? It's like we have to discard the types some how.

```
data Expr a b = Expr (Hom a b)
```

Existential types?

No we are meant to use GADTs.. that's right.

Ok I see now. What we have previous is a SystemTLambda. The other is SystemTCombinator.

> In computer science, combinatory logic is used as a simplified model of computation, used in computability theory and proof theory. Despite its simplicity, combinatory logic captures many essential features of computation.

> Combinatory logic can be viewed as a variant of the lambda calculus, in which lambda expressions (representing functional abstraction) are replaced by a limited set of combinators, primitive functions from which bound variables are absent. It is easy to transform lambda expressions into combinator expressions, and combinator reduction is much simpler than lambda reduction. Hence combinatory logic has been used to model some non-strict functional programming languages and hardware. The purest form of this view is the programming language Unlambda, whose sole primitives are the S and K combinators augmented with character input/output. Although not a practical programming language, Unlambda is of some theoretical interest.


After System T... we will eventually understand System F (https://stackoverflow.com/questions/25255413/how-did-haskell-add-turing-completeness-to-system-f).

> The term simple type is also used to refer to extensions of the simply typed lambda calculus such as products, coproducts or natural numbers (System T) or even full recursion (like PCF). In contrast, systems which introduce polymorphic types (like System F) or dependent types (like the Logical Framework) are not considered simply typed.

> Some compilers for functional languages (not GHC!) use combinators and supercombinators as intermediate steps in compilation. As with any similar compiler technology, the reason for doing this is to admit optimization analysis that is more easily performed in such a simplified, minimal language. One such core language built on supercombinators is Edwin Brady's epic.


---

For some reason, find and lookup functions in the example end up constructing an infinite type. This is because we are using GADTs, and Compose on Fst and Snd... etc, produces a different THom type.

This is not allowed in Haskell. So we have 2 options.

We can nwrap the THom into existentially quantifie variables like:

```
data TExpr = forall a b. TExpr (THom a b)
```

But this just exposes an outside type that encapsulates the a and b. But now we have no constraints on the a and b, so we know nothing about what to do with internal types. But this still doesn't solve the problem.

The find and lookup functions ultimately take a tvar and a context, an try to produce projection expression. I don't even understand why this is necessary. The sync is mean to take a term and synthesize the type. But here it's an elaborator as well. So it returns some target expression and the type as well.

For some reason a `Var TVar` SystemTLambda expression is meant to be converted to a series of composed projection expressions. Why is this? Why is the SystemTLambda somehow a projection expression!?

The projections is a algebraic way of representing https://en.wikipedia.org/wiki/De_Bruijn_index. That is a `Var v` is like `hello` in the raw syntax. So `hello` is the variable, thus we have `Var "hello"` in SystemTLambda. It's a lexical identifier. To convert this to SystemTCombinator, combinators don't have variables. So instead we need to "refer" a specific position in the combinator language to get that variable value. To do so, we use projection expressions as they allow us to go from pairs to some single thing.

Man that is pretty weird.

The OCaml example takes raw syntax exposed as OCaml macros that is translated to Ocaml AST. It then converts that AST to SystemTLambda. Finally it takes SystemTLambda to SystemTCombinator. The `Term.term` and `Term.tp` is what takes OCaml AST to SystemTLambda.

`synth` takes SystemTLambda and turns them into SystemTCombinator and type. We are sharing the same types between SystemTLambda and SystemTCombinator.

`check` takes SystemTLambda and types and gives us back SystemTCombinator as well.

For some reason, that's wrapped in `Ast.expr`.

The Extension module uses `Term.term` as a function to convert the OCaml AST to SystemTLambda. This is passed to `Elaborate.synth` and finally `Context.run`. The result is still another OCaml AST, but this time an AST of SystemTCombinator.

So we just went from raw OCaml Syntax -> OCaml AST -> SystemTLambda -> OCaml AST of SystemTCombinator.

There is no evaluation of the combinators. It's just left as an AST.

Then that AST can be interpreted, and evaluated. That's the `Goedel.run`. But it would have to run within the AST system some how.

Ok in that light, it seems to make sense that our type should be recursive. Since we need an AST type essentially.

But how do you create an AST type of combinators?

```
data AST = Something (THom a b) ...
```

But those types will be encapsulated in the AST.

---

To work on this on Windows, we need to use stack, as that's the only way around it. So the stack needs to make use of the package.yaml.

Oh I'm just using a simple shell.nix. And we got bifunctors there as well. Also we are only using base haskell for this as well.

I remember now, it's just a simple situation with the Haskell environment. There's not even really a package.yaml for this, since it's not a package.

---

@CMCDragonkai Whoops wasn't notified — I'd use plain algebraic data types. Basically, just remove the type parameters from Expr/THom etc. A few more functions will become partial, and you'll need more tests. You can still store (object-language) types inside the AST, so that you can run a "retypechecker" that checks (at runtime) your AST can be typed in System T. That's the most common choice in compilers/interpreters. – Blaisorblade Nov 22 at 20:21

So there are 3 solutions to this infinite type problem:

1. The approach in the original post is to use Template Haskell - find would return Q.Exp, representing some Hom a b. A type error would be caught when checking the template haskell code. Basically the template haskell macros would also be type checked. Type errors are still caught before running the expressions. However tests would be needed to ensure that the macros don't produce ill-typed expressions.
2. Dependent typing or GADTs in the input and output. So basically this means making input dependently typed. This requires faking the dependent typing using GADTs and singletons.

```
data Exp a where
  Lam :: (Exp a -> Exp a) -> Exp (a -> b)
  App :: Exp (a -> b) -> Exp a -> Exp b
  Var :: String -> Exp a
```

Right now the `Lam TVar TTerm`. So we take a variable, and repeated tterm. But the above shows that you actually take a haskell function and embed it into the Exp.

So then when we write this sort of language, we are just directly writing things like `Exp (\a -> a)`. We are just writing a simple version of Haskell, and just putting it inside the Exp container.

So it becomes a domain specific language using the exact same syntax as Haskell. This becomes useless as we are not even doing our own typechecker anymore. Instead we just create an interpreter that interprets that into SystemTCombinator calculus. This is what they meant is that the input is already typed, and the the output stays typed.

Then the third option is to make the input weakly typed, and the output strongly typed. As is what I am thinking since we take raw text and try to parse it. I'm assuming I've already parsed it.

Context.lookup uses Quote.find. The lookup is just a matter of finding if a tvar exists in the context. It's meant to take the string and return us a readererror monad. This monad takes a context and returns a either string a.

So we can give it the context and run it, and it should return us some valeu. In this case we want the systemtcombinator term and type. The type is shared between the 2 representations, but the term is different. That's why the we have `(Quote.find x ctx, List.assoc x ctx)`.

So I think we go back to the drawing board. Not use GADTs at all.

> I'd use plain algebraic data types. Basically, just remove the type parameters from Expr/THom etc. A few more functions will become partial, and you'll need more tests. You can still store (object-language) types inside the AST, so that you can run a "retypechecker" that checks (at runtime) your AST can be typed in System T. That's the most common choice in compilers/interpreters

Ok so if THom has no type parameters. We just need to work on them from the term level. We store "object language types" (the types on the language we want inside the AST) so we run the type checker at runtime.

---

```
data THom = IdH
          | UnitH
          | SuccH
          | ComposeH THom THom
          | PairH THom THom
          | FstH
          | SndH
          | CurryH THom
          | EvalH
          | IterH THom THom
```

So we learned that we cannot just use GADTs or stuff to encode what we want. Instead we want to use just plain algebraic data types, but that means you can construct failing things. But that's fine since we are going to runtime typecheck it anyway! That's the whole idea.

I remember now that I used powershell. That creates a environment by loading envrionment variables. It also creates directory and sets up some environment variables. But because there's no concept of shell.nix, then I expect the environment to exist on Windows already.

Anyway, we removed the type parameters from the type constructor. We are just keeping them as the same type. And instead such type parameters are encapsulated within the constructors. We use tagged constructors to pattern match what to do.

That's the `THom` type which includes many Hom constructors. And it's a `THom`, as in system T homomorphism. It is a combinator style language. An abstract data type for programs in System T. 

In this language, we have all those combinators...

SystemT composition is `>>>` not the same.

Ok I see. It does make sense now that `ComposeH ZeroH SuccH` is needed

Then you also need

Hom a unit COMPOSE Hom unit nat

Note that `compose unit zero` makes sense as well. Ok...

```
ComposeH UnitH ZeroH
```

So we have this idea. Alternatively we have `H` constructors. I actually don't think constructors should be qualified like this. Constructors don't need to be qualified, only within their modules. And that's already done!

But THom is a fine type.

Ther'es a function that can take `Hom unit nat -> Int`. Does that still make sense? It says that 

```
run :: Hom () Nat -> Int
run e = loop $ e ()

loop = \case
  Zero -> 0
  Succ n -> 1 + loop n
  
Ok that only makes sense if our constructor inside is another n. I don't thin run makes sense anymore.
```

The run function is supposed to take a Hom that takes a unit to natural number. And basically resolve the peano numbers down to the original nubmer. But to do this, our Succ or composition of Homs will need to be something that can be unit to natural number. For that to even make sense, you have to do something else entirely. You can no longer assume the types are correct. So if you take a THom, you have to pattern matching on everything, for most are fails. But you can if you can break down. Before the construction gives you a THom is already collapsed. That is the compose function just returns a nother function. But here we have a ComposeH, and we would have to do something there. I think this doesn't make sense.

The construction is not resolved, we keep building up a tree!

On the STC we just expose the THom and constructors of our language. Our language is necessarily weakly typed. But we check this and ensure that we have a correct system afterwards and intepret accordingly!

On error handling. The standard is to use MonadError here from the mtl package. But there's also MonadThrow from the exceptions package from Ed Kmett, which appears to extend on that. Don't use Monad fail.

https://www.reddit.com/r/haskell/comments/3zbgn0/exceptions_best_practices/

Seems to recommend to use MonadCatch and MonadThrow instead.

Using the exceptions package, we get to use `throwM :: (Exception e, MonadThrow m) => e -> m a`.

---

1. Control.Monad.Except (mtl)
2. Control.Monad.Catch (exceptions)
3. Control.Exceptions (base)

Those are your 3 choices, prefer 2.
