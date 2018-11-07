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
