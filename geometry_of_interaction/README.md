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
