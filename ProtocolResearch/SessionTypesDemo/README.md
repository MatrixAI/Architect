# SessionTypesDemo

This is a demonstration of Session Types, which may be used in the type theory of Architect in future. It is based on a paper about [Gradual Session Types](http://homepages.inf.ed.ac.uk/wadler/papers/gradsess/gradsess.pdf)

## SessionType.hs

### Type

The type `Type` is a placeholder for the types of data that could be communicated over a Session Type. For the current implementation, it only matters that it is an instance of `Eq`, and for testing it only matters that it is an instance of `Show` and `Arbitrary`. Some notion of subtyping in `Type` might be useful in future.

### SessionType

`SessionType` is a recursive data structure for representing one side of a typed communication channel. Its constructors represent the main actions of choosing branches, communicating typed data, and ending the communication.

#### Wait and Kill

These constructors represent ending the communication. `Wait`, often notated end<sub>?</sub> in literature, represents waiting for the other peer to end the communication. `Kill`, often notated end<sub>!</sub>, represents actively ending the communication.

In some literature, these two Session Types are merged into a single Session Type, notated end. This implementation may be changed to match that in future.

#### Send and Recv

These constructors represent the communication of data. `Send t a`, often notated ? t . a, or t <?> a, represents sending data of type `t` to the other peer, then continuing with Session Type `a`. `Recv t a`, often notated ! t . a, or t <!> a, represents receiving data of type `t` from the other peer, then continuing with Session Type `a`.

#### Choose and Offer

These constructors represent branching to multiple possible Session Types. Unlike many notations, there can be any positive integral number of branches, and they are labelled with strings. This is based on the notation presented in http://homepages.inf.ed.ac.uk/wadler/papers/gradsess/gradsess.pdf.

`Choose m`, often notated b + d, b ⊕ d, +{a:b, c:d}, or ⊕{a:b, c:d}, represents actively choosing one of the Session Types stored as values in the Map `m`, which is keyed by Strings that label each of the options. The notation +{a:b, c:d} would be constructed by `Choose (fromList [("a",b),("c",d)])`.

`Offer m`, often notated b & d, or &{a:b, c:d}, represents offering the peer a choice of one of the Session Types stored as values in the Map `m`, which is keyed by Strings that label each of the options. The notation &{a:b, c:d} would be constructed by `Offer (fromList [("a",b),("c",d)])`.

Neither `Offer` nor `Choose` can have an empty set of options, and no label may be the empty string.

#### Example Session Types

<!-- TODO -->

### isValid

This function simply checks that a Session Type was constructed in a valid manner. It checks that any Maps are validly structured, and that no choices are empty or contain empty labels.

This function must satisfy that if any SessionTypes contained in this one are invalid, then this one also is.

### isCompatible

This function checks if two session types represent a valid communication. This requires that any peers will reach `Wait` and `Kill`, `Send` and `Recv`, or `Choose` and `Offer` at the same time, that the data sent over a `Send` will be the same type as the data received by a `Recv`, and that every `Choose` is a subset of the choices offered by an `Offer`.

`<=>` is an infix operator for `isCompatible`.

This function must be symmetric and reflexive, so `a <=> a` always, and `a <=> b` iff `b <=> a`.

### isSubType

Subtyping in Session Types is based on the requirement that the supertypes of any compatible pair of Session Types should also be compatible. A more precise definition of subtypes in this manner can be found in http://homepages.inf.ed.ac.uk/wadler/papers/gradsess/gradsess.pdf.

`isSubType a b` or `b <: a` is `True` iff `b` is a subtype of `a`.

This function is reflexive, antisymmetric, and transitive, so `a <: a` always, if `a /= b` and `a <: b` then `b <: a == False`, and if `a <: b` and `b <: c` then `a <: c`. The core requirement of subtypes with relation to compatibility is if `a <: b` and `c <: d` and `a <=> c` then `b <=> d`.

### dual

The dual of a Session Type is the most obvious Session Type that it could communicate. It is found by swapping all `Wait`s and `Kill`s, `Send`s and `Recv`s, and `Choose` and `Offer`s.

This function is self-inverse, bijective, and satisfies `isCompatible`, so `dual (dual a) == a` always, `dual a == dual b` iff `a == b`, and `dual a <=> a` always.

Duality inverts subtyping, so if `a <: b`, then `dual b <: dual a`. Also, the dual of a Session Type is the most specific Session Type it is compatible with, so if `a <=> b` then `dual a <: b`.

### union, strictUnion and smartUnion

These functions represent combining multiple Session Types into one that has all the capabilities of the original. It would require some sort of combinator to decide which Session Type to communicate with, and the three functions represent varying amounts of complexity required in the combinator. The function will return `Nothing` if the Session Types cannot be merged.

These functions must be commutative and associative.

#### strictUnion

This function represents a combinator that looks at the first messages only, and can determine exactly one peer that can handle that request. `strictUnion a a` will always be `Nothing`.

#### smartUnion

This function represents a combinator that looks at some of the first messages, where no data is ever sent or received, and can determine some peers that can handle that request. Since multiple peers might be able to handle a request, it can do some degree of load balancing.

`smartUnion` will work whenever `strictUnion` does and act in the same way, so if `isJust (strictUnion a b)` then `strictUnion a b == smartUnion a b`. Also, `strictUnion a a == Just a`.

#### union

This function represents a combinator that looks at all of the messages, including storing any sent data, and may have to switch which peer is handling those messages multiple times throughout the communication. This combinator may be impossible to implement, such as if the responses by different peers are different and affect which branch is chosen.

`union` will work whenever `smartUnion` does and act in the same way, so if `isJust (smartUnion a b)`, then `smartUnion a b == union a b`.

## Spec.hs

This file includes instances of `Arbitrary` for both `Type` and `SessionType`, and then a variety of tests of some examples and properties of the functions.
