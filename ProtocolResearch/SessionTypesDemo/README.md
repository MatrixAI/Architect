# SessionTypesDemo

This is a demonstration of Session Types, which may be used in the type theory of Architect in future.

## SessionType.hs

### Type

The type `Type` is a placeholder for the types of data that could be communicated over a Session Type. For the current implementation, it only matters that it is an instance of `Eq`, and for testing it only matters that it is an instance of `Show` and `Arbitrary`. Some notion of subtyping in `Type` might be useful in future.

### SessionType

`SessionType` is a recursive data structure for representing one side of a typed communication channel. Internally, it is a list containing a recursive structure, and should be constructed using the exposed functions. It is important this is an instance of `Eq`, and for testing that it is an instance of `Show`.

#### End

The function `end` constructs a session type representing no communication. Internally, this is represented as an empty list. For any `a :: SessionType`, `a ++ end == a` and `end ++ a == a`.

#### Send and Recv

The functions `send` and `recv` represent the communication of data, and take a `Type` that is being transferred. These would be notated as !Type and ?Type respectively, though session types notated recursively such as !Type.SessionType and Type <?> SessionType can be implemented by concatenation.

#### Choose and Offer

These constructors represent branching to multiple possible Session Types. Unlike many notations, there can be any positive integral number of branches, and they are labelled with strings. This is based on the notation presented in http://homepages.inf.ed.ac.uk/wadler/papers/gradsess/gradsess.pdf.

`choose m`, often notated b + d, b ⊕ d, +{a:b, c:d}, or ⊕{a:b, c:d}, represents actively choosing one of the Session Types stored as values in the Map `m`, which is keyed by Strings that label each of the options. The notation +{a:b, c:d} would be constructed by `choose (fromList [("a",b),("c",d)])`.

`offer m`, often notated b & d, or &{a:b, c:d}, represents offering the peer a choice of one of the Session Types stored as values in the Map `m`, which is keyed by Strings that label each of the options. The notation &{a:b, c:d} would be constructed by `offer (fromList [("a",b),("c",d)])`.

Neither `Offer` nor `Choose` can have an empty set of options, and no label may be the empty string.

<!-- TODO

#### Example Session Types

 -->

#### Concatenation

`join` is a synonym for `(++)`, since session types are internally lists. Hence, for any session types `a` and `b`, `a++b == join a b`.

#### Fixpoints or Loops

`mu` and `ref` are used to construct loops or fixpoints in a session type, and are shorthand for the notation µt.a. A simple loop that repeatedly does the session type a would be notated as µt.+{loop:a;t,end:end}, and implemented as `mu (choose $ fromList [("loop",a++ref 1),("end",end)])`.

The `Integer` that `ref` takes is a [De Bruijn Index](https://en.wikipedia.org/wiki/De_Bruijn_index), so that µs.µt.s would be implemented as `mu (mu (ref 2))` and µs.µt.t would be implemented as `mu (mu (ref 1))`.

### isValid

This function simply checks that a Session Type was constructed in a valid manner. It checks that any Maps are validly structured, that no choices are empty or contain empty labels, and that all referenced indices are not out of bounds.

### isCompatible

This function checks if two session types represent a valid communication. This requires that any peers will reach `send` and `recv`, or `choose` and `offer` at the same time, that the data sent over a `send` will be the same type as the data received by a `recv`, and that every `choose` is a subset of the choices offered by an `offer`, and that both protocols enter fixpoints at the same time.

`<=>` is an infix operator for `isCompatible`.

This function must be symmetric and reflexive, so `a <=> a` always, and `a <=> b` iff `b <=> a`.

### isSubType

Subtypes in Session Types is based on the requirement that the subtypes of any compatible pair of Session Types should also be compatible.

`isSubType a b` or `a <: b` is `True` iff `a` is a subtype of `b`.

This function is reflexive, antisymmetric, and transitive, so `a <: a` always, if `a /= b` and `a <: b` then `b <: a == False`, and if `a <: b` and `b <: c` then `a <: c`. The core requirement of subtypes with relation to compatibility is if `a <: b` and `c <: d` and `b <=> d` then `a <=> c`.

If this differs from other notions of subtyping for session types, that is because a session type here is the type of an interface that an automaton presents, and so being compatible with a session type implies being compatible with its subtypes. The usual perspective is that a session type is the type of a channel, so being able to communicate using a type of channel implies being able to communicate any subtype. The result of this is that subtyping is flipped from usual, but this perspective is more inutitive for this application.

To check µs.a <: µt.b, we assume s<:t and see if that allows us to deduce a<:b.

### dual

The dual of a Session Type is the most obvious Session Type that it could communicate. It is found by swapping all `send`s and `recv`s, and `choose` and `offer`s.

This function is self-inverse, bijective, and satisfies `isCompatible`, so `dual (dual a) == a` always, `dual a == dual b` iff `a == b`, and `dual a <=> a` always.

Duality inverts subtyping, so if `a <: b`, then `dual b <: dual a`. Also, the dual of a Session Type is the most specific Session Type it is compatible with, so if `a <=> b` then `dual a <: b`.

<!--

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

#### simpleUnion

This function uses the builtin `Offer` constructor to union multiple session types. It has no nice properties.

-->

## Spec.hs

This file includes instances of `Arbitrary` for `Type` and a series of generators for `SessionType`, and then a variety of tests of some examples and properties of session types.
