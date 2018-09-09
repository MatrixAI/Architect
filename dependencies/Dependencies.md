# Dependency Management

We can establish some shared generalities and features of dependency management across different languages and package managers and ultimately distributed system composition.

Many languages have the concept of "modules". A module is a unit of code that exposes an interface. The code is namespaced so that accessing the interface requires going through a "module name". The module is an abstract concept and may be implemented with a single file, or a multitude of files or a directory or some other kind of namespace. Sometimes a module are [first class concepts](http://wiki.c2.com/?FirstClassModules), and other cases it is a separate system for encapsulation and linking. A module may be in the same language or a completely different language. We are not interested in all the language features of modules, just simply the dependency management of modules. We'll call the space between 2 modules the "module boundary". So calling from 1 module to another is calling through the module boundary. 

Another shared concept is a "package". Packages are generally more featureful than modules, often packing together multiple modules along with associated metadata. For the purposes of dependency management, packages is what associates a module or a set of modules with versioning information. Along with a package is the concept of a version controlled repository.

Note that both Go and Java both use the terminology in reverse. In their systems, a module is a "package" (a namespace), and a package is a "module" (a portable bundle of versioned code).

So currently we create modules, put them in a package, version control them in a repository, and manage them via a package manager and an associated package index.

Let's break down these concepts further, and see where we can improve on this. This is going to use a pseudo-Haskell like language to illustrate the concepts in a syntax-driven way.

```
import A

A.foo
```

When a language imports a module according a module name and later uses it in a body of code, they are doing several things: "lambda abstraction", ["name binding"](https://en.wikipedia.org/wiki/Name_binding), "scope" and eventually ["substitution"](https://en.wikipedia.org/wiki/Substitution_(algebra)).

Therefore the above code is equivalent to a lambda function with an `A` name binding scoped to the module body which is often determined by the rest of the file contents (although there's no reason why you couldn't have multiple modules within a file delimited by brace syntax):

```
\A -> A.foo
```

Modules are often themselves named, sometimes via the name of the file in which the module resides (Python) or through a specific module syntax (Haskell). Modules tend to be automatically mutually recursive. So from this point of view, the module is closer to a named function.

```
ModuleB \A -> A.foo
```

Where `ModuleB` is the name of the function. As a clarifiation, the `A` would be considered a bound variable in `ModuleB`. Compare this against a language that doesn't have modules, any usage of `A` would instead be a free variable. This would be similar to languages that only have [inclusions as a form of code unit composition](https://en.wikipedia.org/wiki/Include_directive) which is now considered very primitive form of implementing modular programming as it makes it difficult to track [code provenance](https://en.wikipedia.org/wiki/Data_lineage).

At this point the evaluator will attempt to substitute `A` for some computational content during interpretation or compilation. This is similar to early/static binding in programming languages, or static linking in compiled languages or beta reduction in lambda calculus. 

```
ModuleB ModuleA
```

Here we `ModuleA` is just a meta-placeholder for the computational content that would be supplied to `A`. So if `A` was intended to be a module supplying a single number like `3`. Then the above should be `ModuleB 3`

Here are some real examples. The NodeJS interpeter tries to find `A` in a `node_modules` directory. Go tries to find `A` in its `$GOPATH` environment variable. The exact specifics are not important. The important part is that evaluators tend to try to resolve `A` according to some local state.

Namings things can be difficult, because of the chance of [naming conflicts](https://en.wikipedia.org/wiki/Identifier#Implicit_context_and_namespace_conflicts). The usual humn way of resolving name conflicts is further namespacing such as using `superName.subName`. In some cases name conflicts can be automatically resolved by using [alpha conversion so that all names become unique (such as single static assignment)](https://en.wikipedia.org/wiki/Name_resolution_(programming_languages)#Alpha_renaming_to_make_name_resolution_trivial), this eliminates all [contextual ambiguity](https://en.wikipedia.org/wiki/Ambiguity), flattening contextual hierarchies and makes it easier to do all sorts of optimisation.

A package manager basically substitutes a value that has computational content for `A` into that local state before the evaluator tries to substitute `A` within the expression. This sort of design is a separation of concerns with regards to dependency management, where the evaluator is concerned with local dependencies while the package manager is concerned with distributed dependencies (dependencies over a network). This results in 2 levels of name resolution, first at the package manager using its package index and later at the evaluator. These 2 levels of name resolution means there is now the problem of [cache invalidation](https://en.wikipedia.org/wiki/Cache_invalidation). The name resolution at the evaluator level can become out of date if the dependencies were updated upstream, and these updates were not yet propagated from the package index to the evaluator name resolution mechanism.

Package indexes are generally implemented using a [name service](https://en.wikipedia.org/wiki/Directory_service) of some sort. JavaScript NPM uses the website npmjs.com that resolves names to saved code archives. While Go uses the Domain Name System to resolve to version controlled respositories. What we can see here is that package index name services tend to be centralised key-value systems (with varying [consistency models](https://en.wikipedia.org/wiki/Consistency_model)) controlled by large organisations. But this is not the only way name services can be constructed. Alternative name service architectures include peer-to-peer service discovery mechanisms (DNS-SD, mDNS, Gossip protocol), or blockchain key-value systems (Namecoin), or [content addressable storage](https://en.wikipedia.org/wiki/Content-addressable_storage) (Nix, IPFS), and everything that uses a [distributed hash table](https://en.wikipedia.org/wiki/Distributed_hash_table). For example the [Dhall language](https://github.com/dhall-lang/dhall-lang) used to relies on IPFS for distributing units of code.

We can evaluate the design of various name service architectures against a criteria of several dimensions. [Zooko's triangle](https://en.wikipedia.org/wiki/Zooko%27s_triangle) lists 3: human meaningful, secure and decentralised. A human meaningful name is a name that a human can easily remember and type into a computer. Secure means "self-authenticating", that is given a name-value pair, you can verify on your own that the mapping is correct. This can be done if the name is a content-hash, and you are able to derive the same hash from the value (e.g. IPFS and Nix). Alternatively the name can encode a public key id, and the value encodes a digital signature, then you rely on public key cryptography to verify the value against the relevant public key. In both cases you do not need to trust a third party to authenticate that name. Decentralised means there's no single point of failure for name resolution. We can see that while NPM and Go uses human meaningful names, neither NPM nor Go's package manager satisfies the secure and decentralised criteria. Beyond these 3 dimensions, we also have what we discussed above which is the cache invalidation problem. The cache coherency protocol used will significantly impact on how we manage __change__ during development and production.

An extra dimension would also be discoverability of name mapping of software modules. This inverts the mapping, and allows one to search names given characteristics of the value. This is outside the scope of this article, however it deserves its own analysis.

Before we can design an ideal dependency manager, we also need to consider the different trade offs required between development and production. During development we want fast trial and error cycles. Quite often when solving a new problem where the solution design is still in flux, we don't solve it linearly block by block, instead we make small changes to multiple parts of the system, and we want to be able to quickly test the integration of all these changes. In a single monolthic repository, this is usually easy. However since [code organisation tends to mirror organisational structure](https://en.wikipedia.org/wiki/Conway%27s_law), we also have a competing desire to modularise our modules into small packages that can be consumed and worked on by independent teams distributed throughout the world. Every act of increasing modularity trades "flexibility" for increased communication overhead. This is communication in every way: from pushing bytes around the network, to communicating how module-boundary interfaces are evolving. Once the project is ready to be deployed, the tradeoffs change such that reliability, reproducibility and security becomes more important. Our production project must be reliable, so that it must be robust against changes in the environment. It must be reproducible so we humans can guarantee what we are delivering, and debug our code when what we delivered doesn't match the requirements. It must be secure because software exploits may be discovered in the future, and we must be able to easily apply security patches.

Nix is purely functional package manager that attempts to address the design of packages and package indexes. The Nix evaluation model also presents an interesting way of structuring substitution. 

Each package in the Nix system is a Nix expression that describes how to build a package, Nix calls these expressions "derivation expressions". They are later translated to "store derivations" which are intended to be deterministic/pure/reproducible, so that evaluation of the store derivation on the same inputs should return the same output. We use the word "same" here loosely, it is not always about bit-reproducibility! [What is considered "equal" depends on the context](https://en.wikipedia.org/wiki/Homotopy_type_theory#Equality).

The difference between a derivation expression and store derivation is described in the [PhD thesis about Nix](https://nixos.org/~eelco/pubs/phd-thesis.pdf). The focus here is on dependency substitutions that have taken place. Nix derivation expressions are still quite "open" in that they contain identifiers that have yet to be substituted for some computational content. Once they become store derivations, they become more "closed" (like a "closure"). This enhances their purity and determinism by minimising potential variability. The way this is done, is that almost every identifier gets substituted, and its computational content gets embedded into the resulting store derivation, or gets stored as a separate derivation into a content addressed `/nix/store` path. This means there is less chance for something that the derivation relies on to not exist or not be available when the final build action takes place. 

The `/nix/store` acts like an orthogonally transparent cache, where every output can be stored. This usually means that if you manage to run a derivation to its final form at least once, then you should be able to run it again with no problems assuming nothing was deletd from the `/nix/store`. The reason failures may occur on the first evaluation of the store derivation is because some derivations relies on upstream artifacts that are not available by default and require fetching from upstream sources. Using content address based name resolution with the orthogonally transparent cache allows Nix to share dependencies between different build outputs.

The transformation from a Nix derivation expression to a store derivation is done through `nix-instantiate`. The transformation of a store derivation to a build output is done through `nix-store --realise`. These stages of transformation thus resemble multi-staged evaluation. At each step identifiers in the program are being substituted for computational content, until you've evaluated to just a value or [bottom](https://en.wikipedia.org/wiki/Bottom_type). This staging isn't unique to Nix, every program goes through variations of this concept, Nix just makes this staging explicit and useful for package management.

The Nix package index is provided by a central repository of Nix expressions. However creating your own package index is just a matter of writing a set of Nix expressions and making it available on the local filesystem or over the network. The usage of human readable and writable Nix expressions in contrast with machine generated store derivations and content addressed `/nix/store` artifacts is intended to match the development to production workflow. With regards to Zooko's triangle, Nix uses namespaced human readable names in its package index. Names can be made secure in order to create "pure expressions" by using content hashes and "fixed output derivations". Decentralisation of the package index is made possible by making the package index just a set of Nix expressions that people can synchronise with version managers or by mirroring the package set. Once the Nix expressions become derivations, that's when the tooling focuses more on machine workflow (content hashes) and tries to remove as much contextual ambiguity from generated data as possible. Remember that even the usage of `import ./somethingelse.nix` becomes staged and embedded into the `/nix/store`, such that while at human interaction level, the `./something.nix` can be swapped out, but inside the `/nix/store`, the associated derivation has `./something.nix` substituted for immutable computational content.

When we are developing, we want to be able to iterate and swap out things and test the composition. This is what the `shell.nix` workflow is about: rapid iteration (but reproducible environments). When moving to production, we want to fully specify our dependencies and convert them into a closure, which is what our `default.nix` is about. The `shell.nix` doesn't try to substitute everything, because in a development environment, what we are doing is very contextual. For example different developers may want to use different text editors, so the `shell.nix` doesn't isolate the environment away from the environment provided text editor, nor does it mandate a specific text editor.

But how do we manage the cache coherency/cache invalidation between development and production? How do we quickly swap out an old dependency in production for a newer dependency due to security changes. The answer isn't to throw away all the content addressing goodness, but by augmenting the development workflow with greater automation. The Nix community uses bots https://github.com/NixOS/nixpkgs/pull/43200#issuecomment-403280455 that scans the package index and upstream sources to notify and automatically submit pull requests that updates packages to the latest version to the package set repository. A human can easily accept this pull request, and then push a new build that contains the new dependency to production.

Not all dependencies are build dependencies, nor runtime dependencies. Some are dependencies mediated over a network. That is dependencies that is always going to be not controlled. Dependencies where you expect mutation to occur, because if the resource was never mutated, you could just embed its constant value into your program and discard the cost of going over the network to fetch the resource. In these cases you often rely on external constraint that on what that dependency is. In the case of web resources, you rely on the domain name system and HTTPS.

---

Enough research, you need to build the dev environment that allows rapid linking between different projects when you're deving, and allows fixing the links with more constraints later. Better would be typed constraints, but absent these type constraints between the module boundaries means you rely on more information collapsed constraints, like hashes, and then version constraints. But if you're just prototyping, we don't really want to fully specify the interface, but allow the interface to be discovered. Basically structure discovery stage (exploratory programming) vs building within an existing structure. The idea is to be able to seamlessly move between prototyping to production. Nix can help us get some of the way there, while also making use of as much sharing as possible.

Dynamic dispatch and polymorphism are thus part of the same thing! Polymorphism of module linking is important.

A generalised tool to resolve names according to constraints from free form constraints to versioned constraints to content hash constraints. With the naming scheme coming in a decentralised manner so that directory structure doesn't matter, thus requiring something akin to network multicast service discovery. If that's not possible, then a centralised system that doesn't require specific project directories, requires something that maintains the truth or local package index, equivalent to a search indexing tool that scans the filesystem and places them into the index. A nix/store is capable of doing this, except you'll need to maintain a "OS" or workspace level package index, that can be essentially the user-profile. The user profile represents the workspace level package index.

Decentralised service discovery tends to be used when the modules need to discover each other in an automated fashion (usually in loosely defined environments). This entails more "agency" and complexity to software modules. This is used when the system needs to be very flexible and work in every environment. While centralised package indexes should be more centralised when it's more about consensus between a community. This implies more control over what a name means. My opinion is that the local evaluator should be able to "discover" modules more flexibly, while the remote package index, should be centralised but should exhibit all dimensions of zooko's triangle. Basically human readable names should be mediated via whatever https://hackernoon.com/ten-terrible-attempts-to-make-the-inter-planetary-file-system-human-friendly-e4e95df0c6fa but the actual names are then done in a content addressable manner in order to satisfy the secure names and reproducibility. To address security in the face of change, this is where extra automation and intelligence is needed. The best example of this is Nixpkgs bots, which automatically track upstream projects, automatically submit changes, and then tests the integration of that package set before allowing a human to make a decision to bring in that code. This is more of a pull based package index compared to a push based package index, and a pull model works nicely more flexibility. That is the naming system supports both push and pull. For things that is acquired elsewhere, pull it. (Pulling is better for lower latency! Polling), while pushing is better for higher latency expectation.

To:

1. Develop a better development environment setup for Matrix AI projects
2. Inform the design of Architect's dependency management.

---

Exploring module systems. It is apparent that there are 2 competing techniques for expressing large scale structure of programs. The brand leader is the 2 level approach in which a language has 2 layers. A core language and a module language. I would argue that with packaging systems, and we end up with multiple levels of languages, one expressing a single language, and then a module language for stitching up together modules of that language, and then another one stitching together remote modules (as packages), and then finally a language that composes different-language modules and packages together. This means for Architect we are attempting a form of first class modules. This also means Nix also has first class modules. But of course Nix is untyped. Nix has first class modules very trivially, since a module is just any attribute set that contains a set of functions.

There are some interesting ideas here (Fully-parameterized, First-class modules with Hygenic Macros):

* Full parameterisation - enables the programmer to choose and exchange imports of a module at anny time during development without modifying the module definition or relying on external programs to assemble the program. With full parameterisation, it is also possible to link several instantiations of the same module against different providers within the same program.
* First-class modules - extend the program at run time, define locl name spaces and enable programmable linking.
* Independent compilation (really independent evaluation or independent instantiation) - permits the development of modules even if imported modules are not yet available yet.
* Code sharing - reduces the size of object code (or in our case just code/data size by sharing the code of modules (the nix store artifacts) with identical static semantics).

In first class modules in typed languages, the interfaces are parameterised record types, and the implementations are records.

Hey separating interfaces and implementation into separate files seem a lot like C, and something you sort of want to avoid having to manage different files. Although you do this with Automatons essentially by allowing one to define protocol specfication outside of an Automaton declaration, and then referring to them.


This paper: The Geometry of Interaction as a Module System is actually really good for explaining this.

---

Go's minimal version selection approach is really just a matter of package discovery, a process of going from an abstract name to more concrete name, the most concrete would be a content hash. And so choosing to do minimal selection may be a valid way, but is independent problem from other areas. In the case of Nix, the selection strategy is not covered, instead you are just meant to figure which specific hash you want. Using repology.org and bots is another way to do it as well that doesn't really conform to semver. https://research.swtch.com/vgo-mvs

Backpack, 1ML, OCAML objects vs modules.

---

Geometry of Interaction (GOI) to ML style module systems.

The OI was proposed by Girard in the context of Linear Logic.

Interacting entities to be programs from the restricted computational model, then such modules can be used for compilation. Intepretation of higher order programs in the model becomes translation to the restricted computiational setting.

GOI constructs an ML-style module system even for very simple programming languages. Few assumptions and applies in particular to low level languages that capture computation in some restricted universe of computation.

The ML style modules are a convenient formalism (syntax + semantics) for working with GOI constructions.

A module system can be seen as a higher order generalisation of systems level linking. It has zero overhead in applications that just need stnadard systems level linking, but it can also handle higher order linking. We use the approach of Fing modules for the definition of the module system.

First order types of integers, disjoint unions.. etc. And there's also mutually recursive functions.

```
fn  fact_aux(x: int, acc: int) -> int {
  if x = 0 then return acc
  else let x = fact_aux(x - 1, acc * x) in return x
}

fn fact(x: int) -> int {
  let y = fact_aux(x, 1) in return y
}
```

At the very least, one would like to split larger programs into several files. A standard approach to do this is to use the linker provided by standard operating systems. Individual files compile to object files, which contain the information which functions are defined in them and which external functions are being used in them. 

The interface X of an object file consists of 2 sets: X- and X+, that contain function names defined in the module and the external functions used in the module respectively. Right so `X-` is the signatures supplied, and the `X+` is the signatures required. Just like in Automaton interface which supplies protocols and yet requires protocols as well. In a way they rely on a protocol defined elsewhere (which is both the depending and the dependent require access to the same protocol) The protocol spec becomes a common dependency.

X is dependent on Y. What do you call X? Then X is the consequent, and Y is the antecedent. Dependent is something that depends on another thing. "dependent".

Depender depends on the dependee!

The system linker usese this information to glue several object files together. One issue is of course doing a sort of topological sort. And then making sure we don't have circular dependencies.

This is called a "weak first order module system".

They say it's possible to define a basic ML module system as a syntactic sugar on top of the first order langauge. First we have "structures" which are records that collect function definitions from the first order language. A simple example is:

```
struct
  fn fact_aux(x: int, a:int) -> int { ... }
  fn fact(x: int) -> int { ... }
  S = struct
    fn f(x: A) -> B { ... }
  end
end
```

Notice that struct here is recursive.

The type of a structure is a signature that records the types of all public components. This is quite similar to Haskell modules where the `where` represents the record of all public exports. But we don't have a separate sig, it's derived from the type signatures of the function itself. Where you hae types, you need to have schema evolution!

```
sig
  fact_aux: int * int -> int
  fact: int -> int
  S: sig f: A -> B end
end
```

Imagine a database of functions!

Functors are parameterised structures. In the simplest case, a functor allows one to write a program that uses external functions. Ah ok, so what this means is that we can now "explicitly" type what we NEED. In our module. This similar to the Nix callPackage design pattern.

```
functor(X: sig f: int -> int end) ->
  struct
    fn f(x: int) -> int { let y = X.f(x + 1) in return y }
    fn g(x: int) -> int { if x then X.f(0) else return 0 }
  end
```

Ah you see, it is the callpackage design pattern!

This is not the same as Haskell's import system, because in there, there is an explicit coupling by importing another module. Not that we are saying, let's assume there was am odule that supplied this type, and we were using it.

Basically we get higher order functions in a way.

Languages often have an explicitly defined way of finding something. But there is something qualitatively different from relying on a module supplying a signature, versus just importing that module directly.

A type of a functor is itself a functor signature.

In this case it is:

```
functor(X: sig f: int -> int end) ->
  sig
    f: int -> int
    g: bool -> int
  end
```

The whole thing above can be just syntactic sugar in a first order language, but just exporting the structure like:

```
fn fact_aux...
fn fact...
fn S.f...
```

Substructures are just achieved via namespacing using `.` as a privileged delimiter.

Also the functor itself can be the same, but again with namespaced prefix. And the usage of `X.f` being replaced with some argument like `arg.f`. Which would also special cased. So you just use unique prefixes for each module... And the `arg` is also unique to your requirement. So you can always compile down to a lower level language that has no namespacing. Namespacing can always be achieved via name mangling.

For example, if we wanted to apply the functor, and thus achieve linking. We can do this:

```
fn f(x: int) -> int { let y = arg.f(x + 1) in return y}
fn g(y: bool) -> int { if x then arg.f(0) else return 0}
fn arg.f(x: int) -> int { return (x+1) }
```

You just concatenate the application below and do some name mangling.

To go higher order again, we can have functors that don't just depend on structure signatures, but functors that rely on other functors (via functor signatures).

Does it also make sense to package signatures as well? What about higher order signatures? That's just higher order types. Types that depend on other types.

In a way, the ability to rely on a signature, is a value that depends on a type. But higher order signatures would be types that depend on types.

Having a functor (a module that allows passing in another module, that itself relies on other modules), means you need to also apply the module you get before you can use the packaged functions.

I can see why Nix calls those overridable functions as functors. In a way they are overridable, you can pass a "callpackage package" into a callpackage package, and also apply and override it with other stuff. And then use it.


```
FS = functor (X: sig f: int -> int end) ->
  sig
    f : int -> int
    g : bool -> int
  end

functor (F: FS) ->
  struct
    A1 = struct f = fn(x: int) -> int { return x + 1 } end
    A2 = struct f = fn(x: int) -> int { return x + 2 } end
    h = fn (x:int) -> int {
      let y1 = F(A1).g(x) in 
        let y2 = F(A2).g(x) in
          return y1 + y2
    }
  end
```

The solution is to use type indices, basically we need to tell the Functor being applied how to find its dependencies.

"linking here" is still just concatenation, nothing fancier.

Apparently this idea is similar to "defunctionalisation". The whole idea is that we add type index arguments only when necessary in favour of reducing case distinction as much as possible. It appears that defunctionalisation would mean to reduce it even more to first order, from functions to things that directly reference the other things.

Ok that explains functors as higher order modules.

Now the second way to read this paper is particle-style Geometry of Interaction. The GOI has its origin in logic and can be seen as the construction of a model of linear logic.

The application of GoI to module systems is interesting. The product operator `X * Y` corresonds to linking 2 programs together. Linking is just concatenation along with any kind pointer rewriting to rewrite the function call addresses. Similarly we have this in Matrix AI but in a distributed sense.

Note that the X and Y are names of the interfaces themselves. So when you link them, you take into account both interfaces offering and requiring. The `send-: X- + Y- -> empty` where the `+` is a disjoint union of both interfaces, being a disjoint union means that you can differentiate left from right. So here this means we combine both offerings together. Just like something that exports 2 modules, we can access each module's functions.

The resulting program has `send-` and `send+`. Where `send-` is a function that is "provided" and may in turn call `send+` internally, which is the functions that is required. Ah yes...

```
functor (FUNCTIONINEED):
  function FUNCTIONIPROVIDE {
    FUNCTIONINEED(...)
  }
```

The send- corresponds to two functions `X- -> empty` and `Y- -> empty`.

The GOI only uses non-returning functions. IO  functions.

This GOI graphical representation is not just notational. In more complicated scenarios like type definition and type abstraction, it becomes more useful.

GOI defines the module system and its low level implementation.

In section 2 and 3 they will define the syntax of the core programming language. They will later equip that with a module system.

The constructions of the particle style GOI allows us to view the module system as a definitional extension of the core language.

We develop a linear form of Fing. In section 6 we deal with linearity in the type inference.

On page 8, they define a simple first order programming language. It has integers, units, products, disjoint unions. It has primitive operations like add, subtract and multiply. So there's no notion of defining functions. There's also return. But there could be effectful operations like print and put and get for global state, but this is not needed to be specified here.

Beyond the first order language, we also have a bit of infrastructure to organise them into programs. It roughly corresponds to C or LLVM IR.

Suppose the behaviour of each GOI node in an interaction graph is given by a core expression. To implement the message passing process, we need more than core expressions. Systems level programs extend core expressions just enough to implement particle style interaction. They can be seen as a simple syntactic formulation of the mathematical assumptions.

A core type constructor means that we include all the cases from the grammar for core types, only now with B in place of A. This is the int... etc types. We also have `rawk`.

So the functions are `fn f(x1:B1, ... xn:Bn) -> B { e }`. This defines a function named f with arguments x1 to xn of types B1 to Bn and return type of B and with a body of e. There's also the empty constructor that ends the program as well. That's why `P` is there. It's right-recursion!

Note that `f(v)` in the core expression allows you to perform function calls. And we can have mutually recursive functions.

The `rawk` allows storage of `k` bits. We can also do `size(B) = k`. Which means you can know the underlying data size of any type. So we have the ability to do type casting. As long as the type size is less or equal the type size of `k`, then it can be cast.

The usage of `COERCb` is using `b` as an index into `COERC` which is a family of functions, or a namespace of coerce functions. One can make this generic by using type polymorphism to later specialise the function.

Programs are incomplete in the sense that they contain calls to external functions. Just like Nix with its multistaged derivations, and also in Architect with points of imports. Every interface is always 2 sets, a set of things that it supplies, and a set of things it needs.

We define a module system for core expressions that elaborates into systems-level programs. Elaboration works by mapping modules to the structure of the GOI, which is then implemented by system porgrams.

The module system is intentionally kept failyr standard in order to express the GOI in terms that are familiar to anyone familiar with ML.

```
Paths p ::= X | p.l
Base Types C ::= core type constructors | p
Module Types E ::=  MC | type | type = C | sig li(xi):Ei end | functor(X:E) -> E | C -> E | B.E
```

Ok so basically we had a core expression language. Then we added in "system level lenguage" that adds type casting of sizes, and the existence of function definitions. (These are not lambdas). Finally thye add a module level language.

So the grammar of the language has grown to deal with extra features. At each level, the base types have been "inherited".

They have been called (this represents abstract syntax, but can be implemented using Haskell's algebraic data types):

* Core type constructors
* Core value constructors
* Core expression constructors (builds on top of core value constructors)
* System level types (builds on top of core type constructors)
* System level expressions (build on top of core expression constructors)
* System level programs builds on top of system level expressions and system level types
* Module Paths adds in X and `p.l`
* Module Base Types builds on top of module paths and core type constructors
* Module Types (this is the signatures, functors...)

In paths, the `X` ranges over infinite supply of module variables. These variables are distinct from the value variables that may appear in core values. Base types are core types with additional base case for paths. So that one can write types like `(int, X.t)`. Oh... the paths are like attribute paths in Nix. Remember it's recursive for the paths `p ::= p.l`. So the idea is that `p` is a path to some label. The `l` is a label. The `Xi` are identifiers for referring to the components from within the signature.

THe labels within a signature cannot be alpha renamed because they can be used to refer to parts inside the signature from the outside. Apparently we introduce additional identifiers that can be alpha renamed. Signatures are an example of translucent sums!? Alpha renaming is involved with name capture in type checking. A "manifest type" is the same as a type alias?

There is manifest typing (explicit types) vs implicit typing (type inference). Alternatively is dynamic typing. So type inference is pretty useful, but not necessary to a programming language.

And then there are Module Terms!

Ah right, those are functors, structs and based on the core expressions as well. Basically we have extended upon types and expressions with now module types and module expressions.

```
M: sig type t = int, f: Mt end
M :> SIGMA
SIGMA = sig type t, f: Mt end
```

"Sealing" means that modules that are sealed with a signature to impose type abstraction are regarded as impure. In other words, sealing is regarded as pro forma computational effect. This is consistent with the informal idea that generativity involves the generation of new types at run time. Moreover, this ensures that sealed moduels are incomparable and non-projectible. Which is suficient to ensure the proper semantics of type abstraction.

The principlan means for defining abstract types is sealing. Written `M :> t`. Sealing M with t prevents any client of M from depending on the identities of any type components specified opaquely.

This is opaque types vs the underlying types. So to seal something appears to mean that you make its internal types opaque. In flow this is also possible across modules, when instead of exporting say the `int`, you export an alias, but you prevent the consumer from relying on the underlying implementation type. In a way this performs type abstraction, abstracting a type into a more abstract type so it can range between many things. For now you may use int, but later you may use something else. Apparenlty making a module sealed, you end up making it incomparable. Because if 2 modules were to seal in the same way, they could still be using different underlying types. Specifically if it were possible to compare, then you would realise that the 2 modules are different, but this violates type abstraction which is ensure that clients cannot differentiate them. (So you're leaking information). Therefore they must be made incomparable. But to do so is apparently "impure", even if there is no actual computation taking place. So apparently you also judge by convention that module sealing is a runtime effect.

Ah I see... right if you seal modules, you make them incomparable.

So here are related things to sealed modules:

* Flow Opaque Type Aliases
* Haskell's Existential Types in Records
* Encapsulation in OOP
* https://news.ycombinator.com/item?id=16459709

Separate compilation is important to being able to compile incrementally. And in the case of Matrix AI, perhaps apply the edit graph.

So apaprently if the types of M1 and M2 match, but are not equal, you must use sealing to make their types equal.

So in the example, we havea Stream module type (signature), and we have Nats acting as a module (struct). And it seals it as Stream. This ensure that the `t` type which is implemented using `int`, is sealed or made opaque as just `t:type` and only usable by the Stream signature.

The usage of `M` is weird, I'm not sure what that means. I would think that `init: t` is enough.

Section 4 describes how the module system will work.

Section 5 describes the type checking mechanism for the module system. It does state that modules can be reordered, but value declarations cannot be. While type declarations must be ordered.

Section 5 defines contexts. They are referring to typing contexts. A context GAMMA is a finite list of variable declarations, of which there are 3 kinds:

* module declarations
* value declarations
* type declarations

They identify contexts up to the qeuivalence induced by...

```
GAMMA, X:S, Y:T DELTA = GAMMA, Y:T, X:S, DELTA
```

This rule tells us that module declarations can be reordered. Value declaration order does matter as this is an imperative language, but type declaration order doesn't matter.

Due to the imperative nature of the language, and the fact that value order matters, then values naturally segment a a context into zones.

Structural rules operate on the meta-level of a logic language. Basically they are inference rules that don't use a logical connective (a logical operator within the logic language). There is weakening, contraction, exchange and cut.

Exponentials in linear logic means that `!a` means that there is an infinite supply of `a`. So that means `!a = (!a, a)`. There is an negative exponential of `?a` that apparently "consumes a". Now the key to understanding this is to understand negation in linear logic. In classical logic, negation roughly means the absence of something, specifically the absence of truth. This does not carry over to resource interpretation of linear logic. Thus the A negated doesn't actually mean the absence of A, but the demand for A. We have in our possession of A and Anegated, we obtain 1  or the absence of resources. Thus `(A, -A) -o 1`. So `?A` means an infinite consumer of As. SO `?A = !(-A)`.

So when the module system uses the `!S`, they are saying you can have an infinite supply of that `S`, because S is a module address that can be infinitely copied using an index.

Interesting the GOI is literally then graphical rendering of the inference rules in linear logic (and the graphs look like message passing graphs). Of course there's a relationship between logic and graphs.

Linear Logic: https://ncatlab.org/nlab/show/linear+logic

Exponentiation turns addition into multiplication:

```
(2 + 3)^3
(2^3 * 3^3)
```

So in linear logic there's an equivalent to additive conjunction which is `&`. And the then multiplication is `*`.

```
!(A & B) == !A * !B
```

https://www.tweag.io/posts/2017-03-13-linear-types.html - thus the implementation of linear types to Haskell.

The `-o` operator is the lollipop operator. Or linear arrow.

Ok so the lollipop or linear arrow (->) is now used as a type constructor, similar to how the function itself is itself a type constructor.

These become not allowable (they are violations of linear type):

```
fst :: (a, b) -o a
fst (x, _) = x

dup :: a -o (a, a)
dup x = (x, x)
```

Can we use a different operator for this? Oh they end up with `->.`. Or `->*` and `->?` or `-.`.

Here is the GHC proposal: https://github.com/tweag/ghc-proposals/blob/linear-types/proposals/0000-linear-types.rst

Branch of GHC with the features: https://github.com/tweag/ghc/tree/linear-types See the compare on files. https://github.com/ghc/ghc/compare/master...tweag:linear-types

What is the difference between additive conjunction and multiplicative conjunction. It appears that the multiplicative conjunction it means the context of the conclusion is split up between the premises, while the additive case, the context is carried whole into the both premises.

See the resource interpretation: https://en.wikipedia.org/wiki/Linear_logic#Sequent_calculus_presentation this gives an analogy to the operators. Especially in terms of linear implication/arrow and the conjunctions and disjunctions.

A * B means simultaneous occurence of resources. To be used as the consumer directs. For example, if you buy a stick of gum AND a bottle of soft drink, you are requesting gum * drink. A constant 1 denotes the absence of any resources, so functions as the unit of `*`. Ah right so it's a monoid as well. Then 1 is the unit element of the `*` monoid.

Additive conjunction `A & B` represents alternative occurence of resources. The choice of which the consumer controls. So if you have a vending machine which sells, candy chips or drink all for 1 dollar you write it as:

```
$1 -o (candy & chips & drink)
```

That is not the same as:

```
$1 -o (candy * chips * drink)
```

Thus that's why the context is split with `*`, because they were availabel at the same time, thus the context that led to A or B being available is independent. But the `A & B`, means the context that leads to this alternation of A or B means that they must be shared in both the premise of providing `A` and `B`.

There seems to be a big relationship between session types and linear types.

Is there a relationship between geometry and logic here. A linear function vs an affine function. Affine function is a linear function with translation. Linear functions, linear algebra, linear logic.

Linear temporal logic is built on top of linear logic with temporality added in.

Session types are then built on top of linear logic as well.

Proof nets (geometry of interaction is one) is an alternative graphical rendering of sequent calculus. And it has some advantages, such as the ability to represent identities more easily.

Linear type systems is an appliation of linear logic to type systems of programming languages. Programmatic versions are always constructive. But this also means that because it's not classical, it cannot implement certain parts of linear logic. Or nobody has figured it out yet. The constructive linear type theory used in the module system is just an application of that.
