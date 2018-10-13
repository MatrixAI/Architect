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

One thing that we do want to cleanly demo is to have a demo environment that demonstrates FFI between Haskell and various languages. Specifically C and Golang and also allows seamless development with nix-shell and the usage of the GHCi interpreter.

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

---

Previously I was reading about Linear Logic applied to modules. We need to continue with that especially with first class modules and linking behaviour of Architect language.

The geometry of interaction is a graphical notation for linear logic.

The geometry of interaction is a proof net.

What's the relationship betwen linear combinatory algebra and linear logic?

The GOI used as Module system is the "particle-style". Apparently there are other styles as well like Wave Style. I'm still not sure about this.

Also system LK is apparently classical logic? LJ is a variant of Sequent Calculus that makes it intuitionistic or constructive.

I didn't realise it until now, but the sequent turnstile is like the "equals" sign in linear algebra. When you swap terms over the `=` equation, you have to apply the `-` operation to it. Similarly if you swap formula between the turnstile `|-` you have to apply the `¬` negation operation to them. One is a linear algebra, another is logical algebra. In a way logic systems like LJ/LK are like extensions of propositonal logic which itself is really similar to boolean algebra.

https://cs.stackexchange.com/a/45359 gives a good overview how a bunch of different logics are built on top of propositional logic. In effect propositional logic is one of the first programming languages!

Demorgan laws show that expressing conjunction and disjunction can be purely done in terms of each other via negation.

* The negation of a disjunction is the conjunction of the negations
* THe negation of a conjunction is the disjunction of the negations

This shows us a relationship between propositonal logic and set theory as well. As conjunction is related to set intersection, while disjunction is related to set union. So the complement/negation of union is the intersection of the complements. The complement/negation of an intersection is the union of the complements.

However I don't see a relationship between demorgan's laws applied to linear algebra.

Soundness means that if and only if every formula that can be proved is logically valid with respect to the semantics of the system.

Completeness is if every formula having the property can be derived using that system. So completeness is with respect to a property if every formula in that system that has that property can be derived using the system.

A variable that occurs inside a formula is called a free variable if it is not bounded by a quantifier. Basically quantifiers in logical systems introduce the concept of a "scope". A scope in programming languages is automatically provided you bound or parameterise a function. In Haskell function parameters are implicitly forall quantified. And if you use a variable inside a function that isn't part that function's parameters, then that variable is a free variable. The fact that haskell functions are automatically universally quantified means that that such a function shoul work on all instances of that type domain. Say a function takes an Int, thus all instances of that Int are valid here.

Note that universal and existential quantification is applied to type formulas in Haskell.

Sequent calculus rules has a "direction" in their inference lines. Sometimes the rules are going top to bottom (DOWN), that means going from theorems to axioms. And sometimes they are going bottom to top that means going from axioms to theorems (UP).

Note that the entire proof tree may be written from theorem to axiom, OR from axiom to theorem. Make sure to note which direction the proof trees are written in (just check if the top has small judgements or the bottom has small judgements, the smaller judgements are usually the axioms). And then the rules also are applied in that manner.

Let's continue with traced monoidal categories, geometry of interaction and eventually first class modules.

Premonoidal categories are a generalisation of Moggi's computational model (Monads). Hughes developed arrows which are the equivalent programming abstraction.

Some monads call for a kind of recursion operator on computations for which informally the recursion only takes place over the values. For example the Haskell prelude defines the internally implemented ST and IO monads for respectively potentially state-manipulating and input/output performing computations.

```
fixST :: (a -> ST a) -> ST a
fixIO :: (a -> IO a) -> IO a
```

The `fixIO` is a `mfix` for `IO`. That is `fix :: (a -> a) -> a`. Which takes a function to itself. Remember anonymous recursion where you have to have an anonymous function take a parameter that is the parameter for itself. You expect `fix` to eventually pass the function parameter to itself. In such a case, that means the type of such a function is in fact `a -> a`, because it takes itself, and returns itself. It's a bit weird yea..

Consider this function:

```
:t (\rec n -> if n <= 1 then 1 else n * rec (n-1))

:: (Num p, Ord p) => (p -> p) -> p -> p
```

You can see that its first parameter is `p -> p`. after that gets taken, it's also `p -> p`.

From `fix` point of view, then `(p -> p) -> p -> p` is the same as `a -> a`. Where `a` is `p -> p`.

Then when you do `fix (f)` that gives you back `:: (Num p, Ord p) => p -> p`. Because it returns the `a`.

So then `fixIO` takes a function which is an anonymous recursion. Such a function would take itself and return itself but wrapped in a monad. Essentially rather than a normal function, it's a monadic action that is anonymously recursive.

The result is of course the right hand side, which is just a monadic action.

What do you use `fixIO` on? Wait... the `a` part must be a function. To be anonymously recursive, the whole thing must be a function.

```
:t (\rec n -> if n <= 1 then return 1 else return $ n * rec (n-1))

:: (a -> a) -> a -> m a
```

Wouldn't it be closer to `(a -> a) -> m (a -> a)`?

```
:t (\rec -> return (\n -> if n <= 1 then 1 else n * rec(n - 1)))
```

You can use `fixIO` on sucha function. But what is this actually?

I don't really understand why this would be needed. Is it an IO context around the extraction of sucha function? The result is `IO (p -> p)`?

The fix is defined here:

```
fix :: (a -> a) -> a
fix f = let x = f x in x
```

The way this works is that laziness is used. Remember in Haskell expressions are not evaluated until they are demanded, essentially "deconstructed" via pattern matching. So here if `x` were to be strictly evaluated, it would never terminate, because `x` expands to `f x` and `x` gets expanded again... etc until `f (f (f (...)))`. Anyway because Haskell is lazy. This doesn't happen. What does happen is when `fix` is applied to an anonymously recursive function, the first parameter of the anonymous function is a thunk (`rec`) that if were to be expanded, would expand to `f x` where `f` is the same anonymous function and `x` is a thunk yet to be expanded. At this point the `rec` is applied to the `(n - 1)`, and if the inside that expression, the `rec` doesn't get expanded, and instead  1 is returned, then the call stack unwinds, and we get a terminating result.

So a way to prevent infinite evaluation is simply that the function has a base condition. That base condition is the terminating branch. An alternative to such a reducing expression is the dual generating expression. Such an expression replaces pattern matching/switch conditions/if conditions (which you'll realise are the same) with instead a constructor. A constructor also preserves laziness. So you can create infinite lists with `[1..] = fix (\xs -> 1 : fmap (+ 1) xs)`.

The reason to use fix is to avoid hardcoding the recursion into the system. Instead you can have units of recursive things, and where that recursion is binded to is instead flexible. So in the protocol spec, we can fix at various points to bind a parameter that refers to itself. And we make use of laziness to ensure that we don't get infinite expansion of the parameter until it's needed. And unlike the pattern matching condition, we are building up a structure, so we actually use constructors to preserve the laziness.

So while that's a cool application of `fix`, actually we have something else too. Nix uses `fix` to simulate object oriented inheritance patterns. And check this out:

```
fix $ \(~(a, b, c)) -> ([1, c-5], head a + 2, b * 4)
([1,7],3,12)
```

And check that out. We have a functional expression that when evaluated will refer to other values returned by the same function. Just like in Nix where a `rec` attribute set allows you to create attributes that refer to other attributes in the same set. Note however you have to be careful here, as it's definitely possible to produce infinite loops. Thankfully Nix actually detects most of these (it's not foolproof though). If we take a look at the type of:

```
:t \(~(a, b, c)) -> ([1, c-5], head a + 2, b * 4)
```

What we see is a function that takes a tuple and returns a tuple. This makes sense as the generation of such a tuple depends on the tuple itself. Thus a `rec` attribute set in Nix is also essentially syntax sugar for applying fix to a function that takes its own attribute set and returns the same attribute set. Fix applies it to itself, and you get back the result type which is just the attribute set.



```
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

let f :: Tree Int -> Int -> IO (Int,Tree Int)
    f (Leaf n) m = do 
      print n
      return (n, Leaf m)
    f (Branch t1 t2) m = do
      (m1,r1) <- f t1 m
      (m2,r2) <- f t2 m
      return (min m1 m2, Branch r1 r2)
  
t =  Branch (Leaf 1) (Leaf 2)

f t 0

-- this prints out 1 and 2
-- and returns the minimum tuple (1, Branch (Leaf 0) (Leaf 0))
```

You can see that the function basically prints things, but returns the `n` inside the leaf and replaces it with m. Basically this function replaces the numbers in the node. That's also waht `f (Branch t1 t2) m` does. It applies that `m` down to both branches on the tree. But it then returns the minimum of both trees.

Basically it's a weird function that replaces the leaf values, and also prints them while returning the minimum value.


On Haskell the MonadFix class, the instance for IO defines:

```
mfix = fixIO
```

So the implementation of monad fix is simply the `fixIO`, and `fixIO` is just a more specific version of the generic `mfix`.

The implementation is:

```
fixIO :: (a -> IO a) -> IO a
fixIO k = do
  m <- newEmptyMVar
  ans <- unsafeDupableInterleaveIO
    (readMVar m `catch` \BlockedIndefiniteOnMvar -> throwIO FixIOException)
  result <- k ans
  putMVar m result
  return result
```

The function first creates a mutable variable. It's a mutable location that is either empty or contains a value of type `t`. The `purMVar` which fills an `MVar` if it is emtpy, and blocks otherwise. The `takeMVar` which empties the `Mvar` if it is full and blocks otherwise. Here we can see creation of it `newEmptyMVar`. But later we use `readMVar` not `takeMVar`. And then it will apply the `k` to the value, while extracting the result, thus unwrapping the IO monad, and putting the result into the `mVar`, and returning the result. Which itself is another `IO a`. The `readMVar` will atomically read the contents of an `MVar`, if the `MVar` is empty, the `readMVar` will wait until it is full.

IT appears that `unsafeDupableInterleaveIO :: IO a -> IO a` will allow the IO computation to be deferred lazily. That is when passed a value of `IO a`, the IO will only be performed when the `a` is pattern matched!? The `Dupable` part is what allows the computation to be performed multiple times by different threads possibly at the same time. Apparently that exception can happen due to a deadlock. Basically if every thread that contains a reference to the MVar is trying to read or write to that location has died or is waiting on another primitive that is blocked forever.

See if if you use `readMVar` or `takeMVar`, that would block, since there's nothing put int othe MVar at this point, the Mvar is just still empty. However by using `unsafeInterleaveIO` we essentially make the IO operation LAZY on the basis of the value itself. It's almost like defer operation in Golang, because we don't even bother evaluating them until we need to.

Wait a minute, is this like a promise? Because it then calls the `k` with the answer from the future, and as long as it's not pattern matched, it won't execute the `takeMVar` (unless it does, in which case IT must terminate). And we also get a result monad. It's like a promise of a promise!?

Both promises and lazy thunks are proxies. Proxies is the key word here.

So this basically is intended to work with lazy cyclic structures. If the `k` attempts to evaluate the thunk given, then it will create an infinite loop. Say for example that you nee to use a thunk to an IO action so it can do some IO and create a data structure with that thunk placed somewhere inside, but you know that it doesn't actually need to force that thunk.

So you can have a cyclic structure that makes use of an IORef and thus is also within the IO context.

```
data Node = Node Int (IORef Node)
mknode = mfix $ \p -> do
  p' <- newIORef (Node 0 p)
  putStrLn "node created"
  return p'
```

The compelling example is Reflex. So you can have widgets defined in a certain order but their logic in the opposite order.

So we basically rely on lazy evaluation representing proxies for some value to avoid needing to an explicit cyclic pointer which is impure.

Adding MonadFix is basically allowing you to create cyclic monadic structures that don't just reduce, but refer to themselves.

So mfix has a syntax extension called `RecursiveDo`. This allows you to use `mfix` more easily.

So instead of `do`, you use `mdo`. And you get the ability to reference values that is extracted later when using the monadic syntax. Ok wow. In a way `mfix` is like value recursion. You can build cyclic data in monadic code. Rather than just cyclic data in `fix`.

I wonder if this has importance to our protocol spec, which depends on whether the protocol spec is in a monad as well?

```
import Control.Monad.Fix
import Data.IORef

-- here we have cyclic algebraic data type using IORef (as pointer)
data Node = Node Int (IORef Node)

-- here we have the smart constructor for the cyclic algebraic datatype
mknode = mfix (\p -> do
    p' <- newIORef (Node 0 p)
    putStrLn "node created"
    return p')
```

Note that recursive data structures fundamentally "productively infinite", but cyclic data structures are bounded infinite. They are not growing, but simply referring back to itself. If you can capture the cyclicness in an expression then a recursive productive is fine, as you can continue to evaluate the expression. So the idea of `f = 1:f` which uses the function to allow you perform it cyclically. Laziness is the alternative way, and laziness combined with fix is how it works.

It appears that a traced monoidal category is introduced to interpret recursion in programming languages. The relationship between traces and fixpoints have attracted much attention in recent years. See Hasegawa's thesis.

A trace on a symmetric monoidal category `(M,*,I,l,p,a,s)`.

Monoidal categories provide a formal basis for reasoning about many of the graphical boxes and wires notations used in computer science. A traced monoidal category provides a formbal basis for circuit like notations involving feedback or cycles.

There is a family of functions with the type:

```
trace :: M(A * U, B * U) -> M(A, B)
```

In a monoidal category, the associative binary operation is sometimes referred to as the "tensor product" or at least they use the tensor operation.

Set is a cartesian monoidal category, the singleton set serves as the unit.

There are "monoidal categories", and a "cartesian monoidal category" is where the monoidal product is the categorical product. A categorical product is basically in category theory where you have an object and a pair of morphisms leading to X1 and X2. Then there is a unique morphsim to `f: Y -> X1 * X2`.

It makes sense that monoidal categories capture the notion of boxes and wires, because a monoidal category is a category equipped with a bifunctor: `* : C,C -> C`. This basically allows you to map some object in the category and another object in the category to another object in the category.

Ok...

A monoidal category has the bifunctor `*`. It seems to imply that his works as a binary operator. There's a identity object `I`. There are 3 natural isomorphisms. A natural isomorphism is related to natural transformation. A natural transformation is morphism betwen functors. Then a natural isomorphism is a isomorphic morphism between functors. The 3 are alpha, lambda and rho. The alpha expresses associativity. So a natural isomorphism is just a natural transformation that is also isomorphic. The lambda expresses left unitor and the rho expresses right unitor.

```
alpha: (A * B) * C <-> A * (B * C)
lambda: I * A <-> A
rho: A * I <-> A
```

Ok now it makes sense what the trace is.

```
trace: M(A * U, B * U) -> M(A,B)
```

What is `U` and what is `M`. I thought that `M` would be a category. What does it mean to have `M(...)`?

---

Hom-set is a homomorphic set (that is the set of structure preserving morphisms between objects A and B in C) denoted by `C(A,B)` or `Hom(A,B)` or `Hom-Set(A,B)`.

There could be multiple morphisms between the same objects in a category.

A and B and U are all objects of the category `M`.

A trace is like a polymorphic function with 3 type parameters. Because whenever category theory talks about a hom-set. Where hom-set represents multiple different morphisms. Then the way you represent that in Haskell is via a "polymorphic" function. Since that function represents a family of functions that can be specialised later. Interesting that generic functions represents a family of morphisms.

So then `trace` takes a family of morphisms between `A*U` to `B*U` and gives back a family of morphisms between `A` to `B`. Oh that's interesting. Then that would mean trace works on functions in Haskell. A polymorphic function that works on functions, then it's a higher order combinator.

The `U` that is being removed can be through of as the internal state of the state machine.

So if we get back `A -> B` as the result of the trace. And it's a "recursive" function. It can be decomposed as 4 non-recursive functions:

* `A -> B`
* `A -> U`
* `U -> U`
* `U -> B`

Only 1 access any particular input. An A either produces a U meaning there's more recursive work to do, or didn't need to recurse so it gives back B. So B is the base case, but U represents the recursion instead.

```
A--base-->B
|         ^
|         |
v         |
U---|-----+
^   |
|   | recurse/inductive
+---+
```

Basically any recursion can be considered as a state machine. Ah I see, that's cool.

Using the above structure, you can implement recursive algorithms in the internal language of a traced monoidal category. So you can construct recursive functions by composing together with `trace`. That seems so much like using `fix`.

However you cannot take a recursive program and computationally deconstruct it to the 4 functions there. The 4 transitions between the 3 states `A`, `B` and `U`. That will require solving the halting problem.

Both flow diagrams from basic imperative programming and fixed points in functional programming fits into the view of recursion in terms of traces.

Oh... LR(0) parsers are still pushdown stack automatons. In fact many context free parsing algorithms are just pushdown automatons. Ohh... while regular languages can be parsed with a finite automaton. Context free languages need a pushdown automaton. Then within that class, we have many algorithms like LR and Earley parsers. Non-deterministic pushdown automatons are the most powerful in that it can parse every context free grammar. Wait... a language can be expressed via a grammar. A grammar can be "parsed" or turned into a machine. Context free grammars can be turned into a machine that parses a context free language. In a way the grammar expresses the "transition" rules almost.

Different context free grammars can generate the same context free language.

I wonder then, why are protocol specs/session types not regular? In what way are they a "context free grammar"?

C is context senstive language, its parser cannot be a pushdown automaton. Instead it has to be at least linear bounded pushdown automaton. C++ is turing complete requiring a turing complete parser. That means its language is considered recursively enumerable and has an "unrestricted grammar". Interesting! This is due to the template metaprogramming allowing computations at compile time. So the parser/interpreter needs to be turing complete as well. The compilation process may not terminate! I think Haskell is similar.
