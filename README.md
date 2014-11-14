`t-regex`: matchers and grammars using tree regular expressions
===============================================================

`t-regex` defines a series of combinators to express tree regular
expressions over any Haskell data type. In addition, with the use
of some combinators (and a bit of Template Haskell), it defines
nice syntax for using this tree regular expressions for matching
and computing attributes over a term.

## Defining your data type

In order to use `t-regex`, you need to define you data type in a
way amenable to inspection. In particular, it means that instead
of a closed data type, you need to define a *pattern functor* in
which recursion is described using the final argument, and which
should instantiate the `Generic1` type class (this can be done
automatically if you are using GHC).

For example, the following block of code defines a `Tree'` data
type in the required fashion:
```haskell
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics

data Tree' f = Leaf'
             | Branch' { elt :: Int, left :: f, right :: f }
             deriving (Generic1, Show)
```
Notice the `f` argument in the place where `Tree'` would usually be
found. In addition, we have declared the constructors using `'` at
the end, but we will get rid of them soon.

Now, if you want to create terms, you need to *close* the type, which
essentially makes the `f` argument refer back to `Tree'`. You do so
by using `Fix`:
```haskell
type Tree = Fix Tree'
```
However, this induces the need to add explicit `Fix` constructors at
each level. To alleviate this problem, let's define *pattern synonyms*,
available from GHC 7.8 on:
```haskell
pattern Leaf = Fix Leaf'
pattern Branch n l r = Fix (Branch' n l r)
```
From an outsider point of view, now your data type is a normal one,
with `Leaf` and `Branch` as its constructors:
```haskell
aTree = Branch 2 (Branch 2 Leaf Leaf) Leaf
```

## Tree regular expressions

Tree regular expressions are parametrized by a pattern functor: in this
way they are flexible enough to be used with different data types,
while keeping our loved Haskell type safety.

The available combinators to build regular expressions follow the syntax
of [Tree Automata Techniques and Applications](http://tata.gforge.inria.fr/),
Cahpter 2.

#### Emptiness

The expressions `empty_` and `none` do not match any value. They can be
used to signal an error branch on an expressions.

#### Whole language

You can match any possible term using `any_`. It is commonly use in
combination with `capture` to extract information from a term.

#### Choice

A regular expression of the form `r1 <||> r2` tries to match `r1`, and if
it not possible, it tries to do so with `r2`. Note than when capturing,
the first regular expression is given priority.

### Injection

Of course, at some point you would like to check whether some term has
a specific shape. In our case, this means that it has been constructed in
some specific way. In order to do so, you need to *inject* the
constructor as a tree regular expression. When doing so, you can use
the same syntax as usual, but where at the recursive positions you write
regular expressions again.

Let's make it clearer with an example. In our initial definition we had
a constructor `Branch'` with type:
```haskell
Branch' :: Int -> f -> f -> Tree' f
```
Once you inject it using `inj`, the resulting constructor becomes:
```haskell
inj . Branch' :: Int -> Regex' c f -> Regex c' f -> Tree' (Regex' c f)
```
Notice that fields with no `f` do not change their type. Now, here is how
you would represent a tree whose top node is a 2:
```haskell
topTwo = inj (Branch' 2 any_ any_)
```

In some cases, though, `inj` is too strict, as every non-recursive field
has to be given a value. But when pattern matching, in some cases you
are only interested on the shape of the tree itself. For that reason, the
library introduces a variant of injection called *shallow injection* and
available using `shallow`. With shallow injection, only constructors from
the pattern functor are checked, nothing else.

For example, here is how you would represent the shape of a tree which
has at least one branch point:
```haskell
topTwo = shallow (Branch' __ any_ any_)
```
If you think about it, we cannot express it using `inj`, as any number has
to match. As a convenience, `t-regex` includes a special constant `__` to
be used alongside `shallow` to represent "I don't care about the value".

#### Iteration and concatenation

Iteration in tree regular expressions is not as easy as in word languages.
The reason is that iteration may occur several times, and in different
positions. For that reason, we need to introduce the notion of *hole*: a
hole is a placeholder where iteration takes place.

In `t-regex` hole names are represented as lambda-bound variables. Then,
you can use any of the functions `square`, `var` or `#` to indicate a
position where the placeholder should be put. Iteration is then indicated
by a call to `iter` or its post-fix version `^*`.

The following two are ways to indicate a `Tree` where all internal nodes
include the number `2`:
```haskell
{-# LANGUAGE PostfixOperators #-}

regex1 = Regex $
           iter $ \k ->
                  inj (Branch' 2 (square k) (square k))
             <||> inj Leaf'

regex2 = Regex $ ( (\k -> inj (Branch' 2 (k#) (k#)) <||> inj Leaf')^* )
```
Notice that the use of `PostfixOperators` enables a much terse language.

Iteration is an instance of a more general construction called *concatenation*,
where a hole in an expression is filled by another given expression. The
general shape of those are:
```haskell
(\k -> ... (k#) ...) <.> r  -- k is replaced by r in the first expression
```

## Matching and capturing

You can check whether a term `t` is matched by a regular expression `r`
by simply using:
```haskell
r `matches` t
```
However, the full power of tree regular expression come at the moment you
start *capturing* subterms of your tree. A capture group is introduced
in a expression by either `capture` or `<<-`, and all of them appearing
in a expression should be from the same type. For example, we can refine
the previous `regex2` to capture leaves:
```haskell
regex2 = Regex $ ( (\k -> inj (Branch' 2 (k#) (k#)) <||> "leaves" <<- inj Leaf')^* )
```

To check whether a term matches a expression and capture the subterms,
you need to call `match`. The result is of type `Maybe (Map c (m (Fix f)))`.
Let's see what it means:

  * The outermost `Maybe` indicates whether the match is successful
    or not. A value of `Nothing` indicates that the given term does
	not match the regular expression,
  * Capture groups are returned as a `Map`, whose keys are those
    given at `capture` or `<<-`. For that reason, you need capture
	identifiers to be instances of `Ord`,
  * Finally, you have a choice about how to save the found subterms,
    given by the `Alternative m`. Most of the time, you will make
	`m = []`, which means that all matches of the group will be
	returned as a list. Other option is using `m = Maybe`, where
	only the first match is returned.

#### Tree regular expression patterns

Capturing is quite simple, but comes with a problem: it becomes easy to
mistake the name of a capture group, so your code becomes quite brittle.
For that reason, `t-regex` includes matchers which you can use at the
same positions where pattern matching is usually done, and which take
care of linking capture groups to variable names, making it impossible
to mistake them.

To use them, you need to import `Data.Regex.TH`. Then, a quasi-quoter
named `rx` is available to you. Here is an example:
```haskell
{-# LANGUAGE QuasiQuotes #-}

example :: Tree -> [Tree]
example [rx| (\k -> inj (Branch' 2 (k#) (k#)) <||> leaves <<- inj Leaf')^* |]
           = leaves
example _  = []
```
The name of the capture group, `leaves`, is now available in the body
of the `example` function. There is no need to look inside maps, this
is all taken care for you.

Note that when using the `rx` quasi-quoter, you have no choice about
the `Alternative m` to use when matching. Instead, you always get as
value of each capture group a list of terms.

For those who don't like using quasi-quotation, `t-regex` provides a
less magical version called `with`. In this case, you need to introduce
the variables in a explicit manner, and then pattern match on a tuple
wrapped inside a `Maybe`. The previous example would be written as:
```haskell
{-# LANGUAGE ViewPatterns #-}

example :: Tree -> [Tree]
example with (\leaves -> Regex $ iter $ \k -> inj (Branch' 2 (k#) (k#))
                                              <||> leaves <<- inj Leaf' )
             -> Just leaves
           = leaves
example _  = []
```
Notice that the pattern is always similar `with (\capture variables ->
regular expression) -> Just (capture variables)`.

## Attribute grammars
