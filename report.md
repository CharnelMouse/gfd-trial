# Null-free schema generation experiment
Mark Webster

``` r
library(targets)
read_dot <- function(dot) {
  cat(
    "```{dot}\n//| fig-width: 100%\n",
    dot,
    "\n```\n",
    sep = ""
  )
}
library(autodb, warn.conflicts = FALSE)
library(arules)
```

    Warning: package 'arules' was built under R version 4.5.3

    Loading required package: Matrix


    Attaching package: 'arules'

    The following objects are masked from 'package:base':

        abbreviate, write

## Replication

This document can be generated from scratch by downloading the project,
and running `targets::tar_make()` in R. A list of used package versions
is included at the end, under Session Info.

## Aim

To experiment with implementing some ideas on automatic null-free schema
generation.

## Context

I’ve been working on an R package called autodb, which implements
existing algorithms to discover functional dependencies in a given data
set, and use them to normalise it (to EKNF using Bernstein synthesis).

As a simple example: take the following table:

``` r
tar_read(ex1)
```

      a b c
    1 1 1 1
    2 2 2 2
    3 3 3 1
    4 4 4 2
    5 5 1 1
    6 6 2 2
    7 7 3 1
    8 8 4 2

Clearly, rows in this table are uniquely defined by `a`, and `b` also
determines `c`. This is discovered by the FDHits search algorithm:

``` r
tar_read(fds1)
```

    3 functional dependencies
    3 attributes: a, b, c
    a -> b
    a -> c
    b -> c

We can then turn this into a schema, and insert the data into the schema
to make a database.

``` r
tar_read(db1)
```

    database with 2 relations
    3 attributes: a, b, c
    relation a: a, b; 8 records
      key 1: a
    relation b: b, c; 4 records
      key 1: b
    references:
    a.{b} -> b.{b}

We can also plot this in a simple diagram:

``` r
read_dot(tar_read(gv1))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-1.png"
style="height:5in" />

</div>

</figure>

</div>

However, this approach does not properly handle missing values (`NA` in
R, `NULL` in most RDBMSs). Consider the following table:

``` r
tar_read(data.ex2)
```

      id start end
    1  1     3   5
    2  2     3   7
    3  3     3  NA
    4  4     3   5
    5  5     6   7
    6  6     6  NA
    7  7     6  NA

This is an abstract representation of some sort of time interval data.
Not all intervals have an end point, because they haven’t finished yet.
I’ve added enough duplicate values so that only `id` determines the row,
or anything else.

Since functional dependency discovery treats missing values as if
they’re non-missing and equal to each other, the automatic schema
returns the original table, with `id` marked as the sole key:

``` r
read_dot(tar_read(gv2))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-20.png"
style="height:5in" />

</div>

</figure>

</div>

This can also lead to odd things like missing values appearing in keys,
or foreign keys.

Ideally, we’d like the option to also generate a null-free schema, so we
don’t have to deal with any missing values at all. This gives a more
complete description of the data’s structure, and means we don’t have to
deal with the unintuitive way in which most RDBMSs handle missing
values.

In this case, we’d like the result to be like this:

``` r
read_dot(tar_read(gv_ideal2))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-19.png"
style="height:5in" />

</div>

</figure>

</div>

The trick lies in finding a generalisation of functional dependencies
that can cover such cases, and making a simple implementation of
searching for them, plus using the result to make a schema.

## Generalising FDs

There are various generalisations of FDs, including a few that try to
handle missing values more explicitly. The three most worth mentioning
here are as follows.

- Literal FDs (LFDs) are like functional dependencies, but they pretend
  that missing values are non-missing and equal. These are effectively
  what functional dependency discovery algorithms are looking for, not
  normal FDs. Pretending that missing values are non-missing discards
  valuable information, but the the advantage is that they can be used
  to create a normalised schema in the same way as usual, and at least
  give something that’s self-consistent.
- Existential FDs (EFDs) are of the form `E: X -> Y`. `X` and `Y` are
  the determinant and dependant, as usual, but with the implicit
  assumption the dependency only applies to the rows where `E \/ X \/ Y`
  are non-missing.
- Conditional FDs (CFDs) address a different problem: some structure
  only occurs when some of the variables take on specific values. These
  are of the form `[X, Z = z] -> [Y, W = w]`, and means that for any two
  tuples `s` and `t` where `s[X] = t[X]` and `s[Z] = t[Z] = z`, we also
  have `s[Y] = t[Y]` and `s[W] = t[W] = w`, and `z` and `w` are tuples
  for the attribute sets `Z` and `W`. This is not the original notation,
  which combined `XZ` and `YW`, and used a “pattern tableau” to indicate
  values, with a wildcard `_` value meaning no value specified, i.e. the
  attribute is in `X` or `Y` rather than `Z` or `W`. There are two
  important implication for CFDs. One is that a given CFD can imply a
  contradiction, e.g. `[Z = 0] -> [Z = 1]` is the same as
  `[Z = 0] -> false`, i.e. no tuple `t` can have `t[Z] = 0`. The other
  implication is that some CFDs can be trivially simplified
  (i.e. simplified regardless of the relation in question): if `Y \ W`
  is empty, i.e. the dependant is entirely fixed-value, then `X` can be
  removed, i.e. we only need the fixed-value part of the determinant.

We use a generalisation of EFDs, making use of CFDs. The key
introduction is that, in addition to the attribute `A`, we also consider
its *presence indicator* `P(A)`. This is always non-missing
(“non-nullable”), and takes values in the domain `{0, 1}`: it’s equal to
1 when `A` is non-missing, and 0 when `A` is missing.

In addition, we also forbid an attribute’s value from being mentioned if
it hasn’t been confirmed that `P(A) = 1`.

For example, for the interval data given above, under these rules we
have `id -> start`, but we can’t say that `id -> end`, because it’s
sometimes missing. However, we can say that `id -> P(end)`.

We also want the equivalent of the EFD `id -> end`. To do so, we
introduce the second key addition: we can also use constant-value terms
for the presence indicators. We can now re-write the EFD as
`[end = 1, id] -> end`: when `end` is present, `id` determines its
value. This is more general than EFDs, because we can also discuss cases
where attributes are known to be missing. It also means that we can note
attributes as being always-present or always-missing:
`{} -> [P(A) = 1]`, or `{} -> [P(A) = 0]`.

It is possible to write inference rules for these generalised FDs, which
I’ll call DEFDs for the sake of argument. However, for intuition, it’s
easier to decompose DEFDs into three simpler cases. Since FDs can be
split into separate FDs with the same determinant and the dependant
split up, we do the same here to split dependants into three types.

- `[P(X) = x] -> [P(Y) = y]` is a presence association rule, where the
  dependant consists of constant-value presence statements. By the same
  rule as for CFDs, this means that any non-constant determinants can be
  removed, so they are determinable by other constant-value presence
  statements alone. Since these only involve presence, we can use the
  shorthand `[X, ¬Z] => [Y, ¬W]` for
  `[P(X) = 1, P(Z) = 0] -> [P(Y) = 1, P(W) = 0]`.
- `[X, P(Z), P(W) = w] -> P(Y)` is a presence dependency: given certain
  agreements on attribute presence, the value of `X` determines which
  attributes in `Y` are present. Note that this can be decomposed
  further: `P(Z)` can be replaced by constant-value statements, where we
  have a presence dependency for each possible value for `P(Z)`.
- `[X, P(Z), P(W) = w] -> Y` is a generalised EFD (GEFD). Again, this
  can be decomposed with respect to `P(Z)`.

These three types of dependency are different enough to be worth
discussing separately WRT discovery.

## Discovery

### Presence association rules

Presence association rules are just association rules on the presence
indicators. This suggests discovering them using association rule
mining. However, we need to be a bit careful: association rules are
usually considered on the basis of only referring to elements when
they’re known to be present, and here we want to refer to the case where
they’re know to be absent as well.

In R, we can use the arules package for mining. We also need to make
ourselves familiar with some terminology.

Association rule mining is usually done on the basis that we’re not just
interested in rules that are always true: we’re also interested in rules
that are often true. This is done using several different metrics.

- The *support* `supp(X) in [0, 1]` for an item set `X` is the
  proportion of records with `X` present.
- The *confidence* `conf(X => Y) := supp(XY)/supp(X)` for the rule
  `X => Y` is the proportion of records containing `X` that also contain
  `Y`, i.e. `X` implies `Y` `supp(Y)` of the time. In other words,
  `conf(X => Y) ~= P(Y|X)`.
- The *lift* `lift(X => Y) := supp(XY)/supp(X)supp(Y) ~= P(XY)/P(X)P(Y)`
  measures how much the presence of `X` affects the frequency of `Y`.
  This, of course, is related to the covariance `P(XY) - P(X)P(Y)`, but
  it’s a relative measure rather than an absolute one. It’s also very
  roughly related to the resolution component of a scoring rule.

Association rule searches usually filter discovered rules to satisfy a
minimum support (for `X`), a minimum confidence, and a minimum lift
(only minimum because the interest is in positive cases).

Running the apriori algorithm on the presence patterns for the interval
example gives the following rules:

``` r
tar_read(rules2)
```

              LHS     RHS   support confidence  coverage lift count
    1          {} {start} 1.0000000          1 1.0000000    1     7
    2          {}    {id} 1.0000000          1 1.0000000    1     7
    3       {end} {start} 0.5714286          1 0.5714286    1     4
    4       {end}    {id} 0.5714286          1 0.5714286    1     4
    5     {start}    {id} 1.0000000          1 1.0000000    1     7
    6        {id} {start} 1.0000000          1 1.0000000    1     7
    7 {start,end}    {id} 0.5714286          1 0.5714286    1     4
    8    {id,end} {start} 0.5714286          1 0.5714286    1     4

We can convert these into functional dependency form, and remove any
that are made redundant by the others using the following:

``` r
rulefds2[apply(
  outer(
    rulefds2,
    rulefds2,
    `>`
  ),
  1,
  Negate(any)
)]
```

``` r
tar_read(minrulefds2)
```

    2 functional dependencies
    3 attributes: id, start, end
     -> start
     -> id

Of course, this just says what we already knew: `id` and `start` are
never missing.

As a test that `apriori` searches for everything we want, we use the
following example of a presence indicator matrix.

``` r
tar_read(presence.ex3)
```

           a     b     c     d
    4   TRUE  TRUE FALSE FALSE
    9  FALSE FALSE FALSE  TRUE
    11 FALSE  TRUE FALSE  TRUE
    12  TRUE  TRUE FALSE  TRUE
    13 FALSE FALSE  TRUE  TRUE
    15 FALSE  TRUE  TRUE  TRUE

This respects the following rules:

- `a => b` and its equivalent form, `¬b => ¬a`
- `a => ¬c` and its equivalent form, `c => ¬a`
- `¬a => d` and its equivalent form, `¬d => a`

Here are the non-dominated rules found:

``` r
tar_read(minrulefds3)
```

        LHS RHS   support confidence  coverage lift count
    1   {a} {b} 0.3333333          1 0.3333333  1.5     2
    2   {c} {d} 0.3333333          1 0.3333333  1.2     2
    3 {a,d} {b} 0.1666667          1 0.1666667  1.5     1
    4 {b,c} {d} 0.1666667          1 0.1666667  1.2     1

Not everything is found, because only the positive cases are used. We
can fix this by doubling up on every variable, producing both presence
and absence columns:

``` r
tar_read(double_presence.ex3)
```

           a     b     c     d    ¬a    ¬b    ¬c    ¬d
    4   TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
    9  FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE
    11 FALSE  TRUE FALSE  TRUE  TRUE FALSE  TRUE FALSE
    12  TRUE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE
    13 FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE
    15 FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE

The results are more like what we’d expect:

``` r
tar_read(minimal_presence_rule_fds.ex3)
```

    10 functional dependencies
    8 attributes: a, b, c, d, ¬a, ¬b, ¬c, ¬d
    ¬d -> a
    ¬d -> b
    ¬d -> ¬c
     a -> b
     a -> ¬c
    ¬b -> ¬a
    ¬b -> d
     c -> ¬a
     c -> d
    ¬a -> d

We can also remove transitive “dependencies” to get something more
minimal:

``` r
tar_read(minntdrulefds3)
```

    6 functional dependencies
    8 attributes: a, b, c, d, ¬a, ¬b, ¬c, ¬d
    ¬d -> a
     a -> b
     a -> ¬c
    ¬b -> ¬a
     c -> ¬a
    ¬a -> d

More usefully for our current purposes, we could also group dependants
by determinants, to find which presence values determine which others:

``` r
tar_read(grpfds3)
```

      det      dep
    1  ¬d a, b, ¬c
    2   a    b, ¬c
    3  ¬b    ¬a, d
    4   c    ¬a, d
    5  ¬a        d

### Embedding pruning

For future purposes, we also need to turn our presence rules into a
graph of allowed embeddings. Embeddings are a set of constant-value
presence conditions, which determine which subset of the data tuples we
are considering. As an example, here are the possible embeddings for the
interval example:

``` r
read_dot(tar_read(dot_all_embeddings.ex2))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-18.png"
style="height:5in" />

</div>

</figure>

</div>

Note that we’re also keeping track of how we can get from one embedding
to another by removing the presence for one attribute.

We can then take our discovered presence rules for the interval data –
`id` and `start` are always non-missing – and use these to reduce to
just the relevant embeddings.

``` r
read_dot(tar_read(dot_searched_embeddings.ex2))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-17.png"
style="height:5in" />

</div>

</figure>

</div>

As we’d expect, the main embedding is now the one with `id` and `start`
present, and the children split this on whether `end` is present.

Does this work for our presence example, too?

``` r
read_dot(tar_read(dot_searched_embeddings.ex3))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-16.png"
style="height:5in" />

</div>

</figure>

</div>

It does indeed! There are a lot of transitive edges here, but we need to
hang on to those to show the disjoint partitions.

Note that a node with `n` unspecified attributes has up to `2n`
children, but we don’t have an equivalent rule going the other way: a
node with `m` specified attributes can have more than `m` parents.

### Presence dependencies

Presence dependencies are of the form `[X, P(W) = w] -> P(Y)` in
simplified form. These can be found naïvely by running a dependency
search on the embedding `[P(W) = w]`, with only `X` allowed as
determinants and `P(Y)` allowed as dependants, and where `X` is a subset
of `W` such that `P(X) = 1`.

We only report presence dependencies that aren’t implied by weaker
embeddings, i.e. `[X, P(W') = w'] -> P(Y)` where `W'` is a strict subset
of `W`, and `w'` is the corresponding set-tuple of `w`.

Here are the results for the interval data:

``` r
tar_read(presence_fds.ex2)
```

    $`[id, start]`
    1 functional dependency
    3 attributes: id, start, end
    id -> end

    $`[id, start, ¬end]`
    0 functional dependencies
    2 attributes: id, start

    $`[id, start, end]`
    0 functional dependencies
    3 attributes: id, start, end

We see exactly the one presence dependency we expect.

### GEFDs

GEFDs are of the form `[X, P(W) = w] -> Y` in simplified form, where
`XY` is a subset of `W` such that `P(XY) = 1`. These take a very similar
approach to presence dependencies: we just take `W' \ X` as candidate
dependants, where `W'` is the largest subset of `W` such that
`P(W') = 1`.

Again, we remove anything implied in parent embeddings.

Here are the results for the interval data:

``` r
tar_read(gefds.ex2)
```

    $`[id, start]`
    1 functional dependency
    2 attributes: id, start
    id -> start

    $`[id, start, ¬end]`
    0 functional dependencies
    2 attributes: id, start

    $`[id, start, end]`
    1 functional dependency
    3 attributes: id, start, end
    id -> end

These are the ones we expect.

## Schema creation

We can now think about creating the schema. The three different types of
dependency have different roles:

- Presence rules, of course, determine which embeddings have any rows in
  them. They also determine how an embedding get split up into further
  disjoint partitions.
- Presence dependencies determine keys for said disjoint partitions.
  This makes them what Darwen called distributed keys: key values can
  occur up to once across all of the involved relations.
- GEFDs, of course, become FDs within their respective embeddings, and
  are used to create relations.

We already used the presence rules to prune our search space for the
other two dependency types. We now make use of the others.

### GEFDs

GEFDs are simple: we just use Bernstein synthesis on them to create a
schema for each embedding. We don’t ensure they’re lossless. We do
include the embedding in their name, to keep them distinguishable when
we join the schemas together later.

``` r
tar_read(prekey_schema.ex2)
```

    $`[id, start]`
    database schema with 1 relation schema
    2 attributes: id, start
    schema [id, start]::id: id, start
      key 1: id
    no references

    $`[id, start, ¬end]`
    database schema with 0 relation schemas
    2 attributes: id, start
    no references

    $`[id, start, end]`
    database schema with 1 relation schema
    3 attributes: id, start, end
    schema [id, start, end]::id: id, end
      key 1: id
    no references

### Presence dependencies

For each connected pair of embeddings, we know which attribute is
associated with that edge. We ensure each of those embeddings has a
relation with that attribute as the key. We then combine the embedding
schemas, and connect those key relations together. We keep the embedding
criteria as a label for the embedding rather than within the individual
schema names, to prevent them from getting cluttered.

``` r
read_dot(tar_read(gv_nullfree_schema.ex2))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-15.png"
style="height:5in" />

</div>

</figure>

</div>

Looking good! All that remains is to insert the data:

``` r
read_dot(tar_read(gv_nullfree_db.ex2))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-14.png"
style="height:5in" />

</div>

</figure>

</div>

The foreign keys here should really be plotted to show they’re a
distributed foreign key, but this is fine for now.

## Test

As another simple test, consider the following:

``` r
tar_read(data.ex5)
```

        a b  c
    1   1 1  1
    2   2 2  2
    3   3 3 NA
    4   4 4 NA
    5   5 1  2
    6   6 2  1
    7   7 3 NA
    8   8 4 NA
    9   9 1  3
    10 10 2  1
    11 11 3 NA
    12 12 4 NA

Minimal presence rules:

``` r
tar_read(minimal_presence_rule_fds.ex5)
```

    2 functional dependencies
    6 attributes: a, b, c, ¬a, ¬b, ¬c
     -> b
     -> a

Remaining embeddings:

``` r
read_dot(tar_read(dot_searched_embeddings.ex5))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-13.png"
style="height:5in" />

</div>

</figure>

</div>

Presence dependencies:

``` r
tar_read(presence_fds.ex5)[lengths(tar_read(presence_fds.ex5)) > 0]
```

    $`[a, b]`
    2 functional dependencies
    3 attributes: a, b, c
    a -> c
    b -> c

GEFDs:

``` r
tar_read(gefds.ex5)[lengths(tar_read(gefds.ex5)) > 0]
```

    $`[a, b]`
    1 functional dependency
    2 attributes: a, b
    a -> b

    $`[a, b, c]`
    1 functional dependency
    3 attributes: a, b, c
    a -> c

Final database:

``` r
read_dot(tar_read(gv_nullfree_db.ex5))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-12.png"
style="height:5in" />

</div>

</figure>

</div>

This looks correct. The schema is not exactly how we’d want it – the two
different partition keys mean that not everything is connected properly
for consistency – but that was a problem I expected, and is something I
want to handle as a separate step.

As a side-note, the two partition keys appear because we have
`a -> P(c)`, but `a -> b` and `b -> P(c)` make that transitive. If we
remove it before we create the schema, we get this:

``` r
read_dot(tar_read(nullfreetrim_dbgv5))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-11.png"
style="height:5in" />

</div>

</figure>

</div>

This has its own problems, of course.

## Test 2

Another interesting small case is the one where we have no information
in the main embedding. For example, suppose the key is the only
non-nullable set of attributes:

``` r
tar_read(data.ex6)
```

      a  b  c
    1 1  1 NA
    2 2  1 NA
    3 3  2 NA
    4 4 NA  1
    5 5 NA  1
    6 6 NA  2

Minimal presence rules:

``` r
tar_read(minimal_presence_rule_fds.ex6)
```

    5 functional dependencies
    6 attributes: a, b, c, ¬a, ¬b, ¬c
       -> a
     b -> ¬c
    ¬c -> b
     c -> ¬b
    ¬b -> c

Remaining embeddings:

``` r
read_dot(tar_read(dot_searched_embeddings.ex6))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-10.png"
style="height:5in" />

</div>

</figure>

</div>

Presence dependencies:

``` r
tar_read(presence_fds.ex6)[lengths(tar_read(presence_fds.ex6)) > 0]
```

    $`[a]`
    2 functional dependencies
    3 attributes: a, b, c
    a -> b
    a -> c

GEFDs:

``` r
tar_read(gefds.ex4)[lengths(tar_read(gefds.ex6)) > 0]
```

    $`[i, ¬v, l, u, d]`
    3 functional dependencies
    4 attributes: i, l, u, d
    i -> l
    i -> u
    i -> d

    $`[i, ¬v, l, u, d, p1]`
    1 functional dependency
    5 attributes: i, l, u, d, p1
    i -> p1

    $`[i, ¬v, l, u, d, ¬p2]`
    0 functional dependencies
    4 attributes: i, l, u, d

    $`[i, ¬p1, ¬p2]`
    0 functional dependencies
    1 attribute: i

    $`[i, ¬v, l, u, d, ¬p1, ¬p2]`
    0 functional dependencies
    4 attributes: i, l, u, d

    $`[i, ¬v, l, u, d, p1, ¬p2]`
    0 functional dependencies
    5 attributes: i, l, u, d, p1

Final database:

``` r
read_dot(tar_read(gv_nullfree_db.ex6))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-9.png"
style="height:5in" />

</div>

</figure>

</div>

## Test 3

To test everything works, here’s another, more elaborate example:

``` r
tar_read(data.ex4)
```

       id value lower_bound upper_bound distribution param1 param2
    1   1   2.3          NA          NA         <NA>     NA     NA
    2   2   2.3          NA          NA         <NA>     NA     NA
    3   3   5.7          NA          NA         <NA>     NA     NA
    4   4    NA         2.4         7.1      uniform     NA     NA
    5   5    NA         0.0        10.0      uniform     NA     NA
    6   6    NA         1.0        10.0      uniform     NA     NA
    7   7    NA         0.0        13.1      uniform     NA     NA
    8   8    NA         5.6        25.8      uniform     NA     NA
    9   9    NA         0.0        13.1       arcsin     NA     NA
    10 10    NA         5.6        25.8       arcsin     NA     NA
    11 11    NA         2.4        10.0         Beta    1.0      1
    12 12    NA         5.3        13.1         Beta    1.0      2
    13 13    NA         5.3        10.0         Beta    1.0      2
    14 14    NA         2.4        25.8         Beta    2.0      2
    15 15    NA         2.4        25.8  Kumaraswamy    2.0      2
    16 16    NA         2.4        25.8  Kumaraswamy    2.1      1
    17 17    NA         2.4        25.8  Kumaraswamy    2.0      1
    18 18    NA         2.4        13.1  Kumaraswamy    2.0      1
    19 19    NA         2.4        13.1         PERT    2.0     NA
    20 20    NA         2.4        25.8         PERT    1.0     NA
    21 21    NA         5.6        25.8         PERT    2.0     NA
    22 22    NA         2.4        25.8         PERT    2.0     NA
    23 23    NA         5.6        25.8       Wigner    2.0     NA
    24 24    NA         2.4        25.8       Wigner    2.0     NA

This has structure similar to the previous one, where the attribute that
determines whether another is present is not the attribute that
determines its value. We therefore expect the schema to not enforce some
structure properly.

We reduce the names to something more compact, so that the relation
names don’t become unmanageably long. We also only allow the ID and the
distribution to be GEFD determinants, since anything else is an artefact
of the limited data sample.

Minimal presence rules:

``` r
tar_read(minimal_presence_rule_fds.ex4)
```

    43 functional dependencies
    14 attributes: i, v, l, u, d, p1, p2, ¬i, ¬v, ¬l, ¬u, ¬d, ¬p1, ¬p2
        -> i
      v -> ¬l
     ¬l -> v
      v -> ¬u
     ¬u -> v
      v -> ¬d
     ¬d -> v
      v -> ¬p1
      v -> ¬p2
     ¬l -> ¬u
     ¬u -> ¬l
     ¬l -> ¬d
     ¬d -> ¬l
     ¬l -> ¬p1
     ¬l -> ¬p2
     ¬u -> ¬d
     ¬d -> ¬u
     ¬u -> ¬p1
     ¬u -> ¬p2
     ¬d -> ¬p1
     ¬d -> ¬p2
     p2 -> p1
     p2 -> l
     p2 -> u
     p2 -> d
     p2 -> ¬v
    ¬p1 -> ¬p2
     p1 -> l
     p1 -> u
     p1 -> d
     p1 -> ¬v
      l -> u
      u -> l
      l -> d
      d -> l
      l -> ¬v
     ¬v -> l
      u -> d
      d -> u
      u -> ¬v
     ¬v -> u
      d -> ¬v
     ¬v -> d

Remaining embeddings:

``` r
read_dot(tar_read(dot_searched_embeddings.ex4))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-8.png"
style="height:5in" />

</div>

</figure>

</div>

Presence dependencies:

``` r
tar_read(presence_fds.ex4)[lengths(tar_read(presence_fds.ex4)) > 0]
```

    $`[i]`
    6 functional dependencies
    7 attributes: i, v, l, u, d, p1, p2
    i -> v
    i -> l
    i -> u
    i -> d
    i -> p1
    i -> p2

    $`[i, ¬v, l, u, d]`
    2 functional dependencies
    6 attributes: i, l, u, d, p1, p2
    d -> p1
    d -> p2

GEFDs:

``` r
tar_read(gefds.ex4)[lengths(tar_read(gefds.ex4)) > 0]
```

    $`[i, ¬v, l, u, d]`
    3 functional dependencies
    4 attributes: i, l, u, d
    i -> l
    i -> u
    i -> d

    $`[i, ¬v, l, u, d, p1]`
    1 functional dependency
    5 attributes: i, l, u, d, p1
    i -> p1

    $`[i, v, ¬l, ¬u, ¬d, ¬p1, ¬p2]`
    1 functional dependency
    2 attributes: i, v
    i -> v

    $`[i, ¬v, l, u, d, p1, p2]`
    1 functional dependency
    6 attributes: i, l, u, d, p1, p2
    i -> p2

Final database:

``` r
read_dot(tar_read(gv_nullfree_db.ex4))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-7.png"
style="height:5in" />

</div>

</figure>

</div>

This looks about right: we can see the two key relation hierarchies for
`id` and `d`.

## Simplifying the schema

Looking around the examples, we can see that there are often embeddings
that don’t contain any embedded FDs: they just contain a key-only schema
for inter-embedding references. To simplify the resulting schema, we can
optionally remove these information-free embeddings. If they are in the
middle of a key chain, this requires joining up the embeddings on either
side of them.

Here’s the trimmed version of the interval example:

``` r
read_dot(tar_read(gv_pruned_nullfree_db.ex2))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-6.png"
style="height:5in" />

</div>

</figure>

</div>

For the simple example where the presence-determining attribute isn’t
the value determining attribute:

``` r
read_dot(tar_read(gv_pruned_nullfree_db.ex5))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-5.png"
style="height:5in" />

</div>

</figure>

</div>

Pruning the example with an empty main embedding shows an issue in our
plots:

``` r
read_dot(tar_read(gv_pruned_nullfree_db.ex6))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-4.png"
style="height:5in" />

</div>

</figure>

</div>

The remaining relations aren’t joined together at all! Removing the main
embedding still leaves us with a distributed key between the two
sub-embeddings, that makes them disjoint, but we’re not currently
showing those.

The distribution data has a similar problem for the distributed key
between embeddings `[iv, ¬ludp1p2]` and `[ilud, ¬v]`:

``` r
read_dot(tar_read(gv_pruned_nullfree_db.ex4))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-3.png"
style="height:5in" />

</div>

</figure>

</div>

We also need to decide how to remove the transitive foreign keys that
emerge from removing empty embeddings, as shown above. This can wait
until we’re showing the distributed keys, I think.

Here’s where I’d like to get to:

``` r
read_dot(paste(readLines(tar_read(goal4)), collapse = "\n"))
```

<div>

<figure class=''>

<div>

<img src="report_files\figure-commonmark\dot-figure-2.png"
style="height:5in" />

</div>

</figure>

</div>

## Session info

``` r
sessionInfo()
```

    R version 4.5.2 (2025-10-31 ucrt)
    Platform: x86_64-w64-mingw32/x64
    Running under: Windows 11 x64 (build 26200)

    Matrix products: default
      LAPACK version 3.12.1

    locale:
    [1] LC_COLLATE=English_United Kingdom.utf8 
    [2] LC_CTYPE=English_United Kingdom.utf8   
    [3] LC_MONETARY=English_United Kingdom.utf8
    [4] LC_NUMERIC=C                           
    [5] LC_TIME=English_United Kingdom.utf8    

    time zone: Europe/London
    tzcode source: internal

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
    [1] arules_1.7.14     Matrix_1.7-4      autodb_3.2.4.9000 targets_1.11.4   

    loaded via a namespace (and not attached):
     [1] vctrs_0.6.5       cli_3.6.5         knitr_1.50        rlang_1.1.7      
     [5] xfun_0.54         processx_3.8.6    generics_0.1.4    jsonlite_2.0.0   
     [9] data.table_1.17.8 glue_1.8.0        prettyunits_1.2.0 backports_1.5.0  
    [13] htmltools_0.5.8.1 ps_1.9.1          rmarkdown_2.30    grid_4.5.2       
    [17] evaluate_1.0.5    tibble_3.3.0      base64url_1.4     fastmap_1.2.0    
    [21] yaml_2.3.10       lifecycle_1.0.4   compiler_4.5.2    codetools_0.2-20 
    [25] igraph_2.2.1      pkgconfig_2.0.3   rstudioapi_0.17.1 lattice_0.22-7   
    [29] digest_0.6.37     R6_2.6.1          tidyselect_1.2.1  pillar_1.11.1    
    [33] callr_3.7.6       magrittr_2.0.4    withr_3.0.2       tools_4.5.2      
    [37] secretbase_1.1.1 
