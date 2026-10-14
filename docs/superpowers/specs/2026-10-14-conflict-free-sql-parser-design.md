# Conflict-Free SQL Parser Design

## Context

The Menhir grammar currently reports 33 shift/reduce states with 125
arbitrarily resolved conflicts and 3 reduce/reduce states with 18 arbitrarily
resolved conflicts. The `ORDER BY` choice feature does not add to those counts;
this work starts from that feature's exact branch state.

## Goal

Reduce both conflict classes to zero without removing SQL forms accepted by the
checked regression corpus or changing their generated bindings and SQL.

## Non-goals

- Replacing Menhir or introducing a second parser.
- Hiding ambiguities with additional precedence declarations.
- Broad AST or type-system cleanup unrelated to resolving a conflict.
- Accepting malformed input and validating it after parsing.

## Design

Refactor the grammar structurally, one conflict family at a time:

1. Expression prefix, postfix, and operator overlaps.
2. DDL type suffixes, column attributes, keys, and constraints.
3. Table lists, join chains, and join conditions.
4. Statement prefixes for `INSERT`, `UPDATE`, routines, and `ALTER TABLE`.

Each family will be left-factored into nonterminals whose alternatives become
distinguishable before reduction. Clause-specific syntax will construct the
most precise AST type directly. A conflict will not be considered fixed when it
is merely resolved by `%prec`.

For every changed ambiguity, add a focused parser or cram regression covering
the accepted interpretation before changing the production. Existing AST
constructors and generated output remain unchanged unless a more precise type
is required to make an invalid parse state unrepresentable.

## Verification

After every conflict family:

- Regenerate the Menhir conflict report and require a monotonically decreasing
  count.
- Run focused tests for the changed productions.
- Run `opam exec -- dune runtest`.

Final acceptance requires:

- zero shift/reduce conflicts;
- zero reduce/reduce conflicts;
- no arbitrarily resolved conflicts;
- `opam exec -- dune build @all` succeeds;
- `opam exec -- dune runtest` succeeds;
- `./run_test` reports `OK`;
- generated output changes only where explicitly covered and reviewed.

## Type-design constraints

- Non-empty syntax remains non-empty in the AST.
- No domain-mode booleans or aliases are introduced.
- Pattern matches remain exhaustive.
- Raw tokens are parsed once into clause-specific types; downstream phases do
  not revalidate token combinations.
- Helpers remain local unless independently reused.
