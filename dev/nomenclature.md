Since we use various types of predicates, we agree on the following
nomenclature:

- In the `Parse` pass, we either _succeed_, _do not attempt_ (the declaration
  is unavailable on this platform, or `libclang` does not know its
  availability), or _fail_ to parse a declaration.
- In the `Select` pass, we either _select_ or _deselect_ a declaration.
- Prescriptive binding specifications can _omit_ types; a declaration can be
  _extern_ due to an external binding specification.
- Edges of the `DeclUse`/`UseDecl` graph are labelled with a `Dependency`: the
  dependent either _needs the shape_ (`NeedsShape`) or _needs the name only_
  (`NeedsNameOnly`) of the dependency. We avoid the old _by-value_/_by-reference_
  framing.
- A declaration's _dependency edges_ go to the declarations it uses; its _use
  edges_ come from the declarations that use it.

The declaration database (`HsBindgen.Frontend.DeclDB`) has one entry per
declaration. The kind of the entry says what we generate for it:

- _active_: a binding, from the declaration we have at the current pass;
- _squashed_: the binding of the target, into which the typedef was squashed;
- _external_: nothing, because an external binding specification defines it
  elsewhere;
- _deselected_: nothing, because `Select` did not select it;
- _inactive_: nothing at all; we failed or did not attempt to parse it, its
  name conflicts, or a pass deactivated it (it is _unusable_ or _omitted_).

_Active_ describes the representation: do we have the declaration at this
pass? _Usable_ and _selected_ are judgements. Every entry but an inactive one
is usable; a usable declaration may still have unusable transitive
dependencies, which `Select` takes into account.

In particular,
- we avoid the terms _exclude_, and _skip_.
- we use the term _omit_ exclusively for the effect of prescriptive binding
  specifications.
