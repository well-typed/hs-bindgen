Since we use various types of predicates, we agree on the following
nomenclature:

- In the `Parse` pass, we either _succeed_, _do not attempt_, or _fail_ to parse
  a declaration.
- In the `Select` pass, we either _select_ or _deselect_ a declaration.
- Prescriptive binding specifications can _omit_ types; a declaration can be
  _extern_ due to an external binding specification.

In particular,
- we avoid the terms _exclude_, and _skip_.
- we use the term _omit_ exclusively for the effect of prescriptive binding
  specifications.
