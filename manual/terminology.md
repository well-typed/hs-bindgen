# Terminology

## Public

### Anonymous struct/union
[t:anon]: #anonymous-structunion

A [field][t:field] that declares a nested [untagged][t:untagged] struct or union
can optionally omit the [field name][t:named_field]. In this case, the [nested
struct/union][t:nested] is called an *anonymous struct/union*.

<details>
<summary>WARNING</summary>

> In our code and documentation, the term *anonymous* is sometimes used
> erroneously for "things that lack names" in general, such as untagged structs
> and unions. It is on our TODO list to audit the code and documentation to
> consistently apply the *anonymous* terminology correctly. See [issue
> #1893][issue:1893].

</details>

<details>
<summary>Example</summary>

```c
struct S {
  union {
    int x;
    char y;
  };
};
```

</details>

<details>
<summary>Binding generation</summary>

* [Binding generation for anonymous structs][manual:structs/nesting-example-e]
* [Binding generation for anonymous unions][manual:unions/nesting-example-e]

</details>

### Bit-field
[t:bit_field]: #bit-field

A *bit-field* is a special kind of [field][t:field] that has a bit-width, used
for packing and [padding][t:padding].

### Enclosing struct/union
[t:enclosing]: #enclosing-structunion

A [field][t:field] is declared inside a struct/union definition. This parent
definition is called the *enclosing struct/union*.

### Field
[t:field]: #field

A *field* is described in the C reference as a "member" of a struct/union.
Fields are declared using variable declarations or [bit-field][t:bit_field]
declarations.

### File scope
[t:file_scope]: #file-scope

<https://en.cppreference.com/w/c/language/scope.html#File_scope>

### Function-like macro
[t:function_like]: #function-like-macro

A *function-like* macro replaces each occurrence of that macro's name with its
definition, additionally taking a list of named arguments, which replace
corresponding occurrences of the argument name in the definition.

### Indirect field

[Fields][t:field] of an [anonymous struct/union][t:anon] can be accessed as if
they were fields of the [enclosing struct/union][t:enclosing]. Such a field is
called an *indirect field* with respect to the enclosing struct/union. Such
fields are still fields of the anonymous struct/union as well.

<details>
<summary>Binding generation</summary>

Haskell bindings are generated for the field declaring the [anonymous
struct/union][t:anon] rather than for the indirect fields.

</details>

### Macro
[t:macro]: #macro

A (text) macro is a name associated with a replacement text.

### Macro definition
[t:macro_def]: #macro-definition

A macro *definition* declares a name in the macro namespace with a replacement
text. A macro can be [object-like][t:object_like] or
[function-like][t:function_like]. Macro definitions might be
[parsable][t:parsable_macro].

### Macro expansion
[t:macro_expansion]: #macro-expansion

A C preprocessor *expands* macro [invocations][t:macro_inv], replacing
invocations by their definition, which may be processed additionally depending
on the type of the macro [definition][t:macro_def].

### Macro invocation
[t:macro_inv]: #macro-invocation

A macro *invocation* references a macro definition by name so that it can be
replaced by its definition. Macro invocations are [expanded][t:macro_expansion]
by a C preprocessor. An [object-like][t:object_like] macro is invoked by
referencing the macro's name. A [function-like][t:function_like] macro is
invoked as if it is a function call.

### Macro type
[t:macro_type]: #macro-type

A [macro][t:macro] that is parsed by `hs-bindgen` as a Haskell type.

### Macro value
[t:macro_value]: #macro-value

A [macro][t:macro] that is parsed by `hs-bindgen` as a Haskell value.

### Named field
[t:named_field]: #named-field

A [field][t:field] with a name is called a *named field*. All fields should be
named, with some exceptions: see [unnamed field][t:unnamed_field].

### Nested struct/union
[t:nested]: #nested-structunion

A [field][t:field] can declare a new struct or union type, in which case it is
called a *nested struct/union*. The nested struct/union can be
[untagged][t:untagged] as usual. The nested struct/union typically has [file
scope][t:file_scope] as long as its [enclosing struct/union][t:enclosing] has
file scope, which is usually if not always the case.

### Object-like macro
[t:object_like]: #object-like-macro

An *object-like* macro replaces each occurrence of that macro's name with its
definition.

### Padding
[t:padding]: #padding

Unnamed padding may be inserted by the compiler in between [fields][t:field].
Unnamed padding can be inserted by the using an [unnamed][t:unnamed_field]
[bit-field][t:bit_field].

<details>
<summary>Binding generation</summary>

Unnamed bit-fields are not translated to fields in the corresponding Haskell
record in the generated Haskell bindings.
</details>

### Parsable macro
[t:parsable_macro]: #parsable-macro

A *parsable* macro is a macro definition that can be parsed as a [macro
type][t:macro_type] or as a [macro value][t:macro_value].

### Reparsing
[t:reparsing]: #reparsing

`hs-bindgen` parses headers once, and a subset of the headers is parsed a second
time (those declarations that contain macro expansions). The second *reparse*
inspects the source C code to augment the Haskell bindings with [macro
types][t:macro_type].

### Tag
[t:tag]: #tag

A struct/union/enum (optionally) declares a name in the tag namespace. We refer
to this name as a *tag*.

### Tagged struct/union/enum

A struct or union or enum with a [tag][t:tag] is called *tagged*.

### Type macro

See [macro type][t:macro_type].

### Unnamed field
[t:unnamed_field]: #unnamed-field

A [field][t:field] without a name is called an *unnamed field*.
[bit-fields][t:bit_field] are allowed to be unnamed. Fields that declare an
[anonymous struct/union][t:anon] are unnamed by definition.

### Untagged struct/union/enum
[t:untagged]: #untagged-structunionenum

A struct or union or enum without a [tag][t:tag] is called *untagged*.

### Value macro

See [macro value][t:macro_value].



## Internal

### Implicit field

Any field that is not directly accessible through the `libclang` API is
represented as an *implicit field* in `libclang`'s C AST. Fields that declare an
anonymous struct/union are such implicit fields. Indirect fields are also
represented in `libclang` internally as implicit fields.



<!-- sources and references -->

[issue:1893]: https://github.com/well-typed/hs-bindgen/issues/1893
[manual:structs/nesting-example-e]: low-level/translation/structs/nesting.md#example-e
[manual:unions/nesting-example-e]: low-level/translation/unions/nesting.md#example-e
