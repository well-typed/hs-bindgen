# Terminology

## Public

### Anonymous struct/union
[t:anon]: #anonymous-structunion

A [field][t:field] that declares a nested [untagged][t:untagged] struct or union
can optionally omit the [field name][t:named_field]. In this case, the [nested
struct/union][t:nested] is called an *anonymous struct/union*.

<details>
<summary>Notice</summary>

> In the past, we used the term *anonymous* erroneously in our code and
> documentation for "things that lack names" in general, such as untagged
> structs and unions.

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

### Direct field
[t:direct_field]: #direct-field

A *direct* field is a member of a struct or union itself as opposed to a member of its children.

### Enclosing struct/union
[t:enclosing]: #enclosing-structunion

A [field][t:field] is declared inside a struct/union definition. This parent
definition is called the *enclosing struct/union*.

### Regular field
[t:regular_field]: #regular-field

A *regular field* is a [direct][t:direct_field], [named][t:named_field] field.

### Field
[t:field]: #field

A *field* is described in the C reference as a "member" of a struct/union.
Fields are declared using variable declarations or [bit-field][t:bit_field]
declarations.

### File scope
[t:file_scope]: #file-scope

<https://en.cppreference.com/w/c/language/scope.html#File_scope>

### Implicit field

An *implicit* field* is a [direct][t:direct_field], [unnamed][t:unnamed_field]
field that references an [anonymous struct/union][t:anon].

<details>
<summary>Binding generation</summary>

* [Binding generation for anonymous structs][manual:structs/nesting-example-e]
* [Binding generation for anonymous unions][manual:unions/nesting-example-e]

</details>


### Indirect field
[t:indirect_field]: #indirect-field

[Named fields][t:named_field] of an [anonymous struct/union][t:anon] can be
accessed as if they were fields of the [enclosing struct/union][t:enclosing].
Such a field is called an *indirect field* with respect to the enclosing
struct/union. Such fields are still fields of the anonymous struct/union as
well.

This also works recursively: if an anonymous struct/union has indirect fields,
then the enclosing struct/union has its own indirect fields for those fields as
well.

<details>
<summary>Binding generation</summary>

* [Binding generation for indirect fields for structs][manual:structs/nesting-indirect-fields]
* [Binding generation for indirect fields for unions][manual:unions/nesting-indirect-fields]

</details>

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

### Tag
[t:tag]: #tag

A struct/union/enum (optionally) declares a name in the tag namespace. We refer
to this name as a *tag*.

### Tagged struct/union/enum

A struct or union or enum with a [tag][t:tag] is called *tagged*.

### Unnamed field
[t:unnamed_field]: #unnamed-field

A [field][t:field] without a name is called an *unnamed field*.
[bit-fields][t:bit_field] are allowed to be unnamed. Fields that declare an
[anonymous struct/union][t:anon] are unnamed by definition.

### Untagged struct/union/enum
[t:untagged]: #untagged-structunionenum

A struct or union or enum without a [tag][t:tag] is called *untagged*.



<!-- sources and references -->

[manual:structs/nesting-example-e]: low-level/translation/structs/nesting.md#example-e
[manual:structs/nesting-indirect-fields]: low-level/translation/structs/nesting.md#indirect-fields
[manual:unions/nesting-example-e]: low-level/translation/unions/nesting.md#example-e
[manual:unions/nesting-indirect-fields]: low-level/translation/unions/nesting.md#indirect-fields
