# Terminology

* [C][t:c] — terms describing the C input
* [Haskell][t:haskell] — terms describing the generated Haskell bindings
* [`hs-bindgen`][t:hs-bindgen] — terms describing how `hs-bindgen` translates
  the C input into Haskell bindings

References of the form (6.10.5) point at the corresponding clause of the [C
standard][c-standard].

## C
[t:c]: #c

### Preprocessor

#### Function-like macro
[t:function-like-macro]: #function-like-macro

A [macro definition][t:macro-definition] of the form

```c
#define NAME(a, b) replacement-list
```

defines a *function-like* macro with [parameters][t:macro-parameter] `a` and `b`
(6.10.5). An occurrence of `NAME` immediately followed by its arguments enclosed
in `(` and `)` (similar to C function calls) is an
[invocation][t:macro-invocation] and is therefore replaced; a bare `NAME` is
left alone. The [arguments][t:macro-argument] of the invocation are substituted
for the corresponding parameters in the [replacement list][t:replacement-list]
(6.10.5.1).

#### Macro
[t:macro]: #macro

A *macro* is a [macro name][t:macro-name] associated with a [replacement
list][t:replacement-list]. Macros are either [object-like][t:object-like-macro]
or [function-like][t:function-like-macro].

#### Macro argument
[t:macro-argument]: #macro-argument

The preprocessing tokens supplied at a [function-like
macro][t:function-like-macro] [invocation][t:macro-invocation], enclosed in
parentheses and separated by commas (6.10.5). Arguments are substituted for
[parameters][t:macro-parameter].

#### Macro definition
[t:macro-definition]: #macro-definition

A macro *definition* is a `#define` preprocessing directive. It binds a [macro
name][t:macro-name] to a [replacement list][t:replacement-list].

#### Macro expansion
[t:macro-expansion]: #macro-expansion

The process by which a C preprocessor replaces [invocations][t:macro-invocation]
with the corresponding [replacement lists][t:replacement-list], substituting
[arguments][t:macro-argument] for [parameters][t:macro-parameter], applying the
`#` and `##` operators, and [rescanning][t:rescanning] the result.

Macro expansion operates on *preprocessing tokens*, not on text. The C standard
specifically refers to the substitution of invocations with their replacement
lists as "macro replacement" (6.10.5), and the overall process as "macro
expansion" (6.10.5.1); we use *macro expansion* throughout.

#### Macro invocation
[t:macro-invocation]: #macro-invocation

A macro *invocation* references a [macro definition][t:macro-definition] by
[name][t:macro-name], so that it can be replaced by the [replacement
list][t:replacement-list]. An [object-like][t:object-like-macro] macro is
invoked by its name alone; a [function-like][t:function-like-macro] macro is
invoked by its name followed by a parenthesised argument list.

#### Macro name
[t:macro-name]: #macro-name

The identifier immediately following `#define` in a [macro
definition][t:macro-definition] (6.10.5).

Macro names have their own namespace (6.10.5), separate from the namespaces of
label names, [tags][t:tag], struct/union [fields][t:field], attributes and
ordinary identifiers (6.2.3). A macro and a tag can therefore share a spelling.

#### Macro parameter
[t:macro-parameter]: #macro-parameter

One of the identifiers in the identifier list of a [function-like
macro][t:function-like-macro] definition (6.10.5). Contrast [macro
argument][t:macro-argument].

#### Object-like macro
[t:object-like-macro]: #object-like-macro

A [macro definition][t:macro-definition] of the form

```c
#define NAME replacement-list
```

defines an *object-like* macro. Every subsequent occurrence of `NAME` is
replaced by the [replacement list][t:replacement-list] (6.10.5).

#### Replacement list
[t:replacement-list]: #replacement-list

The sequence of preprocessing tokens making up the remainder of a [macro
definition][t:macro-definition]. White space preceding or following the list is
not part of it (6.10.1, 6.10.5).

A replacement list is a token sequence rather than text, and it need not form a
well-formed C construct. This is what makes generating bindings for macros hard;
see [macros][manual:translation/macros].

#### Rescanning
[t:rescanning]: #rescanning

After argument substitution and `#`/`##` processing, the resulting token
sequence is scanned again for further macro names to replace (6.10.5.4). This is
how macros chain: `A` expanding to `B` expanding to `C`.

Rescanning does not recurse. If the name of the macro currently being replaced
turns up during its own expansion it is *not* replaced, and it is never eligible
for replacement again.

#### Scope of a macro definition
[t:scope-of-a-macro-definition]: #scope-of-a-macro-definition

A [macro definition][t:macro-definition] lasts from its `#define` until a
matching `#undef`, or until the end of the preprocessing translation unit
(6.10.5.5). This is independent of block structure, and unrelated to the four
kinds of scope (1) function, (2) [file][t:file-scope], (3) block and (4)
function prototype scope (6.2.1).

### Structs, unions and enums

#### Anonymous struct/union
[t:anonymous-structunion]: #anonymous-structunion

An [unnamed][t:unnamed-field] [field][t:field] whose type is an
[untagged][t:untagged-structunionenum] struct or union is called an *anonymous
struct* or *anonymous union* (6.7.3.2). Its [named fields][t:named-field] become
[indirect fields][t:indirect-field] of the [enclosing
struct/union][t:enclosing-structunion].

The C standard applies the adjective to the *field*; we also apply it to the
[nested][t:nested-structunion] struct or union type that the field declares. The
two name the same construct. Our name for the field itself is [implicit
field][t:implicit-field].

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

#### Bit-field
[t:bit-field]: #bit-field

A *bit-field* is a [field][t:field] which is a specified number of bits wide
(6.7.3.2). Bit-fields are used for packing. An [unnamed][t:unnamed-field]
bit-field declares [padding][t:padding] instead.

#### Direct field
[t:direct-field]: #direct-field

A *direct* field is a [field][t:field] of a struct or union itself, as opposed
to a field of one of its [nested][t:nested-structunion] structs or unions.
Contrast [indirect field][t:indirect-field].

#### Enclosing struct/union
[t:enclosing-structunion]: #enclosing-structunion

A [field][t:field] is declared inside a struct/union definition. This parent
definition is called the *enclosing struct/union*.

#### Field
[t:field]: #field

A *field* is what the C standard calls a *member* of a struct or union
(6.7.3.2). Fields are declared using variable declarations or
[bit-field][t:bit-field] declarations.

We say "field" rather than "member" because the Haskell side generates [record
fields][t:record-field], and because the C standard itself already uses field in
*bit-field*.

#### Implicit field
[t:implicit-field]: #implicit-field

An *implicit* field is a [direct][t:direct-field], [unnamed][t:unnamed-field]
field whose type is an [anonymous struct/union][t:anonymous-structunion]. The C
standard has no separate name for it, since it calls the field itself the
anonymous struct or union.

<details>
<summary>Binding generation</summary>

* [Binding generation for anonymous structs][manual:structs/nesting-example-e]
* [Binding generation for anonymous unions][manual:unions/nesting-example-e]
</details>

#### Indirect field
[t:indirect-field]: #indirect-field

The [named fields][t:named-field] of an [anonymous
struct/union][t:anonymous-structunion] are fields of the [enclosing
struct/union][t:enclosing-structunion] as well (6.7.3.2), and can be accessed as
such. With respect to the enclosing struct/union, such a field is called an
*indirect field*. It remains a field of the anonymous struct/union too.

This applies recursively: if an anonymous struct/union has indirect fields, then
the enclosing struct/union has its own indirect fields for those fields as well.

*Indirect field* is a term coined by Clang (`IndirectFieldDecl`), not by the C
standard.

<details>
<summary>Binding generation</summary>

* [Binding generation for indirect fields for structs][manual:structs/nesting-indirect-fields]
* [Binding generation for indirect fields for unions][manual:unions/nesting-indirect-fields]
</details>

#### Named field
[t:named-field]: #named-field

A [field][t:field] with a name is called a *named field*; see also [unnamed
field][t:unnamed-field].

#### Nested struct/union
[t:nested-structunion]: #nested-structunion

A [field][t:field] can declare a new struct or union type, in which case it is
called a *nested struct/union*. A nested struct/union can be
[untagged][t:untagged-structunionenum].

Nesting does not introduce a scope of its own. If the nested struct/union has a
[tag][t:tag], that tag is visible from its declaration onwards in whichever
scope the [enclosing struct/union][t:enclosing-structunion] is declared in
(6.2.1). For declarations at [file scope][t:file-scope], this is the rest of the
[translation unit][t:translation-unit]. An untagged nested struct/union declares
no tag at all and so cannot be referred to elsewhere.

#### Padding
[t:padding]: #padding

A compiler may insert unnamed *padding* between [fields][t:field] and at the end
of a struct or union, but not at the beginning (6.7.3.2).

Padding can also be declared explicitly, using an [unnamed][t:unnamed-field]
[bit-field][t:bit-field].

<details>
<summary>Binding generation</summary>

Unnamed bit-fields are not translated to fields in the corresponding Haskell
record in the generated Haskell bindings.
</details>

#### Regular field
[t:regular-field]: #regular-field

A *regular field* is a [direct][t:direct-field], [named][t:named-field] field.

#### Tag
[t:tag]: #tag

A struct/union/enum (optionally) declares a name in the tag namespace. We refer
to this name as a *tag*.

There is a single tag namespace shared by all three tags (6.2.3), and it is
separate from the namespace of ordinary identifiers, so `struct date` and a
`typedef` named `date` can coexist.

#### Tagged struct/union/enum
[t:tagged-structunionenum]: #tagged-structunionenum

A struct or union or enum with a [tag][t:tag] is called *tagged*. The C standard
says "with a tag"; we use *tagged*.

#### Unnamed field
[t:unnamed-field]: #unnamed-field

A [field][t:field] without a name is called an *unnamed field*.
[Bit-fields][t:bit-field] are allowed to be unnamed, and fields that declare an
[anonymous struct/union][t:anonymous-structunion] are unnamed by definition
(6.7.3.2).

#### Untagged struct/union/enum
[t:untagged-structunionenum]: #untagged-structunionenum

A struct or union or enum without a [tag][t:tag] is called *untagged*. The C
standard says "without a tag"; we use *untagged*.

### Declarations and scope

#### File scope
[t:file-scope]: #file-scope

An identifier whose declarator or type specifier appears outside any block or
parameter list has *file scope*: it is visible from the point of declaration
until the end of the [translation unit][t:translation-unit]. File scope is one
of four kinds of scope, the others being function, block and function prototype
scope (6.2.1).

See also ["scope" on cppreference.com][creference:scope].

#### Translation unit
[t:translation-unit]: #translation-unit

A source file together with all the headers and source files it includes is a
*preprocessing translation unit*; after preprocessing, it is called a
*translation unit* (5.1.1.1). `hs-bindgen` translates one translation unit per
invocation; see [includes][manual:includes].

## Haskell
[t:haskell]: #haskell

### Bindings

#### Binding
[t:binding]: #binding

A *binding* is a Haskell declaration providing access to a C declaration: a
foreign import for a function, a record type for a struct, a value for a [macro
value][t:macro-value], and so on.

#### High-level bindings
[t:high-level-bindings]: #high-level-bindings

Bindings written in idiomatic Haskell, hiding the C representation behind
Haskell types and conventions. `hs-bindgen` does not generate high-level
bindings; they are hand-written on top of the [low-level
bindings][t:low-level-bindings].

See the [roadmap][manual:roadmap].

#### Low-level bindings
[t:low-level-bindings]: #low-level-bindings

Bindings mirroring the C declarations one for one and using C types (`CInt`,
`Ptr`, …). See the [introduction to the low-level
API][manual:low-level/introduction].

### Generated declarations

#### Newtype wrapper
[t:newtype-wrapper]: #newtype-wrapper

Many bindings are generated as a `newtype` around the underlying C
representation rather than as a bare type synonym, so that the C type and its
uses stay distinct in Haskell. [Macro types][t:macro-type] are generated this
way, and so are enums; but beware, enums are not Haskell sum types, see
[enums][manual:enums].

#### Opaque data type
[t:opaque-data-type]: #opaque-data-type

For a C type whose definition is not available (e.g., an incomplete struct),
`hs-bindgen` generates a Haskell data type with no constructors, used only
behind a `Ptr`. See [opaque structs][manual:structs-opaque].

#### Record field
[t:record-field]: #record-field

A field of a generated Haskell record. `hs-bindgen` generates one record field
per [regular field][t:regular-field] of a struct; [unnamed][t:unnamed-field]
[bit-fields][t:bit-field] are not translated.

## `hs-bindgen`
[t:hs-bindgen]: #hs-bindgen

### Macros

#### Macro language
[t:macro-language]: #macro-language

The pluggable `hs-bindgen` component that parses, typechecks and translates
[macro definitions][t:macro-definition]: the [macro name][t:macro-name], the
[parameters][t:macro-parameter], and the [replacement list][t:replacement-list].

#### Macro type
[t:macro-type]: #macro-type

A [macro][t:macro] whose [replacement list][t:replacement-list] parses as a C
*type* expression, and which is therefore translated to a Haskell type using a
[newtype wrapper][t:newtype-wrapper] around the translation of that C type. For
example, `#define YEAR int`.

#### Macro value
[t:macro-value]: #macro-value

A [macro][t:macro] whose [replacement list][t:replacement-list] parses as a C
*expression*, and which is therefore translated to a Haskell value binding. For
example, `#define EPSILON 0.1`.

#### Parsable macro
[t:parsable-macro]: #parsable-macro

A [macro definition][t:macro-definition] that the active [macro
language][t:macro-language] can parse: the [macro name][t:macro-name], the
[parameters][t:macro-parameter] if the macro is
[function-like][t:function-like-macro], and the [replacement
list][t:replacement-list].

#### Reparsing
[t:reparsing]: #reparsing

The second parse of those declarations that contain macro
[expansions][t:macro-expansion], recovering the [macro types][t:macro-type] that
`libclang` had already expanded away; see [macros][manual:translation/macros].

### Selection

#### External declaration
[t:external-declaration]: #external-declaration

A declaration listed in an external [binding
specification][t:binding-specification]. `hs-bindgen` does not generate bindings
for it, and instead imports the declaration from the specified module instead.

#### Omitted declaration
[t:omitted-declaration]: #omitted-declaration

A declaration that is not translated due to a prescriptive [binding
specification][t:binding-specification]. We use *omit* exclusively for this. We
avoid *omit* in the context of [deselection][t:selected-declaration] or
translation failures.

#### Program slicing
[t:program-slicing]: #program-slicing

Translating a declaration is only useful if its transitive dependencies are
translated too. *Program slicing* determines those dependencies and selects
them, even if a [selection predicate][t:selection-predicate] deselected them.
See [selecting and program slicing][manual:selecting-and-program-slicing].

#### Selected declaration
[t:selected-declaration]: #selected-declaration

A declaration that `hs-bindgen` will translate. Declarations are *selected* or
*deselected*, by a [selection predicate][t:selection-predicate] and by [program
slicing][t:program-slicing]. We avoid the terms *exclude*, *skip* and
[omit][t:omitted-declaration] in the context of selection.

#### Selection predicate
[t:selection-predicate]: #selection-predicate

A predicate matched against declarations, determining which ones are
[selected][t:selected-declaration] for translation. See [selecting and program
slicing][manual:selecting-and-program-slicing].

### Naming and configuration

#### Binding specification
[t:binding-specification]: #binding-specification

A YAML or JSON file specifying details about the bindings of a single Haskell
module. A *prescriptive* binding specification guides generation (custom names,
type representations, instances, [omission][t:omitted-declaration]); an
*external* binding specification declares that the listed types come from
another module, so that separately generated bindings compose. See [binding
specifications][manual:binding-specifications].

#### Name mangling
[t:name-mangling]: #name-mangling

C names are not valid Haskell names, and the two languages have different
conventions. The *name mangler* derives all Haskell names from the corresponding
C names, staying as close to the C spelling as possible. See [generated
names][manual:generated-names].

#### Root directive
[t:root-directive]: #root-directive

A preprocessing directive that `hs-bindgen` emits ahead of the headers being
translated, such as `--hash-define NAME VALUE`. Root directives are ordered and
also apply to the compilation of the generated C source. See
[invocation][manual:invocation] and [C stages][manual:c-stages].



<!-- sources and references -->

[c-standard]: https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf
[creference:scope]: https://en.cppreference.com/w/c/language/scope.html
[manual:binding-specifications]: low-level/usage/binding-specifications.md
[manual:c-stages]: low-level/usage/c-stages.md
[manual:enums]: low-level/translation/enums.md
[manual:generated-names]: low-level/translation/generated-names.md
[manual:includes]: low-level/usage/includes.md
[manual:invocation]: low-level/usage/invocation.md
[manual:low-level/introduction]: low-level/introduction.md
[manual:roadmap]: roadmap.md
[manual:selecting-and-program-slicing]: low-level/usage/selecting-and-program-slicing.md
[manual:structs-opaque]: low-level/translation/structs.md#opaque-structs
[manual:structs/nesting-example-e]: low-level/translation/structs/nesting.md#example-e
[manual:structs/nesting-indirect-fields]: low-level/translation/structs/nesting.md#indirect-fields
[manual:translation/macros]: low-level/translation/macros.md
[manual:unions/nesting-example-e]: low-level/translation/unions/nesting.md#example-e
[manual:unions/nesting-indirect-fields]: low-level/translation/unions/nesting.md#indirect-fields
