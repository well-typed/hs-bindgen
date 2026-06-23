# Generate `LibYAML` Haskell bindings with `hs-bindgen`

This example demonstrates generating Haskell bindings for
[LibYAML](https://github.com/yaml/libyaml), a C library for parsing and emitting YAML.

It prints the YAML event stream of a small example document:

```yaml
# just a simple YAML example
string: some string
number: 12345
array:
- one
- two
- three
```

```console
YAML_STREAM_START_EVENT
YAML_DOCUMENT_START_EVENT
YAML_MAPPING_START_EVENT
YAML_SCALAR_EVENT: string
YAML_SCALAR_EVENT: some string
YAML_SCALAR_EVENT: number
YAML_SCALAR_EVENT: 12345
YAML_SCALAR_EVENT: array
YAML_SEQUENCE_START_EVENT
YAML_SCALAR_EVENT: one
YAML_SCALAR_EVENT: two
YAML_SCALAR_EVENT: three
YAML_SEQUENCE_END_EVENT
YAML_MAPPING_END_EVENT
YAML_DOCUMENT_END_EVENT
YAML_STREAM_END_EVENT
```

The project structure is similar to the example on [MiniSat](../c-minisat); see the
[MiniSat example README](../c-minisat/README.md).

## Run the example

```bash
./generate-and-run.sh
```

This script will:

1. Build `LibYAML`
2. Generate Haskell bindings using `hs-bindgen`
3. Create `cabal.project.local`
4. Build and run the example program.

## Notes

### Omitting field prefixes

We use the
[`--omit-field-prefixes` option](../../manual/low-level/translation/generated-names.md#duplicate-record-fields)
to generate field labels that are not prefixed by the name of the type.
Note that for the Haskell type `Yaml_event_t`, this would result in labels `type` and
`data`, which
[conflict with reserved keywords](../../manual/low-level/translation/generated-names.md#reserved-names).
To avoid this conflict, `hs-bindgen` appends an apostrophe/prime symbol `'` to
the reserved names as well as their `HasField` instances:

```haskell
data Yaml_event_t = Yaml_event_t
  { type' :: Yaml_event_type_t
  , data' :: Yaml_event_s_data
  ...
  }

instance ( ty ~ Yaml_event_type_t
         ) => RIP.HasField "type'" (RIP.Ptr Yaml_event_t) (RIP.Ptr ty) where
```

### Unions

The C type `yaml_event_t` contains a union with different content depending on
the event type. `hs-bindgen` generates an opaque type with getters and setters
as well as an instance `HasField "scalar"` that we use to access and print the
values of scalars. For more information, see the
[manual entry on unions](../../manual/low-level/translation/unions.md).
