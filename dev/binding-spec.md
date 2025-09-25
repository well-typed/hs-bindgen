# Binding Specification

## Versioning

* Versioning of binding specifications is *separate* from versioning of
  `hs-bindgen`.

* Binding specification versions are in `MAJOR.MINOR` format, where `MAJOR` is
  an integer `>= 1` and `MINOR` is an integer `>= 0`.

  The first binding specification version is `1.0`.

* Each major version is implemented in a module named `V{{MAJOR}}`, where the
  major version number is *not* padded.  The initial implementation has module
  `HsBindgen.BindingSpec.Private.V1`.

  For a given major version, all minor versions therefore have the same
  representation.

* Each version module defines `version` with the corresponding major version and
  the current minor version.

  To serve as an example, consider a point in time when we have the following
  modules:

    * Module `V1` has version `1.8`.
    * Module `V2` has version `2.3`.
    * Module `V3` has version `3.1`.

* We support two types of compatibility.

  With *strict* compatibility, the default, a version module can parse a binding
  specification with the same major version and a minor version that is not
  greater than the current minor version.

  (Example) With strict compatibility, module `V3` is compatible with binding
  specification files of version `3.0` or `3.1`.  It is *not* compatible with
  binding specification files of version `2.3`, `3.2`, or `4.0`.

  With *allow-newer* compatibility, a version module can parse any binding
  specification with the same major version, even if the minor version is
  greater than the current minor version.  CLI users can use the
  `--binding-spec-allow-newer` option for this.

  (Example) With allow-newer compatibility, module `V3` is compatible with
  binding specification files of version `3.0`, `3.1`, and any later minor
  version such as `3.11`.  It is *not* compatible with binding specification
  files of version `2.3` or `4.0`.

* This design is chosen so that developers have maximum choice in determining
  when to bump the major version number.  Since each major version requires a
  separate module, bumping the major version number has a significant cost.

* The API exposed by `HsBindgen.BindingSpec` and `HsBindgen.BindingSpec.Gen`
  uses the maximum supported version module.  `Frontend` only uses that version.

  The `--version` output is updated to also display the maximum supported
  binding specification version.

  (Example) The public API uses the `V3` module, and `--version` outputs
  `binding specification 3.1`.

* Design goal: a change in the behavior of `hs-bindgen` (such as defaults)
  should not change the interpretation of a binding specification.

  Binding specification files include the version of `hs-bindgen` used.  This
  can be helpful when debugging, and it can even be used to change the way that
  the file is parsed, in order to abide by this design goal.

* Within a version module, functions may be defined to translate from previous
  major versions.

  (Example) `V2` should implement `fromV1` to convert from a
  `V1.UnresolvedBindingSpec` to a `V2.UnresolvedBindingSpec`.  We should strive
  to support conversion from previous versions, making this function total.  If
  we ever run into a situation where we do not want to support conversion, we
  can return a `Maybe` and have a corresponding error.

  Conversion from older versions may be chained.

  (Example) Conversion from a `V1.UnresolvedBindingSpec` to a
  `V3.UnresolvedBindingSpec` may be implemented using a composition of
  `V2.fromV1` and `V3.fromV2`.

  If we ever run into a situation where that is problematic, however, we can
  implement additional conversion functions that skip over major versions.

  (Example) If we make a major mistake in `V2`, `V3` could implement `fromV1` so
  that `V1` binding specifications can be converted directly to `V3` binding
  specifications without going through the problematic `V2` representation.

  Another reason to implement conversion functions that skip over versions is
  performance.  For example, a policy to implement direct conversion from major
  version $`10n | n \in \mathbb{N}`$ to major version $`10(n+1)`$ would reduce
  the number of conversion functions needed to chain conversion functions
  between versions that differ significantly.  We do not need to worry about
  this anytime soon, and this can always be implemented later.

* As a rule, version modules may only import from version modules for previous
  versions.  In other words, all imports must go in the same direction.

  (Example) `V1.toV2` would require importing `V2` within `V1`.  This should
  never be done.

* Module `HsBindgen.BindingSpec.Private.Version` defines type `AVersion`, which
  contains the `hs-bindgen` and binding specification versions.  Module
  `HsBindgen.BindingSpec.Private.Common` defines functions that first parse the
  `AVersion` part of binding specification files.  The parsed `AVersion`
  determines how the whole `Aeson.Value` is parsed.

  Each version module implements a `parseValue` function of the following type,
  where the `UnresolvedBindingSpec` is the version for that module.

    ```haskell
    parseValue ::
         Monad m
      => Tracer m BindingSpecReadMsg
      -> BindingSpecCompatibility
      -> FilePath
      -> AVersion
      -> Aeson.Value
      -> m (Maybe UnresolvedBindingSpec)
    ```

  (Example) A binding specification file with version `3.0` could be parsed with
  the (current) `V3` module.

  (Example) A binding specification file with version `2.2` needs to be parsed
  with the `V2` module.  `V3.parseValue` should call `V2.parseValue` to get a
  `V2.UnresolvedBindingSpec`, and then it should convert to a
  `V3.UnresolvedBindingSpec` using `fromV2`.

  (Example) A binding specification file with version `1.5` needs to be parsed
  with the `V1` module.  `V3.parseValue` should call `V2.parseValue`, which
  should call `V1.parseValue` and use `V2.fromV1` to convert the result, and
  then `V3.fromV2` should convert that to a `V3.UnresolvedBindingSpec`.

  (Example) If `v2` is problematic, `V3.parseValue` should call `V1.parsevalue`
  and convert with `V3.fromV1` to avoid the problematic representation.

* When reading a binding specification file, conversion from a later major
  version is explicitly not supported.  Attempting to do so is an error.  (An
  error is traced and the empty binding specification is returned.)

  (Example) A binding specification file with version `4.0` is not supported by
  the version of `hs-bindgen` being used.

* Each version module can write a binding specification (to YAML or JSON), using
  the associated version.  It *cannot* write previous minor versions.

  (Example) `V1` can write `1.8` binding specification files.  `V2` can write
  `2.3` binding specification files.  `V3` can write `3.1` binding specification
  files.

  (Example) `V3` *cannot* write `3.0` binding specification files.

  Generated binding specifications are always the most recent version.

  (Example) Generated binding specification files have version `3.1`.

* We should implement utility commands to convert between binding specification
  versions.

  Forward conversion can be implemented using the same functions described
  above.  Users can convert to a specified major version, but the minor version
  is determined by `version`.

  (Example) A user may convert binding specification file `acme.yaml` of version
  `1.5` to the current version (`3.1`) with command
  `hs-bindgen-cli binding-spec convert --in-place acme.yaml`.

  (Example) A user may convert binding specification file `acme.yaml` of version
  `1.5` to major version 2 (`2.3`) with command
  `hs-bindgen-cli binding-spec convert --to 2 --in-place acme.yaml`.

  (Example) A user may convert binding specification file `acme.yaml` of version
  `1.5` to the supported major version 1 (`1.8`) with command
  `hs-bindgen-cli binding-spec convert --to 1 --in-place acme.yaml`.

* We may also want to support backward conversion.  While it is dangerous to do
  automatically, such a command would be helpful to users who need to do it for
  some reason.  (For example, a developer may develop binding specifications
  locally and then discover that the production environment is stuck on an older
  version.  They could convert and check/edit the result.)  If we do this, we
  can implement the necessary `to` conversion functions alongside the `from`
  conversion functions described above.

  (Example) A user may convert binding specification file `acme.yaml` of version
  `3.0` to major version 1 (`1.8`) with command
  `hs-bindgen-cli binding-spec convert --to 1 --in-place acme.yaml`.  In this
  case, conversion function `V3.toV2` and `V2.toV1` would be used.  If `V2` is
  problematic, `V3.toV1` could be implemented and used instead.

* Which forward (and backward) conversion are supported depends on which `from`
  (and `to`) conversion functions we implement.  It will always be up to us to
  decide what we want to support.  It is possible to completely drop support for
  really old versions at some point, for example.

* We should *not* get fancy and try to use type classes.  Conversion between
  versions is only done within the version modules.  A polymorphic
  implementation would be more difficult to reason about.

* Versions are implemented in the `Aeson` representations of binding
  specifications (`ABindingSpec`), but they are deliberately *not* included in
  `BindingSpec` types.  One reason for this design choice is that the version
  used to parse a binding specification should *not* be a factor in equality of
  binding specifications.

  The version used to parse a binding specification must *not* affect how the
  binding specification is used.

* Trace messages that inform users of parsed versions are emitted.  We should
  also emit a trace message for every conversion.  In the current
  implementation, parsed version trace messages are at level `Debug`, forward
  conversion trace messages are at level `Info`, and backward conversion trace
  messages are at level `Notice`.
