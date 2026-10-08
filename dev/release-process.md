# Release Process

We release `hs-bindgen` and `hs-bindgen-runtime` to Hackage.

## Prerequisites

* [ ] Changelog checks (`hs-bindgen/CHANGELOG.md`,
  `hs-bindgen-runtime/CHANGELOG.md`):
  * Check that all user-facing changes have been recorded.
  * Check that each changelog entry is in the correct category.
  * Check that each changelog entry links to a PR, if applicable.

* [ ] Dependency checks (`cabal.project.base`, see
  [Managing Dependencies](dependencies.md)):
  * Remove all `source-repository-package` stanzas: the released packages must
    build against Hackage releases only.
  * Update the `index-state` to the current date-time, or the closest valid
    date-time to the current date-time, so that CI builds and tests the
    libraries with the newest versions of dependencies. Revisit the versions
    pinned in `nix/generate.sh` accordingly.

* [ ] Decide on new version number (`MAJOR.MAJOR.MINOR.PATCH`): Releases follow
  the [Haskell Package Versioning Policy](https://pvp.haskell.org/). We use
  version numbers consisting of 4 parts, like `A.B.C.D`.
  * `A.B` is the *major* version number. A bump indicates a breaking change.
  * `C` is the *minor* version number. A bump indicates a non-breaking change.
  * `D` is the *patch* version number. A bump indicates small changes or minor
    fixes not affecting users directly.

* Tag name: `release-${VERSION}`

## Preparation

* [ ] Set the version in all cabal files

* [ ] Set the `source-repository this` tags in `hs-bindgen.cabal` and
  `hs-bindgen-runtime.cabal`

* [ ] Update the `hs-bindgen-runtime` bounds in `hs-bindgen.cabal`

* [ ] Regenerate the Nix expressions of our own packages

    ```
    $ ./nix/generate.sh hs-bindgen hs-bindgen-runtime hs-bindgen-test-runtime
    ```

* [ ] Update the `hs-bindgen` version recorded in binding specifications
  (`hs_bindgen: ${VERSION}`)
  * [ ] Regenerate the golden fixtures

    ```
    $ cabal run test-hs-bindgen -- --accept
    ```

  * [ ] Update the hand-written binding specifications and the manual; find
    them with

    ```
    $ git grep -n -e "hs_bindgen: ${OLD_VERSION}" -e "hs-bindgen ${OLD_VERSION}"
    ```

* [ ] Update both `CHANGELOG`s
    * [ ] Set the version number
    * [ ] Set the release date (UTC)

* [ ] Ensure `cabal check` is green for both released packages

## Git

* [ ] Ensure the changes above land on `main`

* [ ] Tag the release

    ```
    $ git tag "${TAG}" -m "Release ${VERSION}"
    ```

* [ ] Push the tag

    ```
    $ git push origin "${TAG}"
    ```

## Hackage

* [ ] Run `cabal check` in `hs-bindgen-runtime/` and `hs-bindgen/`

* [ ] Release `hs-bindgen-runtime` to Hackage, then `hs-bindgen`

* [ ] Manually create documentation with `cabal-install` HEAD (which contains a
      fix required for Haddocks of re-exports) and upload it

## Preparation for next release

* [ ] Update both `CHANGELOG`s, adding a new section at top

```markdown
## ?.?.?.? -- YYYY-mm-dd

### Breaking changes

### New features

### Minor changes

### Bug fixes
```
