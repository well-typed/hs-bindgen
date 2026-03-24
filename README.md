# `hs-bindgen`

[![Build Status](https://github.com/well-typed/hs-bindgen/actions/workflows/haskell.yml/badge.svg)](https://github.com/well-typed/hs-bindgen/actions)
[![License: BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-lightgray.svg)](https://github.com/well-typed/hs-bindgen/blob/main/hs-bindgen/LICENSE)

`hs-bindgen` is a [Haskell][] library that *automatically* generates Haskell FFI
bindings from C header files.

[Haskell]: <https://www.haskell.org/>

> [!WARNING]
> This project has not had an official release yet.  There is a wide variety of
> C (and C preprocessor) code in the world, so we are currently soliciting
> feedback prior to the first official release.  Please try it out!  If
> something breaks, please check the [issues][] to see if the problem is already
> known, and open an issue if not.

[issues]: <https://github.com/well-typed/hs-bindgen/issues>

Check the [releases][] ([RSS][]) for release information.  A Hackage package
will be made available from the first official release.

[releases]: <https://github.com/well-typed/hs-bindgen/releases>
[RSS]: <https://github.com/well-typed/hs-bindgen/releases.atom>

## Documentation

* [`hs-bindgen` Manual][]
* [`hs-bindgen` Nix tutorial][]

[`hs-bindgen` manual]: <https://github.com/well-typed/hs-bindgen/blob/main/manual/README.md>
[`hs-bindgen` Nix tutorial]: <https://github.com/well-typed/hs-bindgen-tutorial-nix/blob/main/readme.md>

### Blog posts

* [`hs-bindgen 0.1-alpha` release](https://well-typed.com/blog/2026/02/hs-bindgen-alpha/)
  by Edsko de Vries (10 February 2026)
* [Wait, this isn't Haskell...](https://crtschin.com/posts/wait-this-isnt-haskell)
  by Curtis Chin Jen Sem (26 December 2025)

### Papers

* [Automatic C Bindings Generation for Haskell](https://dl.acm.org/doi/10.1145/3759164.3759350)
  (9 October 2025)
    * [Presentation](https://www.youtube.com/watch?v=SeLI3lMnhnA)

## Examples

We test `hs-bindgen` with various C projects, and we include some examples in
this repository.  Please note that these are used for testing and are not
meant to be complete.  See the [examples README][] for details.

[examples README]: <https://github.com/well-typed/hs-bindgen/tree/main/examples#examples>

Examples include:

* [Botan][], a C++ cryptography library:
  [bindings](https://github.com/well-typed/hs-bindgen/tree/main/examples/botan)
* [libpcap][], an interface to various kernel packet capture mechanisms:
  [bindings](https://github.com/well-typed/hs-bindgen/tree/main/examples/libpcap)
* [MiniSat][], a minimalistic, open-source SAT solver:
  [bindings](https://github.com/well-typed/hs-bindgen/tree/main/examples/c-minisat)
* [QR-Code-generator][], a library for generating QR codes:
  [bindings](https://github.com/well-typed/hs-bindgen/tree/main/examples/c-qrcode)
* [RogueUtil][], a cross-platform C library for terminal manipulation:
  [bindings](https://github.com/well-typed/hs-bindgen/tree/main/examples/c-rogueutil)
* [rpm][], a powerful package management system:
  [bindings](https://github.com/well-typed/hs-bindgen/tree/main/examples/c-rpm)

[Botan]: <https://botan.randombit.net/>
[libpcap]: <https://github.com/the-tcpdump-group/libpcap>
[minisat]: <https://github.com/niklasso/minisat-c-bindings>
[QR-Code-generator]: <https://github.com/nayuki/QR-Code-generator>
[RogueUtil]: <https://github.com/sakhmatd/rogueutil>
[rpm]: <https://github.com/rpm-software-management/rpm>

## Contributors

Our thanks go to those who have contributed to this project with development,
bug reports, feature requests, blog posts, etc., including:

* [Alex Mason](https://github.com/axman6)
* [`andromeda-fp`](https://github.com/andromeda-fp)
* [Armando Santos](https://github.com/bolt12)
* [`aveltras`](https://github.com/aveltras)
* [Chandler Barlow](https://github.com/chandler-barlow)
* [Curtis Chin Jen Sem](https://github.com/crtschin)
* [Dominik Schrempf](https://github.com/dschrempf)
* [Edsko de Vries](https://github.com/edsko)
* [Finley McIlwaine](https://github.com/FinleyMcIlwaine)
* [George Thomas](https://github.com/georgefst)
* [Jonathan Lorimer](https://github.com/JonathanLorimer)
* [Joris Dral](https://github.com/jorisdral)
* [`Krantz98`](https://www.reddit.com/user/Krantz98)
* [Lin Jian](https://github.com/jian-lin)
* [`Merivuokko`](https://github.com/Merivuokko)
* [Oleg Grenrus](https://github.com/phadej)
* [Sam Derbyshire](https://github.com/sheaf)
* [Tobias Dammers](https://github.com/tdammers)
* [Travis Cardwell](https://github.com/TravisCardwell)
