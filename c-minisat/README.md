To get this to work you will need to have `minisat` installed (`nix-shell -p minisat` works on NixOS)

Then go to `minisat-c-bindings` folder and run `make`.

After successfully compiling go to `minisat-c-bindings/build/dynamic/lib` and
run:

```
ln -s libminisat-c.so.1.0.0 libminisat-c.so
ln -s libminisat-c.so.1.0.0 libminisat-c.so.1
```

You'll need a cabal.project.local file with the following:

```
package c-minisat
    extra-include-dirs:
        <path-hs-bindgen>/c-minisat/minisat-c-bindings
      , <path-hs-bindgen>/c-minisat/minisat-c-bindings/build/dynamic/lib
    extra-lib-dirs:
        <path-hs-bindgen>/c-minisat/minisat-c-bindings
      , <path-hs-bindgen>/c-minisat/minisat-c-bindings/build/dynamic/lib
```

You'll also need to set

```
export LD_LIBRARY_PATH=<path-hs-bindgen>/c-minisat/minisat-c-bindings/build/dynamic/lib/:$LD_LIBRARY_PATH
```

In `c-minisat.cabal` you'll see a line with `extra-libraries: minisat-c`. This
is needed and the name should match the binary file.

- https://stackoverflow.com/questions/663209/can-someone-explain-linux-library-naming
- https://stackoverflow.com/questions/663209/can-someone-explain-linux-library-naming/21462448#21462448
