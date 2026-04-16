{ llvmPackages, git }:

''
  PROJECT_ROOT=$(${git}/bin/git rev-parse --show-toplevel)
  export PROJECT_ROOT

  # TODO: Adding `libclang` to the linker library path still seems to be
  # necessary, because otherwise Template Haskell issues a warning that it
  # cannot find `libclang.so`.
  LD_LIBRARY_PATH="$PROJECT_ROOT/manual/c:${llvmPackages.libclang.lib}/lib''${LD_LIBRARY_PATH:+:''${LD_LIBRARY_PATH}}"
  export LD_LIBRARY_PATH
''
