{
  lib,
  makeSetupHook,
  #
  llvmPackages,
}:

makeSetupHook {
  name = "hs-bindgen-hook";
  substitutions = {
    inherit (llvmPackages) clang;
    libclang = (lib.getLib llvmPackages.libclang);
  };
} ./hs-bindgen-hook.sh
