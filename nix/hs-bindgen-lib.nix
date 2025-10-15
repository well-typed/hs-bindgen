{
  haskell,
  bash,
  hs-bindgen-cli,
  hsBindgenHook,
}:

let
  hlib = haskell.lib.compose;
in
{
  generateBindings =
    generateBindingsScript:
    hlib.overrideCabal (drv: {
      executableToolDepends = (drv.executableToolDepends or [ ]) ++ [
        hs-bindgen-cli
        hsBindgenHook
      ];
      postUnpack = ''
        ${drv.postUnpack or ""}
        (cd ${drv.pname}; ${bash}/bin/bash ${generateBindingsScript})
      '';
    });
}
