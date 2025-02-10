[
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "erf",
      foreignImportType = HsFun
        (HsPrimType HsPrimCDouble)
        (HsIO
          (HsPrimType HsPrimCDouble)),
      foreignImportOrigName = "erf",
      foreignImportHeader =
      "examples/simple_func.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "erf",
          functionType = TypeFun
            [
              TypePrim
                (PrimFloating PrimDouble)]
            (TypePrim
              (PrimFloating PrimDouble)),
          functionHeader =
          "simple_func.h",
          functionSourceLoc =
          "examples/simple_func.h:1:8"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "bad_fma",
      foreignImportType = HsFun
        (HsPrimType HsPrimCDouble)
        (HsFun
          (HsPrimType HsPrimCDouble)
          (HsFun
            (HsPrimType HsPrimCDouble)
            (HsIO
              (HsPrimType HsPrimCDouble)))),
      foreignImportOrigName =
      "bad_fma",
      foreignImportHeader =
      "examples/simple_func.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "bad_fma",
          functionType = TypeFun
            [
              TypePrim
                (PrimFloating PrimDouble),
              TypePrim
                (PrimFloating PrimDouble),
              TypePrim
                (PrimFloating PrimDouble)]
            (TypePrim
              (PrimFloating PrimDouble)),
          functionHeader =
          "simple_func.h",
          functionSourceLoc =
          "examples/simple_func.h:3:22"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "no_args",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "no_args",
      foreignImportHeader =
      "examples/simple_func.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "no_args",
          functionType = TypeFun
            []
            TypeVoid,
          functionHeader =
          "simple_func.h",
          functionSourceLoc =
          "examples/simple_func.h:7:6"}}]
