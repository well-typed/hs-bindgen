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
      "simple_func.h",
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
      "simple_func.h",
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
        (HsPrimType HsPrimVoid),
      foreignImportOrigName =
      "no_args",
      foreignImportHeader =
      "simple_func.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "no_args",
          functionType = TypeFun
            []
            (TypePrim PrimVoid),
          functionHeader =
          "simple_func.h",
          functionSourceLoc =
          "examples/simple_func.h:7:6"}}]
