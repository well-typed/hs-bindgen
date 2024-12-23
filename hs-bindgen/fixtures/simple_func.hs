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
          functionSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "simple_func.h"],
            singleLocLine = 1,
            singleLocColumn = 8}}},
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
          functionSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "simple_func.h"],
            singleLocLine = 3,
            singleLocColumn = 22}}}]
