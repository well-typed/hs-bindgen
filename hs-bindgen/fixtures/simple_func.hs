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
          functionArgs = [
            TypePrim
              (PrimFloating PrimDouble)],
          functionRes = TypePrim
            (PrimFloating PrimDouble),
          functionHeader =
          "simple_func.h",
          functionSourceLoc =
          "simple_func.h:1:8"}},
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
          functionArgs = [
            TypePrim
              (PrimFloating PrimDouble),
            TypePrim
              (PrimFloating PrimDouble),
            TypePrim
              (PrimFloating PrimDouble)],
          functionRes = TypePrim
            (PrimFloating PrimDouble),
          functionHeader =
          "simple_func.h",
          functionSourceLoc =
          "simple_func.h:3:22"}},
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
      "simple_func.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "no_args",
          functionArgs = [],
          functionRes = TypeVoid,
          functionHeader =
          "simple_func.h",
          functionSourceLoc =
          "simple_func.h:7:6"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "no_args_no_void",
      foreignImportType = HsIO
        (HsPrimType HsPrimUnit),
      foreignImportOrigName =
      "no_args_no_void",
      foreignImportHeader =
      "simple_func.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName
            "no_args_no_void",
          functionArgs = [],
          functionRes = TypeVoid,
          functionHeader =
          "simple_func.h",
          functionSourceLoc =
          "simple_func.h:9:6"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun",
      foreignImportType = HsFun
        (HsPrimType HsPrimCChar)
        (HsFun
          (HsPrimType HsPrimCDouble)
          (HsIO (HsPrimType HsPrimCInt))),
      foreignImportOrigName = "fun",
      foreignImportHeader =
      "simple_func.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "fun",
          functionArgs = [
            TypePrim (PrimChar Nothing),
            TypePrim
              (PrimFloating PrimDouble)],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed),
          functionHeader =
          "simple_func.h",
          functionSourceLoc =
          "simple_func.h:11:5"}}]
