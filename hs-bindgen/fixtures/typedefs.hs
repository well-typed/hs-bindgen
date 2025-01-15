[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Myint",
      newtypeConstr = HsName
        "@NsConstr"
        "Myint",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unMyint",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "myint",
          typedefType = TypePrim
            (PrimIntegral PrimInt Signed),
          typedefSourceLoc =
          "examples/typedefs.h:1:13"}},
  DeclNewtypeInstance
    Storable
    (HsName
      "@NsTypeConstr"
      "Myint"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Intptr",
      newtypeConstr = HsName
        "@NsConstr"
        "Intptr",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unIntptr",
        fieldType = HsPtr
          (HsPrimType HsPrimCInt),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "intptr",
          typedefType = TypePointer
            (TypePrim
              (PrimIntegral PrimInt Signed)),
          typedefSourceLoc =
          "examples/typedefs.h:2:15"}},
  DeclNewtypeInstance
    Storable
    (HsName
      "@NsTypeConstr"
      "Intptr")]
