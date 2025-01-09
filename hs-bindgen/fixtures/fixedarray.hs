[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Triple",
      newtypeConstr = HsName
        "@NsConstr"
        "Triple",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unTriple",
        fieldType = HsConstArray
          3
          (HsPrimType HsPrimCInt),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "triple",
          typedefType = TypeConstArray
            3
            (TypePrim
              (PrimIntegral PrimInt Signed)),
          typedefSourceLoc =
          "examples/fixedarray.h:1:13"}},
  DeclNewtypeInstance
    Storable
    (HsName
      "@NsTypeConstr"
      "Triple")]
