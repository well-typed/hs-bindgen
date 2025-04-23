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
          "un_Myint",
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
          "typedefs.h:1:13"},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Enum,
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Myint"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Myint"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Myint"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Myint"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Myint"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Myint"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Myint"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Myint"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Myint"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Myint"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Myint"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Myint"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
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
          "un_Intptr",
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
          "typedefs.h:2:15"},
      newtypeInstances = Set.fromList
        [Eq, Ord, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Intptr"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Intptr"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Intptr"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Intptr")]
