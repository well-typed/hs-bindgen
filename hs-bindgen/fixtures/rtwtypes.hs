[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "UINT8_T",
      newtypeConstr = HsName
        "@NsConstr"
        "UINT8_T",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_UINT8_T",
        fieldType = HsPrimType
          HsPrimCSChar,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "rtwtypes.h:3:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "UINT8_T",
          macroArgs = [],
          macroBody = MTerm
            (MType
              (TypePrim
                (PrimChar
                  (PrimSignExplicit
                    Unsigned))))}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "UINT8_T"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Boolean_T",
      newtypeConstr = HsName
        "@NsConstr"
        "Boolean_T",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Boolean_T",
        fieldType = HsPrimType
          HsPrimCSChar,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "boolean_T",
          typedefType = TypePrim
            (PrimChar
              (PrimSignExplicit Unsigned)),
          typedefSourceLoc =
          "rtwtypes.h:7:19"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Boolean_T"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "Boolean_T")]
