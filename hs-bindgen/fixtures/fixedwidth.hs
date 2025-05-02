[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Uint32_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Uint32_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Uint32_t",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "uint32_t",
          typedefType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          typedefSourceLoc =
          "alltypes.h:131:25"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Uint64_t",
      newtypeConstr = HsName
        "@NsConstr"
        "Uint64_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Uint64_t",
        fieldType = HsPrimType
          HsPrimCULong,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "uint64_t",
          typedefType = TypePrim
            (PrimIntegral
              PrimLong
              Unsigned),
          typedefSourceLoc =
          "alltypes.h:136:25"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Uint64_t"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Uint64_t"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Uint64_t"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Uint64_t"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Uint64_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Uint64_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName
      "@NsTypeConstr"
      "Uint64_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName
      "@NsTypeConstr"
      "Uint64_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName
      "@NsTypeConstr"
      "Uint64_t"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName
      "@NsTypeConstr"
      "Uint64_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName
      "@NsTypeConstr"
      "Uint64_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName
      "@NsTypeConstr"
      "Uint64_t"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName
      "@NsTypeConstr"
      "Uint64_t"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Foo",
      structConstr = HsName
        "@NsConstr"
        "Foo",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_sixty_four",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Uint64_t"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "sixty_four",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "uint64_t"),
              fieldSourceLoc =
              "fixedwidth.h:4:11"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_thirty_two",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Uint32_t"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "thirty_two",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "uint32_t"),
              fieldSourceLoc =
              "fixedwidth.h:5:11"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "foo"),
          structAliases = [],
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "sixty_four",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "uint64_t"),
              fieldSourceLoc =
              "fixedwidth.h:4:11"},
            StructField {
              fieldName = CName "thirty_two",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "uint32_t"),
              fieldSourceLoc =
              "fixedwidth.h:5:11"}],
          structFlam = Nothing,
          structSourceLoc =
          "fixedwidth.h:3:8"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Foo",
        structConstr = HsName
          "@NsConstr"
          "Foo",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "foo_sixty_four",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Uint64_t"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "sixty_four",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "uint64_t"),
                fieldSourceLoc =
                "fixedwidth.h:4:11"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "foo_thirty_two",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Uint32_t"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "thirty_two",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "uint32_t"),
                fieldSourceLoc =
                "fixedwidth.h:5:11"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "foo"),
            structAliases = [],
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "sixty_four",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "uint64_t"),
                fieldSourceLoc =
                "fixedwidth.h:4:11"},
              StructField {
                fieldName = CName "thirty_two",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "uint32_t"),
                fieldSourceLoc =
                "fixedwidth.h:5:11"}],
            structFlam = Nothing,
            structSourceLoc =
            "fixedwidth.h:3:8"}}
      StorableInstance {
        storableSizeOf = 16,
        storableAlignment = 8,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Foo",
                structConstr = HsName
                  "@NsConstr"
                  "Foo",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_sixty_four",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Uint64_t"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "sixty_four",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint64_t"),
                        fieldSourceLoc =
                        "fixedwidth.h:4:11"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_thirty_two",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Uint32_t"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "thirty_two",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc =
                        "fixedwidth.h:5:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "foo"),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "sixty_four",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint64_t"),
                        fieldSourceLoc =
                        "fixedwidth.h:4:11"},
                      StructField {
                        fieldName = CName "thirty_two",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc =
                        "fixedwidth.h:5:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "fixedwidth.h:3:8"}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 8]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Foo",
                structConstr = HsName
                  "@NsConstr"
                  "Foo",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_sixty_four",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Uint64_t"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "sixty_four",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint64_t"),
                        fieldSourceLoc =
                        "fixedwidth.h:4:11"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_thirty_two",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Uint32_t"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "thirty_two",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc =
                        "fixedwidth.h:5:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "foo"),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "sixty_four",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint64_t"),
                        fieldSourceLoc =
                        "fixedwidth.h:4:11"},
                      StructField {
                        fieldName = CName "thirty_two",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc =
                        "fixedwidth.h:5:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "fixedwidth.h:3:8"}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Foo")]
