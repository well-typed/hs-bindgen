[
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
          "unUint64_t",
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
          "musl-include/x86_64/bits/alltypes.h:136:25"}},
  DeclNewtypeInstance
    Storable
    (HsName
      "@NsTypeConstr"
      "Uint64_t"),
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
          "unUint32_t",
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
          "musl-include/x86_64/bits/alltypes.h:131:25"}},
  DeclNewtypeInstance
    Storable
    (HsName
      "@NsTypeConstr"
      "Uint32_t"),
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
              fieldType = TypeTypedef
                (CName "uint64_t"),
              fieldSourceLoc =
              "examples/fixedwidth.h:4:11"}},
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
              fieldType = TypeTypedef
                (CName "uint32_t"),
              fieldSourceLoc =
              "examples/fixedwidth.h:5:11"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "foo"))
            DeclPathTop,
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "sixty_four",
              fieldOffset = 0,
              fieldType = TypeTypedef
                (CName "uint64_t"),
              fieldSourceLoc =
              "examples/fixedwidth.h:4:11"},
            StructField {
              fieldName = CName "thirty_two",
              fieldOffset = 64,
              fieldType = TypeTypedef
                (CName "uint32_t"),
              fieldSourceLoc =
              "examples/fixedwidth.h:5:11"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/fixedwidth.h:3:8",
          structBitfields = []}},
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
                fieldType = TypeTypedef
                  (CName "uint64_t"),
                fieldSourceLoc =
                "examples/fixedwidth.h:4:11"}},
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
                fieldType = TypeTypedef
                  (CName "uint32_t"),
                fieldSourceLoc =
                "examples/fixedwidth.h:5:11"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "foo"))
              DeclPathTop,
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "sixty_four",
                fieldOffset = 0,
                fieldType = TypeTypedef
                  (CName "uint64_t"),
                fieldSourceLoc =
                "examples/fixedwidth.h:4:11"},
              StructField {
                fieldName = CName "thirty_two",
                fieldOffset = 64,
                fieldType = TypeTypedef
                  (CName "uint32_t"),
                fieldSourceLoc =
                "examples/fixedwidth.h:5:11"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/fixedwidth.h:3:8",
            structBitfields = []}}
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
                        fieldType = TypeTypedef
                          (CName "uint64_t"),
                        fieldSourceLoc =
                        "examples/fixedwidth.h:4:11"}},
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
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc =
                        "examples/fixedwidth.h:5:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "sixty_four",
                        fieldOffset = 0,
                        fieldType = TypeTypedef
                          (CName "uint64_t"),
                        fieldSourceLoc =
                        "examples/fixedwidth.h:4:11"},
                      StructField {
                        fieldName = CName "thirty_two",
                        fieldOffset = 64,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc =
                        "examples/fixedwidth.h:5:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/fixedwidth.h:3:8",
                    structBitfields = []}})
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
                        fieldType = TypeTypedef
                          (CName "uint64_t"),
                        fieldSourceLoc =
                        "examples/fixedwidth.h:4:11"}},
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
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc =
                        "examples/fixedwidth.h:5:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "sixty_four",
                        fieldOffset = 0,
                        fieldType = TypeTypedef
                          (CName "uint64_t"),
                        fieldSourceLoc =
                        "examples/fixedwidth.h:4:11"},
                      StructField {
                        fieldName = CName "thirty_two",
                        fieldOffset = 64,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc =
                        "examples/fixedwidth.h:5:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/fixedwidth.h:3:8",
                    structBitfields = []}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))})]
