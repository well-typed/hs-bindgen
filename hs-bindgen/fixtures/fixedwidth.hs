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
              (PrimLong Unsigned)),
          typedefSourceLoc = SingleLoc {
            singleLocPath = [
              "musl-include",
              "bits",
              "alltypes.h"],
            singleLocLine = 136,
            singleLocColumn = 25}}},
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
            (PrimIntegral
              (PrimInt Unsigned)),
          typedefSourceLoc = SingleLoc {
            singleLocPath = [
              "musl-include",
              "bits",
              "alltypes.h"],
            singleLocLine = 131,
            singleLocColumn = 25}}},
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
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "fixedwidth.h"],
                singleLocLine = 4,
                singleLocColumn = 11}}},
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
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "fixedwidth.h"],
                singleLocLine = 5,
                singleLocColumn = 11}}}],
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
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "fixedwidth.h"],
                singleLocLine = 4,
                singleLocColumn = 11}},
            StructField {
              fieldName = CName "thirty_two",
              fieldOffset = 64,
              fieldType = TypeTypedef
                (CName "uint32_t"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "fixedwidth.h"],
                singleLocLine = 5,
                singleLocColumn = 11}}],
          structFlam = Nothing,
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "fixedwidth.h"],
            singleLocLine = 3,
            singleLocColumn = 8}}},
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
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "fixedwidth.h"],
                  singleLocLine = 4,
                  singleLocColumn = 11}}},
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
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "fixedwidth.h"],
                  singleLocLine = 5,
                  singleLocColumn = 11}}}],
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
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "fixedwidth.h"],
                  singleLocLine = 4,
                  singleLocColumn = 11}},
              StructField {
                fieldName = CName "thirty_two",
                fieldOffset = 64,
                fieldType = TypeTypedef
                  (CName "uint32_t"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "fixedwidth.h"],
                  singleLocLine = 5,
                  singleLocColumn = 11}}],
            structFlam = Nothing,
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "fixedwidth.h"],
              singleLocLine = 3,
              singleLocColumn = 8}}}
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "fixedwidth.h"],
                          singleLocLine = 4,
                          singleLocColumn = 11}}},
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "fixedwidth.h"],
                          singleLocLine = 5,
                          singleLocColumn = 11}}}],
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "fixedwidth.h"],
                          singleLocLine = 4,
                          singleLocColumn = 11}},
                      StructField {
                        fieldName = CName "thirty_two",
                        fieldOffset = 64,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "fixedwidth.h"],
                          singleLocLine = 5,
                          singleLocColumn = 11}}],
                    structFlam = Nothing,
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "fixedwidth.h"],
                      singleLocLine = 3,
                      singleLocColumn = 8}}})
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "fixedwidth.h"],
                          singleLocLine = 4,
                          singleLocColumn = 11}}},
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "fixedwidth.h"],
                          singleLocLine = 5,
                          singleLocColumn = 11}}}],
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "fixedwidth.h"],
                          singleLocLine = 4,
                          singleLocColumn = 11}},
                      StructField {
                        fieldName = CName "thirty_two",
                        fieldOffset = 64,
                        fieldType = TypeTypedef
                          (CName "uint32_t"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "fixedwidth.h"],
                          singleLocLine = 5,
                          singleLocColumn = 11}}],
                    structFlam = Nothing,
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "fixedwidth.h"],
                      singleLocLine = 3,
                      singleLocColumn = 8}}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))})]
