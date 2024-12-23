[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "M1",
      newtypeConstr = HsName
        "@NsConstr"
        "M1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unM1",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion = SingleLoc {
              singleLocPath = [
                "examples",
                "typedef_vs_macro.h"],
              singleLocLine = 4,
              singleLocColumn = 9},
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "M1",
          macroArgs = [],
          macroBody = MTerm
            (MType
              (PrimIntegral
                (PrimInt Signed)))}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "M2",
      newtypeConstr = HsName
        "@NsConstr"
        "M2",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unM2",
        fieldType = HsPrimType
          HsPrimCChar,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion = SingleLoc {
              singleLocPath = [
                "examples",
                "typedef_vs_macro.h"],
              singleLocLine = 5,
              singleLocColumn = 9},
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "M2",
          macroArgs = [],
          macroBody = MTerm
            (MType (PrimChar Nothing))}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "T1",
      newtypeConstr = HsName
        "@NsConstr"
        "T1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unT1",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "T1",
          typedefType = TypePrim
            (PrimIntegral (PrimInt Signed)),
          typedefSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "typedef_vs_macro.h"],
            singleLocLine = 1,
            singleLocColumn = 13}}},
  DeclNewtypeInstance
    Storable
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "T2",
      newtypeConstr = HsName
        "@NsConstr"
        "T2",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "unT2",
        fieldType = HsPrimType
          HsPrimCChar,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "T2",
          typedefType = TypePrim
            (PrimChar Nothing),
          typedefSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "typedef_vs_macro.h"],
            singleLocLine = 2,
            singleLocColumn = 14}}},
  DeclNewtypeInstance
    Storable
    (HsName "@NsTypeConstr" "T2"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "ExampleStruct",
      structConstr = HsName
        "@NsConstr"
        "ExampleStruct",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "exampleStruct_t1",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "T1"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "t1",
              fieldOffset = 0,
              fieldType = TypeTypedef
                (CName "T1"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "typedef_vs_macro.h"],
                singleLocLine = 8,
                singleLocColumn = 6}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "exampleStruct_t2",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "T2"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "t2",
              fieldOffset = 32,
              fieldType = TypeTypedef
                (CName "T2"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "typedef_vs_macro.h"],
                singleLocLine = 9,
                singleLocColumn = 6}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "exampleStruct_m1",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "M1"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "m1",
              fieldOffset = 64,
              fieldType = TypeTypedef
                (CName "M1"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "typedef_vs_macro.h"],
                singleLocLine = 10,
                singleLocColumn = 6}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "exampleStruct_m2",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "M2"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "m2",
              fieldOffset = 96,
              fieldType = TypeTypedef
                (CName "M2"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "typedef_vs_macro.h"],
                singleLocLine = 11,
                singleLocColumn = 6}}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag
              (CName "ExampleStruct"))
            DeclPathTop,
          structSizeof = 16,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "t1",
              fieldOffset = 0,
              fieldType = TypeTypedef
                (CName "T1"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "typedef_vs_macro.h"],
                singleLocLine = 8,
                singleLocColumn = 6}},
            StructField {
              fieldName = CName "t2",
              fieldOffset = 32,
              fieldType = TypeTypedef
                (CName "T2"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "typedef_vs_macro.h"],
                singleLocLine = 9,
                singleLocColumn = 6}},
            StructField {
              fieldName = CName "m1",
              fieldOffset = 64,
              fieldType = TypeTypedef
                (CName "M1"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "typedef_vs_macro.h"],
                singleLocLine = 10,
                singleLocColumn = 6}},
            StructField {
              fieldName = CName "m2",
              fieldOffset = 96,
              fieldType = TypeTypedef
                (CName "M2"),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "typedef_vs_macro.h"],
                singleLocLine = 11,
                singleLocColumn = 6}}],
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "typedef_vs_macro.h"],
            singleLocLine = 7,
            singleLocColumn = 8}}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "ExampleStruct",
        structConstr = HsName
          "@NsConstr"
          "ExampleStruct",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "exampleStruct_t1",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "T1"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "t1",
                fieldOffset = 0,
                fieldType = TypeTypedef
                  (CName "T1"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "typedef_vs_macro.h"],
                  singleLocLine = 8,
                  singleLocColumn = 6}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "exampleStruct_t2",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "T2"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "t2",
                fieldOffset = 32,
                fieldType = TypeTypedef
                  (CName "T2"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "typedef_vs_macro.h"],
                  singleLocLine = 9,
                  singleLocColumn = 6}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "exampleStruct_m1",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "M1"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "m1",
                fieldOffset = 64,
                fieldType = TypeTypedef
                  (CName "M1"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "typedef_vs_macro.h"],
                  singleLocLine = 10,
                  singleLocColumn = 6}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "exampleStruct_m2",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "M2"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "m2",
                fieldOffset = 96,
                fieldType = TypeTypedef
                  (CName "M2"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "typedef_vs_macro.h"],
                  singleLocLine = 11,
                  singleLocColumn = 6}}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag
                (CName "ExampleStruct"))
              DeclPathTop,
            structSizeof = 16,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "t1",
                fieldOffset = 0,
                fieldType = TypeTypedef
                  (CName "T1"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "typedef_vs_macro.h"],
                  singleLocLine = 8,
                  singleLocColumn = 6}},
              StructField {
                fieldName = CName "t2",
                fieldOffset = 32,
                fieldType = TypeTypedef
                  (CName "T2"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "typedef_vs_macro.h"],
                  singleLocLine = 9,
                  singleLocColumn = 6}},
              StructField {
                fieldName = CName "m1",
                fieldOffset = 64,
                fieldType = TypeTypedef
                  (CName "M1"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "typedef_vs_macro.h"],
                  singleLocLine = 10,
                  singleLocColumn = 6}},
              StructField {
                fieldName = CName "m2",
                fieldOffset = 96,
                fieldType = TypeTypedef
                  (CName "M2"),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "typedef_vs_macro.h"],
                  singleLocLine = 11,
                  singleLocColumn = 6}}],
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "typedef_vs_macro.h"],
              singleLocLine = 7,
              singleLocColumn = 8}}}
      StorableInstance {
        storableSizeOf = 16,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "ExampleStruct",
                structConstr = HsName
                  "@NsConstr"
                  "ExampleStruct",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exampleStruct_t1",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "T1"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "t1",
                        fieldOffset = 0,
                        fieldType = TypeTypedef
                          (CName "T1"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 8,
                          singleLocColumn = 6}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exampleStruct_t2",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "T2"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "t2",
                        fieldOffset = 32,
                        fieldType = TypeTypedef
                          (CName "T2"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 9,
                          singleLocColumn = 6}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exampleStruct_m1",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "M1"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "m1",
                        fieldOffset = 64,
                        fieldType = TypeTypedef
                          (CName "M1"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 10,
                          singleLocColumn = 6}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exampleStruct_m2",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "M2"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "m2",
                        fieldOffset = 96,
                        fieldType = TypeTypedef
                          (CName "M2"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 11,
                          singleLocColumn = 6}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "ExampleStruct"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "t1",
                        fieldOffset = 0,
                        fieldType = TypeTypedef
                          (CName "T1"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 8,
                          singleLocColumn = 6}},
                      StructField {
                        fieldName = CName "t2",
                        fieldOffset = 32,
                        fieldType = TypeTypedef
                          (CName "T2"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 9,
                          singleLocColumn = 6}},
                      StructField {
                        fieldName = CName "m1",
                        fieldOffset = 64,
                        fieldType = TypeTypedef
                          (CName "M1"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 10,
                          singleLocColumn = 6}},
                      StructField {
                        fieldName = CName "m2",
                        fieldOffset = 96,
                        fieldType = TypeTypedef
                          (CName "M2"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 11,
                          singleLocColumn = 6}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "typedef_vs_macro.h"],
                      singleLocLine = 7,
                      singleLocColumn = 8}}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 4,
              PeekByteOff (Idx 0) 8,
              PeekByteOff (Idx 0) 12]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "ExampleStruct",
                structConstr = HsName
                  "@NsConstr"
                  "ExampleStruct",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exampleStruct_t1",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "T1"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "t1",
                        fieldOffset = 0,
                        fieldType = TypeTypedef
                          (CName "T1"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 8,
                          singleLocColumn = 6}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exampleStruct_t2",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "T2"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "t2",
                        fieldOffset = 32,
                        fieldType = TypeTypedef
                          (CName "T2"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 9,
                          singleLocColumn = 6}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exampleStruct_m1",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "M1"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "m1",
                        fieldOffset = 64,
                        fieldType = TypeTypedef
                          (CName "M1"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 10,
                          singleLocColumn = 6}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exampleStruct_m2",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "M2"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "m2",
                        fieldOffset = 96,
                        fieldType = TypeTypedef
                          (CName "M2"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 11,
                          singleLocColumn = 6}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag
                        (CName "ExampleStruct"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "t1",
                        fieldOffset = 0,
                        fieldType = TypeTypedef
                          (CName "T1"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 8,
                          singleLocColumn = 6}},
                      StructField {
                        fieldName = CName "t2",
                        fieldOffset = 32,
                        fieldType = TypeTypedef
                          (CName "T2"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 9,
                          singleLocColumn = 6}},
                      StructField {
                        fieldName = CName "m1",
                        fieldOffset = 64,
                        fieldType = TypeTypedef
                          (CName "M1"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 10,
                          singleLocColumn = 6}},
                      StructField {
                        fieldName = CName "m2",
                        fieldOffset = 96,
                        fieldType = TypeTypedef
                          (CName "M2"),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "typedef_vs_macro.h"],
                          singleLocLine = 11,
                          singleLocColumn = 6}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "typedef_vs_macro.h"],
                      singleLocLine = 7,
                      singleLocColumn = 8}}}
              (Add 4)
              (Seq
                [
                  PokeByteOff (Idx 5) 0 (Idx 0),
                  PokeByteOff (Idx 5) 4 (Idx 1),
                  PokeByteOff (Idx 5) 8 (Idx 2),
                  PokeByteOff
                    (Idx 5)
                    12
                    (Idx 3)])))})]
