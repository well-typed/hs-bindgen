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
            multiLocExpansion =
            "typedef_vs_macro.h:4:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "M1",
          macroArgs = [],
          macroBody = MTerm
            (MType
              (TypePrim
                (PrimIntegral
                  PrimInt
                  Signed)))}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "M1"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "M1"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "M1"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "M1"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "M1"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "M1"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "M1"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "M1"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "M1"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "M1"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "M1"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "M1"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "M1"),
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
            multiLocExpansion =
            "typedef_vs_macro.h:5:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "M2",
          macroArgs = [],
          macroBody = MTerm
            (MType
              (TypePrim
                (PrimChar Nothing)))}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "M2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "M2"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "M2"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "M2"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "M2"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "M2"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "M2"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "M2"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "M2"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "M2"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "M2"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "M2"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "M2"),
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
            (PrimIntegral PrimInt Signed),
          typedefSourceLoc =
          "typedef_vs_macro.h:1:13"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "T1"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
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
          typedefSourceLoc =
          "typedef_vs_macro.h:2:14"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "T2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "T2"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "T2"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "T2"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "T2"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "T2"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "T2"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "T2"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "T2"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "T2"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "T2"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "T2"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
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
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "T1"),
              fieldSourceLoc =
              "typedef_vs_macro.h:8:6"}},
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
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "T2"),
              fieldSourceLoc =
              "typedef_vs_macro.h:9:6"}},
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
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "M1"),
              fieldSourceLoc =
              "typedef_vs_macro.h:10:6"}},
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
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "M2"),
              fieldSourceLoc =
              "typedef_vs_macro.h:11:6"}}],
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
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "T1"),
              fieldSourceLoc =
              "typedef_vs_macro.h:8:6"},
            StructField {
              fieldName = CName "t2",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "T2"),
              fieldSourceLoc =
              "typedef_vs_macro.h:9:6"},
            StructField {
              fieldName = CName "m1",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "M1"),
              fieldSourceLoc =
              "typedef_vs_macro.h:10:6"},
            StructField {
              fieldName = CName "m2",
              fieldOffset = 96,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "M2"),
              fieldSourceLoc =
              "typedef_vs_macro.h:11:6"}],
          structFlam = Nothing,
          structSourceLoc =
          "typedef_vs_macro.h:7:8"}},
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
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "T1"),
                fieldSourceLoc =
                "typedef_vs_macro.h:8:6"}},
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
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "T2"),
                fieldSourceLoc =
                "typedef_vs_macro.h:9:6"}},
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
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "M1"),
                fieldSourceLoc =
                "typedef_vs_macro.h:10:6"}},
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
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "M2"),
                fieldSourceLoc =
                "typedef_vs_macro.h:11:6"}}],
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
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "T1"),
                fieldSourceLoc =
                "typedef_vs_macro.h:8:6"},
              StructField {
                fieldName = CName "t2",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "T2"),
                fieldSourceLoc =
                "typedef_vs_macro.h:9:6"},
              StructField {
                fieldName = CName "m1",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "M1"),
                fieldSourceLoc =
                "typedef_vs_macro.h:10:6"},
              StructField {
                fieldName = CName "m2",
                fieldOffset = 96,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "M2"),
                fieldSourceLoc =
                "typedef_vs_macro.h:11:6"}],
            structFlam = Nothing,
            structSourceLoc =
            "typedef_vs_macro.h:7:8"}}
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "T1"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:8:6"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "T2"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:9:6"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "M1"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:10:6"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "M2"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:11:6"}}],
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "T1"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:8:6"},
                      StructField {
                        fieldName = CName "t2",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "T2"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:9:6"},
                      StructField {
                        fieldName = CName "m1",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "M1"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:10:6"},
                      StructField {
                        fieldName = CName "m2",
                        fieldOffset = 96,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "M2"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:11:6"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "typedef_vs_macro.h:7:8"}})
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "T1"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:8:6"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "T2"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:9:6"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "M1"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:10:6"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "M2"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:11:6"}}],
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
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "T1"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:8:6"},
                      StructField {
                        fieldName = CName "t2",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "T2"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:9:6"},
                      StructField {
                        fieldName = CName "m1",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "M1"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:10:6"},
                      StructField {
                        fieldName = CName "m2",
                        fieldOffset = 96,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "M2"),
                        fieldSourceLoc =
                        "typedef_vs_macro.h:11:6"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "typedef_vs_macro.h:7:8"}}
              (Add 4)
              (Seq
                [
                  PokeByteOff (Idx 5) 0 (Idx 0),
                  PokeByteOff (Idx 5) 4 (Idx 1),
                  PokeByteOff (Idx 5) 8 (Idx 2),
                  PokeByteOff
                    (Idx 5)
                    12
                    (Idx 3)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "ExampleStruct"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "ExampleStruct")]
