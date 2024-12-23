[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S1_c",
      structConstr = HsName
        "@NsConstr"
        "S1_c",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s1_c_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 4,
                singleLocColumn = 9}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s1_c_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 5,
                singleLocColumn = 9}}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            DeclNameNone
            (DeclPathField
              (CName "c")
              (DeclPathStruct
                (DeclNameTag (CName "S1"))
                DeclPathTop)),
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 4,
                singleLocColumn = 9}},
            StructField {
              fieldName = CName "b",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 5,
                singleLocColumn = 9}}],
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "anonymous.h"],
            singleLocLine = 3,
            singleLocColumn = 3}}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "S1_c",
        structConstr = HsName
          "@NsConstr"
          "S1_c",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s1_c_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 4,
                  singleLocColumn = 9}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s1_c_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 5,
                  singleLocColumn = 9}}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              DeclNameNone
              (DeclPathField
                (CName "c")
                (DeclPathStruct
                  (DeclNameTag (CName "S1"))
                  DeclPathTop)),
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 4,
                  singleLocColumn = 9}},
              StructField {
                fieldName = CName "b",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 5,
                  singleLocColumn = 9}}],
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "anonymous.h"],
              singleLocLine = 3,
              singleLocColumn = 3}}}
      StorableInstance {
        storableSizeOf = 8,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "S1_c",
                structConstr = HsName
                  "@NsConstr"
                  "S1_c",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_c_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 4,
                          singleLocColumn = 9}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_c_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 5,
                          singleLocColumn = 9}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      DeclNameNone
                      (DeclPathField
                        (CName "c")
                        (DeclPathStruct
                          (DeclNameTag (CName "S1"))
                          DeclPathTop)),
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 4,
                          singleLocColumn = 9}},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 5,
                          singleLocColumn = 9}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "anonymous.h"],
                      singleLocLine = 3,
                      singleLocColumn = 3}}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 4]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "S1_c",
                structConstr = HsName
                  "@NsConstr"
                  "S1_c",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_c_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 4,
                          singleLocColumn = 9}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_c_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 5,
                          singleLocColumn = 9}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      DeclNameNone
                      (DeclPathField
                        (CName "c")
                        (DeclPathStruct
                          (DeclNameTag (CName "S1"))
                          DeclPathTop)),
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 4,
                          singleLocColumn = 9}},
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 5,
                          singleLocColumn = 9}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "anonymous.h"],
                      singleLocLine = 3,
                      singleLocColumn = 3}}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S1",
      structConstr = HsName
        "@NsConstr"
        "S1",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s1_c",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "S1_c"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "c",
              fieldOffset = 0,
              fieldType = TypeStruct
                (DeclPathStruct
                  DeclNameNone
                  (DeclPathField
                    (CName "c")
                    (DeclPathStruct
                      (DeclNameTag (CName "S1"))
                      DeclPathTop))),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 6,
                singleLocColumn = 5}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s1_d",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "d",
              fieldOffset = 64,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 8,
                singleLocColumn = 7}}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "S1"))
            DeclPathTop,
          structSizeof = 12,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "c",
              fieldOffset = 0,
              fieldType = TypeStruct
                (DeclPathStruct
                  DeclNameNone
                  (DeclPathField
                    (CName "c")
                    (DeclPathStruct
                      (DeclNameTag (CName "S1"))
                      DeclPathTop))),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 6,
                singleLocColumn = 5}},
            StructField {
              fieldName = CName "d",
              fieldOffset = 64,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 8,
                singleLocColumn = 7}}],
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "anonymous.h"],
            singleLocLine = 2,
            singleLocColumn = 8}}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "S1",
        structConstr = HsName
          "@NsConstr"
          "S1",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s1_c",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "S1_c"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "c",
                fieldOffset = 0,
                fieldType = TypeStruct
                  (DeclPathStruct
                    DeclNameNone
                    (DeclPathField
                      (CName "c")
                      (DeclPathStruct
                        (DeclNameTag (CName "S1"))
                        DeclPathTop))),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 6,
                  singleLocColumn = 5}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s1_d",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "d",
                fieldOffset = 64,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 8,
                  singleLocColumn = 7}}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "S1"))
              DeclPathTop,
            structSizeof = 12,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "c",
                fieldOffset = 0,
                fieldType = TypeStruct
                  (DeclPathStruct
                    DeclNameNone
                    (DeclPathField
                      (CName "c")
                      (DeclPathStruct
                        (DeclNameTag (CName "S1"))
                        DeclPathTop))),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 6,
                  singleLocColumn = 5}},
              StructField {
                fieldName = CName "d",
                fieldOffset = 64,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 8,
                  singleLocColumn = 7}}],
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "anonymous.h"],
              singleLocLine = 2,
              singleLocColumn = 8}}}
      StorableInstance {
        storableSizeOf = 12,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "S1",
                structConstr = HsName
                  "@NsConstr"
                  "S1",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_c",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "S1_c"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "c")
                              (DeclPathStruct
                                (DeclNameTag (CName "S1"))
                                DeclPathTop))),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 6,
                          singleLocColumn = 5}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_d",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "d",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 8,
                          singleLocColumn = 7}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "S1"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "c")
                              (DeclPathStruct
                                (DeclNameTag (CName "S1"))
                                DeclPathTop))),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 6,
                          singleLocColumn = 5}},
                      StructField {
                        fieldName = CName "d",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 8,
                          singleLocColumn = 7}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "anonymous.h"],
                      singleLocLine = 2,
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
                  "S1",
                structConstr = HsName
                  "@NsConstr"
                  "S1",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_c",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "S1_c"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "c")
                              (DeclPathStruct
                                (DeclNameTag (CName "S1"))
                                DeclPathTop))),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 6,
                          singleLocColumn = 5}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s1_d",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "d",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 8,
                          singleLocColumn = 7}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "S1"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "c")
                              (DeclPathStruct
                                (DeclNameTag (CName "S1"))
                                DeclPathTop))),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 6,
                          singleLocColumn = 5}},
                      StructField {
                        fieldName = CName "d",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 8,
                          singleLocColumn = 7}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "anonymous.h"],
                      singleLocLine = 2,
                      singleLocColumn = 8}}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))}),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S2_inner_deep",
      structConstr = HsName
        "@NsConstr"
        "S2_inner_deep",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_inner_deep_b",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "b",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 16,
                singleLocColumn = 11}}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            DeclNameNone
            (DeclPathField
              (CName "deep")
              (DeclPathStruct
                DeclNameNone
                (DeclPathField
                  (CName "inner")
                  (DeclPathStruct
                    (DeclNameTag (CName "S2"))
                    DeclPathTop)))),
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "b",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 16,
                singleLocColumn = 11}}],
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "anonymous.h"],
            singleLocLine = 15,
            singleLocColumn = 5}}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "S2_inner_deep",
        structConstr = HsName
          "@NsConstr"
          "S2_inner_deep",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s2_inner_deep_b",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "b",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 16,
                  singleLocColumn = 11}}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              DeclNameNone
              (DeclPathField
                (CName "deep")
                (DeclPathStruct
                  DeclNameNone
                  (DeclPathField
                    (CName "inner")
                    (DeclPathStruct
                      (DeclNameTag (CName "S2"))
                      DeclPathTop)))),
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "b",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 16,
                  singleLocColumn = 11}}],
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "anonymous.h"],
              singleLocLine = 15,
              singleLocColumn = 5}}}
      StorableInstance {
        storableSizeOf = 4,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "S2_inner_deep",
                structConstr = HsName
                  "@NsConstr"
                  "S2_inner_deep",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner_deep_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 16,
                          singleLocColumn = 11}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      DeclNameNone
                      (DeclPathField
                        (CName "deep")
                        (DeclPathStruct
                          DeclNameNone
                          (DeclPathField
                            (CName "inner")
                            (DeclPathStruct
                              (DeclNameTag (CName "S2"))
                              DeclPathTop)))),
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 16,
                          singleLocColumn = 11}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "anonymous.h"],
                      singleLocLine = 15,
                      singleLocColumn = 5}}})
            [PeekByteOff (Idx 0) 0]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "S2_inner_deep",
                structConstr = HsName
                  "@NsConstr"
                  "S2_inner_deep",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner_deep_b",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 16,
                          singleLocColumn = 11}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      DeclNameNone
                      (DeclPathField
                        (CName "deep")
                        (DeclPathStruct
                          DeclNameNone
                          (DeclPathField
                            (CName "inner")
                            (DeclPathStruct
                              (DeclNameTag (CName "S2"))
                              DeclPathTop)))),
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "b",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 16,
                          singleLocColumn = 11}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "anonymous.h"],
                      singleLocLine = 15,
                      singleLocColumn = 5}}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S2_inner",
      structConstr = HsName
        "@NsConstr"
        "S2_inner",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_inner_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 14,
                singleLocColumn = 9}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_inner_deep",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "S2_inner_deep"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "deep",
              fieldOffset = 32,
              fieldType = TypeStruct
                (DeclPathStruct
                  DeclNameNone
                  (DeclPathField
                    (CName "deep")
                    (DeclPathStruct
                      DeclNameNone
                      (DeclPathField
                        (CName "inner")
                        (DeclPathStruct
                          (DeclNameTag (CName "S2"))
                          DeclPathTop))))),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 17,
                singleLocColumn = 7}}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            DeclNameNone
            (DeclPathField
              (CName "inner")
              (DeclPathStruct
                (DeclNameTag (CName "S2"))
                DeclPathTop)),
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "a",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 14,
                singleLocColumn = 9}},
            StructField {
              fieldName = CName "deep",
              fieldOffset = 32,
              fieldType = TypeStruct
                (DeclPathStruct
                  DeclNameNone
                  (DeclPathField
                    (CName "deep")
                    (DeclPathStruct
                      DeclNameNone
                      (DeclPathField
                        (CName "inner")
                        (DeclPathStruct
                          (DeclNameTag (CName "S2"))
                          DeclPathTop))))),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 17,
                singleLocColumn = 7}}],
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "anonymous.h"],
            singleLocLine = 13,
            singleLocColumn = 3}}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "S2_inner",
        structConstr = HsName
          "@NsConstr"
          "S2_inner",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s2_inner_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 14,
                  singleLocColumn = 9}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s2_inner_deep",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "S2_inner_deep"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "deep",
                fieldOffset = 32,
                fieldType = TypeStruct
                  (DeclPathStruct
                    DeclNameNone
                    (DeclPathField
                      (CName "deep")
                      (DeclPathStruct
                        DeclNameNone
                        (DeclPathField
                          (CName "inner")
                          (DeclPathStruct
                            (DeclNameTag (CName "S2"))
                            DeclPathTop))))),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 17,
                  singleLocColumn = 7}}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              DeclNameNone
              (DeclPathField
                (CName "inner")
                (DeclPathStruct
                  (DeclNameTag (CName "S2"))
                  DeclPathTop)),
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "a",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 14,
                  singleLocColumn = 9}},
              StructField {
                fieldName = CName "deep",
                fieldOffset = 32,
                fieldType = TypeStruct
                  (DeclPathStruct
                    DeclNameNone
                    (DeclPathField
                      (CName "deep")
                      (DeclPathStruct
                        DeclNameNone
                        (DeclPathField
                          (CName "inner")
                          (DeclPathStruct
                            (DeclNameTag (CName "S2"))
                            DeclPathTop))))),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 17,
                  singleLocColumn = 7}}],
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "anonymous.h"],
              singleLocLine = 13,
              singleLocColumn = 3}}}
      StorableInstance {
        storableSizeOf = 8,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "S2_inner",
                structConstr = HsName
                  "@NsConstr"
                  "S2_inner",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 14,
                          singleLocColumn = 9}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner_deep",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "S2_inner_deep"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "deep",
                        fieldOffset = 32,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "deep")
                              (DeclPathStruct
                                DeclNameNone
                                (DeclPathField
                                  (CName "inner")
                                  (DeclPathStruct
                                    (DeclNameTag (CName "S2"))
                                    DeclPathTop))))),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 17,
                          singleLocColumn = 7}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      DeclNameNone
                      (DeclPathField
                        (CName "inner")
                        (DeclPathStruct
                          (DeclNameTag (CName "S2"))
                          DeclPathTop)),
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 14,
                          singleLocColumn = 9}},
                      StructField {
                        fieldName = CName "deep",
                        fieldOffset = 32,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "deep")
                              (DeclPathStruct
                                DeclNameNone
                                (DeclPathField
                                  (CName "inner")
                                  (DeclPathStruct
                                    (DeclNameTag (CName "S2"))
                                    DeclPathTop))))),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 17,
                          singleLocColumn = 7}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "anonymous.h"],
                      singleLocLine = 13,
                      singleLocColumn = 3}}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 4]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "S2_inner",
                structConstr = HsName
                  "@NsConstr"
                  "S2_inner",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 14,
                          singleLocColumn = 9}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner_deep",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "S2_inner_deep"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "deep",
                        fieldOffset = 32,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "deep")
                              (DeclPathStruct
                                DeclNameNone
                                (DeclPathField
                                  (CName "inner")
                                  (DeclPathStruct
                                    (DeclNameTag (CName "S2"))
                                    DeclPathTop))))),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 17,
                          singleLocColumn = 7}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      DeclNameNone
                      (DeclPathField
                        (CName "inner")
                        (DeclPathStruct
                          (DeclNameTag (CName "S2"))
                          DeclPathTop)),
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 14,
                          singleLocColumn = 9}},
                      StructField {
                        fieldName = CName "deep",
                        fieldOffset = 32,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "deep")
                              (DeclPathStruct
                                DeclNameNone
                                (DeclPathField
                                  (CName "inner")
                                  (DeclPathStruct
                                    (DeclNameTag (CName "S2"))
                                    DeclPathTop))))),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 17,
                          singleLocColumn = 7}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "anonymous.h"],
                      singleLocLine = 13,
                      singleLocColumn = 3}}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "S2",
      structConstr = HsName
        "@NsConstr"
        "S2",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_inner",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "S2_inner"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "inner",
              fieldOffset = 0,
              fieldType = TypeStruct
                (DeclPathStruct
                  DeclNameNone
                  (DeclPathField
                    (CName "inner")
                    (DeclPathStruct
                      (DeclNameTag (CName "S2"))
                      DeclPathTop))),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 18,
                singleLocColumn = 5}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "s2_d",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "d",
              fieldOffset = 64,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 20,
                singleLocColumn = 7}}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "S2"))
            DeclPathTop,
          structSizeof = 12,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "inner",
              fieldOffset = 0,
              fieldType = TypeStruct
                (DeclPathStruct
                  DeclNameNone
                  (DeclPathField
                    (CName "inner")
                    (DeclPathStruct
                      (DeclNameTag (CName "S2"))
                      DeclPathTop))),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 18,
                singleLocColumn = 5}},
            StructField {
              fieldName = CName "d",
              fieldOffset = 64,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "anonymous.h"],
                singleLocLine = 20,
                singleLocColumn = 7}}],
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "anonymous.h"],
            singleLocLine = 12,
            singleLocColumn = 8}}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "S2",
        structConstr = HsName
          "@NsConstr"
          "S2",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "s2_inner",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "S2_inner"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "inner",
                fieldOffset = 0,
                fieldType = TypeStruct
                  (DeclPathStruct
                    DeclNameNone
                    (DeclPathField
                      (CName "inner")
                      (DeclPathStruct
                        (DeclNameTag (CName "S2"))
                        DeclPathTop))),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 18,
                  singleLocColumn = 5}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "s2_d",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "d",
                fieldOffset = 64,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 20,
                  singleLocColumn = 7}}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "S2"))
              DeclPathTop,
            structSizeof = 12,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "inner",
                fieldOffset = 0,
                fieldType = TypeStruct
                  (DeclPathStruct
                    DeclNameNone
                    (DeclPathField
                      (CName "inner")
                      (DeclPathStruct
                        (DeclNameTag (CName "S2"))
                        DeclPathTop))),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 18,
                  singleLocColumn = 5}},
              StructField {
                fieldName = CName "d",
                fieldOffset = 64,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "anonymous.h"],
                  singleLocLine = 20,
                  singleLocColumn = 7}}],
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "anonymous.h"],
              singleLocLine = 12,
              singleLocColumn = 8}}}
      StorableInstance {
        storableSizeOf = 12,
        storableAlignment = 4,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "S2",
                structConstr = HsName
                  "@NsConstr"
                  "S2",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "S2_inner"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "inner",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "inner")
                              (DeclPathStruct
                                (DeclNameTag (CName "S2"))
                                DeclPathTop))),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 18,
                          singleLocColumn = 5}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_d",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "d",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 20,
                          singleLocColumn = 7}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "S2"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "inner",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "inner")
                              (DeclPathStruct
                                (DeclNameTag (CName "S2"))
                                DeclPathTop))),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 18,
                          singleLocColumn = 5}},
                      StructField {
                        fieldName = CName "d",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 20,
                          singleLocColumn = 7}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "anonymous.h"],
                      singleLocLine = 12,
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
                  "S2",
                structConstr = HsName
                  "@NsConstr"
                  "S2",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_inner",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "S2_inner"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "inner",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "inner")
                              (DeclPathStruct
                                (DeclNameTag (CName "S2"))
                                DeclPathTop))),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 18,
                          singleLocColumn = 5}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "s2_d",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "d",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 20,
                          singleLocColumn = 7}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "S2"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "inner",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "inner")
                              (DeclPathStruct
                                (DeclNameTag (CName "S2"))
                                DeclPathTop))),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 18,
                          singleLocColumn = 5}},
                      StructField {
                        fieldName = CName "d",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "anonymous.h"],
                          singleLocLine = 20,
                          singleLocColumn = 7}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "anonymous.h"],
                      singleLocLine = 12,
                      singleLocColumn = 8}}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))})]
