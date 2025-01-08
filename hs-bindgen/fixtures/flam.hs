[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Pascal",
      structConstr = HsName
        "@NsConstr"
        "Pascal",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "pascal_len",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "len",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "flam.h"],
                singleLocLine = 3,
                singleLocColumn = 9}}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "pascal"))
            DeclPathTop,
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "len",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "flam.h"],
                singleLocLine = 3,
                singleLocColumn = 9}}],
          structFlam = Just
            StructField {
              fieldName = CName "data",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "flam.h"],
                singleLocLine = 4,
                singleLocColumn = 10}},
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "flam.h"],
            singleLocLine = 2,
            singleLocColumn = 8}}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Pascal",
        structConstr = HsName
          "@NsConstr"
          "Pascal",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "pascal_len",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "len",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "flam.h"],
                  singleLocLine = 3,
                  singleLocColumn = 9}}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "pascal"))
              DeclPathTop,
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "len",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "flam.h"],
                  singleLocLine = 3,
                  singleLocColumn = 9}}],
            structFlam = Just
              StructField {
                fieldName = CName "data",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "flam.h"],
                  singleLocLine = 4,
                  singleLocColumn = 10}},
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "flam.h"],
              singleLocLine = 2,
              singleLocColumn = 8}}}
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
                  "Pascal",
                structConstr = HsName
                  "@NsConstr"
                  "Pascal",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "pascal_len",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "len",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 3,
                          singleLocColumn = 9}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "pascal"))
                      DeclPathTop,
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "len",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 3,
                          singleLocColumn = 9}}],
                    structFlam = Just
                      StructField {
                        fieldName = CName "data",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 4,
                          singleLocColumn = 10}},
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "flam.h"],
                      singleLocLine = 2,
                      singleLocColumn = 8}}})
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
                  "Pascal",
                structConstr = HsName
                  "@NsConstr"
                  "Pascal",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "pascal_len",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "len",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 3,
                          singleLocColumn = 9}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "pascal"))
                      DeclPathTop,
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "len",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 3,
                          singleLocColumn = 9}}],
                    structFlam = Just
                      StructField {
                        fieldName = CName "data",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 4,
                          singleLocColumn = 10}},
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "flam.h"],
                      singleLocLine = 2,
                      singleLocColumn = 8}}}
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
        "Foo_bar",
      structConstr = HsName
        "@NsConstr"
        "Foo_bar",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_bar_x",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "flam.h"],
                singleLocLine = 11,
                singleLocColumn = 7}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_bar_y",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "flam.h"],
                singleLocLine = 12,
                singleLocColumn = 7}}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            DeclNameNone
            (DeclPathField
              (CName "bar")
              (DeclPathStruct
                (DeclNameTag (CName "foo"))
                DeclPathTop)),
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "flam.h"],
                singleLocLine = 11,
                singleLocColumn = 7}},
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "flam.h"],
                singleLocLine = 12,
                singleLocColumn = 7}}],
          structFlam = Nothing,
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "flam.h"],
            singleLocLine = 10,
            singleLocColumn = 2}}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Foo_bar",
        structConstr = HsName
          "@NsConstr"
          "Foo_bar",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "foo_bar_x",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "flam.h"],
                  singleLocLine = 11,
                  singleLocColumn = 7}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "foo_bar_y",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "flam.h"],
                  singleLocLine = 12,
                  singleLocColumn = 7}}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              DeclNameNone
              (DeclPathField
                (CName "bar")
                (DeclPathStruct
                  (DeclNameTag (CName "foo"))
                  DeclPathTop)),
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "flam.h"],
                  singleLocLine = 11,
                  singleLocColumn = 7}},
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "flam.h"],
                  singleLocLine = 12,
                  singleLocColumn = 7}}],
            structFlam = Nothing,
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "flam.h"],
              singleLocLine = 10,
              singleLocColumn = 2}}}
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
                  "Foo_bar",
                structConstr = HsName
                  "@NsConstr"
                  "Foo_bar",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_bar_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 11,
                          singleLocColumn = 7}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_bar_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 12,
                          singleLocColumn = 7}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      DeclNameNone
                      (DeclPathField
                        (CName "bar")
                        (DeclPathStruct
                          (DeclNameTag (CName "foo"))
                          DeclPathTop)),
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 11,
                          singleLocColumn = 7}},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 12,
                          singleLocColumn = 7}}],
                    structFlam = Nothing,
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "flam.h"],
                      singleLocLine = 10,
                      singleLocColumn = 2}}})
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
                  "Foo_bar",
                structConstr = HsName
                  "@NsConstr"
                  "Foo_bar",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_bar_x",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 11,
                          singleLocColumn = 7}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_bar_y",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 12,
                          singleLocColumn = 7}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      DeclNameNone
                      (DeclPathField
                        (CName "bar")
                        (DeclPathStruct
                          (DeclNameTag (CName "foo"))
                          DeclPathTop)),
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 11,
                          singleLocColumn = 7}},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 12,
                          singleLocColumn = 7}}],
                    structFlam = Nothing,
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "flam.h"],
                      singleLocLine = 10,
                      singleLocColumn = 2}}}
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
        "Foo",
      structConstr = HsName
        "@NsConstr"
        "Foo",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_len",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "len",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "flam.h"],
                singleLocLine = 9,
                singleLocColumn = 6}}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "foo"))
            DeclPathTop,
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "len",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "flam.h"],
                singleLocLine = 9,
                singleLocColumn = 6}}],
          structFlam = Just
            StructField {
              fieldName = CName "bar",
              fieldOffset = 32,
              fieldType = TypeStruct
                (DeclPathStruct
                  DeclNameNone
                  (DeclPathField
                    (CName "bar")
                    (DeclPathStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop))),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "flam.h"],
                singleLocLine = 13,
                singleLocColumn = 4}},
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "flam.h"],
            singleLocLine = 8,
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
              "foo_len",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "len",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "flam.h"],
                  singleLocLine = 9,
                  singleLocColumn = 6}}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "foo"))
              DeclPathTop,
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "len",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "flam.h"],
                  singleLocLine = 9,
                  singleLocColumn = 6}}],
            structFlam = Just
              StructField {
                fieldName = CName "bar",
                fieldOffset = 32,
                fieldType = TypeStruct
                  (DeclPathStruct
                    DeclNameNone
                    (DeclPathField
                      (CName "bar")
                      (DeclPathStruct
                        (DeclNameTag (CName "foo"))
                        DeclPathTop))),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "flam.h"],
                  singleLocLine = 13,
                  singleLocColumn = 4}},
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "flam.h"],
              singleLocLine = 8,
              singleLocColumn = 8}}}
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
                  "Foo",
                structConstr = HsName
                  "@NsConstr"
                  "Foo",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_len",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "len",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 9,
                          singleLocColumn = 6}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop,
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "len",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 9,
                          singleLocColumn = 6}}],
                    structFlam = Just
                      StructField {
                        fieldName = CName "bar",
                        fieldOffset = 32,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "bar")
                              (DeclPathStruct
                                (DeclNameTag (CName "foo"))
                                DeclPathTop))),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 13,
                          singleLocColumn = 4}},
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "flam.h"],
                      singleLocLine = 8,
                      singleLocColumn = 8}}})
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
                  "Foo",
                structConstr = HsName
                  "@NsConstr"
                  "Foo",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_len",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "len",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 9,
                          singleLocColumn = 6}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop,
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "len",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 9,
                          singleLocColumn = 6}}],
                    structFlam = Just
                      StructField {
                        fieldName = CName "bar",
                        fieldOffset = 32,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "bar")
                              (DeclPathStruct
                                (DeclNameTag (CName "foo"))
                                DeclPathTop))),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "flam.h"],
                          singleLocLine = 13,
                          singleLocColumn = 4}},
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "flam.h"],
                      singleLocLine = 8,
                      singleLocColumn = 8}}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))})]
