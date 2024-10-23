[
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
            "foo_i",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "i",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc =
              "examples/nested_types.h:2:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "foo_c",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "c",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/nested_types.h:3:10"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "foo"))
            DeclPathTop,
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "i",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc =
              "examples/nested_types.h:2:9"},
            StructField {
              fieldName = CName "c",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/nested_types.h:3:10"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/nested_types.h:1:8"}},
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
              "foo_i",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "i",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc =
                "examples/nested_types.h:2:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "foo_c",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "c",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/nested_types.h:3:10"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "foo"))
              DeclPathTop,
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "i",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc =
                "examples/nested_types.h:2:9"},
              StructField {
                fieldName = CName "c",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/nested_types.h:3:10"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/nested_types.h:1:8"}}
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
                  "Foo",
                structConstr = HsName
                  "@NsConstr"
                  "Foo",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "i",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc =
                        "examples/nested_types.h:2:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/nested_types.h:3:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "i",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc =
                        "examples/nested_types.h:2:9"},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/nested_types.h:3:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/nested_types.h:1:8"}})
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
                  "Foo",
                structConstr = HsName
                  "@NsConstr"
                  "Foo",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_i",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "i",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc =
                        "examples/nested_types.h:2:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "foo_c",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/nested_types.h:3:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop,
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "i",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc =
                        "examples/nested_types.h:2:9"},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/nested_types.h:3:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/nested_types.h:1:8"}}
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
        "Bar",
      structConstr = HsName
        "@NsConstr"
        "Bar",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "bar_foo1",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "Foo"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "foo1",
              fieldOffset = 0,
              fieldType = TypeStruct
                (DeclPathStruct
                  (DeclNameTag (CName "foo"))
                  DeclPathTop),
              fieldSourceLoc =
              "examples/nested_types.h:7:16"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "bar_foo2",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "Foo"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "foo2",
              fieldOffset = 64,
              fieldType = TypeStruct
                (DeclPathStruct
                  (DeclNameTag (CName "foo"))
                  DeclPathTop),
              fieldSourceLoc =
              "examples/nested_types.h:8:16"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "bar"))
            DeclPathTop,
          structSizeof = 16,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "foo1",
              fieldOffset = 0,
              fieldType = TypeStruct
                (DeclPathStruct
                  (DeclNameTag (CName "foo"))
                  DeclPathTop),
              fieldSourceLoc =
              "examples/nested_types.h:7:16"},
            StructField {
              fieldName = CName "foo2",
              fieldOffset = 64,
              fieldType = TypeStruct
                (DeclPathStruct
                  (DeclNameTag (CName "foo"))
                  DeclPathTop),
              fieldSourceLoc =
              "examples/nested_types.h:8:16"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/nested_types.h:6:8"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Bar",
        structConstr = HsName
          "@NsConstr"
          "Bar",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "bar_foo1",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "Foo"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "foo1",
                fieldOffset = 0,
                fieldType = TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "foo"))
                    DeclPathTop),
                fieldSourceLoc =
                "examples/nested_types.h:7:16"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "bar_foo2",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "Foo"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "foo2",
                fieldOffset = 64,
                fieldType = TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "foo"))
                    DeclPathTop),
                fieldSourceLoc =
                "examples/nested_types.h:8:16"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "bar"))
              DeclPathTop,
            structSizeof = 16,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "foo1",
                fieldOffset = 0,
                fieldType = TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "foo"))
                    DeclPathTop),
                fieldSourceLoc =
                "examples/nested_types.h:7:16"},
              StructField {
                fieldName = CName "foo2",
                fieldOffset = 64,
                fieldType = TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "foo"))
                    DeclPathTop),
                fieldSourceLoc =
                "examples/nested_types.h:8:16"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/nested_types.h:6:8"}}
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
                  "Bar",
                structConstr = HsName
                  "@NsConstr"
                  "Bar",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_foo1",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "Foo"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "foo1",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "examples/nested_types.h:7:16"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_foo2",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "Foo"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "foo2",
                        fieldOffset = 64,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "examples/nested_types.h:8:16"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "bar"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "foo1",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "examples/nested_types.h:7:16"},
                      StructField {
                        fieldName = CName "foo2",
                        fieldOffset = 64,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "examples/nested_types.h:8:16"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/nested_types.h:6:8"}})
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
                  "Bar",
                structConstr = HsName
                  "@NsConstr"
                  "Bar",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_foo1",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "Foo"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "foo1",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "examples/nested_types.h:7:16"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_foo2",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "Foo"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "foo2",
                        fieldOffset = 64,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "examples/nested_types.h:8:16"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "bar"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "foo1",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "examples/nested_types.h:7:16"},
                      StructField {
                        fieldName = CName "foo2",
                        fieldOffset = 64,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "examples/nested_types.h:8:16"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/nested_types.h:6:8"}}
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
        "Ex3",
      structConstr = HsName
        "@NsConstr"
        "Ex3",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "ex3_ex3_c",
          fieldType = HsPrimType
            HsPrimCFloat,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "ex3_c",
              fieldOffset = 64,
              fieldType = TypePrim
                (PrimFloating PrimFloat),
              fieldSourceLoc =
              "examples/nested_types.h:16:11"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "ex3"))
            DeclPathTop,
          structSizeof = 12,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "ex3_c",
              fieldOffset = 64,
              fieldType = TypePrim
                (PrimFloating PrimFloat),
              fieldSourceLoc =
              "examples/nested_types.h:16:11"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/nested_types.h:11:8"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Ex3",
        structConstr = HsName
          "@NsConstr"
          "Ex3",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "ex3_ex3_c",
            fieldType = HsPrimType
              HsPrimCFloat,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "ex3_c",
                fieldOffset = 64,
                fieldType = TypePrim
                  (PrimFloating PrimFloat),
                fieldSourceLoc =
                "examples/nested_types.h:16:11"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "ex3"))
              DeclPathTop,
            structSizeof = 12,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "ex3_c",
                fieldOffset = 64,
                fieldType = TypePrim
                  (PrimFloating PrimFloat),
                fieldSourceLoc =
                "examples/nested_types.h:16:11"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/nested_types.h:11:8"}}
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
                  "Ex3",
                structConstr = HsName
                  "@NsConstr"
                  "Ex3",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex3_ex3_c",
                    fieldType = HsPrimType
                      HsPrimCFloat,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ex3_c",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "examples/nested_types.h:16:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "ex3"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "ex3_c",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "examples/nested_types.h:16:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/nested_types.h:11:8"}})
            [PeekByteOff (Idx 0) 8]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Ex3",
                structConstr = HsName
                  "@NsConstr"
                  "Ex3",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex3_ex3_c",
                    fieldType = HsPrimType
                      HsPrimCFloat,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ex3_c",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "examples/nested_types.h:16:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "ex3"))
                      DeclPathTop,
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "ex3_c",
                        fieldOffset = 64,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "examples/nested_types.h:16:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/nested_types.h:11:8"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    8
                    (Idx 0)])))}),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Ex4_b",
      structConstr = HsName
        "@NsConstr"
        "Ex4_b",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "ex4_b_ex3_a",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "ex3_a",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc =
              "examples/nested_types.h:21:13"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "ex4_b_ex3_b",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "ex3_b",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/nested_types.h:22:14"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "ex4_b_recur",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Ex4_b")),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "recur",
              fieldOffset = 64,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "ex4_b"))
                    (DeclPathField
                      (CName "linkedlist")
                      (DeclPathStruct
                        (DeclNameTag (CName "ex4"))
                        DeclPathTop)))),
              fieldSourceLoc =
              "examples/nested_types.h:23:23"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "ex4_b"))
            (DeclPathField
              (CName "linkedlist")
              (DeclPathStruct
                (DeclNameTag (CName "ex4"))
                DeclPathTop)),
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "ex3_a",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral (PrimInt Signed)),
              fieldSourceLoc =
              "examples/nested_types.h:21:13"},
            StructField {
              fieldName = CName "ex3_b",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "examples/nested_types.h:22:14"},
            StructField {
              fieldName = CName "recur",
              fieldOffset = 64,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "ex4_b"))
                    (DeclPathField
                      (CName "linkedlist")
                      (DeclPathStruct
                        (DeclNameTag (CName "ex4"))
                        DeclPathTop)))),
              fieldSourceLoc =
              "examples/nested_types.h:23:23"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/nested_types.h:20:12"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Ex4_b",
        structConstr = HsName
          "@NsConstr"
          "Ex4_b",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "ex4_b_ex3_a",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "ex3_a",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc =
                "examples/nested_types.h:21:13"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "ex4_b_ex3_b",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "ex3_b",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/nested_types.h:22:14"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "ex4_b_recur",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Ex4_b")),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "recur",
                fieldOffset = 64,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag (CName "ex4_b"))
                      (DeclPathField
                        (CName "linkedlist")
                        (DeclPathStruct
                          (DeclNameTag (CName "ex4"))
                          DeclPathTop)))),
                fieldSourceLoc =
                "examples/nested_types.h:23:23"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "ex4_b"))
              (DeclPathField
                (CName "linkedlist")
                (DeclPathStruct
                  (DeclNameTag (CName "ex4"))
                  DeclPathTop)),
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "ex3_a",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral (PrimInt Signed)),
                fieldSourceLoc =
                "examples/nested_types.h:21:13"},
              StructField {
                fieldName = CName "ex3_b",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "examples/nested_types.h:22:14"},
              StructField {
                fieldName = CName "recur",
                fieldOffset = 64,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag (CName "ex4_b"))
                      (DeclPathField
                        (CName "linkedlist")
                        (DeclPathStruct
                          (DeclNameTag (CName "ex4"))
                          DeclPathTop)))),
                fieldSourceLoc =
                "examples/nested_types.h:23:23"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/nested_types.h:20:12"}}
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
                  "Ex4_b",
                structConstr = HsName
                  "@NsConstr"
                  "Ex4_b",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_b_ex3_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ex3_a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc =
                        "examples/nested_types.h:21:13"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_b_ex3_b",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ex3_b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/nested_types.h:22:14"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_b_recur",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Ex4_b")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "recur",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "ex4_b"))
                              (DeclPathField
                                (CName "linkedlist")
                                (DeclPathStruct
                                  (DeclNameTag (CName "ex4"))
                                  DeclPathTop)))),
                        fieldSourceLoc =
                        "examples/nested_types.h:23:23"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "ex4_b"))
                      (DeclPathField
                        (CName "linkedlist")
                        (DeclPathStruct
                          (DeclNameTag (CName "ex4"))
                          DeclPathTop)),
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "ex3_a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc =
                        "examples/nested_types.h:21:13"},
                      StructField {
                        fieldName = CName "ex3_b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/nested_types.h:22:14"},
                      StructField {
                        fieldName = CName "recur",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "ex4_b"))
                              (DeclPathField
                                (CName "linkedlist")
                                (DeclPathStruct
                                  (DeclNameTag (CName "ex4"))
                                  DeclPathTop)))),
                        fieldSourceLoc =
                        "examples/nested_types.h:23:23"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/nested_types.h:20:12"}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 4,
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
                  "Ex4_b",
                structConstr = HsName
                  "@NsConstr"
                  "Ex4_b",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_b_ex3_a",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ex3_a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc =
                        "examples/nested_types.h:21:13"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_b_ex3_b",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ex3_b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/nested_types.h:22:14"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_b_recur",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Ex4_b")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "recur",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "ex4_b"))
                              (DeclPathField
                                (CName "linkedlist")
                                (DeclPathStruct
                                  (DeclNameTag (CName "ex4"))
                                  DeclPathTop)))),
                        fieldSourceLoc =
                        "examples/nested_types.h:23:23"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "ex4_b"))
                      (DeclPathField
                        (CName "linkedlist")
                        (DeclPathStruct
                          (DeclNameTag (CName "ex4"))
                          DeclPathTop)),
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "ex3_a",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral (PrimInt Signed)),
                        fieldSourceLoc =
                        "examples/nested_types.h:21:13"},
                      StructField {
                        fieldName = CName "ex3_b",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "examples/nested_types.h:22:14"},
                      StructField {
                        fieldName = CName "recur",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "ex4_b"))
                              (DeclPathField
                                (CName "linkedlist")
                                (DeclPathStruct
                                  (DeclNameTag (CName "ex4"))
                                  DeclPathTop)))),
                        fieldSourceLoc =
                        "examples/nested_types.h:23:23"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/nested_types.h:20:12"}}
              (Add 3)
              (Seq
                [
                  PokeByteOff (Idx 4) 0 (Idx 0),
                  PokeByteOff (Idx 4) 4 (Idx 1),
                  PokeByteOff
                    (Idx 4)
                    8
                    (Idx 2)])))}),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Ex4",
      structConstr = HsName
        "@NsConstr"
        "Ex4",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "ex4_linkedlist",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "Ex4_b"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "linkedlist",
              fieldOffset = 0,
              fieldType = TypeStruct
                (DeclPathStruct
                  (DeclNameTag (CName "ex4_b"))
                  (DeclPathField
                    (CName "linkedlist")
                    (DeclPathStruct
                      (DeclNameTag (CName "ex4"))
                      DeclPathTop))),
              fieldSourceLoc =
              "examples/nested_types.h:24:7"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "ex4_ex3_c",
          fieldType = HsPrimType
            HsPrimCFloat,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "ex3_c",
              fieldOffset = 128,
              fieldType = TypePrim
                (PrimFloating PrimFloat),
              fieldSourceLoc =
              "examples/nested_types.h:25:11"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "ex4"))
            DeclPathTop,
          structSizeof = 24,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "linkedlist",
              fieldOffset = 0,
              fieldType = TypeStruct
                (DeclPathStruct
                  (DeclNameTag (CName "ex4_b"))
                  (DeclPathField
                    (CName "linkedlist")
                    (DeclPathStruct
                      (DeclNameTag (CName "ex4"))
                      DeclPathTop))),
              fieldSourceLoc =
              "examples/nested_types.h:24:7"},
            StructField {
              fieldName = CName "ex3_c",
              fieldOffset = 128,
              fieldType = TypePrim
                (PrimFloating PrimFloat),
              fieldSourceLoc =
              "examples/nested_types.h:25:11"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/nested_types.h:19:8"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Ex4",
        structConstr = HsName
          "@NsConstr"
          "Ex4",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "ex4_linkedlist",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "Ex4_b"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "linkedlist",
                fieldOffset = 0,
                fieldType = TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "ex4_b"))
                    (DeclPathField
                      (CName "linkedlist")
                      (DeclPathStruct
                        (DeclNameTag (CName "ex4"))
                        DeclPathTop))),
                fieldSourceLoc =
                "examples/nested_types.h:24:7"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "ex4_ex3_c",
            fieldType = HsPrimType
              HsPrimCFloat,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "ex3_c",
                fieldOffset = 128,
                fieldType = TypePrim
                  (PrimFloating PrimFloat),
                fieldSourceLoc =
                "examples/nested_types.h:25:11"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "ex4"))
              DeclPathTop,
            structSizeof = 24,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "linkedlist",
                fieldOffset = 0,
                fieldType = TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "ex4_b"))
                    (DeclPathField
                      (CName "linkedlist")
                      (DeclPathStruct
                        (DeclNameTag (CName "ex4"))
                        DeclPathTop))),
                fieldSourceLoc =
                "examples/nested_types.h:24:7"},
              StructField {
                fieldName = CName "ex3_c",
                fieldOffset = 128,
                fieldType = TypePrim
                  (PrimFloating PrimFloat),
                fieldSourceLoc =
                "examples/nested_types.h:25:11"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/nested_types.h:19:8"}}
      StorableInstance {
        storableSizeOf = 24,
        storableAlignment = 8,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Ex4",
                structConstr = HsName
                  "@NsConstr"
                  "Ex4",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_linkedlist",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Ex4_b"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "linkedlist",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTag (CName "ex4_b"))
                            (DeclPathField
                              (CName "linkedlist")
                              (DeclPathStruct
                                (DeclNameTag (CName "ex4"))
                                DeclPathTop))),
                        fieldSourceLoc =
                        "examples/nested_types.h:24:7"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_ex3_c",
                    fieldType = HsPrimType
                      HsPrimCFloat,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ex3_c",
                        fieldOffset = 128,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "examples/nested_types.h:25:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "ex4"))
                      DeclPathTop,
                    structSizeof = 24,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "linkedlist",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTag (CName "ex4_b"))
                            (DeclPathField
                              (CName "linkedlist")
                              (DeclPathStruct
                                (DeclNameTag (CName "ex4"))
                                DeclPathTop))),
                        fieldSourceLoc =
                        "examples/nested_types.h:24:7"},
                      StructField {
                        fieldName = CName "ex3_c",
                        fieldOffset = 128,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "examples/nested_types.h:25:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/nested_types.h:19:8"}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 16]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Ex4",
                structConstr = HsName
                  "@NsConstr"
                  "Ex4",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_linkedlist",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "Ex4_b"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "linkedlist",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTag (CName "ex4_b"))
                            (DeclPathField
                              (CName "linkedlist")
                              (DeclPathStruct
                                (DeclNameTag (CName "ex4"))
                                DeclPathTop))),
                        fieldSourceLoc =
                        "examples/nested_types.h:24:7"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_ex3_c",
                    fieldType = HsPrimType
                      HsPrimCFloat,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ex3_c",
                        fieldOffset = 128,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "examples/nested_types.h:25:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "ex4"))
                      DeclPathTop,
                    structSizeof = 24,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "linkedlist",
                        fieldOffset = 0,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTag (CName "ex4_b"))
                            (DeclPathField
                              (CName "linkedlist")
                              (DeclPathStruct
                                (DeclNameTag (CName "ex4"))
                                DeclPathTop))),
                        fieldSourceLoc =
                        "examples/nested_types.h:24:7"},
                      StructField {
                        fieldName = CName "ex3_c",
                        fieldOffset = 128,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "examples/nested_types.h:25:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/nested_types.h:19:8"}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    16
                    (Idx 1)])))})]
