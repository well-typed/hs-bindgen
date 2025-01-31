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
                (PrimIntegral PrimInt Signed),
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
                (PrimIntegral PrimInt Signed),
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
          "examples/nested_types.h:1:8",
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
              "foo_i",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "i",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
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
                  (PrimIntegral PrimInt Signed),
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
            "examples/nested_types.h:1:8",
            structBitfields = []}}
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
                          (PrimIntegral PrimInt Signed),
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
                          (PrimIntegral PrimInt Signed),
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
                    "examples/nested_types.h:1:8",
                    structBitfields = []}})
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
                          (PrimIntegral PrimInt Signed),
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
                          (PrimIntegral PrimInt Signed),
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
                    "examples/nested_types.h:1:8",
                    structBitfields = []}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    4
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Foo"),
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
          "examples/nested_types.h:6:8",
          structBitfields = []}},
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
            "examples/nested_types.h:6:8",
            structBitfields = []}}
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
                    "examples/nested_types.h:6:8",
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
                    "examples/nested_types.h:6:8",
                    structBitfields = []}}
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
    (HsName "@NsTypeConstr" "Bar"),
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
          "examples/nested_types.h:11:8",
          structBitfields = []}},
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
            "examples/nested_types.h:11:8",
            structBitfields = []}}
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
                    "examples/nested_types.h:11:8",
                    structBitfields = []}})
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
                    "examples/nested_types.h:11:8",
                    structBitfields = []}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    8
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Ex3"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Ex4_even",
      structConstr = HsName
        "@NsConstr"
        "Ex4_even",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "ex4_even_ex4_even_value",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName
                "ex4_even_value",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimFloating PrimDouble),
              fieldSourceLoc =
              "examples/nested_types.h:25:16"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "ex4_even_next",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Ex4_odd")),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "next",
              fieldOffset = 64,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "ex4_odd"))
                    DeclPathTop)),
              fieldSourceLoc =
              "examples/nested_types.h:26:25"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "ex4_even"))
            (DeclPathField
              (CName "next")
              (DeclPathStruct
                (DeclNameTag (CName "ex4_odd"))
                DeclPathTop)),
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName
                "ex4_even_value",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimFloating PrimDouble),
              fieldSourceLoc =
              "examples/nested_types.h:25:16"},
            StructField {
              fieldName = CName "next",
              fieldOffset = 64,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "ex4_odd"))
                    DeclPathTop)),
              fieldSourceLoc =
              "examples/nested_types.h:26:25"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/nested_types.h:24:12",
          structBitfields = []}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Ex4_even",
        structConstr = HsName
          "@NsConstr"
          "Ex4_even",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "ex4_even_ex4_even_value",
            fieldType = HsPrimType
              HsPrimCDouble,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName
                  "ex4_even_value",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimFloating PrimDouble),
                fieldSourceLoc =
                "examples/nested_types.h:25:16"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "ex4_even_next",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Ex4_odd")),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "next",
                fieldOffset = 64,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag (CName "ex4_odd"))
                      DeclPathTop)),
                fieldSourceLoc =
                "examples/nested_types.h:26:25"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "ex4_even"))
              (DeclPathField
                (CName "next")
                (DeclPathStruct
                  (DeclNameTag (CName "ex4_odd"))
                  DeclPathTop)),
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName
                  "ex4_even_value",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimFloating PrimDouble),
                fieldSourceLoc =
                "examples/nested_types.h:25:16"},
              StructField {
                fieldName = CName "next",
                fieldOffset = 64,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag (CName "ex4_odd"))
                      DeclPathTop)),
                fieldSourceLoc =
                "examples/nested_types.h:26:25"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/nested_types.h:24:12",
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
                  "Ex4_even",
                structConstr = HsName
                  "@NsConstr"
                  "Ex4_even",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_even_ex4_even_value",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName
                          "ex4_even_value",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "examples/nested_types.h:25:16"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_even_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Ex4_odd")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "ex4_odd"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "examples/nested_types.h:26:25"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "ex4_even"))
                      (DeclPathField
                        (CName "next")
                        (DeclPathStruct
                          (DeclNameTag (CName "ex4_odd"))
                          DeclPathTop)),
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName
                          "ex4_even_value",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "examples/nested_types.h:25:16"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "ex4_odd"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "examples/nested_types.h:26:25"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/nested_types.h:24:12",
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
                  "Ex4_even",
                structConstr = HsName
                  "@NsConstr"
                  "Ex4_even",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_even_ex4_even_value",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName
                          "ex4_even_value",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "examples/nested_types.h:25:16"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_even_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Ex4_odd")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "ex4_odd"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "examples/nested_types.h:26:25"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "ex4_even"))
                      (DeclPathField
                        (CName "next")
                        (DeclPathStruct
                          (DeclNameTag (CName "ex4_odd"))
                          DeclPathTop)),
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName
                          "ex4_even_value",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "examples/nested_types.h:25:16"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "ex4_odd"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "examples/nested_types.h:26:25"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/nested_types.h:24:12",
                    structBitfields = []}}
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
    (HsName
      "@NsTypeConstr"
      "Ex4_even"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Ex4_odd",
      structConstr = HsName
        "@NsConstr"
        "Ex4_odd",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "ex4_odd_ex4_odd_value",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName
                "ex4_odd_value",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/nested_types.h:23:9"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "ex4_odd_next",
          fieldType = HsPtr
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Ex4_even")),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "next",
              fieldOffset = 64,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "ex4_even"))
                    (DeclPathField
                      (CName "next")
                      (DeclPathStruct
                        (DeclNameTag (CName "ex4_odd"))
                        DeclPathTop)))),
              fieldSourceLoc =
              "examples/nested_types.h:27:8"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "ex4_odd"))
            DeclPathTop,
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName
                "ex4_odd_value",
              fieldOffset = 0,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "examples/nested_types.h:23:9"},
            StructField {
              fieldName = CName "next",
              fieldOffset = 64,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "ex4_even"))
                    (DeclPathField
                      (CName "next")
                      (DeclPathStruct
                        (DeclNameTag (CName "ex4_odd"))
                        DeclPathTop)))),
              fieldSourceLoc =
              "examples/nested_types.h:27:8"}],
          structFlam = Nothing,
          structSourceLoc =
          "examples/nested_types.h:22:8",
          structBitfields = []}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Ex4_odd",
        structConstr = HsName
          "@NsConstr"
          "Ex4_odd",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "ex4_odd_ex4_odd_value",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName
                  "ex4_odd_value",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/nested_types.h:23:9"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "ex4_odd_next",
            fieldType = HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "Ex4_even")),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "next",
                fieldOffset = 64,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag (CName "ex4_even"))
                      (DeclPathField
                        (CName "next")
                        (DeclPathStruct
                          (DeclNameTag (CName "ex4_odd"))
                          DeclPathTop)))),
                fieldSourceLoc =
                "examples/nested_types.h:27:8"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "ex4_odd"))
              DeclPathTop,
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName
                  "ex4_odd_value",
                fieldOffset = 0,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "examples/nested_types.h:23:9"},
              StructField {
                fieldName = CName "next",
                fieldOffset = 64,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag (CName "ex4_even"))
                      (DeclPathField
                        (CName "next")
                        (DeclPathStruct
                          (DeclNameTag (CName "ex4_odd"))
                          DeclPathTop)))),
                fieldSourceLoc =
                "examples/nested_types.h:27:8"}],
            structFlam = Nothing,
            structSourceLoc =
            "examples/nested_types.h:22:8",
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
                  "Ex4_odd",
                structConstr = HsName
                  "@NsConstr"
                  "Ex4_odd",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_odd_ex4_odd_value",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName
                          "ex4_odd_value",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/nested_types.h:23:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_odd_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Ex4_even")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "ex4_even"))
                              (DeclPathField
                                (CName "next")
                                (DeclPathStruct
                                  (DeclNameTag (CName "ex4_odd"))
                                  DeclPathTop)))),
                        fieldSourceLoc =
                        "examples/nested_types.h:27:8"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "ex4_odd"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName
                          "ex4_odd_value",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/nested_types.h:23:9"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "ex4_even"))
                              (DeclPathField
                                (CName "next")
                                (DeclPathStruct
                                  (DeclNameTag (CName "ex4_odd"))
                                  DeclPathTop)))),
                        fieldSourceLoc =
                        "examples/nested_types.h:27:8"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/nested_types.h:22:8",
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
                  "Ex4_odd",
                structConstr = HsName
                  "@NsConstr"
                  "Ex4_odd",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_odd_ex4_odd_value",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName
                          "ex4_odd_value",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/nested_types.h:23:9"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "ex4_odd_next",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName
                          "@NsTypeConstr"
                          "Ex4_even")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "ex4_even"))
                              (DeclPathField
                                (CName "next")
                                (DeclPathStruct
                                  (DeclNameTag (CName "ex4_odd"))
                                  DeclPathTop)))),
                        fieldSourceLoc =
                        "examples/nested_types.h:27:8"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "ex4_odd"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName
                          "ex4_odd_value",
                        fieldOffset = 0,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "examples/nested_types.h:23:9"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "ex4_even"))
                              (DeclPathField
                                (CName "next")
                                (DeclPathStruct
                                  (DeclNameTag (CName "ex4_odd"))
                                  DeclPathTop)))),
                        fieldSourceLoc =
                        "examples/nested_types.h:27:8"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "examples/nested_types.h:22:8",
                    structBitfields = []}}
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
    (HsName
      "@NsTypeConstr"
      "Ex4_odd")]
