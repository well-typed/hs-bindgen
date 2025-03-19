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
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "nested_types.h:2:9"}},
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
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "nested_types.h:3:10"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            (DeclNameTag (CName "foo"))
            DeclPathTop,
          structAliases = [],
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "i",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "nested_types.h:2:9"},
            StructField {
              fieldName = CName "c",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "nested_types.h:3:10"}],
          structFlam = Nothing,
          structSourceLoc =
          "nested_types.h:1:8"}},
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
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "nested_types.h:2:9"}},
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
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "nested_types.h:3:10"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              (DeclNameTag (CName "foo"))
              DeclPathTop,
            structAliases = [],
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "i",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "nested_types.h:2:9"},
              StructField {
                fieldName = CName "c",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "nested_types.h:3:10"}],
            structFlam = Nothing,
            structSourceLoc =
            "nested_types.h:1:8"}}
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "nested_types.h:2:9"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "nested_types.h:3:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      (DeclNameTag (CName "foo"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "i",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "nested_types.h:2:9"},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "nested_types.h:3:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_types.h:1:8"}})
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "nested_types.h:2:9"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "nested_types.h:3:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      (DeclNameTag (CName "foo"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "i",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "nested_types.h:2:9"},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "nested_types.h:3:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_types.h:1:8"}}
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
  DeclNewtypeInstance
    DeriveStock
    Eq
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
              fieldWidth = Nothing,
              fieldType = TypeStruct
                (DeclPathConstr
                  (DeclNameTag (CName "foo"))
                  DeclPathTop),
              fieldSourceLoc =
              "nested_types.h:7:16"}},
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
              fieldWidth = Nothing,
              fieldType = TypeStruct
                (DeclPathConstr
                  (DeclNameTag (CName "foo"))
                  DeclPathTop),
              fieldSourceLoc =
              "nested_types.h:8:16"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            (DeclNameTag (CName "bar"))
            DeclPathTop,
          structAliases = [],
          structSizeof = 16,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "foo1",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeStruct
                (DeclPathConstr
                  (DeclNameTag (CName "foo"))
                  DeclPathTop),
              fieldSourceLoc =
              "nested_types.h:7:16"},
            StructField {
              fieldName = CName "foo2",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypeStruct
                (DeclPathConstr
                  (DeclNameTag (CName "foo"))
                  DeclPathTop),
              fieldSourceLoc =
              "nested_types.h:8:16"}],
          structFlam = Nothing,
          structSourceLoc =
          "nested_types.h:6:8"}},
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
                fieldWidth = Nothing,
                fieldType = TypeStruct
                  (DeclPathConstr
                    (DeclNameTag (CName "foo"))
                    DeclPathTop),
                fieldSourceLoc =
                "nested_types.h:7:16"}},
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
                fieldWidth = Nothing,
                fieldType = TypeStruct
                  (DeclPathConstr
                    (DeclNameTag (CName "foo"))
                    DeclPathTop),
                fieldSourceLoc =
                "nested_types.h:8:16"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              (DeclNameTag (CName "bar"))
              DeclPathTop,
            structAliases = [],
            structSizeof = 16,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "foo1",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeStruct
                  (DeclPathConstr
                    (DeclNameTag (CName "foo"))
                    DeclPathTop),
                fieldSourceLoc =
                "nested_types.h:7:16"},
              StructField {
                fieldName = CName "foo2",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypeStruct
                  (DeclPathConstr
                    (DeclNameTag (CName "foo"))
                    DeclPathTop),
                fieldSourceLoc =
                "nested_types.h:8:16"}],
            structFlam = Nothing,
            structSourceLoc =
            "nested_types.h:6:8"}}
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
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathConstr
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "nested_types.h:7:16"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathConstr
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "nested_types.h:8:16"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      (DeclNameTag (CName "bar"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "foo1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathConstr
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "nested_types.h:7:16"},
                      StructField {
                        fieldName = CName "foo2",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathConstr
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "nested_types.h:8:16"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_types.h:6:8"}})
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
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathConstr
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "nested_types.h:7:16"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathConstr
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "nested_types.h:8:16"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      (DeclNameTag (CName "bar"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "foo1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathConstr
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "nested_types.h:7:16"},
                      StructField {
                        fieldName = CName "foo2",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathConstr
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc =
                        "nested_types.h:8:16"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_types.h:6:8"}}
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
  DeclNewtypeInstance
    DeriveStock
    Eq
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
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimFloating PrimFloat),
              fieldSourceLoc =
              "nested_types.h:16:11"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            (DeclNameTag (CName "ex3"))
            DeclPathTop,
          structAliases = [],
          structSizeof = 12,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "ex3_c",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimFloating PrimFloat),
              fieldSourceLoc =
              "nested_types.h:16:11"}],
          structFlam = Nothing,
          structSourceLoc =
          "nested_types.h:11:8"}},
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
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimFloating PrimFloat),
                fieldSourceLoc =
                "nested_types.h:16:11"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              (DeclNameTag (CName "ex3"))
              DeclPathTop,
            structAliases = [],
            structSizeof = 12,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "ex3_c",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimFloating PrimFloat),
                fieldSourceLoc =
                "nested_types.h:16:11"}],
            structFlam = Nothing,
            structSourceLoc =
            "nested_types.h:11:8"}}
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "nested_types.h:16:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      (DeclNameTag (CName "ex3"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "ex3_c",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "nested_types.h:16:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_types.h:11:8"}})
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "nested_types.h:16:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      (DeclNameTag (CName "ex3"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 12,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "ex3_c",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimFloat),
                        fieldSourceLoc =
                        "nested_types.h:16:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_types.h:11:8"}}
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
  DeclNewtypeInstance
    DeriveStock
    Eq
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
            "ex4_even_value",
          fieldType = HsPrimType
            HsPrimCDouble,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "value",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimFloating PrimDouble),
              fieldSourceLoc =
              "nested_types.h:25:16"}},
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
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathConstr
                    (DeclNameTag (CName "ex4_odd"))
                    DeclPathTop)),
              fieldSourceLoc =
              "nested_types.h:26:25"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            (DeclNameTag (CName "ex4_even"))
            (DeclPathPtr
              (DeclPathField
                (CName "next")
                (DeclPathConstr
                  (DeclNameTag (CName "ex4_odd"))
                  DeclPathTop))),
          structAliases = [],
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "value",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimFloating PrimDouble),
              fieldSourceLoc =
              "nested_types.h:25:16"},
            StructField {
              fieldName = CName "next",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathConstr
                    (DeclNameTag (CName "ex4_odd"))
                    DeclPathTop)),
              fieldSourceLoc =
              "nested_types.h:26:25"}],
          structFlam = Nothing,
          structSourceLoc =
          "nested_types.h:24:12"}},
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
              "ex4_even_value",
            fieldType = HsPrimType
              HsPrimCDouble,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "value",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimFloating PrimDouble),
                fieldSourceLoc =
                "nested_types.h:25:16"}},
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
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathConstr
                      (DeclNameTag (CName "ex4_odd"))
                      DeclPathTop)),
                fieldSourceLoc =
                "nested_types.h:26:25"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              (DeclNameTag (CName "ex4_even"))
              (DeclPathPtr
                (DeclPathField
                  (CName "next")
                  (DeclPathConstr
                    (DeclNameTag (CName "ex4_odd"))
                    DeclPathTop))),
            structAliases = [],
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "value",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimFloating PrimDouble),
                fieldSourceLoc =
                "nested_types.h:25:16"},
              StructField {
                fieldName = CName "next",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathConstr
                      (DeclNameTag (CName "ex4_odd"))
                      DeclPathTop)),
                fieldSourceLoc =
                "nested_types.h:26:25"}],
            structFlam = Nothing,
            structSourceLoc =
            "nested_types.h:24:12"}}
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
                      "ex4_even_value",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "value",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "nested_types.h:25:16"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              (DeclNameTag (CName "ex4_odd"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "nested_types.h:26:25"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      (DeclNameTag (CName "ex4_even"))
                      (DeclPathPtr
                        (DeclPathField
                          (CName "next")
                          (DeclPathConstr
                            (DeclNameTag (CName "ex4_odd"))
                            DeclPathTop))),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "value",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "nested_types.h:25:16"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              (DeclNameTag (CName "ex4_odd"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "nested_types.h:26:25"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_types.h:24:12"}})
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
                      "ex4_even_value",
                    fieldType = HsPrimType
                      HsPrimCDouble,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "value",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "nested_types.h:25:16"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              (DeclNameTag (CName "ex4_odd"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "nested_types.h:26:25"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      (DeclNameTag (CName "ex4_even"))
                      (DeclPathPtr
                        (DeclPathField
                          (CName "next")
                          (DeclPathConstr
                            (DeclNameTag (CName "ex4_odd"))
                            DeclPathTop))),
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "value",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimFloating PrimDouble),
                        fieldSourceLoc =
                        "nested_types.h:25:16"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              (DeclNameTag (CName "ex4_odd"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "nested_types.h:26:25"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_types.h:24:12"}}
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
  DeclNewtypeInstance
    DeriveStock
    Eq
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
            "ex4_odd_value",
          fieldType = HsPrimType
            HsPrimCInt,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "value",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "nested_types.h:23:9"}},
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
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathConstr
                    (DeclNameTag (CName "ex4_even"))
                    (DeclPathPtr
                      (DeclPathField
                        (CName "next")
                        (DeclPathConstr
                          (DeclNameTag (CName "ex4_odd"))
                          DeclPathTop))))),
              fieldSourceLoc =
              "nested_types.h:27:8"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            (DeclNameTag (CName "ex4_odd"))
            DeclPathTop,
          structAliases = [],
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "value",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "nested_types.h:23:9"},
            StructField {
              fieldName = CName "next",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathConstr
                    (DeclNameTag (CName "ex4_even"))
                    (DeclPathPtr
                      (DeclPathField
                        (CName "next")
                        (DeclPathConstr
                          (DeclNameTag (CName "ex4_odd"))
                          DeclPathTop))))),
              fieldSourceLoc =
              "nested_types.h:27:8"}],
          structFlam = Nothing,
          structSourceLoc =
          "nested_types.h:22:8"}},
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
              "ex4_odd_value",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "value",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "nested_types.h:23:9"}},
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
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathConstr
                      (DeclNameTag (CName "ex4_even"))
                      (DeclPathPtr
                        (DeclPathField
                          (CName "next")
                          (DeclPathConstr
                            (DeclNameTag (CName "ex4_odd"))
                            DeclPathTop))))),
                fieldSourceLoc =
                "nested_types.h:27:8"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              (DeclNameTag (CName "ex4_odd"))
              DeclPathTop,
            structAliases = [],
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "value",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "nested_types.h:23:9"},
              StructField {
                fieldName = CName "next",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathConstr
                      (DeclNameTag (CName "ex4_even"))
                      (DeclPathPtr
                        (DeclPathField
                          (CName "next")
                          (DeclPathConstr
                            (DeclNameTag (CName "ex4_odd"))
                            DeclPathTop))))),
                fieldSourceLoc =
                "nested_types.h:27:8"}],
            structFlam = Nothing,
            structSourceLoc =
            "nested_types.h:22:8"}}
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
                      "ex4_odd_value",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "value",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "nested_types.h:23:9"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              (DeclNameTag (CName "ex4_even"))
                              (DeclPathPtr
                                (DeclPathField
                                  (CName "next")
                                  (DeclPathConstr
                                    (DeclNameTag (CName "ex4_odd"))
                                    DeclPathTop))))),
                        fieldSourceLoc =
                        "nested_types.h:27:8"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      (DeclNameTag (CName "ex4_odd"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "value",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "nested_types.h:23:9"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              (DeclNameTag (CName "ex4_even"))
                              (DeclPathPtr
                                (DeclPathField
                                  (CName "next")
                                  (DeclPathConstr
                                    (DeclNameTag (CName "ex4_odd"))
                                    DeclPathTop))))),
                        fieldSourceLoc =
                        "nested_types.h:27:8"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_types.h:22:8"}})
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
                      "ex4_odd_value",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "value",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "nested_types.h:23:9"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              (DeclNameTag (CName "ex4_even"))
                              (DeclPathPtr
                                (DeclPathField
                                  (CName "next")
                                  (DeclPathConstr
                                    (DeclNameTag (CName "ex4_odd"))
                                    DeclPathTop))))),
                        fieldSourceLoc =
                        "nested_types.h:27:8"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      (DeclNameTag (CName "ex4_odd"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "value",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "nested_types.h:23:9"},
                      StructField {
                        fieldName = CName "next",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              (DeclNameTag (CName "ex4_even"))
                              (DeclPathPtr
                                (DeclPathField
                                  (CName "next")
                                  (DeclPathConstr
                                    (DeclNameTag (CName "ex4_odd"))
                                    DeclPathTop))))),
                        fieldSourceLoc =
                        "nested_types.h:27:8"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_types.h:22:8"}}
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
      "Ex4_odd"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Ex4_odd")]
