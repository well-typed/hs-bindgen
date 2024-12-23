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
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "nested_types.h"],
                singleLocLine = 2,
                singleLocColumn = 9}}},
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
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "nested_types.h"],
                singleLocLine = 3,
                singleLocColumn = 10}}}],
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
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "nested_types.h"],
                singleLocLine = 2,
                singleLocColumn = 9}},
            StructField {
              fieldName = CName "c",
              fieldOffset = 32,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "nested_types.h"],
                singleLocLine = 3,
                singleLocColumn = 10}}],
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "nested_types.h"],
            singleLocLine = 1,
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
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "nested_types.h"],
                  singleLocLine = 2,
                  singleLocColumn = 9}}},
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
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "nested_types.h"],
                  singleLocLine = 3,
                  singleLocColumn = 10}}}],
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
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "nested_types.h"],
                  singleLocLine = 2,
                  singleLocColumn = 9}},
              StructField {
                fieldName = CName "c",
                fieldOffset = 32,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "nested_types.h"],
                  singleLocLine = 3,
                  singleLocColumn = 10}}],
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "nested_types.h"],
              singleLocLine = 1,
              singleLocColumn = 8}}}
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 2,
                          singleLocColumn = 9}}},
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 3,
                          singleLocColumn = 10}}}],
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 2,
                          singleLocColumn = 9}},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 3,
                          singleLocColumn = 10}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "nested_types.h"],
                      singleLocLine = 1,
                      singleLocColumn = 8}}})
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 2,
                          singleLocColumn = 9}}},
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 3,
                          singleLocColumn = 10}}}],
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 2,
                          singleLocColumn = 9}},
                      StructField {
                        fieldName = CName "c",
                        fieldOffset = 32,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 3,
                          singleLocColumn = 10}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "nested_types.h"],
                      singleLocLine = 1,
                      singleLocColumn = 8}}}
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
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "nested_types.h"],
                singleLocLine = 7,
                singleLocColumn = 16}}},
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
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "nested_types.h"],
                singleLocLine = 8,
                singleLocColumn = 16}}}],
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
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "nested_types.h"],
                singleLocLine = 7,
                singleLocColumn = 16}},
            StructField {
              fieldName = CName "foo2",
              fieldOffset = 64,
              fieldType = TypeStruct
                (DeclPathStruct
                  (DeclNameTag (CName "foo"))
                  DeclPathTop),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "nested_types.h"],
                singleLocLine = 8,
                singleLocColumn = 16}}],
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "nested_types.h"],
            singleLocLine = 6,
            singleLocColumn = 8}}},
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
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "nested_types.h"],
                  singleLocLine = 7,
                  singleLocColumn = 16}}},
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
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "nested_types.h"],
                  singleLocLine = 8,
                  singleLocColumn = 16}}}],
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
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "nested_types.h"],
                  singleLocLine = 7,
                  singleLocColumn = 16}},
              StructField {
                fieldName = CName "foo2",
                fieldOffset = 64,
                fieldType = TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "foo"))
                    DeclPathTop),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "nested_types.h"],
                  singleLocLine = 8,
                  singleLocColumn = 16}}],
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "nested_types.h"],
              singleLocLine = 6,
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 7,
                          singleLocColumn = 16}}},
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 8,
                          singleLocColumn = 16}}}],
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 7,
                          singleLocColumn = 16}},
                      StructField {
                        fieldName = CName "foo2",
                        fieldOffset = 64,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 8,
                          singleLocColumn = 16}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "nested_types.h"],
                      singleLocLine = 6,
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 7,
                          singleLocColumn = 16}}},
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 8,
                          singleLocColumn = 16}}}],
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
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 7,
                          singleLocColumn = 16}},
                      StructField {
                        fieldName = CName "foo2",
                        fieldOffset = 64,
                        fieldType = TypeStruct
                          (DeclPathStruct
                            (DeclNameTag (CName "foo"))
                            DeclPathTop),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "nested_types.h"],
                          singleLocLine = 8,
                          singleLocColumn = 16}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "nested_types.h"],
                      singleLocLine = 6,
                      singleLocColumn = 8}}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    8
                    (Idx 1)])))})]
