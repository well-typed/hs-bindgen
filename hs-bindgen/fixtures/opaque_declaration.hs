[
  DeclEmpty
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
            "bar_ptrA",
          fieldType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "Foo")),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "ptrA",
              fieldOffset = 0,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "foo"))
                    DeclPathTop)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "opaque_declaration.h"],
                singleLocLine = 5,
                singleLocColumn = 17}}},
        Field {
          fieldName = HsName
            "@NsVar"
            "bar_ptrB",
          fieldType = HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "Bar")),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "ptrB",
              fieldOffset = 64,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "bar"))
                    DeclPathTop)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "opaque_declaration.h"],
                singleLocLine = 6,
                singleLocColumn = 17}}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "bar"))
            DeclPathTop,
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "ptrA",
              fieldOffset = 0,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "foo"))
                    DeclPathTop)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "opaque_declaration.h"],
                singleLocLine = 5,
                singleLocColumn = 17}},
            StructField {
              fieldName = CName "ptrB",
              fieldOffset = 64,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathStruct
                    (DeclNameTag (CName "bar"))
                    DeclPathTop)),
              fieldSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "opaque_declaration.h"],
                singleLocLine = 6,
                singleLocColumn = 17}}],
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "opaque_declaration.h"],
            singleLocLine = 4,
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
              "bar_ptrA",
            fieldType = HsPtr
              (HsTypRef
                (HsName "@NsTypeConstr" "Foo")),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "ptrA",
                fieldOffset = 0,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "opaque_declaration.h"],
                  singleLocLine = 5,
                  singleLocColumn = 17}}},
          Field {
            fieldName = HsName
              "@NsVar"
              "bar_ptrB",
            fieldType = HsPtr
              (HsTypRef
                (HsName "@NsTypeConstr" "Bar")),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "ptrB",
                fieldOffset = 64,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag (CName "bar"))
                      DeclPathTop)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "opaque_declaration.h"],
                  singleLocLine = 6,
                  singleLocColumn = 17}}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "bar"))
              DeclPathTop,
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "ptrA",
                fieldOffset = 0,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "opaque_declaration.h"],
                  singleLocLine = 5,
                  singleLocColumn = 17}},
              StructField {
                fieldName = CName "ptrB",
                fieldOffset = 64,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathStruct
                      (DeclNameTag (CName "bar"))
                      DeclPathTop)),
                fieldSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "opaque_declaration.h"],
                  singleLocLine = 6,
                  singleLocColumn = 17}}],
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "opaque_declaration.h"],
              singleLocLine = 4,
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
                  "Bar",
                structConstr = HsName
                  "@NsConstr"
                  "Bar",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_ptrA",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName "@NsTypeConstr" "Foo")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ptrA",
                        fieldOffset = 0,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "foo"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "opaque_declaration.h"],
                          singleLocLine = 5,
                          singleLocColumn = 17}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_ptrB",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName "@NsTypeConstr" "Bar")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ptrB",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "bar"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "opaque_declaration.h"],
                          singleLocLine = 6,
                          singleLocColumn = 17}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "bar"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "ptrA",
                        fieldOffset = 0,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "foo"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "opaque_declaration.h"],
                          singleLocLine = 5,
                          singleLocColumn = 17}},
                      StructField {
                        fieldName = CName "ptrB",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "bar"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "opaque_declaration.h"],
                          singleLocLine = 6,
                          singleLocColumn = 17}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "opaque_declaration.h"],
                      singleLocLine = 4,
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
                      "bar_ptrA",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName "@NsTypeConstr" "Foo")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ptrA",
                        fieldOffset = 0,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "foo"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "opaque_declaration.h"],
                          singleLocLine = 5,
                          singleLocColumn = 17}}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bar_ptrB",
                    fieldType = HsPtr
                      (HsTypRef
                        (HsName "@NsTypeConstr" "Bar")),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "ptrB",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "bar"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "opaque_declaration.h"],
                          singleLocLine = 6,
                          singleLocColumn = 17}}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "bar"))
                      DeclPathTop,
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "ptrA",
                        fieldOffset = 0,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "foo"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "opaque_declaration.h"],
                          singleLocLine = 5,
                          singleLocColumn = 17}},
                      StructField {
                        fieldName = CName "ptrB",
                        fieldOffset = 64,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathStruct
                              (DeclNameTag (CName "bar"))
                              DeclPathTop)),
                        fieldSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "opaque_declaration.h"],
                          singleLocLine = 6,
                          singleLocColumn = 17}}],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "opaque_declaration.h"],
                      singleLocLine = 4,
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
        "Baz",
      structConstr = HsName
        "@NsConstr"
        "Baz",
      structFields = [],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathStruct
            (DeclNameTag (CName "baz"))
            DeclPathTop,
          structSizeof = 0,
          structAlignment = 1,
          structFields = [],
          structSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "opaque_declaration.h"],
            singleLocLine = 9,
            singleLocColumn = 8}}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Baz",
        structConstr = HsName
          "@NsConstr"
          "Baz",
        structFields = [],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathStruct
              (DeclNameTag (CName "baz"))
              DeclPathTop,
            structSizeof = 0,
            structAlignment = 1,
            structFields = [],
            structSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "opaque_declaration.h"],
              singleLocLine = 9,
              singleLocColumn = 8}}}
      StorableInstance {
        storableSizeOf = 0,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Baz",
                structConstr = HsName
                  "@NsConstr"
                  "Baz",
                structFields = [],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "baz"))
                      DeclPathTop,
                    structSizeof = 0,
                    structAlignment = 1,
                    structFields = [],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "opaque_declaration.h"],
                      singleLocLine = 9,
                      singleLocColumn = 8}}})
            []),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Baz",
                structConstr = HsName
                  "@NsConstr"
                  "Baz",
                structFields = [],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathStruct
                      (DeclNameTag (CName "baz"))
                      DeclPathTop,
                    structSizeof = 0,
                    structAlignment = 1,
                    structFields = [],
                    structSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "opaque_declaration.h"],
                      singleLocLine = 9,
                      singleLocColumn = 8}}}
              (Add 0)
              (Seq [])))}),
  DeclEmpty
    (HsName "@NsTypeConstr" "Quu")]
