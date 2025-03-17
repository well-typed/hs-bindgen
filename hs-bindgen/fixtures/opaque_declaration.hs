[
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Foo",
      emptyDataOrigin =
      EmptyDataOriginOpaqueStruct
        OpaqueStruct {
          opaqueStructTag = CName "foo",
          opaqueStructSourceLoc =
          "opaque_declaration.h:1:8"}},
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
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathConstr
                    DeclConstrStruct
                    (DeclNameTag (CName "foo"))
                    DeclPathTop)),
              fieldSourceLoc =
              "opaque_declaration.h:5:17"}},
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
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathConstr
                    DeclConstrStruct
                    (DeclNameTag (CName "bar"))
                    DeclPathTop)),
              fieldSourceLoc =
              "opaque_declaration.h:6:17"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            (DeclNameTag (CName "bar"))
            DeclPathTop,
          structAliases = [],
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "ptrA",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathConstr
                    DeclConstrStruct
                    (DeclNameTag (CName "foo"))
                    DeclPathTop)),
              fieldSourceLoc =
              "opaque_declaration.h:5:17"},
            StructField {
              fieldName = CName "ptrB",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePointer
                (TypeStruct
                  (DeclPathConstr
                    DeclConstrStruct
                    (DeclNameTag (CName "bar"))
                    DeclPathTop)),
              fieldSourceLoc =
              "opaque_declaration.h:6:17"}],
          structFlam = Nothing,
          structSourceLoc =
          "opaque_declaration.h:4:8"}},
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
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop)),
                fieldSourceLoc =
                "opaque_declaration.h:5:17"}},
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
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "bar"))
                      DeclPathTop)),
                fieldSourceLoc =
                "opaque_declaration.h:6:17"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "bar"))
              DeclPathTop,
            structAliases = [],
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "ptrA",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop)),
                fieldSourceLoc =
                "opaque_declaration.h:5:17"},
              StructField {
                fieldName = CName "ptrB",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePointer
                  (TypeStruct
                    (DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "bar"))
                      DeclPathTop)),
                fieldSourceLoc =
                "opaque_declaration.h:6:17"}],
            structFlam = Nothing,
            structSourceLoc =
            "opaque_declaration.h:4:8"}}
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
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              DeclConstrStruct
                              (DeclNameTag (CName "foo"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "opaque_declaration.h:5:17"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              DeclConstrStruct
                              (DeclNameTag (CName "bar"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "opaque_declaration.h:6:17"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "bar"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "ptrA",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              DeclConstrStruct
                              (DeclNameTag (CName "foo"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "opaque_declaration.h:5:17"},
                      StructField {
                        fieldName = CName "ptrB",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              DeclConstrStruct
                              (DeclNameTag (CName "bar"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "opaque_declaration.h:6:17"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "opaque_declaration.h:4:8"}})
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
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              DeclConstrStruct
                              (DeclNameTag (CName "foo"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "opaque_declaration.h:5:17"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              DeclConstrStruct
                              (DeclNameTag (CName "bar"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "opaque_declaration.h:6:17"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "bar"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "ptrA",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              DeclConstrStruct
                              (DeclNameTag (CName "foo"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "opaque_declaration.h:5:17"},
                      StructField {
                        fieldName = CName "ptrB",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePointer
                          (TypeStruct
                            (DeclPathConstr
                              DeclConstrStruct
                              (DeclNameTag (CName "bar"))
                              DeclPathTop)),
                        fieldSourceLoc =
                        "opaque_declaration.h:6:17"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "opaque_declaration.h:4:8"}}
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
        "Baz",
      structConstr = HsName
        "@NsConstr"
        "Baz",
      structFields = [],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            (DeclNameTag (CName "baz"))
            DeclPathTop,
          structAliases = [],
          structSizeof = 0,
          structAlignment = 1,
          structFields = [],
          structFlam = Nothing,
          structSourceLoc =
          "opaque_declaration.h:9:8"}},
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
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "baz"))
              DeclPathTop,
            structAliases = [],
            structSizeof = 0,
            structAlignment = 1,
            structFields = [],
            structFlam = Nothing,
            structSourceLoc =
            "opaque_declaration.h:9:8"}}
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
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "baz"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 0,
                    structAlignment = 1,
                    structFields = [],
                    structFlam = Nothing,
                    structSourceLoc =
                    "opaque_declaration.h:9:8"}})
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
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "baz"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 0,
                    structAlignment = 1,
                    structFields = [],
                    structFlam = Nothing,
                    structSourceLoc =
                    "opaque_declaration.h:9:8"}}
              (Add 0)
              (Seq [])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Baz"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Baz"),
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Quu",
      emptyDataOrigin =
      EmptyDataOriginOpaqueEnum
        OpaqueEnum {
          opaqueEnumTag = CName "quu",
          opaqueEnumSourceLoc =
          "opaque_declaration.h:11:6"}},
  DeclEmpty
    EmptyData {
      emptyDataName = HsName
        "@NsTypeConstr"
        "Opaque_union",
      emptyDataOrigin =
      EmptyDataOriginOpaqueStruct
        OpaqueStruct {
          opaqueStructTag = CName
            "opaque_union",
          opaqueStructSourceLoc =
          "opaque_declaration.h:13:7"}}]
