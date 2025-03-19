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
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "flam.h:3:9"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            (DeclNameTag (CName "pascal"))
            DeclPathTop,
          structAliases = [],
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "len",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc = "flam.h:3:9"}],
          structFlam = Just
            StructField {
              fieldName = CName "data",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc = "flam.h:4:10"},
          structSourceLoc =
          "flam.h:2:8"}},
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
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "flam.h:3:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "pascal"))
              DeclPathTop,
            structAliases = [],
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "len",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc = "flam.h:3:9"}],
            structFlam = Just
              StructField {
                fieldName = CName "data",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc = "flam.h:4:10"},
            structSourceLoc = "flam.h:2:8"}}
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "flam.h:3:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "pascal"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "len",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc = "flam.h:3:9"}],
                    structFlam = Just
                      StructField {
                        fieldName = CName "data",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = "flam.h:4:10"},
                    structSourceLoc =
                    "flam.h:2:8"}})
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "flam.h:3:9"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "pascal"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "len",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc = "flam.h:3:9"}],
                    structFlam = Just
                      StructField {
                        fieldName = CName "data",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = "flam.h:4:10"},
                    structSourceLoc = "flam.h:2:8"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Pascal"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Pascal"),
  DeclInstance
    (InstanceHasFLAM
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
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "flam.h:3:9"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "pascal"))
              DeclPathTop,
            structAliases = [],
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "len",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc = "flam.h:3:9"}],
            structFlam = Just
              StructField {
                fieldName = CName "data",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc = "flam.h:4:10"},
            structSourceLoc = "flam.h:2:8"}}
      (HsPrimType HsPrimCChar)
      4),
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
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "flam.h:11:7"}},
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
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "flam.h:12:7"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            DeclNameNone
            (DeclPathField
              (CName "bar")
              (DeclPathConstr
                DeclConstrStruct
                (DeclNameTag (CName "foo"))
                DeclPathTop)),
          structAliases = [],
          structSizeof = 8,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc = "flam.h:11:7"},
            StructField {
              fieldName = CName "y",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "flam.h:12:7"}],
          structFlam = Nothing,
          structSourceLoc =
          "flam.h:10:2"}},
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
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "flam.h:11:7"}},
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
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "flam.h:12:7"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              DeclNameNone
              (DeclPathField
                (CName "bar")
                (DeclPathConstr
                  DeclConstrStruct
                  (DeclNameTag (CName "foo"))
                  DeclPathTop)),
            structAliases = [],
            structSizeof = 8,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc = "flam.h:11:7"},
              StructField {
                fieldName = CName "y",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "flam.h:12:7"}],
            structFlam = Nothing,
            structSourceLoc =
            "flam.h:10:2"}}
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "flam.h:11:7"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "flam.h:12:7"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      DeclNameNone
                      (DeclPathField
                        (CName "bar")
                        (DeclPathConstr
                          DeclConstrStruct
                          (DeclNameTag (CName "foo"))
                          DeclPathTop)),
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc = "flam.h:11:7"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "flam.h:12:7"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "flam.h:10:2"}})
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "flam.h:11:7"}},
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "flam.h:12:7"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      DeclNameNone
                      (DeclPathField
                        (CName "bar")
                        (DeclPathConstr
                          DeclConstrStruct
                          (DeclNameTag (CName "foo"))
                          DeclPathTop)),
                    structAliases = [],
                    structSizeof = 8,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc = "flam.h:11:7"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "flam.h:12:7"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "flam.h:10:2"}}
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
    (HsName
      "@NsTypeConstr"
      "Foo_bar"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Foo_bar"),
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
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc =
              "flam.h:9:6"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            (DeclNameTag (CName "foo"))
            DeclPathTop,
          structAliases = [],
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "len",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimInt Signed),
              fieldSourceLoc = "flam.h:9:6"}],
          structFlam = Just
            StructField {
              fieldName = CName "bar",
              fieldOffset = 32,
              fieldWidth = Nothing,
              fieldType = TypeStruct
                (DeclPathConstr
                  DeclConstrStruct
                  DeclNameNone
                  (DeclPathField
                    (CName "bar")
                    (DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop))),
              fieldSourceLoc = "flam.h:13:4"},
          structSourceLoc =
          "flam.h:8:8"}},
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
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "flam.h:9:6"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "foo"))
              DeclPathTop,
            structAliases = [],
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "len",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc = "flam.h:9:6"}],
            structFlam = Just
              StructField {
                fieldName = CName "bar",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypeStruct
                  (DeclPathConstr
                    DeclConstrStruct
                    DeclNameNone
                    (DeclPathField
                      (CName "bar")
                      (DeclPathConstr
                        DeclConstrStruct
                        (DeclNameTag (CName "foo"))
                        DeclPathTop))),
                fieldSourceLoc = "flam.h:13:4"},
            structSourceLoc = "flam.h:8:8"}}
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "flam.h:9:6"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "len",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc = "flam.h:9:6"}],
                    structFlam = Just
                      StructField {
                        fieldName = CName "bar",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathConstr
                            DeclConstrStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "bar")
                              (DeclPathConstr
                                DeclConstrStruct
                                (DeclNameTag (CName "foo"))
                                DeclPathTop))),
                        fieldSourceLoc = "flam.h:13:4"},
                    structSourceLoc =
                    "flam.h:8:8"}})
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
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc =
                        "flam.h:9:6"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "foo"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "len",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimInt Signed),
                        fieldSourceLoc = "flam.h:9:6"}],
                    structFlam = Just
                      StructField {
                        fieldName = CName "bar",
                        fieldOffset = 32,
                        fieldWidth = Nothing,
                        fieldType = TypeStruct
                          (DeclPathConstr
                            DeclConstrStruct
                            DeclNameNone
                            (DeclPathField
                              (CName "bar")
                              (DeclPathConstr
                                DeclConstrStruct
                                (DeclNameTag (CName "foo"))
                                DeclPathTop))),
                        fieldSourceLoc = "flam.h:13:4"},
                    structSourceLoc = "flam.h:8:8"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Foo"),
  DeclInstance
    (InstanceHasFLAM
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
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc =
                "flam.h:9:6"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "foo"))
              DeclPathTop,
            structAliases = [],
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "len",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimInt Signed),
                fieldSourceLoc = "flam.h:9:6"}],
            structFlam = Just
              StructField {
                fieldName = CName "bar",
                fieldOffset = 32,
                fieldWidth = Nothing,
                fieldType = TypeStruct
                  (DeclPathConstr
                    DeclConstrStruct
                    DeclNameNone
                    (DeclPathField
                      (CName "bar")
                      (DeclPathConstr
                        DeclConstrStruct
                        (DeclNameTag (CName "foo"))
                        DeclPathTop))),
                fieldSourceLoc = "flam.h:13:4"},
            structSourceLoc = "flam.h:8:8"}}
      (HsTypRef
        (HsName
          "@NsTypeConstr"
          "Foo_bar"))
      4),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Diff",
      structConstr = HsName
        "@NsConstr"
        "Diff",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "diff_first",
          fieldType = HsPrimType
            HsPrimCLong,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "first",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc =
              "flam.h:18:7"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "diff_second",
          fieldType = HsPrimType
            HsPrimCChar,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "second",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "flam.h:19:7"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathConstr
            DeclConstrStruct
            (DeclNameTag (CName "diff"))
            DeclPathTop,
          structAliases = [],
          structSizeof = 16,
          structAlignment = 8,
          structFields = [
            StructField {
              fieldName = CName "first",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimIntegral PrimLong Signed),
              fieldSourceLoc = "flam.h:18:7"},
            StructField {
              fieldName = CName "second",
              fieldOffset = 64,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc =
              "flam.h:19:7"}],
          structFlam = Just
            StructField {
              fieldName = CName "flam",
              fieldOffset = 72,
              fieldWidth = Nothing,
              fieldType = TypePrim
                (PrimChar Nothing),
              fieldSourceLoc = "flam.h:20:7"},
          structSourceLoc =
          "flam.h:17:8"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Diff",
        structConstr = HsName
          "@NsConstr"
          "Diff",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "diff_first",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "first",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "flam.h:18:7"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "diff_second",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "second",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "flam.h:19:7"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "diff"))
              DeclPathTop,
            structAliases = [],
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "first",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc = "flam.h:18:7"},
              StructField {
                fieldName = CName "second",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "flam.h:19:7"}],
            structFlam = Just
              StructField {
                fieldName = CName "flam",
                fieldOffset = 72,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc = "flam.h:20:7"},
            structSourceLoc =
            "flam.h:17:8"}}
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
                  "Diff",
                structConstr = HsName
                  "@NsConstr"
                  "Diff",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "diff_first",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "first",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "flam.h:18:7"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "diff_second",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "second",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "flam.h:19:7"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "diff"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "first",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc = "flam.h:18:7"},
                      StructField {
                        fieldName = CName "second",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "flam.h:19:7"}],
                    structFlam = Just
                      StructField {
                        fieldName = CName "flam",
                        fieldOffset = 72,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = "flam.h:20:7"},
                    structSourceLoc =
                    "flam.h:17:8"}})
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
                  "Diff",
                structConstr = HsName
                  "@NsConstr"
                  "Diff",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "diff_first",
                    fieldType = HsPrimType
                      HsPrimCLong,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "first",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc =
                        "flam.h:18:7"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "diff_second",
                    fieldType = HsPrimType
                      HsPrimCChar,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "second",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "flam.h:19:7"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathConstr
                      DeclConstrStruct
                      (DeclNameTag (CName "diff"))
                      DeclPathTop,
                    structAliases = [],
                    structSizeof = 16,
                    structAlignment = 8,
                    structFields = [
                      StructField {
                        fieldName = CName "first",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimIntegral PrimLong Signed),
                        fieldSourceLoc = "flam.h:18:7"},
                      StructField {
                        fieldName = CName "second",
                        fieldOffset = 64,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc =
                        "flam.h:19:7"}],
                    structFlam = Just
                      StructField {
                        fieldName = CName "flam",
                        fieldOffset = 72,
                        fieldWidth = Nothing,
                        fieldType = TypePrim
                          (PrimChar Nothing),
                        fieldSourceLoc = "flam.h:20:7"},
                    structSourceLoc =
                    "flam.h:17:8"}}
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
    (HsName "@NsTypeConstr" "Diff"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Diff"),
  DeclInstance
    (InstanceHasFLAM
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Diff",
        structConstr = HsName
          "@NsConstr"
          "Diff",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "diff_first",
            fieldType = HsPrimType
              HsPrimCLong,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "first",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc =
                "flam.h:18:7"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "diff_second",
            fieldType = HsPrimType
              HsPrimCChar,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "second",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "flam.h:19:7"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathConstr
              DeclConstrStruct
              (DeclNameTag (CName "diff"))
              DeclPathTop,
            structAliases = [],
            structSizeof = 16,
            structAlignment = 8,
            structFields = [
              StructField {
                fieldName = CName "first",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimIntegral PrimLong Signed),
                fieldSourceLoc = "flam.h:18:7"},
              StructField {
                fieldName = CName "second",
                fieldOffset = 64,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc =
                "flam.h:19:7"}],
            structFlam = Just
              StructField {
                fieldName = CName "flam",
                fieldOffset = 72,
                fieldWidth = Nothing,
                fieldType = TypePrim
                  (PrimChar Nothing),
                fieldSourceLoc = "flam.h:20:7"},
            structSourceLoc =
            "flam.h:17:8"}}
      (HsPrimType HsPrimCChar)
      9)]
