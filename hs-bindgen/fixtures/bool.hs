[
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Bools1",
      structConstr = HsName
        "@NsConstr"
        "Bools1",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "bools1_x",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim PrimBool,
              fieldSourceLoc =
              "bool.h:2:11"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "bools1_y",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "y",
              fieldOffset = 8,
              fieldWidth = Nothing,
              fieldType = TypePrim PrimBool,
              fieldSourceLoc =
              "bool.h:3:11"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "bools1"),
          structAliases = [],
          structSizeof = 2,
          structAlignment = 1,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim PrimBool,
              fieldSourceLoc = "bool.h:2:11"},
            StructField {
              fieldName = CName "y",
              fieldOffset = 8,
              fieldWidth = Nothing,
              fieldType = TypePrim PrimBool,
              fieldSourceLoc =
              "bool.h:3:11"}],
          structFlam = Nothing,
          structSourceLoc =
          "bool.h:1:8"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Bools1",
        structConstr = HsName
          "@NsConstr"
          "Bools1",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "bools1_x",
            fieldType = HsPrimType
              HsPrimCBool,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim PrimBool,
                fieldSourceLoc =
                "bool.h:2:11"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "bools1_y",
            fieldType = HsPrimType
              HsPrimCBool,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "y",
                fieldOffset = 8,
                fieldWidth = Nothing,
                fieldType = TypePrim PrimBool,
                fieldSourceLoc =
                "bool.h:3:11"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "bools1"),
            structAliases = [],
            structSizeof = 2,
            structAlignment = 1,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim PrimBool,
                fieldSourceLoc = "bool.h:2:11"},
              StructField {
                fieldName = CName "y",
                fieldOffset = 8,
                fieldWidth = Nothing,
                fieldType = TypePrim PrimBool,
                fieldSourceLoc =
                "bool.h:3:11"}],
            structFlam = Nothing,
            structSourceLoc = "bool.h:1:8"}}
      StorableInstance {
        storableSizeOf = 2,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Bools1",
                structConstr = HsName
                  "@NsConstr"
                  "Bools1",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bools1_x",
                    fieldType = HsPrimType
                      HsPrimCBool,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "bool.h:2:11"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bools1_y",
                    fieldType = HsPrimType
                      HsPrimCBool,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 8,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "bool.h:3:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "bools1"),
                    structAliases = [],
                    structSizeof = 2,
                    structAlignment = 1,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc = "bool.h:2:11"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 8,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "bool.h:3:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bool.h:1:8"}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 1]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Bools1",
                structConstr = HsName
                  "@NsConstr"
                  "Bools1",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bools1_x",
                    fieldType = HsPrimType
                      HsPrimCBool,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "bool.h:2:11"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bools1_y",
                    fieldType = HsPrimType
                      HsPrimCBool,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 8,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "bool.h:3:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "bools1"),
                    structAliases = [],
                    structSizeof = 2,
                    structAlignment = 1,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc = "bool.h:2:11"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 8,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "bool.h:3:11"}],
                    structFlam = Nothing,
                    structSourceLoc = "bool.h:1:8"}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    1
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Bools1"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Bools1"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Bools2",
      structConstr = HsName
        "@NsConstr"
        "Bools2",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "bools2_x",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim PrimBool,
              fieldSourceLoc =
              "bool.h:9:10"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "bools2_y",
          fieldType = HsPrimType
            HsPrimCBool,
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "y",
              fieldOffset = 8,
              fieldWidth = Nothing,
              fieldType = TypePrim PrimBool,
              fieldSourceLoc =
              "bool.h:10:10"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "bools2"),
          structAliases = [],
          structSizeof = 2,
          structAlignment = 1,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypePrim PrimBool,
              fieldSourceLoc = "bool.h:9:10"},
            StructField {
              fieldName = CName "y",
              fieldOffset = 8,
              fieldWidth = Nothing,
              fieldType = TypePrim PrimBool,
              fieldSourceLoc =
              "bool.h:10:10"}],
          structFlam = Nothing,
          structSourceLoc =
          "bool.h:8:8"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Bools2",
        structConstr = HsName
          "@NsConstr"
          "Bools2",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "bools2_x",
            fieldType = HsPrimType
              HsPrimCBool,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim PrimBool,
                fieldSourceLoc =
                "bool.h:9:10"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "bools2_y",
            fieldType = HsPrimType
              HsPrimCBool,
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "y",
                fieldOffset = 8,
                fieldWidth = Nothing,
                fieldType = TypePrim PrimBool,
                fieldSourceLoc =
                "bool.h:10:10"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "bools2"),
            structAliases = [],
            structSizeof = 2,
            structAlignment = 1,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypePrim PrimBool,
                fieldSourceLoc = "bool.h:9:10"},
              StructField {
                fieldName = CName "y",
                fieldOffset = 8,
                fieldWidth = Nothing,
                fieldType = TypePrim PrimBool,
                fieldSourceLoc =
                "bool.h:10:10"}],
            structFlam = Nothing,
            structSourceLoc = "bool.h:8:8"}}
      StorableInstance {
        storableSizeOf = 2,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Bools2",
                structConstr = HsName
                  "@NsConstr"
                  "Bools2",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bools2_x",
                    fieldType = HsPrimType
                      HsPrimCBool,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "bool.h:9:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bools2_y",
                    fieldType = HsPrimType
                      HsPrimCBool,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 8,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "bool.h:10:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "bools2"),
                    structAliases = [],
                    structSizeof = 2,
                    structAlignment = 1,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc = "bool.h:9:10"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 8,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "bool.h:10:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bool.h:8:8"}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 1]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Bools2",
                structConstr = HsName
                  "@NsConstr"
                  "Bools2",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bools2_x",
                    fieldType = HsPrimType
                      HsPrimCBool,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "bool.h:9:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bools2_y",
                    fieldType = HsPrimType
                      HsPrimCBool,
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 8,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "bool.h:10:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "bools2"),
                    structAliases = [],
                    structSizeof = 2,
                    structAlignment = 1,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc = "bool.h:9:10"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 8,
                        fieldWidth = Nothing,
                        fieldType = TypePrim PrimBool,
                        fieldSourceLoc =
                        "bool.h:10:10"}],
                    structFlam = Nothing,
                    structSourceLoc = "bool.h:8:8"}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    1
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Bools2"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Bools2"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "BOOL",
      newtypeConstr = HsName
        "@NsConstr"
        "BOOL",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_BOOL",
        fieldType = HsPrimType
          HsPrimCBool,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "bool.h:13:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "BOOL",
          macroArgs = [],
          macroBody = MTerm
            (MType (TypePrim PrimBool))}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "BOOL"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "BOOL"),
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "Bools3",
      structConstr = HsName
        "@NsConstr"
        "Bools3",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "bools3_x",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "BOOL"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "BOOL"),
              fieldSourceLoc =
              "bool.h:16:10"}},
        Field {
          fieldName = HsName
            "@NsVar"
            "bools3_y",
          fieldType = HsTypRef
            (HsName "@NsTypeConstr" "BOOL"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "y",
              fieldOffset = 8,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "BOOL"),
              fieldSourceLoc =
              "bool.h:17:10"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "bools3"),
          structAliases = [],
          structSizeof = 2,
          structAlignment = 1,
          structFields = [
            StructField {
              fieldName = CName "x",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "BOOL"),
              fieldSourceLoc =
              "bool.h:16:10"},
            StructField {
              fieldName = CName "y",
              fieldOffset = 8,
              fieldWidth = Nothing,
              fieldType = TypeTypedef
                (CName "BOOL"),
              fieldSourceLoc =
              "bool.h:17:10"}],
          structFlam = Nothing,
          structSourceLoc =
          "bool.h:15:8"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Bools3",
        structConstr = HsName
          "@NsConstr"
          "Bools3",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "bools3_x",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "BOOL"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "BOOL"),
                fieldSourceLoc =
                "bool.h:16:10"}},
          Field {
            fieldName = HsName
              "@NsVar"
              "bools3_y",
            fieldType = HsTypRef
              (HsName "@NsTypeConstr" "BOOL"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "y",
                fieldOffset = 8,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "BOOL"),
                fieldSourceLoc =
                "bool.h:17:10"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "bools3"),
            structAliases = [],
            structSizeof = 2,
            structAlignment = 1,
            structFields = [
              StructField {
                fieldName = CName "x",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "BOOL"),
                fieldSourceLoc =
                "bool.h:16:10"},
              StructField {
                fieldName = CName "y",
                fieldOffset = 8,
                fieldWidth = Nothing,
                fieldType = TypeTypedef
                  (CName "BOOL"),
                fieldSourceLoc =
                "bool.h:17:10"}],
            structFlam = Nothing,
            structSourceLoc =
            "bool.h:15:8"}}
      StorableInstance {
        storableSizeOf = 2,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Bools3",
                structConstr = HsName
                  "@NsConstr"
                  "Bools3",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bools3_x",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "BOOL"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "BOOL"),
                        fieldSourceLoc =
                        "bool.h:16:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bools3_y",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "BOOL"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 8,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "BOOL"),
                        fieldSourceLoc =
                        "bool.h:17:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "bools3"),
                    structAliases = [],
                    structSizeof = 2,
                    structAlignment = 1,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "BOOL"),
                        fieldSourceLoc =
                        "bool.h:16:10"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 8,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "BOOL"),
                        fieldSourceLoc =
                        "bool.h:17:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bool.h:15:8"}})
            [
              PeekByteOff (Idx 0) 0,
              PeekByteOff (Idx 0) 1]),
        storablePoke = Lambda
          (NameHint "ptr")
          (Lambda
            (NameHint "s")
            (ElimStruct
              (Idx 0)
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Bools3",
                structConstr = HsName
                  "@NsConstr"
                  "Bools3",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bools3_x",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "BOOL"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "BOOL"),
                        fieldSourceLoc =
                        "bool.h:16:10"}},
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "bools3_y",
                    fieldType = HsTypRef
                      (HsName "@NsTypeConstr" "BOOL"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 8,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "BOOL"),
                        fieldSourceLoc =
                        "bool.h:17:10"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "bools3"),
                    structAliases = [],
                    structSizeof = 2,
                    structAlignment = 1,
                    structFields = [
                      StructField {
                        fieldName = CName "x",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "BOOL"),
                        fieldSourceLoc =
                        "bool.h:16:10"},
                      StructField {
                        fieldName = CName "y",
                        fieldOffset = 8,
                        fieldWidth = Nothing,
                        fieldType = TypeTypedef
                          (CName "BOOL"),
                        fieldSourceLoc =
                        "bool.h:17:10"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "bool.h:15:8"}}
              (Add 2)
              (Seq
                [
                  PokeByteOff (Idx 3) 0 (Idx 0),
                  PokeByteOff
                    (Idx 3)
                    1
                    (Idx 1)])))}),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Bools3"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Bools3")]
