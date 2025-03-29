[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "First",
      newtypeConstr = HsName
        "@NsConstr"
        "First",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_First",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "first")
            DeclPathCtxtTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "FIRST1",
              valueValue = 0,
              valueSourceLoc = "enums.h:5:5"},
            EnumValue {
              valueName = CName "FIRST2",
              valueValue = 1,
              valueSourceLoc =
              "enums.h:6:5"}],
          enumSourceLoc = "enums.h:4:6"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "First",
        structConstr = HsName
          "@NsConstr"
          "First",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_First",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "first")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "FIRST1",
                valueValue = 0,
                valueSourceLoc = "enums.h:5:5"},
              EnumValue {
                valueName = CName "FIRST2",
                valueValue = 1,
                valueSourceLoc =
                "enums.h:6:5"}],
            enumSourceLoc = "enums.h:4:6"}}
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
                  "First",
                structConstr = HsName
                  "@NsConstr"
                  "First",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_First",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "first")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "FIRST1",
                        valueValue = 0,
                        valueSourceLoc = "enums.h:5:5"},
                      EnumValue {
                        valueName = CName "FIRST2",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:6:5"}],
                    enumSourceLoc = "enums.h:4:6"}})
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
                  "First",
                structConstr = HsName
                  "@NsConstr"
                  "First",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_First",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "first")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "FIRST1",
                        valueValue = 0,
                        valueSourceLoc = "enums.h:5:5"},
                      EnumValue {
                        valueName = CName "FIRST2",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:6:5"}],
                    enumSourceLoc = "enums.h:4:6"}}
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
      "First"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "First"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "First"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "First"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "First"),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "FIRST1",
      patSynType = HsName
        "@NsTypeConstr"
        "First",
      patSynConstr = HsName
        "@NsConstr"
        "First",
      patSynValue = 0,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "FIRST1",
          valueValue = 0,
          valueSourceLoc =
          "enums.h:5:5"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "FIRST2",
      patSynType = HsName
        "@NsTypeConstr"
        "First",
      patSynConstr = HsName
        "@NsConstr"
        "First",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "FIRST2",
          valueValue = 1,
          valueSourceLoc =
          "enums.h:6:5"}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Second",
      newtypeConstr = HsName
        "@NsConstr"
        "Second",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Second",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "second")
            DeclPathCtxtTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Signed),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "SECOND_A",
              valueValue = `-1`,
              valueSourceLoc =
              "enums.h:10:5"},
            EnumValue {
              valueName = CName "SECOND_B",
              valueValue = 0,
              valueSourceLoc =
              "enums.h:11:5"},
            EnumValue {
              valueName = CName "SECOND_C",
              valueValue = 1,
              valueSourceLoc =
              "enums.h:12:5"}],
          enumSourceLoc = "enums.h:9:6"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Second",
        structConstr = HsName
          "@NsConstr"
          "Second",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Second",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "second")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Signed),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "SECOND_A",
                valueValue = `-1`,
                valueSourceLoc =
                "enums.h:10:5"},
              EnumValue {
                valueName = CName "SECOND_B",
                valueValue = 0,
                valueSourceLoc =
                "enums.h:11:5"},
              EnumValue {
                valueName = CName "SECOND_C",
                valueValue = 1,
                valueSourceLoc =
                "enums.h:12:5"}],
            enumSourceLoc = "enums.h:9:6"}}
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
                  "Second",
                structConstr = HsName
                  "@NsConstr"
                  "Second",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Second",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "second")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "SECOND_A",
                        valueValue = `-1`,
                        valueSourceLoc =
                        "enums.h:10:5"},
                      EnumValue {
                        valueName = CName "SECOND_B",
                        valueValue = 0,
                        valueSourceLoc =
                        "enums.h:11:5"},
                      EnumValue {
                        valueName = CName "SECOND_C",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:12:5"}],
                    enumSourceLoc = "enums.h:9:6"}})
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
                  "Second",
                structConstr = HsName
                  "@NsConstr"
                  "Second",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Second",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "second")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "SECOND_A",
                        valueValue = `-1`,
                        valueSourceLoc =
                        "enums.h:10:5"},
                      EnumValue {
                        valueName = CName "SECOND_B",
                        valueValue = 0,
                        valueSourceLoc =
                        "enums.h:11:5"},
                      EnumValue {
                        valueName = CName "SECOND_C",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:12:5"}],
                    enumSourceLoc = "enums.h:9:6"}}
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
      "Second"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Second"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Second"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Second"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Second"),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "SECOND_A",
      patSynType = HsName
        "@NsTypeConstr"
        "Second",
      patSynConstr = HsName
        "@NsConstr"
        "Second",
      patSynValue = `-1`,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "SECOND_A",
          valueValue = `-1`,
          valueSourceLoc =
          "enums.h:10:5"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "SECOND_B",
      patSynType = HsName
        "@NsTypeConstr"
        "Second",
      patSynConstr = HsName
        "@NsConstr"
        "Second",
      patSynValue = 0,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "SECOND_B",
          valueValue = 0,
          valueSourceLoc =
          "enums.h:11:5"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "SECOND_C",
      patSynType = HsName
        "@NsTypeConstr"
        "Second",
      patSynConstr = HsName
        "@NsConstr"
        "Second",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "SECOND_C",
          valueValue = 1,
          valueSourceLoc =
          "enums.h:12:5"}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Same",
      newtypeConstr = HsName
        "@NsConstr"
        "Same",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Same",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "same")
            DeclPathCtxtTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "SAME_A",
              valueValue = 1,
              valueSourceLoc =
              "enums.h:16:5"},
            EnumValue {
              valueName = CName "SAME_B",
              valueValue = 1,
              valueSourceLoc =
              "enums.h:17:5"}],
          enumSourceLoc =
          "enums.h:15:6"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Same",
        structConstr = HsName
          "@NsConstr"
          "Same",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Same",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "same")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "SAME_A",
                valueValue = 1,
                valueSourceLoc =
                "enums.h:16:5"},
              EnumValue {
                valueName = CName "SAME_B",
                valueValue = 1,
                valueSourceLoc =
                "enums.h:17:5"}],
            enumSourceLoc = "enums.h:15:6"}}
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
                  "Same",
                structConstr = HsName
                  "@NsConstr"
                  "Same",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Same",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "same")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "SAME_A",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:16:5"},
                      EnumValue {
                        valueName = CName "SAME_B",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:17:5"}],
                    enumSourceLoc =
                    "enums.h:15:6"}})
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
                  "Same",
                structConstr = HsName
                  "@NsConstr"
                  "Same",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Same",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "same")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "SAME_A",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:16:5"},
                      EnumValue {
                        valueName = CName "SAME_B",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:17:5"}],
                    enumSourceLoc = "enums.h:15:6"}}
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
    (HsName "@NsTypeConstr" "Same"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "Same"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Same"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Same"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "Same"),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "SAME_A",
      patSynType = HsName
        "@NsTypeConstr"
        "Same",
      patSynConstr = HsName
        "@NsConstr"
        "Same",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "SAME_A",
          valueValue = 1,
          valueSourceLoc =
          "enums.h:16:5"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "SAME_B",
      patSynType = HsName
        "@NsTypeConstr"
        "Same",
      patSynConstr = HsName
        "@NsConstr"
        "Same",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "SAME_B",
          valueValue = 1,
          valueSourceLoc =
          "enums.h:17:5"}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Packad",
      newtypeConstr = HsName
        "@NsConstr"
        "Packad",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Packad",
        fieldType = HsPrimType
          HsPrimCSChar,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "packad")
            DeclPathCtxtTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimChar (Just Unsigned)),
          enumSizeof = 1,
          enumAlignment = 1,
          enumValues = [
            EnumValue {
              valueName = CName "PACKED_A",
              valueValue = 0,
              valueSourceLoc =
              "enums.h:21:5"},
            EnumValue {
              valueName = CName "PACKED_B",
              valueValue = 1,
              valueSourceLoc =
              "enums.h:21:15"},
            EnumValue {
              valueName = CName "PACKED_C",
              valueValue = 2,
              valueSourceLoc =
              "enums.h:21:25"}],
          enumSourceLoc =
          "enums.h:20:6"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "Packad",
        structConstr = HsName
          "@NsConstr"
          "Packad",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_Packad",
            fieldType = HsPrimType
              HsPrimCSChar,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "packad")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimChar (Just Unsigned)),
            enumSizeof = 1,
            enumAlignment = 1,
            enumValues = [
              EnumValue {
                valueName = CName "PACKED_A",
                valueValue = 0,
                valueSourceLoc =
                "enums.h:21:5"},
              EnumValue {
                valueName = CName "PACKED_B",
                valueValue = 1,
                valueSourceLoc =
                "enums.h:21:15"},
              EnumValue {
                valueName = CName "PACKED_C",
                valueValue = 2,
                valueSourceLoc =
                "enums.h:21:25"}],
            enumSourceLoc = "enums.h:20:6"}}
      StorableInstance {
        storableSizeOf = 1,
        storableAlignment = 1,
        storablePeek = Lambda
          (NameHint "ptr")
          (Ap
            (StructCon
              Struct {
                structName = HsName
                  "@NsTypeConstr"
                  "Packad",
                structConstr = HsName
                  "@NsConstr"
                  "Packad",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Packad",
                    fieldType = HsPrimType
                      HsPrimCSChar,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "packad")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimChar (Just Unsigned)),
                    enumSizeof = 1,
                    enumAlignment = 1,
                    enumValues = [
                      EnumValue {
                        valueName = CName "PACKED_A",
                        valueValue = 0,
                        valueSourceLoc =
                        "enums.h:21:5"},
                      EnumValue {
                        valueName = CName "PACKED_B",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:21:15"},
                      EnumValue {
                        valueName = CName "PACKED_C",
                        valueValue = 2,
                        valueSourceLoc =
                        "enums.h:21:25"}],
                    enumSourceLoc =
                    "enums.h:20:6"}})
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
                  "Packad",
                structConstr = HsName
                  "@NsConstr"
                  "Packad",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_Packad",
                    fieldType = HsPrimType
                      HsPrimCSChar,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "packad")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimChar (Just Unsigned)),
                    enumSizeof = 1,
                    enumAlignment = 1,
                    enumValues = [
                      EnumValue {
                        valueName = CName "PACKED_A",
                        valueValue = 0,
                        valueSourceLoc =
                        "enums.h:21:5"},
                      EnumValue {
                        valueName = CName "PACKED_B",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:21:15"},
                      EnumValue {
                        valueName = CName "PACKED_C",
                        valueValue = 2,
                        valueSourceLoc =
                        "enums.h:21:25"}],
                    enumSourceLoc = "enums.h:20:6"}}
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
      "Packad"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "Packad"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Packad"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "Packad"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "Packad"),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "PACKED_A",
      patSynType = HsName
        "@NsTypeConstr"
        "Packad",
      patSynConstr = HsName
        "@NsConstr"
        "Packad",
      patSynValue = 0,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "PACKED_A",
          valueValue = 0,
          valueSourceLoc =
          "enums.h:21:5"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "PACKED_B",
      patSynType = HsName
        "@NsTypeConstr"
        "Packad",
      patSynConstr = HsName
        "@NsConstr"
        "Packad",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "PACKED_B",
          valueValue = 1,
          valueSourceLoc =
          "enums.h:21:15"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "PACKED_C",
      patSynType = HsName
        "@NsTypeConstr"
        "Packad",
      patSynConstr = HsName
        "@NsConstr"
        "Packad",
      patSynValue = 2,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "PACKED_C",
          valueValue = 2,
          valueSourceLoc =
          "enums.h:21:25"}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "EnumA",
      newtypeConstr = HsName
        "@NsConstr"
        "EnumA",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_EnumA",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathAnon
            (DeclPathCtxtTypedef
              (CName "enumA")),
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "A_FOO",
              valueValue = 0,
              valueSourceLoc =
              "enums.h:24:16"},
            EnumValue {
              valueName = CName "A_BAR",
              valueValue = 1,
              valueSourceLoc =
              "enums.h:24:23"}],
          enumSourceLoc =
          "enums.h:24:9"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumA",
        structConstr = HsName
          "@NsConstr"
          "EnumA",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumA",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathAnon
              (DeclPathCtxtTypedef
                (CName "enumA")),
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "A_FOO",
                valueValue = 0,
                valueSourceLoc =
                "enums.h:24:16"},
              EnumValue {
                valueName = CName "A_BAR",
                valueValue = 1,
                valueSourceLoc =
                "enums.h:24:23"}],
            enumSourceLoc = "enums.h:24:9"}}
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
                  "EnumA",
                structConstr = HsName
                  "@NsConstr"
                  "EnumA",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumA",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathAnon
                      (DeclPathCtxtTypedef
                        (CName "enumA")),
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "A_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "enums.h:24:16"},
                      EnumValue {
                        valueName = CName "A_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:24:23"}],
                    enumSourceLoc =
                    "enums.h:24:9"}})
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
                  "EnumA",
                structConstr = HsName
                  "@NsConstr"
                  "EnumA",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumA",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathAnon
                      (DeclPathCtxtTypedef
                        (CName "enumA")),
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "A_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "enums.h:24:16"},
                      EnumValue {
                        valueName = CName "A_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:24:23"}],
                    enumSourceLoc = "enums.h:24:9"}}
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
      "EnumA"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "EnumA"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "EnumA"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "EnumA"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "EnumA"),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "A_FOO",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumA",
      patSynConstr = HsName
        "@NsConstr"
        "EnumA",
      patSynValue = 0,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "A_FOO",
          valueValue = 0,
          valueSourceLoc =
          "enums.h:24:16"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "A_BAR",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumA",
      patSynConstr = HsName
        "@NsConstr"
        "EnumA",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "A_BAR",
          valueValue = 1,
          valueSourceLoc =
          "enums.h:24:23"}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "EnumA",
      newtypeConstr = HsName
        "@NsConstr"
        "EnumA",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_EnumA",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "EnumA"),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "enumA",
          typedefType = TypeEnum
            (DeclPathAnon
              (DeclPathCtxtTypedef
                (CName "enumA"))),
          typedefSourceLoc =
          "enums.h:24:31"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "EnumA"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "EnumB",
      newtypeConstr = HsName
        "@NsConstr"
        "EnumB",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_EnumB",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "enumB")
            DeclPathCtxtTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "B_FOO",
              valueValue = 0,
              valueSourceLoc =
              "enums.h:26:22"},
            EnumValue {
              valueName = CName "B_BAR",
              valueValue = 1,
              valueSourceLoc =
              "enums.h:26:29"}],
          enumSourceLoc =
          "enums.h:26:14"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumB",
        structConstr = HsName
          "@NsConstr"
          "EnumB",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumB",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "enumB")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "B_FOO",
                valueValue = 0,
                valueSourceLoc =
                "enums.h:26:22"},
              EnumValue {
                valueName = CName "B_BAR",
                valueValue = 1,
                valueSourceLoc =
                "enums.h:26:29"}],
            enumSourceLoc =
            "enums.h:26:14"}}
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
                  "EnumB",
                structConstr = HsName
                  "@NsConstr"
                  "EnumB",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumB",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "enumB")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "B_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "enums.h:26:22"},
                      EnumValue {
                        valueName = CName "B_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:26:29"}],
                    enumSourceLoc =
                    "enums.h:26:14"}})
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
                  "EnumB",
                structConstr = HsName
                  "@NsConstr"
                  "EnumB",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumB",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "enumB")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "B_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "enums.h:26:22"},
                      EnumValue {
                        valueName = CName "B_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:26:29"}],
                    enumSourceLoc =
                    "enums.h:26:14"}}
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
      "EnumB"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "EnumB"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "EnumB"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "EnumB"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "EnumB"),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "B_FOO",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumB",
      patSynConstr = HsName
        "@NsConstr"
        "EnumB",
      patSynValue = 0,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "B_FOO",
          valueValue = 0,
          valueSourceLoc =
          "enums.h:26:22"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "B_BAR",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumB",
      patSynConstr = HsName
        "@NsConstr"
        "EnumB",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "B_BAR",
          valueValue = 1,
          valueSourceLoc =
          "enums.h:26:29"}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "EnumB",
      newtypeConstr = HsName
        "@NsConstr"
        "EnumB",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_EnumB",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "EnumB"),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "enumB",
          typedefType = TypeEnum
            (DeclPathName
              (CName "enumB")
              DeclPathCtxtTop),
          typedefSourceLoc =
          "enums.h:26:37"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "EnumB"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "EnumC",
      newtypeConstr = HsName
        "@NsConstr"
        "EnumC",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_EnumC",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "enumC")
            DeclPathCtxtTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "C_FOO",
              valueValue = 0,
              valueSourceLoc =
              "enums.h:28:14"},
            EnumValue {
              valueName = CName "C_BAR",
              valueValue = 1,
              valueSourceLoc =
              "enums.h:28:21"}],
          enumSourceLoc =
          "enums.h:28:6"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumC",
        structConstr = HsName
          "@NsConstr"
          "EnumC",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumC",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "enumC")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "C_FOO",
                valueValue = 0,
                valueSourceLoc =
                "enums.h:28:14"},
              EnumValue {
                valueName = CName "C_BAR",
                valueValue = 1,
                valueSourceLoc =
                "enums.h:28:21"}],
            enumSourceLoc = "enums.h:28:6"}}
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
                  "EnumC",
                structConstr = HsName
                  "@NsConstr"
                  "EnumC",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumC",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "enumC")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "C_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "enums.h:28:14"},
                      EnumValue {
                        valueName = CName "C_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:28:21"}],
                    enumSourceLoc =
                    "enums.h:28:6"}})
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
                  "EnumC",
                structConstr = HsName
                  "@NsConstr"
                  "EnumC",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumC",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "enumC")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "C_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "enums.h:28:14"},
                      EnumValue {
                        valueName = CName "C_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:28:21"}],
                    enumSourceLoc = "enums.h:28:6"}}
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
      "EnumC"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "EnumC"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "EnumC"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "EnumC"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "EnumC"),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "C_FOO",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumC",
      patSynConstr = HsName
        "@NsConstr"
        "EnumC",
      patSynValue = 0,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "C_FOO",
          valueValue = 0,
          valueSourceLoc =
          "enums.h:28:14"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "C_BAR",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumC",
      patSynConstr = HsName
        "@NsConstr"
        "EnumC",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "C_BAR",
          valueValue = 1,
          valueSourceLoc =
          "enums.h:28:21"}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "EnumC",
      newtypeConstr = HsName
        "@NsConstr"
        "EnumC",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_EnumC",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "EnumC"),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "enumC",
          typedefType = TypeEnum
            (DeclPathName
              (CName "enumC")
              DeclPathCtxtTop),
          typedefSourceLoc =
          "enums.h:29:20"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "EnumC"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "EnumD",
      newtypeConstr = HsName
        "@NsConstr"
        "EnumD",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_EnumD",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "enumD")
            DeclPathCtxtTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "D_FOO",
              valueValue = 0,
              valueSourceLoc =
              "enums.h:31:14"},
            EnumValue {
              valueName = CName "D_BAR",
              valueValue = 1,
              valueSourceLoc =
              "enums.h:31:21"}],
          enumSourceLoc =
          "enums.h:31:6"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "EnumD",
        structConstr = HsName
          "@NsConstr"
          "EnumD",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_EnumD",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "enumD")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "D_FOO",
                valueValue = 0,
                valueSourceLoc =
                "enums.h:31:14"},
              EnumValue {
                valueName = CName "D_BAR",
                valueValue = 1,
                valueSourceLoc =
                "enums.h:31:21"}],
            enumSourceLoc = "enums.h:31:6"}}
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
                  "EnumD",
                structConstr = HsName
                  "@NsConstr"
                  "EnumD",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumD",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "enumD")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "D_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "enums.h:31:14"},
                      EnumValue {
                        valueName = CName "D_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:31:21"}],
                    enumSourceLoc =
                    "enums.h:31:6"}})
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
                  "EnumD",
                structConstr = HsName
                  "@NsConstr"
                  "EnumD",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_EnumD",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "enumD")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "D_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "enums.h:31:14"},
                      EnumValue {
                        valueName = CName "D_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "enums.h:31:21"}],
                    enumSourceLoc = "enums.h:31:6"}}
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
      "EnumD"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "EnumD"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "EnumD"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "EnumD"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName
      "@NsTypeConstr"
      "EnumD"),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "D_FOO",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumD",
      patSynConstr = HsName
        "@NsConstr"
        "EnumD",
      patSynValue = 0,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "D_FOO",
          valueValue = 0,
          valueSourceLoc =
          "enums.h:31:14"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "D_BAR",
      patSynType = HsName
        "@NsTypeConstr"
        "EnumD",
      patSynConstr = HsName
        "@NsConstr"
        "EnumD",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "D_BAR",
          valueValue = 1,
          valueSourceLoc =
          "enums.h:31:21"}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "EnumD_t",
      newtypeConstr = HsName
        "@NsConstr"
        "EnumD_t",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_EnumD_t",
        fieldType = HsTypRef
          (HsName
            "@NsTypeConstr"
            "EnumD"),
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "enumD_t",
          typedefType = TypeEnum
            (DeclPathName
              (CName "enumD")
              DeclPathCtxtTop),
          typedefSourceLoc =
          "enums.h:32:20"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "EnumD_t")]
