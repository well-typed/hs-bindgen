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
          "unFirst",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumTag = CName "first",
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "FIRST1",
              valueValue = 0,
              valueSourceLoc =
              "examples/enums.h:5:5"},
            EnumValue {
              valueName = CName "FIRST2",
              valueValue = 1,
              valueSourceLoc =
              "examples/enums.h:6:5"}],
          enumSourceLoc =
          "examples/enums.h:4:6"}},
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
              "unFirst",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumTag = CName "first",
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "FIRST1",
                valueValue = 0,
                valueSourceLoc =
                "examples/enums.h:5:5"},
              EnumValue {
                valueName = CName "FIRST2",
                valueValue = 1,
                valueSourceLoc =
                "examples/enums.h:6:5"}],
            enumSourceLoc =
            "examples/enums.h:4:6"}}
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
                      "unFirst",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "first",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "FIRST1",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/enums.h:5:5"},
                      EnumValue {
                        valueName = CName "FIRST2",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:6:5"}],
                    enumSourceLoc =
                    "examples/enums.h:4:6"}})
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
                      "unFirst",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "first",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "FIRST1",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/enums.h:5:5"},
                      EnumValue {
                        valueName = CName "FIRST2",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:6:5"}],
                    enumSourceLoc =
                    "examples/enums.h:4:6"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
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
          "examples/enums.h:5:5"}},
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
          "examples/enums.h:6:5"}},
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
          "unSecond",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumTag = CName "second",
          enumType = TypePrim
            (PrimIntegral PrimInt Signed),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "SECOND_A",
              valueValue = `-1`,
              valueSourceLoc =
              "examples/enums.h:10:5"},
            EnumValue {
              valueName = CName "SECOND_B",
              valueValue = 0,
              valueSourceLoc =
              "examples/enums.h:11:5"},
            EnumValue {
              valueName = CName "SECOND_C",
              valueValue = 1,
              valueSourceLoc =
              "examples/enums.h:12:5"}],
          enumSourceLoc =
          "examples/enums.h:9:6"}},
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
              "unSecond",
            fieldType = HsPrimType
              HsPrimCInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumTag = CName "second",
            enumType = TypePrim
              (PrimIntegral PrimInt Signed),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "SECOND_A",
                valueValue = `-1`,
                valueSourceLoc =
                "examples/enums.h:10:5"},
              EnumValue {
                valueName = CName "SECOND_B",
                valueValue = 0,
                valueSourceLoc =
                "examples/enums.h:11:5"},
              EnumValue {
                valueName = CName "SECOND_C",
                valueValue = 1,
                valueSourceLoc =
                "examples/enums.h:12:5"}],
            enumSourceLoc =
            "examples/enums.h:9:6"}}
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
                      "unSecond",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "second",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "SECOND_A",
                        valueValue = `-1`,
                        valueSourceLoc =
                        "examples/enums.h:10:5"},
                      EnumValue {
                        valueName = CName "SECOND_B",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/enums.h:11:5"},
                      EnumValue {
                        valueName = CName "SECOND_C",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:12:5"}],
                    enumSourceLoc =
                    "examples/enums.h:9:6"}})
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
                      "unSecond",
                    fieldType = HsPrimType
                      HsPrimCInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "second",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Signed),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "SECOND_A",
                        valueValue = `-1`,
                        valueSourceLoc =
                        "examples/enums.h:10:5"},
                      EnumValue {
                        valueName = CName "SECOND_B",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/enums.h:11:5"},
                      EnumValue {
                        valueName = CName "SECOND_C",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:12:5"}],
                    enumSourceLoc =
                    "examples/enums.h:9:6"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
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
          "examples/enums.h:10:5"}},
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
          "examples/enums.h:11:5"}},
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
          "examples/enums.h:12:5"}},
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
          "unSame",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumTag = CName "same",
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "SAME_A",
              valueValue = 1,
              valueSourceLoc =
              "examples/enums.h:16:5"},
            EnumValue {
              valueName = CName "SAME_B",
              valueValue = 1,
              valueSourceLoc =
              "examples/enums.h:17:5"}],
          enumSourceLoc =
          "examples/enums.h:15:6"}},
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
              "unSame",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumTag = CName "same",
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "SAME_A",
                valueValue = 1,
                valueSourceLoc =
                "examples/enums.h:16:5"},
              EnumValue {
                valueName = CName "SAME_B",
                valueValue = 1,
                valueSourceLoc =
                "examples/enums.h:17:5"}],
            enumSourceLoc =
            "examples/enums.h:15:6"}}
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
                      "unSame",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "same",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "SAME_A",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:16:5"},
                      EnumValue {
                        valueName = CName "SAME_B",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:17:5"}],
                    enumSourceLoc =
                    "examples/enums.h:15:6"}})
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
                      "unSame",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "same",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "SAME_A",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:16:5"},
                      EnumValue {
                        valueName = CName "SAME_B",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:17:5"}],
                    enumSourceLoc =
                    "examples/enums.h:15:6"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
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
          "examples/enums.h:16:5"}},
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
          "examples/enums.h:17:5"}},
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
          "unPackad",
        fieldType = HsPrimType
          HsPrimCSChar,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumTag = CName "packad",
          enumType = TypePrim
            (PrimChar (Just Unsigned)),
          enumSizeof = 1,
          enumAlignment = 1,
          enumValues = [
            EnumValue {
              valueName = CName "PACKED_A",
              valueValue = 0,
              valueSourceLoc =
              "examples/enums.h:21:5"},
            EnumValue {
              valueName = CName "PACKED_B",
              valueValue = 1,
              valueSourceLoc =
              "examples/enums.h:21:15"},
            EnumValue {
              valueName = CName "PACKED_C",
              valueValue = 2,
              valueSourceLoc =
              "examples/enums.h:21:25"}],
          enumSourceLoc =
          "examples/enums.h:20:6"}},
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
              "unPackad",
            fieldType = HsPrimType
              HsPrimCSChar,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumTag = CName "packad",
            enumType = TypePrim
              (PrimChar (Just Unsigned)),
            enumSizeof = 1,
            enumAlignment = 1,
            enumValues = [
              EnumValue {
                valueName = CName "PACKED_A",
                valueValue = 0,
                valueSourceLoc =
                "examples/enums.h:21:5"},
              EnumValue {
                valueName = CName "PACKED_B",
                valueValue = 1,
                valueSourceLoc =
                "examples/enums.h:21:15"},
              EnumValue {
                valueName = CName "PACKED_C",
                valueValue = 2,
                valueSourceLoc =
                "examples/enums.h:21:25"}],
            enumSourceLoc =
            "examples/enums.h:20:6"}}
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
                      "unPackad",
                    fieldType = HsPrimType
                      HsPrimCSChar,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "packad",
                    enumType = TypePrim
                      (PrimChar (Just Unsigned)),
                    enumSizeof = 1,
                    enumAlignment = 1,
                    enumValues = [
                      EnumValue {
                        valueName = CName "PACKED_A",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/enums.h:21:5"},
                      EnumValue {
                        valueName = CName "PACKED_B",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:21:15"},
                      EnumValue {
                        valueName = CName "PACKED_C",
                        valueValue = 2,
                        valueSourceLoc =
                        "examples/enums.h:21:25"}],
                    enumSourceLoc =
                    "examples/enums.h:20:6"}})
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
                      "unPackad",
                    fieldType = HsPrimType
                      HsPrimCSChar,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "packad",
                    enumType = TypePrim
                      (PrimChar (Just Unsigned)),
                    enumSizeof = 1,
                    enumAlignment = 1,
                    enumValues = [
                      EnumValue {
                        valueName = CName "PACKED_A",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/enums.h:21:5"},
                      EnumValue {
                        valueName = CName "PACKED_B",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:21:15"},
                      EnumValue {
                        valueName = CName "PACKED_C",
                        valueValue = 2,
                        valueSourceLoc =
                        "examples/enums.h:21:25"}],
                    enumSourceLoc =
                    "examples/enums.h:20:6"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
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
          "examples/enums.h:21:5"}},
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
          "examples/enums.h:21:15"}},
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
          "examples/enums.h:21:25"}},
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
          "unEnumA",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumTag = CName "enumA",
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "A_FOO",
              valueValue = 0,
              valueSourceLoc =
              "examples/enums.h:24:16"},
            EnumValue {
              valueName = CName "A_BAR",
              valueValue = 1,
              valueSourceLoc =
              "examples/enums.h:24:23"}],
          enumSourceLoc =
          "examples/enums.h:24:9"}},
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
              "unEnumA",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumTag = CName "enumA",
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "A_FOO",
                valueValue = 0,
                valueSourceLoc =
                "examples/enums.h:24:16"},
              EnumValue {
                valueName = CName "A_BAR",
                valueValue = 1,
                valueSourceLoc =
                "examples/enums.h:24:23"}],
            enumSourceLoc =
            "examples/enums.h:24:9"}}
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
                      "unEnumA",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "enumA",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "A_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/enums.h:24:16"},
                      EnumValue {
                        valueName = CName "A_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:24:23"}],
                    enumSourceLoc =
                    "examples/enums.h:24:9"}})
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
                      "unEnumA",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "enumA",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "A_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/enums.h:24:16"},
                      EnumValue {
                        valueName = CName "A_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:24:23"}],
                    enumSourceLoc =
                    "examples/enums.h:24:9"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
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
          "examples/enums.h:24:16"}},
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
          "examples/enums.h:24:23"}},
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
          "unEnumB",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumTag = CName "enumB",
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "B_FOO",
              valueValue = 0,
              valueSourceLoc =
              "examples/enums.h:26:22"},
            EnumValue {
              valueName = CName "B_BAR",
              valueValue = 1,
              valueSourceLoc =
              "examples/enums.h:26:29"}],
          enumSourceLoc =
          "examples/enums.h:26:14"}},
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
              "unEnumB",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumTag = CName "enumB",
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "B_FOO",
                valueValue = 0,
                valueSourceLoc =
                "examples/enums.h:26:22"},
              EnumValue {
                valueName = CName "B_BAR",
                valueValue = 1,
                valueSourceLoc =
                "examples/enums.h:26:29"}],
            enumSourceLoc =
            "examples/enums.h:26:14"}}
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
                      "unEnumB",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "enumB",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "B_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/enums.h:26:22"},
                      EnumValue {
                        valueName = CName "B_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:26:29"}],
                    enumSourceLoc =
                    "examples/enums.h:26:14"}})
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
                      "unEnumB",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "enumB",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "B_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/enums.h:26:22"},
                      EnumValue {
                        valueName = CName "B_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:26:29"}],
                    enumSourceLoc =
                    "examples/enums.h:26:14"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
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
          "examples/enums.h:26:22"}},
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
          "examples/enums.h:26:29"}},
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
          "unEnumC",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumTag = CName "enumC",
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "C_FOO",
              valueValue = 0,
              valueSourceLoc =
              "examples/enums.h:28:14"},
            EnumValue {
              valueName = CName "C_BAR",
              valueValue = 1,
              valueSourceLoc =
              "examples/enums.h:28:21"}],
          enumSourceLoc =
          "examples/enums.h:28:6"}},
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
              "unEnumC",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumTag = CName "enumC",
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "C_FOO",
                valueValue = 0,
                valueSourceLoc =
                "examples/enums.h:28:14"},
              EnumValue {
                valueName = CName "C_BAR",
                valueValue = 1,
                valueSourceLoc =
                "examples/enums.h:28:21"}],
            enumSourceLoc =
            "examples/enums.h:28:6"}}
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
                      "unEnumC",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "enumC",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "C_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/enums.h:28:14"},
                      EnumValue {
                        valueName = CName "C_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:28:21"}],
                    enumSourceLoc =
                    "examples/enums.h:28:6"}})
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
                      "unEnumC",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "enumC",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "C_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/enums.h:28:14"},
                      EnumValue {
                        valueName = CName "C_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:28:21"}],
                    enumSourceLoc =
                    "examples/enums.h:28:6"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
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
          "examples/enums.h:28:14"}},
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
          "examples/enums.h:28:21"}},
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
          "unEnumD",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumTag = CName "enumD",
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "D_FOO",
              valueValue = 0,
              valueSourceLoc =
              "examples/enums.h:31:14"},
            EnumValue {
              valueName = CName "D_BAR",
              valueValue = 1,
              valueSourceLoc =
              "examples/enums.h:31:21"}],
          enumSourceLoc =
          "examples/enums.h:31:6"}},
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
              "unEnumD",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumTag = CName "enumD",
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "D_FOO",
                valueValue = 0,
                valueSourceLoc =
                "examples/enums.h:31:14"},
              EnumValue {
                valueName = CName "D_BAR",
                valueValue = 1,
                valueSourceLoc =
                "examples/enums.h:31:21"}],
            enumSourceLoc =
            "examples/enums.h:31:6"}}
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
                      "unEnumD",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "enumD",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "D_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/enums.h:31:14"},
                      EnumValue {
                        valueName = CName "D_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:31:21"}],
                    enumSourceLoc =
                    "examples/enums.h:31:6"}})
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
                      "unEnumD",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "enumD",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "D_FOO",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/enums.h:31:14"},
                      EnumValue {
                        valueName = CName "D_BAR",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/enums.h:31:21"}],
                    enumSourceLoc =
                    "examples/enums.h:31:6"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
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
          "examples/enums.h:31:14"}},
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
          "examples/enums.h:31:21"}},
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
          "unEnumD_t",
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
            (CName "enumD"),
          typedefSourceLoc =
          "examples/enums.h:32:20"}},
  DeclNewtypeInstance
    Storable
    (HsName
      "@NsTypeConstr"
      "EnumD_t")]
