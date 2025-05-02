[
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
          enumDeclPath = DeclPathName
            (CName "enumA"),
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "VALA_1",
              valueValue = 0,
              valueSourceLoc =
              "nested_enums.h:3:17"},
            EnumValue {
              valueName = CName "VALA_2",
              valueValue = 1,
              valueSourceLoc =
              "nested_enums.h:4:17"}],
          enumSourceLoc =
          "nested_enums.h:2:14"}},
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
            enumDeclPath = DeclPathName
              (CName "enumA"),
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "VALA_1",
                valueValue = 0,
                valueSourceLoc =
                "nested_enums.h:3:17"},
              EnumValue {
                valueName = CName "VALA_2",
                valueValue = 1,
                valueSourceLoc =
                "nested_enums.h:4:17"}],
            enumSourceLoc =
            "nested_enums.h:2:14"}}
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
                    enumDeclPath = DeclPathName
                      (CName "enumA"),
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "VALA_1",
                        valueValue = 0,
                        valueSourceLoc =
                        "nested_enums.h:3:17"},
                      EnumValue {
                        valueName = CName "VALA_2",
                        valueValue = 1,
                        valueSourceLoc =
                        "nested_enums.h:4:17"}],
                    enumSourceLoc =
                    "nested_enums.h:2:14"}})
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
                    enumDeclPath = DeclPathName
                      (CName "enumA"),
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "VALA_1",
                        valueValue = 0,
                        valueSourceLoc =
                        "nested_enums.h:3:17"},
                      EnumValue {
                        valueName = CName "VALA_2",
                        valueValue = 1,
                        valueSourceLoc =
                        "nested_enums.h:4:17"}],
                    enumSourceLoc =
                    "nested_enums.h:2:14"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
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
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "EnumA"),
  DeclInstance
    (InstanceCEnum
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
            enumDeclPath = DeclPathName
              (CName "enumA"),
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "VALA_1",
                valueValue = 0,
                valueSourceLoc =
                "nested_enums.h:3:17"},
              EnumValue {
                valueName = CName "VALA_2",
                valueValue = 1,
                valueSourceLoc =
                "nested_enums.h:4:17"}],
            enumSourceLoc =
            "nested_enums.h:2:14"}}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 0 (NE.fromList ["VALA_1"]),
          _×_ 1 (NE.fromList ["VALA_2"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
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
            enumDeclPath = DeclPathName
              (CName "enumA"),
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "VALA_1",
                valueValue = 0,
                valueSourceLoc =
                "nested_enums.h:3:17"},
              EnumValue {
                valueName = CName "VALA_2",
                valueValue = 1,
                valueSourceLoc =
                "nested_enums.h:4:17"}],
            enumSourceLoc =
            "nested_enums.h:2:14"}}
      (HsName "@NsConstr" "VALA_1")
      (HsName "@NsConstr" "VALA_2")),
  DeclInstance
    (InstanceCEnumShow
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
            enumDeclPath = DeclPathName
              (CName "enumA"),
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "VALA_1",
                valueValue = 0,
                valueSourceLoc =
                "nested_enums.h:3:17"},
              EnumValue {
                valueName = CName "VALA_2",
                valueValue = 1,
                valueSourceLoc =
                "nested_enums.h:4:17"}],
            enumSourceLoc =
            "nested_enums.h:2:14"}}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "VALA_1",
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
          valueName = CName "VALA_1",
          valueValue = 0,
          valueSourceLoc =
          "nested_enums.h:3:17"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "VALA_2",
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
          valueName = CName "VALA_2",
          valueValue = 1,
          valueSourceLoc =
          "nested_enums.h:4:17"}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "ExA",
      structConstr = HsName
        "@NsConstr"
        "ExA",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "exA_fieldA1",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "EnumA"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "fieldA1",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeEnum
                (DeclPathName (CName "enumA")),
              fieldSourceLoc =
              "nested_enums.h:5:11"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "exA"),
          structAliases = [],
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "fieldA1",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeEnum
                (DeclPathName (CName "enumA")),
              fieldSourceLoc =
              "nested_enums.h:5:11"}],
          structFlam = Nothing,
          structSourceLoc =
          "nested_enums.h:1:8"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "ExA",
        structConstr = HsName
          "@NsConstr"
          "ExA",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "exA_fieldA1",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "EnumA"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "fieldA1",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeEnum
                  (DeclPathName (CName "enumA")),
                fieldSourceLoc =
                "nested_enums.h:5:11"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "exA"),
            structAliases = [],
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "fieldA1",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeEnum
                  (DeclPathName (CName "enumA")),
                fieldSourceLoc =
                "nested_enums.h:5:11"}],
            structFlam = Nothing,
            structSourceLoc =
            "nested_enums.h:1:8"}}
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
                  "ExA",
                structConstr = HsName
                  "@NsConstr"
                  "ExA",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exA_fieldA1",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "EnumA"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "fieldA1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeEnum
                          (DeclPathName (CName "enumA")),
                        fieldSourceLoc =
                        "nested_enums.h:5:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "exA"),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "fieldA1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeEnum
                          (DeclPathName (CName "enumA")),
                        fieldSourceLoc =
                        "nested_enums.h:5:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_enums.h:1:8"}})
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
                  "ExA",
                structConstr = HsName
                  "@NsConstr"
                  "ExA",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exA_fieldA1",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "EnumA"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "fieldA1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeEnum
                          (DeclPathName (CName "enumA")),
                        fieldSourceLoc =
                        "nested_enums.h:5:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "exA"),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "fieldA1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeEnum
                          (DeclPathName (CName "enumA")),
                        fieldSourceLoc =
                        "nested_enums.h:5:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_enums.h:1:8"}}
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
    (HsName "@NsTypeConstr" "ExA"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "ExA"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "ExB_fieldB1",
      newtypeConstr = HsName
        "@NsConstr"
        "ExB_fieldB1",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_ExB_fieldB1",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathAnon
            (DeclPathCtxtField
              (Just (CName "exB"))
              (CName "fieldB1")
              DeclPathCtxtTop),
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "VALB_1",
              valueValue = 0,
              valueSourceLoc =
              "nested_enums.h:10:17"},
            EnumValue {
              valueName = CName "VALB_2",
              valueValue = 1,
              valueSourceLoc =
              "nested_enums.h:11:17"}],
          enumSourceLoc =
          "nested_enums.h:9:9"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "ExB_fieldB1",
        structConstr = HsName
          "@NsConstr"
          "ExB_fieldB1",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_ExB_fieldB1",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathAnon
              (DeclPathCtxtField
                (Just (CName "exB"))
                (CName "fieldB1")
                DeclPathCtxtTop),
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "VALB_1",
                valueValue = 0,
                valueSourceLoc =
                "nested_enums.h:10:17"},
              EnumValue {
                valueName = CName "VALB_2",
                valueValue = 1,
                valueSourceLoc =
                "nested_enums.h:11:17"}],
            enumSourceLoc =
            "nested_enums.h:9:9"}}
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
                  "ExB_fieldB1",
                structConstr = HsName
                  "@NsConstr"
                  "ExB_fieldB1",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_ExB_fieldB1",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathAnon
                      (DeclPathCtxtField
                        (Just (CName "exB"))
                        (CName "fieldB1")
                        DeclPathCtxtTop),
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "VALB_1",
                        valueValue = 0,
                        valueSourceLoc =
                        "nested_enums.h:10:17"},
                      EnumValue {
                        valueName = CName "VALB_2",
                        valueValue = 1,
                        valueSourceLoc =
                        "nested_enums.h:11:17"}],
                    enumSourceLoc =
                    "nested_enums.h:9:9"}})
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
                  "ExB_fieldB1",
                structConstr = HsName
                  "@NsConstr"
                  "ExB_fieldB1",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_ExB_fieldB1",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathAnon
                      (DeclPathCtxtField
                        (Just (CName "exB"))
                        (CName "fieldB1")
                        DeclPathCtxtTop),
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "VALB_1",
                        valueValue = 0,
                        valueSourceLoc =
                        "nested_enums.h:10:17"},
                      EnumValue {
                        valueName = CName "VALB_2",
                        valueValue = 1,
                        valueSourceLoc =
                        "nested_enums.h:11:17"}],
                    enumSourceLoc =
                    "nested_enums.h:9:9"}}
              (Add 1)
              (Seq
                [
                  PokeByteOff
                    (Idx 2)
                    0
                    (Idx 0)])))}),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "ExB_fieldB1"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "ExB_fieldB1"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName
      "@NsTypeConstr"
      "ExB_fieldB1"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "ExB_fieldB1",
        structConstr = HsName
          "@NsConstr"
          "ExB_fieldB1",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_ExB_fieldB1",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathAnon
              (DeclPathCtxtField
                (Just (CName "exB"))
                (CName "fieldB1")
                DeclPathCtxtTop),
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "VALB_1",
                valueValue = 0,
                valueSourceLoc =
                "nested_enums.h:10:17"},
              EnumValue {
                valueName = CName "VALB_2",
                valueValue = 1,
                valueSourceLoc =
                "nested_enums.h:11:17"}],
            enumSourceLoc =
            "nested_enums.h:9:9"}}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 0 (NE.fromList ["VALB_1"]),
          _×_ 1 (NE.fromList ["VALB_2"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "ExB_fieldB1",
        structConstr = HsName
          "@NsConstr"
          "ExB_fieldB1",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_ExB_fieldB1",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathAnon
              (DeclPathCtxtField
                (Just (CName "exB"))
                (CName "fieldB1")
                DeclPathCtxtTop),
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "VALB_1",
                valueValue = 0,
                valueSourceLoc =
                "nested_enums.h:10:17"},
              EnumValue {
                valueName = CName "VALB_2",
                valueValue = 1,
                valueSourceLoc =
                "nested_enums.h:11:17"}],
            enumSourceLoc =
            "nested_enums.h:9:9"}}
      (HsName "@NsConstr" "VALB_1")
      (HsName "@NsConstr" "VALB_2")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "ExB_fieldB1",
        structConstr = HsName
          "@NsConstr"
          "ExB_fieldB1",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_ExB_fieldB1",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathAnon
              (DeclPathCtxtField
                (Just (CName "exB"))
                (CName "fieldB1")
                DeclPathCtxtTop),
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "VALB_1",
                valueValue = 0,
                valueSourceLoc =
                "nested_enums.h:10:17"},
              EnumValue {
                valueName = CName "VALB_2",
                valueValue = 1,
                valueSourceLoc =
                "nested_enums.h:11:17"}],
            enumSourceLoc =
            "nested_enums.h:9:9"}}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "VALB_1",
      patSynType = HsName
        "@NsTypeConstr"
        "ExB_fieldB1",
      patSynConstr = HsName
        "@NsConstr"
        "ExB_fieldB1",
      patSynValue = 0,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "VALB_1",
          valueValue = 0,
          valueSourceLoc =
          "nested_enums.h:10:17"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "VALB_2",
      patSynType = HsName
        "@NsTypeConstr"
        "ExB_fieldB1",
      patSynConstr = HsName
        "@NsConstr"
        "ExB_fieldB1",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "VALB_2",
          valueValue = 1,
          valueSourceLoc =
          "nested_enums.h:11:17"}},
  DeclData
    Struct {
      structName = HsName
        "@NsTypeConstr"
        "ExB",
      structConstr = HsName
        "@NsConstr"
        "ExB",
      structFields = [
        Field {
          fieldName = HsName
            "@NsVar"
            "exB_fieldB1",
          fieldType = HsTypRef
            (HsName
              "@NsTypeConstr"
              "ExB_fieldB1"),
          fieldOrigin =
          FieldOriginStructField
            StructField {
              fieldName = CName "fieldB1",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeEnum
                (DeclPathAnon
                  (DeclPathCtxtField
                    (Just (CName "exB"))
                    (CName "fieldB1")
                    DeclPathCtxtTop)),
              fieldSourceLoc =
              "nested_enums.h:12:11"}}],
      structOrigin =
      StructOriginStruct
        Struct {
          structDeclPath = DeclPathName
            (CName "exB"),
          structAliases = [],
          structSizeof = 4,
          structAlignment = 4,
          structFields = [
            StructField {
              fieldName = CName "fieldB1",
              fieldOffset = 0,
              fieldWidth = Nothing,
              fieldType = TypeEnum
                (DeclPathAnon
                  (DeclPathCtxtField
                    (Just (CName "exB"))
                    (CName "fieldB1")
                    DeclPathCtxtTop)),
              fieldSourceLoc =
              "nested_enums.h:12:11"}],
          structFlam = Nothing,
          structSourceLoc =
          "nested_enums.h:8:8"}},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "ExB",
        structConstr = HsName
          "@NsConstr"
          "ExB",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "exB_fieldB1",
            fieldType = HsTypRef
              (HsName
                "@NsTypeConstr"
                "ExB_fieldB1"),
            fieldOrigin =
            FieldOriginStructField
              StructField {
                fieldName = CName "fieldB1",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeEnum
                  (DeclPathAnon
                    (DeclPathCtxtField
                      (Just (CName "exB"))
                      (CName "fieldB1")
                      DeclPathCtxtTop)),
                fieldSourceLoc =
                "nested_enums.h:12:11"}}],
        structOrigin =
        StructOriginStruct
          Struct {
            structDeclPath = DeclPathName
              (CName "exB"),
            structAliases = [],
            structSizeof = 4,
            structAlignment = 4,
            structFields = [
              StructField {
                fieldName = CName "fieldB1",
                fieldOffset = 0,
                fieldWidth = Nothing,
                fieldType = TypeEnum
                  (DeclPathAnon
                    (DeclPathCtxtField
                      (Just (CName "exB"))
                      (CName "fieldB1")
                      DeclPathCtxtTop)),
                fieldSourceLoc =
                "nested_enums.h:12:11"}],
            structFlam = Nothing,
            structSourceLoc =
            "nested_enums.h:8:8"}}
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
                  "ExB",
                structConstr = HsName
                  "@NsConstr"
                  "ExB",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exB_fieldB1",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "ExB_fieldB1"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "fieldB1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeEnum
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "exB"))
                              (CName "fieldB1")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "nested_enums.h:12:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "exB"),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "fieldB1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeEnum
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "exB"))
                              (CName "fieldB1")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "nested_enums.h:12:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_enums.h:8:8"}})
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
                  "ExB",
                structConstr = HsName
                  "@NsConstr"
                  "ExB",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "exB_fieldB1",
                    fieldType = HsTypRef
                      (HsName
                        "@NsTypeConstr"
                        "ExB_fieldB1"),
                    fieldOrigin =
                    FieldOriginStructField
                      StructField {
                        fieldName = CName "fieldB1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeEnum
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "exB"))
                              (CName "fieldB1")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "nested_enums.h:12:11"}}],
                structOrigin =
                StructOriginStruct
                  Struct {
                    structDeclPath = DeclPathName
                      (CName "exB"),
                    structAliases = [],
                    structSizeof = 4,
                    structAlignment = 4,
                    structFields = [
                      StructField {
                        fieldName = CName "fieldB1",
                        fieldOffset = 0,
                        fieldWidth = Nothing,
                        fieldType = TypeEnum
                          (DeclPathAnon
                            (DeclPathCtxtField
                              (Just (CName "exB"))
                              (CName "fieldB1")
                              DeclPathCtxtTop)),
                        fieldSourceLoc =
                        "nested_enums.h:12:11"}],
                    structFlam = Nothing,
                    structSourceLoc =
                    "nested_enums.h:8:8"}}
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
    (HsName "@NsTypeConstr" "ExB"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "ExB")]
