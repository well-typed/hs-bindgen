[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "MyEnum",
      newtypeConstr = HsName
        "@NsConstr"
        "MyEnum",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_MyEnum",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "MyEnum"),
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName
                "Say\20320\22909",
              valueValue = 0,
              valueSourceLoc =
              "uses_utf8.h:5:9"},
            EnumValue {
              valueName = CName
                "Say\25308\25308",
              valueValue = 1,
              valueSourceLoc =
              "uses_utf8.h:6:9"}],
          enumSourceLoc =
          "uses_utf8.h:4:6",
          enumTypeSpec = Nothing},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Read,
          Show,
          Storable]},
  DeclInstance
    (InstanceStorable
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "MyEnum",
        structConstr = HsName
          "@NsConstr"
          "MyEnum",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_MyEnum",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "MyEnum"),
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName
                  "Say\20320\22909",
                valueValue = 0,
                valueSourceLoc =
                "uses_utf8.h:5:9"},
              EnumValue {
                valueName = CName
                  "Say\25308\25308",
                valueValue = 1,
                valueSourceLoc =
                "uses_utf8.h:6:9"}],
            enumSourceLoc =
            "uses_utf8.h:4:6",
            enumTypeSpec = Nothing},
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
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
                  "MyEnum",
                structConstr = HsName
                  "@NsConstr"
                  "MyEnum",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_MyEnum",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "MyEnum"),
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName
                          "Say\20320\22909",
                        valueValue = 0,
                        valueSourceLoc =
                        "uses_utf8.h:5:9"},
                      EnumValue {
                        valueName = CName
                          "Say\25308\25308",
                        valueValue = 1,
                        valueSourceLoc =
                        "uses_utf8.h:6:9"}],
                    enumSourceLoc =
                    "uses_utf8.h:4:6",
                    enumTypeSpec = Nothing},
                structInstances = Set.fromList
                  [
                    Eq,
                    Ord,
                    Read,
                    Show,
                    Storable]})
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
                  "MyEnum",
                structConstr = HsName
                  "@NsConstr"
                  "MyEnum",
                structFields = [
                  Field {
                    fieldName = HsName
                      "@NsVar"
                      "un_MyEnum",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "MyEnum"),
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName
                          "Say\20320\22909",
                        valueValue = 0,
                        valueSourceLoc =
                        "uses_utf8.h:5:9"},
                      EnumValue {
                        valueName = CName
                          "Say\25308\25308",
                        valueValue = 1,
                        valueSourceLoc =
                        "uses_utf8.h:6:9"}],
                    enumSourceLoc =
                    "uses_utf8.h:4:6",
                    enumTypeSpec = Nothing},
                structInstances = Set.fromList
                  [Eq, Ord, Read, Show, Storable]}
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
      "MyEnum"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName
      "@NsTypeConstr"
      "MyEnum"),
  DeclInstance
    (InstanceCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "MyEnum",
        structConstr = HsName
          "@NsConstr"
          "MyEnum",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_MyEnum",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "MyEnum"),
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName
                  "Say\20320\22909",
                valueValue = 0,
                valueSourceLoc =
                "uses_utf8.h:5:9"},
              EnumValue {
                valueName = CName
                  "Say\25308\25308",
                valueValue = 1,
                valueSourceLoc =
                "uses_utf8.h:6:9"}],
            enumSourceLoc =
            "uses_utf8.h:4:6",
            enumTypeSpec = Nothing},
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_
            0
            (NE.fromList
              ["Say\20320\22909"]),
          _×_
            1
            (NE.fromList
              ["Say\25308\25308"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "MyEnum",
        structConstr = HsName
          "@NsConstr"
          "MyEnum",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_MyEnum",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "MyEnum"),
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName
                  "Say\20320\22909",
                valueValue = 0,
                valueSourceLoc =
                "uses_utf8.h:5:9"},
              EnumValue {
                valueName = CName
                  "Say\25308\25308",
                valueValue = 1,
                valueSourceLoc =
                "uses_utf8.h:6:9"}],
            enumSourceLoc =
            "uses_utf8.h:4:6",
            enumTypeSpec = Nothing},
        structInstances = Set.fromList
          [Eq, Ord, Read, Show, Storable]}
      (HsName
        "@NsConstr"
        "Say\20320\22909")
      (HsName
        "@NsConstr"
        "Say\25308\25308")),
  DeclInstance
    (InstanceCEnumShow
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "MyEnum",
        structConstr = HsName
          "@NsConstr"
          "MyEnum",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_MyEnum",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "MyEnum"),
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName
                  "Say\20320\22909",
                valueValue = 0,
                valueSourceLoc =
                "uses_utf8.h:5:9"},
              EnumValue {
                valueName = CName
                  "Say\25308\25308",
                valueValue = 1,
                valueSourceLoc =
                "uses_utf8.h:6:9"}],
            enumSourceLoc =
            "uses_utf8.h:4:6",
            enumTypeSpec = Nothing},
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclInstance
    (InstanceCEnumRead
      Struct {
        structName = HsName
          "@NsTypeConstr"
          "MyEnum",
        structConstr = HsName
          "@NsConstr"
          "MyEnum",
        structFields = [
          Field {
            fieldName = HsName
              "@NsVar"
              "un_MyEnum",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "MyEnum"),
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName
                  "Say\20320\22909",
                valueValue = 0,
                valueSourceLoc =
                "uses_utf8.h:5:9"},
              EnumValue {
                valueName = CName
                  "Say\25308\25308",
                valueValue = 1,
                valueSourceLoc =
                "uses_utf8.h:6:9"}],
            enumSourceLoc =
            "uses_utf8.h:4:6",
            enumTypeSpec = Nothing},
        structInstances = Set.fromList
          [
            Eq,
            Ord,
            Read,
            Show,
            Storable]}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Say\20320\22909",
      patSynType = HsName
        "@NsTypeConstr"
        "MyEnum",
      patSynConstr = HsName
        "@NsConstr"
        "MyEnum",
      patSynValue = 0,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "Say\20320\22909",
          valueValue = 0,
          valueSourceLoc =
          "uses_utf8.h:5:9"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "Say\25308\25308",
      patSynType = HsName
        "@NsTypeConstr"
        "MyEnum",
      patSynConstr = HsName
        "@NsConstr"
        "MyEnum",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName
            "Say\25308\25308",
          valueValue = 1,
          valueSourceLoc =
          "uses_utf8.h:6:9"}}]
