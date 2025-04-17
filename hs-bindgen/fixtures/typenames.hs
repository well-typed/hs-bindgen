[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Foo",
      newtypeConstr = HsName
        "@NsConstr"
        "Foo",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Foo",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumDeclPath = DeclPathName
            (CName "foo")
            DeclPathCtxtTop,
          enumAliases = [],
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "FOO1",
              valueValue = 0,
              valueSourceLoc =
              "typenames.h:15:2"},
            EnumValue {
              valueName = CName "FOO2",
              valueValue = 1,
              valueSourceLoc =
              "typenames.h:16:2"}],
          enumSourceLoc =
          "typenames.h:14:6"}},
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
              "un_Foo",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "foo")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "FOO1",
                valueValue = 0,
                valueSourceLoc =
                "typenames.h:15:2"},
              EnumValue {
                valueName = CName "FOO2",
                valueValue = 1,
                valueSourceLoc =
                "typenames.h:16:2"}],
            enumSourceLoc =
            "typenames.h:14:6"}}
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
                      "un_Foo",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "foo")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "FOO1",
                        valueValue = 0,
                        valueSourceLoc =
                        "typenames.h:15:2"},
                      EnumValue {
                        valueName = CName "FOO2",
                        valueValue = 1,
                        valueSourceLoc =
                        "typenames.h:16:2"}],
                    enumSourceLoc =
                    "typenames.h:14:6"}})
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
                      "un_Foo",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumDeclPath = DeclPathName
                      (CName "foo")
                      DeclPathCtxtTop,
                    enumAliases = [],
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "FOO1",
                        valueValue = 0,
                        valueSourceLoc =
                        "typenames.h:15:2"},
                      EnumValue {
                        valueName = CName "FOO2",
                        valueValue = 1,
                        valueSourceLoc =
                        "typenames.h:16:2"}],
                    enumSourceLoc =
                    "typenames.h:14:6"}}
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
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "Foo"),
  DeclInstance
    (InstanceCEnum
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
              "un_Foo",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "foo")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "FOO1",
                valueValue = 0,
                valueSourceLoc =
                "typenames.h:15:2"},
              EnumValue {
                valueName = CName "FOO2",
                valueValue = 1,
                valueSourceLoc =
                "typenames.h:16:2"}],
            enumSourceLoc =
            "typenames.h:14:6"}}
      (HsPrimType HsPrimCUInt)
      (Map.fromList
        [
          _×_ 0 (NE.fromList ["FOO1"]),
          _×_ 1 (NE.fromList ["FOO2"])])
      True),
  DeclInstance
    (InstanceSequentialCEnum
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
              "un_Foo",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "foo")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "FOO1",
                valueValue = 0,
                valueSourceLoc =
                "typenames.h:15:2"},
              EnumValue {
                valueName = CName "FOO2",
                valueValue = 1,
                valueSourceLoc =
                "typenames.h:16:2"}],
            enumSourceLoc =
            "typenames.h:14:6"}}
      (HsName "@NsConstr" "FOO1")
      (HsName "@NsConstr" "FOO2")),
  DeclInstance
    (InstanceCEnumShow
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
              "un_Foo",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumDeclPath = DeclPathName
              (CName "foo")
              DeclPathCtxtTop,
            enumAliases = [],
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "FOO1",
                valueValue = 0,
                valueSourceLoc =
                "typenames.h:15:2"},
              EnumValue {
                valueName = CName "FOO2",
                valueValue = 1,
                valueSourceLoc =
                "typenames.h:16:2"}],
            enumSourceLoc =
            "typenames.h:14:6"}}),
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "FOO1",
      patSynType = HsName
        "@NsTypeConstr"
        "Foo",
      patSynConstr = HsName
        "@NsConstr"
        "Foo",
      patSynValue = 0,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "FOO1",
          valueValue = 0,
          valueSourceLoc =
          "typenames.h:15:2"}},
  DeclPatSyn
    PatSyn {
      patSynName = HsName
        "@NsConstr"
        "FOO2",
      patSynType = HsName
        "@NsTypeConstr"
        "Foo",
      patSynConstr = HsName
        "@NsConstr"
        "Foo",
      patSynValue = 1,
      patSynOrigin =
      PatSynOriginEnumValue
        EnumValue {
          valueName = CName "FOO2",
          valueValue = 1,
          valueSourceLoc =
          "typenames.h:16:2"}},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Foo",
      newtypeConstr = HsName
        "@NsConstr"
        "Foo",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Foo",
        fieldType = HsPrimType
          HsPrimCDouble,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginTypedef
        Typedef {
          typedefName = CName "foo",
          typedefType = TypePrim
            (PrimFloating PrimDouble),
          typedefSourceLoc =
          "typenames.h:19:16"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveNewtype
    Floating
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveNewtype
    Fractional
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFloat
    (HsName "@NsTypeConstr" "Foo"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFrac
    (HsName "@NsTypeConstr" "Foo")]
