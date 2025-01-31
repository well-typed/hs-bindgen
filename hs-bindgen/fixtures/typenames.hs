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
          "unFoo",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumTag = CName "foo",
          enumType = TypePrim
            (PrimIntegral PrimInt Unsigned),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName "FOO1",
              valueValue = 0,
              valueSourceLoc =
              "examples/typenames.h:15:2"},
            EnumValue {
              valueName = CName "FOO2",
              valueValue = 1,
              valueSourceLoc =
              "examples/typenames.h:16:2"}],
          enumSourceLoc =
          "examples/typenames.h:14:6"}},
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
              "unFoo",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumTag = CName "foo",
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName "FOO1",
                valueValue = 0,
                valueSourceLoc =
                "examples/typenames.h:15:2"},
              EnumValue {
                valueName = CName "FOO2",
                valueValue = 1,
                valueSourceLoc =
                "examples/typenames.h:16:2"}],
            enumSourceLoc =
            "examples/typenames.h:14:6"}}
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
                      "unFoo",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "foo",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "FOO1",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/typenames.h:15:2"},
                      EnumValue {
                        valueName = CName "FOO2",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/typenames.h:16:2"}],
                    enumSourceLoc =
                    "examples/typenames.h:14:6"}})
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
                      "unFoo",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "foo",
                    enumType = TypePrim
                      (PrimIntegral PrimInt Unsigned),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName "FOO1",
                        valueValue = 0,
                        valueSourceLoc =
                        "examples/typenames.h:15:2"},
                      EnumValue {
                        valueName = CName "FOO2",
                        valueValue = 1,
                        valueSourceLoc =
                        "examples/typenames.h:16:2"}],
                    enumSourceLoc =
                    "examples/typenames.h:14:6"}}
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
    Read
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
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "Foo"),
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
          "examples/typenames.h:15:2"}},
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
          "examples/typenames.h:16:2"}},
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
          "unFoo",
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
          "examples/typenames.h:19:16"}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "Foo")]
