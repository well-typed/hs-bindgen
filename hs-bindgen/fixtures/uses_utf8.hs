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
          "unMyEnum",
        fieldType = HsPrimType
          HsPrimCUInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginEnum
        Enu {
          enumTag = CName "MyEnum",
          enumType = TypePrim
            (PrimIntegral
              (PrimInt Unsigned)),
          enumSizeof = 4,
          enumAlignment = 4,
          enumValues = [
            EnumValue {
              valueName = CName
                "Say\20320\22909",
              valueValue = 0,
              valueSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "uses_utf8.h"],
                singleLocLine = 5,
                singleLocColumn = 9}},
            EnumValue {
              valueName = CName
                "Say\25308\25308",
              valueValue = 1,
              valueSourceLoc = SingleLoc {
                singleLocPath = [
                  "examples",
                  "uses_utf8.h"],
                singleLocLine = 6,
                singleLocColumn = 9}}],
          enumSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "uses_utf8.h"],
            singleLocLine = 4,
            singleLocColumn = 6}}},
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
              "unMyEnum",
            fieldType = HsPrimType
              HsPrimCUInt,
            fieldOrigin = FieldOriginNone}],
        structOrigin = StructOriginEnum
          Enu {
            enumTag = CName "MyEnum",
            enumType = TypePrim
              (PrimIntegral
                (PrimInt Unsigned)),
            enumSizeof = 4,
            enumAlignment = 4,
            enumValues = [
              EnumValue {
                valueName = CName
                  "Say\20320\22909",
                valueValue = 0,
                valueSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "uses_utf8.h"],
                  singleLocLine = 5,
                  singleLocColumn = 9}},
              EnumValue {
                valueName = CName
                  "Say\25308\25308",
                valueValue = 1,
                valueSourceLoc = SingleLoc {
                  singleLocPath = [
                    "examples",
                    "uses_utf8.h"],
                  singleLocLine = 6,
                  singleLocColumn = 9}}],
            enumSourceLoc = SingleLoc {
              singleLocPath = [
                "examples",
                "uses_utf8.h"],
              singleLocLine = 4,
              singleLocColumn = 6}}}
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
                      "unMyEnum",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "MyEnum",
                    enumType = TypePrim
                      (PrimIntegral
                        (PrimInt Unsigned)),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName
                          "Say\20320\22909",
                        valueValue = 0,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "uses_utf8.h"],
                          singleLocLine = 5,
                          singleLocColumn = 9}},
                      EnumValue {
                        valueName = CName
                          "Say\25308\25308",
                        valueValue = 1,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "uses_utf8.h"],
                          singleLocLine = 6,
                          singleLocColumn = 9}}],
                    enumSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "uses_utf8.h"],
                      singleLocLine = 4,
                      singleLocColumn = 6}}})
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
                      "unMyEnum",
                    fieldType = HsPrimType
                      HsPrimCUInt,
                    fieldOrigin = FieldOriginNone}],
                structOrigin = StructOriginEnum
                  Enu {
                    enumTag = CName "MyEnum",
                    enumType = TypePrim
                      (PrimIntegral
                        (PrimInt Unsigned)),
                    enumSizeof = 4,
                    enumAlignment = 4,
                    enumValues = [
                      EnumValue {
                        valueName = CName
                          "Say\20320\22909",
                        valueValue = 0,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "uses_utf8.h"],
                          singleLocLine = 5,
                          singleLocColumn = 9}},
                      EnumValue {
                        valueName = CName
                          "Say\25308\25308",
                        valueValue = 1,
                        valueSourceLoc = SingleLoc {
                          singleLocPath = [
                            "examples",
                            "uses_utf8.h"],
                          singleLocLine = 6,
                          singleLocColumn = 9}}],
                    enumSourceLoc = SingleLoc {
                      singleLocPath = [
                        "examples",
                        "uses_utf8.h"],
                      singleLocLine = 4,
                      singleLocColumn = 6}}}
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
          valueSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "uses_utf8.h"],
            singleLocLine = 5,
            singleLocColumn = 9}}},
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
          valueSourceLoc = SingleLoc {
            singleLocPath = [
              "examples",
              "uses_utf8.h"],
            singleLocLine = 6,
            singleLocColumn = 9}}}]
