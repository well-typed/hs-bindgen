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
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "uses_utf8.h:4:6",
          declId = NamePair {
            nameC = CName "MyEnum",
            nameHsIdent = HsIdentifier
              "MyEnum"}},
        declKind = Enum
          Enum {
            enumNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "MyEnum",
              newtypeField = HsName
                "@NsVar"
                "un_MyEnum"},
            enumType = TypePrim
              (PrimIntegral PrimInt Unsigned),
            enumSizeof = 4,
            enumAlignment = 4,
            enumConstants = [
              EnumConstant {
                enumConstantLoc =
                "uses_utf8.h:5:9",
                enumConstantName = NamePair {
                  nameC = CName "Say\20320\22909",
                  nameHsIdent = HsIdentifier
                    "Say\20320\22909"},
                enumConstantValue = 0},
              EnumConstant {
                enumConstantLoc =
                "uses_utf8.h:6:9",
                enumConstantName = NamePair {
                  nameC = CName "Say\25308\25308",
                  nameHsIdent = HsIdentifier
                    "Say\25308\25308"},
                enumConstantValue = 1}]},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
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
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
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
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
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
                    fieldOrigin = GeneratedField}],
                structOrigin = Nothing,
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
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
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
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
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
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
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
            fieldOrigin = GeneratedField}],
        structOrigin = Nothing,
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
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "uses_utf8.h:5:9",
          enumConstantName = NamePair {
            nameC = CName "Say\20320\22909",
            nameHsIdent = HsIdentifier
              "Say\20320\22909"},
          enumConstantValue = 0}},
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
      patSynOrigin = EnumConstant
        EnumConstant {
          enumConstantLoc =
          "uses_utf8.h:6:9",
          enumConstantName = NamePair {
            nameC = CName "Say\25308\25308",
            nameHsIdent = HsIdentifier
              "Say\25308\25308"},
          enumConstantValue = 1}}]
